package kyo.internal

import DragSessions.*
import EventDispatch.*
import MountRegions.*
import RowRegions.*
import kyo.*
import kyo.Svg
import kyo.UI.*

/** Normalized reactive UI node. */
private[kyo] case class ReactiveUI(
    path: Seq[String],
    signal: Signal[UI],
    isConst: Boolean,
    children: Seq[ReactiveUI],
    handle: (Seq[String], UIEvent) => Boolean < (Async & Abort[Any]),
    region: ReactiveRegion,
    contentContext: ReactiveRegion.RegionIdentity,
    parentContext: ReactiveRegion.ParentContext,
    discoverContentRootBound: Boolean,
    mounted: Maybe[MountedSpec] = Absent,
    mountDispatch: Maybe[MountDispatch] = Absent,
    // Where the node sits: its path and transparent depth, which is what a region is known by in its mount table (see RegionKey).
    at: ReactiveRegion.RegionIdentity = ReactiveRegion.RegionIdentity(Seq.empty, 0),
    // The row-key depths of its path since the owner of its mount table, the session root or a mount instance (see `rowKeys`).
    rows: Long = 0L,
    deepRows: Chunk[Int] = Chunk.empty,
    // Reactive channels carried from the element's Attrs; subscribeScoped forks the in-place-patch observers
    // (empty for non-element / attribute-less nodes).
    channels: Channels = Channels.empty,
    // Render-time snapshot: the value the enclosing paint already put into the DOM. Subscribe skips an observer's first
    // emission while it still equals its snapshot, so a fresh paint is not patched again.
    renderedValue: Maybe[UI] = Absent,
    renderedChannels: RenderedChannels = RenderedChannels.empty,
    // Present on Foreach regions: carries the AST node (signal, key, render) and the normalize-time items
    // snapshot so subscribe can run the per-row reuse path instead of the whole-region re-render loop.
    foreachSpec: Maybe[ForeachSpec] = Absent,
    // Present on a region whose node kept the signal before its projection to UI (UI.Ast.Reactive.source). Subscribe observes
    // that, so an emission whose value is unchanged never re-renders a subtree that can only be compared as UI; a source lifted
    // from a Signal[String] is bound straight to the backend's text write instead (see bindTextRegion).
    source: Maybe[UI.Ast.Reactive.Source[?]] = Absent,
    // The value of `source` the enclosing paint painted, with what it painted for it.
    renderedSource: Maybe[UI.Ast.Reactive.Painted] = Absent,
    // The AST node's own `Frame`, so [[Devtools]] can name a region by its user call site rather than its path.
    frame: Frame = Frame.internal,
    bounded: Maybe[UI.Ast.Bounded] = Absent,
    // What an element's whileMounted effects run in, which the mount table keeps like a keyed mount (see MountDraw.ofElement).
    shown: Maybe[MountedSpec] = Absent
):
    /** Whether a mount is at or below this node, through regions but not into a mount's content: what a pass must record. */
    lazy val containsMount: Boolean = mounted.nonEmpty || shown.nonEmpty || children.exists(_.containsMount)
end ReactiveUI

/** Normalization-time companion of a [[kyo.UI.Ast.Foreach]] node: keeps the typed machinery (item signal, key
  * function, row render) reachable at subscribe time, plus the items snapshot matching the walked children.
  * `reusable` gates the per-row registry path: keys must exist (positional rows shift paths on removal) and the
  * render must not bake the index into row content. `rows` is the region's row registry, created at normalize
  * time so the node's event handler and the subscribe loop share it: dispatch resolves the target row from the
  * live registry instead of re-rendering and re-walking the whole list per event.
  */
final private[kyo] case class ForeachSpec(
    node: UI.Ast.Foreach[?, ?],
    renderedItems: Maybe[Chunk[?]],
    rows: RowRegistry
):
    def reusable: Boolean = node.key.nonEmpty && !node.indexed
end ForeachSpec

private[kyo] object ReactiveUI:

    import UI.Ast.*

    def init(
        path: Seq[String],
        signal: Signal[UI],
        isConst: Boolean,
        children: Seq[ReactiveUI],
        svgContext: Boolean,
        regionIdentity: ReactiveRegion.RegionIdentity,
        contentContext: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        rows: Long = 0L,
        deepRows: Chunk[Int] = Chunk.empty
    )(
        handle: (Seq[String], UIEvent) => Boolean < (Async & Abort[Any])
    )(using frame: Frame): ReactiveUI =
        ReactiveUI(
            path,
            signal,
            isConst,
            children,
            handle,
            ReactiveRegion.from(regionIdentity, svgContext),
            contentContext,
            parentContext,
            discoverContentRootBound = true,
            at = regionIdentity,
            rows = rows,
            deepRows = deepRows,
            frame = frame
        )

    /** The root of a mount session: creates the session's [[MountDispatch]] and records it on the returned node for `subscribe`. */
    def normalize(ui: UI, path: Seq[String], svg: Boolean = false): ReactiveUI < Sync =
        given Frame = ui.frame
        // Same reason as HtmlRenderer.render: every child and row path is built with `:+`, which on a List
        // root copies the whole prefix per node. A Vector root makes every derived path a Vector; paths are
        // only built, compared and joined.
        val rootPath = path.toVector
        // The walk is one Sync effect, not one per node: its only effects are signal reads and atomic cells, so it runs as a
        // direct recursion under a single defer and reads through SignalNow. A suspension per read would cost a keyed list a
        // continuation per enclosing frame for every row.
        SignalNow.Locals.take(Sync.Unsafe.defer {
            val dispatch = MountDispatch.initNow()
            // The root is a range of its own, as every region's value is: the calls of one keyed site in it are told apart by their order.
            val root = DrawContext.inRange(normalizeAt(
                ui,
                rootPath,
                svg,
                ReactiveRegion.RegionIdentity.root(rootPath),
                ReactiveRegion.ParentContext.Other,
                dispatch,
                rows = 0L,
                deepRows = Chunk.empty
            ))
            root.copy(mountDispatch = Present(dispatch))
        })
    end normalize

    /** The same tree without its render-time snapshots, for a normalize that painted nothing.
      *
      * The snapshots let subscribe skip a first emission the page already shows, which holds only where this normalize
      * is the one the page was rendered from. A server session is not: the page came from an earlier HTTP render, and a
      * reconnecting client still shows whatever the previous session last sent. Without snapshots every observer's first
      * emission goes out, so a session starts by bringing the page to the current state.
      */
    def unpainted(rui: ReactiveUI): ReactiveUI =
        rui.copy(
            children = rui.children.map(unpainted),
            foreachSpec = rui.foreachSpec.map(_.copy(renderedItems = Absent)),
            renderedValue = Absent,
            renderedSource = Absent,
            renderedChannels = RenderedChannels.empty
        )

    /* Normalization and the tree walk carry, for the mounted nodes and regions they meet, the session's dispatch table and which
     * depths of the path, since the enclosing reactive region, hold the key of a keyed list row: a bit per depth below 63 in `rows`,
     * and the rare deeper ones in `deepRows`. Both are read out of the path only when a mount or a region's table needs them, so the
     * walk itself allocates nothing for them.
     */

    /** The row keys `rows` and `deepRows` mark in `path`, innermost first. */
    private def rowKeys(path: Seq[String], rows: Long, deepRows: Chunk[Int]): Chunk[String] =
        if rows == 0L && deepRows.isEmpty then Chunk.empty
        else
            path.iterator.zipWithIndex.foldLeft(Chunk.empty[String]) { case (acc, (segment, depth)) =>
                val isRow = if depth < 63 then (rows & (1L << depth)) != 0L else deepRows.contains(depth)
                if isRow then acc.append(segment) else acc
            }.reverse

    private def normalizeAt(
        ui: UI,
        path: Seq[String],
        svg: Boolean,
        regionIdentity: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        cells: MountDispatch,
        rows: Long,
        deepRows: Chunk[Int]
    )(using AllowUnsafe, SignalNow.Locals): ReactiveUI =
        given Frame = ui.frame
        ui match
            case ui: Reactive[?] =>
                val contentContext = regionIdentity.transparent
                // With a source, what the node paints is one value of it: the first emission can tell it is the same one.
                val painted: Maybe[UI.Ast.Reactive.Painted] = ui.source.map(_.paintedNow)
                val current                                 = painted.fold(SignalNow(ui.signal))(_.ui)
                val contentParentContext                    = nestedParentContext(parentContext, current)
                val (kids, _)                               =
                    DrawContext.inRegion(walkStatic(current, path, svg, contentContext, contentParentContext, cells, rows, deepRows))
                init(
                    path,
                    ui.signal,
                    isConst = false,
                    kids,
                    svg,
                    regionIdentity,
                    contentContext,
                    parentContext,
                    rows,
                    deepRows
                ) {
                    (targetPath, event) =>
                        Sync.Unsafe.defer {
                            val currentUI            = SignalNow(ui.signal)
                            val contentParentContext = nestedParentContext(parentContext, currentUI)
                            val (_, freshHdl)        =
                                DrawContext.inRegion(walkStatic(
                                    currentUI,
                                    path,
                                    svg,
                                    contentContext,
                                    contentParentContext,
                                    cells,
                                    rows,
                                    deepRows
                                ))
                            freshHdl(targetPath, event)
                        }
                }.copy(renderedValue = Present(current), source = ui.source, renderedSource = painted)

            case ui: Foreach[?, ?] @unchecked =>
                ui.applyTyped {
                    [T] => (itemSignal, keyFn, renderFn) =>
                        val sig                  = itemSignal.map(items => foreachFragment(keyFn, renderFn, items))
                        val contentContext       = regionIdentity
                        val registry             = RowRegistry.initNow()
                        val items0               = SignalNow(itemSignal)
                        val current              = foreachFragment(keyFn, renderFn, items0)
                        val contentParentContext = nestedParentContext(parentContext, current)
                        val (kids, _)            =
                            DrawContext.inRegion(walkStatic(
                                current,
                                path,
                                svg,
                                contentContext,
                                contentParentContext,
                                cells,
                                rows,
                                deepRows
                            ))
                        // Fallback dispatch: re-render the whole list from the current value and route
                        // through the fresh fragment handler. Used whenever the row registry cannot
                        // resolve the target (region not subscribed on the reuse path, row painted but
                        // not yet registered, or an event addressed to the region node itself).
                        val fullWalk: Handler = (targetPath, event) =>
                            Sync.Unsafe.defer {
                                val currentUI          = SignalNow(sig)
                                val freshParentContext = nestedParentContext(parentContext, currentUI)
                                val (_, freshHdl)      =
                                    DrawContext.inRegion(walkStatic(
                                        currentUI,
                                        path,
                                        svg,
                                        contentContext,
                                        freshParentContext,
                                        cells,
                                        rows,
                                        deepRows
                                    ))
                                freshHdl(targetPath, event)
                            }
                        init(
                            path,
                            sig,
                            isConst = false,
                            kids,
                            svg,
                            regionIdentity,
                            contentContext,
                            parentContext,
                            rows,
                            deepRows
                        ) {
                            (targetPath, event) =>
                                // Row-registry fast path: the segment after the region path is the row
                                // key, so the live RowInstance resolves the target directly. Its cached
                                // handler is as fresh as a re-derived one: the registry retains a row
                                // only while its item value is unchanged (a changed item rebuilds the
                                // row, and with it the handler), which is the same purity assumption
                                // the paint reuse already makes. A seeded row has no cached handler
                                // yet; its cached rowUI is walked on demand (one row, not the list).
                                if targetPath.size > path.size then
                                    registry.find(targetPath(path.size)).map {
                                        case Present(inst) =>
                                            inst.handler match
                                                case Present(h) => h(targetPath, event)
                                                case Absent     =>
                                                    Sync.Unsafe.defer {
                                                        walkRow(
                                                            inst.rowUI,
                                                            path :+ inst.key,
                                                            svg,
                                                            contentContext.child(inst.key),
                                                            nestedParentContext(parentContext, inst.rowUI),
                                                            cells,
                                                            rows,
                                                            deepRows
                                                        )._2(targetPath, event)
                                                    }
                                        case Absent => fullWalk(targetPath, event)
                                    }
                                else fullWalk(targetPath, event)
                        }.copy(
                            renderedValue = Present(current),
                            foreachSpec = Present(ForeachSpec(ui, Present(items0), registry))
                        )
                }

            case ui: Element =>
                // An element with a SignalRef-bound attribute (`.value(ref)` xor `.checked(ref)`; an element binds at most
                // one, see collectSignalRef) must re-render when that signal changes. The change is carried by the ref
                // (read afresh at render time), not by the rendered value's identity: the value is always the same `ui`
                // object, kept with its `Bound.Ref` attributes so the rendered HTML carries the auto-binding event markers
                // `data-kyo-ev="input"/"change"` the client needs and the dispatch handler resolves the ref. The region's
                // signal is therefore `changesTo`, not `map(_ => ui)`: observation deduplicates on the emitted value, which
                // never changes here, while `changesTo` detects change on the ref's own values and keeps its exact
                // protocol. An element with no bound ref is const.
                val (elementSignal, isConstNode) =
                    collectSignalRef(ui).fold((Signal.initConst(ui: UI), true))(ref => (ref.changesTo(ui: UI), false))
                val (kids, hdl) = walkStatic(
                    ui,
                    path,
                    svg,
                    regionIdentity,
                    parentContext,
                    cells,
                    rows,
                    deepRows,
                    discoverRootBound = false
                )
                val channels = Channels.of(ui)
                val snapshot = channels.rendered
                ReactiveUI(
                    path,
                    elementSignal,
                    isConst = isConstNode,
                    kids,
                    hdl,
                    ReactiveRegion.from(regionIdentity, svg),
                    regionIdentity,
                    parentContext,
                    discoverContentRootBound = false,
                    at = regionIdentity,
                    rows = rows,
                    deepRows = deepRows,
                    channels = channels,
                    renderedChannels = snapshot,
                    frame = ui.frame,
                    shown =
                        if ui.attrs.whileMounted.isEmpty then Absent
                        else
                            Present(MountDraw.ofElement(ui.attrs, path, rowKeys(path, rows, deepRows), ui.frame))
                )

            case ui: Mounted if Boundaries.implicitOf(ui).nonEmpty =>
                normalizeAt(Boundaries.implicitOf(ui).get, path, svg, regionIdentity, parentContext, cells, rows, deepRows)

            case b: Bounded =>
                // A region of its own, so its mounts get a table that carries its error view, at the path of the node it wraps.
                val contentContext       = regionIdentity.transparent
                val contentParentContext = nestedParentContext(parentContext, b.child)
                val (kids, childHandle)  =
                    DrawContext.inRange(walkStatic(b.child, path, svg, contentContext, contentParentContext, cells, rows, deepRows))
                // What the render put on the page for this node, by the rule `HtmlRenderer` paints a boundary with. Until the boundary's
                // subscription registers its gate that paint is all the reader has seen, so a click on the placeholder must reach the
                // placeholder: the content it stands for has no instance yet, and the click would fall through to the elements around it.
                val paintedPlaceholder: Maybe[UI] =
                    if b.display.isEmpty && b.settings.waits && {
                            val locals = summon[SignalNow.Locals]
                            val here   = locals.read(DrawContext.scope).copy(rows = rowKeys(path, rows, deepRows))
                            Boundaries.catchesNow(b.child, contentContext, cells)(using
                                summon[AllowUnsafe],
                                ui.frame,
                                locals.bound(DrawContext.scope, here).bound(DrawContext.range, Present(new DrawContext.Occurrences))
                            )
                        }
                    then
                        b.settings.placeholderView
                    else Absent
                def toPlaceholder(ph: UI, targetPath: Seq[String], event: UIEvent): Boolean < (Async & Abort[Any]) =
                    Sync.Unsafe.defer {
                        val (_, placeholderHandle) =
                            DrawContext.inRange(walkStatic(
                                ph,
                                path,
                                svg,
                                contentContext,
                                nestedParentContext(parentContext, ph),
                                cells,
                                rows,
                                deepRows
                            ))
                        placeholderHandle(targetPath, event)
                    }
                // Content a boundary holds back is not on the page, so no event reaches it, while its placeholder is and takes them;
                // and a range that keeps showing old content while its new content loads takes no event, which would otherwise land
                // in the new content.
                val handle: Handler = (targetPath, event) =>
                    Failures.within(FailureFrame.Boundary(b.settings))(cells.boundary(contentContext).map {
                        case Present(gate) if !gate.isRevealed =>
                            gate.livePlaceholder match
                                case Present(ph) => toPlaceholder(ph, targetPath, event)
                                case Absent      => true
                        case Absent if paintedPlaceholder.nonEmpty => toPlaceholder(paintedPlaceholder.get, targetPath, event)
                        case _ => cells.held(targetPath).map(held => if held then true else childHandle(targetPath, event))
                    })
                ReactiveUI(
                    path = path,
                    signal = Signal.initConst(b.child),
                    isConst = false,
                    children = kids,
                    handle = handle,
                    region = ReactiveRegion.from(regionIdentity, svg),
                    contentContext = contentContext,
                    parentContext = parentContext,
                    discoverContentRootBound = true,
                    at = regionIdentity,
                    rows = rows,
                    deepRows = deepRows,
                    frame = b.frame,
                    bounded = Present(b)
                )

            case ui: MountNode =>
                val contentContext = regionIdentity.transparent
                val specRows       = rowKeys(path, rows, deepRows)
                // Where this node sits for its ids, as this walk sees it: an event that draws it later draws it here.
                val scopeHere = summon[SignalNow.Locals].read(DrawContext.scope).copy(rows = specRows)
                val spec      = MountedSpec(
                    ui,
                    path,
                    specRows,
                    contentContext,
                    occurrence = DrawContext.occurrence(specRows, ui),
                    scope = scopeHere
                )
                ui match
                    case d: Drawn   => cells.noteOccurrence(contentContext, d, spec.occurrence)
                    case _: Mounted => ()
                // The content is the instance's: what an event draws in it derives its ids from `base`.
                def intoContent(content: UI, base: String, targetPath: Seq[String], event: UIEvent): Boolean < (Async & Abort[Any]) =
                    DrawContext.scope.let(DrawContext.contentScope(base, ui, contentContext))(Sync.Unsafe.defer(
                        DrawContext.inContent(base, ui, contentContext) {
                            val (_, freshHdl) = walkStatic(
                                content,
                                path,
                                svg,
                                contentContext,
                                nestedParentContext(parentContext, content),
                                cells,
                                rows = 0L,
                                deepRows = Chunk.empty
                            )
                            freshHdl(targetPath, event)
                        }
                    ))
                // The node value is fresh on every dispatch, so the handler finds the live instance through the dispatch table.
                val handle: Handler = (targetPath, event) =>
                    cells.instanceForEvent(contentContext, ui).map {
                        case Absent =>
                            // What a drawn node shows takes events from the paint on, with the state its instance will hold.
                            ui match
                                case d: Drawn =>
                                    cells.drawn(contentContext, d, scopeHere, spec.occurrence).map((ui, base) =>
                                        intoContent(ui, base, targetPath, event)
                                    )
                                case _: Mounted => true
                        case Present(running) =>
                            running.cell.current.map {
                                case MountState.Pending(Present(_)) => true
                                case state                          =>
                                    Failures.within(FailureFrame.Mount(running.supervision))(
                                        intoContent(state.content(spec), running.children.idBase, targetPath, event)
                                    )
                            }
                    }
                ReactiveUI(
                    path = path,
                    signal = Signal.initConst(spec.placeholder),
                    isConst = false,
                    children = Seq.empty,
                    handle = handle,
                    region = ReactiveRegion.from(regionIdentity, svg),
                    contentContext = contentContext,
                    parentContext = parentContext,
                    discoverContentRootBound = true,
                    mounted = Present(spec),
                    at = regionIdentity,
                    rows = rows,
                    deepRows = deepRows,
                    frame = ui.frame
                )

            case ui =>
                // Catch-all for static leaf nodes: Text, Fragment, KeyedChild.
                // All interactive types extend Element (handled above) and all reactive types are
                // Reactive, Foreach or Mounted (also handled above). If a new UI subtype is added, the
                // exhaustiveness checker will NOT warn here; any new type that is interactive must
                // be added as an explicit case above before this catch-all.
                ReactiveUI(
                    path,
                    Signal.initConst(ui),
                    isConst = true,
                    Seq.empty,
                    (_, _) => true,
                    ReactiveRegion.from(regionIdentity, svg),
                    regionIdentity,
                    parentContext,
                    discoverContentRootBound = false,
                    at = regionIdentity,
                    frame = ui.frame
                )
        end match
    end normalizeAt

    /** Returns the element's single SignalRef-bound attribute (its `.value` or `.checked`), if any, so the element can be made reactive over
      * it (its HTML re-renders when that signal changes). An element binds at most one such ref.
      */
    private[kyo] def collectSignalRef(elem: Element): Maybe[Signal[?]] =
        def refOf(v: Maybe[Bound[?]]): Maybe[Signal[?]] = v match
            case Present(Bound.Ref(ref)) => Present(ref)
            case _                       => Absent
        elem match
            case ti: TextInput    => refOf(ti.value)
            case ri: RangeInput   => refOf(ri.value)
            case pi: PickerInput  => refOf(pi.value)
            case bi: BooleanInput => refOf(bi.checked)
            case h: HiddenInput   => refOf(h.value)
            case _                => Absent
        end match
    end collectSignalRef

    private[internal] def nestedParentContext(parentContext: ReactiveRegion.ParentContext, current: UI): ReactiveRegion.ParentContext =
        (parentContext, ReactiveRegion.tableContent(current)) match
            case (ReactiveRegion.ParentContext.HtmlTable, ReactiveRegion.TableContent.Rows) => ReactiveRegion.ParentContext.Other
            case _                                                                          => parentContext

    /** True when a child UI needs its own ReactiveUI node during a static walk: reactive nodes always, and an
      * element whose ROOT carries a binding (a bound input ref or any reactive attr/bool-attr/class channel).
      * Shared by the element-children and the fragment-children walk so the promotion rule cannot diverge
      * between them.
      */
    private def needsOwnNode(ui: UI): Boolean = ui match
        case _: Reactive[?] | _: Foreach[?, ?] | _: MountNode | _: Bounded => true
        case el: Element                                                   =>
            collectSignalRef(el).nonEmpty || el.attrs.hasChannels || Channels.follows(el) || el.attrs.whileMounted.nonEmpty
        case _ => false

    /** The Fragment a Foreach paints for `items`: one KeyedChild per element, keyed by the key function (or
      * the index when unkeyed). Single source of truth for the mapped region signal, the normalize-time
      * snapshot, and the reuse path's per-row rebuilds.
      */
    private def foreachFragment[A](keyFn: Maybe[A => String], renderFn: (Int, A) => UI, items: Chunk[A])(using Frame): UI =
        val arr = items.toSeq.zipWithIndex.map { (item, i) =>
            val key = if keyFn.nonEmpty then keyFn.get(item) else i.toString
            KeyedChild[UI](key, renderFn(i, item))
        }
        Fragment[UI](Chunk.from(arr)): UI
    end foreachFragment

    /** Walk one reusable Foreach row at `rowPath`, whose last segment is the row's key. A row root carrying a binding channel (or
      * being itself reactive) is promoted to its own ReactiveUI node, exactly like a fragment child in walkStatic. Returns the walked
      * reactive kids plus the row's dispatch handler (cached on the RowInstance for registry-first dispatch).
      */
    private[internal] def walkRow(
        rowUI: UI,
        rowPath: Seq[String],
        svg: Boolean,
        context: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        cells: MountDispatch,
        outerRows: Long,
        outerDeepRows: Chunk[Int]
    )(using AllowUnsafe, Frame, SignalNow.Locals): (Seq[ReactiveUI], Handler) =
        val depth    = rowPath.size - 1
        val rows     = if depth < 63 then outerRows | (1L << depth) else outerRows
        val deepRows = if depth < 63 then outerDeepRows else outerDeepRows.append(depth)
        // A row is a range of its own: walked alone (a rebuilt row) it counts the same as walked with its region, where its row key
        // tells its calls apart from the other rows' anyway.
        DrawContext.inRegion {
            if needsOwnNode(rowUI) then
                val rui = normalizeAt(rowUI, rowPath, svg, context, parentContext, cells, rows, deepRows)
                (Seq(rui), rui.handle)
            else walkStatic(rowUI, rowPath, svg, context, parentContext, cells, rows, deepRows)
            end if
        }
    end walkRow

    /** Walk a static UI tree. Collect reactive children, build handle. */
    private[internal] def walkStatic(
        ui: UI,
        basePath: Seq[String],
        svg: Boolean = false,
        context: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        cells: MountDispatch,
        rows: Long,
        deepRows: Chunk[Int],
        discoverRootBound: Boolean = true
    )(using AllowUnsafe, Frame, SignalNow.Locals): (Seq[ReactiveUI], Handler) =
        ui match
            case elem: Element if discoverRootBound && (collectSignalRef(elem).nonEmpty || elem.attrs.whileMounted.nonEmpty) =>
                val rui = normalizeAt(elem, basePath, svg, context, parentContext, cells, rows, deepRows)
                (Seq(rui), rui.handle)

            case elem: Element =>
                // ForeignObject bridges back to HTML, so reset svg context to false. It MUST be matched
                // before SvgElement (ForeignObject IS an SvgElement).
                val childSvg = elem match
                    case _: Svg.ForeignObject => false
                    case _: Svg.SvgElement    => true
                    case _                    => svg
                val childParentContext = elem match
                    case _: Table => ReactiveRegion.ParentContext.HtmlTable
                    case _        => ReactiveRegion.ParentContext.Other
                val kids     = Chunk.newBuilder[ReactiveUI]
                val handlers = Chunk.newBuilder[(Int, Handler)]
                var i        = 0
                elem.children.foreach { child =>
                    val segment      = i.toString
                    val childPath    = basePath :+ segment
                    val childContext = context.child(segment)
                    if needsOwnNode(child) then
                        // Normalize into a ReactiveUI node so subscribeScoped wires the updates. Required
                        // even for a const element carrying ONLY reactive attrs: otherwise it is not walked
                        // into a node and its in-place-patch observers would never start.
                        discard(kids += normalizeAt(child, childPath, childSvg, childContext, childParentContext, cells, rows, deepRows))
                    else
                        val (innerKids, innerHandle) =
                            walkStatic(child, childPath, childSvg, childContext, childParentContext, cells, rows, deepRows)
                        kids ++= innerKids
                        discard(handlers += ((i, innerHandle)))
                    end if
                    i += 1
                }
                val reactiveChildren: Seq[ReactiveUI]   = kids.result()
                val staticHandlers: Seq[(Int, Handler)] = handlers.result()
                // Where this element sits for ids, as this walk sees it: the instance that holds it and the rows around it. A node the
                // dispatch draws on its way to the target derives its id from there, as the page's did.
                val scopeHere       = summon[SignalNow.Locals].read(DrawContext.scope).copy(rows = rowKeys(basePath, rows, deepRows))
                val handle: Handler = (targetPath, event) =>
                    DrawContext.scope.let(scopeHere)(
                        dispatch(elem, basePath, context, targetPath, event, reactiveChildren, staticHandlers, cells)
                    )
                (reactiveChildren, handle)

            case Fragment(children) =>
                val kids    = Chunk.newBuilder[ReactiveUI]
                val handles = Chunk.newBuilder[Handler]
                var i       = 0
                children.foreach { child =>
                    // Three plain values rather than a tuple: this runs per row on every walk, dispatch included.
                    val keyed   = child.isInstanceOf[KeyedChild[?]]
                    val segment = child match
                        case kc: KeyedChild[?] => kc.key
                        case _                 => i.toString
                    val childPath    = basePath :+ segment
                    val childContext = context.child(segment)
                    val depth        = if keyed then basePath.size else 0
                    val inner        = child match
                        case kc: KeyedChild[?] => kc.child
                        case _                 => child
                    val innerRows     = if keyed && depth < 63 then rows | (1L << depth) else rows
                    val innerDeepRows = if keyed && depth >= 63 then deepRows.append(depth) else deepRows
                    if needsOwnNode(inner) then
                        // Same contract as an element's reactive child: a row whose root carries a binding channel (a keyed row
                        // with a reactive class, say) gets its own node, or its in-place-patch observers would never start.
                        val rui = normalizeAt(inner, childPath, svg, childContext, parentContext, cells, innerRows, innerDeepRows)
                        discard(kids += rui)
                        discard(handles += rui.handle)
                    else
                        val (innerKids, innerHandle) =
                            walkStatic(inner, childPath, svg, childContext, parentContext, cells, innerRows, innerDeepRows)
                        kids ++= innerKids
                        discard(handles += innerHandle)
                    end if
                    i += 1
                }
                locally {
                    val allKids    = kids.result()
                    val allHandles = handles.result()
                    // Only a fragment with keyed children needs the key lookup.
                    val keyMap: Map[String, Int] =
                        if children.exists(_.isInstanceOf[KeyedChild[?]]) then
                            val b = Map.newBuilder[String, Int]
                            var i = 0
                            children.foreach { child =>
                                child match
                                    case kc: KeyedChild[?] => b += kc.key -> i
                                    case _                 => ()
                                i += 1
                            }
                            b.result()
                        else Map.empty
                    val handle: Handler = (targetPath, event) =>
                        if targetPath.size > basePath.size then
                            val segment = targetPath(basePath.size)
                            val idx     = Maybe.fromOption(keyMap.get(segment)).orElse(Maybe.fromOption(segment.toIntOption))
                            // Every child has a handle, so a segment matching none names a child that has left the page
                            // since the client sent the event; like a missing child of an element, it reaches nothing.
                            idx.filter(i => i >= 0 && i < allHandles.length).map(allHandles(_))
                                .fold(false: Boolean < (Async & Abort[Any]))(h => h(targetPath, event))
                        else true
                    (allKids, handle)
                }

            case _: Reactive[?] | _: Foreach[?, ?] | _: MountNode | _: Bounded =>
                // When walkStatic is called with a Reactive, Foreach or mount as the top-level node
                // (not as a child of an Element), normalize it at basePath so subscribeNode
                // sets up a subscription for it. This handles the case where an outer reactive's
                // signal value is itself a Reactive or Foreach (e.g. outer.map { _ => inner.map(UI.span(_)) }).
                val rui = normalizeAt(ui, basePath, svg, context, parentContext, cells, rows, deepRows)
                (Seq(rui), rui.handle)

            case _ =>
                val noHandle: Handler = (_, _) => true
                (Seq.empty[ReactiveUI], noHandle)

    // ---- Subscribe ----

    /** Result of subscribe: dispatch handler + signal change tracking. The enclosing Scope owns every
      * region fiber's lifecycle; the per-value Scope (opened by observe) owns each region's children,
      * so closing the root scope cascade-tears-down the tree.
      */
    case class Subscription(
        handle: (Seq[String], UIEvent) => Boolean < Async,
        private[kyo] val handleValidated: ValidatedHandler,
        lastSignalChangeTime: AtomicRef[Instant],
        private val dragSessionCountFn: () => Int < Sync = () => 0,
        private val dragExpiryWorkerCountFn: () => Int < Sync = () => 0,
        private val dragLock: Meter = Meter.Noop,
        /** The session's mount tables report their work here (see [[MountCounters]]). */
        private[kyo] val mountSession: MountSession = new MountSession
    ):
        private[kyo] def dragSessionCount(using Frame): Int < Sync      = dragSessionCountFn()
        private[kyo] def dragExpiryWorkerCount(using Frame): Int < Sync = dragExpiryWorkerCountFn()

        /** Runs `v` under the mutex that serializes drag dispatch and the expiry worker's clock reading and timer arming. A test moves a
          * controlled clock here so the move never splits the worker's reading from its arming.
          */
        private[kyo] def withDragLock[A](v: => A < Async)(using Frame): A < (Async & Abort[Closed]) = dragLock.run(v)
    end Subscription

    /** Starts a region's fiber in the current scope, which interrupts it and awaits its unwind when it closes. What ends it other than
      * that interrupt is logged as `what` at `path`.
      */
    private[internal] def regionFiber(path: Seq[String], what: String)(observe: => Unit < (Async & Abort[Throwable]))(using
        Frame
    ): Unit < (Async & Scope) =
        Fiber.init {
            Abort.run[Throwable](observe).map { result =>
                result.fold(
                    _ => (),
                    err => Log.error(s"$what failed at path=${path.mkString(".")}", err),
                    panic =>
                        if panic.isInstanceOf[Interrupted] then ()
                        else Log.error(s"$what failed at path=${path.mkString(".")}", panic)
                )
            }
        }.unit

    /** Subscribe all reactive boundaries under the caller's Scope. Returns the dispatch handle + change
      * time; lifecycle is owned by the enclosing Scope (closing it interrupts every region fiber and awaits its unwind).
      */
    def subscribe(
        rui: ReactiveUI,
        exchange: UIExchange,
        dragLimits: DragSessionLimits = DragSessionLimits()
    )(using Frame): Subscription < (Async & Scope) =
        for
            signalChangeTime <- AtomicRef.init(Instant.Epoch)
            dragSessions     <- AtomicRef.init(Map.empty[String, DragSession])
            dragMutex        <- Meter.initMutex
            expiryWake       <- Channel.init[Unit](1)
            expiryWorkers    <- AtomicInt.init(0)
            _                <- Scope.ensure(dragSessions.set(Map.empty))
            // The worker clears its own count as it unwinds, which is right when it exits on its own but is not
            // enough at scope close: interrupting a fiber and awaiting its result does not await the finalizer
            // that clears the count, so the count can still read 1 after the scope has returned. Scope
            // finalizers run last-registered-first, so registering this BEFORE the worker is started puts it
            // after the worker's own teardown and makes "closed scope implies no worker" hold on every exit.
            _ <- Scope.ensure(expiryWorkers.set(0))
            _ <- expiryWorkers.set(1)
            _ <- Fiber.init(
                Abort.run[Any] {
                    Sync.ensure(expiryWorkers.set(0))(expiryScheduler(dragSessions, dragMutex, expiryWake))
                }.unit
            )
            // Absent only for a ReactiveUI a test builds by hand; `normalize` always records the table.
            dispatch <- rui.mountDispatch match
                case Present(d) => Kyo.lift(d)
                case Absent     => MountDispatch.init
            audited <- MountSession.audited.get
            session = new MountSession(audited)
            // The session is a group of its own, released before anything it subscribed is. Every fiber it forks paints for this table.
            _ <- MountDispatch.live.let(Present(dispatch))(ReleaseGroup.owning { group =>
                for
                    (rootTable, rootMounts) <- MountTable.root(rui.path, session, group)
                    _                       <- Scope.ensure(rootTable.evictAll)
                    // The root's pass covers the whole tree, so every identity shown twice anywhere in it is found here.
                    rootPass <- rootMounts.beginWalk(Seq(rui))
                    _        <- subscribeScoped(rui, exchange, signalChangeTime, rootMounts, dispatch, rootPass)
                    _        <- rootMounts.endWalk
                yield ()
            })
            // Keyboard activation is synthesized at the bottom, on the tree's own handler, because the
            // two transports enter at different heights: the browser mount goes through `handle`, and
            // the server-push session through `handleValidated` (UIServer). Wrapping either one alone
            // suppresses the browser's activation on both while replacing it on only one, which is a
            // button that stops working rather than one that works twice.
            activating = activatingHandle(assertingHandle(rui.handle, exchange))
            dragging   = dragHandle(activating, dragSessions, dragMutex, expiryWake, dragLimits)
            // A handler runs in the session, so a UI.batch inside it finds it; the handler itself is no change of its own (see MountTable).
            validatedHandle: ValidatedHandler = (path, event) => MountSession.current.let(Present(session))(dragging(path, event))
        yield Subscription(
            (path, event) =>
                DragProtocol.validateEventAndDomain(event, DragProtocol.Limits.default) match
                    case Result.Success(validated) => validatedHandle(path, validated)
                    case _                         => true,
            validatedHandle,
            signalChangeTime,
            () => dragSessions.get.map(_.size),
            () => expiryWorkers.get,
            dragMutex,
            session
        )

    /** Runs `ui` under `exchange` in a scope of its own and hands over the subscription. What runs a tree for its effects, and what a test
      * drives one with.
      */
    private[kyo] def subscribed[A, S](
        ui: UI,
        exchange: UIExchange,
        dragLimits: DragSessionLimits = DragSessionLimits()
    )(f: Subscription => A < (Async & Scope & S))(using Frame): A < (Async & S) =
        Scope.run(normalize(ui, Seq.empty).map(root => subscribe(root, exchange, dragLimits).map(f)))

    /** The tail of showing a walked value in `table`: the channels of the root it painted, its kids, the end of its pass. */
    private[internal] def subscribeWalked(
        path: Seq[String],
        shown: UI,
        kids: Seq[ReactiveUI],
        pass: MountPass,
        table: MountScope,
        through: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        dispatch: MountDispatch
    )(using Frame): Unit < (Async & Scope) =
        forkRootChannels(path, shown, through)
            .andThen(Kyo.foreachDiscard(kids)(subscribeScoped(_, through, signalChangeTime, table, dispatch, pass)))
            .andThen(table.endWalk)

    /** Puts back an edit its owner did not take on an element that follows a signal. The browser changed the control before the event got here,
      * and an owner that declines leaves the signal as it was, so no emission would ever reach the control. Compares once the event has
      * finished bubbling, and sends through the session's own exchange: content a boundary holds back takes no event, so there is nothing to
      * wait for. Only an edit records, so every other event goes straight through.
      */
    private def assertingHandle(handle: Handler, exchange: UIExchange)(using Frame): Handler =
        (path, event) =>
            event match
                case _: (UIEvent.Input | UIEvent.Change | UIEvent.ChangeChecked | UIEvent.ChangeNumeric) =>
                    for
                        recorded <- AtomicRef.init(Maybe.empty[UIExchange => Unit < Async])
                        keep     <- followEdit.let(Present(recorded))(handle(path, event))
                        send     <- recorded.get
                        _        <- send.fold(Kyo.unit)(_(exchange))
                    yield keep
                case _ => handle(path, event)

    /** Turns a keydown's recorded activation into the click it stands for.
      *
      * A browser answers Enter or Space on a button by firing a click at it, and Enter in a field by
      * firing one at the form's default button; the form submit is that click's activation behaviour,
      * never the key's. This is where kyo does the same: the keydown bubbles first, recording at most
      * one path in [[activationTarget]], and the click is dispatched at that path afterwards, through
      * the whole chain, so ancestors see it exactly as they see a mouse click.
      *
      * Dispatching it after the keydown has finished is what keeps the order the platform has, with the
      * element's own `onKeyDown` running before its activation. Only a keydown records, so the click
      * this dispatches cannot record another: there is no recursion here.
      */
    private def activatingHandle(handle: Handler)(using Frame): Handler =
        (path, event) =>
            event match
                // Only these keys record an activation (a button's Enter or Space, a form's implicit submission on Enter), so
                // every other keydown, every letter typed into a field among them, goes straight through.
                case kd: UIEvent.KeyDown if KeyPolicy.mayActivate(kd.keyboard.key) =>
                    for
                        recorded <- AtomicRef.init(Maybe.empty[Seq[String]])
                        keep     <- activationTarget.let(Present(recorded))(handle(path, kd))
                        target   <- recorded.get
                        _        <- target.fold(Kyo.lift(())) { clickPath =>
                            handle(
                                clickPath,
                                UIEvent.Click(
                                    clickPath,
                                    MouseEventData(modifiers = kd.keyboard.modifiers, targetId = kd.keyboard.targetId)
                                )
                            ).unit
                        }
                    yield keep
                case _ => handle(path, event)

    // Each reactive region starts an observer fiber as one scoped resource. Its finalizer interrupts the observer and
    // awaits the fiber's terminal result. Signal.observe runs each value in a fresh Scope; each value renders, re-walks,
    // and starts its children in that Scope, so the next value or an interrupt closes them transitively. A const node
    // has no signal: it subscribes its children in the enclosing scope. `mounts` is the enclosing region's scope on the mount table
    // that holds its keyed instances, outside every subscription's scopes, and `pass` the render pass of that region.
    private[internal] def subscribeScoped(
        rui: ReactiveUI,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        mounts: MountScope,
        dispatch: MountDispatch,
        pass: MountPass,
        // Where a keyed row collects its channel releases (see RowScope); Absent everywhere else, where a release registers
        // on the Scope. Forwarded only along the row's own static children: a region or mount below the row owns its bindings
        // in its own scope.
        releases: Maybe[RowScope] = Absent
    )(using Frame): Unit < (Async & Scope) =
        // Start the scoped in-place-patch observers for this node's reactive channels. Runs unconditionally: a
        // no-op for attribute-less nodes, but an element carrying only reactive attrs is `isConst = true` and
        // would otherwise fork no observer of its own.
        forkChannelObservers(rui.path, rui.channels, rui.renderedChannels, exchange, rui.frame, releases).andThen(
            rui.shown.fold(Kyo.unit: Unit < (Async & Scope))(subscribeElementEffects(rui, _, exchange, mounts, pass))
        ).andThen {
            if rui.mounted.nonEmpty then
                subscribeMounted(rui, rui.mounted.get, exchange, signalChangeTime, mounts, pass, dispatch)
            else if rui.bounded.nonEmpty then
                subscribeBoundary(rui, rui.bounded.get, exchange, signalChangeTime, mounts, dispatch, pass)
            else if rui.isConst then
                // A constant node belongs to the enclosing region's pass, which already counts its children.
                Kyo.foreachDiscard(rui.children)(subscribeScoped(_, exchange, signalChangeTime, mounts, dispatch, pass, releases))
            // Inside a waiting boundary a list goes through the whole-region path, whose passes the boundary counts.
            // A region or a list runs again, so what its passes draw can be shown again with other props.
            else if rui.foreachSpec.exists(_.reusable) && exchange.gate.isEmpty then
                DrawContext.inRegionScope(
                    subscribeForeachRegion(rui, rui.foreachSpec.get, exchange, signalChangeTime, mounts.child(pass, rui), dispatch)
                )
            else
                bindTextRegion(rui, exchange, releases).map { bound =>
                    if bound then Kyo.unit
                    else DrawContext.inRegionScope(subscribeRegion(rui, exchange, signalChangeTime, mounts.child(pass, rui), dispatch))
                }
        }
    end subscribeScoped

    /** Subscribes a boundary's content in the current scope, as a region of its own that carries its error view. A boundary with a
      * placeholder puts a [[BoundaryGate]] between its content and the page, and leaves what it shows for a successor at its path when it
      * ends, which a successor under `KeepLatest` shows while its own mounts load.
      */
    private def subscribeBoundary(
        rui: ReactiveUI,
        node: Bounded,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        mounts: MountScope,
        dispatch: MountDispatch,
        pass: MountPass
    )(using Frame): Unit < (Async & Scope) =
        val table                                                         = mounts.child(pass, rui)
        def subscribeContent(through: UIExchange): Unit < (Async & Scope) =
            table.beginWalk(rui.children)
                .map(inner => Kyo.foreachDiscard(rui.children)(subscribeScoped(_, through, signalChangeTime, table, dispatch, inner)))
                .andThen(table.endWalk)
        if !node.settings.waits then subscribeContent(exchange)
        else
            for
                holds <- DrawContext.scope.use(now =>
                    DrawContext.scope.let(now.copy(owner = mounts.table.idBase, rows = rowKeys(rui.path, rui.rows, rui.deepRows)))(
                        Boundaries.catches(node.child, rui.contentContext, dispatch)
                    )
                )
                seed  <- mounts.left(rui.path)
                mutex <- Meter.initMutex(reentrant = true)
                gate = new BoundaryGate(exchange, rui, node, dispatch, mutex, seed, initiallyRevealed = !holds)
                _ <- dispatch.registerBoundary(rui.contentContext, gate)
                _ <- Scope.ensure(gate.close.andThen(dispatch.unregisterBoundary(rui.contentContext, gate)))
                _ <- subscribeContent(gate)
                // Registered after the content, so it runs before the content's own release and still sees it live.
                _ <- Scope.ensure(gate.displayed.map(shown => mounts.leave(rui.path, shown)))
                _ <- gate.started
            yield ()
        end if
    end subscribeBoundary

    /** Bind a lone-text region straight to the backend's text write, skipping the region fiber; `false` means the
      * caller must subscribe it as a normal region.
      *
      * A region lifted from a `Signal[String]` paints one text node and nothing else. The region path runs the
      * full apparatus for it (a fiber, a per-value Scope with its finalizer queue, a re-walk and an HTML render)
      * to arrive at a single `Text.data` write. Binding is the same trade the attribute channels already
      * make (see forkChannelObservers), and for the same reason: the handler is one DOM write that cannot suspend.
      *
      * Three conditions, all necessary:
      *
      *   - the region carries its string signal, so its content is statically one text node;
      *   - the backend offers a synchronous text write (the server transport does not, and stays on the region
      *     path, since its repaint is the equivalent patch);
      *   - the walk found no reactive children under it. A `Text` value cannot produce any, so this is a
      *     guard against a future lift that fills `text` on a region whose content is not only text.
      *
      * The baseline is the render-time string, so an unchanged first emission is dropped before the write, and a
      * change that landed between render and subscribe still fires (`Signal.Unsafe.subscribe` delivers the
      * current value on registration). Release goes to the row's scope or the current Scope (see registerRelease),
      * the scope that would have owned the fiber, because the next-promise is masked and nothing interrupts it.
      */
    private def bindTextRegion(rui: ReactiveUI, exchange: UIExchange, releases: Maybe[RowScope])(using
        Frame
    ): Boolean < (Sync & Scope) =
        (rui.source, exchange.textPatcherNow) match
            case (Present(UI.Ast.Reactive.Source.Lifted(sig)), Present(patch)) if rui.children.isEmpty =>
                val rendered = rui.renderedValue match
                    case Present(t: Text) => Present(t.value)
                    case _                => Absent
                // The sink is read here and not inside the callback: the write fires inside the writer's own
                // `set`, where no effect context exists to read a `Local` from. With none installed the
                // original patcher is used unwrapped, so the off path writes exactly what an unobserved one does.
                Devtools.currentSink.map { devSink =>
                    val write = reporting[String](
                        devSink,
                        cheapEvent(rui.path, ReactiveRegion.htmlIdOf(rui.region), rui.frame, UI.RenderKind.Text)
                    )(v => patch(rui.path, v))
                    Log.use { log =>
                        Sync.Unsafe.defer {
                            // A failing write is logged to the Log bound here: the write runs inside the writer's `set`.
                            val report: Throwable => Unit =
                                ex => log.unsafe.error(s"kyo-ui: text binding failed at path=${rui.path.mkString(".")}", ex)
                            sig.unsafeObserveProjected[String](identity, rendered, write, report) match
                                case Absent           => Kyo.lift(false)
                                case Present(release) => registerRelease(release, releases).andThen(true)
                        }
                    }
                }
            case _ => false
    end bindTextRegion

    /** The report for a write that re-renders nothing: one attribute, one class, or one text node. It carries no duration
      * and no bytes, since a single DOM assignment is not measured on a repaint's axis.
      */
    private def cheapEvent(
        path: Seq[String],
        regionId: Maybe[String],
        frame: Frame,
        kind: UI.RenderKind
    ): UI.RenderEvent =
        UI.RenderEvent(path, regionId, frame, kind, UI.RenderCause.Signal, wasted = false, duration = Duration.Zero, bytes = 0)

    /** `write` that reports `event` after each write, for a write that runs on the writer's stack. With no sink `write` itself comes back, so
      * the off path writes exactly what an unobserved one does.
      */
    private[kyo] def reporting[A](sink: Maybe[UI.RenderEvent => Unit], event: => UI.RenderEvent)(write: A => Unit): A => Unit =
        sink match
            case Absent          => write
            case Present(report) =>
                a =>
                    write(a)
                    report(event)

    /** [[reporting]] for a write that goes over a channel's fiber. */
    private def reportingAsync[A](sink: Maybe[UI.RenderEvent => Unit], event: => UI.RenderEvent)(write: A => Unit < Async)(using
        Frame
    ): A => Unit < Async =
        sink match
            case Absent          => write
            case Present(report) => a => write(a).andThen(Sync.defer(report(event)))

    /** Starts the channel observers of the element a region paints as its root. A walk only gives the root's children nodes of their
      * own, so the root's channels at `path` are started here, in the current scope like the children's subscriptions.
      */
    private[internal] def forkRootChannels(path: Seq[String], content: UI, exchange: UIExchange)(using Frame): Unit < (Async & Scope) =
        content match
            // An element with whileMounted is a node of its own, which binds its channels itself.
            case el: Element if (el.attrs.hasChannels || Channels.follows(el)) && el.attrs.whileMounted.isEmpty =>
                forkChannelObservers(path, Channels.of(el), RenderedChannels.empty, exchange, el.frame, Absent)
            case _ => Kyo.unit

    /** Fork the scoped in-place-patch observers for one node's reactive attr/bool-attr/class channels at
      * `path`. Shared by subscribeScoped (walked nodes) and by forkRootChannels (the painted ROOT element,
      * which is absent from the walk's kids), so the two sites cannot drift apart.
      */
    private def forkChannelObservers(
        path: Seq[String],
        channels: Channels,
        rendered: RenderedChannels,
        exchange: UIExchange,
        // The node these channels belong to, for the devtools report.
        nodeFrame: Frame,
        releases: Maybe[RowScope]
    )(using Frame): Unit < (Async & Scope) =
        // Most nodes bind no channel at all; they leave before the sink is read or a loop is set up.
        if channels.isEmpty then Kyo.unit
        else
            // Read once, at bind time, for the same reason bindTextRegion does: the fast path's write runs inside
            // the writer's `set`, with no effect context to read a `Local` from. With no sink installed both the
            // patcher and the fallback are passed through untouched.
            Devtools.currentSink.map { devSink =>
                // A channel's handler is one attribute write, so where the exchange offers a synchronous sink the
                // channel binds through `Signal.onChange`: for a signal rooted in a SignalRef the patch runs inside the
                // writer's own `set`, and the comparison with the previous value happens before any scheduler hop.
                // An exchange without a sink (the server transport) runs the channel on its own fiber. Both are seeded
                // with the render-time value: normalize happens-before the HTML render on every paint path, so the DOM
                // already shows it, while a change landing between render and subscribe differs from it and still fires.
                def bindAll[A](
                    signals: Map[String, Signal[A]],
                    seen: Map[String, A],
                    patcher: Maybe[(Seq[String], String, A) => Unit],
                    slow: (String, A) => Unit < Async
                )(using CanEqual[A, A]): Unit < (Async & Scope) =
                    if signals.isEmpty then Kyo.unit
                    else
                        Kyo.foreachDiscard(signals.toSeq) { (name, sig) =>
                            val baseline = Maybe.fromOption(seen.get(name))
                            def event    = cheapEvent(path, Absent, nodeFrame, UI.RenderKind.Channel(name))
                            patcher match
                                case Present(patch) =>
                                    bindChannel(sig, baseline, reporting[A](devSink, event)(v => patch(path, name, v)), releases)
                                case Absent =>
                                    val write = reportingAsync[A](devSink, event)(v => slow(name, v))
                                    Fiber.init(sig.observe(baseline, Signal.defaultRepairInterval)(write)).unit
                            end match
                        }
                end bindAll

                /** The inline-style channel. A value is its declarations, and a patch sets them and removes the properties the
                  * previous value had and this one lacks: the previous value is what this observer last wrote, seeded with what
                  * the render wrote, which only the observer knows (the client never saw a value as a value).
                  */
                def bindStyle(sig: Signal[Style]): Unit < (Async & Scope) =
                    given CanEqual[Seq[(String, String)], Seq[(String, String)]] = CanEqual.derived
                    val decls                                                    = sig.map(CssStyleRenderer.declarations)
                    // Unsafe: one channel's own last write, read and replaced in one step by whichever of its writes runs, on
                    // the writer's stack (the synchronous sink) or on its fiber; it never leaves this observer.
                    val last = new java.util.concurrent.atomic.AtomicReference[Seq[(String, String)]](rendered.style.getOrElse(Nil))
                    def step(next: Seq[(String, String)]): Seq[String] =
                        val names = next.iterator.map(_._1).toSet
                        last.getAndSet(next).map(_._1).filterNot(names.contains)
                    def event = cheapEvent(path, Absent, nodeFrame, UI.RenderKind.Channel("style"))
                    exchange.stylePatcherNow match
                        case Present(patch) =>
                            val write = reporting[Seq[(String, String)]](devSink, event)(next => patch(path, next, step(next)))
                            bindChannel(decls, rendered.style, write, releases)
                        case Absent =>
                            val write = reportingAsync[Seq[(String, String)]](devSink, event)(next =>
                                Sync.defer(step(next)).map(exchange.onStylePatch(path, next, _))
                            )
                            Fiber.init(decls.observe(rendered.style, Signal.defaultRepairInterval)(write)).unit
                    end match
                end bindStyle

                bindAll(channels.attrs, rendered.attrs, exchange.attrPatcherNow, exchange.onAttrPatch(path, _, _))
                    .andThen {
                        // An attribute a signal may leave out: set while it holds a value, taken away (the boolean op, off) while not.
                        given CanEqual[Maybe[String], Maybe[String]] = CanEqual.derived
                        val patcher                                  =
                            for
                                set <- exchange.attrPatcherNow
                                off <- exchange.boolAttrPatcherNow
                            yield (p: Seq[String], name: String, v: Maybe[String]) => v.fold(off(p, name, false))(set(p, name, _))
                        bindAll[Maybe[String]](
                            channels.maybeAttrs,
                            rendered.maybeAttrs,
                            patcher,
                            (name, v) => v.fold(exchange.onBoolAttrPatch(path, name, false))(exchange.onAttrPatch(path, name, _))
                        )
                    }
                    .andThen(bindAll(channels.bools, rendered.bools, exchange.boolAttrPatcherNow, exchange.onBoolAttrPatch(path, _, _)))
                    .andThen(bindAll(channels.classes, rendered.classes, exchange.classPatcherNow, exchange.onClassPatch(path, _, _)))
                    .andThen(channels.style.fold(Kyo.unit: Unit < (Async & Scope))(bindStyle))
            }
    end forkChannelObservers

    /** Bind one channel to its synchronous sink. A signal with a callback path runs the write inside the writer's own `set` and
      * registers its release (see registerRelease); one without goes through `Signal.onChange`, which runs it on a fiber of the
      * current Scope.
      */
    private def bindChannel[A](sig: Signal[A], rendered: Maybe[A], write: A => Unit, releases: Maybe[RowScope])(
        using
        CanEqual[A, A],
        Frame
    ): Unit < (Sync & Scope) =
        Log.use { log =>
            Sync.Unsafe.defer {
                // A failing write is logged to the Log bound here, as `onChange` logs one: the write runs inside the writer's `set`.
                val report: Throwable => Unit = ex => log.unsafe.error("kyo-ui: channel binding failed", ex)
                sig.unsafeObserveProjected[A](identity, rendered, write, report) match
                    case Present(release) => registerRelease(release, releases)
                    case Absent           => sig.onChange(rendered)(v => Sync.defer(write(v)))
            }
        }

    /** Where a callback binding's release goes: into the row's scope when one is collecting, else onto the Scope. */
    private def registerRelease(release: () => Unit, releases: Maybe[RowScope])(using Frame): Unit < (Sync & Scope) =
        releases match
            case Present(row) => Sync.Unsafe.defer(row.add(release))
            case Absent       => Scope.ensure(Sync.defer(release()))

    /** How a region or a mount inside a waiting boundary takes its values: each is a pass with a group of its own, and the first one is due
      * from now, so the boundary does not reveal before this has rendered and subscribed what it shows. `shownOf` is what a value paints,
      * `step` shows it through the group.
      */
    private[internal] def throughHold[A](hold: BoundaryHold, rui: ReactiveUI)(shownOf: A => UI)(
        step: (A, UIExchange) => Unit < (Async & Scope)
    )(using Frame): (A => Unit < (Async & Scope)) < (Async & Scope) =
        for
            first <- hold.group
            // Unsafe: read and cleared in one step by this region's own loop, created before that loop is forked.
            due = AtomicRef.Unsafe.init(Maybe(first))(using AllowUnsafe.embrace.danger)
            _ <- Scope.ensure(first.close)
        yield (value: A) =>
            Sync.Unsafe.defer(due.getAndSet(Absent)).map(_.fold(hold.group)(Kyo.lift)).map { group =>
                Scope.ensure(group.close)
                    .andThen(group.paints(BoundaryRange(
                        rui.region,
                        rui.path,
                        rui.contentContext,
                        rui.parentContext,
                        shownOf(value),
                        rui
                    )))
                    .andThen(step(value, group))
                    .andThen(BoundaryHold.duringPass.use(_(rui.path)))
                    .andThen(group.started)
            }

    /** Starts the fiber of a region over a `Signal[UI]`, with a fresh Scope per value: each value is painted, walked, and its
      * reactive children subscribed in that Scope, so the next value or an interrupt tears them down by cascade. `regionMounts` is the
      * region's scope on the mount table, which outlives the subscription (see [[MountTable]]).
      */
    private def subscribeRegion(
        rui: ReactiveUI,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        regionMounts: MountScope,
        dispatch: MountDispatch
    )(using Frame): Unit < (Async & Scope) =
        // The tree this region last rendered, so the exchange can send only what moved instead of the whole region.
        // Unsafe: region-scoped, single-writer state created before the observe loop is forked, where no effect context
        // exists to supply AllowUnsafe; only this region's own loop reads or writes it.
        val rendered = AtomicRef.Unsafe.init(Maybe.empty[UI])(using AllowUnsafe.embrace.danger)
        def subscribeKids(kids: Seq[ReactiveUI], through: UIExchange): Unit < (Async & Scope) =
            regionMounts.beginWalk(kids)
                .map(pass => Kyo.foreachDiscard(kids)(subscribeScoped(_, through, signalChangeTime, regionMounts, dispatch, pass)))
                .andThen(regionMounts.endWalk)
        end subscribeKids
        def renderValue(current: UI, cause: UI.RenderCause, through: UIExchange): Unit < (Async & Scope) =
            for
                now      <- Clock.now
                _        <- signalChangeTime.set(now)
                previous <- Sync.Unsafe.defer(rendered.getAndSet(Present(current)))
                svgContext = rui.region match
                    case _: ReactiveRegion.HtmlRange  => false
                    case _: ReactiveRegion.SvgElement => true
                (newKids, _) <- SignalNow.Locals.take(Sync.Unsafe.defer(DrawContext.inRegion(walkStatic(
                    current,
                    rui.path,
                    svgContext,
                    rui.contentContext,
                    nestedParentContext(rui.parentContext, current),
                    dispatch,
                    rows = rui.rows,
                    deepRows = rui.deepRows,
                    discoverRootBound = rui.discoverContentRootBound
                ))))
                // The pass is opened before the paint, so a mount that starts from the snapshot of the instance before it is painted
                // with that snapshot: one paint, where painting first showed an empty slot until the new instance painted its own.
                (pass, release) <- regionMounts.openWalk(newKids)
                _               <- Scope.ensure(release())
                painted         <- regionMounts.withSeeds(current, rui.path, newKids, pass)
                // What the last value drew here is gone with it, and a place an instance of this table keeps is left to that instance.
                _ <- dispatch.forgetBelow(rui.contentContext)
                _ <- Sync.Unsafe.defer(regionMounts.noteLive(newKids, dispatch))
                _ <- deliverMeasured(rui, cause, through)(
                    _.onChange(rui.region, rui.path, rui.contentContext, rui.parentContext, previous, painted)
                )
                _ <- release()
                _ <- subscribeWalked(rui.path, current, newKids, pass, regionMounts, through, signalChangeTime, dispatch)
            yield ()

        def onValue(current: UI, isFirst: Boolean, through: UIExchange): Unit < (Async & Scope) =
            if isFirst && rui.renderedValue.exists(_.equals(current)) then
                // The enclosing render already painted exactly this value: skip the repaint and the re-walk, but
                // still subscribe what renderValue would have. The value is recorded as this region's last render,
                // so the NEXT emission diffs against it rather than re-sending the whole region.
                //
                // Devtools still hears about it, as the "rebuilt by my parent" half of the story (see reportUnpainted).
                Sync.Unsafe.defer(rendered.set(Present(current)))
                    .andThen(reportUnpainted(rui, UI.RenderKind.Repaint))
                    .andThen(forkRootChannels(rui.path, current, through))
                    .andThen(subscribeKids(rui.children, through))
            else renderValue(current, if isFirst then UI.RenderCause.Created else UI.RenderCause.Signal, through)

        // What the region's fiber listens to. With a source present that is value space: `observe` drops an emission whose
        // value equals the last delivered one, so a record that ticks once a second stops reaching the regions drawing the
        // slices of it that did not move. Without a source (a lifted `Signal[UI]`, where no value exists before the UI) it
        // stays the projected signal and the only available comparison is the UI itself.
        def observed(stamp: Signal.ReadStamp)(paint: UI => Unit < (Async & Scope)): Unit < Async =
            rui.source match
                case Present(src) => src.observe(stamp, rui.renderedSource)(paint)
                case Absent       => rui.signal.observeStamped(stamp)(paint)

        def start(step: (UI, Boolean) => Unit < (Async & Scope)): Unit < (Async & Scope) =
            regionMounts.register.map { watch =>
                regionFiber(rui.path, "Reactive subscription fiber") {
                    var first = true
                    observed(watch.stamp) { current =>
                        val isFirst = first
                        first = false
                        // Each value is a group of its own, released before anything it subscribed is.
                        ReleaseGroup.owning(_ => step(current, isFirst))
                    }
                }
            }

        exchange.gate match
            case Absent        => start(onValue(_, _, exchange))
            case Present(hold) =>
                throughHold[(UI, Boolean)](hold, rui)(_._1)((value, group) => onValue(value._1, value._2, group))
                    .map(held => start((current, isFirst) => held((current, isFirst))))
        end match
    end subscribeRegion

    /** Runs `paint` under a fresh devtools probe, which the backend writes the paint's cost into, and reports it as `kind`.
      * With no sink installed the probe is `Absent`, and nothing is allocated or clocked.
      *
      * `alsoWasted` is for a keyed-list emission, where `wasted` is stricter than "the backend changed nothing": a pure reorder
      * repaints no row and still moves the DOM, and calling that wasted would send a reader looking for a bug that is not there.
      * It is by name because answering it can cost a pass over the rows, and only a measured paint ever asks.
      */
    /** A repaint of `rui`'s range, measured where it reaches the page. Inside a waiting boundary the paint waits in a hold, so it is
      * measured when the hold lets it out, and not at all when the hold drops it: what a dropped paint would have shown is painted
      * later from the live state, and that paint is the one reported.
      */
    private[kyo] def deliverMeasured(rui: ReactiveUI, cause: UI.RenderCause, through: UIExchange)(
        paint: UIExchange => Unit < Async
    )(using Frame): Unit < Async =
        def measured(exchange: UIExchange): Unit < Async = measuredPaint(rui, UI.RenderKind.Repaint, cause)(paint(exchange))
        through.gate match
            case Present(hold) => hold.deliver(measured)
            case Absent        => measured(through)
    end deliverMeasured

    private[internal] def measuredPaint[A, S](rui: ReactiveUI, kind: UI.RenderKind, cause: UI.RenderCause, alsoWasted: => Boolean = false)(
        paint: => A < S
    )(using Frame): A < (S & Sync) =
        for
            probe   <- Devtools.newProbe
            started <- if probe.isEmpty then Kyo.lift(Duration.Zero) else Clock.nowMonotonic
            result  <- Devtools.probe.let(probe)(paint)
            _       <- report(rui, kind, cause, probe, started, alsoWasted)
        yield result
    end measuredPaint

    /** Reports content the enclosing render already painted. Zero duration and zero bytes are not a rounding-off: the cost sits
      * in the enclosing region's own event, and charging it twice would make every parent look cheap and every child expensive.
      */
    private[internal] def reportUnpainted(rui: ReactiveUI, kind: UI.RenderKind)(using Frame): Unit < Sync =
        report(rui, kind, UI.RenderCause.Created, Absent, Duration.Zero, alsoWasted = false)

    /** One render event to the devtools sink; a no-op with none installed. `probe` is `Absent` both when devtools is off and
      * when nothing was painted, which is why the two collapse into the same zero-cost event.
      */
    private[internal] def report(
        rui: ReactiveUI,
        kind: UI.RenderKind,
        cause: UI.RenderCause,
        probe: Maybe[Devtools.Probe],
        started: Duration,
        alsoWasted: => Boolean
    )(using Frame): Unit < Sync =
        def event(wasted: Boolean, duration: Duration, bytes: Int) =
            UI.RenderEvent(rui.path, ReactiveRegion.htmlIdOf(rui.region), rui.frame, kind, cause, wasted, duration, bytes)
        probe match
            case Absent     => Devtools.emit(event(wasted = false, duration = Duration.Zero, bytes = 0))
            case Present(p) =>
                Clock.nowMonotonic.map(ended =>
                    Devtools.emit(event(p.wasted || alsoWasted, ended.minusOrZero(started), p.bytes))
                )
        end match
    end report

    // ---- Shared UI tree utilities (used by both backends) ----

    /** Resolve all reactive signals in a UI tree, returning a static snapshot. */
    def resolveReactives(ui: UI)(using Frame): UI < Sync =
        ui match
            case r: Reactive[?] =>
                for
                    current  <- r.signal.current(using r.frame)
                    resolved <- resolveReactives(current)
                yield resolved
            case fe: Foreach[?, ?] @unchecked =>
                fe.applyTyped {
                    [T] => (signal, keyFn, renderFn) =>
                        for
                            items <- signal.current(using fe.frame)
                            children = items.toSeq.zipWithIndex.map { (item, i) =>
                                val key = keyFn match
                                    case Present(f) => f(item)
                                    case Absent     => i.toString
                                KeyedChild[UI](key, renderFn(i, item))
                            }
                            resolved <- Kyo.foreach(children)(resolveReactives)
                        yield Fragment[UI](Chunk.from(resolved))
                        end for
                }
            case elem: Element =>
                Kyo.foreach(elem.children.toSeq)(resolveReactives).map { resolved =>
                    val resolvedChunk = Chunk.from(resolved)
                    if resolvedChunk == elem.children then ui
                    else rebuildElement(elem, resolvedChunk)
                }
            case Fragment(children) =>
                Kyo.foreach(children.toSeq)(resolveReactives).map(r => Fragment[UI](Chunk.from(r)))
            case KeyedChild(key, child) =>
                resolveReactives(child).map(r => KeyedChild[UI](key, r))
            case _ => ui

    private[kyo] def rebuildElement(elem: Element, newChildren: Chunk[UI]): UI =
        given Frame = elem.frame
        elem match
            case e: Div            => e.copy(children = newChildren)
            case e: SpanElement    => e.copy(children = newChildren)
            case e: P              => e.copy(children = newChildren)
            case e: Section        => e.copy(children = newChildren)
            case e: Main           => e.copy(children = newChildren)
            case e: Header         => e.copy(children = newChildren)
            case e: Footer         => e.copy(children = newChildren)
            case e: Pre            => e.copy(children = newChildren)
            case e: Code           => e.copy(children = newChildren)
            case e: Ul             => e.copy(children = newChildren)
            case e: Ol             => e.copy(children = newChildren)
            case e: Table          => e.copy(children = newChildren)
            case e: H1             => e.copy(children = newChildren)
            case e: H2             => e.copy(children = newChildren)
            case e: H3             => e.copy(children = newChildren)
            case e: H4             => e.copy(children = newChildren)
            case e: H5             => e.copy(children = newChildren)
            case e: H6             => e.copy(children = newChildren)
            case e: Nav            => e.copy(children = newChildren)
            case e: Li             => e.copy(children = newChildren)
            case e: Tr             => e.copy(children = newChildren)
            case e: Form           => e.copy(children = newChildren)
            case e: Label          => e.copy(children = newChildren)
            case e: Button         => e.copy(children = newChildren)
            case e: Td             => e.copy(children = newChildren)
            case e: Th             => e.copy(children = newChildren)
            case e: Select         => e.copy(children = newChildren)
            case e: Opt            => e.copy(children = newChildren)
            case e: Anchor         => e.copy(children = newChildren)
            case e: Svg.SvgElement => rebuildSvgElement(e, newChildren)
            case _                 => elem
        end match
    end rebuildElement

    /** Exhaustive match over all SvgElement concrete classes. NO case _ fallback: a missing arm is
      * a compile error (the kyo-ui build escalates the non-exhaustive-match warning to an error for
      * this file; see build.sbt).
      */
    private def rebuildSvgElement(elem: Svg.SvgElement, newChildren: Chunk[UI]): UI =
        given Frame = elem.frame
        elem match
            case e: Svg.Root           => e.copy(children = newChildren)
            case e: Svg.G              => e.copy(children = newChildren)
            case e: Svg.Defs           => e.copy(children = newChildren)
            case e: Svg.Symbol         => e.copy(children = newChildren)
            case e: Svg.Switch         => e.copy(children = newChildren)
            case e: Svg.SvgAnchor      => e.copy(children = newChildren)
            case e: Svg.Use            => e.copy(children = newChildren)
            case e: Svg.Rect           => e.copy(children = newChildren)
            case e: Svg.Circle         => e.copy(children = newChildren)
            case e: Svg.Ellipse        => e.copy(children = newChildren)
            case e: Svg.Line           => e.copy(children = newChildren)
            case e: Svg.Polyline       => e.copy(children = newChildren)
            case e: Svg.Polygon        => e.copy(children = newChildren)
            case e: Svg.Path           => e.copy(children = newChildren)
            case e: Svg.Text           => e.copy(children = newChildren)
            case e: Svg.TSpan          => e.copy(children = newChildren)
            case e: Svg.TextPath       => e.copy(children = newChildren)
            case e: Svg.LinearGradient => e.copy(children = newChildren)
            case e: Svg.RadialGradient => e.copy(children = newChildren)
            case e: Svg.Stop           => e.copy(children = newChildren)
            case e: Svg.Pattern        => e.copy(children = newChildren)
            case e: Svg.ClipPath       => e.copy(children = newChildren)
            case e: Svg.Mask           => e.copy(children = newChildren)
            case e: Svg.Image          => e.copy(children = newChildren)
            case e: Svg.ForeignObject  => e.copy(children = newChildren)
            case e: Svg.Marker         => e.copy(children = newChildren)
            case e: Svg.Title          => e.copy(children = newChildren)
            case e: Svg.Desc           => e.copy(children = newChildren)
            case e: Svg.Metadata       => e.copy(children = newChildren)
            // filter family: only Filter and FeMerge carry reactive children; the rest are leaves.
            case e: Svg.Filter            => e.copy(children = newChildren)
            case e: Svg.FeGaussianBlur    => e
            case e: Svg.FeOffset          => e
            case e: Svg.FeBlend           => e
            case e: Svg.FeColorMatrix     => e
            case e: Svg.FeFlood           => e
            case e: Svg.FeComposite       => e
            case e: Svg.FeMerge           => e.copy(children = newChildren)
            case e: Svg.FeMergeNode       => e
            case e: Svg.FeImage           => e
            case e: Svg.FeTile            => e
            case e: Svg.FeMorphology      => e
            case e: Svg.FeTurbulence      => e
            case e: Svg.FeDisplacementMap => e
            // SMIL family: all leaves.
            case e: Svg.Animate          => e
            case e: Svg.AnimateTransform => e
            case e: Svg.AnimateMotion    => e
            case e: Svg.SetAnim          => e
        end match
    end rebuildSvgElement

    /** Find the path to an element by ID in a resolved UI tree. Returns the dot-separated path string. */
    def findPathById(ui: UI, id: String, currentPath: String = ""): Maybe[String] =
        def searchChildren(children: Chunk[UI]): Maybe[String] =
            @scala.annotation.tailrec
            def loop(i: Int): Maybe[String] =
                if i >= children.size then Absent
                else
                    val childPath = if currentPath.isEmpty then i.toString else s"$currentPath.$i"
                    val child     = children(i) match
                        case kc: KeyedChild[?] => kc.child
                        case c                 => c
                    findPathById(child, id, childPath) match
                        case Absent  => loop(i + 1)
                        case present => present
            loop(0)
        end searchChildren
        ui match
            case elem: Element =>
                if elem.attrs.identifier == Present(id) then Present(currentPath)
                else searchChildren(elem.children)
            case Fragment(children) => searchChildren(children)
            case _                  => Absent
        end match
    end findPathById

    /** Find element by ID in a resolved UI tree. */
    def findElementById(ui: UI, id: String): Maybe[Element] =
        def searchChildren(children: Chunk[UI]): Maybe[Element] =
            @scala.annotation.tailrec
            def loop(i: Int): Maybe[Element] =
                if i >= children.size then Absent
                else
                    findElementById(children(i), id) match
                        case Absent  => loop(i + 1)
                        case present => present
            loop(0)
        end searchChildren
        ui match
            case elem: Element =>
                if elem.attrs.identifier == Present(id) then Present(elem)
                else searchChildren(elem.children)
            case Fragment(children)   => searchChildren(children)
            case KeyedChild(_, child) => findElementById(child, id)
            case _                    => Absent
        end match
    end findElementById

    /** Find element at a dot-separated path in the UI tree. */
    @scala.annotation.tailrec
    def findAtPath(ui: UI, path: String): UI =
        if path.isEmpty then ui
        else
            val dotIdx  = path.indexOf('.')
            val segment = if dotIdx < 0 then path else path.substring(0, dotIdx)
            val rest    = if dotIdx < 0 then "" else path.substring(dotIdx + 1)
            childAt(ui, segment) match
                case Present(child) => findAtPath(child, rest)
                case Absent         => ui

    private def childAt(ui: UI, segment: String): Maybe[UI] =
        ui match
            case elem: Element =>
                Maybe.fromOption(segment.toIntOption) match
                    case Present(i) if i >= 0 && i < elem.children.size =>
                        elem.children(i) match
                            case kc: KeyedChild[?] => Present(kc.child)
                            case child             => Present(child)
                    case _ => Absent
            case Fragment(children) =>
                val keyIdx = Maybe.fromOption(children.toSeq.zipWithIndex.collectFirst {
                    case (kc: KeyedChild[?], _) if kc.key == segment => kc.child
                })
                keyIdx match
                    case Present(child) => Present(child)
                    case Absent         =>
                        Maybe.fromOption(segment.toIntOption) match
                            case Present(i) if i >= 0 && i < children.size =>
                                children(i) match
                                    case kc: KeyedChild[?] => Present(kc.child)
                                    case child             => Present(child)
                            case _ => Absent
                end match
            case _ => Absent

end ReactiveUI
