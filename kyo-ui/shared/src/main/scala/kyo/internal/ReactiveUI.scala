package kyo.internal

import kyo.*
import kyo.Svg
import kyo.UI.*
import kyo.kernel.ContextEffect

/** Normalized reactive UI node. */
private[kyo] case class ReactiveUI(
    path: Seq[String],
    signal: Signal[UI],
    isConst: Boolean,
    children: Seq[ReactiveUI],
    handle: (Seq[String], UIEvent) => Boolean < Async,
    region: ReactiveRegion,
    contentContext: ReactiveRegion.RegionIdentity,
    parentContext: ReactiveRegion.ParentContext,
    discoverContentRootBound: Boolean,
    mounted: Maybe[MountedSpec] = Absent,
    mountDispatch: Maybe[MountDispatch] = Absent,
    // The call site of the node a region comes from and the row-key depths of its path since the enclosing region (see `rowKeys`):
    // what places a region's mount table, independently of the region's position among its siblings.
    origin: Maybe[Frame] = Absent,
    rows: Long = 0L,
    deepRows: List[Int] = Nil,
    // Reactive channels carried from the element's Attrs; subscribeScoped forks the in-place-patch observers
    // (empty for non-element / attribute-less nodes).
    reactiveAttrs: Map[String, Signal[String]] = Map.empty,
    reactiveBoolAttrs: Map[String, Signal[Boolean]] = Map.empty,
    reactiveClasses: Map[String, Signal[Boolean]] = Map.empty,
    // Render-time snapshots: the values the enclosing paint already put into the DOM. Subscribe skips an observer's first
    // emission while it still equals its snapshot, so a fresh paint is not patched again.
    renderedValue: Maybe[UI] = Absent,
    renderedAttrValues: Map[String, String] = Map.empty,
    renderedBoolAttrValues: Map[String, Boolean] = Map.empty,
    renderedClassValues: Map[String, Boolean] = Map.empty,
    // Present on Foreach regions: carries the AST node (signal, key, render) and the normalize-time items
    // snapshot so subscribe can run the per-row reuse path instead of the whole-region re-render loop.
    foreachSpec: Maybe[ForeachSpec] = Absent,
    // Present on a region whose node kept the signal before its projection to UI (UI.Ast.Reactive.source). Subscribe observes
    // that, so an emission whose value is unchanged never re-renders a subtree that can only be compared as UI; a source lifted
    // from a Signal[String] is bound straight to the backend's text write instead (see bindTextRegion).
    source: Maybe[UI.Ast.Reactive.Source[?]] = Absent,
    // The AST node's own `Frame`, so [[Devtools]] can name a region by its user call site rather than its path.
    frame: Frame = Frame.internal
)

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
    rows: ReactiveUI.RowRegistry
):
    def reusable: Boolean = node.key.nonEmpty && !node.indexed
end ForeachSpec

/** Where event dispatch finds a mounted node's live content.
  *
  * Dispatch re-derives handler chains from the current UI on every event, so the `Mounted` node it meets is a fresh value that cannot
  * carry the instance. The instance registers its content cell here by path while it is subscribed, and the dispatch-time handler of
  * whatever node occupies that path reads it. One table per normalized root.
  */
final private[kyo] class MountDispatch(cells: AtomicRef[Dict[Seq[String], Chunk[Signal[ReactiveUI.MountState]]]]):
    def register(path: Seq[String], cell: Signal[ReactiveUI.MountState])(using Frame): Unit < Sync =
        cells.getAndUpdate(m => m.update(path, m.get(path).getOrElse(Chunk.empty).append(cell))).unit

    /** Removes this registration only. Two subscriptions can hold one path for a moment, an old one still ending and its successor,
      * in either order; the other keeps its entry.
      */
    def unregister(path: Seq[String], cell: Signal[ReactiveUI.MountState])(using Frame): Unit < Sync =
        cells.getAndUpdate { m =>
            m.get(path) match
                case Absent        => m
                case Present(held) =>
                    val at   = held.lastIndexWhere(_ eq cell)
                    val left = if at < 0 then held else held.take(at).concat(held.drop(at + 1))
                    if left.isEmpty then m.remove(path) else m.update(path, left)
        }.unit

    /** The cell registered last at `path`. */
    def lookup(path: Seq[String])(using Frame): Maybe[Signal[ReactiveUI.MountState]] < Sync =
        cells.get.map(_.get(path).flatMap(_.lastMaybe))
end MountDispatch

private[kyo] object MountDispatch:
    def init(using Frame): MountDispatch < Sync = Sync.Unsafe.defer(initNow())

    /** The table for a pass that already holds the thread (see [[SignalNow]]). */
    def initNow()(using AllowUnsafe): MountDispatch =
        new MountDispatch(AtomicRef.Unsafe.init(Dict.empty[Seq[String], Chunk[Signal[ReactiveUI.MountState]]]).safe)
end MountDispatch

/** A `Mounted` node as normalization found it: the node, its render path, and the keys of the keyed list rows between it and its
  * enclosing reactive region, innermost first.
  */
final private[kyo] case class MountedSpec(node: UI.Ast.Mounted, path: Seq[String], rows: List[String]):
    def placeholder: UI = node.placeholderUI.getOrElse(UI.empty(using node.frame))

    def errorUI(error: UI.MountError): UI =
        node.errorRender match
            case Present(render) => render(error)
            case Absent          =>
                given Frame = node.frame
                UI.span(UI.Ast.Text(error.message)).cssClass("kyo-mount-error")

    def mountError(t: Throwable, source: UI.MountErrorSource): UI.MountError =
        UI.MountError(t, source, Maybe(t.getMessage).getOrElse(t.toString), node.frame, path, node.key)

    /** The instance identity of a keyed node; a keyless node has none, since every re-render replaces its instance. */
    def identity: Maybe[ReactiveUI.MountIdentity] =
        node.key.map(ReactiveUI.MountIdentity(rows, node.site, _))
end MountedSpec

private[kyo] object ReactiveUI:

    import UI.Ast.*

    /** The app's sink for non-fatal failures, read by [[kyo.UI.notify]] and installed by
      * [[kyo.UI.notices]]. Inheritable, so it reaches mount effects, the fibers they fork and the
      * event handlers of the tree below the installation point, all of which descend from the runner's
      * caller.
      */
    private[kyo] val noticeSink: Local[Maybe[Throwable => Unit < Async]] =
        Local.init(Maybe.empty[Throwable => Unit < Async])

    def init(
        path: Seq[String],
        signal: Signal[UI],
        isConst: Boolean,
        children: Seq[ReactiveUI],
        svgContext: Boolean,
        regionIdentity: ReactiveRegion.RegionIdentity,
        contentContext: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        origin: Maybe[Frame] = Absent,
        rows: Long = 0L,
        deepRows: List[Int] = Nil
    )(
        handle: (Seq[String], UIEvent) => Boolean < Async
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
            origin = origin,
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
        Sync.Unsafe.defer {
            val dispatch = MountDispatch.initNow()
            val root     = normalizeAt(
                ui,
                rootPath,
                svg,
                ReactiveRegion.RegionIdentity.root(rootPath),
                ReactiveRegion.ParentContext.Other,
                dispatch,
                rows = 0L,
                deepRows = Nil
            )
            root.copy(mountDispatch = Present(dispatch))
        }
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
            renderedAttrValues = Map.empty,
            renderedBoolAttrValues = Map.empty,
            renderedClassValues = Map.empty
        )

    /* Normalization and the tree walk carry, for the mounted nodes and regions they meet, the session's dispatch table and which
     * depths of the path, since the enclosing reactive region, hold the key of a keyed list row: a bit per depth below 63 in `rows`,
     * and the rare deeper ones in `deepRows`. Both are read out of the path only when a mount or a region's table needs them, so the
     * walk itself allocates nothing for them.
     */

    /** The row keys `rows` and `deepRows` mark in `path`, innermost first. */
    private def rowKeys(path: Seq[String], rows: Long, deepRows: List[Int]): List[String] =
        if rows == 0L && deepRows.isEmpty then Nil
        else
            path.iterator.zipWithIndex.foldLeft(List.empty[String]) { case (acc, (segment, depth)) =>
                val isRow = if depth < 63 then (rows & (1L << depth)) != 0L else deepRows.contains(depth)
                if isRow then segment :: acc else acc
            }

    private def normalizeAt(
        ui: UI,
        path: Seq[String],
        svg: Boolean,
        regionIdentity: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        cells: MountDispatch,
        rows: Long,
        deepRows: List[Int]
    )(using AllowUnsafe): ReactiveUI =
        given Frame = ui.frame
        ui match
            case ui: Reactive[?] =>
                val contentContext       = regionIdentity.transparent
                val current              = SignalNow(ui.signal)
                val contentParentContext = nestedParentContext(parentContext, current)
                val (kids, _) = walkStatic(current, path, svg, contentContext, contentParentContext, cells, rows = 0L, deepRows = Nil)
                init(
                    path,
                    ui.signal,
                    isConst = false,
                    kids,
                    svg,
                    regionIdentity,
                    contentContext,
                    parentContext,
                    Present(ui.frame),
                    rows,
                    deepRows
                ) {
                    (targetPath, event) =>
                        Sync.Unsafe.defer {
                            val currentUI            = SignalNow(ui.signal)
                            val contentParentContext = nestedParentContext(parentContext, currentUI)
                            val (_, freshHdl)        =
                                walkStatic(currentUI, path, svg, contentContext, contentParentContext, cells, rows = 0L, deepRows = Nil)
                            freshHdl(targetPath, event)
                        }
                }.copy(renderedValue = Present(current), source = ui.source)

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
                            walkStatic(current, path, svg, contentContext, contentParentContext, cells, rows = 0L, deepRows = Nil)
                        // Fallback dispatch: re-render the whole list from the current value and route
                        // through the fresh fragment handler. Used whenever the row registry cannot
                        // resolve the target (region not subscribed on the reuse path, row painted but
                        // not yet registered, or an event addressed to the region node itself).
                        val fullWalk: Handler = (targetPath, event) =>
                            Sync.Unsafe.defer {
                                val currentUI          = SignalNow(sig)
                                val freshParentContext = nestedParentContext(parentContext, currentUI)
                                val (_, freshHdl)      =
                                    walkStatic(currentUI, path, svg, contentContext, freshParentContext, cells, rows = 0L, deepRows = Nil)
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
                            Present(ui.frame),
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
                                    registry.snapshot.map { live =>
                                        live.find(_.key == targetPath(path.size)) match
                                            case Some(inst) =>
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
                                                                cells
                                                            )._2(targetPath, event)
                                                        }
                                            case None => fullWalk(targetPath, event)
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
                // A constant element belongs to the enclosing region; a bound one is a region of its own.
                val contentRows     = if isConstNode then rows else 0L
                val contentDeepRows = if isConstNode then deepRows else Nil
                val (kids, hdl)     = walkStatic(
                    ui,
                    path,
                    svg,
                    regionIdentity,
                    parentContext,
                    cells,
                    contentRows,
                    contentDeepRows,
                    discoverRootBound = false
                )
                val attrSnap  = currentValues(ui.attrs.reactiveAttrs)
                val boolSnap  = currentValues(ui.attrs.reactiveBoolAttrs)
                val classSnap = currentValues(ui.attrs.reactiveClasses)
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
                    origin = Present(ui.frame),
                    rows = rows,
                    deepRows = deepRows,
                    reactiveAttrs = ui.attrs.reactiveAttrs,
                    reactiveBoolAttrs = ui.attrs.reactiveBoolAttrs,
                    reactiveClasses = ui.attrs.reactiveClasses,
                    renderedAttrValues = attrSnap,
                    renderedBoolAttrValues = boolSnap,
                    renderedClassValues = classSnap,
                    frame = ui.frame
                )

            case ui: Mounted =>
                val spec           = MountedSpec(ui, path, rowKeys(path, rows, deepRows))
                val contentContext = regionIdentity.transparent
                // The node value is fresh on every dispatch, so the handler finds the live instance through the dispatch table.
                val handle: Handler = (targetPath, event) =>
                    cells.lookup(path).map {
                        case Absent        => true
                        case Present(cell) =>
                            cell.current.map {
                                case MountState.Pending(Present(_)) => true
                                case state                          =>
                                    val content = state.content(spec)
                                    Sync.Unsafe.defer {
                                        val (_, freshHdl) = walkStatic(
                                            content,
                                            path,
                                            svg,
                                            contentContext,
                                            nestedParentContext(parentContext, content),
                                            cells,
                                            rows = 0L,
                                            deepRows = Nil
                                        )
                                        freshHdl(targetPath, event)
                                    }
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

    private def nestedParentContext(parentContext: ReactiveRegion.ParentContext, current: UI): ReactiveRegion.ParentContext =
        (parentContext, ReactiveRegion.tableContent(current)) match
            case (ReactiveRegion.ParentContext.HtmlTable, ReactiveRegion.TableContent.Rows) => ReactiveRegion.ParentContext.Other
            case _                                                                          => parentContext

    /** True when a child UI needs its own ReactiveUI node during a static walk: reactive nodes always, and an
      * element whose ROOT carries a binding (a bound input ref or any reactive attr/bool-attr/class channel).
      * Shared by the element-children and the fragment-children walk so the promotion rule cannot diverge
      * between them.
      */
    private def needsOwnNode(ui: UI): Boolean = ui match
        case _: Reactive[?] | _: Foreach[?, ?] | _: Mounted => true
        case el: Element                                    =>
            collectSignalRef(el).nonEmpty || el.attrs.reactiveAttrs.nonEmpty
            || el.attrs.reactiveBoolAttrs.nonEmpty || el.attrs.reactiveClasses.nonEmpty
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
    private def walkRow(
        rowUI: UI,
        rowPath: Seq[String],
        svg: Boolean,
        context: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        cells: MountDispatch
    )(using AllowUnsafe, Frame): (Seq[ReactiveUI], Handler) =
        val depth    = rowPath.size - 1
        val rows     = if depth < 63 then 1L << depth else 0L
        val deepRows = if depth < 63 then Nil else List(depth)
        if needsOwnNode(rowUI) then
            val rui = normalizeAt(rowUI, rowPath, svg, context, parentContext, cells, rows, deepRows)
            (Seq(rui), rui.handle)
        else walkStatic(rowUI, rowPath, svg, context, parentContext, cells, rows, deepRows)
        end if
    end walkRow

    private type Handler = (Seq[String], UIEvent) => Boolean < Async

    private type ValidatedHandler = (Seq[String], DragProtocol.ValidatedEvent) => Boolean < Async

    private type DragResolver = Maybe[(String, Drag.Decision) => Unit < Async]

    final private case class DragSession(
        event: Drag.Event,
        terminalResolved: Boolean,
        /** A reading of [[Clock.nowMonotonic]]. */
        expiresAt: Duration,
        resolver: DragResolver
    )

    final private[kyo] case class DragSessionLimits(
        maxSessions: Int = 128,
        lifetime: Duration = 5.minutes
    ):
        require(maxSessions > 0, s"DragSessionLimits.maxSessions must be positive: $maxSessions")
        require(lifetime > Duration.Zero, s"DragSessionLimits.lifetime must be positive: $lifetime")
    end DragSessionLimits

    /** The current value of every bound signal in `channels`, by name. Most elements bind none, so an empty
      * map answers without a loop.
      */
    private def currentValues[A](channels: Map[String, Signal[A]])(using AllowUnsafe, Frame): Map[String, A] =
        if channels.isEmpty then Map.empty[String, A]
        else
            val b = Map.newBuilder[String, A]
            channels.foreach((n, s) => b += n -> SignalNow(s))
            b.result()

    /** Walk a static UI tree. Collect reactive children, build handle. */
    private def walkStatic(
        ui: UI,
        basePath: Seq[String],
        svg: Boolean = false,
        context: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        cells: MountDispatch,
        rows: Long,
        deepRows: List[Int],
        discoverRootBound: Boolean = true
    )(using AllowUnsafe, Frame): (Seq[ReactiveUI], Handler) =
        ui match
            case elem: Element if discoverRootBound && collectSignalRef(elem).nonEmpty =>
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
                val handle: Handler                     = (targetPath, event) =>
                    dispatch(elem, basePath, targetPath, event, reactiveChildren, staticHandlers, cells)
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
                    val innerDeepRows = if keyed && depth >= 63 then depth :: deepRows else deepRows
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
                            idx.filter(i => i >= 0 && i < allHandles.length).map(allHandles(_))
                                .fold(true: Boolean < Async)(h => h(targetPath, event))
                        else true
                    (allKids, handle)
                }

            case _: Reactive[?] | _: Foreach[?, ?] | _: Mounted =>
                // When walkStatic is called with a Reactive, Foreach or Mounted as the top-level node
                // (not as a child of an Element), normalize it at basePath so subscribeNode
                // sets up a subscription for it. This handles the case where an outer reactive's
                // signal value is itself a Reactive or Foreach (e.g. outer.map { _ => inner.map(UI.span(_)) }).
                val rui = normalizeAt(ui, basePath, svg, context, parentContext, cells, rows, deepRows)
                (Seq(rui), rui.handle)

            case _ =>
                val noHandle: Handler = (_, _) => true
                (Seq.empty[ReactiveUI], noHandle)

    /** Dispatch an event through an element. */
    private def dispatch(
        elem: Element,
        myPath: Seq[String],
        targetPath: Seq[String],
        event: UIEvent,
        reactiveChildren: Seq[ReactiveUI],
        staticHandlers: Seq[(Int, Handler)],
        mounts: MountDispatch
    )(using Frame): Boolean < Async =
        if targetPath == myPath then
            dispatchToElement(elem, event, isTarget = true, selfPath = myPath)
        else if targetPath.startsWith(myPath) && targetPath.size > myPath.size then
            val childSegment = targetPath(myPath.size)
            val childIdx     = childSegment.toIntOption

            // Find the reactive child whose path is a prefix of (or equals) targetPath. Matching by
            // `lastOption.contains(childSegment)` is incorrect when reactive children are nested
            // multiple levels deep; their last segment may collide with a sibling's index at the
            // current level. Using prefix-match correctly routes only when the target lies inside.
            val reactiveChild = reactiveChildren.find(rc => targetPath.startsWith(rc.path))
            // `reactiveChildren` also holds reactive descendants of static children. A target inside a static child goes
            // through that child's handler chain, so an element between this one and the region (a button around a
            // reactive label) still sees the event; only a reactive DIRECT child, which has no static handler, is entered
            // directly.
            val staticChild = Maybe.fromOption(childIdx).flatMap(i => Maybe.fromOption(staticHandlers.find(_._1 == i)).map(_._2))

            val isClick   = event.isInstanceOf[UIEvent.Click]
            val isKeyDown = event.isInstanceOf[UIEvent.KeyDown]
            for
                // Disabled-target (Form submit suppression), Button-target (only Button clicks submit
                // forms), Select-target (Enter on Select must not submit) and control-below all resolve
                // through any Reactive/Foreach boundary wrapping the target, so signal-typed setters do
                // not hide it. All four come out of one resolution: they are four questions about one
                // element, not four searches for it.
                facts <-
                    if isClick || isKeyDown then targetFacts(elem, myPath, targetPath, mounts)
                    else Kyo.lift(TargetFacts.none)
                // "Is it disabled" is a question about the current value behind a channel, not about a
                // field (see boolAttrNow), so it is read from the resolved element rather than carried.
                targetDisabled <-
                    if isClick then facts.target.fold(Kyo.lift(false))(isDisabled) else Kyo.lift(false)
                // A button's type decides whether activating it submits, not its element kind:
                // `type="button"` submits nothing in a browser (see ButtonActivation). `Absent` here
                // means the target is no button at all, which is not the same as "a button that does
                // not submit".
                targetButtonSubmits = facts.target match
                    case Present(b: Button) => Present(ButtonActivation.submits(b.attrs.jsProps.getOrElse("type", "")))
                    case _                  => Absent
                // Enter submits a form implicitly from a single-line field or a button, as a browser does; from a textarea it is a
                // line break, from a select a dropdown interaction, and from anything else nothing at all.
                targetEnterSubmits = isKeyDown && facts.target.exists { t =>
                    val tag = HtmlRenderer.tagName(t)
                    tag == "input" || tag == "button"
                }
                // Whether a control of the reader's own stands between this element and the target,
                // reported to the handler rather than acted on here: only the handler knows whether
                // its click means something the control already means (see UI.MouseEvent.onControl).
                targetOnControl = isClick && facts.controlBelow
                bubble          = dispatchToElement(
                    elem,
                    event,
                    isTarget = false,
                    selfPath = myPath,
                    disabledTarget = targetDisabled,
                    submitOrigin = isClick && targetButtonSubmits.getOrElse(false),
                    enterSubmits = targetEnterSubmits,
                    controlTarget = targetOnControl
                )
                result <- staticChild match
                    case Present(childHandle) =>
                        safeDispatch(childHandle, targetPath, event).map(keep => if keep then bubble else false)
                    case Absent =>
                        Maybe.fromOption(reactiveChild).fold(bubble)(child =>
                            safeDispatch(child.handle, targetPath, event).map(keep => if keep then bubble else false)
                        )
            yield result
            end for
        else
            true

    /** Where a keydown records the element whose click it stands for, read by [[subscribe]]'s wrapper once
      * the keydown has finished bubbling.
      *
      * HTML has exactly one route to a form submit: an activation behaviour, which runs as part of a
      * click. Keyboard activation of a button is a click the browser fires, and Enter in a field is
      * implicit submission, which the specification also defines as firing a click, at the form's
      * default button. Neither is "submit on a key". Recording a path here and dispatching a real click
      * at it afterwards keeps activation on that one path: an ancestor sees a keyboard activation
      * exactly as it sees a mouse click, and a form submits from that click.
      */
    private val activationTarget: Local[Maybe[AtomicRef[Maybe[Seq[String]]]]] = Local.init(Absent)

    /** Record that this keydown's activation is a click at `path`, unless one is already recorded.
      *
      * First writer wins, and the order is the bubble order: the target runs before its ancestors, so a
      * button's own activation is what a form sees rather than the form's implicit submission.
      */
    private def recordActivation(path: Seq[String])(using Frame): Unit < Async =
        activationTarget.use {
            case Present(ref) => ref.updateAndGet(existing => if existing.isDefined then existing else Present(path)).unit
            case Absent       => Kyo.lift(())
        }

    /** The form's default button: the first button in tree order whose `type` submits, as HTML defines it.
      *
      * Only the statically known children are walked. A button behind a signal is not found, and the
      * caller falls back to submitting the form directly, the answer HTML gives a form with no submit
      * button at all.
      */
    private def defaultSubmitButton(elem: Element, basePath: Seq[String]): Maybe[Seq[String]] =
        val start: Maybe[Seq[String]] = Absent
        elem.children.toSeq.zipWithIndex.foldLeft(start) { case (found, (child, i)) =>
            if found.isDefined then found
            else
                val childPath = basePath :+ i.toString
                child match
                    case b: Button if ButtonActivation.submits(b.attrs.jsProps.getOrElse("type", "")) =>
                        Present(childPath)
                    case c: Element => defaultSubmitButton(c, childPath)
                    case _          => Absent
                end match
        }
    end defaultSubmitButton

    /** Submit the form itself, HTML's fallback when implicit submission finds no default button to click.
      *
      * Skipped when the keydown already recorded an activation: that click reaches this form on its own
      * and submitting here as well would be the double all over again.
      */
    private def submitDirectly(f: Form)(using Frame): Unit < Async =
        activationTarget.use {
            case Present(ref) => ref.get.map(recorded => if recorded.isDefined then Kyo.lift(()) else invoke(f.onSubmit))
            case Absent       => invoke(f.onSubmit)
        }

    /** Invoke a Maybe[Any < Async] handler, discarding the result. */
    private def invoke(handler: Maybe[Any < Async])(using Frame): Unit < Async =
        if handler.isEmpty then () else handler.get.unit

    /** Invoke a Maybe[A => Any < Async] handler with a value, discarding the result. */
    private def invokeWith[A](handler: Maybe[A => Any < Async], value: A)(using Frame): Unit < Async =
        if handler.isEmpty then () else handler.get(value).unit

    /** Dispatch an event safely, logging and recovering from handler errors without stopping the bubble chain. */
    private def safeDispatch(handle: (Seq[String], UIEvent) => Boolean < Async, path: Seq[String], event: UIEvent)(using
        Frame
    ): Boolean < Async =
        DragCommands.current.use { context =>
            val decisions = context.flatMap(_.decisions)
            Abort.recover[Throwable](
                onFail = err =>
                    Log.error(s"Handler error during ${event.getClass.getSimpleName} at ${path.mkString(".")}: ${err.getMessage}")
                        .andThen(markDecisionFailure(event, decisions))
                        .andThen(true), // continue bubbling
                onPanic = thr =>
                    Log.error(s"Handler panic during ${event.getClass.getSimpleName} at ${path.mkString(".")}: ${thr.getMessage}")
                        .andThen(markDecisionFailure(event, decisions))
                        .andThen(true) // continue bubbling
            )(handle(path, event))
        }

    private def markDecisionFailure(event: UIEvent, decisions: Maybe[AtomicRef[DragCommands.DecisionState]])(using
        Frame
    ): Unit < Async =
        event match
            case _: UIEvent.Drop     => failDecision(decisions, "The drop handler failed.")
            case _: UIEvent.SortMove => failDecision(decisions, "The sort handler failed.")
            case _                   => ()

    /** Read current checked value from Maybe[Bound[Boolean]]. */
    private def readChecked(checked: Maybe[Bound[Boolean]])(using Frame): Boolean < Sync =
        checked match
            case Present(Bound.Const(b)) => b
            case Present(Bound.Ref(ref)) => ref.get
            case _                       => false

    /** Toggle checked and auto-set SignalRef if bound. Returns the new value. */
    private def toggleChecked(checked: Maybe[Bound[Boolean]])(using Frame): Boolean < Sync =
        checked match
            case Present(Bound.Ref(ref)) =>
                ref.getAndUpdate(!_).map(!_)
            case Present(Bound.Const(b)) => !b
            case _                       => true

    /** Cycle a Select to the next or previous option. Updates SignalRef and fires onChange. */
    private def cycleSelectOption(sel: Select, forward: Boolean)(using Frame): Unit < Async =
        val options = sel.children.toSeq.collect { case opt: Opt => opt }
        val values  = options.flatMap(_.value.toList)
        if values.isEmpty then Kyo.lift(())
        else
            for
                current <- sel.value match
                    case Present(Bound.Ref(ref)) => ref.get
                    case Present(Bound.Const(s)) => Kyo.lift(s)
                    case _                       => Kyo.lift("")
                currentIdx = values.indexOf(current).max(0)
                nextIdx    =
                    if forward then (currentIdx + 1)    % values.size
                    else (currentIdx - 1 + values.size) % values.size
                newValue = values(nextIdx)
                _ <- sel.value match
                    case Present(Bound.Ref(ref)) => ref.set(newValue)
                    case _                       => Kyo.lift(())
                _ <- invokeWith(sel.onChange, newValue)
            yield ()
        end if
    end cycleSelectOption

    private def formatDouble(v: Double): String = NumberFormat.double(v)

    /** Is the boolean attribute `name` set on `elem` RIGHT NOW, statically or through the reactive channel a
      * `Signal`-typed setter installs.
      *
      * `.disabled(Signal)` and `.readOnly(Signal)` do not wrap their element and do not fill the static field:
      * they register a boolean-attribute channel (`Attrs.reactiveBoolAttrs`) that patches the live attribute
      * in place. A guard reading only the static field therefore counts every signal-disabled element as
      * enabled, and silently, because a browser suppresses the click on a rendered `disabled` button, so it shows
      * only where dispatch decides for itself: the server transport, and any forged event.
      *
      * WHERE A CHANNEL EXISTS, THE CHANNEL IS THE VALUE: the static field is not consulted at all. Same rule
      * the renderer follows (`renderElementAttrs`), and the only one consistent with the client, whose channel
      * patch sets and REMOVES the attribute on every emission whatever the initial HTML said.
      */
    private def boolAttrNow(elem: Element, name: String, static: => Boolean)(using Frame): Boolean < Sync =
        elem.attrs.reactiveBoolAttrs.get(name) match
            case Some(sig) => sig.current
            case None      => static

    /** Check if element is disabled. */
    private def isDisabled(elem: Element)(using Frame): Boolean < Sync =
        boolAttrNow(
            elem,
            "disabled",
            elem match
                case hd: HasDisabled => hd.disabled.getOrElse(false)
                case _               => false
        )

    /** Check if element is readOnly. The channel name is the ATTRIBUTE spelling (`readonly`), not the setter's. */
    private def isReadOnly(elem: Element)(using Frame): Boolean < Sync =
        boolAttrNow(
            elem,
            "readonly",
            elem match
                case ti: TextInput => ti.readOnly.getOrElse(false)
                case _             => false
        )

    /** A TARGET that is inert (disabled, hidden, read-only) ignores its own handler but still lets the event
      * bubble, so this yields `true` (keep bubbling) without running `body`. Only the target is guarded: an
      * ancestor on the bubble path is not inert just because the target was.
      */
    private def unlessInert(isTarget: Boolean, inert: => Boolean < Sync)(body: => Boolean < Async)(using
        Frame
    ): Boolean < Async =
        if !isTarget then body
        else inert.map(i => if i then true else body)

    /** Check if element is hidden. Reads the channel too: `.hidden(Signal)` installs a boolean-attribute
      * channel exactly like `.disabled(Signal)`, so the static field is empty whenever the signal form was used.
      */
    private def isHidden(elem: Element)(using Frame): Boolean < Sync =
        boolAttrNow(elem, "hidden", elem.attrs.hidden.getOrElse(false))

    /** What a bubbling dispatch needs to know about the event's target, resolved once.
      *
      * @param target
      *   the concrete element the path ends at, `Absent` when it does not resolve
      * @param controlBelow
      *   whether any element strictly below the dispatching one, the target included, is a control of
      *   the reader's own
      */
    final private case class TargetFacts(target: Maybe[Element], controlBelow: Boolean)

    private object TargetFacts:
        /** Nothing was asked, so nothing was found: the answer for an event that resolves no target. */
        val none: TargetFacts = TargetFacts(Absent, false)

    /** Resolve the (possibly reactive) node at `targetPath` to the concrete element there, noting on the way
      * whether the path passed through a control.
      *
      * Mirrors the renderer's path scheme: element children are index-addressed, a `Reactive`'s rendered content occupies the same path as
      * the boundary, `Foreach` items live at `path :+ key` (or `:+ index`), and `Fragment` children at `path :+ index`. Resolving through
      * these boundaries is what lets an element wrapped by a signal-typed setter (e.g. `.hidden(Signal)`, which wraps it in a `Reactive`)
      * still be recognized by its concrete type, so button-click form submit and disabled detection keep working through such wrappers.
      *
      * One walk answers every question a click asks. A walk is not free: resolving through a `Foreach`
      * renders the target row to reach it, so a walk per question would render the same row once per
      * question on every click.
      *
      * `controlBelow` is accumulated rather than tested at the leaf because it is a question about the
      * whole chain: a click on the icon inside a button targets the icon, and the icon alone cannot say
      * that a control was involved. `rootDepth` is what makes it strictly below: the dispatching element
      * never answers for itself, or a button would decline its own click.
      */
    private def resolveTarget(
        node: UI,
        nodePath: Seq[String],
        targetPath: Seq[String],
        rootDepth: Int,
        controlBelow: Boolean,
        mounts: MountDispatch
    )(using
        Frame
    ): TargetFacts < Sync =
        // A path that stops short still answers the control question: a control above a boundary the
        // resolver cannot see through (a mount, a text leaf) counts, so the flag is carried out.
        def unresolved(seen: Boolean): TargetFacts = TargetFacts(Absent, seen)
        node match
            case kc: KeyedChild[?] =>
                resolveTarget(kc.child, nodePath, targetPath, rootDepth, controlBelow, mounts)
            case r: Reactive[?] =>
                // A Reactive's rendered content occupies the same path as the boundary, so re-resolve at nodePath.
                r.signal.current(using r.frame).map(cur => resolveTarget(cur, nodePath, targetPath, rootDepth, controlBelow, mounts))
            case Fragment(children) =>
                if targetPath.size <= nodePath.size then unresolved(controlBelow)
                else
                    val seg = targetPath(nodePath.size)
                    Maybe.fromOption(seg.toIntOption) match
                        case Present(i) if i >= 0 && i < children.size =>
                            resolveTarget(children(i), nodePath :+ seg, targetPath, rootDepth, controlBelow, mounts)
                        case _ => unresolved(controlBelow)
                    end match
            case fe: Foreach[?, ?] @unchecked =>
                if targetPath.size <= nodePath.size then unresolved(controlBelow)
                else
                    val seg = targetPath(nodePath.size)
                    fe.applyTyped {
                        [T] => (signal, keyFn, renderFn) =>
                            signal.current(using fe.frame).map { items =>
                                val idx = keyFn match
                                    case Present(f) => items.indexWhere(it => f(it) == seg)
                                    case Absent     => Maybe.fromOption(seg.toIntOption).getOrElse(-1)
                                if idx >= 0 && idx < items.size then
                                    resolveTarget(renderFn(idx, items(idx)), nodePath :+ seg, targetPath, rootDepth, controlBelow, mounts)
                                else unresolved(controlBelow)
                            }
                    }
            case e: Element =>
                val seen = controlBelow || (nodePath.size > rootDepth && isOwnControl(e))
                if targetPath.size <= nodePath.size then TargetFacts(Present(e), seen)
                else
                    val seg = targetPath(nodePath.size)
                    Maybe.fromOption(seg.toIntOption) match
                        case Present(i) if i >= 0 && i < e.children.size =>
                            resolveTarget(e.children(i), nodePath :+ seg, targetPath, rootDepth, seen, mounts)
                        case _ => unresolved(seen)
                    end match
                end if
            // Text and RawHtml are leaf content with no kyo-addressable Element children, so no event
            // target can resolve through them.
            case _: Text | _: RawHtml => unresolved(controlBelow)
            // A mount's content lives with its instance, found through the dispatch table like the node's handler finds it, and
            // occupies the node's path. An inert snapshot resolves to nothing, since no event reaches it.
            case m: Mounted =>
                mounts.lookup(nodePath).map {
                    case Absent        => unresolved(controlBelow)
                    case Present(cell) =>
                        cell.current.map {
                            case MountState.Pending(Present(_)) => unresolved(controlBelow)
                            case state                          =>
                                val content = state.content(MountedSpec(m, nodePath, Nil))
                                resolveTarget(content, nodePath, targetPath, rootDepth, controlBelow, mounts)
                        }
                }
        end match
    end resolveTarget

    /** The facts a bubbling dispatch reads about its target, in a single resolution. */
    private def targetFacts(elem: Element, myPath: Seq[String], targetPath: Seq[String], mounts: MountDispatch)(using
        Frame
    ): TargetFacts < Sync =
        resolveTarget(elem, myPath, targetPath, myPath.size, false, mounts)

    /** Whether an element is a control in its own right, something the reader operates directly, as
      * opposed to markup an ancestor made clickable.
      *
      * `Focusable` is the set: every input flavour, `Textarea`, `Select`, the checkbox/radio pair,
      * `Button` and `Anchor`. An anchor qualifies only when it actually goes somewhere or does
      * something, since `a` with neither an href nor a handler is inert markup that happens to be in
      * the hierarchy.
      */
    private def isOwnControl(e: Element): Boolean = e match
        case a: Anchor    => a.href.isDefined || a.attrs.onClick.nonEmpty || a.attrs.onClickEvt.nonEmpty
        case _: Focusable => true
        case _            => false

    /** Bubble-continue value after an element handled `event`: `false` (consume) only when the element set
      * `stopPropagation(true)` AND actually declared a handler for this event's type (`declared`). The result flows up the
      * dispatch recursion, so a `false` makes every element ABOVE this one skip its own handler.
      */
    private def keepBubbling(elem: Element, declared: Boolean): Boolean =
        !(declared && elem.attrs.stopPropagation.getOrElse(false))

    /** Dispatch event to element. isTarget=true when element is the click target, false on bubble. disabledTarget=true when the original
      * click target was disabled (prevents Form onSubmit on bubble).
      *
      * `submitOrigin` is the only question left about whether the button under the event submits, and
      * it is asked on the click path alone. Keyboard activation reaches that same path as a synthesized
      * click (see [[activationTarget]]), so there is nothing for the keydown path to decide.
      *
      * `selfPath` is this element's own path, needed because a keydown records where its activation
      * click belongs rather than acting on it here.
      */
    private def dispatchToElement(
        elem: Element,
        event: UIEvent,
        isTarget: Boolean,
        selfPath: Seq[String] = Seq.empty,
        disabledTarget: Boolean = false,
        submitOrigin: Boolean = false,
        enterSubmits: Boolean = false,
        controlTarget: Boolean = false
    )(
        using Frame
    ): Boolean < Async =
        val attrs = elem.attrs
        event match
            case ev: UIEvent.Click =>
                // Disabled or hidden elements ignore their own click handler, but allow bubbling
                unlessInert(isTarget, isDisabled(elem).map(d => if d then true else isHidden(elem))) {
                    val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position, controlTarget)
                    // The target element is the nearest one that carries a path, so `isTarget` alone would also hold
                    // for a click on a plain descendant; `self` is the client's word that it was this element.
                    val onSelf = isTarget && ev.mouse.self
                    val self   = if onSelf then
                        invoke(attrs.onClickSelf).andThen(invokeWith(attrs.onClickSelfEvt, mouse))
                    else Kyo.lift(())
                    // Checkbox/radio toggle is handled by UIControlSession.click() which dispatches
                    // ChangeChecked after Click. Don't toggle here to avoid double-toggle.
                    val activateToggle = Kyo.lift(())
                    // When a Click on a submitting Button bubbles to a Form, trigger onSubmit (browser
                    // behavior). Not every Button: `type="button"` and `type="reset"` activate without
                    // submitting (see ButtonActivation).
                    val formSubmit = if !isTarget && !disabledTarget && submitOrigin then
                        elem match
                            case f: Form =>
                                invoke(f.onSubmit).andThen(invokeWith(f.onSubmitEvt, mouse))
                            case _ => Kyo.lift(())
                    else Kyo.lift(())
                    // Self handlers only count as "declared" when they actually fired.
                    val declared = attrs.onClick.nonEmpty || attrs.onClickEvt.nonEmpty ||
                        (onSelf && (attrs.onClickSelf.nonEmpty || attrs.onClickSelfEvt.nonEmpty))
                    self
                        .andThen(invokeWith(attrs.onClickEvt, mouse))
                        .andThen(invoke(attrs.onClick))
                        .andThen(activateToggle)
                        .andThen(formSubmit)
                        .andThen(keepBubbling(elem, declared))
                }
            case ev: UIEvent.ContextMenu =>
                // Mirrors Click: a disabled or hidden target skips its own handler but still bubbles.
                unlessInert(isTarget, isDisabled(elem).map(d => if d then true else isHidden(elem))) {
                    val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position)
                    invoke(attrs.onContextMenu)
                        .andThen(invokeWith(attrs.onContextMenuEvt, mouse))
                        .andThen(keepBubbling(elem, attrs.onContextMenu.nonEmpty || attrs.onContextMenuEvt.nonEmpty))
                }
            case ev: UIEvent.Focus =>
                val isFocusable = elem.isInstanceOf[Focusable] || elem.attrs.tabIndex.nonEmpty
                if isTarget && isFocusable then
                    val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position)
                    invoke(attrs.onFocus).andThen(invokeWith(attrs.onFocusEvt, mouse)).andThen(true)
                else if isTarget then Kyo.lift(false) // not focusable; reject
                else true
                end if
            case ev: UIEvent.Blur =>
                if isTarget then
                    val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position)
                    invoke(attrs.onBlur).andThen(invokeWith(attrs.onBlurEvt, mouse)).andThen(true)
                else true
            case e: UIEvent.KeyDown =>
                unlessInert(isTarget, isDisabled(elem)) {
                    val kbEvent = UI.KeyboardEvent(
                        key = Keyboard.fromString(e.keyboard.key),
                        modifiers = e.keyboard.modifiers,
                        targetId = e.keyboard.targetId
                    )
                    val keyHandler = invokeWith(attrs.onKeyDown, kbEvent)
                    // Keyboard activation, emulated here so a component behaves the same where no browser
                    // runs it (the TUI, the pure tests). The browser's own activation is suppressed by
                    // KeyPolicy.doubleActivates, so exactly one of the two reaches the handler.
                    //
                    // A button takes Enter and Space; an anchor takes Enter alone. That is not a
                    // simplification of the browser but the browser: Space with a link focused scrolls
                    // the page, it does not follow the link, and the ARIA link pattern says the same.
                    val activateClick = if isTarget && KeyPolicy.activatesButton(e.keyboard.key) then
                        elem match
                            case _: Button                                            => recordActivation(selfPath)
                            case _: Anchor if KeyPolicy.activatesLink(e.keyboard.key) => recordActivation(selfPath)
                            case _                                                    => Kyo.lift(())
                    else Kyo.lift(())
                    // Enter/Space on checkbox/radio triggers onChange. Space is the browser's; Enter is
                    // kyo's own, kept deliberately so a checkbox answers the same key in the TUI, where
                    // Enter is the one activation key a terminal reliably delivers. The client shim
                    // synthesizes the same Enter click for browser parity (see HtmlRenderer's keydown
                    // branch), so both transports agree.
                    val activateToggle = if isTarget && (e.keyboard.key == "Enter" || e.keyboard.key == " ") then
                        elem match
                            case cb: Checkbox =>
                                toggleChecked(cb.checked).map(newVal => invokeWith(cb.onChange, newVal))
                            case rb: Radio =>
                                val autoSet = rb.checked match
                                    case Present(Bound.Ref(ref)) => ref.set(true)
                                    case _                       => Kyo.lift(())
                                autoSet.andThen(invokeWith(rb.onChange, true))
                            case _ => Kyo.lift(())
                    else Kyo.lift(())
                    // ArrowDown/ArrowUp/Space/Enter on Select cycles through options (browser behavior)
                    val selectCycle =
                        if isTarget &&
                            (e.keyboard.key == "ArrowDown" || e.keyboard.key == "ArrowUp" || e.keyboard.key == " " ||
                                e.keyboard.key == "Enter")
                        then
                            elem match
                                case sel: Select => cycleSelectOption(sel, forward = e.keyboard.key != "ArrowUp")
                                case _           => Kyo.lift(())
                        else Kyo.lift(())
                    // Implicit submission: HTML's rule for Enter in a field, and the only thing a keydown
                    // does about submitting. The specification fires a click at the form's default
                    // button, so that is what gets recorded; the form's own onSubmit runs from that
                    // click, through the same `submitOrigin` a mouse click uses. A form with no
                    // reachable submit button has no click to fire, and HTML submits it directly.
                    //
                    // Nothing is recorded when the target already recorded its own activation (a
                    // button's Enter is that button's click, not the form's implicit submission), and
                    // only a target that submits on Enter submits at all (see `enterSubmits` at the dispatch site).
                    val formSubmit =
                        if !isTarget && KeyPolicy.submitsImplicitly(e.keyboard.key) && enterSubmits then
                            elem match
                                case f: Form =>
                                    defaultSubmitButton(f, selfPath) match
                                        case Present(buttonPath) => recordActivation(buttonPath)
                                        case Absent              => submitDirectly(f)
                                case _ => Kyo.lift(())
                        else Kyo.lift(())
                    keyHandler.andThen(activateClick).andThen(activateToggle).andThen(selectCycle).andThen(formSubmit)
                        .andThen(keepBubbling(elem, attrs.onKeyDown.nonEmpty))
                }
            case e: UIEvent.KeyUp =>
                val kbEvent = UI.KeyboardEvent(
                    key = Keyboard.fromString(e.keyboard.key),
                    modifiers = e.keyboard.modifiers,
                    targetId = e.keyboard.targetId
                )
                invokeWith(attrs.onKeyUp, kbEvent).andThen(keepBubbling(elem, attrs.onKeyUp.nonEmpty))
            case e: UIEvent.Input =>
                unlessInert(isTarget, isDisabled(elem).map(d => if d then true else isReadOnly(elem))) {
                    elem match
                        case ti: TextInput =>
                            val autoSet = ti.value match
                                case Present(Bound.Ref(ref)) => ref.set(e.value)
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(ti.onInput, e.value)).andThen(true)
                        case _ => true
                }
            case e: UIEvent.Change =>
                unlessInert(isTarget, isDisabled(elem).map(d => if d then true else isReadOnly(elem))) {
                    elem match
                        case ti: TextInput =>
                            val autoSet = ti.value match
                                case Present(Bound.Ref(ref)) => ref.set(e.value)
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(ti.onChange, e.value)).andThen(true)
                        case dd: Dropdown =>
                            val autoSet = dd.value match
                                case Present(Bound.Ref(ref)) => ref.set(e.value)
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(dd.onChange, e.value)).andThen(true)
                        case pi: PickerInput =>
                            val autoSet = pi.value match
                                case Present(Bound.Ref(ref)) => ref.set(e.value)
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(pi.onChange, e.value)).andThen(true)
                        case fi: FileInput => invokeWith(fi.onChange, e.value).andThen(true)
                        case _             => true
                }
            case e: UIEvent.FileSelect =>
                // Bubbles like Change: the files ride the payload, nothing is read from the element here.
                unlessInert(isTarget, isDisabled(elem)) {
                    invokeWith(attrs.onFileSelect, e.files).andThen(keepBubbling(elem, attrs.onFileSelect.nonEmpty))
                }
            case e: UIEvent.ChangeChecked =>
                unlessInert(isTarget, isDisabled(elem)) {
                    elem match
                        case bi: BooleanInput =>
                            val autoSet = bi.checked match
                                case Present(Bound.Ref(ref)) => ref.set(e.checked)
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(bi.onChange, e.checked)).andThen(true)
                        case _ => true
                }
            case e: UIEvent.ChangeNumeric =>
                unlessInert(isTarget, isDisabled(elem)) {
                    elem match
                        case ni: NumberInput =>
                            val autoSet = ni.value match
                                case Present(Bound.Ref(ref)) => ref.set(formatDouble(e.value))
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(ni.onChangeNumeric, e.value)).andThen(true)
                        case ri: RangeInput =>
                            val autoSet = ri.value match
                                case Present(Bound.Ref(ref)) => ref.set(e.value)
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(ri.onChange, e.value)).andThen(true)
                        case _ => true
                }
            case ev: UIEvent.Submit =>
                elem match
                    case f: Form =>
                        val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position)
                        invoke(f.onSubmit).andThen(invokeWith(f.onSubmitEvt, mouse)).andThen(true)
                    case _ => true
            case ev: UIEvent.Hover =>
                val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position)
                invoke(attrs.onHover).andThen(invokeWith(attrs.onHoverEvt, mouse))
                    .andThen(keepBubbling(elem, attrs.onHover.nonEmpty || attrs.onHoverEvt.nonEmpty))
            case ev: UIEvent.Unhover =>
                val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position)
                invoke(attrs.onUnhover).andThen(invokeWith(attrs.onUnhoverEvt, mouse))
                    .andThen(keepBubbling(elem, attrs.onUnhover.nonEmpty || attrs.onUnhoverEvt.nonEmpty))
            case ev: UIEvent.Scroll =>
                val wheel = UI.WheelEvent(ev.deltaX, ev.deltaY, ev.targetId, ev.modifiers)
                invoke(attrs.onScroll).andThen(invokeWith(attrs.onScrollEvt, wheel))
                    .andThen(keepBubbling(elem, attrs.onScroll.nonEmpty || attrs.onScrollEvt.nonEmpty))
            case _: UIEvent.DragStart =>
                if !isTarget then true
                else
                    DragCommands.current.use {
                        case Present(DragCommands.Dispatch(DragCommands.Payload.Event(value), _)) =>
                            invokeDrag(attrs.onDragStart, attrs.onDragStartEvt.map(_(value)), "drag start")
                                .andThen(keepBubbling(elem, attrs.onDragStart.nonEmpty || attrs.onDragStartEvt.nonEmpty))
                        case _ => true
                    }
            case _: UIEvent.DragEnd =>
                if !isTarget then true
                else
                    DragCommands.current.use {
                        case Present(DragCommands.Dispatch(DragCommands.Payload.End(value), _)) =>
                            invokeDrag(attrs.onDragEnd, attrs.onDragEndEvt.map(_(value)), "drag end")
                                .andThen(keepBubbling(elem, attrs.onDragEnd.nonEmpty || attrs.onDragEndEvt.nonEmpty))
                        case _ => true
                    }
            case _: UIEvent.DragEnter => dispatchDragEvent(elem, attrs.onDragEnter, attrs.onDragEnterEvt, "drag enter")
            case _: UIEvent.DragLeave => dispatchDragEvent(elem, attrs.onDragLeave, attrs.onDragLeaveEvt, "drag leave")
            case _: UIEvent.DragOver  => dispatchDragEvent(elem, attrs.onDragOver, attrs.onDragOverEvt, "drag over")
            case _: UIEvent.Drop      =>
                DragCommands.current.use {
                    case Present(DragCommands.Dispatch(DragCommands.Payload.Event(value), decisions)) =>
                        invokeDecision(attrs.onDrop, decisions, "The drop handler failed.")
                            .andThen(invokeDecision(attrs.onDropEvt.map(_(value)), decisions, "The drop handler failed."))
                            .andThen(keepBubbling(elem, attrs.onDrop.nonEmpty || attrs.onDropEvt.nonEmpty))
                    case _ => true
                }
            case _: UIEvent.SortMove =>
                DragCommands.current.use {
                    case Present(DragCommands.Dispatch(DragCommands.Payload.Move(value), decisions)) =>
                        invokeDecision(attrs.onSortMove, decisions, "The sort handler failed.")
                            .andThen(invokeDecision(attrs.onSortMoveEvt.map(_(value)), decisions, "The sort handler failed."))
                            .andThen(keepBubbling(elem, attrs.onSortMove.nonEmpty || attrs.onSortMoveEvt.nonEmpty))
                    case _ => true
                }
            // Payload (UI.PointerEvent) rides the wire directly like FileSelect, so nothing is read from the element.
            case ev: UIEvent.PointerDown =>
                invokeWith(attrs.onPointerDown, ev.pointer).andThen(keepBubbling(elem, attrs.onPointerDown.nonEmpty))
            case ev: UIEvent.PointerMove =>
                invokeWith(attrs.onPointerMove, ev.pointer).andThen(keepBubbling(elem, attrs.onPointerMove.nonEmpty))
            case ev: UIEvent.PointerUp =>
                invokeWith(attrs.onPointerUp, ev.pointer).andThen(keepBubbling(elem, attrs.onPointerUp.nonEmpty))
            case ev: UIEvent.ScrollPosition =>
                val sp = UI.ScrollPositionEvent(ev.scrollTop, ev.scrollLeft, ev.targetId)
                invokeWith(attrs.onScrollPos, sp)
                    .andThen(keepBubbling(elem, attrs.onScrollPos.nonEmpty))
            case _ => true
        end match
    end dispatchToElement

    private def invokeDrag(action: Maybe[Any < Async], typed: Maybe[Any < Async], label: String)(using Frame): Unit < Async =
        def safe(handler: Maybe[Any < Async]): Unit < Async =
            handler.fold((): Unit < Async) { effect =>
                Abort.recover[Throwable](
                    err => Log.error(s"Handler error during $label: ${err.getMessage}"),
                    panic => Log.error(s"Handler panic during $label: ${panic.getMessage}")
                )(effect.unit)
            }
        safe(action).andThen(safe(typed))
    end invokeDrag

    private def dispatchDragEvent(
        elem: Element,
        action: Maybe[Any < Async],
        typed: Maybe[Drag.Event => Any < Async],
        label: String
    )(using Frame): Boolean < Async =
        DragCommands.current.use {
            case Present(DragCommands.Dispatch(DragCommands.Payload.Event(value), _)) =>
                invokeDrag(action, typed.map(_(value)), label)
                    .andThen(keepBubbling(elem, action.nonEmpty || typed.nonEmpty))
            case _ => true
        }

    private def invokeDecision(
        handler: Maybe[Drag.Decision < Async],
        decisions: Maybe[AtomicRef[DragCommands.DecisionState]],
        failure: String
    )(using Frame): Unit < Async =
        handler.fold((): Unit < Async) { effect =>
            for
                result <- Abort.run[Any](effect)
                _      <- result match
                    case Result.Success(decision) => recordDecision(decisions, decision)
                    case Result.Failure(err)      =>
                        Log.error(s"Drag decision handler error: $err").andThen(failDecision(decisions, failure))
                    case Result.Panic(panic) =>
                        Log.error(s"Drag decision handler panic: ${panic.getMessage}").andThen(failDecision(decisions, failure))
            yield ()
        }

    private def recordDecision(ref: Maybe[AtomicRef[DragCommands.DecisionState]], decision: Drag.Decision)(using
        Frame
    ): Unit < Sync =
        ref.fold((): Unit < Sync)(_.getAndUpdate {
            case failed: DragCommands.DecisionState.Failed     => failed
            case rejected: DragCommands.DecisionState.Rejected => rejected
            case _                                             =>
                decision match
                    case Drag.Decision.Accept         => DragCommands.DecisionState.Accepted
                    case reject: Drag.Decision.Reject => DragCommands.DecisionState.Rejected(reject)
        }.unit)

    private def failDecision(ref: Maybe[AtomicRef[DragCommands.DecisionState]], reason: String)(using Frame): Unit < Sync =
        ref.fold((): Unit < Sync)(_.getAndUpdate(_ =>
            DragCommands.DecisionState.Failed(Drag.Decision.Reject(Drag.Rejection.Application(reason)))
        ).unit)

    // ---- Subscribe ----

    /** Result of subscribe: dispatch handler + signal change tracking. The enclosing Scope owns every
      * region fiber's lifecycle; the per-value Scope (opened by observe) owns each region's children,
      * so closing the root scope cascade-tears-down the tree.
      */
    case class Subscription(
        handle: Handler,
        private[kyo] val handleValidated: ValidatedHandler,
        lastSignalChangeTime: AtomicRef[Instant],
        private val dragSessionCountFn: () => Int < Sync = () => 0,
        private val dragExpiryWorkerCountFn: () => Int < Sync = () => 0,
        private val dragLock: Meter = Meter.Noop
    ):
        private[kyo] def dragSessionCount(using Frame): Int < Sync      = dragSessionCountFn()
        private[kyo] def dragExpiryWorkerCount(using Frame): Int < Sync = dragExpiryWorkerCountFn()

        /** Runs `v` under the mutex that serializes drag dispatch and the expiry worker's clock reading and timer arming. A test moves a
          * controlled clock here so the move never splits the worker's reading from its arming.
          */
        private[kyo] def withDragLock[A](v: => A < Async)(using Frame): A < (Async & Abort[Closed]) = dragLock.run(v)
    end Subscription

    private enum OwnedFiberState derives CanEqual:
        case Waiting
        case Running(fiber: Fiber[Unit, Any])
        case Closed
    end OwnedFiberState

    private def startOwnedFiber(task: => Unit < Async)(using Frame): Unit < (Async & Scope) =
        for
            state <- AtomicRef.init[OwnedFiberState](OwnedFiberState.Waiting)
            start <- Promise.init[Unit, Any]
            _     <- Scope.ensure(closeOwnedFiber(state))
            fiber <- Fiber.initUnscoped(start.get.andThen(task))
            _     <- installOwnedFiber(state, start, fiber)
        yield ()
    end startOwnedFiber

    private def installOwnedFiber(
        state: AtomicRef[OwnedFiberState],
        start: Promise[Unit, Any],
        fiber: Fiber[Unit, Any]
    )(using Frame): Unit < Async =
        state.get.flatMap {
            case OwnedFiberState.Waiting =>
                state.compareAndSet(OwnedFiberState.Waiting, OwnedFiberState.Running(fiber)).flatMap { installed =>
                    if installed then start.completeUnitDiscard
                    else installOwnedFiber(state, start, fiber)
                }
            case _: OwnedFiberState.Running =>
                Abort.panic(IllegalStateException("Reactive observer fiber was installed twice"))
            case OwnedFiberState.Closed =>
                fiber.interrupt.andThen(fiber.getResult.unit)
        }
    end installOwnedFiber

    private def closeOwnedFiber(state: AtomicRef[OwnedFiberState])(using Frame): Unit < Async =
        state.getAndSet(OwnedFiberState.Closed).flatMap {
            case OwnedFiberState.Running(fiber) => fiber.interrupt.andThen(fiber.getResult.unit)
            case OwnedFiberState.Waiting        => ()
            case OwnedFiberState.Closed         => ()
        }
    end closeOwnedFiber

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
            _ <- startOwnedFiber(
                Abort.run[Any] {
                    Sync.ensure(expiryWorkers.set(0))(expiryScheduler(dragSessions, dragMutex, expiryWake))
                }.unit
            )
            // Absent only for a ReactiveUI a test builds by hand; `normalize` always records the table.
            dispatch <- rui.mountDispatch match
                case Present(d) => Kyo.lift(d)
                case Absent     => MountDispatch.init
            rootMounts = MountTableRef.init()
            _ <- Scope.ensure(rootMounts.evictAll)
            _ <- subscribeScoped(rui, exchange, signalChangeTime, rootMounts, dispatch, RegionKeys(Seq(rui)))
            _ <- rootMounts.endWalk
            // Keyboard activation is synthesized at the bottom, on the tree's own handler, because the
            // two transports enter at different heights: the browser mount goes through `handle`, and
            // the server-push session through `handleValidated` (UIServer). Wrapping either one alone
            // suppresses the browser's activation on both while replacing it on only one, which is a
            // button that stops working rather than one that works twice.
            activating      = activatingHandle(rui.handle)
            validatedHandle = dragHandle(activating, dragSessions, dragMutex, expiryWake, dragLimits)
        yield Subscription(
            (path, event) =>
                DragProtocol.validateEventAndDomain(event, DragProtocol.Limits.default) match
                    case Result.Success(validated) => validatedHandle(path, validated)
                    case _                         => true,
            validatedHandle,
            signalChangeTime,
            () => dragSessions.get.map(_.size),
            () => expiryWorkers.get,
            dragMutex
        )

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

    private def dragHandle(
        handle: Handler,
        sessions: AtomicRef[Map[String, DragSession]],
        mutex: Meter,
        expiryWake: Channel[Unit],
        limits: DragSessionLimits
    )(using Frame): ValidatedHandler =
        (path, event) =>
            event match
                case event: DragProtocol.ValidatedEvent.Click          => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.ClickSelf      => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.ContextMenu    => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Input          => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Change         => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.ChangeChecked  => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.ChangeNumeric  => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Submit         => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.KeyDown        => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.KeyUp          => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Focus          => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Blur           => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Scroll         => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.ScrollPosition => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.FileSelect     => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.PointerDown    => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.PointerMove    => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.PointerUp      => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Hover          => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Unhover        => safeDispatch(handle, path, event.wire)
                // A measure reply is the client's answer to a `requestMeasureById`, not an element event: the session
                // completes the pending reply on UI.Commands before dispatch, so one never reaches the handler
                // tree. Bubbling is the right answer for an event no element declared.
                case _: DragProtocol.ValidatedEvent.MeasureById | _: DragProtocol.ValidatedEvent.MeasureMissing => true
                case _: DragProtocol.ValidatedEvent.Start | _: DragProtocol.ValidatedEvent.End |
                    _: DragProtocol.ValidatedEvent.Enter | _: DragProtocol.ValidatedEvent.Leave |
                    _: DragProtocol.ValidatedEvent.Over | _: DragProtocol.ValidatedEvent.Drop |
                    _: DragProtocol.ValidatedEvent.SortMove =>
                    Abort.runPartial[Closed](
                        mutex.run(dragDispatch(handle, sessions, expiryWake, limits, path, event))
                    ).map {
                        case Result.Success(value) => value
                        case Result.Failure(_)     => true
                    }

    private def dragDispatch(
        handle: Handler,
        sessions: AtomicRef[Map[String, DragSession]],
        expiryWake: Channel[Unit],
        limits: DragSessionLimits,
        path: Seq[String],
        event: DragProtocol.ValidatedEvent
    )(using Frame): Boolean < Async =
        event match
            case DragProtocol.ValidatedEvent.Start(wire @ UIEvent.DragStart(_, data), domainItems) =>
                DragCommands.resolveSink.use { resolver =>
                    sessions.get.map { current =>
                        if current.contains(data.sessionId) then
                            DragCommands.resolve(
                                data.sessionId,
                                Drag.Decision.Reject(
                                    Drag.Rejection.Application("A drag session with this identifier is already active.")
                                )
                            ).andThen(true)
                        else if current.size >= limits.maxSessions then
                            DragCommands.resolve(
                                data.sessionId,
                                Drag.Decision.Reject(Drag.Rejection.Application("Too many active drag sessions."))
                            ).andThen(true)
                        else
                            val domain = Drag.Event(
                                data.sessionId,
                                domainItems,
                                data.operation,
                                data.sourceKey,
                                Absent,
                                Present(data.point),
                                data.modifiers,
                                Absent
                            )
                            Clock.nowMonotonic.map { now =>
                                sessions.set(current +
                                    (data.sessionId -> DragSession(
                                        domain,
                                        terminalResolved = false,
                                        now + limits.lifetime,
                                        resolver
                                    )))
                                    .andThen(wakeExpiryScheduler(expiryWake))
                                    .andThen(DragCommands.current.let(Present(DragCommands.Dispatch(
                                        DragCommands.Payload.Event(domain),
                                        Absent
                                    ))) {
                                        safeDispatch(handle, path, event.wire)
                                    })
                            }
                    }
                }
            case DragProtocol.ValidatedEvent.End(wire @ UIEvent.DragEnd(_, data)) =>
                sessions.get.map(_.get(data.sessionId)).map {
                    case Some(session) =>
                        val finalEvent = session.event.copy(operation = data.operation)
                        sessions.getAndUpdate(_ - data.sessionId).unit
                            .andThen(DragCommands.current.let(Present(DragCommands.Dispatch(
                                DragCommands.Payload.End(Drag.End(finalEvent, canceled = data.cancelled)),
                                Absent
                            ))) {
                                safeDispatch(handle, path, wire)
                            })
                    case None => true
                }
            case DragProtocol.ValidatedEvent.Enter(wire) =>
                dispatchTarget(handle, path, wire, wire.event, sessions, terminal = false)
            case DragProtocol.ValidatedEvent.Leave(wire) =>
                dispatchTarget(handle, path, wire, wire.event, sessions, terminal = false)
            case DragProtocol.ValidatedEvent.Over(wire) =>
                dispatchTarget(handle, path, wire, wire.event, sessions, terminal = false)
            case DragProtocol.ValidatedEvent.Drop(wire) =>
                dispatchTarget(handle, path, wire, wire.event, sessions, terminal = true)
            case DragProtocol.ValidatedEvent.SortMove(wire @ UIEvent.SortMove(_, sessionId, move)) =>
                sessions.get.map(_.get(sessionId)).map {
                    case Some(session) if !session.terminalResolved =>
                        for
                            decisions <- AtomicRef.init[DragCommands.DecisionState](DragCommands.DecisionState.None)
                            _         <- DragCommands.current.let(Present(DragCommands.Dispatch(
                                DragCommands.Payload.Move(move),
                                Present(decisions)
                            ))) {
                                safeDispatch(handle, path, wire)
                            }
                            decision <- finalDecision(decisions, "No sort handler accepted the move.")
                            _        <- sessions.getAndUpdate(_ + (sessionId -> session.copy(terminalResolved = true)))
                            _        <- DragCommands.resolve(sessionId, decision)
                        yield true
                    case None =>
                        DragCommands.resolve(
                            sessionId,
                            Drag.Decision.Reject(Drag.Rejection.Application("No sort handler accepted the move."))
                        ).andThen(true)
                    case _ => true
                }
            // Never reaches here: the session completes a measure reply on UI.Commands before dispatch.
            case _: DragProtocol.ValidatedEvent.MeasureById | _: DragProtocol.ValidatedEvent.MeasureMissing => true
            case _: DragProtocol.ValidatedEvent.Click | _: DragProtocol.ValidatedEvent.ClickSelf |
                _: DragProtocol.ValidatedEvent.ContextMenu |
                _: DragProtocol.ValidatedEvent.Input | _: DragProtocol.ValidatedEvent.Change |
                _: DragProtocol.ValidatedEvent.ChangeChecked | _: DragProtocol.ValidatedEvent.ChangeNumeric |
                _: DragProtocol.ValidatedEvent.Submit | _: DragProtocol.ValidatedEvent.KeyDown |
                _: DragProtocol.ValidatedEvent.KeyUp | _: DragProtocol.ValidatedEvent.Focus |
                _: DragProtocol.ValidatedEvent.Blur | _: DragProtocol.ValidatedEvent.Scroll |
                _: DragProtocol.ValidatedEvent.Hover | _: DragProtocol.ValidatedEvent.Unhover |
                _: DragProtocol.ValidatedEvent.PointerDown | _: DragProtocol.ValidatedEvent.PointerMove |
                _: DragProtocol.ValidatedEvent.PointerUp |
                _: DragProtocol.ValidatedEvent.ScrollPosition | _: DragProtocol.ValidatedEvent.FileSelect =>
                safeDispatch(handle, path, event.wire)

    private def wakeExpiryScheduler(expiryWake: Channel[Unit])(using Frame): Unit < Sync =
        Abort.runPartial[Closed](expiryWake.offer(())).unit

    private def expiryScheduler(
        sessions: AtomicRef[Map[String, DragSession]],
        mutex: Meter,
        expiryWake: Channel[Unit]
    )(using Frame): Unit < (Async & Abort[Closed]) =
        Loop.foreach {
            mutex.run(expireAndArm(sessions)).map { timer =>
                val next: Unit < (Async & Abort[Closed]) = timer match
                    case Present(timer) => Async.race(timer.get, expiryWake.take.unit)
                    case Absent         => expiryWake.take
                Abort.runPartial[Closed](next).map {
                    case Result.Success(_) => Loop.continue
                    case Result.Failure(_) => Loop.done
                }
            }
        }

    // The clock reading, the expiry pass, and the arming of the next timer form one section under the drag mutex, which
    // also serializes every session admission. A clock moved while holding the same mutex never lands between the reading
    // and the arming, so the armed deadline is exactly the earliest remaining expiry.
    private def expireAndArm(sessions: AtomicRef[Map[String, DragSession]])(using Frame): Maybe[Fiber[Unit, Any]] < Async =
        Clock.nowMonotonic.map { now =>
            expireSessions(now, sessions).andThen(sessions.get.map { current =>
                current.valuesIterator.map(_.expiresAt).reduceOption(_.min(_)) match
                    case None            => Maybe.empty
                    case Some(expiresAt) => Clock.sleep(expiresAt.minusOrZero(now)).map(Maybe(_))
            })
        }

    private def expireSessions(now: Duration, sessions: AtomicRef[Map[String, DragSession]])(using Frame): Unit < Async =
        sessions.get.map { current =>
            val (expired, retained) = current.partition((_, session) => session.expiresAt <= now)
            sessions.set(retained).andThen(Kyo.foreach(expired.toSeq) { case (sessionId, session) =>
                if session.terminalResolved then ()
                else
                    session.resolver.fold((): Unit < Async)(_(
                        sessionId,
                        Drag.Decision.Reject(Drag.Rejection.Application("The drag session expired."))
                    ))
            }.unit)
        }

    private def dispatchTarget(
        handle: Handler,
        path: Seq[String],
        wire: UIEvent,
        data: DragProtocol.TargetData,
        sessions: AtomicRef[Map[String, DragSession]],
        terminal: Boolean
    )(using Frame): Boolean < Async =
        sessions.get.map(_.get(data.sessionId)).map {
            case Some(session) if !session.terminalResolved =>
                val domain = session.event.copy(
                    operation = data.operation,
                    targetKey = data.targetKey,
                    point = Present(data.point),
                    modifiers = data.modifiers,
                    position = data.position
                )
                if terminal then
                    for
                        decisions <- AtomicRef.init[DragCommands.DecisionState](DragCommands.DecisionState.None)
                        _         <- sessions.getAndUpdate(_ + (data.sessionId -> session.copy(event = domain)))
                        _         <- DragCommands.current.let(Present(DragCommands.Dispatch(
                            DragCommands.Payload.Event(domain),
                            Present(decisions)
                        ))) {
                            safeDispatch(handle, path, wire)
                        }
                        decision <- finalDecision(decisions, "No drop handler accepted the operation.")
                        _        <- sessions.getAndUpdate(_ + (data.sessionId -> session.copy(event = domain, terminalResolved = true)))
                        _        <- DragCommands.resolve(data.sessionId, decision)
                    yield true
                else
                    sessions.getAndUpdate(_ + (data.sessionId -> session.copy(event = domain))).unit
                        .andThen(DragCommands.current.let(Present(DragCommands.Dispatch(DragCommands.Payload.Event(domain), Absent))) {
                            safeDispatch(handle, path, wire)
                        })
                end if
            case None if terminal =>
                DragCommands.resolve(
                    data.sessionId,
                    Drag.Decision.Reject(Drag.Rejection.Application("No drop handler accepted the operation."))
                ).andThen(true)
            case _ => true
        }

    private def finalDecision(ref: AtomicRef[DragCommands.DecisionState], emptyReason: String)(using Frame): Drag.Decision < Sync =
        ref.get.map {
            case DragCommands.DecisionState.None            => Drag.Decision.Reject(Drag.Rejection.Application(emptyReason))
            case DragCommands.DecisionState.Accepted        => Drag.Decision.Accept
            case DragCommands.DecisionState.Rejected(value) => value
            case DragCommands.DecisionState.Failed(value)   => value
        }

    // Each reactive region starts an observer fiber as one scoped resource. Its finalizer interrupts the observer and
    // awaits the fiber's terminal result. Signal.observe runs each value in a fresh Scope; each value renders, re-walks,
    // and starts its children in that Scope, so the next value or an interrupt closes them transitively. A const node
    // has no signal: it subscribes its children in the enclosing scope. `mounts` is the mount table of the enclosing
    // region; keyed instances live in that tree of tables, outside every subscription's scopes.
    private def subscribeScoped(
        rui: ReactiveUI,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        mounts: MountTableRef,
        dispatch: MountDispatch,
        pass: RegionKeys,
        // Where a keyed row collects its channel releases (see RowScope); Absent everywhere else, where a release registers
        // on the Scope. Forwarded only along the row's own static children: a region or mount below the row owns its bindings
        // in its own scope.
        releases: Maybe[RowScope] = Absent
    )(using Frame): Unit < (Async & Scope) =
        // Start the scoped in-place-patch observers for this node's reactive channels. Runs unconditionally: a
        // no-op for attribute-less nodes, but an element carrying only reactive attrs is `isConst = true` and
        // would otherwise fork no observer of its own.
        forkChannelObservers(
            rui.path,
            rui.reactiveAttrs,
            rui.reactiveBoolAttrs,
            rui.reactiveClasses,
            exchange,
            rui.frame,
            rui.renderedAttrValues,
            rui.renderedBoolAttrValues,
            rui.renderedClassValues,
            releases
        ).andThen {
            if rui.mounted.nonEmpty then
                subscribeMounted(rui, rui.mounted.get, exchange, signalChangeTime, mounts, dispatch)
            else if rui.isConst then
                // A constant node belongs to the enclosing region's pass, which already counts its children.
                Kyo.foreachDiscard(rui.children)(subscribeScoped(_, exchange, signalChangeTime, mounts, dispatch, pass, releases))
            else if rui.foreachSpec.exists(_.reusable) then
                subscribeForeachRegion(rui, rui.foreachSpec.get, exchange, signalChangeTime, mounts.child(pass, rui), dispatch)
            else
                bindTextRegion(rui, exchange, releases).map { bound =>
                    if bound then Kyo.unit
                    else subscribeRegion(rui, exchange, signalChangeTime, mounts.child(pass, rui), dispatch)
                }
        }
    end subscribeScoped

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
                    val write: String => Unit = devSink match
                        case Absent          => v => patch(rui.path, v)
                        case Present(report) =>
                            v =>
                                patch(rui.path, v)
                                report(cheapEvent(rui.path, ReactiveRegion.htmlIdOf(rui.region), rui.frame, UI.RenderKind.Text))
                    Sync.Unsafe.defer {
                        sig.unsafeObserveProjected[String](identity, rendered, write) match
                            case Absent           => Kyo.lift(false)
                            case Present(release) => registerRelease(release, releases).andThen(true)
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

    /** Starts the channel observers of the element a region paints as its root. A walk only gives the root's children nodes of their
      * own, so the root's channels at `path` are started here, in the current scope like the children's subscriptions.
      */
    private def forkRootChannels(path: Seq[String], content: UI, exchange: UIExchange)(using Frame): Unit < (Async & Scope) =
        content match
            case el: Element
                if el.attrs.reactiveAttrs.nonEmpty || el.attrs.reactiveBoolAttrs.nonEmpty || el.attrs.reactiveClasses.nonEmpty =>
                forkChannelObservers(path, el.attrs.reactiveAttrs, el.attrs.reactiveBoolAttrs, el.attrs.reactiveClasses, exchange, el.frame)
            case _ => Kyo.unit

    /** Fork the scoped in-place-patch observers for one node's reactive attr/bool-attr/class channels at
      * `path`. Shared by subscribeScoped (walked nodes) and by forkRootChannels (the painted ROOT element,
      * which is absent from the walk's kids), so the two sites cannot drift apart.
      */
    private def forkChannelObservers(
        path: Seq[String],
        attrs: Map[String, Signal[String]],
        boolAttrs: Map[String, Signal[Boolean]],
        classes: Map[String, Signal[Boolean]],
        exchange: UIExchange,
        // The node these channels belong to, for the devtools report.
        nodeFrame: Frame,
        renderedAttrs: Map[String, String] = Map.empty,
        renderedBools: Map[String, Boolean] = Map.empty,
        renderedClasses: Map[String, Boolean] = Map.empty,
        releases: Maybe[RowScope] = Absent
    )(using Frame): Unit < (Async & Scope) =
        // Most nodes bind no channel at all; they leave before the sink is read or a loop is set up.
        if attrs.isEmpty && boolAttrs.isEmpty && classes.isEmpty then Kyo.unit
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
                    rendered: Map[String, A],
                    patcher: Maybe[(Seq[String], String, A) => Unit],
                    slow: (String, A) => Unit < Async
                )(using CanEqual[A, A]): Unit < (Async & Scope) =
                    if signals.isEmpty then Kyo.unit
                    else
                        val now: Maybe[(Seq[String], String, A) => Unit] = devSink match
                            case Absent          => patcher
                            case Present(report) =>
                                patcher.map(patch =>
                                    (target, name, value) =>
                                        patch(target, name, value)
                                        report(cheapEvent(target, Absent, nodeFrame, UI.RenderKind.Channel(name)))
                                )
                        Kyo.foreachDiscard(signals.toSeq) { (name, sig) =>
                            val baseline = Maybe.fromOption(rendered.get(name))
                            now match
                                case Present(patch) => bindChannel(sig, baseline, v => patch(path, name, v), releases)
                                case Absent         =>
                                    val write: A => Unit < Async = devSink match
                                        case Absent          => v => slow(name, v)
                                        case Present(report) =>
                                            v =>
                                                slow(name, v).andThen(
                                                    Sync.defer(report(cheapEvent(path, Absent, nodeFrame, UI.RenderKind.Channel(name))))
                                                )
                                    Fiber.init(sig.observe(baseline, Signal.defaultRepairInterval)(write)).unit
                            end match
                        }
                end bindAll

                bindAll(attrs, renderedAttrs, exchange.attrPatcherNow, exchange.onAttrPatch(path, _, _))
                    .andThen(bindAll(boolAttrs, renderedBools, exchange.boolAttrPatcherNow, exchange.onBoolAttrPatch(path, _, _)))
                    .andThen(bindAll(classes, renderedClasses, exchange.classPatcherNow, exchange.onClassPatch(path, _, _)))
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
        Sync.Unsafe.defer {
            sig.unsafeObserveProjected[A](identity, rendered, write) match
                case Present(release) => registerRelease(release, releases)
                case Absent           => sig.onChange(rendered)(v => Sync.defer(write(v)))
        }

    /** Where a callback binding's release goes: into the row's scope when one is collecting, else onto the Scope. */
    private def registerRelease(release: () => Unit, releases: Maybe[RowScope])(using Frame): Unit < (Sync & Scope) =
        releases match
            case Present(row) => Sync.Unsafe.defer(row.add(release))
            case Absent       => Scope.ensure(Sync.defer(release()))

    /** Starts the fiber of a region over a `Signal[UI]`, with a fresh Scope per value: each value is painted, walked, and its
      * reactive children subscribed in that Scope, so the next value or an interrupt tears them down by cascade. `regionMounts` is the
      * region's mount table, which outlives the subscription (see [[MountTableRef]]).
      */
    private def subscribeRegion(
        rui: ReactiveUI,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        regionMounts: MountTableRef,
        dispatch: MountDispatch
    )(using Frame): Unit < (Async & Scope) =
        // The tree this region last rendered, so the exchange can send only what moved instead of the whole region.
        // Unsafe: region-scoped, single-writer state created before the observe loop is forked, where no effect context
        // exists to supply AllowUnsafe; only this region's own loop reads or writes it.
        val rendered = AtomicRef.Unsafe.init(Maybe.empty[UI])(using AllowUnsafe.embrace.danger)
        def subscribeKids(kids: Seq[ReactiveUI]): Unit < (Async & Scope) =
            val keys = RegionKeys(kids)
            regionMounts.beginWalk(kids, keys)
                .andThen(Kyo.foreachDiscard(kids)(subscribeScoped(_, exchange, signalChangeTime, regionMounts, dispatch, keys)))
                .andThen(regionMounts.endWalk)
        end subscribeKids
        def renderValue(current: UI, cause: UI.RenderCause): Unit < (Async & Scope) =
            for
                now      <- Clock.now
                _        <- signalChangeTime.set(now)
                previous <- Sync.Unsafe.defer(rendered.getAndSet(Present(current)))
                _        <- measuredPaint(rui, UI.RenderKind.Repaint, cause)(
                    exchange.onChange(rui.region, rui.path, rui.contentContext, rui.parentContext, previous, current)
                )
                svgContext = rui.region match
                    case _: ReactiveRegion.HtmlRange  => false
                    case _: ReactiveRegion.SvgElement => true
                (newKids, _) <- Sync.Unsafe.defer(walkStatic(
                    current,
                    rui.path,
                    svgContext,
                    rui.contentContext,
                    nestedParentContext(rui.parentContext, current),
                    dispatch,
                    rows = 0L,
                    deepRows = Nil,
                    discoverRootBound = rui.discoverContentRootBound
                ))
                _ <- forkRootChannels(rui.path, current, exchange)
                _ <- subscribeKids(newKids)
            yield ()

        // What the region's fiber listens to. With a source present that is value space: `observe` drops an emission whose
        // value equals the last delivered one, so a record that ticks once a second stops reaching the regions drawing the
        // slices of it that did not move. Without a source (a lifted `Signal[UI]`, where no value exists before the UI) it
        // stays the projected signal and the only available comparison is the UI itself.
        def observed(paint: UI => Unit < (Async & Scope)): Unit < Async =
            rui.source match
                case Present(src) => src.observe(paint)
                case Absent       => rui.signal.observe(paint)

        startOwnedFiber {
            Abort.run[Throwable] {
                var first = true
                observed { current =>
                    val isFirst = first
                    first = false
                    if isFirst && rui.renderedValue.exists(_.equals(current)) then
                        // The enclosing render already painted exactly this value: skip the repaint and the re-walk, but
                        // still subscribe what renderValue would have. The value is recorded as this region's last render,
                        // so the NEXT emission diffs against it rather than re-sending the whole region.
                        //
                        // Devtools still hears about it, as the "rebuilt by my parent" half of the story (see reportUnpainted).
                        Sync.Unsafe.defer(rendered.set(Present(current)))
                            .andThen(reportUnpainted(rui, UI.RenderKind.Repaint))
                            .andThen(forkRootChannels(rui.path, current, exchange))
                            .andThen(subscribeKids(rui.children))
                    else renderValue(current, if isFirst then UI.RenderCause.Created else UI.RenderCause.Signal)
                    end if
                }
            }.map { result =>
                result.fold(
                    _ => (),
                    err => Log.error(s"Reactive subscription fiber failed at path=${rui.path.mkString(".")}", err),
                    panic =>
                        if panic.isInstanceOf[Interrupted] then ()
                        else Log.error(s"Reactive subscription fiber failed at path=${rui.path.mkString(".")}", panic)
                )
            }
        }
    end subscribeRegion

    /** Runs `paint` under a fresh devtools probe, which the backend writes the paint's cost into, and reports it as `kind`.
      * With no sink installed the probe is `Absent`, and nothing is allocated or clocked.
      *
      * `alsoWasted` is for a keyed-list emission, where `wasted` is stricter than "the backend changed nothing": a pure reorder
      * repaints no row and still moves the DOM, and calling that wasted would send a reader looking for a bug that is not there.
      * It is by name because answering it can cost a pass over the rows, and only a measured paint ever asks.
      */
    private def measuredPaint[A, S](rui: ReactiveUI, kind: UI.RenderKind, cause: UI.RenderCause, alsoWasted: => Boolean = false)(
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
    private def reportUnpainted(rui: ReactiveUI, kind: UI.RenderKind)(using Frame): Unit < Sync =
        report(rui, kind, UI.RenderCause.Created, Absent, Duration.Zero, alsoWasted = false)

    /** One render event to the devtools sink; a no-op with none installed. `probe` is `Absent` both when devtools is off and
      * when nothing was painted, which is why the two collapse into the same zero-cost event.
      */
    private def report(
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

    /** One live row of a reusable keyed Foreach region (see subscribeForeachRegion): the cached render
      * output (reused for paints while the item is unchanged), the walked reactive descendants, the row's
      * dispatch handler (cached so an event on a live row skips the whole-region walk; Absent for seeded rows,
      * whose initial walk produced no per-row handler), and the finalizer holding their subscriptions open (it
      * survives the region's per-value cascade; the registry's owner scope ends it).
      */
    final private[kyo] class RowInstance(
        val key: String,
        val item: Any,
        val rowUI: UI,
        val kids: Seq[ReactiveUI],
        val handler: Maybe[Handler],
        val scope: RowScope
    )

    /** What keeps one keyed row's subscriptions open, closed by the registry when the row is evicted.
      *
      * A plain keyed row binds a class channel and a text: two callbacks on their signals' promises, each released by one call. On a
      * Scope finalizer each would be an effect of its own, an `Abort` handler around a deferred call, run by the finalizer's close and
      * await. So bare releases collect here and run in one pass; the finalizer keeps only what is not a bare release (a nested
      * region's fiber, a mount), which for a plain row is nothing.
      */
    final private[kyo] class RowScope(val finalizer: Scope.Finalizer):
        private var releases: List[() => Unit] = Nil

        def add(release: () => Unit)(using AllowUnsafe): Unit = releases = release :: releases

        /** Runs every release, newest first, and reports the first failure instead of stopping at it. */
        def releaseAll()(using AllowUnsafe): Maybe[Throwable] =
            var failed: Maybe[Throwable] = Absent
            var rest                     = releases
            while rest.nonEmpty do
                try rest.head()
                catch case t if scala.util.control.NonFatal(t) => if failed.isEmpty then failed = Present(t)
                rest = rest.tail
            end while
            releases = Nil
            failed
        end releaseAll
    end RowScope

    /** Per-region ownership of reusable Foreach rows, in list order. Mutation happens only from the owning
      * region's sequential observe loop, so plain get/set on the ref suffices.
      *
      * A row is the registry's from the moment it starts subscribing, not from [[replaceAll]]: its scope is a root nothing else
      * closes, so an emission interrupted in between would otherwise leave what the row had already subscribed running.
      */
    final private[kyo] class RowRegistry(
        rows: AtomicRef[Vector[RowInstance]],
        building: AtomicRef[List[RowScope]]
    ):
        def snapshot(using Frame): Vector[RowInstance] < Sync = rows.get

        def replaceAll(ordered: Seq[RowInstance])(using Frame): Unit < Sync =
            rows.set(Vector.from(ordered)).andThen(building.set(Nil))

        /** Take a row that is about to subscribe; [[replaceAll]] installs it, or [[evictAll]] closes it. */
        def track(scope: RowScope)(using Frame): Unit < Sync = building.getAndUpdate(scope :: _).unit

        /** Close-and-await every row whose key is not in `keep`: run before the new value paints, so a
          * vanished or changed row's observers are fully released while the old content is still on screen
          * (break-before-make, the region-level closed-and-awaited guarantee extended to rows).
          */
        def evictExcept(keep: Set[String])(using Frame): Unit < Async =
            for
                old <- rows.getAndUpdate(_.filter(r => keep.contains(r.key)))
                gone = old.filterNot(r => keep.contains(r.key))
                // The channel releases of every evicted row in one pass, then the finalizers, which for a plain row hold
                // nothing and complete without suspending (initInline).
                _ <- release(gone.map(_.scope))
            yield ()

        private def release(scopes: Seq[RowScope])(using Frame): Unit < Async =
            for
                failures <- Sync.Unsafe.defer(scopes.flatMap(_.releaseAll().toList))
                _        <- Kyo.foreachDiscard(failures)(t => Log.error("kyo-ui: a row's release failed", t))
                _        <- Kyo.foreachDiscard(scopes)(s => s.finalizer.close(Absent).andThen(s.finalizer.await))
            yield ()

        def evictAll(using Frame): Unit < Async =
            evictExcept(Set.empty).andThen(building.getAndSet(Nil).map(release))
    end RowRegistry

    private[kyo] object RowRegistry:
        /** The registry for a pass that already holds the thread (see [[SignalNow]]). */
        def initNow()(using AllowUnsafe): RowRegistry =
            new RowRegistry(AtomicRef.Unsafe.init(Vector.empty[RowInstance]).safe, AtomicRef.Unsafe.init(List.empty[RowScope]).safe)
    end RowRegistry

    /** Subscribe a reusable keyed Foreach region: rows are owned by a RowRegistry instead of the per-value
      * Scope cascade. On each list emission only added rows and rows whose item value changed are re-rendered,
      * re-walked, and re-subscribed; a row whose key and item persist keeps its live observers (its path,
      * `regionPath :+ key`, is stable across reorders, so no retargeting is needed) and its cached row UI is
      * reused for the paint. Removed and changed rows are closed and awaited before the new value paints
      * (break-before-make, matching the whole-region path's ordering contract). Event dispatch resolves through the
      * same registry (shared via the ForeachSpec): an event on a live row routes to the row's cached handler
      * instead of re-rendering and re-walking the whole list, falling back to the full walk only when the
      * registry cannot resolve the target. Duplicate keys disable reuse for that emission (all rows rebuilt,
      * loud warning), keeping the painted output identical to the non-reusable path. Unkeyed and *Indexed
      * variants stay on the whole-region path entirely.
      *
      * The mounts of every row, re-walked or retained, are one pass of the region's mount table, so a retained row's keyed
      * instances are kept. A row's key is part of every mount identity in it, so no claim in one row can collide with another's.
      */
    private def subscribeForeachRegion(
        rui: ReactiveUI,
        spec: ForeachSpec,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        regionMounts: MountTableRef,
        dispatch: MountDispatch
    )(using Frame): Unit < (Async & Scope) =
        val path        = rui.path
        val initialKids = rui.children
        val svg         = rui.region match
            case _: ReactiveRegion.HtmlRange  => false
            case _: ReactiveRegion.SvgElement => true
        // The tree this region last rendered, so the exchange can send only what moved.
        // Unsafe: region-scoped, single-writer state created before the observe loop is started.
        val rendered = AtomicRef.Unsafe.init(Maybe.empty[UI])(using AllowUnsafe.embrace.danger)
        spec.node.applyTyped {
            [T] => (itemSignal, keyFnMaybe, renderFn) =>
                val keyFn = keyFnMaybe.get // reusable guarantees a key function
                val rows  = spec.rows
                for
                    _ <- Scope.ensure(rows.evictAll)
                    _ <- startOwnedFiber {
                        // A row owns a bare Finalizer, not a fiber: the registry closes and awaits it, and
                        // scoped children still die with it, since a nested region's or mount's release is
                        // registered on it. Subscribing inline rather than on a forked fiber makes a row that
                        // suspends finish before the emission does; the paint is already out by then.
                        // The registry always closes and awaits a row, so its finalizers run on the evicting
                        // fiber (initInline) instead of a fiber per row, and its channel releases collect in the
                        // row's scope, which the eviction runs in one pass.
                        def rowRunner(kids: Seq[ReactiveUI], pass: RegionKeys): RowScope < Async =
                            Sync.Unsafe.defer {
                                val fin   = Scope.Finalizer.Unsafe.initInline()
                                val scope = new RowScope(fin)
                                // A root, as `Scope.runUnowned`'s is: the registry owns the row, not the enclosing scope.
                                rows.track(scope).andThen(ContextEffect.handle[Scope.Finalizer, Scope, Unit, Async](
                                    Tag[Scope],
                                    derive = (_: Maybe[Scope.Finalizer]) => fin,
                                    fork = (parent: Scope.Finalizer) => parent.forked,
                                    join = (parent: Scope.Finalizer, _: Scope.Finalizer, _: Scope.Finalizer) => parent
                                ) {
                                    Kyo.foreachDiscard(kids)(
                                        subscribeScoped(_, exchange, signalChangeTime, regionMounts, dispatch, pass, Present(scope))
                                    )
                                }).andThen(scope)
                            }

                        def distinctKeys(items: Chunk[T]): Boolean =
                            val ks = items.toSeq.map(keyFn)
                            ks.distinct.sizeIs == ks.size

                        // First emission over the already-painted value: skip render and walk entirely; adopt the
                        // walked initial kids as rows (grouped by their row path segment) and just subscribe them.
                        def seed(items: Chunk[T]): Unit < (Async & Scope) =
                            val pass    = RegionKeys(initialKids)
                            val grouped = initialKids.groupBy(k => k.path.lift(path.length).getOrElse(""))
                            for
                                _     <- regionMounts.beginWalk(initialKids, pass)
                                built <- Kyo.foreach(Chunk.from(items.toSeq.zipWithIndex)) { (item, i) =>
                                    val key  = keyFn(item)
                                    val kids = grouped.getOrElse(key, Seq.empty)
                                    rowRunner(kids, pass).map(scope => new RowInstance(key, item, renderFn(i, item), kids, Absent, scope))
                                }
                                _ <- regionMounts.endWalk
                                // No paint here: the enclosing render already put exactly this fragment in the
                                // DOM. It is still recorded as this region's last render, so the NEXT emission
                                // diffs against it rather than re-sending every row.
                                _ <- Sync.Unsafe.defer(
                                    rendered.set(Present(Fragment[UI](Chunk.from(built.toSeq.map(r => KeyedChild[UI](r.key, r.rowUI))))))
                                )
                                _ <- rows.replaceAll(built.toSeq)
                                // No paint happened, so no cost is charged, but the rows did come into existence, in the
                                // enclosing region's paint. Same accounting as the region skip branch in subscribeRegion.
                                _ <- reportUnpainted(rui, UI.RenderKind.ListPatch(0, built.size))
                            yield ()
                            end for
                        end seed

                        def handle(items: Chunk[T]): Unit < (Async & Scope) =
                            for
                                now  <- Clock.now
                                _    <- signalChangeTime.set(now)
                                prev <- rows.snapshot
                                prevByKey  = prev.map(r => r.key -> r).toMap
                                keyed      = Chunk.from(items.toSeq.zipWithIndex.map((item, i) => (keyFn(item), item, i)))
                                duplicates = !distinctKeys(items)
                                _ <-
                                    if duplicates then
                                        Log.warn(
                                            s"kyo-ui: duplicate keys in foreachKeyed at ${path.mkString(".")} within one emission: " +
                                                "row reuse is disabled for this emission (all rows rebuilt); use distinct keys"
                                        )
                                    else Kyo.unit
                                retainedKeys =
                                    if duplicates then Set.empty[String]
                                    else keyed.collect { case (k, item, _) if prevByKey.get(k).exists(_.item.equals(item)) => k }.toSet
                                _ <- rows.evictExcept(retainedKeys) // removed AND changed rows close before the paint
                                // One synchronous pass over the rows (see SignalNow), not an effect loop that suspends per row.
                                built <- Sync.Unsafe.defer {
                                    keyed.map { (key, item, i) =>
                                        prevByKey.get(key).filter(_ => retainedKeys.contains(key)) match
                                            case Some(inst) => (key, item, inst.rowUI, inst.kids, inst.handler, Present(inst))
                                            case None       =>
                                                val rowUI       = renderFn(i, item)
                                                val (kids, hdl) = walkRow(
                                                    rowUI,
                                                    path :+ key,
                                                    svg,
                                                    rui.contentContext.child(key),
                                                    nestedParentContext(rui.parentContext, rowUI),
                                                    dispatch
                                                )
                                                (key, item, rowUI, kids, Present(hdl), Absent: Maybe[RowInstance])
                                    }
                                }
                                allKids = built.toSeq.flatMap((_, _, _, kids, _, _) => kids)
                                pass    = RegionKeys(allKids)
                                _ <- regionMounts.beginWalk(allKids, pass)
                                fragment =
                                    Fragment[UI](Chunk.from(built.toSeq.map((key, _, rowUI, _, _, _) => KeyedChild[UI](key, rowUI))))
                                previous <- Sync.Unsafe.defer(rendered.getAndSet(Present(fragment)))
                                // A structural command addresses rows by key, so it can say nothing useful
                                // about an emission whose keys are not unique: two rows would name one
                                // slot. That emission already rebuilds every row and warns; it also keeps
                                // the whole-fragment paint, where duplicates degrade positionally instead
                                // of aliasing. `retained.isEmpty` is exactly "this row was re-rendered
                                // above": retained iff key survived AND item compared equal, which is the
                                // same condition under which the row's DOM was left alone.
                                // A structural command addresses rows BY KEY, so a row that paints as several
                                // roots or as none has nothing for a key to name. The wire cannot discover that
                                // late and change its mind: once the untouched rows are left out of a frame, the
                                // client has nothing to rebuild them from. So the shape is decided here, beside
                                // the duplicate-key gate it restates, and without rendering anything.
                                // So does the emission after one with duplicate keys: its page still shows two rows under one key.
                                addressable = built.forall((_, _, rowUI, _, _, _) => HtmlRenderer.paintsAsKeyedRoot(rowUI))
                                changedRows = built.count((_, _, _, _, _, retained) => retained.isEmpty)
                                kept <- measuredPaint(
                                    rui,
                                    UI.RenderKind.ListPatch(changedRows, built.size),
                                    UI.RenderCause.Signal,
                                    alsoWasted = changedRows == 0 && built.toSeq.corresponds(prev)((b, p) => b._1 == p.key)
                                ) {
                                    if duplicates || prevByKey.size != prev.size || !addressable then
                                        exchange.onChange(rui.region, path, rui.contentContext, rui.parentContext, previous, fragment)
                                            .andThen(false)
                                    else
                                        exchange.onListPatch(
                                            rui.region,
                                            path,
                                            rui.contentContext,
                                            rui.parentContext,
                                            previous,
                                            built.toSeq.map((key, _, rowUI, _, _, retained) => ListRow(key, rowUI, retained.isEmpty))
                                        )
                                }
                                // A repaint of the kept rows replaced what their subscriptions had painted into them, a
                                // mount's content among it: subscribe them again, as a whole-list repaint always did.
                                _         <- if kept then Kyo.unit else rows.evictAll
                                finalRows <- Kyo.foreach(built) { (key, item, rowUI, kids, hdl, retained) =>
                                    retained match
                                        case Present(inst) if kept => Kyo.lift(inst)
                                        case _                     =>
                                            rowRunner(kids, pass).map(scope => new RowInstance(key, item, rowUI, kids, hdl, scope))
                                }
                                _ <- regionMounts.endWalk
                                _ <- rows.replaceAll(finalRows.toSeq)
                            yield ()

                        Abort.run[Throwable] {
                            var first = true
                            itemSignal.observe { items =>
                                val isFirst = first
                                first = false
                                if isFirst && spec.renderedItems.exists(_.equals(items)) && distinctKeys(items) then seed(items)
                                else handle(items)
                            }
                        }.map { result =>
                            result.fold(
                                _ => (),
                                err => Log.error(s"Reactive subscription fiber failed at path=${path.mkString(".")}", err),
                                panic =>
                                    if panic.isInstanceOf[Interrupted] then ()
                                    else Log.error(s"Reactive subscription fiber failed at path=${path.mkString(".")}", panic)
                            )
                        }
                    }
                yield ()
                end for
        }
    end subscribeForeachRegion

    // ---- Mounted nodes ----

    /** What a mounted node's content cell holds.
      *
      *   - `Pending(Absent)`: the effect has not published, and the node shows its placeholder.
      *   - `Pending(Present(snapshot))`: the same, showing the inert snapshot of what the slot showed before. A snapshot is painted and
      *     nothing else: no region in it is subscribed and no event reaches it.
      *   - `Ready(ui)`: the published content.
      *   - `Failed(ui)`: the rendered failure. It is final: a later publish or failure of the same instance leaves it as it is.
      */
    private[kyo] enum MountState derives CanEqual:
        case Pending(snapshot: Maybe[UI])
        case Ready(ui: UI)
        case Failed(ui: UI)

        def content(spec: MountedSpec): UI =
            this match
                case Pending(snapshot) => snapshot.getOrElse(spec.placeholder)
                case Ready(ui)         => ui
                case Failed(ui)        => ui
    end MountState

    /** A hash of a call site that reads at most its first 64 characters, where the file, line and column sit. A frame is a long string,
      * and Scala.js does not cache string hashes, so a full hash would cost its whole length on every use.
      */
    private def siteHash(frame: Frame): Int =
        val s   = frame.toString
        val end = math.min(s.length, 64)
        var h   = s.length
        var i   = 0
        while i < end do
            h = 31 * h + s.charAt(i)
            i += 1
        h
    end siteHash

    /** The identity a keyed mount instance keeps across re-renders: the keys of the list rows it sits in, where its `.keyed` call is
      * written and its key. Positions among siblings are deliberately not part of it, so siblings that come and go leave it alone; they
      * only tell apart nodes of one render pass that share an identity (see [[MountTable]]). The hash is computed once.
      */
    final private[kyo] case class MountIdentity(rows: List[String], site: UI.MountSite, key: Any) derives CanEqual:
        override val hashCode: Int = (rows.hashCode * 31 + site.hashCode) * 31 + key.hashCode

    /** Where a region's mount table sits in the table of the enclosing region: the keys of the list rows the region sits in, the call
      * site of the node it comes from, and its rank among the regions of one render pass that share those two. Like a mount's identity
      * it leaves out positions among siblings; only regions repeated from one call site, outside a keyed list, are told apart by order.
      * A region built by hand, with no call site, is placed by its path.
      */
    final private[kyo] case class RegionKey(rows: List[String], site: AnyRef, rank: Int) derives CanEqual:
        override val hashCode: Int =
            val siteCode = site match
                case path: Seq[?] => path.hashCode
                case other        => siteHash(other.asInstanceOf[Frame])
            (rows.hashCode * 31 + siteCode) * 31 + rank
        end hashCode
    end RegionKey

    /** The nodes one render pass of a region owns: its reactive children, and through a constant child (an element that only
      * carries reactive attributes) that child's own, which the constant node subscribes in the same pass and the same table.
      */
    private def passMembers(kids: Seq[ReactiveUI]): Iterator[ReactiveUI] =
        kids.iterator.flatMap(kid => if kid.isConst && kid.mounted.isEmpty then passMembers(kid.children) else Iterator.single(kid))

    /** The keys of the regions among one render pass's reactive children, computed on first use, which only a pass with mounts at or
      * below it ever makes.
      */
    final private[kyo] class RegionKeys(kids: Seq[ReactiveUI]):
        private lazy val keys: java.util.IdentityHashMap[ReactiveUI, RegionKey] =
            val result = new java.util.IdentityHashMap[ReactiveUI, RegionKey]()
            // Keyed by the rank-0 key, whose hash is the cheap one.
            val ranks = new java.util.HashMap[RegionKey, Integer]()
            passMembers(kids).foreach { kid =>
                if kid.mounted.isEmpty && !kid.isConst then
                    val first = RegionKey(rowKeys(kid.path, kid.rows, kid.deepRows), kid.origin.fold[AnyRef](kid.path)(f => f), 0)
                    val rank  = Maybe(ranks.get(first)).fold(0)(_.intValue + 1)
                    discard(ranks.put(first, rank))
                    discard(result.put(kid, if rank == 0 then first else first.copy(rank = rank)))
            }
            result
        end keys

        def of(kid: ReactiveUI): RegionKey =
            Maybe(keys.get(kid)).getOrElse(RegionKey(Nil, kid.path, 0))

        /** The keys of every region in the pass. */
        def all: Set[RegionKey] =
            val it  = keys.values.iterator
            var set = Set.empty[RegionKey]
            while it.hasNext do set += it.next()
            set
        end all
    end RegionKeys

    private[kyo] object RegionKeys:
        val empty: RegionKeys = new RegionKeys(Seq.empty)

        def apply(kids: Seq[ReactiveUI]): RegionKeys = new RegionKeys(kids)
    end RegionKeys

    /** A live mount: its content cell, the fiber running its effect, the `Scope` that owns what the effect acquired, and the table of
      * keyed mounts nested in its content.
      */
    final private[kyo] class MountInstance(
        val cell: SignalRef[MountState],
        val children: MountTableRef,
        runner: Fiber[Unit, Any],
        scope: Scope.Finalizer
    ):
        /** What the slot keeps showing under `KeepLatest` once this instance is gone. */
        def snapshot(using Frame): Maybe[UI] < Sync =
            cell.current.map {
                case MountState.Pending(snapshot) => snapshot
                case MountState.Ready(ui)         => resolveReactives(ui).map(Present(_))
                case MountState.Failed(ui)        => resolveReactives(ui).map(Present(_))
            }

        /** Stops the effect if it still runs, then releases nested mounts and everything the effect acquired, waiting for all of it.
          * The wait is explicit because an interrupted `Scope` releases on a fiber of its own that nothing awaits.
          */
        def teardown(using Frame): Unit < Async =
            runner.interrupt
                .andThen(runner.getResult.unit)
                .andThen(children.evictAll)
                .andThen(scope.close(Absent))
                .andThen(scope.await)
    end MountInstance

    private[kyo] object MountInstance:
        def start(spec: MountedSpec, seed: Maybe[UI])(using Frame): MountInstance < Sync =
            for
                cell   <- Signal.initRef[MountState](MountState.Pending(seed))
                scope  <- Scope.Finalizer.init(1)
                runner <- Fiber.initUnscoped(runEffect(spec, cell, scope))
            yield new MountInstance(cell, MountTableRef.init(), runner, scope)

        /** Runs the effect with `scope` as its `Scope`, so what it acquires outlives this fiber, and publishes the outcome. */
        private def runEffect(spec: MountedSpec, cell: SignalRef[MountState], scope: Scope.Finalizer)(using Frame): Unit < Async =
            val supervision = MountSupervision(spec, cell)
            val effect      = MountSupervision.local.let(Present(supervision))(spec.node.effect)
            Abort.run[Throwable](ContextEffect.handleInheritable(Tag[Scope], scope)(effect)).map {
                case Result.Success(ui) => cell.getAndUpdate(MountSupervision.publish(ui)).unit
                case Result.Failure(t)  =>
                    Log.error(s"Mounted effect failed (frame=${spec.node.frame.position.show})", t)
                        .andThen(supervision.fail(t, UI.MountErrorSource.Effect))
                case Result.Panic(t) =>
                    if t.isInstanceOf[Interrupted] then Kyo.unit
                    else
                        Log.error(s"Mounted effect panicked (frame=${spec.node.frame.position.show})", t)
                            .andThen(supervision.fail(t, UI.MountErrorSource.Panic))
            }
        end runEffect
    end MountInstance

    /** A fiber forked with [[kyo.UI.fork]] failed with an `Abort` value that is not a `Throwable`. */
    final private[kyo] case class MountFiberFailure(value: Any)(using Frame)
        extends KyoException(s"Mounted background fiber failed: $value")

    /** Routes the failures of one mount instance into its content cell. The first failure wins, and a publish that arrives after a
      * failure does not overwrite it.
      */
    final private[kyo] class MountSupervision(spec: MountedSpec, cell: SignalRef[MountState]):

        /** Completion callback of a [[kyo.UI.fork]] fiber: a terminal failure that is not an interrupt fails the node. */
        def report[E, A](result: Result[E, A])(using Frame): Unit < Sync =
            result match
                case Result.Panic(_: Interrupted)   => Kyo.unit
                case Result.Failure(_: Interrupted) => Kyo.unit
                case Result.Panic(t)                => forked(t)
                case Result.Failure(t: Throwable)   => forked(t)
                case Result.Failure(other)          => forked(MountFiberFailure(other))
                case _                              => Kyo.unit

        private def forked(t: Throwable)(using Frame): Unit < Sync =
            Log.error(s"Mounted background fiber failed (frame=${spec.node.frame.position.show})", t)
                .andThen(fail(t, UI.MountErrorSource.Fork))

        def fail(t: Throwable, source: UI.MountErrorSource)(using Frame): Unit < Sync =
            val failed = MountState.Failed(spec.errorUI(spec.mountError(t, source)))
            cell.getAndUpdate {
                case done: MountState.Failed => done
                case _                       => failed
            }.unit
        end fail
    end MountSupervision

    private[kyo] object MountSupervision:
        def publish(ui: UI)(state: MountState): MountState =
            state match
                case done: MountState.Failed => done
                case _                       => MountState.Ready(ui)

        /** The mount instance whose effect is running, read by [[kyo.UI.fork]]. Inheritable, so a `UI.fork` inside a fiber the effect
          * forked still reports to the same node. Installed around the effect only: engine fibers never see it.
          */
        private[kyo] val local: Local[Maybe[MountSupervision]] = Local.init(Maybe.empty[MountSupervision])
    end MountSupervision

    /** The mount instances of one reactive region, created by the first mounted node at or below the region, so a region without one
      * allocates nothing beyond this reference and skips every step below.
      *
      * The table belongs to the table of the enclosing region, under this region's path, not to this region's subscription. When an
      * enclosing region re-renders and subscribes this one again at the same path, the new subscription gets the same table, and its
      * keyed instances carry on: their continuity does not end at the nearest region.
      *
      * The old subscription of that region stops only once the enclosing region's previous value has released, which can be after
      * the new one started. So a table follows the newest reference bound to it, and an older one that still runs a pass changes
      * nothing in it: it releases nothing, records nothing, and a node it subscribes shows the live instance of its identity, or one
      * of its own that its scope releases.
      */
    final private[kyo] class MountTableRef private (
        slot: AtomicRef.Unsafe[Maybe[MountTable]],
        parent: Maybe[MountTableRef],
        keys: RegionKeys,
        kid: Maybe[ReactiveUI]
    ):
        import AllowUnsafe.embrace.danger

        /** Creation order: a later subscription of a region gets a larger serial, whichever binds the table first. */
        val serial: Long = MountTableRef.serials.incrementAndGet()

        /** The reference for the region `region` of the render pass `pass` inside this one, holding the table an earlier subscription
          * of that region created. The region's key is only computed when this table exists, or once the region forces its own.
          */
        def child(pass: RegionKeys, region: ReactiveUI): MountTableRef =
            val existing = slot.get().flatMap(table => table.region(pass.of(region)))
            val ref      = new MountTableRef(AtomicRef.Unsafe.init(existing), Present(this), pass, Present(region))
            existing.foreach(_.bind(ref))
            ref
        end child

        private def force(using Frame): MountTable < Sync =
            Sync.Unsafe.defer(slot.get()).map {
                case Present(table) => Kyo.lift(table)
                case Absent         =>
                    val table: MountTable < Sync = parent match
                        case Present(enclosing) =>
                            enclosing.force.map(t => Sync.Unsafe.defer(t.adoptRegion(keys.of(kid.get), new MountTable)))
                        case Absent => Sync.Unsafe.defer(new MountTable)
                    table.map { fresh =>
                        Sync.Unsafe.defer {
                            discard(slot.compareAndSet(Absent, Present(fresh)))
                            val bound = slot.get().getOrElse(fresh)
                            bound.bind(this)
                            bound
                        }
                    }
            }

        private def owned: Maybe[MountTable] = slot.get().filter(_.ownedBy(this))

        /** Opens a render pass of the region: tears down, and waits for, every keyed instance the new tree no longer shows, before any
          * new instance starts.
          */
        def beginWalk(kids: Seq[ReactiveUI], pass: RegionKeys)(using Frame): Unit < Async =
            Sync.Unsafe.defer(owned).map {
                case Absent         => Kyo.unit
                case Present(table) => table.evictExcept(kids, pass)
            }

        def endWalk(using Frame): Unit < Sync =
            Sync.Unsafe.defer(owned.foreach(_.endWalk()))

        def evictAll(using Frame): Unit < Async =
            Sync.Unsafe.defer(owned).map {
                case Absent         => Kyo.unit
                case Present(table) => table.evictAll
            }

        /** The instance for the keyed node `rui`: the one this pass gave it, or, for a reference the table no longer follows, the live
          * instance of its identity, or else one of its own that the caller's scope releases.
          */
        def claim(rui: ReactiveUI, identity: MountIdentity, spec: MountedSpec)(using Frame): MountInstance < (Async & Scope) =
            force.map { table =>
                if table.ownedBy(this) then table.claim(rui, identity, spec)
                else
                    table.live(identity, spec.path) match
                        case Present(instance) => Kyo.lift(instance)
                        case Absent            => MountInstance.start(spec, Absent).map(i => Scope.ensure(i.teardown).andThen(i))
            }

        /** A keyless instance for `spec`, released with the caller's scope; only the reference the table follows leaves its snapshot. */
        def startKeyless(spec: MountedSpec)(using Frame): MountInstance < (Async & Scope) =
            force.map { table =>
                table.start(spec).map { instance =>
                    Scope.ensure(if table.ownedBy(this) then table.retire(instance, spec.path) else instance.teardown).andThen(instance)
                }
            }
    end MountTableRef

    private[kyo] object MountTableRef:
        private val serials = new java.util.concurrent.atomic.AtomicLong()

        /** A reference no enclosing table owns: the root of a session, or the content of a mount instance. */
        def init(): MountTableRef =
            new MountTableRef(
                AtomicRef.Unsafe.init(Maybe.empty[MountTable])(using AllowUnsafe.embrace.danger),
                Absent,
                RegionKeys.empty,
                Absent
            )
    end MountTableRef

    /** One keyed instance of a [[MountTable]] and the path it was last shown at. A slot a pass reserves for a node that had no instance
      * is filled by that node's claim.
      */
    final private class MountSlot(@volatile var path: Seq[String]):
        @volatile var instance: Maybe[MountInstance] = Absent

    /** Keyed instances by identity, the tables of the regions directly inside this one by [[RegionKey]], and the state of the current render
      * pass: which slot each keyed node of the pass takes, and the snapshots left by instances that ended, keyed by the path they were
      * shown at, for a `KeepLatest` successor at the same place.
      *
      * An identity usually belongs to one node. When a pass shows it several times (one helper, one key, several places), each node
      * gets a slot of its own: a node takes the slot last shown at its path, and the rest are handed out in order, so nodes that keep
      * their places keep their instances and one that goes away takes only its own.
      *
      * The instances and the pass state are written only from the region's own observe loop; the region tables also from the loops of
      * the regions inside it, and the snapshots from the finalizer of a keyless instance, which runs as a loop moves on.
      */
    final private[kyo] class MountTable:
        import AllowUnsafe.embrace.danger

        private val instances = AtomicRef.Unsafe.init(Dict.empty[MountIdentity, Chunk[MountSlot]])
        private val regions   = AtomicRef.Unsafe.init(Dict.empty[RegionKey, MountTable])
        private val assigned  = AtomicRef.Unsafe.init(Maybe.empty[java.util.IdentityHashMap[ReactiveUI, MountSlot]])
        private val handoff   = AtomicRef.Unsafe.init(Dict.empty[Seq[String], UI])
        private val owner     = AtomicRef.Unsafe.init(Maybe.empty[MountTableRef])

        /** Makes `ref` the reference this table follows, unless a newer one already is. */
        def bind(ref: MountTableRef): Unit =
            discard(owner.getAndUpdate(current => if current.exists(_.serial > ref.serial) then current else Present(ref)))

        def ownedBy(ref: MountTableRef): Boolean = owner.get().exists(_ eq ref)

        /** The running instance of `identity`, the one last shown at `path` when there are several. */
        def live(identity: MountIdentity, path: Seq[String]): Maybe[MountInstance] =
            instances.get().get(identity).flatMap { slots =>
                Maybe.fromOption(slots.find(slot => slot.instance.nonEmpty && slot.path == path).orElse(slots.find(_.instance.nonEmpty)))
                    .flatMap(_.instance)
            }

        def region(key: RegionKey): Maybe[MountTable] = regions.get().get(key)

        /** The table registered for the region `key`, which becomes `fresh` when there is none yet. */
        def adoptRegion(key: RegionKey, fresh: MountTable): MountTable =
            regions.getAndUpdate(m => if m.contains(key) then m else m.update(key, fresh)).get(key).getOrElse(fresh)

        /** The instance of the slot this pass gave `rui`, started if the slot is empty. A node no pass has seen belongs to a table the
          * pass created, which holds nothing yet, so it starts a new instance.
          */
        def claim(rui: ReactiveUI, identity: MountIdentity, spec: MountedSpec)(using Frame): MountInstance < Sync =
            Sync.Unsafe.defer(assigned.get().flatMap(slots => Maybe(slots.get(rui)))).map {
                case Present(slot) =>
                    slot.instance match
                        case Present(instance) => Kyo.lift(instance)
                        case Absent => start(spec).map(instance => Sync.Unsafe.defer { slot.instance = Present(instance); instance })
                case Absent =>
                    start(spec).map { instance =>
                        Sync.Unsafe.defer {
                            val slot = new MountSlot(spec.path)
                            slot.instance = Present(instance)
                            discard(instances.getAndUpdate(m => m.update(identity, m.get(identity).getOrElse(Chunk.empty).append(slot))))
                            instance
                        }
                    }
            }

        /** A new instance, seeded with the snapshot the slot's previous instance left when the node keeps the latest content. */
        def start(spec: MountedSpec)(using Frame): MountInstance < Sync =
            val seed = spec.node.pendingPolicy match
                case UI.PendingPolicy.Placeholder => Absent
                case UI.PendingPolicy.KeepLatest  => handoff.get().get(spec.path)
            MountInstance.start(spec, seed)
        end start

        /** Tears down `instance` and leaves its snapshot at `path` for this pass's successor. */
        def retire(instance: MountInstance, path: Seq[String])(using Frame): Unit < Async =
            instance.snapshot.map { snapshot =>
                Sync.Unsafe.defer(snapshot.foreach(ui => discard(handoff.getAndUpdate(_.update(path, ui)))))
            }.andThen(instance.teardown)

        /** Opens a render pass over `kids`: releases every keyed instance and every region table the pass no longer shows, and waits
          * for all of it. The instances leave the table before they are released, so the release runs out of reach of an interrupt: an
          * outer re-render that interrupts this region's loop ends the wait, not the release.
          */
        def evictExcept(kids: Seq[ReactiveUI], pass: RegionKeys)(using Frame): Unit < Async =
            Sync.Unsafe.defer {
                // The pass's keyed nodes by identity, each group in walk order.
                val groups = new java.util.HashMap[MountIdentity, java.util.ArrayList[ReactiveUI]]()
                passMembers(kids).foreach { kid =>
                    kid.mounted.flatMap(_.identity).foreach { identity =>
                        discard(groups.computeIfAbsent(identity, _ => new java.util.ArrayList[ReactiveUI](1)).add(kid))
                    }
                }
                val previous  = instances.get()
                val slots     = new java.util.IdentityHashMap[ReactiveUI, MountSlot]()
                var kept      = Dict.empty[MountIdentity, Chunk[MountSlot]]
                var goneSlots = Chunk.empty[MountSlot]
                groups.forEach { (identity, nodes) =>
                    val old           = previous.get(identity).getOrElse(Chunk.empty)
                    val (taken, left) =
                        if nodes.size == 1 && old.size <= 1 then
                            (if old.isEmpty then Chunk(new MountSlot(nodes.get(0).path)) else old, Chunk.empty)
                        else pair(nodes, old)
                    var i = 0
                    while i < nodes.size do
                        val slot = taken(i)
                        slot.path = nodes.get(i).path
                        discard(slots.put(nodes.get(i), slot))
                        i += 1
                    end while
                    kept = kept.update(identity, taken)
                    goneSlots = goneSlots.concat(left)
                }
                previous.foreach { (identity, old) =>
                    if !groups.containsKey(identity) then goneSlots = goneSlots.concat(old)
                }
                instances.set(kept)
                assigned.set(Present(slots))
                val keepRegions     = if regions.get().isEmpty then Set.empty[RegionKey] else pass.all
                val previousRegions = regions.getAndUpdate(_.filter((key, _) => keepRegions.contains(key)))
                val goneRegions     = previousRegions.foldLeft(Chunk.empty[MountTable]) { (acc, key, table) =>
                    if keepRegions.contains(key) then acc else acc.append(table)
                }
                if goneRegions.isEmpty && goneSlots.isEmpty then Kyo.unit
                else
                    Async.uninterruptible {
                        Kyo.foreachDiscard(goneRegions)(_.evictAll)
                            .andThen(Kyo.foreachDiscard(goneSlots)(slot => slot.instance.fold(Kyo.unit)(retire(_, slot.path))))
                    }
                end if
            }
        end evictExcept

        /** The slots of `old` for the nodes of one identity, in the nodes' order: a slot last shown at a node's path goes to that node,
          * the others in order, and nodes left over get new slots. Also answers the slots no node took.
          */
        private def pair(nodes: java.util.ArrayList[ReactiveUI], old: Chunk[MountSlot]): (Chunk[MountSlot], Chunk[MountSlot]) =
            val free   = scala.collection.mutable.ArrayBuffer.from(old)
            val result = new Array[MountSlot](nodes.size)
            var i      = 0
            while i < nodes.size do
                val at = free.indexWhere(_.path == nodes.get(i).path)
                if at >= 0 then result(i) = free.remove(at)
                i += 1
            end while
            i = 0
            while i < nodes.size do
                if result(i) == null then
                    result(i) = if free.nonEmpty then free.remove(0) else new MountSlot(nodes.get(i).path)
                i += 1
            end while
            (Chunk.from(result), Chunk.from(free))
        end pair

        def evictAll(using Frame): Unit < Async = evictExcept(Seq.empty, RegionKeys.empty)

        /** Ends a pass. The slots stay assigned until the next pass, so a node that subscribes late still finds its own. */
        def endWalk(): Unit =
            handoff.set(Dict.empty)
    end MountTable

    /** Subscribes a mounted node in the current scope. A keyed node adopts or creates its instance in the enclosing region's table; a
      * keyless node gets a new instance that ends with the current scope, which for a node inside a region is the region's current
      * value.
      */
    private def subscribeMounted(
        rui: ReactiveUI,
        spec: MountedSpec,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        mounts: MountTableRef,
        dispatch: MountDispatch
    )(using Frame): Unit < (Async & Scope) =
        val instance = spec.identity match
            case Present(identity) => mounts.claim(rui, identity, spec)
            case Absent            => mounts.startKeyless(spec)
        instance.map(subscribeMountContent(rui, spec, _, exchange, signalChangeTime, dispatch))
    end subscribeMounted

    /** Shows an instance's content cell in the node's range. The content as it is now is painted at once, so a kept instance replaces
      * the placeholder its enclosing region just rendered without a visible gap; then a region over the cell follows it. Live content
      * gets its regions and nested mounts subscribed; a snapshot is only painted.
      */
    private def subscribeMountContent(
        rui: ReactiveUI,
        spec: MountedSpec,
        instance: MountInstance,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        dispatch: MountDispatch
    )(using Frame): Unit < (Async & Scope) =
        val svgContext = rui.region match
            case _: ReactiveRegion.HtmlRange  => false
            case _: ReactiveRegion.SvgElement => true
        for
            _     <- dispatch.register(rui.path, instance.cell)
            _     <- Scope.ensure(dispatch.unregister(rui.path, instance.cell))
            first <- instance.cell.current
            // Instrumented like any other paint: every `onChange` the engine issues is reported, or the wasted-render figure
            // quietly excludes a whole class of paint and a page full of mounts reads as cleaner than it is.
            paint = (cause: UI.RenderCause, previous: Maybe[UI], content: UI) =>
                for
                    _ <- measuredPaint(rui, UI.RenderKind.Repaint, cause)(
                        exchange.onChange(rui.region, rui.path, rui.contentContext, rui.parentContext, previous, content)
                    )
                yield ()
            _ <- first match
                case MountState.Pending(Absent) => Kyo.unit
                case state                      => paint(UI.RenderCause.Created, Absent, state.content(spec))
            // Unsafe: single-writer state of this region's own loop, as in subscribeScoped.
            painted     = AtomicRef.Unsafe.init[MountState](first)(using AllowUnsafe.embrace.danger)
            renderState = (state: MountState) =>
                val content = state.content(spec)
                for
                    previous <- Sync.Unsafe.defer(painted.getAndSet(state))
                    _        <-
                        if previous eq state then Kyo.unit
                        else
                            Clock.now.map(signalChangeTime.set).andThen(
                                paint(UI.RenderCause.Signal, Present(previous.content(spec)), content)
                            )
                    _ <- state match
                        case MountState.Pending(Present(_)) => Kyo.unit
                        case _                              =>
                            for
                                (kids, _) <- Sync.Unsafe.defer(walkStatic(
                                    content,
                                    rui.path,
                                    svgContext,
                                    rui.contentContext,
                                    nestedParentContext(rui.parentContext, content),
                                    dispatch,
                                    rows = 0L,
                                    deepRows = Nil
                                ))
                                keys = RegionKeys(kids)
                                _ <- instance.children.beginWalk(kids, keys)
                                _ <- forkRootChannels(rui.path, content, exchange)
                                _ <- Kyo.foreachDiscard(kids)(
                                    subscribeScoped(_, exchange, signalChangeTime, instance.children, dispatch, keys)
                                )
                                _ <- instance.children.endWalk
                            yield ()
                yield ()
                end for
            _ <- startOwnedFiber(
                Abort.run[Throwable](instance.cell.observe(renderState)).map { result =>
                    result.fold(
                        _ => (),
                        err => Log.error(s"Mounted content region failed at path=${rui.path.mkString(".")}", err),
                        panic =>
                            if panic.isInstanceOf[Interrupted] then ()
                            else Log.error(s"Mounted content region failed at path=${rui.path.mkString(".")}", panic)
                    )
                }
            )
        yield ()
        end for
    end subscribeMountContent

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
