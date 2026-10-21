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
    frame: Frame = Frame.internal,
    bounded: Maybe[UI.Ast.Bounded] = Absent
):
    /** Whether a mount is at or below this node, through regions but not into a mount's content: what a pass must record. */
    lazy val containsMount: Boolean = mounted.nonEmpty || children.exists(_.containsMount)
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
    rows: ReactiveUI.RowRegistry
):
    def reusable: Boolean = node.key.nonEmpty && !node.indexed
end ForeachSpec

/** Where event dispatch finds a mounted node's live content.
  *
  * Dispatch re-derives handler chains from the current UI on every event, so the `Mounted` node it meets is a fresh value that cannot
  * carry the instance. The instance registers its content cell here while it is subscribed, and the dispatch-time handler of whatever
  * node occupies that place reads it. One table per normalized root.
  *
  * A path alone does not name one node: a mount's content sits at the mount's own path, so a mount whose content is directly another
  * mount, or a boundary around one, puts two of them there. Nor does the call site, since a helper that takes a `Frame` makes every
  * node in it under the caller's. An entry is therefore filed under the identity of the content it owns, the path plus how many
  * mounts, boundaries and regions enclose it there, which is the identity its range has on the page. By path alone, the inner
  * mount's boundary found the outer one before the inner instance had registered, the outer showed its content, which held the inner
  * boundary again, and resolution recursed without end.
  */
final private[kyo] class MountDispatch(
    cells: AtomicRef[Dict[ReactiveRegion.RegionIdentity, Chunk[ReactiveUI.MountInstance]]],
    gates: AtomicRef[Dict[ReactiveRegion.RegionIdentity, Chunk[BoundaryGate]]],
    holds: AtomicRef[Chunk[Seq[String]]]
):
    /** A range a boundary keeps showing old content in while its new content loads: no event may reach the new content through it. */
    def hold(path: Seq[String])(using Frame): Unit < Sync = holds.getAndUpdate(_.append(path)).unit

    def release(path: Seq[String])(using Frame): Unit < Sync =
        holds.getAndUpdate { all =>
            val at = all.indexOf(path)
            if at < 0 then all else all.take(at).concat(all.drop(at + 1))
        }.unit

    /** Whether `target` lies in a range that is held. */
    def held(target: Seq[String])(using Frame): Boolean < Sync = holds.get.map(_.exists(target.startsWith(_)))

    /** Registers `gate` of the boundary whose content has identity `at`. */
    def registerBoundary(at: ReactiveRegion.RegionIdentity, gate: BoundaryGate)(using Frame): Unit < Sync =
        gates.getAndUpdate(m => m.update(at, m.get(at).getOrElse(Chunk.empty).append(gate))).unit

    def unregisterBoundary(at: ReactiveRegion.RegionIdentity, gate: BoundaryGate)(using Frame): Unit < Sync =
        gates.getAndUpdate { m =>
            m.get(at) match
                case Absent        => m
                case Present(held) =>
                    val left = held.filterNot(_ eq gate)
                    if left.isEmpty then m.remove(at) else m.update(at, left)
        }.unit

    /** The gate registered last for the boundary whose content has identity `at`. */
    def boundary(at: ReactiveRegion.RegionIdentity)(using Frame): Maybe[BoundaryGate] < Sync =
        gates.get.map(_.get(at).flatMap(_.lastMaybe))

    /** Registers `instance` of the mount whose content has identity `at`. */
    def register(at: ReactiveRegion.RegionIdentity, instance: ReactiveUI.MountInstance)(using Frame): Unit < Sync =
        cells.getAndUpdate(m => m.update(at, m.get(at).getOrElse(Chunk.empty).append(instance))).unit

    /** Removes this registration only. Two subscriptions can hold one path for a moment, an old one still ending and its successor,
      * in either order; the other keeps its entry.
      */
    def unregister(at: ReactiveRegion.RegionIdentity, instance: ReactiveUI.MountInstance)(using Frame): Unit < Sync =
        cells.getAndUpdate { m =>
            m.get(at) match
                case Absent        => m
                case Present(held) =>
                    val i    = held.lastIndexWhere(_ eq instance)
                    val left = if i < 0 then held else held.take(i).concat(held.drop(i + 1))
                    if left.isEmpty then m.remove(at) else m.update(at, left)
        }.unit

    /** The cell of the instance registered last for the mount whose content has identity `at`. */
    def lookup(at: ReactiveRegion.RegionIdentity)(using Frame): Maybe[Signal[ReactiveUI.MountState]] < Sync =
        instance(at).map(_.map(_.cell))

    /** The instance registered last for the mount whose content has identity `at`. */
    def instance(at: ReactiveRegion.RegionIdentity)(using Frame): Maybe[ReactiveUI.MountInstance] < Sync =
        cells.get.map(_.get(at).flatMap(_.lastMaybe))
end MountDispatch

private[kyo] object MountDispatch:
    def init(using Frame): MountDispatch < Sync = Sync.Unsafe.defer(initNow())

    /** The table for a pass that already holds the thread (see [[SignalNow]]). */
    def initNow()(using AllowUnsafe): MountDispatch =
        new MountDispatch(
            AtomicRef.Unsafe.init(Dict.empty[ReactiveRegion.RegionIdentity, Chunk[ReactiveUI.MountInstance]]).safe,
            AtomicRef.Unsafe.init(Dict.empty[ReactiveRegion.RegionIdentity, Chunk[BoundaryGate]]).safe,
            AtomicRef.Unsafe.init(Chunk.empty[Seq[String]]).safe
        )
end MountDispatch

/** A `Mounted` node as normalization found it: the node, its render path, and the keys of the list rows between it and the owner of
  * its mount table, innermost first.
  */
final private[kyo] case class MountedSpec(node: UI.Ast.Mounted, path: Seq[String], rows: List[String]):
    def placeholder: UI = node.placeholderUI.getOrElse(UI.empty(using node.frame))

    /** The node's own error view, else the first view of the boundaries around it, innermost first, that matches, else a built-in one. */
    def errorUI(error: UI.MountError, views: List[PartialFunction[UI.MountError, UI]] = Nil): UI =
        node.errorRender match
            case Present(render) => render(error)
            case Absent          =>
                views.find(_.isDefinedAt(error)) match
                    case Some(view) => view(error)
                    case None       =>
                        given Frame = node.frame
                        UI.span(UI.Ast.Text(error.message)).cssClass("kyo-mount-error")

    def mountError(failure: Any, source: UI.MountErrorSource): UI.MountError =
        val t = failure match
            case t: Throwable => t
            case other        => UI.FailureException(other)(using node.frame)
        UI.MountError(t, source, Maybe(t.getMessage).getOrElse(t.toString), node.frame, path, node.key)
    end mountError

    /** The instance identity of a keyed node; a keyless node has none, since every re-render replaces its instance. `rows` are the keys of
      * the list rows between the node and the owner of its table, the session root or the mount instance whose content it is in, across
      * every region in between.
      */
    def identity: Maybe[ReactiveUI.MountIdentity] =
        node.key.map(ReactiveUI.MountIdentity(rows, node.site, _))
end MountedSpec

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
        deepRows: List[Int] = Nil
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
    )(using AllowUnsafe, SignalNow.Locals): ReactiveUI =
        given Frame = ui.frame
        ui match
            case ui: Reactive[?] =>
                val contentContext       = regionIdentity.transparent
                val current              = SignalNow(ui.signal)
                val contentParentContext = nestedParentContext(parentContext, current)
                val (kids, _)            = walkStatic(current, path, svg, contentContext, contentParentContext, cells, rows, deepRows)
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
                                walkStatic(currentUI, path, svg, contentContext, contentParentContext, cells, rows, deepRows)
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
                            walkStatic(current, path, svg, contentContext, contentParentContext, cells, rows, deepRows)
                        // Fallback dispatch: re-render the whole list from the current value and route
                        // through the fresh fragment handler. Used whenever the row registry cannot
                        // resolve the target (region not subscribed on the reuse path, row painted but
                        // not yet registered, or an event addressed to the region node itself).
                        val fullWalk: Handler = (targetPath, event) =>
                            Sync.Unsafe.defer {
                                val currentUI          = SignalNow(sig)
                                val freshParentContext = nestedParentContext(parentContext, currentUI)
                                val (_, freshHdl)      =
                                    walkStatic(currentUI, path, svg, contentContext, freshParentContext, cells, rows, deepRows)
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
                    at = regionIdentity,
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

            case ui: Mounted if Boundaries.implicitOf(ui).nonEmpty =>
                normalizeAt(Boundaries.implicitOf(ui).get, path, svg, regionIdentity, parentContext, cells, rows, deepRows)

            case b: Bounded =>
                // A region of its own, so its mounts get a table that carries its error view, at the path of the node it wraps.
                val contentContext       = regionIdentity.transparent
                val contentParentContext = nestedParentContext(parentContext, b.child)
                val (kids, childHandle)  =
                    walkStatic(b.child, path, svg, contentContext, contentParentContext, cells, rows, deepRows)
                // Content a boundary holds back is not on the page, so no event reaches it, while its placeholder is and takes them;
                // and a range that keeps showing old content while its new content loads takes no event, which would otherwise land
                // in the new content.
                val handle: Handler = (targetPath, event) =>
                    Failures.within(FailureFrame.Boundary(b.settings))(cells.boundary(contentContext).map {
                        case Present(gate) if !gate.isRevealed =>
                            gate.livePlaceholder match
                                case Present(ph) =>
                                    Sync.Unsafe.defer {
                                        val (_, placeholderHandle) = walkStatic(
                                            ph,
                                            path,
                                            svg,
                                            contentContext,
                                            nestedParentContext(parentContext, ph),
                                            cells,
                                            rows,
                                            deepRows
                                        )
                                        placeholderHandle(targetPath, event)
                                    }
                                case Absent => true
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

            case ui: Mounted =>
                val spec           = MountedSpec(ui, path, rowKeys(path, rows, deepRows))
                val contentContext = regionIdentity.transparent
                // The node value is fresh on every dispatch, so the handler finds the live instance through the dispatch table.
                val handle: Handler = (targetPath, event) =>
                    cells.instance(contentContext).map {
                        case Absent           => true
                        case Present(running) =>
                            running.cell.current.map {
                                case MountState.Pending(Present(_)) => true
                                case state                          =>
                                    val content = state.content(spec)
                                    Failures.within(FailureFrame.Mount(running.supervision))(Sync.Unsafe.defer {
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
                                    })
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
        case _: Reactive[?] | _: Foreach[?, ?] | _: Mounted | _: Bounded => true
        case el: Element                                                 =>
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
        cells: MountDispatch,
        outerRows: Long,
        outerDeepRows: List[Int]
    )(using AllowUnsafe, Frame, SignalNow.Locals): (Seq[ReactiveUI], Handler) =
        val depth    = rowPath.size - 1
        val rows     = if depth < 63 then outerRows | (1L << depth) else outerRows
        val deepRows = if depth < 63 then outerDeepRows else depth :: outerDeepRows
        if needsOwnNode(rowUI) then
            val rui = normalizeAt(rowUI, rowPath, svg, context, parentContext, cells, rows, deepRows)
            (Seq(rui), rui.handle)
        else walkStatic(rowUI, rowPath, svg, context, parentContext, cells, rows, deepRows)
        end if
    end walkRow

    private type Handler = (Seq[String], UIEvent) => Boolean < (Async & Abort[Any])

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
    private def currentValues[A](channels: Map[String, Signal[A]])(using AllowUnsafe, Frame, SignalNow.Locals): Map[String, A] =
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
    )(using AllowUnsafe, Frame, SignalNow.Locals): (Seq[ReactiveUI], Handler) =
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
                    dispatch(elem, basePath, context, targetPath, event, reactiveChildren, staticHandlers, cells)
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
                                .fold(true: Boolean < (Async & Abort[Any]))(h => h(targetPath, event))
                        else true
                    (allKids, handle)
                }

            case _: Reactive[?] | _: Foreach[?, ?] | _: Mounted | _: Bounded =>
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
        at: ReactiveRegion.RegionIdentity,
        targetPath: Seq[String],
        event: UIEvent,
        reactiveChildren: Seq[ReactiveUI],
        staticHandlers: Seq[(Int, Handler)],
        mounts: MountDispatch
    )(using Frame): Boolean < (Async & Abort[Any]) =
        elem.attrs.onFailure match
            case Present(rules) => Failures.within(FailureFrame.Rules(rules))(dispatchThrough(
                    elem,
                    myPath,
                    at,
                    targetPath,
                    event,
                    reactiveChildren,
                    staticHandlers,
                    mounts
                ))
            case Absent => dispatchThrough(elem, myPath, at, targetPath, event, reactiveChildren, staticHandlers, mounts)

    /** [[dispatch]] with the element's `onFailure` rules in effect. A handler's failure goes to the rules and the dispatch goes on. */
    private def dispatchThrough(
        elem: Element,
        myPath: Seq[String],
        at: ReactiveRegion.RegionIdentity,
        targetPath: Seq[String],
        event: UIEvent,
        reactiveChildren: Seq[ReactiveUI],
        staticHandlers: Seq[(Int, Handler)],
        mounts: MountDispatch
    )(using Frame): Boolean < (Async & Abort[Any]) =
        if targetPath == myPath then
            Failures.recover(true)(dispatchToElement(elem, event, isTarget = true, selfPath = myPath))
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
                    if isClick || isKeyDown then targetFacts(elem, myPath, at, targetPath, mounts)
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
                bubble          = Failures.recover(true)(dispatchToElement(
                    elem,
                    event,
                    isTarget = false,
                    selfPath = myPath,
                    disabledTarget = targetDisabled,
                    submitOrigin = isClick && targetButtonSubmits.getOrElse(false),
                    enterSubmits = targetEnterSubmits,
                    controlTarget = targetOnControl
                ))
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
    private def submitDirectly(f: Form)(using Frame): Unit < (Async & Abort[Any]) =
        activationTarget.use {
            case Present(ref) => ref.get.map(recorded => if recorded.isDefined then Kyo.lift(()) else invoke(f.onSubmit))
            case Absent       => invoke(f.onSubmit)
        }

    /** Invoke a Maybe[UI.Action] handler, discarding the result. */
    private def invoke(handler: Maybe[UI.Action])(using Frame): Unit < (Async & Abort[Any]) =
        if handler.isEmpty then () else handler.get.unit

    /** Invoke a Maybe[A => UI.Action] handler with a value, discarding the result. */
    private def invokeWith[A](handler: Maybe[A => UI.Action], value: A)(using Frame): Unit < (Async & Abort[Any]) =
        if handler.isEmpty then () else handler.get(value).unit

    /** Dispatch an event safely, logging and recovering from handler errors without stopping the bubble chain. */
    private def safeDispatch(handle: Handler, path: Seq[String], event: UIEvent)(using
        Frame
    ): Boolean < Async =
        DragCommands.current.use { context =>
            val decisions = context.flatMap(_.decisions)
            Abort.recover[Any](
                onFail = err =>
                    Log.error(s"Handler error during ${event.getClass.getSimpleName} at ${path.mkString(".")}: $err")
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
    private def cycleSelectOption(sel: Select, forward: Boolean)(using Frame): Unit < (Async & Abort[Any]) =
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
    private def unlessInert(isTarget: Boolean, inert: => Boolean < Sync)(body: => Boolean < (Async & Abort[Any]))(using
        Frame
    ): Boolean < (Async & Abort[Any]) =
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
        at: ReactiveRegion.RegionIdentity,
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
                resolveTarget(kc.child, nodePath, at, targetPath, rootDepth, controlBelow, mounts)
            case r: Reactive[?] =>
                // A Reactive's rendered content occupies the same path as the boundary, so re-resolve at nodePath.
                r.signal.current(using r.frame).map(cur =>
                    resolveTarget(cur, nodePath, at.transparent, targetPath, rootDepth, controlBelow, mounts)
                )
            case Fragment(children) =>
                if targetPath.size <= nodePath.size then unresolved(controlBelow)
                else
                    val seg = targetPath(nodePath.size)
                    Maybe.fromOption(seg.toIntOption) match
                        case Present(i) if i >= 0 && i < children.size =>
                            resolveTarget(children(i), nodePath :+ seg, at.child(seg), targetPath, rootDepth, controlBelow, mounts)
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
                                    resolveTarget(
                                        renderFn(idx, items(idx)),
                                        nodePath :+ seg,
                                        at.child(seg),
                                        targetPath,
                                        rootDepth,
                                        controlBelow,
                                        mounts
                                    )
                                else unresolved(controlBelow)
                                end if
                            }
                    }
            case e: Element =>
                val seen = controlBelow || (nodePath.size > rootDepth && isOwnControl(e))
                if targetPath.size <= nodePath.size then TargetFacts(Present(e), seen)
                else
                    val seg = targetPath(nodePath.size)
                    Maybe.fromOption(seg.toIntOption) match
                        case Present(i) if i >= 0 && i < e.children.size =>
                            resolveTarget(e.children(i), nodePath :+ seg, at.child(seg), targetPath, rootDepth, seen, mounts)
                        case _ => unresolved(seen)
                    end match
                end if
            // Text and RawHtml are leaf content with no kyo-addressable Element children, so no event
            // target can resolve through them.
            case _: Text | _: RawHtml => unresolved(controlBelow)
            // A mount's content lives with its instance, found through the dispatch table like the node's handler finds it, and
            // occupies the node's path. An inert snapshot resolves to nothing, since no event reaches it.
            case m: Mounted if Boundaries.implicitOf(m).nonEmpty =>
                resolveTarget(Boundaries.implicitOf(m).get, nodePath, at, targetPath, rootDepth, controlBelow, mounts)
            case m: Mounted =>
                mounts.lookup(at.transparent).map {
                    case Absent        => unresolved(controlBelow)
                    case Present(cell) =>
                        cell.current.map {
                            case MountState.Pending(Present(_)) => unresolved(controlBelow)
                            case state                          =>
                                val content = state.content(MountedSpec(m, nodePath, Nil))
                                resolveTarget(content, nodePath, at.transparent, targetPath, rootDepth, controlBelow, mounts)
                        }
                }
            // Held-back content is not on the page; once revealed it occupies the boundary's path.
            case b: Bounded =>
                mounts.boundary(at.transparent).map {
                    case Present(gate) if !gate.isRevealed =>
                        gate.livePlaceholder.fold(Kyo.lift(unresolved(controlBelow)))(ph =>
                            resolveTarget(ph, nodePath, at.transparent, targetPath, rootDepth, controlBelow, mounts)
                        )
                    case _ =>
                        mounts.held(targetPath).map(held =>
                            if held then unresolved(controlBelow)
                            else resolveTarget(b.child, nodePath, at.transparent, targetPath, rootDepth, controlBelow, mounts)
                        )
                }
        end match
    end resolveTarget

    /** The facts a bubbling dispatch reads about its target, in a single resolution. */
    private def targetFacts(
        elem: Element,
        myPath: Seq[String],
        at: ReactiveRegion.RegionIdentity,
        targetPath: Seq[String],
        mounts: MountDispatch
    )(using
        Frame
    ): TargetFacts < Sync =
        resolveTarget(elem, myPath, at, targetPath, myPath.size, false, mounts)

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
    ): Boolean < (Async & Abort[Any]) =
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
                            invokeDrag(attrs.onDragStart, attrs.onDragStartEvt.map(_(value)))
                                .andThen(keepBubbling(elem, attrs.onDragStart.nonEmpty || attrs.onDragStartEvt.nonEmpty))
                        case _ => true
                    }
            case _: UIEvent.DragEnd =>
                if !isTarget then true
                else
                    DragCommands.current.use {
                        case Present(DragCommands.Dispatch(DragCommands.Payload.End(value), _)) =>
                            invokeDrag(attrs.onDragEnd, attrs.onDragEndEvt.map(_(value)))
                                .andThen(keepBubbling(elem, attrs.onDragEnd.nonEmpty || attrs.onDragEndEvt.nonEmpty))
                        case _ => true
                    }
            case _: UIEvent.DragEnter => dispatchDragEvent(elem, attrs.onDragEnter, attrs.onDragEnterEvt)
            case _: UIEvent.DragLeave => dispatchDragEvent(elem, attrs.onDragLeave, attrs.onDragLeaveEvt)
            case _: UIEvent.DragOver  => dispatchDragEvent(elem, attrs.onDragOver, attrs.onDragOverEvt)
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

    private def invokeDrag(action: Maybe[UI.Action], typed: Maybe[UI.Action])(using Frame): Unit < Async =
        def safe(handler: Maybe[UI.Action]): Unit < Async =
            handler.fold((): Unit < Async)(effect => Failures.recover(())(effect.unit))
        safe(action).andThen(safe(typed))
    end invokeDrag

    private def dispatchDragEvent(
        elem: Element,
        action: Maybe[UI.Action],
        typed: Maybe[Drag.Event => UI.Action]
    )(using Frame): Boolean < Async =
        DragCommands.current.use {
            case Present(DragCommands.Dispatch(DragCommands.Payload.Event(value), _)) =>
                invokeDrag(action, typed.map(_(value)))
                    .andThen(keepBubbling(elem, action.nonEmpty || typed.nonEmpty))
            case _ => true
        }

    private def invokeDecision(
        handler: Maybe[Drag.Decision < (Async & Abort[Any])],
        decisions: Maybe[AtomicRef[DragCommands.DecisionState]],
        failure: String
    )(using Frame): Unit < Async =
        handler.fold((): Unit < Async) { effect =>
            for
                result <- Abort.run[Any](effect)
                _      <- result match
                    case Result.Success(decision)     => recordDecision(decisions, decision)
                    case Result.Panic(_: Interrupted) => failDecision(decisions, failure)
                    case Result.Failure(err)          => failDecision(decisions, failure).andThen(Failures.recover(())(Abort.fail(err)))
                    case Result.Panic(panic)          => failDecision(decisions, failure).andThen(Failures.recover(())(Abort.panic(panic)))
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
            session = new MountSession
            (rootTable, rootMounts) <- MountTable.root(rui.path, session)
            _                       <- Scope.ensure(rootTable.evictAll)
            // The root's pass covers the whole tree, so every identity shown twice anywhere in it is found here.
            rootPass <- rootMounts.beginWalk(Seq(rui))
            _        <- subscribeScoped(rui, exchange, signalChangeTime, rootMounts, dispatch, rootPass)
            _        <- rootMounts.endWalk
            // Keyboard activation is synthesized at the bottom, on the tree's own handler, because the
            // two transports enter at different heights: the browser mount goes through `handle`, and
            // the server-push session through `handleValidated` (UIServer). Wrapping either one alone
            // suppresses the browser's activation on both while replacing it on only one, which is a
            // button that stops working rather than one that works twice.
            activating = activatingHandle(rui.handle)
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
    // has no signal: it subscribes its children in the enclosing scope. `mounts` is the enclosing region's scope on the mount table
    // that holds its keyed instances, outside every subscription's scopes, and `pass` the render pass of that region.
    private def subscribeScoped(
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
                subscribeMounted(rui, rui.mounted.get, exchange, signalChangeTime, mounts, pass, dispatch)
            else if rui.bounded.nonEmpty then
                subscribeBoundary(rui, rui.bounded.get, exchange, signalChangeTime, mounts, dispatch, pass)
            else if rui.isConst then
                // A constant node belongs to the enclosing region's pass, which already counts its children.
                Kyo.foreachDiscard(rui.children)(subscribeScoped(_, exchange, signalChangeTime, mounts, dispatch, pass, releases))
            // Inside a waiting boundary a list goes through the whole-region path, whose passes the boundary counts.
            else if rui.foreachSpec.exists(_.reusable) && exchange.gate.isEmpty then
                subscribeForeachRegion(rui, rui.foreachSpec.get, exchange, signalChangeTime, mounts.child(pass, rui), dispatch)
            else
                bindTextRegion(rui, exchange, releases).map { bound =>
                    if bound then Kyo.unit
                    else subscribeRegion(rui, exchange, signalChangeTime, mounts.child(pass, rui), dispatch)
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
                holds <- Boundaries.catches(node.child)
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
                (newKids, _) <- SignalNow.Locals.take(Sync.Unsafe.defer(walkStatic(
                    current,
                    rui.path,
                    svgContext,
                    rui.contentContext,
                    nestedParentContext(rui.parentContext, current),
                    dispatch,
                    rows = rui.rows,
                    deepRows = rui.deepRows,
                    discoverRootBound = rui.discoverContentRootBound
                )))
                // The pass is opened before the paint, so a mount that starts from the snapshot of the instance before it is painted
                // with that snapshot: one paint, where painting first showed an empty slot until the new instance painted its own.
                (pass, release) <- regionMounts.openWalk(newKids)
                _               <- Scope.ensure(release())
                painted         <- regionMounts.withSeeds(current, rui.path, newKids, pass)
                _               <- deliverMeasured(rui, cause, through)(
                    _.onChange(rui.region, rui.path, rui.contentContext, rui.parentContext, previous, painted)
                )
                _ <- release()
                _ <- forkRootChannels(rui.path, current, through)
                _ <- Kyo.foreachDiscard(newKids)(subscribeScoped(_, through, signalChangeTime, regionMounts, dispatch, pass))
                _ <- regionMounts.endWalk
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
        def observed(paint: UI => Unit < (Async & Scope)): Unit < Async =
            rui.source match
                case Present(src) => src.observe(paint)
                case Absent       => rui.signal.observe(paint)

        def start(step: (UI, Boolean) => Unit < (Async & Scope)): Unit < (Async & Scope) =
            regionMounts.register.map { watch =>
                startOwnedFiber {
                    Abort.run[Throwable] {
                        var first = true
                        Signal.passWatch.let(Present(watch))(observed { current =>
                            val isFirst = first
                            first = false
                            step(current, isFirst)
                        })
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
            }

        exchange.gate match
            case Absent        => start(onValue(_, _, exchange))
            case Present(hold) =>
                // Inside a waiting boundary every value is a pass with a group of its own, and the first one is due from now, so
                // the boundary does not reveal before this region has rendered and subscribed what it shows.
                for
                    first <- hold.group
                    due = AtomicRef.Unsafe.init(Maybe(first))(using AllowUnsafe.embrace.danger)
                    _ <- Scope.ensure(first.close)
                    _ <- start { (current, isFirst) =>
                        Sync.Unsafe.defer(due.getAndSet(Absent)).map(_.fold(hold.group)(Kyo.lift)).map { group =>
                            Scope.ensure(group.close)
                                .andThen(group.paints(BoundaryRange(
                                    rui.region,
                                    rui.path,
                                    rui.contentContext,
                                    rui.parentContext,
                                    current,
                                    rui
                                )))
                                .andThen(onValue(current, isFirst, group))
                                .andThen(group.started)
                        }
                    }
                yield ()
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

    /** Per-region ownership of reusable Foreach rows, in list order, plus the same rows by key. Mutation
      * happens only from the owning region's sequential observe loop, so plain get/set on the refs suffices.
      *
      * The key index is maintained incrementally (evicted keys leave it, fresh instances join it), so
      * neither an emission nor an event lookup rebuilds a map over every live row.
      *
      * A row is the registry's from the moment it starts subscribing, not from [[replaceAll]]: its scope is a root nothing else
      * closes, so an emission interrupted in between would otherwise leave what the row had already subscribed running.
      */
    final private[kyo] class RowRegistry(
        rows: AtomicRef[Vector[RowInstance]],
        index: AtomicRef[Map[String, RowInstance]],
        building: AtomicRef[List[RowScope]]
    ):
        def snapshot(using Frame): Vector[RowInstance] < Sync = rows.get

        def byKey(using Frame): Map[String, RowInstance] < Sync = index.get

        def find(key: String)(using Frame): Maybe[RowInstance] < Sync =
            index.get.map(m => Maybe.fromOption(m.get(key)))

        /** Install the new list order; `fresh` are the instances this emission created, the others are
          * already indexed.
          */
        def replaceAll(ordered: Seq[RowInstance], fresh: Seq[RowInstance])(using Frame): Unit < Sync =
            rows.set(Vector.from(ordered)).andThen(building.set(Nil)).andThen(
                if fresh.isEmpty then Kyo.unit
                else index.getAndUpdate(m => fresh.foldLeft(m)((acc, r) => acc.updated(r.key, r))).unit
            )

        /** Take a row that is about to subscribe; [[replaceAll]] installs it, or [[evictAll]] closes it. */
        def track(scope: RowScope)(using Frame): Unit < Sync = building.getAndUpdate(scope :: _).unit

        /** Close-and-await every row whose key `keep` rejects: run before the new value paints, so a
          * vanished or changed row's observers are fully released while the old content is still on screen
          * (break-before-make, the region-level closed-and-awaited guarantee extended to rows).
          */
        def evictExcept(keep: String => Boolean)(using Frame): Unit < Async =
            for
                old <- rows.getAndUpdate(_.filter(r => keep(r.key)))
                gone = old.filterNot(r => keep(r.key))
                _ <-
                    if gone.isEmpty then Kyo.unit
                    // A cleared list drops the whole index at once rather than one key at a time.
                    else if gone.size == old.size then index.set(Map.empty)
                    else index.getAndUpdate(_ -- gone.map(_.key)).unit
                // The channel releases of every evicted row in one pass, then the finalizers, which for a plain row hold
                // nothing and complete without suspending (initInline).
                _ <- release(gone.map(_.scope))
            yield ()

        private def release(scopes: Seq[RowScope])(using Frame): Unit < Async =
            for
                failures <- Sync.Unsafe.defer(scopes.flatMap(_.releaseAll()))
                _        <- Kyo.foreachDiscard(failures)(t => Log.error("kyo-ui: a row's release failed", t))
                _        <- Kyo.foreachDiscard(scopes)(s => s.finalizer.close(Absent).andThen(s.finalizer.await))
            yield ()

        def evictAll(using Frame): Unit < Async =
            evictExcept(_ => false).andThen(building.getAndSet(Nil).map(release))
    end RowRegistry

    private[kyo] object RowRegistry:
        /** The registry for a pass that already holds the thread (see [[SignalNow]]). */
        def initNow()(using AllowUnsafe): RowRegistry =
            new RowRegistry(
                AtomicRef.Unsafe.init(Vector.empty[RowInstance]).safe,
                AtomicRef.Unsafe.init(Map.empty[String, RowInstance]).safe,
                AtomicRef.Unsafe.init(List.empty[RowScope]).safe
            )
    end RowRegistry

    /** What one list emission changes (see [[RowPlan.decide]]).
      *
      * Per position: the key, and the live row that survives there (same key, item compares equal) or `null` where the row is
      * rebuilt. Everything is an array indexed by position, so an emission that moves two rows of a thousand costs the list one
      * object per array and the change its rows, rather than a pass over the whole list that allocates per row.
      *
      * `built` and `finalRows` are filled in by the caller for the `rebuilt` positions (the effects that walk and subscribe a row
      * run there); `decide` fills the retained ones.
      */
    final private class RowPlan[T](
        val items: Chunk.Indexed[T],
        val keys: Array[String],
        val duplicates: Boolean,
        val built: Array[RowPlan.Built],
        val finalRows: Array[RowInstance],
        var rebuilt: Chunk[Int],
        retainedKeys: scala.collection.mutable.HashSet[String],
        prev: Vector[RowInstance]
    ):
        def size: Int = keys.length

        def isRetained(key: String): Boolean = retainedKeys.contains(key)

        /** The keys of the rows this emission keeps as they are, read only: no copy, since a list emission pays for this whether or not
          * anything below it reads them.
          */
        def retainedRows: scala.collection.Set[String] = retainedKeys

        /** Every row gets a new instance: the transport repainted the retained rows too, so their subscriptions start
          * again from their cached UI and walk.
          */
        def rebuildAll(): Unit =
            java.util.Arrays.fill(finalRows.asInstanceOf[Array[AnyRef]], null)
            rebuilt = Chunk.from(0 until size)

        /** The reactive kids of every row, the region's walk for this emission. */
        lazy val allKids: Seq[ReactiveUI] = built.toSeq.flatMap(_.kids)

        def fragmentChildren(using Frame): Chunk[UI] =
            val out = new Array[UI](size)
            var i   = 0
            while i < size do
                out(i) = KeyedChild[UI](built(i).key, built(i).rowUI)
                i += 1
            Chunk.from(out)
        end fragmentChildren

        def listRows: Seq[ListRow] =
            val out = new Array[ListRow](size)
            var i   = 0
            while i < size do
                val b = built(i)
                out(i) = ListRow(b.key, b.rowUI, b.retained.isEmpty)
                i += 1
            end while
            scala.collection.immutable.ArraySeq.unsafeWrapArray(out)
        end listRows

        def reordered: Boolean =
            if prev.size != size then true
            else
                var i = 0
                while i < size && keys(i) == prev(i).key do i += 1
                i < size

        def finalRowsSeq: Seq[RowInstance] = scala.collection.immutable.ArraySeq.unsafeWrapArray(finalRows)

        def freshRows: Seq[RowInstance] = rebuilt.map(i => finalRows(i)).toSeq
    end RowPlan

    private object RowPlan:
        final class Built(
            val key: String,
            val item: Any,
            val rowUI: UI,
            val kids: Seq[ReactiveUI],
            val handler: Maybe[Handler],
            val retained: Maybe[RowInstance]
        )

        /** Reads an emission once: the key at each position, and the live row kept there. A key that repeats within the emission
          * disables reuse for all of it.
          */
        def decide[T](
            chunk: Chunk[T],
            prev: Vector[RowInstance],
            prevByKey: Map[String, RowInstance],
            keyFn: T => String
        ): RowPlan[T] =
            val items     = chunk.toIndexed
            val size      = items.length
            val keys      = new Array[String](size)
            val kept      = new Array[RowInstance](size)
            val seen      = scala.collection.mutable.HashSet.empty[String]
            var dup       = false
            var keptCount = 0
            var i         = 0
            while i < size do
                val item = items(i)
                val key  = keyFn(item)
                keys(i) = key
                if !seen.add(key) then dup = true
                prevByKey.get(key) match
                    case Some(inst) if inst.item.equals(item) =>
                        kept(i) = inst
                        keptCount += 1
                    case _ => ()
                end match
                i += 1
            end while
            if dup then
                java.util.Arrays.fill(kept.asInstanceOf[Array[AnyRef]], null)
                keptCount = 0
            val built        = new Array[Built](size)
            val retainedKeys = scala.collection.mutable.HashSet.empty[String]
            val rebuilt      = new Array[Int](size - keptCount)
            var j            = 0
            i = 0
            while i < size do
                val inst = kept(i)
                if inst eq null then
                    rebuilt(j) = i
                    j += 1
                else
                    retainedKeys += inst.key
                    built(i) = new Built(inst.key, inst.item, inst.rowUI, inst.kids, inst.handler, Present(inst))
                end if
                i += 1
            end while
            new RowPlan(items, keys, dup, built, kept, Chunk.from(rebuilt), retainedKeys, prev)
        end decide
    end RowPlan

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
      * The mounts of every row, re-walked or retained, are one pass of the region, so a retained row's keyed instances are kept, and
      * the regions inside a retained row, whose subscriptions carry on, are left as they are. A row's key is part of every mount
      * identity in it, so no claim in one row can collide with another's.
      */
    private def subscribeForeachRegion(
        rui: ReactiveUI,
        spec: ForeachSpec,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        regionMounts: MountScope,
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
                    _     <- Scope.ensure(rows.evictAll)
                    watch <- regionMounts.register
                    _     <- startOwnedFiber {
                        // A row owns a bare Finalizer, not a fiber: the registry closes and awaits it, and
                        // scoped children still die with it, since a nested region's or mount's release is
                        // registered on it. Subscribing inline rather than on a forked fiber makes a row that
                        // suspends finish before the emission does; the paint is already out by then.
                        // The registry always closes and awaits a row, so its finalizers run on the evicting
                        // fiber (initInline) instead of a fiber per row, and its channel releases collect in the
                        // row's scope, which the eviction runs in one pass.
                        def rowRunner(kids: Seq[ReactiveUI], pass: MountPass): RowScope < Async =
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
                            val grouped = initialKids.groupBy(k => k.path.lift(path.length).getOrElse(""))
                            for
                                pass  <- regionMounts.beginWalk(initialKids)
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
                                _ <- rows.replaceAll(built.toSeq, built.toSeq)
                                // No paint happened, so no cost is charged, but the rows did come into existence, in the
                                // enclosing region's paint. Same accounting as the region skip branch in subscribeRegion.
                                _ <- reportUnpainted(rui, UI.RenderKind.ListPatch(0, built.size))
                            yield ()
                            end for
                        end seed

                        def handle(items: Chunk[T]): Unit < (Async & Scope) =
                            for
                                now       <- Clock.now
                                _         <- signalChangeTime.set(now)
                                prev      <- rows.snapshot
                                prevByKey <- rows.byKey
                                plan = RowPlan.decide(items, prev, prevByKey, keyFn)
                                _ <-
                                    if plan.duplicates then
                                        Log.warn(
                                            s"kyo-ui: duplicate keys in foreachKeyed at ${path.mkString(".")} within one emission: " +
                                                "row reuse is disabled for this emission (all rows rebuilt); use distinct keys"
                                        )
                                    else Kyo.unit
                                _ <- rows.evictExcept(plan.isRetained) // removed and changed rows close before the paint
                                // One synchronous pass over the rebuilt rows (see SignalNow), not an effect loop that suspends per row.
                                _ <- SignalNow.Locals.take(Sync.Unsafe.defer {
                                    plan.rebuilt.foreach { i =>
                                        val key         = plan.keys(i)
                                        val item        = plan.items(i)
                                        val rowUI       = renderFn(i, item)
                                        val (kids, hdl) = walkRow(
                                            rowUI,
                                            path :+ key,
                                            svg,
                                            rui.contentContext.child(key),
                                            nestedParentContext(rui.parentContext, rowUI),
                                            dispatch,
                                            rui.rows,
                                            rui.deepRows
                                        )
                                        plan.built(i) = new RowPlan.Built(key, item, rowUI, kids, Present(hdl), Absent)
                                    }
                                })
                                // The regions inside a retained row keep their subscriptions, so the pass leaves them as they are.
                                firstPass <- regionMounts.beginWalk(plan.allKids, plan.retainedRows)
                                fragment = Fragment[UI](plan.fragmentChildren)
                                previous <- Sync.Unsafe.defer(rendered.getAndSet(Present(fragment)))
                                // A structural command addresses rows by key, so it can say nothing useful
                                // about an emission whose keys are not unique: two rows would name one
                                // slot. That emission already rebuilds every row and warns; it also keeps
                                // the whole-fragment paint, where duplicates degrade positionally instead
                                // of aliasing. Likewise a row that paints as several roots or as none has
                                // nothing for a key to name. The wire cannot discover that late and change
                                // its mind: once the untouched rows are left out of a frame, the client has
                                // nothing to rebuild them from. So the shape is decided here, beside the
                                // duplicate-key gate it restates, and without rendering anything.
                                // So does the emission after one with duplicate keys: its page still shows two rows under one key.
                                addressable = plan.built.forall(b => HtmlRenderer.paintsAsKeyedRoot(b.rowUI))
                                kept <- measuredPaint(
                                    rui,
                                    UI.RenderKind.ListPatch(plan.rebuilt.length, plan.size),
                                    UI.RenderCause.Signal,
                                    alsoWasted = plan.rebuilt.isEmpty && !plan.reordered
                                ) {
                                    if plan.duplicates || prevByKey.size != prev.size || !addressable then
                                        exchange.onChange(rui.region, path, rui.contentContext, rui.parentContext, previous, fragment)
                                            .andThen(false)
                                    else
                                        exchange.onListPatch(
                                            rui.region,
                                            path,
                                            rui.contentContext,
                                            rui.parentContext,
                                            previous,
                                            plan.listRows
                                        )
                                }
                                // A repaint of the kept rows replaced what their subscriptions had painted into them, a
                                // mount's content among it: subscribe them again, as a whole-list repaint always did.
                                // Every row is subscribed again, so a pass covers the regions of all of them.
                                pass <-
                                    if kept then Kyo.lift(firstPass)
                                    else rows.evictAll.andThen(Sync.defer(plan.rebuildAll())).andThen(regionMounts.beginWalk(plan.allKids))
                                _ <- Kyo.foreachDiscard(plan.rebuilt) { i =>
                                    val b = plan.built(i)
                                    rowRunner(b.kids, pass).map(scope =>
                                        plan.finalRows(i) = new RowInstance(b.key, b.item, b.rowUI, b.kids, b.handler, scope)
                                    )
                                }
                                _ <- regionMounts.endWalk
                                _ <- rows.replaceAll(plan.finalRowsSeq, plan.freshRows)
                            yield ()

                        Abort.run[Throwable] {
                            var first = true
                            Signal.passWatch.let(Present(watch))(itemSignal.observe { items =>
                                val isFirst = first
                                first = false
                                if isFirst && spec.renderedItems.exists(_.equals(items)) && distinctKeys(items) then seed(items)
                                else handle(items)
                            })
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

        /** Whether the effect has published or failed. */
        def settled: Boolean =
            this match
                case Pending(_) => false
                case _          => true

        def content(spec: MountedSpec): UI =
            this match
                case Pending(snapshot) => snapshot.getOrElse(spec.placeholder)
                case Ready(ui)         => ui
                case Failed(ui)        => ui
    end MountState

    /** A live mount: its content cell, the fiber running its effect, the `Scope` that owns what the effect acquired, and the table of
      * keyed mounts nested in its content.
      */
    final private[kyo] class MountInstance(
        val cell: SignalRef[MountState],
        val supervision: MountSupervision,
        val children: MountTable,
        runner: Fiber[Unit, Any],
        scope: Scope.Finalizer,
        go: Promise[Unit, Any]
    ):
        /** Lets the effect run. An instance claimed while its table settles what is in transit is made before it may run, so it shows in
          * its place at once, and runs once the cut has released what had to go.
          */
        def launch(using Frame): Unit < Sync = go.completeUnitDiscard

        // Unsafe: written as a subscription showing the instance ends, read when the instance leaves its snapshot after it.
        private val lastShown = AtomicRef.Unsafe.init(Maybe.empty[UI])(using AllowUnsafe.embrace.danger)
        // Unsafe: set once, by the first paint of this instance that reaches the page, whichever subscription makes it.
        private val painted = AtomicBoolean.Unsafe.init(false)(using AllowUnsafe.embrace.danger)

        /** Whether the instance has not shown its own content yet, before a paint of `state`, which counts as that first showing unless it
          * is the snapshot of the instance before it. Until then the slots in its range belong to no instance: the snapshot's slots are
          * copies of the slots of the instance that left it. A kept instance a later subscription shows again is not new there.
          */
        def beforeOwnContent(state: MountState)(using Frame): Boolean < Sync =
            Sync.Unsafe.defer(if showsSnapshot(state) then !painted.get() else painted.compareAndSet(false, true))

        /** Keeps what a subscription showed as it ends, the content of the mounts nested in it resolved while they are still there. */
        def keepShown(ui: UI)(using Frame): Unit < Sync = Sync.Unsafe.defer(lastShown.set(Present(ui)))

        /** What the slot keeps showing under `KeepLatest` once this instance is gone: what it showed last, nested mounts included,
          * which as bare nodes would show only their placeholders. An instance no subscription has shown resolves its regions only.
          */
        def snapshot(using Frame): Maybe[UI] < Sync =
            cell.current.map {
                case MountState.Pending(snapshot) => snapshot
                case MountState.Ready(ui)         =>
                    Sync.Unsafe.defer(lastShown.get()).map(_.fold(resolveReactives(ui).map(Present(_)))(Present(_)))
                case MountState.Failed(ui) =>
                    Sync.Unsafe.defer(lastShown.get()).map(_.fold(resolveReactives(ui).map(Present(_)))(Present(_)))
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
        /** `frames` are the boundaries and mounts around the node, innermost first; the content's mounts inherit them with this one. */
        def start(spec: MountedSpec, seed: Maybe[UI], frames: Failures.Scope, parent: MountTable, launch: Boolean = true)(using
            Frame
        ): MountInstance < Sync =
            for
                cell <- Signal.initRef[MountState](MountState.Pending(seed))
                supervision = MountSupervision(spec, cell, frames)
                inner       = FailureFrame.Mount(supervision) :: frames
                scope  <- Scope.Finalizer.init(1)
                go     <- Promise.init[Unit, Any]
                _      <- if launch then go.completeUnitDiscard else Kyo.unit
                runner <- Fiber.initUnscoped(go.get.andThen(runEffect(spec, cell, scope, supervision, inner, parent.session)))
            yield new MountInstance(cell, supervision, childTable(supervision, parent), runner, scope, go)

        /** The table of the content of an instance supervised by `supervision`: its mounts meet the instance and what is around it now. */
        private def childTable(supervision: MountSupervision, parent: MountTable): MountTable =
            new MountTable(parent.session, () => FailureFrame.Mount(supervision) :: supervision.frames)

        /** An instance that runs no effect and shows its node's error view for `message` from the start: what each place of an identity
          * shown more than once in one pass gets.
          */
        def failed(spec: MountedSpec, message: String, frames: Failures.Scope, parent: MountTable)(using Frame): MountInstance < Sync =
            for
                cell <- Signal.initRef[MountState](MountState.Pending(Absent))
                supervision = MountSupervision(spec, cell, frames)
                _      <- supervision.fail(UI.DuplicateMountException(message), UI.MountErrorSource.Duplicate)
                scope  <- Scope.Finalizer.init(1)
                go     <- Promise.init[Unit, Any]
                runner <- Fiber.initUnscoped(Kyo.unit)
            yield new MountInstance(cell, supervision, childTable(supervision, parent), runner, scope, go)

        /** Runs the effect with `scope` as its `Scope`, so what it acquires outlives this fiber, and publishes the outcome. */
        private def runEffect(
            spec: MountedSpec,
            cell: SignalRef[MountState],
            scope: Scope.Finalizer,
            supervision: MountSupervision,
            frames: Failures.Scope,
            session: MountSession
        )(using Frame): Unit < Async =
            val effect = MountSession.current.let(Present(session))(
                Failures.local.let(frames)(MountSupervision.local.let(Present(supervision))(spec.node.effect))
            )
            Abort.run[Any](ContextEffect.handleInheritable(Tag[Scope], scope)(effect)).map {
                case Result.Success(ui)      => cell.getAndUpdate(MountSupervision.publish(ui)).unit
                case Result.Failure(failure) =>
                    Log.error(s"Mounted effect failed (frame=${spec.node.frame.position.show}): $failure")
                        .andThen(supervision.fail(failure, UI.MountErrorSource.Effect))
                case Result.Panic(t) =>
                    if t.isInstanceOf[Interrupted] then Kyo.unit
                    else
                        Log.error(s"Mounted effect panicked (frame=${spec.node.frame.position.show})", t)
                            .andThen(supervision.fail(t, UI.MountErrorSource.Panic))
            }
        end runEffect
    end MountInstance

    /** Routes the failures of one mount instance into its content cell. The first failure wins, and a publish that arrives after a
      * failure does not overwrite it. The node and the frames are those of the node that shows the instance now: an instance that moves
      * to another node takes that node's error view, its `onFailure` rules and its boundaries.
      */
    final private[kyo] class MountSupervision(
        initialSpec: MountedSpec,
        cell: SignalRef[MountState],
        initialFrames: Failures.Scope
    ):
        // Unsafe: written by the claim that adopts the instance, read wherever a failure is routed.
        private val at      = AtomicRef.Unsafe.init((initialSpec, initialFrames))(using AllowUnsafe.embrace.danger)
        private val failure = AtomicRef.Unsafe.init(Maybe.empty[(Any, UI.MountErrorSource)])(using AllowUnsafe.embrace.danger)

        private def spec: MountedSpec = at.get()(using AllowUnsafe.embrace.danger)._1

        def frames: Failures.Scope = at.get()(using AllowUnsafe.embrace.danger)._2

        /** Makes `spec` and `frames` the node and frames of this instance. A failed instance that `moved` to another node is shown again
          * in that node's error view.
          */
        def adopt(spec: MountedSpec, frames: Failures.Scope, moved: Boolean)(using Frame): Unit < Sync =
            Sync.Unsafe.defer {
                at.set((spec, frames))
                if !moved then Kyo.unit
                else
                    failure.get() match
                        case Present((raised, source)) => cell.set(MountState.Failed(errorView(raised, source)))
                        case Absent                    => Kyo.unit
                end if
            }

        private def errorView(raised: Any, source: UI.MountErrorSource): UI =
            val (s, f) = at.get()(using AllowUnsafe.embrace.danger)
            s.errorUI(s.mountError(raised, source), Failures.views(f))

        /** Completion callback of a [[kyo.UI.fork]] fiber: a terminal failure that is not an interrupt goes to the `onFailure` rules
          * around the node, and fails the node when none matches.
          */
        def report[E, A](result: Result[E, A])(using Frame): Unit < Sync =
            result match
                case Result.Panic(_: Interrupted)   => Kyo.unit
                case Result.Failure(_: Interrupted) => Kyo.unit
                case Result.Panic(t)                => forked(t)
                case Result.Failure(failure)        => forked(failure)
                case _                              => Kyo.unit

        /** The rules may notify or run an action, which suspends, so they run on a fiber of their own. */
        private def forked(failure: Any)(using Frame): Unit < Sync =
            Log.error(s"Mounted background fiber failed (frame=${spec.node.frame.position.show}): $failure")
                .andThen(Fiber.initUnscoped(Failures.handle(own, failure, UI.Reaction.Escalate, UI.MountErrorSource.Fork)).unit)

        /** The frames this node's own failures meet: the node, then what is around it. */
        private def own: Failures.Scope = FailureFrame.Mount(this) :: frames

        def fail(raised: Any, source: UI.MountErrorSource)(using Frame): Unit < Sync =
            Sync.Unsafe.defer(discard(failure.compareAndSet(Absent, Present((raised, source))))).andThen {
                val failed = MountState.Failed(errorView(raised, source))
                cell.getAndUpdate {
                    case done: MountState.Failed => done
                    case _                       => failed
                }.unit
            }
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

    /** The identity a keyed mount instance keeps across re-renders: the keys of the list rows between it and the owner of its table (the
      * session root or the mount instance whose content it is in), where its `.keyed` call is written, and its key. Neither positions among
      * siblings nor the regions around it are part of it, so siblings that come and go, and regions that appear, disappear or re-render
      * around it, leave it alone (see [[MountTable]]). The hash is computed once.
      */
    final private[kyo] case class MountIdentity(rows: List[String], site: UI.MountSite, key: Any) derives CanEqual:
        override val hashCode: Int = (rows.hashCode * 31 + site.hashCode) * 31 + key.hashCode

    /** Where a region sits in the [[MountTable]] that holds the keyed mounts around it: its path and transparent depth, relative to where
      * the table's content is shown. That is the region's place in the structure of the rendered tree. A literal argument is a fixed slot,
      * an `if/else` in it stays in it, and nothing about a region's siblings or the `Frame` it was built under changes its key. Relative,
      * so a kept instance whose content is shown at another path finds its regions under the same keys. [[RegionKey.top]] stands for the
      * content itself, which encloses every region in it.
      */
    final private[kyo] case class RegionKey(path: Seq[String], depth: Int) derives CanEqual:
        /** Whether `other` lies inside this region: below its path, or at its path and more deeply nested. */
        def encloses(other: RegionKey): Boolean =
            other.path.startsWith(path) && (other.path.size > path.size || other.depth > depth)
    end RegionKey

    private[kyo] object RegionKey:
        val top: RegionKey = RegionKey(Vector.empty, -1)

    /** The nodes one render pass of a region owns: its reactive children, and through a constant child (an element that only
      * carries reactive attributes) that child's own, which the constant node subscribes in the same pass and the same table.
      */
    private def passMembers(kids: Seq[ReactiveUI]): Iterator[ReactiveUI] =
        kids.iterator.flatMap(kid => if kid.isConst && kid.mounted.isEmpty then passMembers(kid.children) else Iterator.single(kid))

    /** What one render pass decided for the keyed mounts it shows, its own and those of the regions nested in it that it subscribes: the
      * nodes whose identity the pass shows more than once, with the message each of them shows instead of an instance. The regions the pass
      * subscribes are born in it, under its `id` (see [[MountScope]]).
      */
    final private[kyo] class MountPass(val id: Long, duplicates: java.util.IdentityHashMap[ReactiveUI, String]):
        /** The message `node` shows when the pass found its identity in another place too. */
        def duplicate(node: ReactiveUI): Maybe[String] = Maybe(duplicates.get(node))

    private[kyo] object MountPass:
        private val serials = new java.util.concurrent.atomic.AtomicLong()

        /** A number no pass or subscription had before. */
        def fresh(): Long = serials.incrementAndGet()

        private val noDuplicates = new java.util.IdentityHashMap[ReactiveUI, String]()

        /** The pass of a subscription the table no longer follows: it decides nothing, and nothing it subscribes is followed either. */
        val stale: MountPass = new MountPass(0L, noDuplicates)

        /** A pass that found nothing to decide. */
        def clean(id: Long): MountPass = new MountPass(id, noDuplicates)
    end MountPass

    /** What a [[MountTable]] knows about one region: `cover`, the pass its current subscription was born in, `holds`, the keyed identities
      * its latest pass shows directly (an identity it shows twice is left out), `walked` and `pass`, the walk that pass saw and what it
      * decided, so the first pass of a subscription over the very walk its parent decided reuses that decision, and `handoff`, the
      * snapshots instances that left this region left at their paths for a `KeepLatest` successor here.
      *
      * Only regions whose walk holds a mount have an entry. A region without one is followed by the subscription with the newest birth
      * that acts on it, so a region that gains its first mount in a pass of its own takes its entry then.
      */
    final private case class RegionEntry(
        cover: Long,
        holds: Set[MountIdentity],
        walked: Maybe[Seq[ReactiveUI]],
        complete: Boolean,
        pass: MountPass,
        handoff: Dict[Seq[String], UI],
        // The node that shows each held identity, as the pass recorded it: what a duplicate found at a cut is named by.
        places: Dict[MountIdentity, ReactiveUI] = Dict.empty
    )

    /** A live keyed instance, where it was last shown, and in which region. */
    final private case class MountSlot(instance: MountInstance, path: Seq[String], region: RegionKey)

    /** `waiting` are the identities a cut has to decide: an instance no region shows any more (in transit), one that a second region
      * claims while the first still shows it, one that waits to start until what is in transit is settled, and one whose duplicate
      * report may have ended.
      */
    final private case class TableState(
        instances: Dict[MountIdentity, MountSlot],
        regions: Dict[RegionKey, RegionEntry],
        waiting: Set[MountIdentity],
        views: Dict[MountIdentity, Chunk[ClaimView]],
        // Instances a cut is releasing right now: a new one may not run before they are gone, and the table's end waits for them.
        releasing: Chunk[MountInstance],
        // What the cuts decided and the settling fiber has still to carry out, oldest first.
        decided: Chunk[Decision]
    ):
        /** Whether a new instance has to wait for a cut before its effect runs. */
        def unsettled: Boolean = waiting.nonEmpty || releasing.nonEmpty || decided.nonEmpty

        def holders(identity: MountIdentity): Int =
            regions.foldLeft(0)((n, _, e) => if e.holds.contains(identity) then n + 1 else n)
    end TableState

    private object TableState:
        val empty: TableState = TableState(Dict.empty, Dict.empty, Set.empty, Dict.empty, Chunk.empty, Chunk.empty)

    /** What one cut decided, for the settling fiber to carry out in this order: release, let new instances run, switch the nodes, start
      * the instances that were still to start.
      */
    final private case class Decision(
        released: Chunk[MountSlot],
        launches: Chunk[MountInstance],
        switches: Chunk[(ClaimView, ClaimState)],
        starts: Chunk[(MountIdentity, ClaimView)]
    )

    /** What a keyed node shows: the live instance of its identity, what its `PendingPolicy` shows while a cut decides who the instance
      * belongs to, or the report of an identity shown in more than one place.
      */
    private[kyo] enum ClaimState:
        case Live(instance: MountInstance)
        case Waiting(shown: Maybe[UI])
        case Duplicate(message: String)
    end ClaimState

    private[kyo] object ClaimState:
        given CanEqual[ClaimState, ClaimState] = CanEqual.derived

    /** One keyed node's claim on its identity, which a cut can switch. */
    final private[kyo] class ClaimView(
        val node: ReactiveUI,
        val spec: MountedSpec,
        val region: RegionKey,
        val frames: () => Failures.Scope,
        val seed: Maybe[UI],
        val state: SignalRef[ClaimState]
    )

    /** What opening a pass changed: the pass, the instances it put in transit, and the duplicate reports it made. */
    final private case class Opened(pass: MountPass, dropped: Chunk[MountSlot], waits: Boolean, duplicates: Chunk[String], work: Int)

    /** Work counts of one session, for measuring what the tables cost: regions registered and unregistered, region passes that planned
      * their walk and passes that skipped it (no mount in it and none recorded below), cuts recorded and settled, and regions a cut check
      * looked at.
      */
    final private[kyo] class MountCounters:
        val registered   = new java.util.concurrent.atomic.AtomicLong()
        val unregistered = new java.util.concurrent.atomic.AtomicLong()
        val planned      = new java.util.concurrent.atomic.AtomicLong()
        val skipped      = new java.util.concurrent.atomic.AtomicLong()
        val cuts         = new java.util.concurrent.atomic.AtomicLong()
        val settled      = new java.util.concurrent.atomic.AtomicLong()
        val scanned      = new java.util.concurrent.atomic.AtomicLong()
    end MountCounters

    /** One mount session: the open [[kyo.UI.batch]] blocks, which together form one change, and the tables waiting for that change to end
      * before they can take a cut.
      */
    final private[kyo] class MountSession:
        import AllowUnsafe.embrace.danger

        private val open    = new java.util.concurrent.atomic.AtomicInteger()
        private val pending = AtomicRef.Unsafe.init(Set.empty[MountTable])
        val counters        = new MountCounters

        def isOpen: Boolean = open.get() > 0

        def enter(): Unit = discard(open.incrementAndGet())

        /** Ends one batch; the last one to end lets every waiting table take its cut. */
        def exit(): Unit =
            if open.decrementAndGet() == 0 then pending.getAndSet(Set.empty).foreach(_.closeEpoch())

        /** `table` takes its cut when the change now open ends, or at once when none is open. */
        def defer(table: MountTable): Unit =
            discard(pending.getAndUpdate(_ + table))
            if !isOpen && pending.getAndUpdate(_ - table).contains(table) then table.closeEpoch()
        end defer

        /** Runs `v` as one change: the cuts its writes cause wait until it ends. */
        def within[A, S](v: => A < S)(using Frame): A < (S & Sync) =
            Sync.Unsafe.defer(enter()).andThen(Sync.ensure(Sync.Unsafe.defer(exit()))(MountSession.current.let(Present(this))(v)))
    end MountSession

    private[kyo] object MountSession:
        /** The session of the handler or mount effect that is running, inherited by the fibers they fork: where [[kyo.UI.batch]] opens its
          * change.
          */
        val current: Local[Maybe[MountSession]] = Local.init(Maybe.empty[MountSession])
    end MountSession

    /** The passes of one region subscription as its observation reports them (see `Signal.PassWatch`): how many started, which of them
      * last recorded its walk in the table, and whether a write landed since the latest read. A cut waits for a region until it has seen
      * every write of the change: it was current when the change ended, or a pass that started after that has recorded its walk.
      */
    final private[kyo] class RegionWatch(scope: MountScope) extends Signal.PassWatch:
        private val starts                         = new java.util.concurrent.atomic.AtomicLong()
        private val planned                        = new java.util.concurrent.atomic.AtomicLong()
        private val rests                          = new java.util.concurrent.atomic.AtomicLong()
        private val ended                          = new java.util.concurrent.atomic.AtomicBoolean()
        @volatile private var probe: () => Boolean = () => false

        def started(behind: () => Boolean): Unit =
            probe = behind
            discard(starts.incrementAndGet())

        def rested(): Unit =
            discard(rests.incrementAndGet())
            scope.table.recheck()

        /** The pass that started last has recorded its walk. */
        def recorded(): Unit =
            planned.set(starts.get())
            scope.table.recheck()

        def end(): Unit =
            ended.set(true)
            scope.table.recheck()

        def started: Long = starts.get()

        def restedSoFar: Long = rests.get()

        /** Whether the region has seen every write up to a cut taken when it had started `at` passes and rested `restsAt` times: a pass
          * started since has recorded its walk, or its latest pass has, and its probe says nothing landed since, or it re-read and found
          * its value unchanged since. The last is what catches up a region whose probe cannot tell (a signal without an exact read).
          */
        def caughtUp(at: Long, restsAt: Long): Boolean =
            ended.get() || !scope.followed || {
                val p = planned.get()
                p > at || (p == starts.get() && (!probe() || rests.get() > restsAt))
            }
    end RegionWatch

    /** The keyed instances of one owner, the session root or a mount instance's content, found by their own [[MountIdentity]], and what
      * every region of that content shows.
      *
      * Regions own no instances. A region's pass records which identities it shows; an instance lives while a region of the table shows it.
      * A pass covers every region nested in it that it subscribes, so a move between two of them is recorded at once.
      *
      * Between regions that render separately the table decides nothing in a pass: release, move and duplicate are decided at a cut. An
      * instance no region shows any more stays in transit, alive, and a region that shows it next adopts it. A second region that claims
      * one the first still shows waits, showing what its node's `PendingPolicy` says. A new instance waits to start while anything is in
      * transit, so what is released is released before it starts. When the change ends (the pass itself, or the [[kyo.UI.batch]] blocks of
      * the session it falls into), the table records where every region's passes stand; the cut is reached once each has seen every write
      * so far. A region over a signal without an exact read (one built with `Signal.initRaw`, or a combinator over one) cannot tell whether
      * a write reached it, so it counts as behind until its next re-read, which its repair timer brings within one repair interval
      * (`Signal.defaultRepairInterval`): a cut in a table that holds such a region can wait that long. The decision is taken the moment the
      * cut is reached, by whatever reports the last region caught up: an identity shown once has moved there, one shown nowhere is
      * released, and one shown in several places is reported at all of them. It depends on what the regions show once they all saw the
      * writes, never on the order they rendered them in, and a write after it is a change of its own, also when the table's settling fiber
      * has not yet carried the decision out: a node hidden and shown again gets a new instance.
      * An event handler is no change of its own: it may wait for what its writes cause, a release or a new instance's content, and a
      * change that stayed open while it waits would never end.
      *
      * An identity shown more than once by one pass is reported at once: that pass is a cut of its own.
      *
      * The old subscription of a region stops only once the enclosing region's previous value has released, which can be after the new one
      * started. Each subscription is born in the pass that subscribed it, and an entry follows the subscription of the pass that last
      * covered it, so an older one that still runs a pass changes nothing.
      *
      * All of it is one immutable state behind one atomic reference, changed by compare-and-set: regions of a table run their passes on
      * fibers of their own.
      */
    final private[kyo] class MountTable(val session: MountSession, framesOf: () => Failures.Scope):
        import AllowUnsafe.embrace.danger

        private val state   = AtomicRef.Unsafe.init(TableState.empty)
        private val watches = AtomicRef.Unsafe.init(Set.empty[RegionWatch])
        private val cut     = AtomicRef.Unsafe.init(Maybe.empty[Chunk[(RegionWatch, Long, Long)]])
        private val wake    = AtomicRef.Unsafe.init(Promise.Unsafe.init[Unit, Any]())
        private val running = new java.util.concurrent.atomic.AtomicBoolean()
        private val closed  = new java.util.concurrent.atomic.AtomicBoolean()

        def frames: Failures.Scope = framesOf()

        private[ReactiveUI] def read: TableState = state.get()

        @scala.annotation.tailrec
        final private[ReactiveUI] def modify[A](f: TableState => (TableState, A)): A =
            val before       = state.get()
            val (after, out) = f(before)
            if (after eq before) || state.compareAndSet(before, after) then out else modify(f)
        end modify

        /** A scope for one subscription of the table's content, shown at `path` with transparent depth `depth`. The table follows the
          * newest one that `follow`s: a subscription made by a region the table around it no longer follows shows the content and
          * changes nothing.
          */
        def content(path: Seq[String], depth: Int, follow: Boolean = true)(using Frame): MountScope < Sync =
            Sync.Unsafe.defer {
                val birth = if follow then MountPass.fresh() else MountPass.stale.id
                if follow then
                    modify { s =>
                        val entry = s.regions.get(RegionKey.top) match
                            case Present(old) => old.copy(cover = birth)
                            case Absent       => RegionEntry(birth, Set.empty, Absent, complete = true, MountPass.stale, Dict.empty)
                        (s.copy(regions = s.regions.update(RegionKey.top, entry)), ())
                    }
                end if
                new MountScope(this, RegionKey.top, birth, path, depth, Nil)
            }

        /** Registers a region subscription's watch for the cuts of this table, until the current scope closes. */
        def register(scope: MountScope)(using Frame): RegionWatch < (Sync & Scope) =
            Sync.Unsafe.defer {
                val watch = new RegionWatch(scope)
                discard(watches.getAndUpdate(_ + watch))
                discard(session.counters.registered.incrementAndGet())
                Scope.ensure(Sync.Unsafe.defer {
                    discard(watches.getAndUpdate(_ - watch))
                    discard(session.counters.unregistered.incrementAndGet())
                    watch.end()
                }).andThen(watch)
            }

        /** Wakes the settling fiber, if one waits. Callable from any thread, without effects. */
        def recheck(): Unit =
            decide()
            if running.get() then wake.getAndSet(Promise.Unsafe.init[Unit, Any]()).completeDiscard(Result.succeed(()))
        end recheck

        /** The change that put identities in waiting has ended: records where every region's passes stand. */
        def closeEpoch(): Unit =
            if state.get().waiting.nonEmpty then
                val all = watches.get()
                cut.set(Present(Chunk.from(all.iterator.map(w => (w, w.started, w.restedSoFar)))))
                discard(session.counters.cuts.incrementAndGet())
                recheck()
        end closeEpoch

        /** Makes sure what is waiting gets decided: a settling fiber runs, and the cut is taken when the change ends. */
        private[ReactiveUI] def awaitCut()(using Frame): Unit < Sync =
            Sync.Unsafe.defer {
                val start = running.compareAndSet(false, true)
                session.defer(this)
                if start then Fiber.initUnscoped(settling).unit else Kyo.unit
            }

        private def reached(all: Chunk[(RegionWatch, Long, Long)]): Boolean =
            discard(session.counters.scanned.addAndGet(all.size))
            all.forall((w, at, restsAt) => w.caughtUp(at, restsAt))

        /** Takes the cut's decision the moment the cut is reached: called by whatever reports a region caught up, and when the change
          * ends. Deciding later, on the settling fiber, would let a region that renders a later write record an identity again before
          * the decision, which would then adopt an instance the cut had to release. Only one caller takes a cut: the one that clears it.
          * The decision is a change of the table state; what it asks for (releases, launches, switches, starts) is left there for the
          * settling fiber, which carries it out in order.
          */
        private def decide(): Unit =
            val taken = cut.get()
            if taken.nonEmpty && reached(taken.get) && cut.compareAndSet(taken, Absent) then
                discard(session.counters.settled.incrementAndGet())
                modify { s =>
                    var instances = s.instances
                    var released  = Chunk.empty[MountSlot]
                    var starts    = Chunk.empty[(MountIdentity, ClaimView)]
                    var switches  = Chunk.empty[(ClaimView, ClaimState)]
                    var launches  = Chunk.empty[MountInstance]
                    s.waiting.foreach { identity =>
                        val views   = s.views.get(identity).getOrElse(Chunk.empty)
                        val holding = views.filter(v => s.regions.get(v.region).exists(_.holds.contains(identity)))
                        s.holders(identity) match
                            case 0 =>
                                instances.get(identity).foreach { slot =>
                                    released = released.append(slot)
                                    instances = instances.remove(identity)
                                }
                            case 1 =>
                                instances.get(identity) match
                                    case Present(slot) =>
                                        launches = launches.append(slot.instance)
                                        holding.foreach(v => switches = switches.append((v, ClaimState.Live(slot.instance))))
                                        holding.headMaybe.foreach(v =>
                                            instances = instances.update(identity, slot.copy(path = v.spec.path, region = v.region))
                                        )
                                    case Absent =>
                                        holding.headMaybe.foreach(v => starts = starts.append((identity, v)))
                            case _ =>
                                instances.get(identity).foreach { slot =>
                                    released = released.append(slot)
                                    instances = instances.remove(identity)
                                }
                                // Named from what the regions' passes recorded, so a place whose claim is not filed yet is named too.
                                val nodes =
                                    s.regions.foldLeft(Chunk.empty[ReactiveUI])((acc, _, e) => e.places.get(identity).fold(acc)(acc.append))
                                val message = duplicateMessage(identity, nodes)
                                holding.foreach(v => switches = switches.append((v, ClaimState.Duplicate(message))))
                        end match
                    }
                    (
                        s.copy(
                            instances = instances,
                            waiting = Set.empty,
                            releasing = s.releasing.concat(released.map(_.instance)),
                            decided = s.decided.append(Decision(released, launches, switches, starts))
                        ),
                        ()
                    )
                }
            end if
        end decide

        /** Carries out what the cuts decided, one decision after another, while anything waits or is still to be carried out. */
        private def settling(using Frame): Unit < Async =
            Sync.Unsafe.defer {
                val signal = wake.get()
                if closed.get() then
                    running.set(false)
                    Kyo.unit
                else
                    val next = modify[Maybe[Decision]] { s =>
                        s.decided.headMaybe match
                            case Present(d) => (s.copy(decided = s.decided.drop(1)), Present(d))
                            case Absent     => (s, Absent)
                    }
                    next match
                        case Present(decision) => carryOut(decision).andThen(settling)
                        case Absent            =>
                            val s = state.get()
                            if s.waiting.isEmpty && s.decided.isEmpty then
                                running.set(false)
                                // An identity may have started waiting, or a decision been taken, between the read and the flag.
                                val again = state.get()
                                if (again.waiting.nonEmpty || again.decided.nonEmpty) && running.compareAndSet(false, true) then settling
                                else Kyo.unit
                            else signal.safe.get.andThen(settling)
                            end if
                    end match
                end if
            }

        /** Releases what a decision released, then lets the instances it kept run, switches the nodes, and starts what was still to start. */
        private def carryOut(decision: Decision)(using Frame): Unit < Async =
            val released = decision.released
            Async.uninterruptible(
                Kyo.foreachDiscard(released)(_.instance.teardown)
                    .andThen(Sync.Unsafe.defer(modify(s =>
                        (s.copy(releasing = s.releasing.filterNot(i => released.exists(_.instance eq i))), ())
                    )))
            )
                .andThen(Kyo.foreachDiscard(decision.launches)(_.launch))
                .andThen(Kyo.foreachDiscard(decision.switches) { (view, next) =>
                    next match
                        case ClaimState.Live(instance) =>
                            instance.supervision.adopt(view.spec, view.frames(), moved = true).andThen(view.state.set(next))
                        case _ => view.state.set(next)
                })
                .andThen(Kyo.foreachDiscard(decision.starts) { (identity, view) =>
                    MountInstance.start(view.spec, view.seed, view.frames(), this).map { fresh =>
                        Sync.Unsafe.defer(modify { s =>
                            if s.instances.contains(identity) || s.holders(identity) == 0 then (s, false)
                            else (s.copy(instances = s.instances.update(identity, MountSlot(fresh, view.spec.path, view.region))), true)
                        }).map(kept => if kept then view.state.set(ClaimState.Live(fresh)) else fresh.teardown)
                    }
                })
        end carryOut

        /** Releases every instance, in transit or not, and waits for all of it, also for the releases a cut has already started: a
          * teardown of an instance that is already being torn down waits for that one to finish.
          */
        def evictAll(using Frame): Unit < Async =
            Sync.Unsafe.defer {
                closed.set(true)
                val all = modify(s =>
                    (TableState.empty, s.instances.foldLeft(s.releasing)((acc, _, slot) => acc.append(slot.instance)))
                )
                recheck()
                if all.isEmpty then Kyo.unit
                else Async.uninterruptible(Kyo.foreachDiscard(all)(_.teardown))
            }
    end MountTable

    private[kyo] object MountTable:
        val nothingToRelease: () => Unit < Async = () => Kyo.unit

        /** The table of a session rooted at `path`, and the scope of its one subscription. */
        def root(path: Seq[String], session: MountSession = new MountSession)(using Frame): (MountTable, MountScope) < Sync =
            val table = new MountTable(session, () => Nil)
            table.content(path, 0).map(scope => (table, scope))
    end MountTable

    /** One subscription's handle on a [[MountTable]]: the region it renders, the pass it was born in, and where the table's content is shown,
      * which makes region keys relative. The boundaries between the region and the table's content come first in `frames`.
      */
    final private[kyo] class MountScope private[ReactiveUI] (
        val table: MountTable,
        val region: RegionKey,
        birth: Long,
        basePath: Seq[String],
        baseDepth: Int,
        boundaries: Failures.Scope
    ):
        import AllowUnsafe.embrace.danger

        @volatile private var watch: Maybe[RegionWatch] = Absent

        def frames: Failures.Scope = boundaries ++ table.frames

        /** The entry this subscription acts on: its own, one it takes over as the newest subscription of a region without a covering
          * pass, or a fresh one for a region that had none. `Absent` when a newer subscription follows the region.
          */
        private def entryIn(s: TableState): Maybe[RegionEntry] =
            s.regions.get(region) match
                case Present(e) if e.cover == birth => Present(e)
                case Present(e) if e.cover < birth  => Present(e.copy(cover = birth))
                case Present(_)                     => Absent
                case Absent                         =>
                    if birth == MountPass.stale.id then Absent
                    else Present(RegionEntry(birth, Set.empty, Absent, complete = false, MountPass.stale, Dict.empty))

        /** Whether the table still follows this subscription. */
        def followed: Boolean = entryIn(table.read).nonEmpty

        def current(using Frame): Boolean < Sync = Sync.Unsafe.defer(followed)

        private[ReactiveUI] def keyOf(at: ReactiveRegion.RegionIdentity): RegionKey =
            val rel = at.path.drop(basePath.size)
            RegionKey(rel, if rel.isEmpty then at.transparentDepth - baseDepth else at.transparentDepth)

        /** The scope of the region `kid`, which `pass` subscribes. */
        def child(pass: MountPass, kid: ReactiveUI): MountScope =
            val inner = kid.bounded.fold(boundaries)(b => FailureFrame.Boundary(b.settings) :: boundaries)
            new MountScope(table, keyOf(kid.at), pass.id, basePath, baseDepth, inner)

        /** Registers this subscription's passes for the table's cuts; its observation reports to the answer. */
        def register(using Frame): RegionWatch < (Sync & Scope) =
            table.register(this).map { w =>
                watch = Present(w)
                w
            }

        /** The pass that started last has recorded everything it shows. */
        def recorded(using Frame): Unit < Sync = Sync.Unsafe.defer(watch.foreach(_.recorded()))

        /** Opens a render pass of the region over `kids`, the region's walk. It records what the region and every region nested in the walk
          * show, gives none of the places of an identity shown twice an instance, and drops the regions the walk no longer has, except
          * those inside `retained` rows of a keyed list, whose subscriptions carry on. An instance no region shows any more goes in
          * transit, and leaves its snapshot for a `KeepLatest` successor in the region that showed it; a cut decides it (see
          * [[MountTable]]). A pass whose walk has no mount, in a region with nothing recorded below it, records nothing.
          */
        def openWalk(kids: Seq[ReactiveUI], retained: scala.collection.Set[String] = Set.empty)(using
            Frame
        ): (MountPass, () => Unit < Async) < Sync =
            Sync.Unsafe.defer {
                val id     = MountPass.fresh()
                val opened = table.modify { s =>
                    entryIn(s) match
                        case Absent => (s, Opened(MountPass.stale, Chunk.empty, false, Chunk.empty, 0))
                        // The first pass of a subscription over the walk the pass that subscribed it already decided.
                        case Present(entry) if retained.isEmpty && entry.complete && entry.walked.exists(_ eq kids) =>
                            (s, Opened(entry.pass, Chunk.empty, false, Chunk.empty, 0))
                        case Present(entry) if !kids.exists(_.containsMount) && nothingBelow(s, entry) =>
                            (s, Opened(MountPass.clean(id), Chunk.empty, false, Chunk.empty, 1))
                        case Present(entry) => plan(s, entry, kids, retained, id)
                }
                if opened.work == 1 then discard(table.session.counters.skipped.incrementAndGet())
                else if opened.work == 2 then discard(table.session.counters.planned.incrementAndGet())
                watch.foreach(_.recorded())
                val reported = Kyo.foreachDiscard(opened.duplicates)(message => Log.warn(s"kyo-ui: $message"))
                val dropped  = Kyo.foreachDiscard(opened.dropped)(slot => leaveSnapshot(slot.instance, slot.region, slot.path))
                val waits    = if opened.waits then table.awaitCut() else Kyo.unit
                reported.andThen(dropped).andThen(waits).andThen((opened.pass, MountTable.nothingToRelease))
            }

        private def nothingBelow(s: TableState, entry: RegionEntry): Boolean =
            entry.holds.isEmpty && s.regions.foldLeft(true)((ok, key, _) => ok && (key == region || !region.encloses(key)))

        private def plan(
            s: TableState,
            entry: RegionEntry,
            kids: Seq[ReactiveUI],
            retained: scala.collection.Set[String],
            id: Long
        ): (TableState, Opened) =
            val rowDepth                               = region.path.size
            def inRetainedRow(key: RegionKey): Boolean =
                retained.nonEmpty && key.path.size > rowDepth && key.path.startsWith(region.path) && retained.contains(key.path(rowDepth))
            val covered =
                new java.util.LinkedHashMap[
                    RegionKey,
                    (Seq[ReactiveUI], scala.collection.mutable.ArrayBuffer[(MountIdentity, ReactiveUI)])
                ]()
            val seen = new java.util.LinkedHashMap[MountIdentity, java.util.ArrayList[ReactiveUI]]()
            def visit(at: RegionKey, walked: Seq[ReactiveUI]): Unit =
                val holds = scala.collection.mutable.ArrayBuffer.empty[(MountIdentity, ReactiveUI)]
                discard(covered.put(at, (walked, holds)))
                passMembers(walked).foreach { member =>
                    member.mounted match
                        case Present(spec) =>
                            spec.identity.foreach { identity =>
                                holds += ((identity, member))
                                discard(seen.computeIfAbsent(identity, _ => new java.util.ArrayList[ReactiveUI](1)).add(member))
                            }
                        case Absent =>
                            // A nested region without a mount in its walk needs no entry: its subscription takes one if it gains a mount.
                            val key = keyOf(member.at)
                            if member.containsMount && !inRetainedRow(key) then visit(key, member.children)
                }
            end visit
            visit(region, kids)
            val duplicates = new java.util.IdentityHashMap[ReactiveUI, String]()
            var doubled    = Set.empty[MountIdentity]
            var messages   = Chunk.empty[String]
            seen.forEach { (identity, nodes) =>
                if nodes.size > 1 then
                    doubled += identity
                    val message = duplicateMessage(identity, Chunk.from((0 until nodes.size).map(nodes.get)))
                    messages = messages.append(message)
                    nodes.forEach(node => discard(duplicates.put(node, message)))
            }
            val pass    = new MountPass(id, duplicates)
            var lost    = Set.empty[MountIdentity]
            var regions = s.regions.filter { (key, e) =>
                val keep = !(region.encloses(key) && !covered.containsKey(key) && !inRetainedRow(key))
                if !keep then lost ++= e.holds
                keep
            }
            var inside = Set.empty[MountIdentity]
            covered.forEach { (key, value) =>
                val (walked, found) = value
                val kept            = found.iterator.filterNot((identity, _) => doubled.contains(identity)).toSeq
                val holds           = kept.iterator.map(_._1).toSet
                val places          = Dict.from(kept.toMap)
                inside ++= holds
                s.regions.get(key).foreach(old => lost ++= old.holds.diff(holds))
                val next =
                    if key == region then
                        entry.copy(holds = holds, walked = Present(walked), complete = retained.isEmpty, pass = pass, places = places)
                    else
                        RegionEntry(
                            id,
                            holds,
                            Present(walked),
                            complete = true,
                            pass,
                            s.regions.get(key).fold(Dict.empty[Seq[String], UI])(_.handoff),
                            places
                        )
                regions = regions.update(key, next)
            }
            val next = s.copy(regions = regions)
            // In transit: instances no region shows any more. Contested: identities this pass shows that a region outside it shows too.
            // Reported duplicates whose places changed may have ended.
            val dropped = s.instances.foldLeft(Chunk.empty[(MountIdentity, MountSlot)]) { (acc, identity, slot) =>
                if next.holders(identity) == 0 && !s.waiting.contains(identity) then acc.append((identity, slot)) else acc
            }
            val contested = inside.filter(identity => next.holders(identity) > 1)
            val reopened  = lost.filter(identity => s.views.get(identity).exists(_.nonEmpty))
            val waiting   = s.waiting ++ dropped.map(_._1) ++ contested ++ reopened
            (next.copy(waiting = waiting), Opened(pass, dropped.map(_._2), waiting.size > s.waiting.size, messages, 2))
        end plan

        /** Leaves what `instance` showed at `path` in `at`, if that region is still there, for a `KeepLatest` successor. */
        private def leaveSnapshot(instance: MountInstance, at: RegionKey, path: Seq[String])(using Frame): Unit < Sync =
            instance.snapshot.map {
                case Absent            => Kyo.unit
                case Present(snapshot) =>
                    Sync.Unsafe.defer(table.modify { s =>
                        s.regions.get(at) match
                            case Present(e) =>
                                (s.copy(regions = s.regions.update(at, e.copy(handoff = e.handoff.update(path, snapshot)))), ())
                            case Absent => (s, ())
                    })
            }

        /** [[openWalk]], with the release run and awaited at once. */
        def beginWalk(kids: Seq[ReactiveUI], retained: scala.collection.Set[String] = Set.empty)(using Frame): MountPass < Async =
            openWalk(kids, retained).map((pass, release) => release().andThen(pass))

        /** `ui` as this pass paints it, rooted at `path`: a mount among `kids` that starts from the snapshot of the instance before it
          * shows that snapshot in its slot already, as the instance will once it starts, rather than an empty slot until then.
          */
        def withSeeds(ui: UI, path: Seq[String], kids: Seq[ReactiveUI], pass: MountPass)(using Frame): UI < Sync =
            Sync.Unsafe.defer {
                val s = table.read
                entryIn(s) match
                    case Present(entry) if !entry.handoff.isEmpty =>
                        val seeds = kids.foldLeft(Map.empty[Seq[String], UI]) { (acc, kid) =>
                            kid.mounted.flatMap(spec => seedFor(s, entry, kid, spec, pass)).fold(acc)(seed => acc.updated(kid.path, seed))
                        }
                        if seeds.isEmpty then ui else showingSeeds(ui, path, seeds)
                    case _ => ui
                end match
            }

        private def seedFor(s: TableState, entry: RegionEntry, kid: ReactiveUI, spec: MountedSpec, pass: MountPass): Maybe[UI] =
            spec.node.pendingPolicy match
                case UI.PendingPolicy.Placeholder => Absent
                case UI.PendingPolicy.KeepLatest  =>
                    val starts = pass.duplicate(kid).isEmpty && spec.identity.forall(identity => !s.instances.contains(identity))
                    if starts then entry.handoff.get(spec.path) else Absent

        /** Ends a pass: the snapshots it could hand on are gone. */
        def endWalk(using Frame): Unit < Sync =
            Sync.Unsafe.defer(table.modify { s =>
                s.regions.get(region) match
                    case Present(e) if e.cover == birth && !e.handoff.isEmpty =>
                        (s.copy(regions = s.regions.update(region, e.copy(handoff = Dict.empty))), ())
                    case _ => (s, ())
            })

        /** Leaves what a boundary at `path` showed when it ended, for a successor this region's next pass starts there. */
        def leave(path: Seq[String], ui: UI)(using Frame): Unit < Sync =
            Sync.Unsafe.defer(table.modify { s =>
                entryIn(s) match
                    case Present(e) => (s.copy(regions = s.regions.update(region, e.copy(handoff = e.handoff.update(path, ui)))), ())
                    case Absent     => (s, ())
            })

        /** What a boundary that ended at `path` left there. */
        def left(path: Seq[String])(using Frame): Maybe[UI] < Sync =
            Sync.Unsafe.defer(entryIn(table.read).flatMap(_.handoff.get(path)))

        /** The claim of the keyed node `rui` of `pass` on `identity`: the report when the pass shows the identity in another place too;
          * the live instance when no other region shows it; otherwise, or while something is in transit and the instance is still to
          * start, what the node's `PendingPolicy` shows until the table's next cut decides. A subscription the table no longer follows
          * gets the live instance, or one of its own that its scope releases.
          */
        def claim(rui: ReactiveUI, identity: MountIdentity, spec: MountedSpec, pass: MountPass)(using
            Frame
        ): ClaimView < (Async & Scope) =
            def view(state: ClaimState, seed: Maybe[UI]): ClaimView < Sync =
                Signal.initRef[ClaimState](state).map(ref => new ClaimView(rui, spec, region, () => frames, seed, ref))
            pass.duplicate(rui) match
                case Present(message) => view(ClaimState.Duplicate(message), Absent)
                case Absent           =>
                    Sync.Unsafe.defer(table.read).map { s =>
                        val live = s.instances.get(identity)
                        entryIn(s) match
                            case Absent =>
                                live match
                                    case Present(slot) => view(ClaimState.Live(slot.instance), Absent)
                                    case Absent        =>
                                        MountInstance.start(spec, Absent, frames, table).map(i =>
                                            Scope.ensure(i.teardown).andThen(view(ClaimState.Live(i), Absent))
                                        )
                            case Present(entry) =>
                                val others  = s.holders(identity) > 1
                                val pending = s.unsettled
                                val seed    = spec.node.pendingPolicy match
                                    case UI.PendingPolicy.Placeholder => Absent
                                    case UI.PendingPolicy.KeepLatest  => entry.handoff.get(spec.path)
                                live match
                                    case Present(slot) if !others =>
                                        val moved = slot.region != region
                                        Sync.Unsafe.defer(table.modify { now =>
                                            now.instances.get(identity) match
                                                case Present(same) if same.instance eq slot.instance =>
                                                    (
                                                        now.copy(instances =
                                                            now.instances.update(identity, MountSlot(same.instance, spec.path, region))
                                                        ),
                                                        ()
                                                    )
                                                case _ => (now, ())
                                        }).andThen(slot.instance.supervision.adopt(spec, frames, moved))
                                            .andThen(view(ClaimState.Live(slot.instance), Absent))
                                            .map(registered(identity, _, false))
                                    case Present(slot) =>
                                        val shown = spec.node.pendingPolicy match
                                            case UI.PendingPolicy.Placeholder => Kyo.lift(Maybe.empty[UI])
                                            case UI.PendingPolicy.KeepLatest  => slot.instance.snapshot
                                        shown.map(snapshot => view(ClaimState.Waiting(snapshot), seed)).map(registered(identity, _, true))
                                    case Absent =>
                                        // While something is in transit, or another region shows the identity too, the instance is made
                                        // and shown but its effect waits for the cut: what is released goes first, and a duplicate never runs.
                                        // The effect is let go only once the instance is filed with nothing unsettled in the table.
                                        val launch = !others && !pending
                                        MountInstance.start(spec, seed, frames, table, launch = false).map { fresh =>
                                            Sync.Unsafe.defer(table.modify[(Maybe[MountInstance], Boolean)] { now =>
                                                now.instances.get(identity) match
                                                    case Present(other) => (now, (Present(other.instance), false))
                                                    case Absent         =>
                                                        if now.holders(identity) == 0 then (now, (Absent, false))
                                                        else
                                                            val waits = !launch || now.unsettled
                                                            (
                                                                now.copy(instances =
                                                                    now.instances.update(identity, MountSlot(fresh, spec.path, region))
                                                                ),
                                                                (Present(fresh), waits)
                                                            )
                                            }).map { (kept, waits) =>
                                                if kept.isEmpty then
                                                    // No region holds it any more: a scope that ends with this node.
                                                    fresh.launch.andThen(Scope.ensure(fresh.teardown)).andThen(view(
                                                        ClaimState.Live(fresh),
                                                        seed
                                                    ))
                                                else if kept.get eq fresh then
                                                    (if waits then Kyo.unit else fresh.launch)
                                                        .andThen(view(ClaimState.Live(fresh), seed))
                                                        .map(registered(identity, _, waits))
                                                else
                                                    fresh.teardown.andThen(view(
                                                        ClaimState.Live(kept.get),
                                                        Absent
                                                    )).map(registered(identity, _, false))
                                            }
                                        }
                                end match
                        end match
                    }
            end match
        end claim

        /** Files `view` under `identity` while the current scope lives; `waits` puts the identity in waiting for the next cut. */
        private def registered(identity: MountIdentity, view: ClaimView, waits: Boolean)(using Frame): ClaimView < (Sync & Scope) =
            Sync.Unsafe.defer {
                val added = table.modify { s =>
                    val views = s.views.update(identity, s.views.get(identity).getOrElse(Chunk.empty).append(view))
                    (
                        s.copy(views = views, waiting = if waits then s.waiting + identity else s.waiting),
                        waits && !s.waiting.contains(identity)
                    )
                }
                val unfile = Scope.ensure(Sync.Unsafe.defer(table.modify { s =>
                    val left = s.views.get(identity).getOrElse(Chunk.empty).filterNot(_ eq view)
                    (s.copy(views = if left.isEmpty then s.views.remove(identity) else s.views.update(identity, left)), ())
                }))
                unfile.andThen(if added then table.awaitCut() else Kyo.unit).andThen(view)
            }

        /** A keyless instance for `spec`, released with the caller's scope; only a subscription the table still follows leaves its snapshot. */
        def startKeyless(spec: MountedSpec)(using Frame): MountInstance < (Async & Scope) =
            Sync.Unsafe.defer(entryIn(table.read)).map { entry =>
                val seed = spec.node.pendingPolicy match
                    case UI.PendingPolicy.Placeholder => Absent
                    case UI.PendingPolicy.KeepLatest  => entry.flatMap(_.handoff.get(spec.path))
                MountInstance.start(spec, seed, frames, table).map { instance =>
                    Scope.ensure(current.map(still => if still then leaveSnapshot(instance, region, spec.path) else Kyo.unit).andThen(
                        instance.teardown
                    ))
                        .andThen(instance)
                }
            }
    end MountScope

    /** The message each place of an identity shown in several places at once shows. */
    private def duplicateMessage(identity: MountIdentity, nodes: Chunk[ReactiveUI]): String =
        val places = nodes.map(_.path.mkString(".")).toSeq.sorted.mkString(", ")
        val site   = nodes.headMaybe.flatMap(_.mounted).fold("")(spec => s" (UI.mounted at ${spec.node.frame.position.show})")
        s"the keyed mount ${identity.key}$site is shown in ${nodes.size} places at once: $places. " +
            "A keyed instance can show in one place only, so none of them gets one. Give each place a key of its own, " +
            "such as the id of the data it shows, or render repeating data with foreachKeyed."
    end duplicateMessage

    /** Subscribes a mounted node in the current scope. A keyed node adopts or creates the instance of its identity in the table of the
      * enclosing mount instance or session; a keyless node gets a new instance that ends with the current scope, which for a node inside
      * a region is the region's current value.
      */
    private def subscribeMounted(
        rui: ReactiveUI,
        spec: MountedSpec,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        mounts: MountScope,
        pass: MountPass,
        dispatch: MountDispatch
    )(using Frame): Unit < (Async & Scope) =
        def show(instance: MountInstance, follow: Boolean): Unit < (Async & Scope) =
            subscribeMountContent(rui, spec, instance, exchange, signalChangeTime, dispatch, follow)
        mounts.current.map { follow =>
            spec.identity match
                case Absent            => mounts.startKeyless(spec).map(show(_, follow))
                case Present(identity) =>
                    mounts.claim(rui, identity, spec, pass).map { view =>
                        def showClaim(state: ClaimState): Unit < (Async & Scope) =
                            state match
                                case ClaimState.Live(instance)     => show(instance, follow)
                                case ClaimState.Waiting(shown)     => showWaiting(rui, shown, spec, exchange)
                                case ClaimState.Duplicate(message) =>
                                    MountInstance.failed(spec, message, mounts.frames, mounts.table)
                                        .map(failed => Scope.ensure(failed.teardown).andThen(show(failed, follow)))
                        // The state the claim starts in is shown in this pass, like any node; a cut that switches it later closes what
                        // that showed and shows the next state in a scope of its own.
                        for
                            first <- view.state.get
                            shown <- Scope.Finalizer.init(1)
                            _     <- Scope.ensure(shown.close(Absent).andThen(shown.await))
                            _     <- ContextEffect.handle[Scope.Finalizer, Scope, Unit, Async](
                                Tag[Scope],
                                derive = (_: Maybe[Scope.Finalizer]) => shown,
                                fork = (parent: Scope.Finalizer) => parent.forked,
                                join = (parent: Scope.Finalizer, _: Scope.Finalizer, _: Scope.Finalizer) => parent
                            )(showClaim(first))
                            _ <- startOwnedFiber(
                                Abort.run[Throwable](view.state.observe(Present(first), Signal.defaultRepairInterval) { next =>
                                    shown.close(Absent).andThen(shown.await).andThen(showClaim(next))
                                }).unit
                            )
                        yield ()
                        end for
                    }
        }
    end subscribeMounted

    /** Shows what a keyed node waiting for a cut shows: the snapshot its `KeepLatest` policy keeps, held like any snapshot so nothing in
      * it looks live, or its placeholder. No instance is behind it, so no event reaches it.
      */
    private def showWaiting(rui: ReactiveUI, shown: Maybe[UI], spec: MountedSpec, exchange: UIExchange)(using
        Frame
    ): Unit < (Async & Scope) =
        deliverMeasured(rui, UI.RenderCause.Signal, exchange)(
            _.onChange(rui.region, rui.path, rui.contentContext, rui.parentContext, Absent, shown.getOrElse(spec.placeholder))
        ).andThen(
            if shown.isEmpty then Kyo.unit
            else exchange.onHold(rui.region, true).andThen(Scope.ensure(exchange.onHold(rui.region, false)))
        )

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
        dispatch: MountDispatch,
        // Whether the region showing the node is one its table follows; a subscription that is not takes nothing over.
        follow: Boolean
    )(using Frame): Unit < (Async & Scope) =
        val svgContext = rui.region match
            case _: ReactiveRegion.HtmlRange  => false
            case _: ReactiveRegion.SvgElement => true
        for
            _       <- dispatch.register(rui.contentContext, instance)
            _       <- Scope.ensure(dispatch.unregister(rui.contentContext, instance))
            content <- instance.children.content(rui.path, rui.contentContext.transparentDepth, follow)
            watch   <- content.register
            first   <- instance.cell.current
            // Instrumented like any other paint: every `onChange` the engine issues is reported, or the wasted-render figure
            // quietly excludes a whole class of paint and a page full of mounts reads as cleaner than it is.
            // Until a new instance shows its own content, each paint tells the page that the slots in its range are no one's: they were
            // the slots of the instance before it, gone with it, and an adopted slot the page kept would show that instance's mounts. A
            // snapshot paint does not end this, since the slots it brings are the old instance's again.
            paint = (cause: UI.RenderCause, previous: Maybe[UI], state: MountState, through: UIExchange) =>
                for
                    _ <- deliverMeasured(rui, cause, through)(exchange =>
                        instance.beforeOwnContent(state).map(fresh => if fresh then exchange.onFreshInstance(rui.region) else Kyo.unit)
                            .andThen(
                                exchange.onChange(
                                    rui.region,
                                    rui.path,
                                    rui.contentContext,
                                    rui.parentContext,
                                    previous,
                                    state.content(spec)
                                )
                            )
                    )
                yield ()
            _ <- first match
                case MountState.Pending(Absent) => Kyo.unit
                case state                      => paint(UI.RenderCause.Created, Absent, state, exchange)
            // A snapshot takes no events, so its range is held while it shows: nothing in it looks like a field the reader can use.
            _ <- if showsSnapshot(first) then exchange.onHold(rui.region, true) else Kyo.unit
            // Unsafe: single-writer state of this region's own loop, as in subscribeScoped.
            painted = AtomicRef.Unsafe.init[MountState](first)(using AllowUnsafe.embrace.danger)
            _ <- Scope.ensure(Sync.Unsafe.defer(painted.get()).map(last =>
                if showsSnapshot(last) then exchange.onHold(rui.region, false) else Kyo.unit
            ))
            renderState = (state: MountState, through: UIExchange) =>
                val shown = state.content(spec)
                for
                    previous <- Sync.Unsafe.defer(painted.getAndSet(state))
                    _        <-
                        if previous eq state then Kyo.unit
                        else
                            Clock.now.map(signalChangeTime.set).andThen(
                                paint(UI.RenderCause.Signal, Present(previous.content(spec)), state, through)
                            )
                    _ <- (showsSnapshot(previous), showsSnapshot(state)) match
                        case (false, true) => through.onHold(rui.region, true)
                        case (true, false) => through.onHold(rui.region, false)
                        case _             => Kyo.unit
                    _ <- state match
                        case MountState.Pending(Present(_)) => content.recorded
                        case _                              =>
                            for
                                (kids, _) <- SignalNow.Locals.take(Sync.Unsafe.defer(walkStatic(
                                    shown,
                                    rui.path,
                                    svgContext,
                                    rui.contentContext,
                                    nestedParentContext(rui.parentContext, shown),
                                    dispatch,
                                    rows = 0L,
                                    deepRows = Nil
                                )))
                                pass <- content.beginWalk(kids)
                                _    <- forkRootChannels(rui.path, shown, through)
                                _    <- Kyo.foreachDiscard(kids)(subscribeScoped(_, through, signalChangeTime, content, dispatch, pass))
                                _    <- content.endWalk
                            yield ()
                yield ()
                end for
            // Inside a waiting boundary every state is a pass with a group of its own, the first one due from now, and the mount loads
            // until it has published or failed.
            observed <- exchange.gate match
                case Absent        => Kyo.lift((state: MountState) => renderState(state, exchange))
                case Present(hold) =>
                    import AllowUnsafe.embrace.danger
                    val loaded = AtomicBoolean.Unsafe.init(first.settled)
                    for
                        firstGroup <- hold.group
                        due = AtomicRef.Unsafe.init(Maybe(firstGroup))
                        registered <- if first.settled then Kyo.lift(Maybe.empty[BoundaryHold]) else hold.load.map(Present(_))
                        _          <- Scope.ensure(firstGroup.close)
                        _ <- Scope.ensure(Sync.Unsafe.defer(if loaded.compareAndSet(false, true) then registered.foreach(_.settle)))
                    yield (state: MountState) =>
                        Sync.Unsafe.defer(due.getAndSet(Absent)).map(_.fold(hold.group)(Kyo.lift)).map { group =>
                            Scope.ensure(group.close)
                                .andThen(group.paints(BoundaryRange(
                                    rui.region,
                                    rui.path,
                                    rui.contentContext,
                                    rui.parentContext,
                                    state.content(spec),
                                    rui
                                )))
                                .andThen(renderState(state, group))
                                .andThen(Sync.Unsafe.defer(if state.settled && loaded.compareAndSet(false, true) then
                                    registered.foreach(_.settle)))
                                .andThen(group.started)
                        }
                    end for
            _ <- startOwnedFiber(
                Abort.run[Throwable](Signal.passWatch.let(Present(watch))(instance.cell.observe(observed))).map { result =>
                    result.fold(
                        _ => (),
                        err => Log.error(s"Mounted content region failed at path=${rui.path.mkString(".")}", err),
                        panic =>
                            if panic.isInstanceOf[Interrupted] then ()
                            else Log.error(s"Mounted content region failed at path=${rui.path.mkString(".")}", panic)
                    )
                }
            )
            // Registered after the fiber, so it runs before the fiber's interrupt: the nested mounts are still registered, and a
            // snapshot of this instance shows their content rather than their placeholders.
            _ <- Scope.ensure(instance.cell.current.map {
                case MountState.Pending(_) => Kyo.unit
                case state                 =>
                    Boundaries.resolveLive(state.content(spec), rui.path, rui.contentContext, dispatch)
                        .map(resolveReactives).map(instance.keepShown)
            })
        yield ()
        end for
    end subscribeMountContent

    /** `ui` rooted at `path` with each mount at a path of `seeds` shown as its slot holding that snapshot. Only the static part of `ui`
      * is walked: the mounts inside a region, a boundary or a mount are painted by those.
      */
    private def showingSeeds(ui: UI, path: Seq[String], seeds: Map[Seq[String], UI]): UI =
        ui match
            case m: Mounted =>
                seeds.get(path) match
                    // A boundary around one mount renders as that mount's slot, here showing the snapshot.
                    case Some(seed) if Boundaries.implicitOf(m).isEmpty =>
                        Bounded(UI.Boundary(Absent, Absent, Absent), m, Present(seed))(using m.frame)
                    case _ => m
            case e: Element =>
                val children = e.children.toSeq
                val shown    = children.zipWithIndex.map((c, i) => showingSeeds(c, path :+ i.toString, seeds))
                if shown.corresponds(children)(_ eq _) then e else rebuildElement(e, Chunk.from(shown))
            case f: Fragment[?] =>
                val children = f.children.toSeq
                val shown    = children.zipWithIndex.map {
                    case (kc: KeyedChild[?], _) =>
                        val inner = showingSeeds(kc.child, path :+ kc.key, seeds)
                        if inner eq kc.child then kc else KeyedChild[UI](kc.key, inner)(using kc.frame)
                    case (c, i) => showingSeeds(c, path :+ i.toString, seeds)
                }
                if shown.corresponds(children)(_ eq _) then f else Fragment[UI](Chunk.from(shown))(using f.frame)
            case _ => ui

    /** Whether a mount in `state` shows the inert snapshot of the instance before it. */
    private def showsSnapshot(state: MountState): Boolean =
        state match
            case MountState.Pending(Present(_)) => true
            case _                              => false

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
