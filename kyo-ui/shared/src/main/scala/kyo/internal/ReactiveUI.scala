package kyo.internal

import DragSessions.*
import EventDispatch.*
import MountRegions.*
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
    source: Maybe[UI.Ast.Reactive.Source[?]] = Absent
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
    rows: RowRegistry
):
    def reusable: Boolean = node.key.nonEmpty && !node.indexed
end ForeachSpec

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
        deepRows: Chunk[Int] = Chunk.empty
    )(
        handle: (Seq[String], UIEvent) => Boolean < Async
    ): ReactiveUI =
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
            deepRows = deepRows
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
                deepRows = Chunk.empty
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
            renderedChannels = RenderedChannels.empty
        )

    /* Normalization and the tree walk carry, for the mounted nodes and regions they meet, the session's dispatch table and which
     * depths of the path, since the enclosing reactive region, hold the key of a keyed list row: a bit per depth below 63 in `rows`,
     * and the rare deeper ones in `deepRows`. Both are read out of the path only when a mount or a region's table needs them, so the
     * walk itself allocates nothing for them.
     */

    /** The row keys `rows` and `deepRows` mark in `path`, innermost first. */
    private[internal] def rowKeys(path: Seq[String], rows: Long, deepRows: Chunk[Int]): Chunk[String] =
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
                val contentContext       = regionIdentity.transparent
                val current              = SignalNow(ui.signal)
                val contentParentContext = nestedParentContext(parentContext, current)
                val (kids, _)            =
                    walkStatic(current, path, svg, contentContext, contentParentContext, cells, rows = 0L, deepRows = Chunk.empty)
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
                                walkStatic(
                                    currentUI,
                                    path,
                                    svg,
                                    contentContext,
                                    contentParentContext,
                                    cells,
                                    rows = 0L,
                                    deepRows = Chunk.empty
                                )
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
                            walkStatic(current, path, svg, contentContext, contentParentContext, cells, rows = 0L, deepRows = Chunk.empty)
                        // Fallback dispatch: re-render the whole list from the current value and route
                        // through the fresh fragment handler. Used whenever the row registry cannot
                        // resolve the target (region not subscribed on the reuse path, row painted but
                        // not yet registered, or an event addressed to the region node itself).
                        val fullWalk: Handler = (targetPath, event) =>
                            Sync.Unsafe.defer {
                                val currentUI          = SignalNow(sig)
                                val freshParentContext = nestedParentContext(parentContext, currentUI)
                                val (_, freshHdl)      =
                                    walkStatic(
                                        currentUI,
                                        path,
                                        svg,
                                        contentContext,
                                        freshParentContext,
                                        cells,
                                        rows = 0L,
                                        deepRows = Chunk.empty
                                    )
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
                                                            cells
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
                // A constant element belongs to the enclosing region; a bound one is a region of its own.
                val contentRows     = if isConstNode then rows else 0L
                val contentDeepRows = if isConstNode then deepRows else Chunk.empty
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
                    origin = Present(ui.frame),
                    rows = rows,
                    deepRows = deepRows,
                    channels = channels,
                    renderedChannels = snapshot
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
                                            deepRows = Chunk.empty
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
                    mounted = Present(spec)
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
                    discoverContentRootBound = false
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
        case _: Reactive[?] | _: Foreach[?, ?] | _: Mounted => true
        case el: Element                                    =>
            collectSignalRef(el).nonEmpty || el.attrs.hasChannels
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
        cells: MountDispatch
    )(using AllowUnsafe, Frame, SignalNow.Locals): (Seq[ReactiveUI], Handler) =
        val depth    = rowPath.size - 1
        val rows     = if depth < 63 then 1L << depth else 0L
        val deepRows = if depth < 63 then Chunk.empty[Int] else Chunk(depth)
        if needsOwnNode(rowUI) then
            val rui = normalizeAt(rowUI, rowPath, svg, context, parentContext, cells, rows, deepRows)
            (Seq(rui), rui.handle)
        else walkStatic(rowUI, rowPath, svg, context, parentContext, cells, rows, deepRows)
        end if
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
                            idx.filter(i => i >= 0 && i < allHandles.length).map(allHandles(_))
                                .fold(false: Boolean < Async)(h => h(targetPath, event))
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

    private[internal] def startOwnedFiber(task: => Unit < Async)(using Frame): Unit < (Async & Scope) =
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

    /** Runs `ui` under `exchange` in a scope of its own and hands over the subscription. What runs a tree for its effects, and what a test
      * drives one with.
      */
    private[kyo] def subscribed[A, S](
        ui: UI,
        exchange: UIExchange,
        dragLimits: DragSessionLimits = DragSessionLimits()
    )(f: Subscription => A < (Async & Scope & S))(using Frame): A < (Async & S) =
        Scope.run(normalize(ui, Seq.empty).map(root => subscribe(root, exchange, dragLimits).map(f)))

    /** The tail of a walked value: its kids subscribed against `table`, then the end of the table's pass. A region and a mount share it. */
    private[internal] def subscribeWalked(
        kids: Seq[ReactiveUI],
        keys: RegionKeys,
        table: MountTableRef,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        dispatch: MountDispatch
    )(using Frame): Unit < (Async & Scope) =
        Kyo.foreachDiscard(kids)(subscribeScoped(_, exchange, signalChangeTime, table, dispatch, keys)).andThen(table.endWalk)

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
    // has no signal: it subscribes its children in the enclosing scope. `mounts` is the mount table of the enclosing
    // region; keyed instances live in that tree of tables, outside every subscription's scopes.
    private[internal] def subscribeScoped(
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
        forkChannelObservers(rui.path, rui.channels, rui.renderedChannels, exchange, releases).andThen {
            if rui.mounted.nonEmpty then
                subscribeMounted(rui, rui.mounted.get, exchange, signalChangeTime, mounts, dispatch)
            else if rui.isConst then
                // A constant node belongs to the enclosing region's pass, which already counts its children.
                Kyo.foreachDiscard(rui.children)(subscribeScoped(_, exchange, signalChangeTime, mounts, dispatch, pass, releases))
            else if rui.foreachSpec.exists(_.reusable) then
                // Unsafe: the list region's reference is made before its loop is forked, where no proof is at hand, as for a region below.
                val listMounts = mounts.child(pass, rui)(using AllowUnsafe.embrace.danger)
                RowRegions.subscribeForeachRegion(rui, rui.foreachSpec.get, exchange, signalChangeTime, listMounts, dispatch)
            else
                bindTextRegion(rui, exchange, releases).map { bound =>
                    if bound then Kyo.unit
                    else
                        // Unsafe: the region's reference is made before its loop is forked, where no proof is at hand.
                        subscribeRegion(
                            rui,
                            exchange,
                            signalChangeTime,
                            mounts.child(pass, rui)(using AllowUnsafe.embrace.danger),
                            dispatch
                        )
                }
            end if
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
                Log.use { log =>
                    Sync.Unsafe.defer {
                        // A failing write is logged to the Log bound here: the write runs inside the writer's `set`.
                        val report: Throwable => Unit =
                            ex => log.unsafe.error(s"kyo-ui: text binding failed at path=${rui.path.mkString(".")}", ex)
                        sig.unsafeObserveProjected[String](identity, rendered, v => patch(rui.path, v), report) match
                            case Absent           => Kyo.lift(false)
                            case Present(release) => registerRelease(release, releases).andThen(true)
                    }
                }
            case _ => false
    end bindTextRegion

    /** Starts the channel observers of the element a region paints as its root. A walk only gives the root's children nodes of their
      * own, so the root's channels at `path` are started here, in the current scope like the children's subscriptions.
      */
    private[internal] def forkRootChannels(path: Seq[String], content: UI, exchange: UIExchange)(using Frame): Unit < (Async & Scope) =
        content match
            case el: Element if el.attrs.hasChannels =>
                forkChannelObservers(path, Channels.of(el), RenderedChannels.empty, exchange, Absent)
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
        releases: Maybe[RowScope]
    )(using Frame): Unit < (Async & Scope) =
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
                    patcher match
                        case Present(patch) => bindChannel(sig, baseline, (v: A) => patch(path, name, v), releases)
                        case Absent         => Fiber.init(sig.observe(baseline, Signal.defaultRepairInterval)(slow(name, _))).unit
                }
        end bindAll

        bindAll(channels.attrs, rendered.attrs, exchange.attrPatcherNow, exchange.onAttrPatch(path, _, _))
            .andThen(bindAll(channels.bools, rendered.bools, exchange.boolAttrPatcherNow, exchange.onBoolAttrPatch(path, _, _)))
            .andThen(bindAll(channels.classes, rendered.classes, exchange.classPatcherNow, exchange.onClassPatch(path, _, _)))
            .andThen(channels.style.fold(Kyo.unit: Unit < (Async & Scope))(bindStyle(path, _, rendered.style, exchange, releases)))
    end forkChannelObservers

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
                .andThen(subscribeWalked(kids, keys, regionMounts, exchange, signalChangeTime, dispatch))
        end subscribeKids
        // Per-value setup, run inside each value's fresh Scope: render the region and fork its children into that scope, so the next
        // value (or an interrupt) tears them down by cascade.
        def renderValue(current: UI): Unit < (Async & Scope) =
            for
                now      <- Clock.now
                _        <- signalChangeTime.set(now)
                previous <- Sync.Unsafe.defer(rendered.getAndSet(Present(current)))
                _        <- exchange.onChange(rui.region, rui.path, rui.contentContext, rui.parentContext, previous, current)
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
                    rows = 0L,
                    deepRows = Chunk.empty,
                    discoverRootBound = rui.discoverContentRootBound
                )))
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
                        Sync.Unsafe.defer(rendered.set(Present(current)))
                            .andThen(forkRootChannels(rui.path, current, exchange))
                            .andThen(subscribeKids(rui.children))
                    else renderValue(current)
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

    /** The inline-style channel. A value is its declarations, and a patch sets them and removes the properties the previous value had
      * and this one lacks: the previous value is what this observer last wrote, seeded with what the render wrote, which only the
      * observer knows (the client never saw a value as a value).
      */
    private def bindStyle(
        path: Seq[String],
        sig: Signal[Style],
        renderedStyle: Maybe[Seq[(String, String)]],
        exchange: UIExchange,
        releases: Maybe[RowScope]
    )(using Frame): Unit < (Async & Scope) =
        given CanEqual[Seq[(String, String)], Seq[(String, String)]] = CanEqual.derived
        val decls                                                    = sig.map(CssStyleRenderer.declarations)
        // Unsafe: one channel's own last write, read and replaced in one step by whichever of its writes runs, on the writer's
        // stack (the synchronous sink) or on its fiber; it never leaves this observer.
        val last = AtomicRef.Unsafe.init[Seq[(String, String)]](renderedStyle.getOrElse(Nil))(using AllowUnsafe.embrace.danger)
        def step(next: Seq[(String, String)])(using AllowUnsafe): Seq[String] =
            val names = next.iterator.map(_._1).toSet
            last.getAndSet(next).map(_._1).filterNot(names.contains)
        exchange.stylePatcherNow match
            case Present(patch) =>
                bindChannel(
                    decls,
                    renderedStyle,
                    (next: Seq[(String, String)]) => patch(path, next, step(next)(using AllowUnsafe.embrace.danger)),
                    releases
                )
            case Absent =>
                Fiber.init(decls.observe(renderedStyle, Signal.defaultRepairInterval)(next =>
                    Sync.Unsafe.defer(step(next)).map(exchange.onStylePatch(path, next, _))
                )).unit
        end match
    end bindStyle

    /** Binds a channel's synchronous `write` as a callback on `sig`: inside the writer's `set` where the signal provides that path, through
      * `onChange` where it does not. The Log is read here, where the subscription is made: a write runs inside somebody else's `set`, where
      * none can be read, so a failing write is logged to the Log bound at the subscription. A callback's release goes to the row that
      * collects it, or onto the Scope (see registerRelease).
      */
    private def bindChannel[A](sig: Signal[A], rendered: Maybe[A], write: A => Unit, releases: Maybe[RowScope])(using
        CanEqual[A, A],
        Frame
    ): Unit < (Sync & Scope) =
        Log.use { log =>
            Sync.Unsafe.defer {
                val report: Throwable => Unit = ex => log.unsafe.error("kyo-ui: channel binding failed", ex)
                sig.unsafeObserveProjected[A](identity, rendered, write, report) match
                    case Present(release) => registerRelease(release, releases)
                    case Absent           => sig.onChange(rendered)(v => Sync.defer(write(v)))
            }
        }
    end bindChannel

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
