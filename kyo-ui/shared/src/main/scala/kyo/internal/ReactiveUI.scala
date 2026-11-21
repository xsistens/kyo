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
    deepRows: List[Int] = Nil,
    // Reactive channels carried from the element's Attrs; subscribeScoped forks the in-place-patch observers
    // (empty for non-element / attribute-less nodes).
    reactiveAttrs: Map[String, Signal[String]] = Map.empty,
    reactiveBoolAttrs: Map[String, Signal[Boolean]] = Map.empty,
    reactiveClasses: Map[String, Signal[Boolean]] = Map.empty,
    reactiveStyle: Maybe[Signal[Style]] = Absent,
    // Render-time snapshots: the values the enclosing paint already put into the DOM. Subscribe skips an observer's first
    // emission while it still equals its snapshot, so a fresh paint is not patched again.
    renderedValue: Maybe[UI] = Absent,
    renderedAttrValues: Map[String, String] = Map.empty,
    renderedBoolAttrValues: Map[String, Boolean] = Map.empty,
    renderedClassValues: Map[String, Boolean] = Map.empty,
    // The inline-style channel's declarations at render (see UI.Element.inlineStyle): what the first patch diffs against.
    renderedStyleValue: Maybe[Seq[(String, String)]] = Absent
)

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
        for
            dispatch <- MountDispatch.init
            root     <- normalizeAt(
                ui,
                path,
                svg,
                ReactiveRegion.RegionIdentity.root(path),
                ReactiveRegion.ParentContext.Other,
                dispatch,
                rows = 0L,
                deepRows = Nil
            )
        yield root.copy(mountDispatch = Present(dispatch))
        end for
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
            renderedValue = Absent,
            renderedAttrValues = Map.empty,
            renderedBoolAttrValues = Map.empty,
            renderedClassValues = Map.empty,
            renderedStyleValue = Absent
        )

    /* Normalization and the tree walk carry, for the mounted nodes and regions they meet, the session's dispatch table and which
     * depths of the path, since the enclosing reactive region, hold the key of a keyed list row: a bit per depth below 63 in `rows`,
     * and the rare deeper ones in `deepRows`. Both are read out of the path only when a mount or a region's table needs them, so the
     * walk itself allocates nothing for them.
     */

    /** The row keys `rows` and `deepRows` mark in `path`, innermost first. */
    private[internal] def rowKeys(path: Seq[String], rows: Long, deepRows: List[Int]): List[String] =
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
    ): ReactiveUI < Sync =
        given Frame = ui.frame
        ui match
            case ui: Reactive[?] =>
                val contentContext = regionIdentity.transparent
                for
                    current <- ui.signal.current
                    contentParentContext = nestedParentContext(parentContext, current)
                    (kids, _) <- walkStatic(current, path, svg, contentContext, contentParentContext, cells, rows = 0L, deepRows = Nil)
                yield init(
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
                        for
                            currentUI <- ui.signal.current
                            contentParentContext = nestedParentContext(parentContext, currentUI)
                            (_, freshHdl) <-
                                walkStatic(currentUI, path, svg, contentContext, contentParentContext, cells, rows = 0L, deepRows = Nil)
                            result <- freshHdl(targetPath, event)
                        yield result
                }.copy(renderedValue = Present(current))
                end for

            case ui: Foreach[?, ?] @unchecked =>
                val contentContext = regionIdentity
                val sig            =
                    ui.signal.map { items =>
                        val arr = items.toSeq.zipWithIndex.map { (item, i) =>
                            val key = if ui.key.nonEmpty then ui.key.get(item) else i.toString
                            KeyedChild[UI](key, ui.render(i, item))
                        }
                        Fragment[UI](Chunk.from(arr)): UI
                    }
                for
                    current <- sig.current
                    contentParentContext = nestedParentContext(parentContext, current)
                    (kids, _) <- walkStatic(current, path, svg, contentContext, contentParentContext, cells, rows = 0L, deepRows = Nil)
                yield init(
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
                        for
                            currentUI <- sig.current
                            contentParentContext = nestedParentContext(parentContext, currentUI)
                            (_, freshHdl) <-
                                walkStatic(currentUI, path, svg, contentContext, contentParentContext, cells, rows = 0L, deepRows = Nil)
                            result <- freshHdl(targetPath, event)
                        yield result
                }.copy(renderedValue = Present(current))
                end for

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
                for
                    (kids, hdl) <- walkStatic(
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
                    attrSnap  <- Kyo.foreach(ui.attrs.reactiveAttrs.toSeq)((n, s) => s.current.map(v => n -> v))
                    boolSnap  <- Kyo.foreach(ui.attrs.reactiveBoolAttrs.toSeq)((n, s) => s.current.map(v => n -> v))
                    classSnap <- Kyo.foreach(ui.attrs.reactiveClasses.toSeq)((n, s) => s.current.map(v => n -> v))
                    styleSnap <- Kyo.foreach(ui.attrs.reactiveStyle.toList)(s => s.current.map(CssStyleRenderer.declarations))
                yield ReactiveUI(
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
                    reactiveStyle = ui.attrs.reactiveStyle,
                    renderedAttrValues = attrSnap.toMap,
                    renderedBoolAttrValues = boolSnap.toMap,
                    renderedClassValues = classSnap.toMap,
                    renderedStyleValue = Maybe.fromOption(styleSnap.headOption)
                )
                end for

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
                                    for
                                        (_, freshHdl) <- walkStatic(
                                            content,
                                            path,
                                            svg,
                                            contentContext,
                                            nestedParentContext(parentContext, content),
                                            cells,
                                            rows = 0L,
                                            deepRows = Nil
                                        )
                                        result <- freshHdl(targetPath, event)
                                    yield result
                                    end for
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
            collectSignalRef(el).nonEmpty || el.attrs.reactiveAttrs.nonEmpty
            || el.attrs.reactiveBoolAttrs.nonEmpty || el.attrs.reactiveClasses.nonEmpty || el.attrs.reactiveStyle.nonEmpty
        case _ => false

    /** Walk a static UI tree. Collect reactive children, build handle. */
    private[internal] def walkStatic(
        ui: UI,
        basePath: Seq[String],
        svg: Boolean = false,
        context: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        cells: MountDispatch,
        rows: Long,
        deepRows: List[Int],
        discoverRootBound: Boolean = true
    )(using Frame): (Seq[ReactiveUI], Handler) < Sync =
        ui match
            case elem: Element if discoverRootBound && collectSignalRef(elem).nonEmpty =>
                for rui <- normalizeAt(elem, basePath, svg, context, parentContext, cells, rows, deepRows)
                yield (Seq(rui), rui.handle)

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
                for childWalks <- Kyo.foreach(elem.children.toSeq.zipWithIndex) { (child, i) =>
                        val childPath    = basePath :+ i.toString
                        val childContext = context.child(i.toString)
                        if needsOwnNode(child) then
                            // Normalize into a ReactiveUI node so subscribeScoped wires the updates. Required
                            // even for a const element carrying ONLY reactive attrs: otherwise it is not walked
                            // into a node and its in-place-patch observers would never start.
                            for rui <- normalizeAt(child, childPath, childSvg, childContext, childParentContext, cells, rows, deepRows)
                            yield (Seq(rui), Seq.empty[(Int, Handler)])
                        else
                            for
                                (innerKids, innerHandle) <-
                                    walkStatic(child, childPath, childSvg, childContext, childParentContext, cells, rows, deepRows)
                            yield (innerKids, Seq((i, innerHandle)))
                        end if
                    }
                yield
                    val reactiveChildren = childWalks.flatMap(_._1)
                    val staticHandlers   = childWalks.flatMap(_._2)
                    val handle: Handler  = (targetPath, event) =>
                        dispatch(elem, basePath, targetPath, event, reactiveChildren, staticHandlers, cells)
                    (reactiveChildren, handle)
                end for

            case Fragment(children) =>
                for childWalks <- Kyo.foreach(children.toSeq.zipWithIndex) { (child, i) =>
                        val childPath = child match
                            case kc: KeyedChild[?] => basePath :+ kc.key
                            case _                 => basePath :+ i.toString
                        val childContext = child match
                            case kc: KeyedChild[?] => context.child(kc.key)
                            case _                 => context.child(i.toString)
                        // Three plain values rather than a tuple: this runs per row on every walk, dispatch included.
                        val keyed = child.isInstanceOf[KeyedChild[?]]
                        val depth = if keyed then basePath.size else 0
                        val inner = child match
                            case kc: KeyedChild[?] => kc.child
                            case _                 => child
                        val innerRows     = if keyed && depth < 63 then rows | (1L << depth) else rows
                        val innerDeepRows = if keyed && depth >= 63 then depth :: deepRows else deepRows
                        if needsOwnNode(inner) then
                            // Same contract as an element's reactive child: a row whose root carries a binding channel (a keyed row
                            // with a reactive class, say) gets its own node, or its in-place-patch observers would never start.
                            for rui <- normalizeAt(inner, childPath, svg, childContext, parentContext, cells, innerRows, innerDeepRows)
                            yield (Seq(rui), rui.handle)
                        else walkStatic(inner, childPath, svg, childContext, parentContext, cells, innerRows, innerDeepRows)
                        end if
                    }
                yield
                    val allKids    = childWalks.flatMap(_._1)
                    val allHandles = childWalks.zipWithIndex.map { case ((_, h), i) => (i, h) }
                    val keyMap     = children.toSeq.zipWithIndex.collect {
                        case (kc: KeyedChild[?], i) => kc.key -> i
                    }.toMap
                    val handle: Handler = (targetPath, event) =>
                        if targetPath.size > basePath.size then
                            val segment = targetPath(basePath.size)
                            val idx     = Maybe.fromOption(keyMap.get(segment)).orElse(Maybe.fromOption(segment.toIntOption))
                            idx.flatMap(i => Maybe.fromOption(allHandles.lift(i)).map(_._2))
                                .fold(false: Boolean < Async)(h => h(targetPath, event))
                        else true
                    (allKids, handle)

            case _: Reactive[?] | _: Foreach[?, ?] | _: Mounted =>
                // When walkStatic is called with a Reactive, Foreach or Mounted as the top-level node
                // (not as a child of an Element), normalize it at basePath so subscribeNode
                // sets up a subscription for it. This handles the case where an outer reactive's
                // signal value is itself a Reactive or Foreach (e.g. outer.map { _ => inner.map(UI.span(_)) }).
                for rui <- normalizeAt(ui, basePath, svg, context, parentContext, cells, rows, deepRows)
                yield (Seq(rui), rui.handle)

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
        pass: RegionKeys
    )(using Frame): Unit < (Async & Scope) =
        // Start the scoped in-place-patch observers for this node's reactive channels. Runs unconditionally: a
        // no-op for attribute-less nodes, but an element carrying ONLY reactive attrs is `isConst = true` and
        // would otherwise fork no observer of its own.
        forkChannelObservers(
            rui.path,
            rui.reactiveAttrs,
            rui.reactiveBoolAttrs,
            rui.reactiveClasses,
            exchange,
            rui.renderedAttrValues,
            rui.renderedBoolAttrValues,
            rui.renderedClassValues,
            rui.reactiveStyle,
            rui.renderedStyleValue
        ).andThen {
            if rui.mounted.nonEmpty then
                subscribeMounted(rui, rui.mounted.get, exchange, signalChangeTime, mounts, dispatch)
            else if rui.isConst then
                // A constant node belongs to the enclosing region's pass, which already counts its children.
                Kyo.foreachDiscard(rui.children)(subscribeScoped(_, exchange, signalChangeTime, mounts, dispatch, pass))
            else
                // The tree this region last rendered, so the exchange can send only what moved instead of the
                // whole region.
                // Unsafe: region-scoped, single-writer state created before the observe loop is forked, where no
                // effect context exists to supply AllowUnsafe; only this region's own loop reads or writes it.
                val rendered = AtomicRef.Unsafe.init(Maybe.empty[UI])(using AllowUnsafe.embrace.danger)
                // Unsafe: as for `rendered`, the region's reference is made before its loop is forked, where no proof is at hand.
                val regionMounts                                                 = mounts.child(pass, rui)(using AllowUnsafe.embrace.danger)
                def subscribeKids(kids: Seq[ReactiveUI]): Unit < (Async & Scope) =
                    val keys = RegionKeys(kids)
                    regionMounts.beginWalk(kids, keys)
                        .andThen(subscribeWalked(kids, keys, regionMounts, exchange, signalChangeTime, dispatch))
                end subscribeKids
                // Per-value setup, run inside each value's fresh Scope: render the region and fork its children into
                // that scope, so the next value (or an interrupt) tears them down by cascade.
                def renderValue(current: UI): Unit < (Async & Scope) =
                    for
                        now      <- Clock.now
                        _        <- signalChangeTime.set(now)
                        previous <- Sync.Unsafe.defer(rendered.getAndSet(Present(current)))
                        _        <- exchange.onChange(rui.region, rui.path, rui.contentContext, rui.parentContext, previous, current)
                        svgContext = rui.region match
                            case _: ReactiveRegion.HtmlRange  => false
                            case _: ReactiveRegion.SvgElement => true
                        contentParentContext = nestedParentContext(rui.parentContext, current)
                        (newKids, _) <- walkStatic(
                            current,
                            rui.path,
                            svgContext,
                            rui.contentContext,
                            contentParentContext,
                            dispatch,
                            rows = 0L,
                            deepRows = Nil,
                            discoverRootBound = rui.discoverContentRootBound
                        )
                        _ <- forkRootChannels(rui.path, current, exchange)
                        _ <- subscribeKids(newKids)
                    yield ()

                startOwnedFiber {
                    Abort.run[Throwable] {
                        var first = true
                        rui.signal.observe { current =>
                            val isFirst = first
                            first = false
                            if isFirst && rui.renderedValue.exists(_.equals(current)) then
                                // The enclosing render already painted exactly this value: skip the repaint and the
                                // re-walk, but still subscribe the already-walked children, exactly as renderValue
                                // would have. The value is recorded as this region's last render, so the NEXT emission
                                // diffs against it rather than re-sending the whole region.
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
            end if
        }
    end subscribeScoped

    /** Starts the channel observers of the element a region paints as its root. A walk only gives the root's children nodes of their
      * own, so the root's channels at `path` are started here, in the current scope like the children's subscriptions.
      */
    private[internal] def forkRootChannels(path: Seq[String], content: UI, exchange: UIExchange)(using Frame): Unit < (Async & Scope) =
        content match
            case el: Element
                if el.attrs.reactiveAttrs.nonEmpty || el.attrs.reactiveBoolAttrs.nonEmpty || el.attrs.reactiveClasses.nonEmpty
                    || el.attrs.reactiveStyle.nonEmpty =>
                forkChannelObservers(
                    path,
                    el.attrs.reactiveAttrs,
                    el.attrs.reactiveBoolAttrs,
                    el.attrs.reactiveClasses,
                    exchange,
                    style = el.attrs.reactiveStyle
                )
            case _ => Kyo.unit

    private def forkChannelObservers(
        path: Seq[String],
        attrs: Map[String, Signal[String]],
        boolAttrs: Map[String, Signal[Boolean]],
        classes: Map[String, Signal[Boolean]],
        exchange: UIExchange,
        renderedAttrs: Map[String, String] = Map.empty,
        renderedBools: Map[String, Boolean] = Map.empty,
        renderedClasses: Map[String, Boolean] = Map.empty,
        style: Maybe[Signal[Style]] = Absent,
        renderedStyle: Maybe[Seq[(String, String)]] = Absent
    )(using Frame): Unit < (Async & Scope) =
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
                Kyo.foreachDiscard(signals.toSeq) { (name, sig) =>
                    val baseline = Maybe.fromOption(rendered.get(name))
                    patcher match
                        case Present(patch) => bindChannel(sig, baseline, (v: A) => patch(path, name, v))
                        case Absent         => Fiber.init(sig.observe(baseline, Signal.defaultRepairInterval)(slow(name, _))).unit
                }
        end bindAll

        bindAll(attrs, renderedAttrs, exchange.attrPatcherNow, exchange.onAttrPatch(path, _, _))
            .andThen(bindAll(boolAttrs, renderedBools, exchange.boolAttrPatcherNow, exchange.onBoolAttrPatch(path, _, _)))
            .andThen(bindAll(classes, renderedClasses, exchange.classPatcherNow, exchange.onClassPatch(path, _, _)))
            .andThen(style.fold(Kyo.unit: Unit < (Async & Scope))(bindStyle(path, _, renderedStyle, exchange)))
    end forkChannelObservers

    /** The inline-style channel. A value is its declarations, and a patch sets them and removes the properties the previous value had
      * and this one lacks: the previous value is what this observer last wrote, seeded with what the render wrote, which only the
      * observer knows (the client never saw a value as a value).
      */
    private def bindStyle(path: Seq[String], sig: Signal[Style], renderedStyle: Maybe[Seq[(String, String)]], exchange: UIExchange)(using
        Frame
    ): Unit < (Async & Scope) =
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
                    (next: Seq[(String, String)]) => patch(path, next, step(next)(using AllowUnsafe.embrace.danger))
                )
            case Absent =>
                Fiber.init(decls.observe(renderedStyle, Signal.defaultRepairInterval)(next =>
                    Sync.Unsafe.defer(step(next)).map(exchange.onStylePatch(path, next, _))
                )).unit
        end match
    end bindStyle

    /** Binds a channel's synchronous `write` as a callback on `sig`: inside the writer's `set` where the signal provides that path, through
      * `onChange` where it does not. The Log is read here, where the subscription is made: a write runs inside somebody else's `set`, where
      * none can be read, so a failing write is logged to the Log bound at the subscription.
      */
    private def bindChannel[A](sig: Signal[A], rendered: Maybe[A], write: A => Unit)(using CanEqual[A, A], Frame): Unit < (Sync & Scope) =
        Log.use { log =>
            Sync.Unsafe.defer {
                val report: Throwable => Unit = ex => log.unsafe.error("kyo-ui: channel binding failed", ex)
                sig.unsafeObserveProjected[A](identity, rendered, write, report) match
                    case Present(release) => Scope.ensure(Sync.defer(release()))
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
