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
    mountedSpec: Maybe[MountedSpec] = Absent,
    mountDispatch: Maybe[MountDispatch] = Absent, // set on the ROOT normalize product only; see subscribe
    // Reactive channels carried from the element's Attrs; subscribeScoped forks the in-place-patch observers
    // (empty for non-element / attribute-less nodes).
    reactiveAttrs: Map[String, Signal[String]] = Map.empty,
    reactiveBoolAttrs: Map[String, Signal[Boolean]] = Map.empty,
    reactiveClasses: Map[String, Signal[Boolean]] = Map.empty,
    // Render-time snapshots: the values the enclosing paint already put into the DOM, captured at normalize
    // time (which happens-before the HTML render on every paint path). Subscribe skips an observer's FIRST
    // emission when the current value still equals its snapshot: the DOM is already correct, so the initial
    // patch would only repaint freshly painted state (per child this compounds into a multi-second post-paint
    // tail on 1k-child keyed lists). Values that cannot compare structurally never match and keep the
    // unskipped behavior.
    renderedValue: Maybe[UI] = Absent,
    renderedAttrValues: Map[String, String] = Map.empty,
    renderedBoolAttrValues: Map[String, Boolean] = Map.empty,
    renderedClassValues: Map[String, Boolean] = Map.empty,
    // Present on Foreach regions: carries the AST node (signal, key, render) and the normalize-time items
    // snapshot so subscribe can run the per-row reuse path instead of the whole-region re-render loop.
    foreachSpec: Maybe[ForeachSpec] = Absent,
    // Present on a region lifted from a Signal[String] (UI.Ast.Reactive.text): the content is one text node
    // whatever the signal emits, so subscribe can bind the backend's text write instead of forking the
    // render-walk-paint loop. Carried through normalize rather than re-derived, because "this renders to a lone
    // text node" is knowable at the lift site and only guessable afterwards.
    textSignal: Maybe[Signal[String]] = Absent,
    // Present on a region built by `Signal.render` (UI.Ast.Reactive.source): the signal BEFORE the projection
    // to UI, with the projection. Subscribe observes this instead of the projected signal, so an emission whose
    // value is unchanged is dropped by `observe` itself rather than re-rendering a subtree that can only be
    // compared as UI — and a UI built from handler closures never compares equal. Foreach carries the same pair
    // for its rows; this is that treatment for the single-value region.
    sourceSpec: Maybe[UI.Ast.Reactive.Source[?]] = Absent,
    // Source position of the AST node this was normalized from: file, line, enclosing method, snippet. Carried
    // for [[Devtools]], which needs a name for a region and has only a path otherwise — and a path ("3.1.0")
    // says nothing to a reader. Free to carry: every UI node already holds a macro-derived `Frame`, and the
    // `Frame` macro refuses to expand inside the `kyo` package, so it always points at user code.
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

/** Session-level dispatch table for mounted nodes: path → live content cell.
  *
  * Event dispatch re-derives handler chains from `signal.current` on every event, re-running any `Signal.map`
  * lambdas on the way, so the Mounted node value (and any state carried on it or its normalize product) is FRESH
  * per dispatch and cannot hold the subscribe-time wiring. The table is the stable meeting point: created once per
  * normalize root (one per mount session), closed over by every handler the normalization tree builds, written by
  * subscribeMounted (registration is Scope-bound: the per-value scope that subscribed a node unregisters it, and
  * observe's closed-and-awaited ordering makes unregister-then-reregister safe across region rebuilds), and read by
  * the dispatch-time handler of whatever fresh Mounted node value occupies the same path.
  */
final private[kyo] class MountDispatch(
    cells: AtomicRef[Dict[Seq[String], Signal[UI]]],
    // Session-scoped, and that is the whole point: a MountRegistry cannot count this, because the
    // registry is exactly what goes missing. See `noteKeyedMiss`.
    keyedMisses: AtomicRef[Dict[Seq[String], (Any, Int)]]
):
    def register(path: Seq[String], cell: Signal[UI])(using Frame): Unit < Sync =
        cells.getAndUpdate(_.update(path, cell)).unit

    /** Dev hint against the keyed-mount-in-a-doomed-registry anti-pattern: a key that keeps MISSING.
      *
      * A keyed mount claims its instance from the registry of the nearest enclosing region, so its
      * continuity spans re-renders of THAT region and ends with it. Under an intervening region —
      * one that is itself re-subscribed when something further out re-renders — the registry is new
      * and empty every time, the claim misses, and the instance is rebuilt exactly as a keyless one
      * would be. The state the mount allocated is gone with it.
      *
      * Nothing else reports this. `noteKeylessRemount` counts only KEYLESS mounts, and `noteKeyAt`'s
      * unstable-key streak lives in the registry that just vanished, so both are silent here.
      *
      * The same key missing at the same path, over and over, is the signal: the first miss is an
      * ordinary first mount, and a key that CHANGED is a deliberate re-creation, which resets the
      * count rather than adding to it.
      *
      * Returns the running count, which is what decides whether anything is logged — the streak is
      * the testable part, the message is not.
      */
    def noteKeyedMiss(path: Seq[String], key: Any)(using Frame): Int < Sync =
        keyedMisses.getAndUpdate { m =>
            m.get(path) match
                case Present((prev, n)) if prev.equals(key) => m.update(path, (key, n + 1))
                case _                                      => m.update(path, (key, 1))
        }.map { prev =>
            val n = prev.get(path) match
                case Present((p, c)) if p.equals(key) => c + 1
                case _                                => 1
            if n == 10 || n == 100 || n == 1000 then
                Log.warn(
                    s"kyo-ui: keyed UI.mounted '$key' at ${path.mkString(".")} was re-created $n times: its key is " +
                        "stable, but the mount registry it is claimed from is not — a region between it and the " +
                        "nearest enclosing mount is re-subscribed on every pass, so the instance never survives. " +
                        "Keyed continuity spans re-renders of the immediately enclosing region and ends with that " +
                        "region. Either place the mount directly in the content of a mount that outlives those " +
                        "passes, or give the state to the caller as a bound ref instead of allocating it here."
                ).andThen(n)
            else Kyo.lift(n)
            end if
        }
    end noteKeyedMiss

    /** Unregister only if `cell` is still the registered one: a successor that already re-registered wins. */
    def unregister(path: Seq[String], cell: Signal[UI])(using Frame): Unit < Sync =
        cells.getAndUpdate(m => if m.get(path).exists(_ eq cell) then m.remove(path) else m).unit

    def lookup(path: Seq[String])(using Frame): Maybe[Signal[UI]] < Sync =
        cells.get.map(_.get(path))
end MountDispatch

private[kyo] object MountDispatch:
    def init(using Frame): MountDispatch < Sync =
        for
            cells  <- AtomicRef.init(Dict.empty[Seq[String], Signal[UI]])
            misses <- AtomicRef.init(Dict.empty[Seq[String], (Any, Int)])
        yield new MountDispatch(cells, misses)
end MountDispatch

/** Normalization-time companion of a [[kyo.UI.Ast.Mounted]] node: the node, the render path it was normalized at, and its
  * rendering defaults.
  */
final private[kyo] case class MountedSpec(node: UI.Ast.Mounted, path: Seq[String]):
    def placeholderUI(using Frame): UI = node.placeholderUI.getOrElse(UI.empty)

    def errorUI(err: UI.MountError): UI =
        node.errorRender match
            case Present(f) => f(err)
            case Absent =>
                given Frame = node.frame
                UI.span(UI.Ast.Text(err.message)).cssClass("kyo-mount-error")

    def mountError(t: Throwable, source: UI.MountErrorSource): UI.MountError =
        UI.MountError(t, source, Maybe(t.getMessage).getOrElse(t.toString), node.frame, path, node.key)
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
        parentContext: ReactiveRegion.ParentContext
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
            frame = frame
        )

    /** Root entry point: creates the session's MountDispatch table and stamps it on the returned root node
      * (subscribe picks it up from there). All recursion goes through normalizeWith so every handler closure
      * shares the one table.
      */
    def normalize(ui: UI, path: Seq[String], svg: Boolean = false): ReactiveUI < Sync =
        given Frame = ui.frame
        for
            mountDispatch <- MountDispatch.init
            root <- normalizeWith(
                ui,
                path,
                svg,
                ReactiveRegion.RegionIdentity.root(path),
                ReactiveRegion.ParentContext.Other,
                mountDispatch
            )
        yield root.copy(mountDispatch = Present(mountDispatch))
        end for
    end normalize

    private def normalizeWith(
        ui: UI,
        path: Seq[String],
        svg: Boolean,
        regionIdentity: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        mountDispatch: MountDispatch
    ): ReactiveUI < Sync =
        given Frame = ui.frame
        ui match
            case ui: Reactive[?] =>
                val contentContext = regionIdentity.transparent
                for
                    current <- ui.signal.current
                    contentParentContext = nestedParentContext(parentContext, current)
                    (kids, _) <- walkStatic(current, path, svg, contentContext, contentParentContext, mountDispatch)
                yield init(path, ui.signal, isConst = false, kids, svg, regionIdentity, contentContext, parentContext) {
                    (targetPath, event) =>
                        for
                            currentUI <- ui.signal.current
                            contentParentContext = nestedParentContext(parentContext, currentUI)
                            (_, freshHdl) <- walkStatic(currentUI, path, svg, contentContext, contentParentContext, mountDispatch)
                            result        <- freshHdl(targetPath, event)
                        yield result
                }.copy(renderedValue = Present(current), textSignal = ui.text, sourceSpec = ui.source)
                end for

            case ui: Foreach[?, ?] @unchecked =>
                ui.applyTyped {
                    [T] =>
                        (itemSignal, keyFn, renderFn) =>
                            val sig            = itemSignal.map(items => foreachFragment(keyFn, renderFn, items))
                            val contentContext = regionIdentity
                            for
                                rows   <- RowRegistry.init
                                items0 <- itemSignal.current
                                current              = foreachFragment(keyFn, renderFn, items0)
                                contentParentContext = nestedParentContext(parentContext, current)
                                (kids, _) <- walkStatic(current, path, svg, contentContext, contentParentContext, mountDispatch)
                            yield
                                // Fallback dispatch: re-render the whole list from the current value and route
                                // through the fresh fragment handler. Used whenever the row registry cannot
                                // resolve the target (region not subscribed on the reuse path, row painted but
                                // not yet registered, or an event addressed to the region node itself).
                                val fullWalk: Handler = (targetPath, event) =>
                                    for
                                        currentUI <- sig.current
                                        freshParentContext = nestedParentContext(parentContext, currentUI)
                                        (_, freshHdl) <- walkStatic(currentUI, path, svg, contentContext, freshParentContext, mountDispatch)
                                        result        <- freshHdl(targetPath, event)
                                    yield result
                                init(path, sig, isConst = false, kids, svg, regionIdentity, contentContext, parentContext) {
                                    (targetPath, event) =>
                                        // Row-registry fast path: the segment after the region path is the row
                                        // key, so the live RowInstance resolves the target directly. Its cached
                                        // handler is as fresh as a re-derived one: the registry retains a row
                                        // only while its item value is unchanged (a changed item rebuilds the
                                        // row, and with it the handler), which is the same purity assumption
                                        // the paint reuse already makes. A seeded row has no cached handler
                                        // yet; its cached rowUI is walked on demand (one row, not the list).
                                        if targetPath.size > path.size then
                                            rows.snapshot.map { live =>
                                                live.find(_.key == targetPath(path.size)) match
                                                    case Some(inst) =>
                                                        inst.handler match
                                                            case Present(h) => h(targetPath, event)
                                                            case Absent =>
                                                                walkRow(
                                                                    inst.rowUI,
                                                                    path :+ inst.key,
                                                                    svg,
                                                                    contentContext.child(inst.key),
                                                                    nestedParentContext(parentContext, inst.rowUI),
                                                                    mountDispatch
                                                                )
                                                                    .map(_._2(targetPath, event))
                                                    case None => fullWalk(targetPath, event)
                                            }
                                        else fullWalk(targetPath, event)
                                }.copy(
                                    renderedValue = Present(current),
                                    foreachSpec = Present(ForeachSpec(ui, Present(items0), rows))
                                )
                            end for
                }

            case ui: Element =>
                // An element with a SignalRef-bound attribute (`.value(ref)` xor `.checked(ref)`; an element binds at most
                // one, see collectSignalRef) must re-render when that signal changes. The change is carried by the ref
                // (read afresh at render time), not by the emitted value's identity: the emitted value is always the same
                // `ui` object, kept with its `Bound.Ref` attributes so the rendered HTML carries the auto-binding event
                // markers `data-kyo-ev="input"/"change"` the client needs and the dispatch handler resolves the ref.
                //
                // `changesTo`, NOT `map(_ => ui)`: an observation deduplicates on the emitted value, and this one never
                // changes, so a map would deliver the first paint and then go silent — the field would stop tracking its
                // own ref forever. `changesTo` observes the ref on the REF's values and hands over the constant, which is
                // exactly what a region rebuilt from the ref at render time needs. An element with no bound ref is const.
                val (elementSignal, isConstNode) =
                    collectSignalRef(ui).fold((Signal.initConst(ui: UI), true))(ref => (ref.changesTo(ui: UI), false))
                for
                    (kids, hdl) <- walkStatic(ui, path, svg, regionIdentity, parentContext, mountDispatch, discoverRootBound = false)
                    attrSnap    <- Kyo.foreach(ui.attrs.reactiveAttrs.toSeq)((n, s) => s.current.map(v => n -> v))
                    boolSnap    <- Kyo.foreach(ui.attrs.reactiveBoolAttrs.toSeq)((n, s) => s.current.map(v => n -> v))
                    classSnap   <- Kyo.foreach(ui.attrs.reactiveClasses.toSeq)((n, s) => s.current.map(v => n -> v))
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
                    reactiveAttrs = ui.attrs.reactiveAttrs,
                    reactiveBoolAttrs = ui.attrs.reactiveBoolAttrs,
                    reactiveClasses = ui.attrs.reactiveClasses,
                    renderedAttrValues = attrSnap.toMap,
                    renderedBoolAttrValues = boolSnap.toMap,
                    renderedClassValues = classSnap.toMap,
                    frame = ui.frame
                )
                end for

            case ui: Mounted =>
                // The handler resolves through the MountDispatch TABLE (by path), not the node: dispatch re-normalizes
                // a fresh node value per event, so no live wiring can ride the node. The stored signal is a placeholder
                // constant the subscribe path never observes (subscribeScoped branches on mountedSpec first).
                val spec = MountedSpec(ui, path)
                val handle: Handler = (targetPath, event) =>
                    mountDispatch.lookup(path).map {
                        case Present(cell) =>
                            for
                                currentUI <- cell.current
                                contentParentContext = nestedParentContext(parentContext, currentUI)
                                (_, freshHdl) <- walkStatic(
                                    currentUI,
                                    path,
                                    svg,
                                    regionIdentity.transparent,
                                    contentParentContext,
                                    mountDispatch
                                )
                                result <- freshHdl(targetPath, event)
                            yield result
                        case Absent => (true: Boolean) // not (or no longer) wired: bubble
                    }
                init(
                    path,
                    Signal.initConst(spec.placeholderUI),
                    isConst = false,
                    Seq.empty,
                    svg,
                    regionIdentity,
                    regionIdentity.transparent,
                    parentContext
                )(handle).copy(mountedSpec = Present(spec))

            case ui =>
                // Catch-all for static leaf nodes: Text, Fragment, KeyedChild.
                // All interactive types extend Element (handled above) and all reactive types are
                // Reactive, Foreach, or Mounted (also handled above). If a new UI subtype is added,
                // the exhaustiveness checker will NOT warn here; any new type that is interactive must
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
    end normalizeWith

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
      * between them: a fragment child (e.g. a keyed Foreach row) is promoted by exactly the same predicate as
      * an element child, or its in-place-patch observers would silently never start.
      */
    private def needsOwnNode(ui: UI): Boolean = ui match
        case _: Reactive[?] | _: Foreach[?, ?] | _: Mounted => true
        case el: Element =>
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

    private type Handler = (Seq[String], UIEvent) => Boolean < Async

    private type ValidatedHandler = (Seq[String], DragProtocol.ValidatedEvent) => Boolean < Async

    private type DragResolver = Maybe[(String, Drag.Decision) => Unit < Async]

    final private case class DragSession(
        event: Drag.Event,
        terminalResolved: Boolean,
        expiresAt: Instant,
        resolver: DragResolver
    )

    final private[kyo] case class DragSessionLimits(
        maxSessions: Int = 128,
        lifetime: Duration = 5.minutes
    ):
        require(maxSessions > 0, s"DragSessionLimits.maxSessions must be positive: $maxSessions")
        require(lifetime > Duration.Zero, s"DragSessionLimits.lifetime must be positive: $lifetime")
    end DragSessionLimits

    /** Walk a static UI tree. Collect reactive children, build handle. */
    private def walkStatic(
        ui: UI,
        basePath: Seq[String],
        svg: Boolean,
        context: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        mountDispatch: MountDispatch,
        discoverRootBound: Boolean = true
    )(using Frame): (Seq[ReactiveUI], Handler) < Sync =
        ui match
            case elem: Element if discoverRootBound && collectSignalRef(elem).nonEmpty =>
                for rui <- normalizeWith(elem, basePath, svg, context, parentContext, mountDispatch)
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
                            for rui <- normalizeWith(child, childPath, childSvg, childContext, childParentContext, mountDispatch)
                            yield (Seq(rui), Seq.empty[(Int, Handler)])
                        else
                            for (innerKids, innerHandle) <-
                                    walkStatic(child, childPath, childSvg, childContext, childParentContext, mountDispatch)
                            yield (innerKids, Seq((i, innerHandle)))
                        end if
                    }
                yield
                    val reactiveChildren = childWalks.flatMap(_._1)
                    val staticHandlers   = childWalks.flatMap(_._2)
                    val handle: Handler = (targetPath, event) =>
                        dispatch(elem, basePath, targetPath, event, reactiveChildren, staticHandlers)
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
                        val inner = child match
                            case kc: KeyedChild[?] => kc.child
                            case _                 => child
                        if needsOwnNode(inner) then
                            // Same contract as an Element's reactive child: the renderer paints the
                            // node's own anchor at childPath (no content descent), so normalize there
                            // directly. Recursing through walkStatic would hit the TOP-LEVEL branch,
                            // which registers at childPath :+ "$r", so every patch would then target a
                            // path the painted DOM does not have (mount stuck on its placeholder,
                            // reactive updates silently dropped). Element roots with binding channels
                            // (e.g. a keyed row carrying a reactive class) are promoted by the same
                            // predicate, or their in-place-patch observers would never start.
                            for rui <- normalizeWith(inner, childPath, svg, childContext, parentContext, mountDispatch)
                            yield (Seq(rui), rui.handle)
                        else
                            walkStatic(inner, childPath, svg, childContext, parentContext, mountDispatch)
                        end if
                    }
                yield
                    val allKids    = childWalks.flatMap(_._1)
                    val allHandles = childWalks.zipWithIndex.map { case ((_, h), i) => (i, h) }
                    val keyMap = children.toSeq.zipWithIndex.collect {
                        case (kc: KeyedChild[?], i) => kc.key -> i
                    }.toMap
                    val handle: Handler = (targetPath, event) =>
                        if targetPath.size > basePath.size then
                            val segment = targetPath(basePath.size)
                            val idx     = Maybe.fromOption(keyMap.get(segment)).orElse(Maybe.fromOption(segment.toIntOption))
                            idx.flatMap(i => Maybe.fromOption(allHandles.lift(i)).map(_._2))
                                .fold(true: Boolean < Async)(h => h(targetPath, event))
                        else true
                    (allKids, handle)

            case _: Reactive[?] | _: Foreach[?, ?] | _: Mounted =>
                // When walkStatic is called with a Reactive, Foreach, or Mounted as the top-level node
                // (not as a child of an Element), normalize it at basePath so subscribeNode
                // sets up a subscription for it. This handles the case where an outer reactive's
                // signal value is itself a Reactive or Foreach (e.g. outer.map { _ => inner.map(UI.span(_)) }).
                for rui <- normalizeWith(ui, basePath, svg, context, parentContext, mountDispatch)
                yield (Seq(rui), rui.handle)

            case _ =>
                val noHandle: Handler = (_, _) => true
                (Seq.empty[ReactiveUI], noHandle)

    /** Walk one reusable Foreach row. A row ROOT carrying a binding channel (or being itself reactive) must be
      * promoted to its own ReactiveUI node, exactly like a fragment child in walkStatic: a bare walkStatic on
      * the row would only walk its children and silently never start the root's observers. Returns the walked
      * reactive kids plus the row's dispatch handler (cached on the RowInstance for registry-first dispatch).
      */
    private def walkRow(
        rowUI: UI,
        rowPath: Seq[String],
        svg: Boolean,
        context: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        mountDispatch: MountDispatch
    )(using Frame): (Seq[ReactiveUI], Handler) < Sync =
        if needsOwnNode(rowUI) then
            normalizeWith(rowUI, rowPath, svg, context, parentContext, mountDispatch).map(rui => (Seq(rui), rui.handle))
        else walkStatic(rowUI, rowPath, svg, context, parentContext, mountDispatch)

    /** Dispatch an event through an element. */
    private def dispatch(
        elem: Element,
        myPath: Seq[String],
        targetPath: Seq[String],
        event: UIEvent,
        reactiveChildren: Seq[ReactiveUI],
        staticHandlers: Seq[(Int, Handler)]
    )(using Frame): Boolean < Async =
        if targetPath == myPath then
            dispatchToElement(elem, event, isTarget = true, selfPath = myPath)
        else if targetPath.startsWith(myPath) && targetPath.size > myPath.size then
            val childSegment = targetPath(myPath.size)
            val childIdx     = Maybe.fromOption(childSegment.toIntOption)

            // Find the reactive child whose path is a prefix of (or equals) targetPath. Matching by
            // `lastOption.contains(childSegment)` is incorrect when reactive children are nested
            // multiple levels deep; their last segment may collide with a sibling's index at the
            // current level. Using prefix-match correctly routes only when the target lies inside.
            val reactiveChild = Maybe.fromOption(reactiveChildren.find(rc => targetPath.startsWith(rc.path)))
            // STATIC descent takes precedence: route through the static child's handler chain so intermediate bubble
            // handlers run. Jumping straight to a DEEP reactive child would skip them: a click on a reactive label
            // inside a button (`button(labelSignal)`) would bypass the button's own onClick. Only a DIRECT reactive
            // child (no staticHandlers entry at its index) takes the reactive jump.
            val staticChild = childIdx.flatMap(i => Maybe.fromOption(staticHandlers.find(_._1 == i)).map(_._2))

            val isClick   = event.isInstanceOf[UIEvent.Click]
            val isKeyDown = event.isInstanceOf[UIEvent.KeyDown]
            for
                // Disabled-target (Form submit suppression), Button-target (only Button clicks submit
                // forms), Select-target (Enter on Select must not submit) and control-below all resolve
                // through any Reactive/Foreach boundary wrapping the target, so signal-typed setters do
                // not hide it — and all four come out of ONE resolution, because they are four questions
                // about one element rather than four searches for it.
                facts <-
                    if isClick || isKeyDown then targetFacts(elem, myPath, targetPath)
                    else Kyo.lift(TargetFacts.none)
                // "Is it disabled" is a question about the CURRENT value behind a channel, not about a
                // field (see boolAttrNow), so it is read from the resolved element rather than carried.
                targetDisabled <-
                    if isClick then facts.target.fold(Kyo.lift(false))(isDisabled) else Kyo.lift(false)
                // A button's TYPE decides whether activating it submits, not its element kind:
                // `type="button"` — which is `uic.Button`'s default — submits nothing in a
                // browser, and both DOM clients already read it. `Absent` here means the target
                // is no button at all, which the Enter path below needs to tell apart from "a
                // button that does not submit": one still submits (implicit submission from a
                // text field), the other must not.
                targetButtonSubmits = facts.target match
                    case Present(b: Button) => Present(ButtonActivation.submits(b.attrs.jsProps.getOrElse("type", "")))
                    case _                  => Absent
                targetIsSelect = isKeyDown && facts.target.exists(_.isInstanceOf[Select])
                // Whether a control of the reader's own stands between this element and the target,
                // reported to the handler rather than acted on here: only the handler knows whether
                // its click means something the control already means (see UI.MouseEvent.onControl).
                targetOnControl = isClick && facts.controlBelow
                bubble = dispatchToElement(
                    elem,
                    event,
                    isTarget = false,
                    selfPath = myPath,
                    disabledTarget = targetDisabled,
                    submitOrigin = isClick && targetButtonSubmits.getOrElse(false),
                    selectTarget = targetIsSelect,
                    controlTarget = targetOnControl
                )
                result <- staticChild match
                    case Present(childHandle) =>
                        safeDispatch(childHandle, targetPath, event).map(keep => if keep then bubble else false)
                    case Absent =>
                        reactiveChild.fold(bubble)(child =>
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
      * click. Keyboard activation of a button IS a click the browser fires, and Enter in a field is
      * implicit submission, which the specification also defines as firing a click — at the form's
      * default button. Neither is "submit on a key". The dispatcher used to shortcut both by calling
      * `onClick` at the target (which does not bubble, so no ancestor saw the activation) and by
      * submitting straight from the form on Enter, which is why the type rule had to exist twice and
      * why the two disagreed. Recording a PATH here and dispatching a real click at it afterwards puts
      * the whole of activation back on the one path the platform has.
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
      * caller falls back to submitting the form directly — the same answer HTML gives a form with no
      * submit button at all, and the same thing this code did for every form before.
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
                nextIdx =
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

    /** Is the boolean attribute `name` set on `elem` RIGHT NOW — statically, or through the reactive channel a
      * `Signal`-typed setter installs.
      *
      * `.disabled(Signal)` and `.readOnly(Signal)` do not wrap their element and do not fill the static field:
      * they register a boolean-attribute channel (`Attrs.reactiveBoolAttrs`) that patches the live attribute
      * in place. A guard reading only the static field therefore counts every signal-disabled element as
      * enabled — silently, because a browser suppresses the click on a rendered `disabled` button, so it shows
      * only where dispatch decides for itself: the server transport, and any forged event.
      *
      * WHERE A CHANNEL EXISTS, THE CHANNEL IS THE VALUE — the static field is not consulted at all. Same rule
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

    /** A TARGET that is inert — disabled, hidden, read-only — ignores its own handler but still lets the event
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
        /** Nothing was asked, so nothing was found — the answer for an event that resolves no target. */
        val none: TargetFacts = TargetFacts(Absent, false)

    /** Resolve the (possibly reactive) node at `targetPath` to the concrete element there, noting on the way
      * whether the path passed through a control.
      *
      * Mirrors the renderer's path scheme: element children are index-addressed, a `Reactive`'s rendered content occupies the same path as
      * the boundary, `Foreach` items live at `path :+ key` (or `:+ index`), and `Fragment` children at `path :+ index`. Resolving through
      * these boundaries is what lets an element wrapped by a signal-typed setter (e.g. `.hidden(Signal)`, which wraps it in a `Reactive`)
      * still be recognized by its concrete type, so button-click form submit and disabled detection keep working through such wrappers.
      *
      * ONE walk answers every question a click asks. It used to be one walk per question — disabled,
      * button, control-below — and a walk is not free: resolving through a `Foreach` renders the target
      * row to reach it, so three questions rendered the same row three times per click. The questions are
      * about one element; they are now asked of one resolution.
      *
      * `controlBelow` is accumulated rather than tested at the leaf because it is a question about the
      * whole chain: a click on the icon inside a button targets the icon, and the icon alone cannot say
      * that a control was involved. `rootDepth` is what makes it STRICTLY below — the dispatching element
      * never answers for itself, or a button would decline its own click.
      */
    private def resolveTarget(
        node: UI,
        nodePath: Seq[String],
        targetPath: Seq[String],
        rootDepth: Int,
        controlBelow: Boolean
    )(using
        Frame
    ): TargetFacts < Sync =
        // A path that stops short still answers the control question: the old per-question walk
        // short-circuited to `true` the moment it saw a control, so a control ABOVE a boundary the
        // resolver cannot see through (a mount, a text leaf) counted. Carrying the flag out keeps that.
        def unresolved(seen: Boolean): TargetFacts = TargetFacts(Absent, seen)
        node match
            case kc: KeyedChild[?] =>
                resolveTarget(kc.child, nodePath, targetPath, rootDepth, controlBelow)
            case r: Reactive[?] =>
                // A Reactive's rendered content occupies the same path as the boundary, so re-resolve at nodePath.
                r.signal.current(using r.frame).map(cur => resolveTarget(cur, nodePath, targetPath, rootDepth, controlBelow))
            case Fragment(children) =>
                if targetPath.size <= nodePath.size then unresolved(controlBelow)
                else
                    val seg = targetPath(nodePath.size)
                    Maybe.fromOption(seg.toIntOption) match
                        case Present(i) if i >= 0 && i < children.size =>
                            resolveTarget(children(i), nodePath :+ seg, targetPath, rootDepth, controlBelow)
                        case _ => unresolved(controlBelow)
                    end match
            case fe: Foreach[?, ?] @unchecked =>
                if targetPath.size <= nodePath.size then unresolved(controlBelow)
                else
                    val seg = targetPath(nodePath.size)
                    fe.applyTyped {
                        [T] =>
                            (signal, keyFn, renderFn) =>
                                signal.current(using fe.frame).map { items =>
                                    val idx = keyFn match
                                        case Present(f) => items.indexWhere(it => f(it) == seg)
                                        case Absent     => Maybe.fromOption(seg.toIntOption).getOrElse(-1)
                                    if idx >= 0 && idx < items.size then
                                        resolveTarget(renderFn(idx, items(idx)), nodePath :+ seg, targetPath, rootDepth, controlBelow)
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
                            resolveTarget(e.children(i), nodePath :+ seg, targetPath, rootDepth, seen)
                        case _ => unresolved(seen)
                    end match
                end if
            // Text and RawHtml are leaf content with no kyo-addressable Element children, so no event
            // target can resolve through them.
            case _: Text | _: RawHtml => unresolved(controlBelow)
            // The path does not resolve THROUGH a mount boundary: the content lives behind a subscribe-time cell this
            // static resolver cannot reach (v1 limitation: a Button in a mounted subtree does not trigger an OUTER
            // Form's submit-on-bubble refinement). Event dispatch still resolves via the node's handler indirection.
            case _: Mounted => unresolved(controlBelow)
        end match
    end resolveTarget

    /** The facts a bubbling dispatch reads about its target, in a single resolution. */
    private def targetFacts(elem: Element, myPath: Seq[String], targetPath: Seq[String])(using Frame): TargetFacts < Sync =
        resolveTarget(elem, myPath, targetPath, myPath.size, false)

    /** Whether an element is a control in its own right — something the reader operates directly, as
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
      * click (see [[activationTarget]]), so there is nothing for the keydown path to decide: it used to
      * carry `nonSubmitButton` and `submitButtonTarget` for a submit rule of its own, and both are gone
      * with it.
      *
      * `selfPath` is this element's own path, needed because a keydown records WHERE its activation
      * click belongs rather than acting on it here.
      */
    private def dispatchToElement(
        elem: Element,
        event: UIEvent,
        isTarget: Boolean,
        selfPath: Seq[String] = Seq.empty,
        disabledTarget: Boolean = false,
        submitOrigin: Boolean = false,
        selectTarget: Boolean = false,
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
                    val self = if isTarget then
                        invoke(attrs.onClickSelf).andThen(invokeWith(attrs.onClickSelfEvt, mouse))
                    else Kyo.lift(())
                    // Checkbox/radio toggle is handled by UIControlSession.click() which dispatches
                    // ChangeChecked after Click. Don't toggle here to avoid double-toggle.
                    val activateToggle = Kyo.lift(())
                    // When a Click on a SUBMITTING Button bubbles to a Form, trigger onSubmit (browser
                    // behavior). Not every Button: `type="button"` and `type="reset"` activate without
                    // submitting, and `uic.Button` defaults to the former — see ButtonActivation, whose
                    // rule both DOM clients apply to the same markup.
                    val formSubmit = if !isTarget && !disabledTarget && submitOrigin then
                        elem match
                            case f: Form =>
                                invoke(f.onSubmit).andThen(invokeWith(f.onSubmitEvt, mouse))
                            case _ => Kyo.lift(())
                    else Kyo.lift(())
                    // Self handlers only count as "declared" when they actually fired (isTarget).
                    val declared = attrs.onClick.nonEmpty || attrs.onClickEvt.nonEmpty ||
                        (isTarget && (attrs.onClickSelf.nonEmpty || attrs.onClickSelfEvt.nonEmpty))
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
                        if isTarget && (e.keyboard.key == "ArrowDown" || e.keyboard.key == "ArrowUp" || e.keyboard.key == " " || e.keyboard.key == "Enter")
                        then
                            elem match
                                case sel: Select => cycleSelectOption(sel, forward = e.keyboard.key != "ArrowUp")
                                case _           => Kyo.lift(())
                        else Kyo.lift(())
                    // Implicit submission: HTML's rule for Enter in a field, and the ONLY thing a keydown
                    // does about submitting now. The specification fires a click at the form's default
                    // button, so that is what gets recorded; the form's own onSubmit runs from that
                    // click, through the same `submitOrigin` a mouse click uses. A form with no
                    // reachable submit button has no click to fire, and HTML submits it directly.
                    //
                    // Nothing is recorded when the target already recorded its own activation — a
                    // button's Enter is that button's click, not the form's implicit submission — and
                    // Enter on a Select is a dropdown interaction rather than a submit.
                    val formSubmit =
                        if !isTarget && e.keyboard.key == "Enter" && !selectTarget then
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
            case _: UIEvent.Drop =>
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
                _ <- result match
                    case Result.Success(decision) => recordDecision(decisions, decision)
                    case Result.Failure(err) =>
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
            case _ =>
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
        private val dragExpiryWorkerCountFn: () => Int < Sync = () => 0
    ):
        private[kyo] def dragSessionCount(using Frame): Int < Sync      = dragSessionCountFn()
        private[kyo] def dragExpiryWorkerCount(using Frame): Int < Sync = dragExpiryWorkerCountFn()
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
            _ <- startOwnedFiber(
                Abort.run[Any] {
                    Sync.ensure(expiryWorkers.set(0)) {
                        expiryWorkers.set(1).andThen(expiryScheduler(dragSessions, dragMutex, expiryWake))
                    }
                }.unit
            )
            rootMounts <- MountRegistry.init
            _          <- Scope.ensure(rootMounts.evictAll)
            // Absent only for hand-built ReactiveUIs in tests; a normalize-produced root always carries the table.
            mountDispatch <- rui.mountDispatch match
                case Present(d) => Kyo.lift(d)
                case Absent     => MountDispatch.init
            _ <- subscribeScoped(rui, exchange, signalChangeTime, rootMounts, mountDispatch)
            // Keyboard activation is synthesized at the BOTTOM, on the tree's own handler, because the
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
            () => expiryWorkers.get
        )

    /** Turns a keydown's recorded activation into the click it stands for.
      *
      * A browser answers Enter or Space on a button by firing a click at it, and Enter in a field by
      * firing one at the form's default button; the form submit is that click's activation behaviour,
      * never the key's. This is where kyo does the same: the keydown bubbles first, recording at most
      * one path in [[activationTarget]], and the click is dispatched at that path afterwards — through
      * the whole chain, so ancestors see it exactly as they see a mouse click.
      *
      * Dispatching it AFTER the keydown has finished is what keeps the order the platform has, with the
      * element's own `onKeyDown` running before its activation. Only a keydown records, so the click
      * this dispatches cannot record another: there is no recursion here.
      */
    private def activatingHandle(handle: Handler)(using Frame): Handler =
        (path, event) =>
            event match
                case kd: UIEvent.KeyDown =>
                    for
                        recorded <- AtomicRef.init(Maybe.empty[Seq[String]])
                        keep     <- activationTarget.let(Present(recorded))(handle(path, kd))
                        target   <- recorded.get
                        _ <- target.fold(Kyo.lift(())) { clickPath =>
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
                // A measure reply is the client's answer to a `requestMeasure`, not an element event: the session
                // completes the pending reply on UI.Commands before dispatch, so one never reaches the handler
                // tree. Bubbling is the right answer for an event no element declared.
                case _: DragProtocol.ValidatedEvent.Measure | _: DragProtocol.ValidatedEvent.MeasureById => true
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
                            Clock.now.map { now =>
                                sessions.set(current + (data.sessionId -> DragSession(
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
                            _ <- DragCommands.current.let(Present(DragCommands.Dispatch(
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
            case _: DragProtocol.ValidatedEvent.Measure | _: DragProtocol.ValidatedEvent.MeasureById => true
            case _: DragProtocol.ValidatedEvent.Click | _: DragProtocol.ValidatedEvent.ClickSelf |
                _: DragProtocol.ValidatedEvent.ContextMenu |
                _: DragProtocol.ValidatedEvent.Input | _: DragProtocol.ValidatedEvent.Change |
                _: DragProtocol.ValidatedEvent.ChangeChecked | _: DragProtocol.ValidatedEvent.ChangeNumeric |
                _: DragProtocol.ValidatedEvent.Submit | _: DragProtocol.ValidatedEvent.KeyDown |
                _: DragProtocol.ValidatedEvent.KeyUp | _: DragProtocol.ValidatedEvent.Focus |
                _: DragProtocol.ValidatedEvent.Blur | _: DragProtocol.ValidatedEvent.Scroll |
                _: DragProtocol.ValidatedEvent.Hover | _: DragProtocol.ValidatedEvent.Unhover |
                _: DragProtocol.ValidatedEvent.PointerDown | _: DragProtocol.ValidatedEvent.PointerMove |
                _: DragProtocol.ValidatedEvent.PointerUp | _: DragProtocol.ValidatedEvent.ContextMenu |
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
            sessions.get.map { current =>
                current.valuesIterator.map(_.expiresAt).minOption match
                    case None =>
                        Abort.runPartial[Closed](expiryWake.take).map {
                            case Result.Success(_) => Loop.continue
                            case Result.Failure(_) => Loop.done
                        }
                    case Some(expiresAt) =>
                        Clock.now.map { now =>
                            val wait                = expiresAt - now
                            val sleep: Unit < Async = if wait > Duration.Zero then Clock.sleep(wait).map(_.get) else ()
                            Abort.runPartial[Closed](Async.race(sleep, expiryWake.take.unit)).map {
                                case Result.Success(_) =>
                                    Clock.now.map(expireNow => mutex.run(expireSessions(expireNow, sessions)))
                                        .andThen(Loop.continue)
                                case Result.Failure(_) => Loop.done
                            }
                        }
            }
        }

    private def expireSessions(now: Instant, sessions: AtomicRef[Map[String, DragSession]])(using Frame): Unit < Async =
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
                        _ <- DragCommands.current.let(Present(DragCommands.Dispatch(
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
    // has no signal: it subscribes its children in the enclosing scope.
    //
    // `mounts` is the MountRegistry of the nearest enclosing region (or the subscribe root): keyed Mounted instances
    // escape the per-value cascade by living on fibers the registry owns, so keyed continuity spans re-renders of the
    // immediately enclosing region and ends with that region.
    private def subscribeScoped(
        rui: ReactiveUI,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        mounts: MountRegistry,
        mountDispatch: MountDispatch
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
            rui.frame,
            rui.renderedAttrValues,
            rui.renderedBoolAttrValues,
            rui.renderedClassValues
        ).andThen {
            rui.mountedSpec match
                case Present(spec) =>
                    subscribeMounted(rui, spec, exchange, signalChangeTime, mounts, mountDispatch)
                case Absent =>
                    if rui.isConst then
                        Kyo.foreachDiscard(rui.children)(
                            subscribeScoped(_, exchange, signalChangeTime, mounts, mountDispatch)
                        )
                    else
                        rui.foreachSpec match
                            case Present(spec) if spec.reusable =>
                                subscribeForeachRegion(
                                    rui,
                                    spec,
                                    exchange,
                                    signalChangeTime,
                                    mountDispatch,
                                    rui.children
                                )
                            case _ =>
                                bindTextRegion(rui, exchange).map { bound =>
                                    if bound then Kyo.unit
                                    else
                                        subscribeRegion(
                                            rui,
                                            rui.signal,
                                            exchange,
                                            signalChangeTime,
                                            Absent,
                                            mountDispatch,
                                            initialKids = rui.children,
                                            renderedSnapshot = rui.renderedValue
                                        )
                                }
        }
    end subscribeScoped

    /** Bind a lone-text region straight to the backend's text write, skipping the region fiber; `false` means the
      * caller must subscribe it as a normal region.
      *
      * A region lifted from a `Signal[String]` paints one text node and nothing else. The region path still runs
      * the full apparatus for it — a fiber, a per-value Scope with its finalizer queue, a re-walk and an HTML
      * render — to arrive at a single `Text.data` write. Measured on `03_update10th`, that write is under 2% of
      * the cost; the rest is the machinery around it. Binding is the same trade the attribute channels already
      * make (see bindChannel), and for the same reason: the handler is one DOM write that cannot suspend.
      *
      * Three conditions, all necessary:
      *
      *   - the region carries its string signal, so its content is statically one text node;
      *   - the backend offers a synchronous text write (the server transport does not, and stays on the region
      *     path — its repaint IS the equivalent patch);
      *   - the walk found no reactive children under it. A `Text` value cannot produce any, so this is a
      *     guard against a future lift that fills `text` on a region whose content is not only text.
      *
      * The baseline is the render-time string, so an unchanged first emission is dropped before the write, and a
      * change that landed between render and subscribe still fires (`Signal.Unsafe.subscribe` delivers the
      * current value on registration). Release is registered on the current Scope — the same scope that would
      * have owned the fiber — because the next-promise is masked and nothing interrupts it.
      */
    private def bindTextRegion(rui: ReactiveUI, exchange: UIExchange)(using Frame): Boolean < (Sync & Scope) =
        (rui.textSignal, exchange.textPatcherNow) match
            case (Present(sig), Present(patch)) if rui.children.isEmpty =>
                val rendered = rui.renderedValue match
                    case Present(t: Text) => Present(t.value)
                    case _                => Absent
                // The sink is read HERE and not inside the callback: the write fires inside the writer's own
                // `set`, where no effect context exists to read a `Local` from. With none installed the
                // original patcher is used unwrapped, so the off path is byte-for-byte what it was.
                Devtools.currentSink.map { devSink =>
                    val write: String => Unit = devSink match
                        case Absent => v => patch(rui.path, v)
                        case Present(report) =>
                            v =>
                                patch(rui.path, v)
                                report(cheapEvent(rui.path, ReactiveRegion.htmlIdOf(rui.region), rui.frame, UI.RenderKind.Text))
                    Sync.Unsafe.defer {
                        sig.unsafeObserveProjected[String](identity, rendered, write) match
                            case Absent           => Kyo.lift(false)
                            case Present(release) => Scope.ensure(Sync.defer(release())).andThen(true)
                    }
                }
            case _ => false
    end bindTextRegion

    /** The report for a write that re-renders nothing: one attribute, one class, or one text node.
      *
      * No duration and no bytes, deliberately. The whole write is a single DOM assignment; putting a number
      * on it would invite a reader to weigh it against a repaint's, and the point of separating these kinds
      * is that they do not belong on the same axis.
      */
    private def cheapEvent(
        path: Seq[String],
        regionId: Maybe[String],
        frame: Frame,
        kind: UI.RenderKind
    ): UI.RenderEvent =
        UI.RenderEvent(path, regionId, frame, kind, UI.RenderCause.Signal, wasted = false, durationNanos = 0L, bytes = 0)

    /** Fork the scoped in-place-patch observers for one node's reactive attr/bool-attr/class channels at
      * `path`. Shared by subscribeScoped (walked nodes) and by a region's renderValue (the painted ROOT
      * element, which is absent from the walk's kids), so the two sites cannot drift apart.
      */
    private def forkChannelObservers(
        path: Seq[String],
        attrs: Map[String, Signal[String]],
        boolAttrs: Map[String, Signal[Boolean]],
        classes: Map[String, Signal[Boolean]],
        exchange: UIExchange,
        // Source position of the node these channels belong to, for the devtools report. A channel write
        // addresses the ELEMENT (by `data-kyo-path`), never a region, which is why no region id travels with it.
        nodeFrame: Frame,
        renderedAttrs: Map[String, String] = Map.empty,
        renderedBools: Map[String, Boolean] = Map.empty,
        renderedClasses: Map[String, Boolean] = Map.empty
    )(using Frame): Unit < (Async & Scope) =
        // The first emission is skipped when it still equals the render-time snapshot: normalize
        // happens-before the HTML render on every paint path, so the DOM already shows that value and the
        // initial patch would only repaint freshly painted state. A change landing between render and
        // subscribe differs from the snapshot and still fires.
        def observeSkippingRendered[A](sig: Signal[A], rendered: Maybe[A])(f: A => Unit < Async)(using Frame): Unit < Async =
            var first = true
            sig.observe { v =>
                val skip = first && rendered.exists(_.equals(v))
                first = false
                if skip then (): Unit < Async else f(v)
            }
        end observeSkippingRendered
        // A channel's handler is one attribute write, so where both ends offer the raw path it binds as a
        // callback on the signal's next-promise instead of a fiber: delivered inside the writer's own `set`,
        // with the image comparison happening BEFORE the scheduler hop instead of after it. Falls back to the
        // fiber whenever either end says no (server transport, or a signal not rooted in a SignalRef), so
        // behaviour is unchanged wherever the fast path is unavailable.
        def bindOrFork[A](
            name: String,
            sig: Signal[A],
            rendered: Maybe[A],
            patcher: Maybe[(Seq[String], String, A) => Unit],
            slow: A => Unit < Async
        )(using CanEqual[A, A], Frame): Unit < (Async & Scope) =
            bindChannel(path, name, sig, rendered, patcher).map { bound =>
                if bound then Kyo.unit
                else Fiber.init(observeSkippingRendered(sig, rendered)(slow)).unit
            }
        // Read once, at bind time, for the same reason bindTextRegion does: the fast path's write runs inside
        // the writer's `set`, with no effect context to read a `Local` from. With no sink installed both the
        // patcher and the fallback are passed through untouched.
        Devtools.currentSink.map { devSink =>
            def instrument[A](patcher: Maybe[(Seq[String], String, A) => Unit]): Maybe[(Seq[String], String, A) => Unit] =
                devSink match
                    case Absent => patcher
                    case Present(report) =>
                        patcher.map(patch =>
                            (target, name, value) =>
                                patch(target, name, value)
                                report(cheapEvent(target, Absent, nodeFrame, UI.RenderKind.Channel(name)))
                        )

            def instrumented[A](name: String, slow: A => Unit < Async): A => Unit < Async =
                devSink match
                    case Absent => slow
                    case Present(report) =>
                        v =>
                            slow(v).andThen(
                                Sync.defer(report(cheapEvent(path, Absent, nodeFrame, UI.RenderKind.Channel(name))))
                            )

            Kyo.foreachDiscard(attrs.toSeq) { case (name, sig) =>
                bindOrFork(
                    name,
                    sig,
                    Maybe.fromOption(renderedAttrs.get(name)),
                    instrument(exchange.attrPatcherNow),
                    instrumented(name, v => exchange.onAttrPatch(path, name, v))
                )
            }.andThen(Kyo.foreachDiscard(boolAttrs.toSeq) { case (name, sig) =>
                bindOrFork(
                    name,
                    sig,
                    Maybe.fromOption(renderedBools.get(name)),
                    instrument(exchange.boolAttrPatcherNow),
                    instrumented(name, v => exchange.onBoolAttrPatch(path, name, v))
                )
            }).andThen(Kyo.foreachDiscard(classes.toSeq) { case (name, sig) =>
                bindOrFork(
                    name,
                    sig,
                    Maybe.fromOption(renderedClasses.get(name)),
                    instrument(exchange.classPatcherNow),
                    instrumented(name, v => exchange.onClassPatch(path, name, v))
                )
            })
        }
    end forkChannelObservers

    /** Try to bind one channel without a fiber; `false` means the caller must fork. The release is registered
      * on the current Scope, which is the same scope that would have owned the fiber — the registration sits
      * on a masked promise and would otherwise outlive its subscriber.
      */
    private def bindChannel[A](
        path: Seq[String],
        name: String,
        sig: Signal[A],
        rendered: Maybe[A],
        patcher: Maybe[(Seq[String], String, A) => Unit]
    )(using CanEqual[A, A], Frame): Boolean < (Sync & Scope) =
        patcher match
            case Absent => false
            case Present(patch) =>
                Sync.Unsafe.defer {
                    sig.unsafeObserveProjected[A](identity, rendered, v => patch(path, name, v)) match
                        case Absent           => Kyo.lift(false)
                        case Present(release) => Scope.ensure(Sync.defer(release())).andThen(true)
                }
    end bindChannel

    /** Start one region fiber observing `signal`, with a per-value Scope per emission (see subscribeScoped's
      * contract note). `presetMounts` supplies the region's MountRegistry when the caller owns it already (the
      * content region of a KEYED mounted instance reuses the instance's registry so nested keyed mounts survive
      * re-subscription); `Absent` creates a fresh registry owned by the current scope: the same scope that owns
      * the region fiber, so registry and region die together.
      *
      * renderValue ordering is walk -> evict -> paint -> subscribe: mount keys of the new value are known after the
      * walk, so instances whose key vanished (or was swapped) are closed AND awaited BEFORE the new HTML paints
      * and before any successor effect runs (the region-level closed-and-awaited guarantee, extended to the
      * keyed instances that deliberately escape the per-value cascade).
      */
    private def subscribeRegion(
        rui: ReactiveUI,
        signal: Signal[UI],
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        presetMounts: Maybe[MountRegistry],
        mountDispatch: MountDispatch,
        // The already-walked children and normalize-time value snapshot of this region's node: when the first
        // observed value still equals the snapshot, the repaint is skipped (the enclosing render already put
        // it in the DOM) and only the mount claims and child subscriptions run. Element roots carry handler
        // lambdas and never compare equal, keeping their behavior unchanged.
        initialKids: Seq[ReactiveUI] = Seq.empty,
        renderedSnapshot: Maybe[UI] = Absent
    )(using Frame): Unit < (Async & Scope) =
        for
            regionMounts <- presetMounts match
                case Present(r) => Kyo.lift(r)
                case Absent     => MountRegistry.init.map(r => Scope.ensure(r.evictAll).andThen(r))
            _ <-
                // The tree this region last rendered, so the exchange can send only what moved instead of the
                // whole region.
                // Unsafe: region-scoped, single-writer state created before the observe loop is forked, where no
                // effect context exists to supply AllowUnsafe; only this region's own loop reads or writes it.
                val rendered = AtomicRef.Unsafe.init(Maybe.empty[UI])(using AllowUnsafe.embrace.danger)
                // Per-value setup, run inside each value's fresh Scope: render the region and fork its children into
                // that scope, so the next value (or an interrupt) tears them down by cascade.
                def renderValue(current: UI, cause: UI.RenderCause): Unit < (Async & Scope) =
                    for
                        now <- Clock.now
                        _   <- signalChangeTime.set(now)
                        // `Absent`, and nothing allocated or clocked, unless a devtools sink is installed. The
                        // probe is what the backend writes the paint's cost into; see Devtools.Probe.
                        probe    <- Devtools.newProbe
                        started  <- if probe.isEmpty then Kyo.lift(Duration.Zero) else Clock.nowMonotonic
                        previous <- Sync.Unsafe.defer(rendered.getAndSet(Present(current)))
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
                            mountDispatch,
                            discoverRootBound = rui.discoverContentRootBound
                        )
                        _ <- regionMounts.evictExcept(collectMountKeys(newKids))
                        _ <- Devtools.probe.let(probe)(
                            exchange.onChange(rui.region, rui.path, rui.contentContext, rui.parentContext, previous, current)
                        )
                        _ <- reportRepaint(rui, cause, probe, started)
                        // walkStatic only forks observers for reactive-attr CHILD elements; the region's painted
                        // ROOT element carries its own reactive channels at `path` and is absent from newKids, so
                        // start its observers here, scoped to this per-value fiber like the child subscriptions.
                        _ <- current match
                            case el: Element
                                if el.attrs.reactiveAttrs.nonEmpty || el.attrs.reactiveBoolAttrs.nonEmpty || el.attrs.reactiveClasses.nonEmpty =>
                                forkChannelObservers(
                                    rui.path,
                                    el.attrs.reactiveAttrs,
                                    el.attrs.reactiveBoolAttrs,
                                    el.attrs.reactiveClasses,
                                    exchange,
                                    el.frame
                                )
                            case _ => Kyo.unit
                        _ <- Kyo.foreachDiscard(newKids)(
                            subscribeScoped(_, exchange, signalChangeTime, regionMounts, mountDispatch)
                        )
                    yield ()

                // What the region's fiber listens to. With a source present that is VALUE space: `observe`
                // drops an emission whose value equals the last delivered one, so a record that ticks once a
                // second stops reaching the regions drawing the slices of it that did not move. Without a
                // source (a lifted `Signal[UI]`, where no value exists before the UI) it stays the projected
                // signal and the only available comparison is the UI itself, exactly as before.
                def observed(paint: UI => Unit < (Async & Scope)): Unit < Async =
                    rui.sourceSpec match
                        case Present(src) =>
                            src.applyTyped([T] => (values: Signal[T], project: T => UI) => values.observe(v => paint(project(v))))
                        case Absent => signal.observe(paint)

                startOwnedFiber {
                    Abort.run[Throwable] {
                        var first = true
                        observed { current =>
                            val isFirst = first
                            first = false
                            if isFirst && renderedSnapshot.exists(_.equals(current)) then
                                // The enclosing render already painted exactly this value: skip the redundant
                                // repaint and re-walk, but still claim mounts and subscribe the already-walked
                                // children, exactly as renderValue would have. The value is still recorded as
                                // this region's last render, so the NEXT emission diffs against it rather than
                                // re-sending the whole region.
                                //
                                // Devtools still hears about it, and this is the branch that carries the
                                // "rebuilt by my parent" half of the story: the region's content WAS produced,
                                // by the enclosing paint's walk. Zero duration and zero bytes are not a
                                // rounding-off — the cost sits in the enclosing region's own Repaint event, and
                                // charging it twice would make every parent look cheap and every child
                                // expensive. Skipping the event entirely would be worse: a repaint that
                                // rebuilds two hundred child regions would report as one render.
                                Sync.Unsafe.defer(rendered.set(Present(current))).andThen(
                                    reportRepaint(rui, UI.RenderCause.Created, Absent, Duration.Zero).andThen(
                                        regionMounts.evictExcept(collectMountKeys(initialKids)).andThen(
                                            Kyo.foreachDiscard(initialKids)(
                                                subscribeScoped(_, exchange, signalChangeTime, regionMounts, mountDispatch)
                                            )
                                        )
                                    )
                                )
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
        yield ()
    end subscribeRegion

    /** Report one region repaint to the devtools sink; a no-op with none installed.
      *
      * `probe` is what the backend wrote the paint's cost into: `Absent` both when devtools is off and on the
      * branch that painted nothing at all, which is why the two collapse into the same zero-cost report.
      */
    private def reportRepaint(
        rui: ReactiveUI,
        cause: UI.RenderCause,
        probe: Maybe[Devtools.Probe],
        started: Duration
    )(using Frame): Unit < Sync =
        def event(wasted: Boolean, durationNanos: Long, bytes: Int) =
            UI.RenderEvent(
                rui.path,
                ReactiveRegion.htmlIdOf(rui.region),
                rui.frame,
                UI.RenderKind.Repaint,
                cause,
                wasted,
                durationNanos,
                bytes
            )
        probe match
            case Absent => Devtools.emit(event(wasted = false, durationNanos = 0L, bytes = 0))
            case Present(p) =>
                Clock.nowMonotonic.map(ended => Devtools.emit(event(p.wasted, (ended - started).toNanos, p.bytes)))
        end match
    end reportRepaint

    /** Report one keyed-list emission.
      *
      * `wasted` is stricter here than "no row was repainted": a pure reorder repaints nothing and still moves
      * the DOM, and calling that wasted would send a reader looking for a bug that is not there. `reordered`
      * is by name because answering it costs a pass over the rows, and only a paint that repainted nothing
      * ever asks.
      */
    private def reportListPatch(
        rui: ReactiveUI,
        cause: UI.RenderCause,
        probe: Maybe[Devtools.Probe],
        started: Duration,
        changedRows: Int,
        totalRows: Int,
        reordered: => Boolean
    )(using Frame): Unit < Sync =
        def event(wasted: Boolean, durationNanos: Long, bytes: Int) =
            UI.RenderEvent(
                rui.path,
                ReactiveRegion.htmlIdOf(rui.region),
                rui.frame,
                UI.RenderKind.ListPatch(changedRows, totalRows),
                cause,
                wasted,
                durationNanos,
                bytes
            )
        probe match
            case Absent => Devtools.emit(event(wasted = false, durationNanos = 0L, bytes = 0))
            case Present(p) =>
                Clock.nowMonotonic.map { ended =>
                    Devtools.emit(event(
                        p.wasted || (changedRows == 0 && !reordered),
                        (ended - started).toNanos,
                        p.bytes
                    ))
                }
        end match
    end reportListPatch

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
        val finalizer: Scope.Finalizer.Awaitable
    )

    /** Per-region ownership of reusable Foreach rows, in list order. Mutation happens only from the owning
      * region's sequential observe loop, so plain get/set on the ref suffices.
      */
    final private[kyo] class RowRegistry(rows: AtomicRef[Vector[RowInstance]]):
        def snapshot(using Frame): Vector[RowInstance] < Sync = rows.get

        def replaceAll(ordered: Seq[RowInstance])(using Frame): Unit < Sync =
            rows.set(Vector.from(ordered))

        /** Close-and-await every row whose key is not in `keep`: run BEFORE the new value paints, so a
          * vanished or changed row's observers are fully released while the old content is still on screen
          * (break-before-make, the region-level closed-and-awaited guarantee extended to rows).
          */
        def evictExcept(keep: Set[String])(using Frame): Unit < Async =
            for
                old <- rows.getAndUpdate(_.filter(r => keep.contains(r.key)))
                _ <- Kyo.foreachDiscard(old.filterNot(r => keep.contains(r.key)))(r =>
                    r.finalizer.close(Absent).andThen(r.finalizer.await)
                )
            yield ()

        def evictAll(using Frame): Unit < Async = evictExcept(Set.empty)
    end RowRegistry

    private[kyo] object RowRegistry:
        def init(using Frame): RowRegistry < Sync =
            AtomicRef.init(Vector.empty[RowInstance]).map(new RowRegistry(_))

    /** Subscribe a reusable keyed Foreach region: rows are owned by a RowRegistry instead of the per-value
      * Scope cascade. On each list emission only added rows and rows whose item VALUE changed are re-rendered,
      * re-walked, and re-subscribed; a row whose key and item persist keeps its live observers (its path,
      * `regionPath :+ key`, is stable across reorders, so no retargeting is needed) and its cached row UI is
      * reused for the paint. Removed and changed rows are closed AND awaited before the new value paints
      * (break-before-make, matching subscribeRegion's ordering contract). Event dispatch resolves through the
      * same registry (shared via the ForeachSpec): an event on a live row routes to the row's cached handler
      * instead of re-rendering and re-walking the whole list, falling back to the full walk only when the
      * registry cannot resolve the target. Duplicate keys disable reuse for that emission (all rows rebuilt,
      * loud warning), keeping the painted output identical to the non-reusable path. Unkeyed and *Indexed
      * variants stay on subscribeRegion entirely.
      */
    private def subscribeForeachRegion(
        rui: ReactiveUI,
        spec: ForeachSpec,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        mountDispatch: MountDispatch,
        initialKids: Seq[ReactiveUI]
    )(using Frame): Unit < (Async & Scope) =
        val path = rui.path
        val svg = rui.region match
            case _: ReactiveRegion.HtmlRange  => false
            case _: ReactiveRegion.SvgElement => true
        // The tree this region last rendered, so the exchange can send only what moved (see subscribeRegion).
        // Unsafe: region-scoped, single-writer state created before the observe loop is started.
        val rendered = AtomicRef.Unsafe.init(Maybe.empty[UI])(using AllowUnsafe.embrace.danger)
        spec.node.applyTyped {
            [T] =>
                (itemSignal, keyFnMaybe, renderFn) =>
                    val keyFn = keyFnMaybe.get // reusable guarantees a key function
                    for
                        regionMounts <- MountRegistry.init.map(r => Scope.ensure(r.evictAll).andThen(r))
                        rows = spec.rows
                        _ <- Scope.ensure(rows.evictAll)
                        _ <- startOwnedFiber {
                            // A row owns a bare Finalizer, not a fiber: the registry closes and awaits it, and
                            // scoped children still die with it: `Fiber.init` is `Scope.acquireRelease(_.interrupt)`,
                            // so a nested region's or mount's interrupt is registered HERE. Subscribing inline
                            // rather than on a forked fiber makes a row that suspends (only one containing a
                            // Mounted node can) finish before the emission does; the paint is already out by then.
                            def rowRunner(kids: Seq[ReactiveUI]): Scope.Finalizer.Awaitable < Async =
                                Sync.Unsafe.defer {
                                    val fin = Scope.Finalizer.Awaitable.Unsafe.init(1)
                                    ContextEffect.handle[Scope.Finalizer, Scope, Unit, Async](Tag[Scope], fin, _ => fin) {
                                        Kyo.foreachDiscard(kids)(
                                            subscribeScoped(_, exchange, signalChangeTime, regionMounts, mountDispatch)
                                        )
                                    }.andThen(fin)
                                }

                            def distinctKeys(items: Chunk[T]): Boolean =
                                val ks = items.toSeq.map(keyFn)
                                ks.distinct.sizeIs == ks.size

                            // First emission over the already-painted value: skip render and walk entirely; adopt the
                            // walked initial kids as rows (grouped by their row path segment) and just subscribe them.
                            def seed(items: Chunk[T]): Unit < (Async & Scope) =
                                for
                                    _ <- regionMounts.evictExcept(collectMountKeys(initialKids))
                                    grouped = initialKids.groupBy(k => k.path.lift(path.length).getOrElse(""))
                                    built <- Kyo.foreach(Chunk.from(items.toSeq.zipWithIndex)) { (item, i) =>
                                        val key  = keyFn(item)
                                        val kids = grouped.getOrElse(key, Seq.empty)
                                        rowRunner(kids).map(fiber =>
                                            new RowInstance(key, item, renderFn(i, item), kids, Absent, fiber)
                                        )
                                    }
                                    // No paint here: the enclosing render already put exactly this fragment in the
                                    // DOM. It is still recorded as this region's last render, so the NEXT emission
                                    // diffs against it rather than re-sending every row.
                                    _ <- Sync.Unsafe.defer(
                                        rendered.set(Present(Fragment[UI](Chunk.from(built.toSeq.map(r =>
                                            KeyedChild[UI](r.key, r.rowUI)
                                        )))))
                                    )
                                    _ <- rows.replaceAll(built.toSeq)
                                    // No paint happened, so no cost is charged — but the rows DID come into
                                    // existence, in the enclosing region's paint. Same accounting as the region
                                    // skip branch in subscribeRegion.
                                    _ <- reportListPatch(
                                        rui,
                                        UI.RenderCause.Created,
                                        Absent,
                                        Duration.Zero,
                                        changedRows = 0,
                                        totalRows = built.size,
                                        reordered = false
                                    )
                                yield ()

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
                                    built <- Kyo.foreach(keyed) { (key, item, i) =>
                                        prevByKey.get(key).filter(_ => retainedKeys.contains(key)) match
                                            case Some(inst) => Kyo.lift((key, item, inst.rowUI, inst.kids, inst.handler, Present(inst)))
                                            case None =>
                                                val rowUI = renderFn(i, item)
                                                walkRow(
                                                    rowUI,
                                                    path :+ key,
                                                    svg,
                                                    rui.contentContext.child(key),
                                                    nestedParentContext(rui.parentContext, rowUI),
                                                    mountDispatch
                                                ).map((kids, hdl) =>
                                                    (key, item, rowUI, kids, Present(hdl), Absent: Maybe[RowInstance])
                                                )
                                    }
                                    // Both key sets are built INSIDE the call, not bound as vals: evictExcept is
                                    // inline, so a region with no mounted node (the common case, and every row of
                                    // a plain keyed list) skips two full passes over every row plus their flattens
                                    // and Set builds, instead of computing them to hand a registry that would
                                    // discard them.
                                    _ <- regionMounts.evictExcept(
                                        collectMountKeys(built.toSeq.flatMap((_, _, _, kids, _, _) => kids)),
                                        collectMountKeys(built.toSeq.collect { case (_, _, _, kids, _, Present(_)) => kids }.flatten)
                                    )
                                    fragment =
                                        Fragment[UI](Chunk.from(built.toSeq.map((key, _, rowUI, _, _, _) => KeyedChild[UI](key, rowUI))))
                                    previous <- Sync.Unsafe.defer(rendered.getAndSet(Present(fragment)))
                                    // A structural command addresses rows BY KEY, so it can say nothing useful
                                    // about an emission whose keys are not unique — two rows would name one
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
                                    addressable = built.forall((_, _, rowUI, _, _, _) => HtmlRenderer.paintsAsKeyedRoot(rowUI))
                                    probe   <- Devtools.newProbe
                                    started <- if probe.isEmpty then Kyo.lift(Duration.Zero) else Clock.nowMonotonic
                                    _ <- Devtools.probe.let(probe) {
                                        if duplicates || !addressable then
                                            exchange.onChange(
                                                rui.region,
                                                path,
                                                rui.contentContext,
                                                rui.parentContext,
                                                previous,
                                                fragment
                                            )
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
                                    _ <- reportListPatch(
                                        rui,
                                        UI.RenderCause.Signal,
                                        probe,
                                        started,
                                        changedRows = built.count((_, _, _, _, _, retained) => retained.isEmpty),
                                        totalRows = built.size,
                                        reordered = !built.toSeq.corresponds(prev)((b, p) => b._1 == p.key)
                                    )
                                    finalRows <- Kyo.foreach(built) { (key, item, rowUI, kids, hdl, retained) =>
                                        retained match
                                            case Present(inst) => Kyo.lift(inst)
                                            case Absent =>
                                                rowRunner(kids).map(fiber => new RowInstance(key, item, rowUI, kids, hdl, fiber))
                                    }
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

    /** The keys of the Mounted nodes among a walk's reactive children: the claims of the upcoming subscribe
      * pass, computed up front so evictExcept can run before paint.
      */
    private def collectMountKeys(kids: Seq[ReactiveUI]): Set[Any] =
        kids.flatMap(_.mountedSpec.flatMap(_.node.key).toChunk).toSet

    /** Subscribe one Mounted node.
      *
      * KEYED: claim (or adopt) the live instance from the enclosing region's registry. The instance runner is an
      * UNSCOPED fiber (it survives the per-value cascade; the registry's owner scope ends it) holding the node
      * Scope open; then push one synchronous paint of the cell's current content (so an adopted instance's content
      * replaces the placeholder the parent's wholesale re-render just painted, within the same task, with no visible
      * placeholder blink), and finally subscribe the content region over the cell, reusing the instance's child
      * registry so keyed mounts NESTED in the content survive as long as this instance does.
      *
      * KEYLESS: the instance lifetime IS the current scope (the enclosing region's per-value scope, or the
      * session scope for a static position): the effect runs on a normally-scoped fiber and the next parent
      * emission tears it down by cascade (remount semantics). A thrash note counts remounts per path and logs a
      * dev hint when a keyless node keeps remounting inside a hot region.
      */
    private def subscribeMounted(
        rui: ReactiveUI,
        spec: MountedSpec,
        exchange: UIExchange,
        signalChangeTime: AtomicRef[Instant],
        mounts: MountRegistry,
        mountDispatch: MountDispatch
    )(using Frame): Unit < (Async & Scope) =
        spec.node.key match
            case Present(key) =>
                mounts.claim(key, rui.path, spec, () => mountDispatch.noteKeyedMiss(rui.path, key).unit).map {
                    case Present(inst) =>
                        for
                            _       <- mountDispatch.register(rui.path, inst.cell)
                            _       <- Scope.ensure(mountDispatch.unregister(rui.path, inst.cell))
                            current <- inst.cell.current
                            // Instrumented like any other paint: every `onChange` the engine issues is
                            // reported, or the wasted-render figure quietly excludes a whole class of paint
                            // (every adopted keyed mount) and a page full of them reads as cleaner than it is.
                            probe   <- Devtools.newProbe
                            started <- if probe.isEmpty then Kyo.lift(Duration.Zero) else Clock.nowMonotonic
                            _ <- Devtools.probe.let(probe)(
                                exchange.onChange(
                                    rui.region,
                                    rui.path,
                                    rui.contentContext,
                                    rui.parentContext,
                                    Absent,
                                    current
                                )
                            )
                            _ <- reportRepaint(rui, UI.RenderCause.Created, probe, started)
                            _ <- subscribeRegion(
                                rui,
                                inst.cell,
                                exchange,
                                signalChangeTime,
                                Present(inst.childMounts),
                                mountDispatch
                            )
                        yield ()
                    case Absent =>
                        // Duplicate key this render pass (claim already warned): paint a loud inline error instead of
                        // silently sharing the first slot's instance. No cell or dispatch entry: a dead slot until the
                        // caller gives distinct keys.
                        given Frame = spec.node.frame
                        exchange
                            .onChange(
                                rui.region,
                                rui.path,
                                rui.contentContext,
                                rui.parentContext,
                                Absent,
                                UI.span(UI.Ast.Text(s"kyo-ui: duplicate mounted key '$key'")).cssClass("kyo-mount-error")
                            )
                            .unit
                }
            case Absent =>
                for
                    _           <- mounts.noteKeylessRemount(rui.path)
                    seed        <- mounts.keepLatestSeed(rui.path, spec)
                    cell        <- Signal.initRef[UI](seed)
                    _           <- mountDispatch.register(rui.path, cell)
                    _           <- Scope.ensure(mountDispatch.unregister(rui.path, cell))
                    supervision <- MountSupervision.init
                    _ <- Fiber.init(runMountEffect(
                        spec,
                        cell,
                        ui => mounts.recordContent(rui.path, ui),
                        supervision
                    ))
                    _ <- subscribeRegion(rui, cell, exchange, signalChangeTime, Absent, mountDispatch)
                yield ()

    /** A supervised background fiber of a mounted node failed with a non-Throwable `Abort` value; the node's
      * error rendering needs a Throwable, so the value is carried in this wrapper.
      */
    final private[kyo] case class MountFiberFailure(value: Any)(using Frame)
        extends KyoException(s"Mounted background fiber failed: $value")

    /** The node-scope supervision seam of one mount instance: collects the FIRST non-interrupt failure of any
      * supervised background fiber (see [[kyo.UI.fork]]) and lets the node runner park on it AFTER the effect
      * outcome, flipping an already-published node into its error rendering retroactively.
      *
      * Publish then flip is serialized on the node's own fiber (the park runs after [[runMountEffect]]'s
      * outcome handling), so a supervised failure can never be overwritten by a late `cell.set(ui)` of the
      * success path. The `routed` once-guard is shared between the effect-failure path and the park, so an
      * effect failure racing a supervised failure routes exactly once.
      */
    final private[kyo] class MountSupervision(
        firstFailure: Fiber.Promise.Unsafe[Throwable, Any],
        routed: AtomicRef[Boolean]
    ):
        /** Fiber-completion callback registered by [[kyo.UI.fork]]: maps a non-interrupt terminal error to a
          * Throwable and completes the once-only failure promise; later failures are dropped by the promise.
          * Runs on the completing fiber's thread, so it must stay allocation-light and must not block.
          */
        def report[E, A](result: Result[E, A])(using Frame): Unit < Sync =
            result match
                case Result.Panic(_: Interrupted)   => Kyo.lift(())
                case Result.Failure(_: Interrupted) => Kyo.lift(())
                case Result.Panic(exception)        => recordFailure(exception)
                case Result.Failure(failure) =>
                    failure match
                        case t: Throwable => recordFailure(t)
                        case other        => recordFailure(MountFiberFailure(other))
                case _ => Kyo.lift(())

        private def recordFailure(t: Throwable)(using Frame): Unit < Sync =
            Sync.Unsafe.defer(discard(firstFailure.complete(Result.succeed(t))))

        /** Route a failure through `route` at most once across effect-fail and supervised-fail paths. */
        def routeOnce(t: Throwable)(route: Throwable => Unit < Async)(using Frame): Unit < Async =
            routed.getAndSet(true).map(was => if was then () else route(t))

        /** Park the node fiber until a supervised failure arrives, then log + route it. Never returns: the
          * park (interrupted on teardown) is what holds the node Scope open for the instance's lifetime.
          */
        def park(spec: MountedSpec, route: Throwable => Unit < Async)(using Frame): Nothing < Async =
            firstFailure.safe.get.map { t =>
                Log.error(s"Mounted background fiber failed (frame=${spec.node.frame.position.show})", t)
                    .andThen(routeOnce(t)(route))
            }.andThen(Async.never)
    end MountSupervision

    private[kyo] object MountSupervision:
        def init(using Frame): MountSupervision < Sync =
            AtomicRef.init(false).map { routed =>
                Sync.Unsafe.defer {
                    new MountSupervision(Fiber.Promise.Unsafe.init[Throwable, Any](), routed)
                }
            }

        /** The mount instance a computation runs under, read by [[kyo.UI.fork]]. Inheritable, so a `UI.fork`
          * inside an already forked fiber still reports to the node that started the chain. Installed around
          * the USER effect only: engine fibers run outside it and are never supervised.
          */
        private[kyo] val local: Local[Maybe[MountSupervision]] = Local.init(Maybe.empty[MountSupervision])

        private[kyo] def let[A, S](sup: MountSupervision)(v: A < S)(using Frame): A < S =
            local.let(Present(sup))(v)
    end MountSupervision

    /** Run a Mounted node's effect, publish the outcome into its content cell, then PARK on `supervision` for
      * the instance's lifetime (this method never returns; the park is interrupted by the node's teardown).
      * Failure routing (shared by the effect outcome and a later supervised background-fiber failure, at most
      * once per instance): a node-local `.onError` wins, otherwise the node's default error rendering applies.
      * The enclosing region stays alive either way (a flip keeps the node Scope open, like a pending-phase
      * failure always has); failures are additionally logged out-of-band.
      */
    private def runMountEffect(
        spec: MountedSpec,
        cell: SignalRef[UI],
        record: UI => Unit < Sync,
        supervision: MountSupervision
    )(using Frame): Nothing < (Async & Scope) =
        def failed(source: UI.MountErrorSource)(t: Throwable): Unit < Async =
            cell.set(spec.errorUI(spec.mountError(t, source))).unit
        Abort.run[Throwable](MountSupervision.let(supervision)(spec.node.effect)).map {
            case Result.Success(ui) => cell.set(ui).andThen(record(ui))
            case Result.Failure(t) =>
                Log.error(s"Mounted effect failed (frame=${spec.node.frame.position.show})", t)
                    .andThen(supervision.routeOnce(t)(failed(UI.MountErrorSource.Effect)))
            case Result.Panic(t) =>
                if t.isInstanceOf[Interrupted] then ()
                else
                    Log.error(s"Mounted effect panicked (frame=${spec.node.frame.position.show})", t)
                        .andThen(supervision.routeOnce(t)(failed(UI.MountErrorSource.Panic)))
        }.map(_ => supervision.park(spec, failed(UI.MountErrorSource.Fork)))
    end runMountEffect

    /** A live keyed mount: the content cell, the unscoped runner fiber that owns the node Scope, and the child
      * registry that carries keyed mounts nested in this instance's content across re-subscriptions.
      */
    final private[kyo] class MountInstance(
        val key: Any,
        val cell: SignalRef[UI],
        val fiber: Fiber[Nothing, Any],
        val childMounts: MountRegistry
    )

    /** Per-region ownership of keyed mount instances (plus keep-latest seeds and the keyless thrash counter).
      * All mutation happens from the owning region's sequential observe loop (or once, at static subscribe), so
      * the AtomicRefs guard memory visibility across fiber steps, not concurrent writers.
      */
    final private[kyo] class MountRegistry(
        instances: AtomicRef[Dict[Any, MountInstance]],
        lastContent: AtomicRef[Dict[Seq[String], UI]],
        keylessRemounts: AtomicRef[Dict[Seq[String], Int]],
        claimedThisWalk: AtomicRef[Set[Any]],
        lastKeyByPath: AtomicRef[Dict[Seq[String], (Any, Int)]]
    ):

        /** Content last published at a path, the KeepLatest seed: a re-mounting node at this slot starts from the
          * previous content instead of the placeholder (frozen snapshot; its regions re-attach when the new effect
          * publishes). Placeholder policy, or a slot that never published, starts from the placeholder.
          */
        def keepLatestSeed(path: Seq[String], spec: MountedSpec)(using Frame): UI < Sync =
            spec.node.pendingPolicy match
                case UI.PendingPolicy.Placeholder => spec.placeholderUI
                case UI.PendingPolicy.KeepLatest =>
                    lastContent.get.map(_.getOrElse(path, spec.placeholderUI))

        def recordContent(path: Seq[String], ui: UI)(using Frame): Unit < Sync =
            lastContent.getAndUpdate(_.update(path, ui)).unit

        /** Reuse the live instance under `key`, or create one: seed the cell (keep-latest by slot), fork the
          * UNSCOPED runner (it must survive the caller's per-value scope; this registry's owner ends it), which
          * holds the node Scope open until interrupted and evicts nested keyed mounts on the way out. Two claims
          * of one key within a single walk (evictExcept resets the claim set per walk) are a caller bug: the
          * duplicate claim returns `Absent` (the caller paints a loud inline error card in that slot instead of
          * silently sharing the first slot's instance), and the warning names key and path (Laminar's
          * duplicate-split-key discipline, made visible in the page).
          */
        def claim(key: Any, path: Seq[String], spec: MountedSpec, onMiss: () => Unit < Sync)(using
            Frame
        ): Maybe[MountInstance] < (Async & Scope) =
            claimedThisWalk.getAndUpdate(_ + key).map(_.contains(key)).map {
                case true =>
                    Log.warn(
                        s"kyo-ui: duplicate mounted key '$key' claimed at ${path.mkString(".")} within one render pass: " +
                            "the duplicate slot renders an inline error card; use distinct keys (e.g. .keyed(Component -> id))"
                    ).andThen(Absent: Maybe[MountInstance])
                case false =>
                    for
                        _ <- noteKeyAt(path, key)
                        m <- instances.get
                        inst <- m.get(key) match
                            case Present(inst) => Kyo.lift(inst)
                            case Absent        =>
                                // A miss means a fresh instance: either the first mount, or continuity
                                // that did not happen. The caller counts them; only a repeat is a fault.
                                onMiss().andThen(for
                                    seed        <- keepLatestSeed(path, spec)
                                    cell        <- Signal.initRef[UI](seed)
                                    childMounts <- MountRegistry.init
                                    supervision <- MountSupervision.init
                                    fiber <- Fiber.initUnscoped {
                                        Scope.run {
                                            // runMountEffect never returns (it parks on `supervision`), which holds this
                                            // node Scope open until teardown.
                                            Scope.ensure(childMounts.evictAll)
                                                .andThen(runMountEffect(
                                                    spec,
                                                    cell,
                                                    ui => recordContent(path, ui),
                                                    supervision
                                                ))
                                        }
                                    }
                                    inst = new MountInstance(key, cell, fiber, childMounts)
                                    _ <- instances.getAndUpdate(_.update(key, inst))
                                yield inst)
                    yield Present(inst)
            }

        /** Dev hint against the unstable-key anti-pattern: a key that is REBUILT per render (a fresh tuple or
          * object identity, an inline-derived `Signal` inside the key) evicts and re-creates the instance on
          * every enclosing re-render: keyed continuity silently degrades to remount semantics. A key that
          * changes ONCE (`.keyed(EraPanel -> era)` on an era switch) is the intended re-creation and resets the
          * streak; only consecutive-change streaks of keys WITHOUT value identity are flagged, since a
          * location-driven region (a route outlet keyed by its route) re-renders exclusively on key changes and
          * would otherwise be flagged after three navigations.
          */
        private def noteKeyAt(path: Seq[String], key: Any)(using Frame): Unit < Sync =
            if MountRegistry.valueIdentityKey(key) then lastKeyByPath.getAndUpdate(_.remove(path)).unit
            else noteCompositeKeyAt(path, key)

        private def noteCompositeKeyAt(path: Seq[String], key: Any)(using Frame): Unit < Sync =
            lastKeyByPath.getAndUpdate { m =>
                m.get(path) match
                    case Present((prev, streak)) if !prev.equals(key) => m.update(path, (key, streak + 1))
                    case _                                            => m.update(path, (key, 0))
            }.map { prev =>
                prev.get(path) match
                    case Present((prevKey, streak)) if !prevKey.equals(key) =>
                        val n = streak + 1
                        if n == 3 || n == 10 || n == 100 then
                            Log.warn(
                                s"kyo-ui: mounted key at ${path.mkString(".")} changed in $n consecutive render passes " +
                                    s"(now '$key'): the key value is likely rebuilt per render (unstable tuple/object/" +
                                    "Signal identity), so the instance is evicted and re-created every pass. Derive keys " +
                                    "from stable data if the instance should live across re-renders."
                            )
                        else Kyo.unit
                        end if
                    case _ => Kyo.unit
            }.unit

        /** Close-and-await every instance whose key is not in `keep`: run BEFORE the new value paints, so a
          * vanished or swapped key's resources are fully released while the OLD content is still on screen
          * (break-before-make; under KeepLatest the successor then seeds from the recorded content). Also opens
          * the next claim generation for duplicate detection.
          */
        def evictExcept(keep: Set[Any], preClaimed: Set[Any] = Set.empty)(using Frame): Unit < Async =
            for
                // `preClaimed` marks mount keys owned by rows that are NOT re-walked this pass (retained
                // Foreach rows never re-claim): a fresh duplicate claim of such a key must warn instead of
                // silently adopting the retained row's live instance.
                _   <- claimedThisWalk.set(preClaimed)
                old <- instances.getAndUpdate(_.filter((k, _) => keep.contains(k)))
                evicted = old.foldLeft(Chunk.empty[MountInstance])((acc, k, inst) =>
                    if keep.contains(k) then acc else acc.append(inst)
                )
                _ <- Kyo.foreachDiscard(evicted)(teardown)
            yield ()

        def evictAll(using Frame): Unit < Async =
            evictExcept(Set.empty)

        private def teardown(inst: MountInstance)(using Frame): Unit < Async =
            inst.fiber.interrupt.andThen(inst.fiber.getResult.unit)

        /** Dev hint against the keyless-node-in-hot-region anti-pattern: a keyless Mounted re-runs its effect on
          * every enclosing region re-render; keys are the opt-in continuity mechanism.
          */
        def noteKeylessRemount(path: Seq[String])(using Frame): Unit < Sync =
            keylessRemounts.getAndUpdate(m => m.update(path, m.getOrElse(path, 0) + 1)).map { prev =>
                val n = prev.getOrElse(path, 0) + 1
                if n == 10 || n == 100 || n == 1000 then
                    Log.warn(
                        s"kyo-ui: keyless UI.mounted at ${path.mkString(".")} remounted $n times: its effect re-runs on " +
                            "every enclosing region re-render. Give it a stable identity with .keyed(...) if the instance " +
                            "should live across re-renders OF THAT REGION — a key does not reach further: the instance is " +
                            "claimed from the enclosing region's registry and ends with it, so a mount under a region that " +
                            "is itself re-subscribed is rebuilt keyed or not. Otherwise move it out of the hot region, or " +
                            "let the caller hold the state in a bound ref."
                    )
                else Kyo.unit
                end if
            }
    end MountRegistry

    private[kyo] object MountRegistry:

        /** Whether a key's equality is by VALUE, which is what makes a CHANGE meaningful.
          *
          * The streak hint looks for a key rebuilt per render — an object identity, a lambda, a
          * `Signal` captured inside a tuple — where "changed" really means "was never the same value
          * twice". A key with value identity that changes says the opposite: the caller meant a
          * different instance. A route outlet does that on every navigation, which is why the
          * exemption exists at all.
          *
          * Beyond primitives and `String`:
          *
          *   - `java.lang.Class`: one instance per class by construction, so keying a routed page by
          *     `ct.runtimeClass` is as stable as keying it by name
          *   - a `Product` whose elements all have value identity, which covers a case OBJECT (arity
          *     zero, `Route.Queue`) and a case class over stable values (`Route.Track(id)`)
          *
          * A tuple is a `Product` too, and that is not a hole in the hint. A tuple of stable parts
          * compares equal and never registers as a change in the first place; a tuple holding a
          * `Signal`, a function or a bare object still fails here, because none of those is a
          * `Product`, and stays flagged.
          */
        private[kyo] def valueIdentityKey(key: Any): Boolean = key match
            case _: String | _: Int | _: Long | _: Boolean | _: Double | _: Float | _: Short |
                _: Byte | _: Char =>
                true
            case _: Class[?] => true
            case p: Product  => p.productIterator.forall(valueIdentityKey)
            case _           => false

        def init(using Frame): MountRegistry < Sync =
            for
                instances       <- AtomicRef.init(Dict.empty[Any, MountInstance])
                lastContent     <- AtomicRef.init(Dict.empty[Seq[String], UI])
                keylessRemounts <- AtomicRef.init(Dict.empty[Seq[String], Int])
                claimedThisWalk <- AtomicRef.init(Set.empty[Any])
                lastKeyByPath   <- AtomicRef.init(Dict.empty[Seq[String], (Any, Int)])
            yield new MountRegistry(instances, lastContent, keylessRemounts, claimedThisWalk, lastKeyByPath)
    end MountRegistry

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
                    [T] =>
                        (signal, keyFn, renderFn) =>
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
            case m: Mounted =>
                // A static snapshot cannot run the mount effect; the node's static projection is its placeholder.
                m.placeholderUI.getOrElse(UI.empty(using m.frame))
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
                    val child = children(i) match
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
                    case Absent =>
                        Maybe.fromOption(segment.toIntOption) match
                            case Present(i) if i >= 0 && i < children.size =>
                                children(i) match
                                    case kc: KeyedChild[?] => Present(kc.child)
                                    case child             => Present(child)
                            case _ => Absent
                end match
            case _ => Absent

end ReactiveUI
