package kyo.internal

import kyo.*
import kyo.UI.Ast.*

/** How a [[kyo.UI.boundary]] relates to the tree around it, read by the renderer, normalization and the engine alike. */
private[kyo] object Boundaries:

    /** A mount's own `.placeholder` or `whilePending(Placeholder)` is a boundary around that mount alone. */
    def implicitOf(m: Mounted): Maybe[Bounded] =
        val ownWait = m.pendingPolicy match
            case UI.PendingPolicy.Placeholder => true
            case UI.PendingPolicy.KeepLatest  => false
        if m.placeholderUI.isEmpty && !ownWait then Absent
        else
            given Frame  = m.frame
            val settings = UI.Boundary(Present(m.placeholderUI.getOrElse(UI.empty)), Absent, Present(m.pendingPolicy))
            Present(Bounded(settings, m.copy(placeholderUI = Absent, pendingPolicy = UI.PendingPolicy.KeepLatest), Absent))
        end if
    end implicitOf

    /** Whether `ui`, as it renders now, holds a mount that a waiting boundary around it catches: one that no waiting boundary inside
      * `ui`, its own included, catches first.
      */
    def catches(ui: UI)(using Frame): Boolean < Sync =
        ui match
            case m: Mounted                   => implicitOf(m).isEmpty
            case b: Bounded                   => if b.settings.waits then false else catches(b.child)
            case r: Reactive[?]               => r.signal.current(using r.frame).map(catches)
            case fe: Foreach[?, ?] @unchecked =>
                fe.applyTyped {
                    [T] => (signal, _, render) =>
                        signal.current(using fe.frame).map(items => any(items.toSeq.zipWithIndex.map((item, i) => render(i, item))))
                }
            case e: Element       => any(e.children.toSeq)
            case Fragment(cs)     => any(cs.toSeq)
            case KeyedChild(_, c) => catches(c)
            case _                => false

    private def any(uis: Seq[UI])(using Frame): Boolean < Sync =
        uis.headOption match
            case None    => false
            case Some(h) => catches(h).map(found => if found then true else any(uis.tail))

    /** `ui` at `path` as the engine runs it now: every mount shows its instance's current content and every waiting boundary what it
      * shows, while regions and mounts keep their ranges, so a static rendering of the result is the live screen and later updates
      * still find where they land.
      */
    def resolveLive(ui: UI, path: Seq[String], dispatch: MountDispatch)(using Frame): UI < Sync =
        ui match
            case m: Mounted =>
                implicitOf(m) match
                    case Present(b) => resolveLive(b, path, dispatch)
                    case Absent     =>
                        dispatch.lookup(path).map {
                            case Absent        => m
                            case Present(cell) =>
                                cell.current.map {
                                    case ReactiveUI.MountState.Pending(Present(snapshot)) =>
                                        m.copy(placeholderUI = Present(snapshot))(using m.frame)
                                    case state =>
                                        resolveLive(state.content(MountedSpec(m, path, Nil)), path, dispatch)
                                            .map(c => m.copy(placeholderUI = Present(c))(using m.frame))
                                }
                        }
            case b: Bounded =>
                val shown: UI < Sync =
                    if !b.settings.waits then resolveLive(b.child, path, dispatch)
                    else
                        dispatch.boundary(path).map {
                            case Present(gate) => gate.displayed
                            case Absent        => b.display.getOrElse(b.settings.placeholderView.getOrElse(b.child))
                        }
                shown.map(d => b.copy(display = Present(d))(using b.frame))
            case r: Reactive[?] =>
                r.signal.current(using r.frame).map(c => resolveLive(c, path, dispatch)).map(c =>
                    Reactive[UI](Signal.initConst(c))(using r.frame)
                )
            case fe: Foreach[?, ?] @unchecked =>
                fe.applyTyped {
                    [T] => (signal, keyFn, render) =>
                        signal.current(using fe.frame).map { items =>
                            Kyo.foreach(items.toSeq.zipWithIndex) { (item, i) =>
                                val key = keyFn match
                                    case Present(f) => f(item)
                                    case Absent     => i.toString
                                resolveLive(render(i, item), path :+ key, dispatch)
                            }.map { rows =>
                                val resolved = Chunk.from(rows)
                                Foreach[T, UI](
                                    Signal.initConst(items)(using fe.frame, CanEqual.derived),
                                    keyFn,
                                    (i, _) => resolved(i)
                                )(using fe.frame): UI
                            }
                        }
                }
            case e: Element =>
                Kyo.foreach(e.children.toSeq.zipWithIndex)((c, i) => resolveLive(c, path :+ i.toString, dispatch))
                    .map(cs => ReactiveUI.rebuildElement(e, Chunk.from(cs)))
            case f: Fragment[?] =>
                Kyo.foreach(f.children.toSeq.zipWithIndex) { (c, i) =>
                    c match
                        case kc: KeyedChild[?] =>
                            resolveLive(kc.child, path :+ kc.key, dispatch).map(r => KeyedChild[UI](kc.key, r)(using kc.frame): UI)
                        case _ => resolveLive(c, path :+ i.toString, dispatch)
                }.map(cs => Fragment[UI](Chunk.from(cs))(using f.frame))
            case KeyedChild(k, c) => resolveLive(c, path, dispatch).map(r => KeyedChild[UI](k, r))
            case other            => other
end Boundaries

/** A pass that is due on a [[BoundaryHold]]: it ends once, by [[end]] when it has run or by [[abandon]] from a finalizer when it never
  * will.
  */
final private[kyo] class BoundaryPass(hold: BoundaryHold):
    import AllowUnsafe.embrace.danger
    private val ended = AtomicBoolean.Unsafe.init(false)

    def end(using Frame): Unit < Async =
        Sync.Unsafe.defer(ended.compareAndSet(false, true)).map(first => if first then hold.passEnded else Kyo.unit)

    def abandon(using AllowUnsafe): Unit =
        if ended.compareAndSet(false, true) then hold.passAbandoned()
end BoundaryPass

/** The range one pass of a region or a mount paints, with the content it shows. */
final private[kyo] case class BoundaryRange(
    region: ReactiveRegion,
    path: Seq[String],
    contentContext: ReactiveRegion.RegionIdentity,
    parentContext: ReactiveRegion.ParentContext,
    content: UI
)

/** An exchange inside a waiting boundary that counts what is still due below it: passes of regions and mounts that are due or running,
  * and caught mounts that have not loaded. Paints from below wait in it until it knows whether they may show.
  */
sealed abstract private[kyo] class BoundaryHold(mutex: Meter) extends UIExchange:
    import AllowUnsafe.embrace.danger

    final protected case class Paint(
        region: ReactiveRegion,
        path: Seq[String],
        contentContext: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        previous: Maybe[UI],
        ui: UI
    )

    // A hold's own first pass is due from the start, until what it was made for has been subscribed.
    protected val passes  = AtomicInt.Unsafe.init(1)
    protected val loading = AtomicInt.Unsafe.init(0)
    protected val closed  = AtomicBoolean.Unsafe.init(false)

    def root: BoundaryGate

    override private[kyo] def gate: Maybe[BoundaryHold] = Present(this)

    /** A pass that is due now and ends with [[BoundaryPass.end]]. */
    def pass(using Frame): BoundaryPass < Sync =
        Sync.Unsafe.defer {
            discard(passes.incrementAndGet())
            new BoundaryPass(this)
        }

    /** The hold for one pass of a region or a mount below this one, due from now: it holds that pass's paints, and those of the passes
      * below it, until the mounts it subscribed have loaded.
      */
    def group(using Frame): PassGroup < Sync = pass.map(up => new PassGroup(this, up, mutex))

    /** A caught mount that has not loaded; it settles with [[settle]] on the hold this returns. */
    def load(using Frame): BoundaryHold < Sync = Sync.Unsafe.defer { discard(loading.incrementAndGet()); this }

    /** A caught mount that loaded, failed, or went away before either; the pass it happened in decides what follows. */
    def settle(using AllowUnsafe): Unit = discard(loading.decrementAndGet())

    /** Ends the hold's own first pass. */
    def started(using Frame): Unit < Async = passEnded

    private[internal] def passEnded(using Frame): Unit < Async =
        Sync.Unsafe.defer(passes.decrementAndGet()).andThen(advance)

    private[internal] def passAbandoned(): Unit = discard(passes.decrementAndGet())

    protected def advance(using Frame): Unit < Async

    // Reentrant, since a hold passes paints and ends passes on the hold above it in the same fiber, under the mutex they share.
    protected def locked(v: => Unit < Async)(using Frame): Unit < Async = Abort.run[Closed](mutex.run(v)).unit

    protected def due: Boolean = passes.get() > 0 || loading.get() > 0
end BoundaryHold

/** The hold of one pass of a region or a mount inside a waiting boundary.
  *
  * Until the pass and every pass below it have ended, its paints wait, since the pass may still subscribe mounts that load. When they
  * end with nothing loading, the paints go out in order, as they would without a boundary. When they end with mounts loading, the
  * range keeps what it shows (the boundary's other content stays live), the paints are dropped, and once the mounts have loaded the
  * range is painted once from the live state. Then the group passes everything on.
  */
final private[kyo] class PassGroup(parent: BoundaryHold, up: BoundaryPass, mutex: Meter) extends BoundaryHold(mutex):
    import AllowUnsafe.embrace.danger

    def root: BoundaryGate = parent.root

    @volatile private var range: Maybe[BoundaryRange] = Absent
    @volatile private var done                        = false
    @volatile private var waited                      = false
    private val waitEnded                             = AtomicBoolean.Unsafe.init(false)
    private var held                                  = Chunk.empty[Paint]

    private val holding = AtomicBoolean.Unsafe.init(false)

    /** What the pass paints, set once it knows. Until the group is through, the range shows what it showed before while dispatch
      * already resolves the new content, so the range takes no event.
      */
    def paints(r: BoundaryRange)(using Frame): Unit < Sync =
        Sync.Unsafe.defer {
            range = Present(r)
            holding.compareAndSet(false, true)
        }.map(first => if first then root.dispatch.hold(r.path) else Kyo.unit)

    private def unhold(using Frame): Unit < Sync =
        Sync.Unsafe.defer(holding.compareAndSet(true, false)).map { was =>
            if was then range.fold(Kyo.unit)(r => root.dispatch.release(r.path)) else Kyo.unit
        }

    def onChange(
        region: ReactiveRegion,
        path: Seq[String],
        contentContext: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        previous: Maybe[UI],
        ui: UI
    )(using Frame): Unit < Async =
        locked {
            if closed.get() then Kyo.unit
            else if done then parent.onChange(region, path, contentContext, parentContext, previous, ui)
            else Sync.Unsafe.defer { held = held.append(Paint(region, path, contentContext, parentContext, previous, ui)) }
        }

    // Once the group is through, what runs below it is due on the holds above, like the pass of any later sibling.
    override def pass(using Frame): BoundaryPass < Sync = if done then parent.pass else super.pass

    override def load(using Frame): BoundaryHold < Sync = if done then parent.load else super.load

    override def onHold(region: ReactiveRegion, held: Boolean)(using Frame): Unit < Async = parent.onHold(region, held)

    /** The pass went away, a newer value of its region or the end of its scope: nothing it holds shows, and nothing waits for it. */
    def close(using Frame): Unit < Async =
        Sync.Unsafe.defer(closed.compareAndSet(false, true)).map { first =>
            if !first then Kyo.unit
            else unhold.andThen(endWait).andThen(up.end)
        }

    /** Ends a wait this group began: the range is no longer marked on the page, and the boundary no longer counts it. */
    private def endWait(using Frame): Unit < Async =
        if waited && waitEnded.compareAndSet(false, true) then
            range.fold(Kyo.unit)(r => parent.onHold(r.region, false)).andThen(root.rewaitEnded)
        else Kyo.unit

    protected def advance(using Frame): Unit < Async =
        locked {
            if closed.get() || done || passes.get() > 0 then Kyo.unit
            else if loading.get() == 0 then
                val out: Unit < Async =
                    if waited then
                        range match
                            case Present(r) =>
                                Boundaries.resolveLive(r.content, r.path, root.dispatch).map(ui =>
                                    parent.onChange(r.region, r.path, r.contentContext, r.parentContext, Absent, ui)
                                )
                            case Absent => Kyo.unit
                    else
                        val replay = held
                        Kyo.foreachDiscard(replay)(p =>
                            parent.onChange(p.region, p.path, p.contentContext, p.parentContext, p.previous, p.ui)
                        )
                out.andThen(Sync.Unsafe.defer {
                    done = true
                    held = Chunk.empty
                }).andThen(unhold).andThen(endWait).andThen(up.end)
            else if !waited then
                // The range keeps what it shows; it is marked on the page so that nobody types into what will not reach the server.
                Sync.Unsafe.defer {
                    waited = true
                    held = Chunk.empty
                }.andThen(range.fold(Kyo.unit)(r => parent.onHold(r.region, true))).andThen(root.rewaitBegan)
            else Kyo.unit
        }
end PassGroup

/** The exchange a waiting boundary puts between its content and the page.
  *
  * Until everything below it is through and nothing it catches is loading, it shows its placeholder, or under `KeepLatest` what a
  * predecessor at its place left, and drops the paints of its content; then it reveals the content in one paint, rendered from the live
  * state, which is why no paint it dropped is missing. After that, the [[PassGroup]]s below decide what shows, and it passes their
  * paints on; only under `Placeholder` does a later load bring its placeholder back, until that load is through.
  */
final private[kyo] class BoundaryGate(
    outer: UIExchange,
    rui: ReactiveUI,
    node: Bounded,
    val dispatch: MountDispatch,
    mutex: Meter,
    seed: Maybe[UI],
    initiallyRevealed: Boolean
) extends BoundaryHold(mutex):
    import AllowUnsafe.embrace.danger

    def root: BoundaryGate = this

    private val rewaits                    = AtomicInt.Unsafe.init(0)
    @volatile private var revealed         = initiallyRevealed
    @volatile private var shown: Maybe[UI] = if initiallyRevealed then Absent else Present(placeholder)
    @volatile private var seedPainted      = false

    private def placeholder: UI = node.settings.placeholderView.getOrElse(UI.empty(using node.frame))

    private def policy: UI.PendingPolicy = node.settings.pending.getOrElse(UI.PendingPolicy.KeepLatest)

    def isRevealed: Boolean = revealed

    @volatile private var showingSnapshot = false

    /** The placeholder, while it is what the boundary shows, so events reach it; a snapshot a predecessor left is inert, like the one
      * `KeepLatest` keeps for a mount.
      */
    def livePlaceholder: Maybe[UI] = if revealed || showingSnapshot then Absent else Present(placeholder)

    def onChange(
        region: ReactiveRegion,
        path: Seq[String],
        contentContext: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        previous: Maybe[UI],
        ui: UI
    )(using Frame): Unit < Async =
        locked {
            if closed.get() || !revealed then Kyo.unit
            else outer.onChange(region, path, contentContext, parentContext, previous, ui)
        }

    // Content the boundary holds back is not on the page, so there is nothing to mark there.
    override def onHold(region: ReactiveRegion, held: Boolean)(using Frame): Unit < Async =
        locked(if closed.get() || !revealed then Kyo.unit else outer.onHold(region, held))

    def close(using Frame): Unit < Sync = Sync.Unsafe.defer(closed.set(true))

    /** What the boundary shows now, resolved like [[Boundaries.resolveLive]]. */
    def displayed(using Frame): UI < Sync =
        if revealed then Boundaries.resolveLive(node.child, rui.path, dispatch) else shown.getOrElse(placeholder)

    /** A pass below ended with mounts loading after the boundary revealed. */
    private[internal] def rewaitBegan(using Frame): Unit < Async =
        locked {
            Sync.Unsafe.defer(discard(rewaits.incrementAndGet())).andThen {
                if closed.get() || !revealed then Kyo.unit
                else
                    policy match
                        case UI.PendingPolicy.KeepLatest  => Kyo.unit
                        case UI.PendingPolicy.Placeholder =>
                            paintRange(placeholder).andThen(Sync.Unsafe.defer {
                                revealed = false
                                showingSnapshot = false
                                shown = Present(placeholder)
                            })
            }
        }

    private[internal] def rewaitEnded(using Frame): Unit < Async =
        Sync.Unsafe.defer(rewaits.decrementAndGet()).andThen(advance)

    private def paintRange(ui: UI)(using Frame): Unit < Async =
        outer.onChange(rui.region, rui.path, rui.contentContext, rui.parentContext, Absent, ui)

    protected def advance(using Frame): Unit < Async =
        locked {
            if closed.get() || revealed then Kyo.unit
            else if !due && rewaits.get() == 0 then
                Boundaries.resolveLive(node.child, rui.path, dispatch).map(paintRange).andThen(Sync.Unsafe.defer {
                    revealed = true
                    shown = Absent
                })
            else if passes.get() == 0 && !seedPainted then
                (seed, policy) match
                    case (Present(snapshot), UI.PendingPolicy.KeepLatest) =>
                        paintRange(snapshot).andThen(Sync.Unsafe.defer {
                            seedPainted = true
                            showingSnapshot = true
                            shown = Present(snapshot)
                        })
                    case _ => Kyo.unit
            else Kyo.unit
        }
end BoundaryGate
