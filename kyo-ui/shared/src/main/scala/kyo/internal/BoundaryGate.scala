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
    def catches(ui: UI)(using Frame): Boolean < Sync = SignalNow.Locals.take(Sync.Unsafe.defer(catchesNow(ui)))

    /** [[catches]] for a pass that already holds the thread (see [[SignalNow]]). */
    def catchesNow(ui: UI)(using AllowUnsafe, Frame, SignalNow.Locals): Boolean =
        ui match
            case m: Mounted     => implicitOf(m).isEmpty
            case b: Bounded     => if b.settings.waits then false else catchesNow(b.child)
            case r: Reactive[?] => catchesNow(SignalNow(r.signal)(using r.frame, summon[SignalNow.Locals], summon[AllowUnsafe]))
            case fe: Foreach[?, ?] @unchecked =>
                fe.applyTyped {
                    [T] => (signal, _, render) =>
                        val items = SignalNow(signal)(using fe.frame, summon[SignalNow.Locals], summon[AllowUnsafe])
                        items.toSeq.iterator.zipWithIndex.exists((item, i) => catchesNow(render(i, item)))
                }
            case e: Element       => e.children.exists(catchesNow)
            case Fragment(cs)     => cs.exists(catchesNow)
            case KeyedChild(_, c) => catchesNow(c)
            case _                => false

    /** `ui` at `path` as the engine runs it now: every mount shows its instance's current content and every waiting boundary what it
      * shows, while regions and mounts keep their ranges, so a static rendering of the result is the live screen and later updates
      * still find where they land.
      */
    def resolveLive(ui: UI, path: Seq[String], at: ReactiveRegion.RegionIdentity, dispatch: MountDispatch)(using Frame): UI < Sync =
        ui match
            case m: Mounted =>
                implicitOf(m) match
                    case Present(b) => resolveLive(b, path, at, dispatch)
                    case Absent     =>
                        dispatch.lookup(at.transparent).map {
                            case Absent        => m
                            case Present(cell) =>
                                cell.current.map {
                                    case MountState.Pending(Present(snapshot)) =>
                                        m.copy(placeholderUI = Present(snapshot))(using m.frame)
                                    case state =>
                                        resolveLive(state.content(MountedSpec(m, path, Nil)), path, at.transparent, dispatch)
                                            .map(c => m.copy(placeholderUI = Present(c))(using m.frame))
                                }
                        }
            case b: Bounded =>
                val shown: UI < Sync =
                    if !b.settings.waits then resolveLive(b.child, path, at.transparent, dispatch).map(ownContent(b, _))
                    else
                        dispatch.boundary(at.transparent).map {
                            case Present(gate) => gate.displayed
                            case Absent        => b.display.getOrElse(b.settings.placeholderView.getOrElse(b.child))
                        }
                shown.map(d => b.copy(display = Present(d))(using b.frame))
            case r: Reactive[?] =>
                r.signal.current(using r.frame).map(c => resolveLive(c, path, at.transparent, dispatch)).map(c =>
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
                                resolveLive(render(i, item), path :+ key, at.child(key), dispatch)
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
                Kyo.foreach(e.children.toSeq.zipWithIndex)((c, i) => resolveLive(c, path :+ i.toString, at.child(i.toString), dispatch))
                    .map(cs => ReactiveUI.rebuildElement(e, Chunk.from(cs)))
            case f: Fragment[?] =>
                Kyo.foreach(f.children.toSeq.zipWithIndex) { (c, i) =>
                    c match
                        case kc: KeyedChild[?] =>
                            resolveLive(kc.child, path :+ kc.key, at.child(kc.key), dispatch).map(r =>
                                KeyedChild[UI](kc.key, r)(using kc.frame): UI
                            )
                        case _ => resolveLive(c, path :+ i.toString, at.child(i.toString), dispatch)
                }.map(cs => Fragment[UI](Chunk.from(cs))(using f.frame))
            case KeyedChild(k, c) => resolveLive(c, path, at, dispatch).map(r => KeyedChild[UI](k, r))
            case other            => other

    /** What boundary `b` shows when it shows its content, given that content as [[resolveLive]] resolved it.
      *
      * A boundary around one mount, which a mount's own `.placeholder` is, is that mount's slot, and its range carries the slot's
      * marker. The mount resolves to a copy of itself with its live content as the placeholder, which renders as a slot of its own
      * inside the first. A client that has adopted that inner range takes it for a slot some other instance owns and leaves it alone
      * on every later paint of the boundary, which is exactly the paint that brings the instance's content, so the page stays on what
      * the first instance showed. The mount is therefore shown as a constant region of its live content: the same range at the same
      * id, where the instance's later updates land, without a second slot marker. A placeholder that is a mount of its own is a
      * different call site and stays as it is.
      */
    def ownContent(b: Bounded, shown: UI): UI =
        (b.child, shown) match
            case (child: Mounted, m: Mounted) if m.frame == child.frame && sameKey(m.key, child.key) =>
                given Frame = m.frame
                Reactive[UI](Signal.initConst(m.placeholderUI.getOrElse(UI.empty)))
            case _ => shown

    /** Whether two mounts carry the same `.keyed` key, compared by value as the engine's mount identity compares it. */
    private def sameKey(a: Maybe[Any], b: Maybe[Any]): Boolean =
        (a, b) match
            case (Present(x), Present(y)) => x.equals(y)
            case (Absent, Absent)         => true
            case _                        => false
end Boundaries

/** A pass that is due on a [[BoundaryHold]]: it ends once, by [[end]] when it has run or by [[abandon]] from a finalizer when it never
  * will.
  */
final private[kyo] class BoundaryPass(hold: BoundaryHold):
    // Unsafe: a pass is made with `new` inside `Sync.Unsafe.defer`, and a proof on the constructor would reach every method.
    private val ended = AtomicBoolean.Unsafe.init(false)(using AllowUnsafe.embrace.danger)

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
    content: UI,
    origin: ReactiveUI
)

/** An exchange inside a waiting boundary that counts what is still due below it: passes of regions and mounts that are due or running,
  * and caught mounts that have not loaded. Paints from below wait in it until it knows whether they may show.
  */
sealed abstract private[kyo] class BoundaryHold(mutex: Meter) extends UIExchange:

    // A hold's own first pass is due from the start, until what it was made for has been subscribed.
    // Unsafe: a hold is made with `new`, by a subscription with no proof to pass on; a proof on the constructor would reach every method.
    protected val passes  = AtomicInt.Unsafe.init(1)(using AllowUnsafe.embrace.danger)
    protected val loading = AtomicInt.Unsafe.init(0)(using AllowUnsafe.embrace.danger)
    protected val closed  = AtomicBoolean.Unsafe.init(false)(using AllowUnsafe.embrace.danger)

    def root: BoundaryGate

    override private[kyo] def gate: Maybe[BoundaryHold] = Present(this)

    /** An in-place patch from below, held, passed on or dropped like a paint: it lands on the same range a paint would. */
    protected def forward(patch: UIExchange => Unit < Async)(using Frame): Unit < Async

    /** A paint or patch from below, applied to the exchange it finally reaches. A paint measured for devtools travels this way, so it is
      * measured where it reaches the page, and not at all when a hold drops it.
      */
    def deliver(patch: UIExchange => Unit < Async)(using Frame): Unit < Async = forward(patch)

    override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
        forward(_.onAttrPatch(path, name, value))
    override def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async =
        forward(_.onBoolAttrPatch(path, name, value))
    override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
        forward(_.onClassPatch(path, name, on))
    override def onStylePatch(path: Seq[String], set: Seq[(String, String)], removed: Seq[String])(using Frame): Unit < Async =
        forward(_.onStylePatch(path, set, removed))
    override def onFreshInstance(region: ReactiveRegion)(using Frame): Unit < Async =
        forward(_.onFreshInstance(region))

    /** Sends `command` once everything that reached this hold before it is on the page, and drops it when that never will be. */
    def afterShown(command: HeldCommand)(using Frame): Unit < Async

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
    def load(using Frame): BoundaryHold < Async = holdRanges.andThen(Sync.Unsafe.defer { discard(loading.incrementAndGet()); this })

    /** Something below may load from now on, so the ranges of the passes it is part of keep showing what they show for as long as that
      * takes, and take no event while dispatch resolves their new content. A boundary has no range of its own to hold: until it reveals,
      * its placeholder takes the events.
      */
    private[internal] def holdRanges(using Frame): Unit < Async

    /** A caught mount that loaded, failed, or went away before either; the pass it happened in decides what follows. */
    def settle(using AllowUnsafe): Unit = discard(loading.decrementAndGet())

    /** Ends the hold's own first pass. */
    def started(using Frame): Unit < Async = passEnded

    private[internal] def passEnded(using Frame): Unit < Async =
        Sync.Unsafe.defer(passes.decrementAndGet()).andThen(advance)

    private[internal] def passAbandoned()(using AllowUnsafe): Unit = discard(passes.decrementAndGet())

    protected def advance(using Frame): Unit < Async

    // Reentrant, since a hold passes paints and ends passes on the hold above it in the same fiber, under the mutex they share.
    protected def locked(v: AllowUnsafe ?=> Unit < Async)(using Frame): Unit < Async =
        Abort.run[Closed](mutex.run(Sync.Unsafe.defer(v))).unit

    protected def due(using AllowUnsafe): Boolean = passes.get() > 0 || loading.get() > 0
end BoundaryHold

private[kyo] object BoundaryHold:

    /** What a pass of the range at the given path does once it has subscribed what it shows and before it ends: nothing, except in a test
      * that pauses a pass there, its paints still waiting, to send an event while it runs.
      */
    val duringPass: Local[Seq[String] => Unit < Async] = Local.init[Seq[String] => Unit < Async](_ => Kyo.unit)
end BoundaryHold

/** The hold of one pass of a region or a mount inside a waiting boundary.
  *
  * Until the pass and every pass below it have ended, its paints wait, since the pass may still subscribe mounts that load. When they
  * end with nothing loading, the paints go out in order, as they would without a boundary. When they end with mounts loading, the
  * range keeps what it shows (the boundary's other content stays live), the paints are dropped, and once the mounts have loaded the
  * range is painted once from the live state. Then the group passes everything on.
  */
final private[kyo] class PassGroup(parent: BoundaryHold, up: BoundaryPass, mutex: Meter) extends BoundaryHold(mutex):

    def root: BoundaryGate = parent.root

    @volatile private var range: Maybe[BoundaryRange] = Absent
    @volatile private var done                        = false
    @volatile private var waited                      = false
    // Unsafe: as for the hold's counters above.
    private val waitEnded = AtomicBoolean.Unsafe.init(false)(using AllowUnsafe.embrace.danger)
    private var held      = Chunk.empty[UIExchange => Unit < Async]
    private var commands  = Chunk.empty[HeldCommand]

    // Unsafe: as for the hold's counters above.
    private val holding = AtomicBoolean.Unsafe.init(false)(using AllowUnsafe.embrace.danger)

    /** What the pass paints, set once it knows. A pass that loads nothing takes events the whole time, as a region outside any boundary
      * does, since nothing marks its range inert and its paints are about to go out: an event reaches the live state.
      */
    def paints(r: BoundaryRange)(using Frame): Unit < Sync = Sync.Unsafe.defer { range = Present(r) }

    // Under the lock, since a pass that ends releases its range and turns done in one step: a range held after that would stay held.
    private[internal] def holdRanges(using Frame): Unit < Async =
        locked {
            if closed.get() then Kyo.unit
            else if done then parent.holdRanges
            else
                val first = holding.compareAndSet(false, true)
                (if first then range.fold(Kyo.unit)(r => root.dispatch.hold(r.path)) else Kyo.unit).andThen(parent.holdRanges)
        }

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
        forward(_.onChange(region, path, contentContext, parentContext, previous, ui))

    protected def forward(patch: UIExchange => Unit < Async)(using Frame): Unit < Async =
        locked {
            if closed.get() then Kyo.unit
            else if done then parent.deliver(patch)
            else Sync.Unsafe.defer { held = held.append(patch) }
        }

    // A command goes up with the group's paints, behind them, once the group is through.
    def afterShown(command: HeldCommand)(using Frame): Unit < Async =
        locked {
            if closed.get() then command.drop
            else if done then parent.afterShown(command)
            else Sync.Unsafe.defer { commands = commands.append(command) }
        }

    // Once the group is through, what runs below it is due on the holds above, like the pass of any later sibling.
    override def pass(using Frame): BoundaryPass < Sync = if done then parent.pass else super.pass

    override def load(using Frame): BoundaryHold < Async = if done then parent.load else super.load

    override def onHold(region: ReactiveRegion, held: Boolean)(using Frame): Unit < Async = parent.onHold(region, held)

    /** The pass went away, a newer value of its region or the end of its scope: nothing it holds shows, and nothing waits for it. */
    def close(using Frame): Unit < Async =
        Sync.Unsafe.defer(closed.compareAndSet(false, true)).map { first =>
            if !first then Kyo.unit
            else dropCommands.andThen(unhold).andThen(endWait).andThen(up.end)
        }

    private def dropCommands(using Frame): Unit < Sync =
        Sync.Unsafe.defer {
            val dropped = commands
            commands = Chunk.empty
            dropped
        }.map(Kyo.foreachDiscard(_)(_.drop))

    /** Ends a wait this group began: the range is no longer marked on the page, and the boundary no longer counts it. */
    private def endWait(using Frame): Unit < Async =
        Sync.Unsafe.defer(waited && waitEnded.compareAndSet(false, true)).map { ended =>
            if ended then range.fold(Kyo.unit)(r => parent.onHold(r.region, false)).andThen(root.rewaitEnded)
            else Kyo.unit
        }

    protected def advance(using Frame): Unit < Async =
        locked {
            if closed.get() || done || passes.get() > 0 then Kyo.unit
            else if loading.get() == 0 then
                val out: Unit < Async =
                    if waited then
                        range match
                            case Present(r) =>
                                Boundaries.resolveLive(r.content, r.path, r.contentContext, root.dispatch).map(ui =>
                                    ReactiveUI.deliverMeasured(r.origin, UI.RenderCause.Signal, parent)(
                                        _.onChange(r.region, r.path, r.contentContext, r.parentContext, Absent, ui)
                                    )
                                )
                            case Absent => Kyo.unit
                    else
                        val replay = held
                        Kyo.foreachDiscard(replay)(parent.deliver)
                // The range takes events again before the paint goes out: a client that has it may answer at once.
                unhold.andThen(out).andThen(Sync.Unsafe.defer {
                    done = true
                    held = Chunk.empty
                    val waiting = commands
                    commands = Chunk.empty
                    waiting
                }).map(Kyo.foreachDiscard(_)(parent.afterShown)).andThen(endWait).andThen(up.end)
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
  *
  * One paint it drops is not made good by its own reveal on every client: that of a boundary below that revealed in the meantime. Its
  * live content is in the reveal paint, but a client that morphs the reveal over a placeholder of the same shape (a tab strip whose
  * placeholder is the strip itself) finds that boundary's keyed slot already there under the same key and leaves it for the instance to
  * paint, which it did, into the paint this gate dropped. So every gate below registers with the gate above it, and a reveal paints
  * each of them that is revealed again, right after its own paint and through its own exchange, all the way down.
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

    def root: BoundaryGate = this

    /** The waiting boundaries directly below this one, whose reveal paints this one drops while it waits (see the class doc). */
    // Unsafe: as for the hold's counters above.
    private val nested = AtomicRef.Unsafe.init(Set.empty[BoundaryGate])(using AllowUnsafe.embrace.danger)

    /** The gate above this one, the one whose waiting drops this one's paints, if there is one. */
    private val above: Maybe[BoundaryGate] = outer.gate.map(_.root)

    // Unsafe: as for the hold's counters above.
    private val rewaits                    = AtomicInt.Unsafe.init(0)(using AllowUnsafe.embrace.danger)
    @volatile private var revealed         = initiallyRevealed
    @volatile private var shown: Maybe[UI] = if initiallyRevealed then Absent else Present(placeholder)
    @volatile private var seedPainted      = false
    private var commands                   = Chunk.empty[HeldCommand]

    private def placeholder: UI = node.settings.placeholderView.getOrElse(UI.empty(using node.frame))

    private def policy: UI.PendingPolicy = node.settings.pending.getOrElse(UI.PendingPolicy.KeepLatest)

    def isRevealed: Boolean = revealed

    private[internal] def holdRanges(using Frame): Unit < Async = Kyo.unit

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
        forward(_.onChange(region, path, contentContext, parentContext, previous, ui))

    // Until the reveal, which renders the live state, nothing below is on the page.
    protected def forward(patch: UIExchange => Unit < Async)(using Frame): Unit < Async =
        locked {
            if closed.get() || !revealed then Kyo.unit
            else
                outer.gate match
                    case Present(hold) => hold.deliver(patch)
                    case Absent        => patch(outer)
        }

    // Content the boundary holds back is not on the page, so there is nothing to mark there.
    override def onHold(region: ReactiveRegion, held: Boolean)(using Frame): Unit < Async =
        locked(if closed.get() || !revealed then Kyo.unit else outer.onHold(region, held))

    def close(using Frame): Unit < Sync =
        Sync.Unsafe.defer {
            closed.set(true)
            above.foreach(g => discard(g.nested.getAndUpdate(_ - this)))
            val dropped = commands
            commands = Chunk.empty
            dropped
        }.map(Kyo.foreachDiscard(_)(_.drop))

    // Until the reveal nothing below is on the page, so a command waits for it; after it, it follows the paints out as they go.
    def afterShown(command: HeldCommand)(using Frame): Unit < Async =
        locked {
            if closed.get() then command.drop
            else if revealed then sendOut(command)
            else Sync.Unsafe.defer { commands = commands.append(command) }
        }

    private def sendOut(command: HeldCommand)(using Frame): Unit < Async =
        outer.gate match
            case Present(hold) => hold.afterShown(command)
            case Absent        => command.send

    /** What the boundary shows now, resolved like [[Boundaries.resolveLive]]. */
    def displayed(using Frame): UI < Sync =
        if revealed then displayedContent else shown.getOrElse(placeholder)

    /** The content as the boundary shows it once revealed, see [[Boundaries.ownContent]]. */
    private def displayedContent(using Frame): UI < Sync =
        Boundaries.resolveLive(node.child, rui.path, rui.contentContext, dispatch).map(Boundaries.ownContent(node, _))

    /** A pass below ended with mounts loading after the boundary revealed. */
    private[internal] def rewaitBegan(using Frame): Unit < Async =
        locked {
            Sync.Unsafe.defer(discard(rewaits.incrementAndGet())).andThen {
                if closed.get() || !revealed then Kyo.unit
                else
                    policy match
                        case UI.PendingPolicy.KeepLatest  => Kyo.unit
                        case UI.PendingPolicy.Placeholder =>
                            // Before the paint goes out, as for a reveal.
                            Sync.Unsafe.defer {
                                revealed = false
                                showingSnapshot = false
                                shown = Present(placeholder)
                            }.andThen(paintRange(placeholder))
            }
        }

    private[internal] def rewaitEnded(using Frame): Unit < Async =
        Sync.Unsafe.defer(rewaits.decrementAndGet()).andThen(advance)

    /** Whether the boundary's own first pass is through: a paint of its range before that is part of its creation. */
    @volatile private var begun = false

    override def started(using Frame): Unit < Async = super.started.andThen(Sync.Unsafe.defer { begun = true })

    /** Paints again every boundary below this one that revealed while this one waited, whose own reveal paint this one dropped (see
      * the class doc), and so on down.
      *
      * Every one of them goes out through this gate's own exchange, as its reveal paint did, and none through the gates between. A
      * boundary in between paints by taking its own lock and then this one's, so a repaint that went through its lock while holding
      * this one's would wait on a boundary that waits on this one, and the page would stop there.
      */
    private def repaintNested(using Frame): Unit < Async = repaintNestedThrough(outer)

    private def repaintNestedThrough(through: UIExchange)(using Frame): Unit < Async =
        Sync.Unsafe.defer {
            Kyo.foreachDiscard(nested.get().toSeq)(g => if g.isRevealed && !g.closed.get() then g.repaintThrough(through) else Kyo.unit)
        }

    private def repaintThrough(through: UIExchange)(using Frame): Unit < Async =
        displayedContent.map(paintRangeThrough(_, through)).andThen(repaintNestedThrough(through))

    private def paintRange(ui: UI)(using Frame): Unit < Async = paintRangeThrough(ui, outer)

    private def paintRangeThrough(ui: UI, through: UIExchange)(using Frame): Unit < Async =
        ReactiveUI.deliverMeasured(rui, if begun then UI.RenderCause.Signal else UI.RenderCause.Created, through)(
            _.onChange(rui.region, rui.path, rui.contentContext, rui.parentContext, Absent, ui)
        )

    protected def advance(using Frame): Unit < Async =
        locked {
            if closed.get() || revealed then Kyo.unit
            else if !due && rewaits.get() == 0 then
                displayedContent.map { content =>
                    // Before the paint goes out: a client that has it may answer at once, and that event has to reach the content.
                    Sync.Unsafe.defer {
                        revealed = true
                        shown = Absent
                        val waiting = commands
                        commands = Chunk.empty
                        waiting
                        // The commands follow the nested paints, since they may be about what those paints bring.
                    }.map(waiting => paintRange(content).andThen(repaintNested).andThen(Kyo.foreachDiscard(waiting)(sendOut)))
                }
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

    // Last in the body, once every field is set: from here on the gate above may reveal and repaint this one at any moment.
    // Unsafe: the constructor's last statement, where a gate with a proof on its constructor would hand it to every method.
    above.foreach(g => discard(g.nested.getAndUpdate(_ + this)(using AllowUnsafe.embrace.danger)))
end BoundaryGate
