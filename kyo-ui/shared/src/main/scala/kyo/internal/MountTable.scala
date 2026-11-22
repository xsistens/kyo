package kyo.internal

import DragSessions.*
import EventDispatch.*
import MountRegions.*
import ReactiveUI.*
import RowRegions.*
import kyo.*
import kyo.UI.*
import kyo.UI.Ast.*
import kyo.kernel.ContextEffect

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
    cells: AtomicRef[Dict[ReactiveRegion.RegionIdentity, Chunk[Signal[MountState]]]],
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

    /** Registers `cell` of the mount whose content has identity `at`. */
    def register(at: ReactiveRegion.RegionIdentity, cell: Signal[MountState])(using Frame): Unit < Sync =
        cells.getAndUpdate(m => m.update(at, m.get(at).getOrElse(Chunk.empty).append(cell))).unit

    /** Removes this registration only. Two subscriptions can hold one path for a moment, an old one still ending and its successor,
      * in either order; the other keeps its entry.
      */
    def unregister(at: ReactiveRegion.RegionIdentity, cell: Signal[MountState])(using Frame): Unit < Sync =
        cells.getAndUpdate { m =>
            m.get(at) match
                case Absent        => m
                case Present(held) =>
                    val i    = held.lastIndexWhere(_ eq cell)
                    val left = if i < 0 then held else held.take(i).concat(held.drop(i + 1))
                    if left.isEmpty then m.remove(at) else m.update(at, left)
        }.unit

    /** The cell registered last for the mount whose content has identity `at`. */
    def lookup(at: ReactiveRegion.RegionIdentity)(using Frame): Maybe[Signal[MountState]] < Sync =
        cells.get.map(_.get(at).flatMap(_.lastMaybe))
end MountDispatch

private[kyo] object MountDispatch:
    def init(using Frame): MountDispatch < Sync = Sync.Unsafe.defer(initNow())

    /** The table for a pass that already holds the thread (see [[SignalNow]]). */
    def initNow()(using AllowUnsafe): MountDispatch =
        new MountDispatch(
            AtomicRef.Unsafe.init(Dict.empty[ReactiveRegion.RegionIdentity, Chunk[Signal[MountState]]]).safe,
            AtomicRef.Unsafe.init(Dict.empty[ReactiveRegion.RegionIdentity, Chunk[BoundaryGate]]).safe,
            AtomicRef.Unsafe.init(Chunk.empty[Seq[String]]).safe
        )
end MountDispatch

/** A `Mounted` node as normalization found it: the node, its render path, and the keys of the list rows between it and the owner of
  * its mount table, innermost first.
  */
final private[kyo] case class MountedSpec(node: UI.Ast.Mounted, path: Seq[String], rows: Chunk[String]):
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

    def mountError(t: Throwable, source: UI.MountErrorSource): UI.MountError =
        UI.MountError(t, source, Maybe(t.getMessage).getOrElse(t.toString), node.frame, path, node.key)

    /** The instance identity of a keyed node; a keyless node has none, since every re-render replaces its instance. `rows` are the keys of
      * the list rows between the node and the owner of its table, the session root or the mount instance whose content it is in, across
      * every region in between.
      */
    def identity: Maybe[MountIdentity] =
        node.key.map(MountIdentity(rows, node.site, _))
end MountedSpec

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
            .andThen(supervision.afterShown.drop)
end MountInstance

private[kyo] object MountInstance:
    /** `views` are the error views of the boundaries around the node, innermost first; the content's mounts inherit them. */
    def start(
        spec: MountedSpec,
        seed: Maybe[UI],
        views: List[PartialFunction[UI.MountError, UI]],
        parent: MountTable,
        launch: Boolean = true
    )(using
        Frame
    ): MountInstance < Sync =
        for
            cell <- Signal.initRef[MountState](MountState.Pending(seed))
            supervision = MountSupervision(spec, cell, views)
            scope  <- Scope.Finalizer.init(1)
            go     <- Promise.init[Unit, Any]
            _      <- if launch then go.completeUnitDiscard else Kyo.unit
            runner <- Fiber.initUnscoped(go.get.andThen(runEffect(spec, scope, supervision, parent.session)))
        yield new MountInstance(cell, supervision, childTable(supervision, parent), runner, scope, go)

    /** The table of the content of an instance supervised by `supervision`: its mounts meet the boundaries around it now. */
    private def childTable(supervision: MountSupervision, parent: MountTable): MountTable =
        new MountTable(parent.session, () => supervision.views)

    /** An instance that runs no effect and shows its node's error view for `message` from the start: what each place of an identity
      * shown more than once in one pass gets.
      */
    def failed(spec: MountedSpec, message: String, views: List[PartialFunction[UI.MountError, UI]], parent: MountTable)(using
        Frame
    ): MountInstance < Sync =
        for
            cell <- Signal.initRef[MountState](MountState.Pending(Absent))
            supervision = MountSupervision(spec, cell, views)
            _      <- supervision.fail(UI.DuplicateMountException(message), UI.MountErrorSource.Duplicate)
            scope  <- Scope.Finalizer.init(1)
            go     <- Promise.init[Unit, Any]
            runner <- Fiber.initUnscoped(Kyo.unit)
        yield new MountInstance(cell, supervision, childTable(supervision, parent), runner, scope, go)

    /** Runs the effect with `scope` as its `Scope`, so what it acquires outlives this fiber, and publishes the outcome. */
    private def runEffect(spec: MountedSpec, scope: Scope.Finalizer, supervision: MountSupervision, session: MountSession)(using
        Frame
    ): Unit < Async =
        val effect = MountSession.current.let(Present(session))(MountSupervision.local.let(Present(supervision))(spec.node.effect))
        Abort.run[Throwable](ContextEffect.handleInheritable(Tag[Scope], scope)(effect)).map { result =>
            supervision.afterShown.built.andThen(result)
        }.map {
            case Result.Success(ui) => supervision.publish(ui)
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
  * failure does not overwrite it. The node and the error views are those of the node that shows the instance now: an instance that
  * moves to another node takes that node's error view and its boundaries'.
  */
final private[kyo] class MountSupervision(
    initialSpec: MountedSpec,
    cell: SignalRef[MountState],
    initialViews: List[PartialFunction[UI.MountError, UI]]
):
    // Unsafe: written by the claim that adopts the instance, read wherever a failure is routed.
    private val at      = AtomicRef.Unsafe.init((initialSpec, initialViews))(using AllowUnsafe.embrace.danger)
    private val failure = AtomicRef.Unsafe.init(Maybe.empty[(Throwable, UI.MountErrorSource)])(using AllowUnsafe.embrace.danger)

    private def spec: MountedSpec = at.get()(using AllowUnsafe.embrace.danger)._1

    def views: List[PartialFunction[UI.MountError, UI]] = at.get()(using AllowUnsafe.embrace.danger)._2

    /** Makes `spec` and `views` the node and error views of this instance. A failed instance that `moved` to another node is shown
      * again in that node's error view.
      */
    def adopt(spec: MountedSpec, views: List[PartialFunction[UI.MountError, UI]], moved: Boolean)(using Frame): Unit < Sync =
        Sync.Unsafe.defer {
            at.set((spec, views))
            if !moved then Kyo.unit
            else
                failure.get() match
                    case Present((raised, source)) => cell.set(MountState.Failed(errorView(raised, source)))
                    case Absent                    => Kyo.unit
            end if
        }

    private def errorView(raised: Throwable, source: UI.MountErrorSource): UI =
        val (s, v) = at.get()(using AllowUnsafe.embrace.danger)
        s.errorUI(s.mountError(raised, source), v)

    /** The commands the instance sent that wait for its content to be on the page. */
    val afterShown = new AfterShown

    /** The effect's content, unless a failure came first. */
    def publish(ui: UI)(using Frame): Unit < Sync = cell.getAndUpdate(MountSupervision.publish(ui)).unit

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

    def fail(raised: Throwable, source: UI.MountErrorSource)(using Frame): Unit < Sync =
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
final private[kyo] case class MountIdentity(rows: Chunk[String], site: UI.MountSite, key: Any) derives CanEqual:
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
    val views: () => List[PartialFunction[UI.MountError, UI]],
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

/** One mount session: the open handlers and [[kyo.UI.batch]] blocks, which together form one change, and the tables waiting for that
  * change to end before they can take a cut.
  */
final private[kyo] class MountSession:
    import AllowUnsafe.embrace.danger

    private val open    = new java.util.concurrent.atomic.AtomicInteger()
    private val pending = AtomicRef.Unsafe.init(Set.empty[MountTable])
    val counters        = new MountCounters

    def isOpen: Boolean = open.get() > 0

    def enter(): Unit = discard(open.incrementAndGet())

    /** Ends one handler or batch; the last one to end lets every waiting table take its cut. */
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
    /** The session of the handler or mount effect that is running, inherited by the fibers they fork. */
    val current: Local[Maybe[MountSession]] = Local.init(Maybe.empty[MountSession])
end MountSession

/** The passes of one region subscription as its observation records them in [[stamp]] (see `Signal.ReadStamp`): how many started,
  * which of them last recorded its walk in the table, and whether a write landed since the latest read. A cut waits for a region
  * until it has seen every write of the change: it was current when the change ended, or a pass that started after that has recorded
  * its walk.
  */
final private[kyo] class RegionWatch(scope: MountScope):
    /** What the region's observation records, through `Signal.observeStamped`; each rest rechecks the table. */
    val stamp: Signal.ReadStamp = new Signal.ReadStamp(() => scope.table.recheck())

    private val planned         = new java.util.concurrent.atomic.AtomicLong()
    private[internal] val ended = new java.util.concurrent.atomic.AtomicBoolean()

    /** The pass that started last has recorded its walk. */
    def recorded(): Unit =
        planned.set(stamp.passes)
        scope.table.recheck()

    def end(): Unit =
        ended.set(true)
        scope.table.recheck()

    def started: Long = stamp.passes

    def restedSoFar: Long = stamp.rests

    /** Whether the region has seen every write up to a cut taken when it had started `at` passes and rested `restsAt` times: a pass
      * started since has recorded its walk, or its latest pass has, and its stamp says nothing landed since, or it re-read and found
      * its value unchanged since. The last is what catches up a region whose stamp cannot tell (a signal without an exact read).
      */
    def caughtUp(at: Long, restsAt: Long)(using AllowUnsafe): Boolean =
        ended.get() || !scope.followed || {
            val p = planned.get()
            p > at || (p == stamp.passes && (!stamp.behind() || stamp.rests > restsAt))
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
  * transit, so what is released is released before it starts. When the change ends (the handlers and [[kyo.UI.batch]] blocks of the
  * session, or the pass itself outside them), the table records where every region's passes stand; the cut is reached once each has
  * seen every write of the change. A region over a signal without an exact read (one built with `Signal.initRaw`, or a combinator over one) cannot tell whether
  * a write reached it, so it counts as behind until its next re-read, which its repair timer brings within one repair interval
  * (`Signal.defaultRepairInterval`): a cut in a table that holds such a region can wait that long. The decision is taken the moment the
  * cut is reached, by whatever reports the last region caught up: an identity shown once has moved there, one shown nowhere is
  * released, and one shown in several places is reported at all of them. It depends on what the regions show once they all saw the
  * writes, never on the order they rendered them in, and a write after it is a change of its own, also when the table's settling fiber
  * has not yet carried the decision out: a node hidden and shown again gets a new instance.
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
final private[kyo] class MountTable(val session: MountSession, viewsOf: () => List[PartialFunction[UI.MountError, UI]]):
    import AllowUnsafe.embrace.danger

    private val state   = AtomicRef.Unsafe.init(TableState.empty)
    private val watches = AtomicRef.Unsafe.init(Set.empty[RegionWatch])
    private val cut     = AtomicRef.Unsafe.init(Maybe.empty[Chunk[(RegionWatch, Long, Long)]])
    private val wake    = AtomicRef.Unsafe.init(Promise.Unsafe.init[Unit, Any]())
    private val running = new java.util.concurrent.atomic.AtomicBoolean()
    private val closed  = new java.util.concurrent.atomic.AtomicBoolean()

    def views: List[PartialFunction[UI.MountError, UI]] = viewsOf()

    private[internal] def read: TableState = state.get()

    @scala.annotation.tailrec
    final private[internal] def modify[A](f: TableState => (TableState, A)): A =
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
    private[internal] def awaitCut()(using Frame): Unit < Sync =
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
                        instance.supervision.adopt(view.spec, view.views(), moved = true).andThen(view.state.set(next))
                    case _ => view.state.set(next)
            })
            .andThen(Kyo.foreachDiscard(decision.starts) { (identity, view) =>
                MountInstance.start(view.spec, view.seed, view.views(), this).map { fresh =>
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
  * which makes region keys relative. The error views of the boundaries between the region and the table's content come first in `views`.
  */
final private[kyo] class MountScope private[internal] (
    val table: MountTable,
    val region: RegionKey,
    birth: Long,
    basePath: Seq[String],
    baseDepth: Int,
    boundaries: List[PartialFunction[UI.MountError, UI]]
):
    import AllowUnsafe.embrace.danger

    @volatile private var watch: Maybe[RegionWatch] = Absent

    def views: List[PartialFunction[UI.MountError, UI]] = boundaries ++ table.views

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

    private[internal] def keyOf(at: ReactiveRegion.RegionIdentity): RegionKey =
        val rel = at.path.drop(basePath.size)
        RegionKey(rel, if rel.isEmpty then at.transparentDepth - baseDepth else at.transparentDepth)

    /** The scope of the region `kid`, which `pass` subscribes. */
    def child(pass: MountPass, kid: ReactiveUI): MountScope =
        val inner = kid.bounded.flatMap(_.settings.errorView).fold(boundaries)(_ :: boundaries)
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
    def openWalk(kids: Seq[ReactiveUI], retained: Set[String] = Set.empty)(using Frame): (MountPass, () => Unit < Async) < Sync =
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

    private def plan(s: TableState, entry: RegionEntry, kids: Seq[ReactiveUI], retained: Set[String], id: Long): (TableState, Opened) =
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
    def beginWalk(kids: Seq[ReactiveUI], retained: Set[String] = Set.empty)(using Frame): MountPass < Async =
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

    /** Whether `identity` has an instance in the table, which a claim on it keeps rather than starts. */
    def hasInstance(identity: MountIdentity)(using Frame): Boolean < Sync =
        Sync.Unsafe.defer(table.read.instances.get(identity).nonEmpty)

    /** The claim of the keyed node `rui` of `pass` on `identity`: the report when the pass shows the identity in another place too;
      * the live instance when no other region shows it; otherwise, or while something is in transit and the instance is still to
      * start, what the node's `PendingPolicy` shows until the table's next cut decides. A subscription the table no longer follows
      * gets the live instance, or one of its own that its scope releases.
      */
    def claim(rui: ReactiveUI, identity: MountIdentity, spec: MountedSpec, pass: MountPass)(using
        Frame
    ): ClaimView < (Async & Scope) =
        def view(state: ClaimState, seed: Maybe[UI]): ClaimView < Sync =
            Signal.initRef[ClaimState](state).map(ref => new ClaimView(rui, spec, region, () => views, seed, ref))
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
                                    MountInstance.start(spec, Absent, views, table).map(i =>
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
                                    }).andThen(slot.instance.supervision.adopt(spec, views, moved))
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
                                    MountInstance.start(spec, seed, views, table, launch = false).map { fresh =>
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
            MountInstance.start(spec, seed, views, table).map { instance =>
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
private[kyo] object MountRegions:

    /** The nodes one render pass of a region owns: its reactive children, and through a constant child (an element that only
      * carries reactive attributes) that child's own, which the constant node subscribes in the same pass and the same table.
      */
    private[internal] def siteHash(frame: Frame): Int =
        val s   = frame.toString
        val end = math.min(s.length, 64)
        var h   = s.length
        var i   = 0
        while i < end do
            h = 31 * h + s.charAt(i)
            i += 1
        h
    end siteHash

    /** Subscribes a mounted node in the current scope. A keyed node adopts or creates the instance of its identity in the table of the
      * enclosing mount instance or session; a keyless node gets a new instance that ends with the current scope, which for a node inside
      * a region is the region's current value.
      */
    private[internal] def subscribeMounted(
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
        // An effect may run from the moment it starts, before the content subscription counts it as loading, so what it may hold up takes
        // no event from before that. A kept instance starts nothing.
        def beforeStart: Unit < Async = exchange.gate.fold(Kyo.unit: Unit < Async)(_.holdRanges)
        mounts.current.map { follow =>
            spec.identity match
                case Absent            => beforeStart.andThen(mounts.startKeyless(spec)).map(show(_, follow))
                case Present(identity) =>
                    mounts.hasInstance(identity).map(kept => if kept then Kyo.unit else beforeStart).andThen(mounts.claim(
                        rui,
                        identity,
                        spec,
                        pass
                    )).map { view =>
                        def showClaim(state: ClaimState): Unit < (Async & Scope) =
                            state match
                                case ClaimState.Live(instance)     => show(instance, follow)
                                case ClaimState.Waiting(shown)     => showWaiting(rui, shown, spec, exchange)
                                case ClaimState.Duplicate(message) =>
                                    MountInstance.failed(spec, message, mounts.views, mounts.table)
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
            _       <- dispatch.register(rui.contentContext, instance.cell)
            _       <- Scope.ensure(dispatch.unregister(rui.contentContext, instance.cell))
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
                                    deepRows = Chunk.empty
                                )))
                                pass <- content.beginWalk(kids)
                                _    <- forkRootChannels(rui.path, shown, through)
                                _    <- Kyo.foreachDiscard(kids)(subscribeScoped(_, through, signalChangeTime, content, dispatch, pass))
                                _    <- content.endWalk
                            yield ()
                    // The instance's own content is on the page now, and the regions inside it are subscribed against what it shows, so
                    // the commands it sent about it follow this paint. Sent before the walk, a reply could land between the paint and the
                    // walk's snapshot, and the region over it would skip a value the page never showed.
                    _ <- state match
                        case MountState.Ready(_) | MountState.Failed(_) => instance.supervision.afterShown.shown(through)
                        case MountState.Pending(_)                      => Kyo.unit
                yield ()
                end for
            // Inside a waiting boundary every state is a pass with a group of its own, the first one due from now, and the mount loads
            // until it has published or failed.
            observed <- exchange.gate match
                case Absent        => Kyo.lift((state: MountState) => renderState(state, exchange))
                case Present(hold) =>
                    // Unsafe: whether this mount has told the boundary it loaded, made before its loop is forked, where no proof is at hand;
                    // read and written only inside Sync.Unsafe.defer below.
                    val loaded = AtomicBoolean.Unsafe.init(first.settled)(using AllowUnsafe.embrace.danger)
                    for
                        registered <- if first.settled then Kyo.lift(Maybe.empty[BoundaryHold]) else hold.load.map(Present(_))
                        held       <- throughHold[MountState](hold, rui)(_.content(spec)) { (state, group) =>
                            renderState(state, group).andThen(Sync.Unsafe.defer(
                                if state.settled && loaded.compareAndSet(false, true) then registered.foreach(_.settle)
                            ))
                        }
                        _ <- Scope.ensure(Sync.Unsafe.defer(if loaded.compareAndSet(false, true) then registered.foreach(_.settle)))
                    yield held
                    end for
            _ <- startOwnedFiber(
                Abort.run[Throwable](instance.cell.observeStamped(watch.stamp)(observed)).map { result =>
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
    private[internal] def showingSeeds(ui: UI, path: Seq[String], seeds: Map[Seq[String], UI]): UI =
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
    private[internal] def showsSnapshot(state: MountState): Boolean =
        state match
            case MountState.Pending(Present(_)) => true
            case _                              => false

end MountRegions
