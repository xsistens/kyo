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

/** A `Mounted` node as normalization found it: the node, its render path, and the keys of the keyed list rows between it and its
  * enclosing reactive region, innermost first.
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

    def mountError(t: Throwable, source: UI.MountErrorSource): UI.MountError =
        UI.MountError(t, source, Maybe(t.getMessage).getOrElse(t.toString), node.frame, path, node.key)

    /** The instance identity of a keyed node; a keyless node has none, since every re-render replaces its instance. */
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

/** The keys of the regions among one render pass's reactive children, computed on first use, which only a pass with mounts at or
  * below it ever makes.
  */
final private[kyo] class RegionKeys(kids: Seq[ReactiveUI]):
    import RegionKeys.passMembers
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

    /** The nodes one render pass of a region owns: its reactive children, and through a constant child (an element that only
      * carries reactive attributes) that child's own, which the constant node subscribes in the same pass and the same table.
      */
    private[internal] def passMembers(kids: Seq[ReactiveUI]): Iterator[ReactiveUI] =
        kids.iterator.flatMap(kid => if kid.isConst && kid.mounted.isEmpty then passMembers(kid.children) else Iterator.single(kid))
    val empty: RegionKeys = new RegionKeys(Seq.empty)

    def apply(kids: Seq[ReactiveUI]): RegionKeys = new RegionKeys(kids)
end RegionKeys

/** A live mount: its content cell, the fiber running its effect, the `Scope` that owns what the effect acquired, and the table of
  * keyed mounts nested in its content.
  */
final private[kyo] class MountInstance(
    val cell: SignalRef[MountState],
    val children: MountTableRef,
    val supervision: MountSupervision,
    runner: Fiber[Unit, Any],
    scope: Scope.Finalizer
):
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
    def start(spec: MountedSpec, seed: Maybe[UI], views: List[PartialFunction[UI.MountError, UI]])(using
        Frame
    ): MountInstance < Sync =
        for
            cell <- Signal.initRef[MountState](MountState.Pending(seed))
            supervision = MountSupervision(spec, cell, views)
            scope  <- Scope.Finalizer.init(1)
            runner <- Fiber.initUnscoped(runEffect(spec, supervision, scope))
        yield new MountInstance(cell, MountTableRef.init(views), supervision, runner, scope)

    /** Runs the effect with `scope` as its `Scope`, so what it acquires outlives this fiber, and publishes the outcome. */
    private def runEffect(spec: MountedSpec, supervision: MountSupervision, scope: Scope.Finalizer)(using Frame): Unit < Async =
        val effect = MountSupervision.local.let(Present(supervision))(spec.node.effect)
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
  * failure does not overwrite it.
  */
final private[kyo] class MountSupervision(
    spec: MountedSpec,
    cell: SignalRef[MountState],
    views: List[PartialFunction[UI.MountError, UI]]
):

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

    def fail(t: Throwable, source: UI.MountErrorSource)(using Frame): Unit < Sync =
        val failed = MountState.Failed(spec.errorUI(spec.mountError(t, source), views))
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
    kid: Maybe[ReactiveUI],
    val views: List[PartialFunction[UI.MountError, UI]]
):

    /** Creation order: a later subscription of a region gets a larger serial, whichever binds the table first. */
    val serial: Long = MountTableRef.serials.incrementAndGet()

    /** The reference for the region `region` of the render pass `pass` inside this one, holding the table an earlier subscription
      * of that region created. The region's key is only computed when this table exists, or once the region forces its own. A
      * boundary's error view comes first for the mounts below it.
      */
    def child(pass: RegionKeys, region: ReactiveUI)(using AllowUnsafe): MountTableRef =
        val existing = slot.get().flatMap(table => table.region(pass.of(region)))
        val inner    = region.bounded.flatMap(_.settings.errorView).fold(views)(_ :: views)
        val ref      = new MountTableRef(AtomicRef.Unsafe.init(existing), Present(this), pass, Present(region), inner)
        existing.foreach(_.bind(ref))
        ref
    end child

    /** Leaves what a boundary at `path` showed when it ended, for a successor this pass starts there. */
    def leave(path: Seq[String], ui: UI)(using Frame): Unit < Sync =
        force.map(table => Sync.Unsafe.defer(if table.ownedBy(this) then table.leave(path, ui)))

    /** What a boundary that ended at `path` left there in this pass. */
    def left(path: Seq[String])(using Frame): Maybe[UI] < Sync =
        Sync.Unsafe.defer(owned.flatMap(_.left(path)))

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

    private def owned(using AllowUnsafe): Maybe[MountTable] = slot.get().filter(_.ownedBy(this))

    /** Opens a render pass of the region: tears down, and waits for, every keyed instance the new tree no longer shows, before any
      * new instance starts.
      */
    def beginWalk(kids: Seq[ReactiveUI], pass: RegionKeys)(using Frame): Unit < Async =
        Sync.Unsafe.defer(owned).map {
            case Absent         => Kyo.unit
            case Present(table) => table.evictExcept(kids, pass)
        }

    /** [[beginWalk]] in two steps: the slots and the snapshots now, the release of what the pass no longer shows when the caller
      * runs the answer, which it does before any new instance starts.
      */
    def openWalk(kids: Seq[ReactiveUI], pass: RegionKeys)(using Frame): (() => Unit < Async) < Sync =
        Sync.Unsafe.defer(owned).map {
            case Absent         => Kyo.lift(MountTable.nothingToRelease)
            case Present(table) => table.openWalk(kids, pass)
        }

    /** `ui` as this pass paints it, rooted at `path`: a mount among `kids` that starts from the snapshot of the instance before it
      * shows that snapshot in its slot already, as the instance will once it starts, rather than an empty slot until then.
      */
    def withSeeds(ui: UI, path: Seq[String], kids: Seq[ReactiveUI])(using Frame): UI < Sync =
        Sync.Unsafe.defer {
            owned match
                case Absent         => ui
                case Present(table) =>
                    val seeds = kids.foldLeft(Map.empty[Seq[String], UI]) { (acc, kid) =>
                        kid.mounted.flatMap(spec => table.seedFor(kid, spec)).fold(acc)(seed => acc.updated(kid.path, seed))
                    }
                    if seeds.isEmpty then ui else showingSeeds(ui, path, seeds)
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
            Sync.Unsafe.defer {
                if table.ownedBy(this) then table.claim(rui, identity, spec, views)
                else
                    table.live(identity, spec.path) match
                        case Present(instance) => Kyo.lift(instance)
                        case Absent            => MountInstance.start(spec, Absent, views).map(i => Scope.ensure(i.teardown).andThen(i))
            }
        }

    /** A keyless instance for `spec`, released with the caller's scope; only the reference the table follows leaves its snapshot. */
    def startKeyless(spec: MountedSpec)(using Frame): MountInstance < (Async & Scope) =
        force.map { table =>
            table.start(spec, views).map { instance =>
                Scope.ensure(Sync.Unsafe.defer(table.ownedBy(this)).map(owns =>
                    if owns then table.retire(instance, spec.path) else instance.teardown
                )).andThen(instance)
            }
        }
end MountTableRef

private[kyo] object MountTableRef:
    private val serials = new java.util.concurrent.atomic.AtomicLong()

    /** A reference no enclosing table owns: the root of a session, or the content of a mount instance. */
    def init(views: List[PartialFunction[UI.MountError, UI]] = Nil): MountTableRef =
        new MountTableRef(
            AtomicRef.Unsafe.init(Maybe.empty[MountTable])(using AllowUnsafe.embrace.danger),
            Absent,
            RegionKeys.empty,
            Absent,
            views
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

    // Unsafe: a table is made with `new` by the reference that creates it, which holds no proof to pass on to the constructor (a proof
    // there would reach every method). The fields are only read and written by methods that hold one.
    private val instances =
        AtomicRef.Unsafe.init(Dict.empty[MountIdentity, Chunk[MountSlot]])(using AllowUnsafe.embrace.danger)
    private val regions  = AtomicRef.Unsafe.init(Dict.empty[RegionKey, MountTable])(using AllowUnsafe.embrace.danger)
    private val assigned =
        AtomicRef.Unsafe.init(Maybe.empty[java.util.IdentityHashMap[ReactiveUI, MountSlot]])(using AllowUnsafe.embrace.danger)
    private val handoff = AtomicRef.Unsafe.init(Dict.empty[Seq[String], UI])(using AllowUnsafe.embrace.danger)
    private val owner   = AtomicRef.Unsafe.init(Maybe.empty[MountTableRef])(using AllowUnsafe.embrace.danger)

    /** Makes `ref` the reference this table follows, unless a newer one already is. */
    def bind(ref: MountTableRef)(using AllowUnsafe): Unit =
        discard(owner.getAndUpdate(current => if current.exists(_.serial > ref.serial) then current else Present(ref)))

    def ownedBy(ref: MountTableRef)(using AllowUnsafe): Boolean = owner.get().exists(_ eq ref)

    /** The running instance of `identity`, the one last shown at `path` when there are several. */
    def live(identity: MountIdentity, path: Seq[String])(using AllowUnsafe): Maybe[MountInstance] =
        instances.get().get(identity).flatMap { slots =>
            Maybe.fromOption(slots.find(slot => slot.instance.nonEmpty && slot.path == path).orElse(slots.find(_.instance.nonEmpty)))
                .flatMap(_.instance)
        }

    def region(key: RegionKey)(using AllowUnsafe): Maybe[MountTable] = regions.get().get(key)

    /** The table registered for the region `key`, which becomes `fresh` when there is none yet. */
    def adoptRegion(key: RegionKey, fresh: MountTable)(using AllowUnsafe): MountTable =
        regions.getAndUpdate(m => if m.contains(key) then m else m.update(key, fresh)).get(key).getOrElse(fresh)

    /** The instance of the slot this pass gave `rui`, started if the slot is empty. A node no pass has seen belongs to a table the
      * pass created, which holds nothing yet, so it starts a new instance.
      */
    def claim(rui: ReactiveUI, identity: MountIdentity, spec: MountedSpec, views: List[PartialFunction[UI.MountError, UI]])(using
        Frame
    ): MountInstance < Sync =
        Sync.Unsafe.defer(assigned.get().flatMap(slots => Maybe(slots.get(rui)))).map {
            case Present(slot) =>
                slot.instance match
                    case Present(instance) => Kyo.lift(instance)
                    case Absent            =>
                        start(spec, views).map(instance => Sync.Unsafe.defer { slot.instance = Present(instance); instance })
            case Absent =>
                start(spec, views).map { instance =>
                    Sync.Unsafe.defer {
                        val slot = new MountSlot(spec.path)
                        slot.instance = Present(instance)
                        discard(instances.getAndUpdate(m => m.update(identity, m.get(identity).getOrElse(Chunk.empty).append(slot))))
                        instance
                    }
                }
        }

    /** A new instance, seeded with the snapshot the slot's previous instance left when the node keeps the latest content. */
    def start(spec: MountedSpec, views: List[PartialFunction[UI.MountError, UI]])(using Frame): MountInstance < Sync =
        Sync.Unsafe.defer {
            spec.node.pendingPolicy match
                case UI.PendingPolicy.Placeholder => Absent
                case UI.PendingPolicy.KeepLatest  => handoff.get().get(spec.path)
        }.map(MountInstance.start(spec, _, views))

    def leave(path: Seq[String], ui: UI)(using AllowUnsafe): Unit = discard(handoff.getAndUpdate(_.update(path, ui)))

    def left(path: Seq[String])(using AllowUnsafe): Maybe[UI] = handoff.get().get(path)

    /** Tears down `instance` and leaves its snapshot at `path` for this pass's successor. */
    def retire(instance: MountInstance, path: Seq[String])(using Frame): Unit < Async =
        leaveSnapshot(instance, path).andThen(instance.teardown)

    private def leaveSnapshot(instance: MountInstance, path: Seq[String])(using Frame): Unit < Sync =
        instance.snapshot.map(snapshot => Sync.Unsafe.defer(snapshot.foreach(ui => discard(handoff.getAndUpdate(_.update(path, ui))))))

    /** Opens a render pass over `kids`: releases every keyed instance and every region table the pass no longer shows, and waits
      * for all of it.
      */
    def evictExcept(kids: Seq[ReactiveUI], pass: RegionKeys)(using Frame): Unit < Async =
        openWalk(kids, pass).map(release => release())

    /** Opens a render pass over `kids`: gives each keyed node its slot, and leaves the snapshot of every keyed instance the pass no
      * longer shows for a `KeepLatest` successor at its place, so the paint of the pass can already show it. Answers the release
      * of those instances and of the region tables the pass no longer shows, which the caller runs, and waits for, before any new
      * instance starts. The instances leave the table before they are released, and the release runs once and out of reach of an
      * interrupt: an outer re-render that interrupts this region's loop ends the wait, not the release.
      */
    def openWalk(kids: Seq[ReactiveUI], pass: RegionKeys)(using Frame): (() => Unit < Async) < Sync =
        Sync.Unsafe.defer {
            // The pass's keyed nodes by identity, each group in walk order.
            val groups = new java.util.HashMap[MountIdentity, java.util.ArrayList[ReactiveUI]]()
            RegionKeys.passMembers(kids).foreach { kid =>
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
            if goneRegions.isEmpty && goneSlots.isEmpty then Kyo.lift(MountTable.nothingToRelease)
            else
                Kyo.foreachDiscard(goneSlots)(slot => slot.instance.fold(Kyo.unit)(leaveSnapshot(_, slot.path))).andThen {
                    val released = AtomicBoolean.Unsafe.init(false)
                    () =>
                        Sync.Unsafe.defer {
                            if !released.compareAndSet(false, true) then Kyo.unit
                            else
                                Async.uninterruptible {
                                    Kyo.foreachDiscard(goneRegions)(_.evictAll)
                                        .andThen(Kyo.foreachDiscard(goneSlots)(_.instance.fold(Kyo.unit)(_.teardown)))
                                }
                        }
                }
            end if
        }
    end openWalk

    /** The snapshot a node starting a new instance shows until that instance publishes: what the instance before it at its place
      * left, when the node keeps the latest content and this pass gives it no running instance.
      */
    def seedFor(rui: ReactiveUI, spec: MountedSpec)(using AllowUnsafe): Maybe[UI] =
        spec.node.pendingPolicy match
            case UI.PendingPolicy.Placeholder => Absent
            case UI.PendingPolicy.KeepLatest  =>
                val starts = spec.identity.isEmpty || assigned.get().flatMap(slots => Maybe(slots.get(rui))).forall(_.instance.isEmpty)
                if starts then handoff.get().get(spec.path) else Absent

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
    def endWalk()(using AllowUnsafe): Unit =
        handoff.set(Dict.empty)
end MountTable

private[kyo] object MountTable:
    val nothingToRelease: () => Unit < Async = () => Kyo.unit
end MountTable

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

    /** Subscribes a mounted node in the current scope. A keyed node adopts or creates its instance in the enclosing region's table; a
      * keyless node gets a new instance that ends with the current scope, which for a node inside a region is the region's current
      * value.
      */
    private[internal] def subscribeMounted(
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
            _     <- dispatch.register(rui.contentContext, instance.cell)
            _     <- Scope.ensure(dispatch.unregister(rui.contentContext, instance.cell))
            first <- instance.cell.current
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
                val content = state.content(spec)
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
                        case MountState.Pending(Present(_)) => Kyo.unit
                        case _                              =>
                            for
                                (kids, _) <- SignalNow.Locals.take(Sync.Unsafe.defer(walkStatic(
                                    content,
                                    rui.path,
                                    svgContext,
                                    rui.contentContext,
                                    nestedParentContext(rui.parentContext, content),
                                    dispatch,
                                    rows = 0L,
                                    deepRows = Nil
                                )))
                                keys = RegionKeys(kids)
                                _ <- instance.children.beginWalk(kids, keys)
                                _ <- forkRootChannels(rui.path, content, through)
                                _ <- subscribeWalked(kids, keys, instance.children, through, signalChangeTime, dispatch)
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
                Abort.run[Throwable](instance.cell.observe(observed)).map { result =>
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
