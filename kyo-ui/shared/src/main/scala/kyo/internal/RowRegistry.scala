package kyo.internal

import DragSessions.*
import EventDispatch.*
import ReactiveUI.*
import kyo.*
import kyo.UI.*
import kyo.UI.Ast.*
import kyo.kernel.ContextEffect

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
    val scope: RowScope,
    val holdsMount: Boolean
)

/** What keeps one keyed row's subscriptions open, closed by the registry when the row is evicted.
  *
  * A plain keyed row binds a class channel and a text: two callbacks on their signals' promises, each released by one call. On a
  * Scope finalizer each would be an effect of its own, an `Abort` handler around a deferred call, run by the finalizer's close and
  * await. So bare releases collect here and run in one pass; the finalizer keeps only what is not a bare release (a nested
  * region's fiber, a mount), which for a plain row is nothing. `group` is the row's release group, marked before any of it is
  * released.
  */
final private[kyo] class RowScope(val finalizer: Scope.Finalizer, val group: ReleaseGroup):
    private[internal] var releases: List[() => Unit] = Nil

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
  * happens only from the owning region's sequential observe loop, and from [[evictAll]] once that loop has ended.
  *
  * The key index is maintained incrementally (evicted keys leave it, fresh instances join it), so
  * neither an emission nor an event lookup rebuilds a map over every live row.
  *
  * A row is the registry's from the moment it starts subscribing, not from [[replaceAll]]: its scope is a root nothing else
  * closes, so an emission interrupted in between would otherwise leave what the row had already subscribed running. It stays
  * the registry's until its release has completed, for the same reason: an emission evicting rows is interrupted when the region
  * around the list closes, and the rows it had taken out of the list but not yet closed would be left to nobody.
  */
final private[kyo] class RowRegistry(
    rows: AtomicRef[Vector[RowInstance]],
    index: AtomicRef[Map[String, RowInstance]],
    building: AtomicRef[List[RowScope]],
    releasing: AtomicRef[List[Seq[RowScope]]]
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
        Sync.Unsafe.defer {
            val old  = rows.unsafe.getAndUpdate(_.filter(r => keep(r.key)))
            val gone = old.filterNot(r => keep(r.key))
            if gone.isEmpty then Kyo.unit
            else
                // A cleared list drops the whole index at once rather than one key at a time.
                if gone.size == old.size then index.unsafe.set(Map.empty)
                else discard(index.unsafe.getAndUpdate(_ -- gone.map(_.key)))
                val scopes   = gone.map(_.scope)
                val failures = take(scopes)
                Async.uninterruptible(logFailures(failures).andThen(settle(scopes)))
            end if
        }

    /** Closes every row: the listed ones, those still subscribing, and those an emission is releasing, whose release it waits for. */
    def evictAll(using Frame): Unit < Async =
        Sync.Unsafe.defer {
            val scopes   = rows.unsafe.getAndSet(Vector.empty).map(_.scope) ++ building.unsafe.getAndSet(Nil)
            val failures =
                if scopes.isEmpty then Seq.empty
                else
                    index.unsafe.set(Map.empty)
                    take(scopes)
            val batches = releasing.unsafe.get()
            if batches.isEmpty then Kyo.unit
            else Async.uninterruptible(logFailures(failures).andThen(Kyo.foreachDiscard(batches)(settle)))
        }

    /** Moves `scopes` from the list to the rows being released, marks their groups, and runs their channel releases, in the step that
      * took them out of the list: no interrupt falls between taking a row and owning its release. Returns the failed releases.
      */
    private[internal] def take(scopes: Seq[RowScope])(using AllowUnsafe): Seq[Throwable] =
        discard(releasing.unsafe.getAndUpdate(scopes :: _))
        scopes.foreach(_.group.release())
        scopes.flatMap(_.releaseAll())
    end take

    private def logFailures(failures: Seq[Throwable])(using Frame): Unit < Async =
        Kyo.foreachDiscard(failures)(t => Log.error("kyo-ui: a row's release failed", t))

    /** Closes and awaits the finalizers of one batch of rows, which for a plain row hold nothing and complete without suspending
      * (initInline). A finalizer another fiber is already closing is awaited, so [[evictAll]] waits for an emission's batch.
      */
    private def settle(scopes: Seq[RowScope])(using Frame): Unit < Async =
        Kyo.foreachDiscard(scopes)(s => s.finalizer.close(Absent).andThen(s.finalizer.await))
            .andThen(releasing.getAndUpdate(_.filterNot(_ eq scopes)).unit)
end RowRegistry

private[kyo] object RowRegistry:
    /** The registry for a pass that already holds the thread (see [[SignalNow]]). */
    def initNow()(using AllowUnsafe): RowRegistry =
        new RowRegistry(
            AtomicRef.Unsafe.init(Vector.empty[RowInstance]).safe,
            AtomicRef.Unsafe.init(Map.empty[String, RowInstance]).safe,
            AtomicRef.Unsafe.init(List.empty[RowScope]).safe,
            AtomicRef.Unsafe.init(List.empty[Seq[RowScope]]).safe
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

    /** The reactive kids of every row, the region's walk for this emission. Built only when the region's pass plans. */
    lazy val allKids: Seq[ReactiveUI] = built.toSeq.flatMap(_.kids)

    /** Whether a mount is in any row's walk, from the flag each row took when it was walked, without collecting the walk. */
    def holdsMount: Boolean =
        var i = 0
        while i < size && !built(i).holdsMount do i += 1
        i < size
    end holdsMount

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
        val retained: Maybe[RowInstance],
        val holdsMount: Boolean
    )

    /** Whether a mount is at or below `kids`, a row's walk: fixed by the walk, so a row takes it once and keeps it. */
    def holdsMount(kids: Seq[ReactiveUI]): Boolean = kids.exists(_.containsMount)

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
        val seen      = presized(size)
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
        val retainedKeys = presized(keptCount)
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
                built(i) = new Built(inst.key, inst.item, inst.rowUI, inst.kids, inst.handler, Present(inst), inst.holdsMount)
            end if
            i += 1
        end while
        new RowPlan(items, keys, dup, built, kept, Chunk.from(rebuilt), retainedKeys, prev)
    end decide

    /** A set that holds `n` keys without growing. */
    private def presized(n: Int): scala.collection.mutable.HashSet[String] =
        val loadFactor = scala.collection.mutable.HashSet.defaultLoadFactor
        new scala.collection.mutable.HashSet[String](math.max(16, (n / loadFactor).toInt + 1), loadFactor)
end RowPlan

private[kyo] object RowRegions:

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
    private[internal] def subscribeForeachRegion(
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
                    // Rows outlive the emission that made them, so each is a group inside the list's, not inside an emission's.
                    around <- ReleaseGroup.current.use(group => group.getOrElse(regionMounts.table.group))
                    _      <- Scope.ensure(rows.evictAll)
                    watch  <- regionMounts.register
                    _      <- regionFiber(rui.path, "Reactive subscription fiber") {
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
                                val scope = new RowScope(fin, around.child())
                                // A root, as `Scope.runUnowned`'s is: the registry owns the row, not the enclosing scope.
                                rows.track(scope).andThen(ContextEffect.handle[Scope.Finalizer, Scope, Unit, Async](
                                    Tag[Scope],
                                    derive = (_: Maybe[Scope.Finalizer]) => fin,
                                    fork = (parent: Scope.Finalizer) => parent.forked,
                                    join = (parent: Scope.Finalizer, _: Scope.Finalizer, _: Scope.Finalizer) => parent
                                ) {
                                    ReleaseGroup.current.let(Present(scope.group))(Kyo.foreachDiscard(kids)(
                                        subscribeScoped(_, exchange, signalChangeTime, regionMounts, dispatch, pass, Present(scope))
                                    ))
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
                                    rowRunner(kids, pass).map(scope =>
                                        new RowInstance(key, item, renderFn(i, item), kids, Absent, scope, RowPlan.holdsMount(kids))
                                    )
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
                                        plan.built(i) =
                                            new RowPlan.Built(key, item, rowUI, kids, Present(hdl), Absent, RowPlan.holdsMount(kids))
                                    }
                                })
                                // The regions inside a retained row keep their subscriptions, so the pass leaves them as they are.
                                firstPass <- regionMounts.beginWalk(plan.allKids, plan.retainedRows, Present(plan.holdsMount))
                                // What the old value of a rebuilt row drew is gone with it, and a place an instance keeps is left to it.
                                _ <- Kyo.foreachDiscard(plan.rebuilt)(i => dispatch.forgetBelow(rui.contentContext.child(plan.keys(i))))
                                _ <- Sync.Unsafe.defer(plan.rebuilt.foreach(i => regionMounts.noteLive(plan.built(i).kids, dispatch)))
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
                                    else
                                        rows.evictAll.andThen(Sync.defer(plan.rebuildAll())).andThen(regionMounts.beginWalk(
                                            plan.allKids,
                                            holdsMount = Present(plan.holdsMount)
                                        ))
                                _ <- Kyo.foreachDiscard(plan.rebuilt) { i =>
                                    val b = plan.built(i)
                                    rowRunner(b.kids, pass).map(scope =>
                                        plan.finalRows(i) = new RowInstance(b.key, b.item, b.rowUI, b.kids, b.handler, scope, b.holdsMount)
                                    )
                                }
                                _ <- regionMounts.endWalk
                                _ <- rows.replaceAll(plan.finalRowsSeq, plan.freshRows)
                            yield ()

                        var first = true
                        itemSignal.observeStamped(watch.stamp) {
                            items =>
                                val isFirst = first
                                first = false
                                if isFirst && spec.renderedItems.exists(_.equals(items)) && distinctKeys(items) then seed(items)
                                else handle(items)
                        }
                    }
                yield ()
                end for
        }
    end subscribeForeachRegion

end RowRegions
