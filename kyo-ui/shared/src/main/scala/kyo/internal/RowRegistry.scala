package kyo.internal

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
    val finalizer: Scope.Finalizer
)

/** Per-region ownership of reusable Foreach rows, in list order. Mutation happens only from the owning
  * region's sequential observe loop, so plain get/set on the ref suffices.
  *
  * A row is the registry's from the moment it starts subscribing, not from [[replaceAll]]: its scope is a root nothing else
  * closes, so an emission interrupted in between would otherwise leave what the row had already subscribed running.
  */
final private[kyo] class RowRegistry(
    rows: AtomicRef[Vector[RowInstance]],
    building: AtomicRef[List[Scope.Finalizer]]
):
    def snapshot(using Frame): Vector[RowInstance] < Sync = rows.get

    def replaceAll(ordered: Seq[RowInstance])(using Frame): Unit < Sync =
        rows.set(Vector.from(ordered)).andThen(building.set(Nil))

    /** Take a row that is about to subscribe; [[replaceAll]] installs it, or [[evictAll]] closes it. */
    def track(fin: Scope.Finalizer)(using Frame): Unit < Sync = building.getAndUpdate(fin :: _).unit

    /** Close-and-await every row whose key is not in `keep`: run before the new value paints, so a
      * vanished or changed row's observers are fully released while the old content is still on screen
      * (break-before-make, the region-level closed-and-awaited guarantee extended to rows).
      */
    def evictExcept(keep: Set[String])(using Frame): Unit < Async =
        for
            old <- rows.getAndUpdate(_.filter(r => keep.contains(r.key)))
            _   <- Kyo.foreachDiscard(old.filterNot(r => keep.contains(r.key)))(r =>
                r.finalizer.close(Absent).andThen(r.finalizer.await)
            )
        yield ()

    def evictAll(using Frame): Unit < Async =
        evictExcept(Set.empty).andThen(
            building.getAndSet(Nil).map(fins => Kyo.foreachDiscard(fins)(fin => fin.close(Absent).andThen(fin.await)))
        )
end RowRegistry

private[kyo] object RowRegistry:
    def init(using Frame): RowRegistry < Sync =
        for
            rows     <- AtomicRef.init(Vector.empty[RowInstance])
            building <- AtomicRef.init(List.empty[Scope.Finalizer])
        yield new RowRegistry(rows, building)
end RowRegistry

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
      * The mounts of every row, re-walked or retained, are one pass of the region's mount table, so a retained row's keyed
      * instances are kept. A row's key is part of every mount identity in it, so no claim in one row can collide with another's.
      */
    private[internal] def subscribeForeachRegion(
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
                        // fiber (initInline) instead of a fiber per row.
                        def rowRunner(kids: Seq[ReactiveUI], pass: RegionKeys): Scope.Finalizer < Async =
                            Sync.Unsafe.defer {
                                val fin = Scope.Finalizer.Unsafe.initInline()
                                // A root, as `Scope.runUnowned`'s is: the registry owns the row, not the enclosing scope.
                                rows.track(fin).andThen(ContextEffect.handle[Scope.Finalizer, Scope, Unit, Async](
                                    Tag[Scope],
                                    derive = (_: Maybe[Scope.Finalizer]) => fin,
                                    fork = (parent: Scope.Finalizer) => parent.forked,
                                    join = (parent: Scope.Finalizer, _: Scope.Finalizer, _: Scope.Finalizer) => parent
                                ) {
                                    Kyo.foreachDiscard(kids)(subscribeScoped(_, exchange, signalChangeTime, regionMounts, dispatch, pass))
                                }).andThen(fin)
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
                                    rowRunner(kids, pass).map(fin => new RowInstance(key, item, renderFn(i, item), kids, Absent, fin))
                                }
                                _ <- regionMounts.endWalk
                                // No paint here: the enclosing render already put exactly this fragment in the
                                // DOM. It is still recorded as this region's last render, so the NEXT emission
                                // diffs against it rather than re-sending every row.
                                _ <- Sync.Unsafe.defer(
                                    rendered.set(Present(Fragment[UI](Chunk.from(built.toSeq.map(r => KeyedChild[UI](r.key, r.rowUI))))))
                                )
                                _ <- rows.replaceAll(built.toSeq)
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
                                _     <- rows.evictExcept(retainedKeys) // removed AND changed rows close before the paint
                                built <- Kyo.foreach(keyed) { (key, item, i) =>
                                    prevByKey.get(key).filter(_ => retainedKeys.contains(key)) match
                                        case Some(inst) => Kyo.lift((key, item, inst.rowUI, inst.kids, inst.handler, Present(inst)))
                                        case None       =>
                                            val rowUI = renderFn(i, item)
                                            walkRow(
                                                rowUI,
                                                path :+ key,
                                                svg,
                                                rui.contentContext.child(key),
                                                nestedParentContext(rui.parentContext, rowUI),
                                                dispatch
                                            ).map((kids, hdl) => (key, item, rowUI, kids, Present(hdl), Absent: Maybe[RowInstance]))
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
                                kept <-
                                    if duplicates then
                                        exchange.onChange(rui.region, path, rui.contentContext, rui.parentContext, previous, fragment)
                                            .andThen(true)
                                    else
                                        exchange.onListPatch(
                                            rui.region,
                                            path,
                                            rui.contentContext,
                                            rui.parentContext,
                                            previous,
                                            built.toSeq.map((key, _, rowUI, _, _, retained) => ListRow(key, rowUI, retained.isEmpty))
                                        )
                                // A repaint of the kept rows replaced what their subscriptions had painted into them, a
                                // mount's content among it: subscribe them again, as a whole-list repaint always did.
                                _         <- if kept then Kyo.unit else rows.evictAll
                                finalRows <- Kyo.foreach(built) { (key, item, rowUI, kids, hdl, retained) =>
                                    retained match
                                        case Present(inst) if kept => Kyo.lift(inst)
                                        case _                     =>
                                            rowRunner(kids, pass).map(fin => new RowInstance(key, item, rowUI, kids, hdl, fin))
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

end RowRegions
