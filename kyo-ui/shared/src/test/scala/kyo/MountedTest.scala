package kyo

import kyo.internal.HtmlRenderer
import kyo.internal.MountDispatch
import kyo.internal.MountedSpec
import kyo.internal.MouseEventData
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.UIEvent
import kyo.internal.UIExchange

/** Behavior of [[kyo.UI.mounted]] on the shared engine, without a browser: a tree is normalized and subscribed against an exchange that
  * records every paint, and events go straight to the subscription's handler.
  *
  * What a path shows is the last paint at that path or at one of its ancestors, since an ancestor's paint replaces everything below it.
  */
class MountedTest extends kyo.test.Test[Any]:

    import UI.*

    /** @param keepsRows
      *   answers a list patch the way the DOM backend does: only the changed rows are painted, the kept ones stay in place
      */
    private class Recorder(keepsRows: Boolean = false) extends UIExchange:
        private val paints  = AtomicRef.Unsafe.init(Chunk.empty[(Seq[String], String)])(using AllowUnsafe.embrace.danger)
        private val holdLog = AtomicRef.Unsafe.init(Chunk.empty[(ReactiveRegion, Boolean)])(using AllowUnsafe.embrace.danger)

        override def onHold(region: ReactiveRegion, held: Boolean)(using Frame): Unit < Async =
            Sync.Unsafe.defer(discard(holdLog.getAndUpdate(_.append((region, held)))))

        /** Every hold and release so far, oldest first. */
        def holds(using Frame): Chunk[(ReactiveRegion, Boolean)] < Sync = Sync.Unsafe.defer(holdLog.get())

        override def onListPatch(
            region: ReactiveRegion,
            path: Seq[String],
            contentContext: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            rows: Seq[kyo.internal.ListRow]
        )(using Frame): Boolean < Async =
            if !keepsRows then super.onListPatch(region, path, contentContext, parentContext, previous, rows)
            else
                Kyo.foreachDiscard(rows.filter(_.changed)) { row =>
                    HtmlRenderer.render(row.ui, path :+ row.key).map(html =>
                        Sync.Unsafe.defer(discard(paints.getAndUpdate(_.append((path :+ row.key, html)))))
                    )
                }.andThen(true)

        def onChange(
            region: ReactiveRegion,
            path: Seq[String],
            context: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            ui: UI
        )(using Frame): Unit < Async =
            HtmlRenderer.render(ui, path).map(html => Sync.Unsafe.defer(discard(paints.getAndUpdate(_.append((path, html))))))

        /** The initial page, which a transport renders itself before subscribing. */
        def initial(ui: UI)(using Frame): Unit < Sync =
            HtmlRenderer.render(ui, Seq.empty).map(html => Sync.Unsafe.defer(discard(paints.getAndUpdate(_.append((Seq.empty, html))))))

        def shown(path: Seq[String])(using Frame): String < Sync =
            Sync.Unsafe.defer(paints.get()).map { all =>
                all.reverseIterator.find((p, _) => path.startsWith(p)).map(_._2).getOrElse("")
            }

        def paintsAt(path: Seq[String])(using Frame): Int < Sync =
            Sync.Unsafe.defer(paints.get()).map(_.count((p, _) => p == path))

        def paintCount(using Frame): Int < Sync = Sync.Unsafe.defer(paints.get().size)

        /** Every paint after the first `n`, oldest first. */
        def paintsAfter(n: Int)(using Frame): Chunk[(Seq[String], String)] < Sync = Sync.Unsafe.defer(paints.get().drop(n))

        def paintsEndingWith(suffix: Seq[String])(using Frame): Int < Sync =
            Sync.Unsafe.defer(paints.get()).map(_.count((p, _) => p.endsWith(suffix)))
    end Recorder

    private def click(path: String*): UIEvent = UIEvent.Click(path, MouseEventData(UI.Modifiers.none, Absent))

    /** Subscribes `root` with its mount tables checking every state they take against their invariants (see `ReactiveUI.MountAudit`). */
    private def subscribeAudited(root: ReactiveUI, exchange: UIExchange)(using Frame): ReactiveUI.Subscription < (Async & Scope) =
        ReactiveUI.MountSession.audited.let(true)(ReactiveUI.subscribe(root, exchange))

    /** Asserts that the mount tables of `sub` checked their states and found every one consistent. */
    private def assertConsistent(sub: ReactiveUI.Subscription)(using Frame, kyo.test.AssertScope): Unit < Sync =
        Sync.defer {
            val audit = sub.mountSession.audit
            assert(audit.nonEmpty, "the subscription was not audited")
            val found = audit.get.inconsistencies
            assert(audit.get.checked.get > 0 && found.isEmpty, s"mount table states against the invariants: ${found.mkString("; ")}")
        }

    private def mount[A](ui: UI, keepsRows: Boolean = false)(
        f: (Recorder, (Seq[String], UIEvent) => Boolean < Async) => A < (Async & Scope)
    )(using Frame, kyo.test.AssertScope): A < Async =
        mountSession(ui, keepsRows)((recorder, sub) => f(recorder, sub.handle))

    /** [[mount]] with the whole subscription. Once the session has ended, no verdict on an interrupted effect waits any more. */
    private def mountSession[A](ui: UI, keepsRows: Boolean = false)(
        f: (Recorder, ReactiveUI.Subscription) => A < (Async & Scope)
    )(using Frame, kyo.test.AssertScope): A < Async =
        Scope.run {
            val recorder = new Recorder(keepsRows)
            for
                _    <- recorder.initial(ui)
                root <- ReactiveUI.normalize(ui, Seq.empty)
                sub  <- subscribeAudited(root, recorder)
                a    <- f(recorder, sub)
                _    <- assertConsistent(sub)
            yield (a, sub.mountSession)
            end for
        }.map((a, session) => assertEventually(pendingVerdicts(session).map(_ == 0)).andThen(a))

    /** The verdicts on interrupted effects that wait for a cut in `session` now. */
    private def pendingVerdicts(session: ReactiveUI.MountSession)(using Frame): Long < Sync =
        Sync.defer(session.counters.pendingVerdicts.get)

    private def showsEventually(recorder: Recorder, path: Seq[String], text: String)(using Frame, kyo.test.AssertScope): Unit < Async =
        assertEventually(recorder.shown(path).map(_.contains(text)))

    "a mounted node shows its placeholder until the effect publishes, then the content" in {
        for
            gate <- Promise.init[Unit, Any]
            ui = div(UI.mounted(gate.get.andThen(span("ready"))).placeholder(span("loading")))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0"), "loading")
                    _ <- gate.completeUnitDiscard
                    _ <- showsEventually(recorder, Seq("0"), "ready")
                yield ()
            }
        yield succeed
    }

    "a keyless node gets a new instance whenever its region re-renders" in {
        for
            runs <- AtomicInt.init(0)
            tick <- Signal.initRef(0)
            ui = div(tick.render(_ => div(UI.mounted(runs.incrementAndGet.map(n => span(s"runs:$n"))))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0"), "runs:1")
                    _ <- tick.set(1)
                    _ <- showsEventually(recorder, Seq("0", "0"), "runs:2")
                yield ()
            }
        yield succeed
    }

    "a keyed node keeps its instance while its region re-renders" in {
        for
            runs <- AtomicInt.init(0)
            tick <- Signal.initRef(0)
            ui = div(tick.render(n => div(span(s"tick:$n"), UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("box"))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "1"), "runs:1")
                    _ <- tick.set(1)
                    _ <- showsEventually(recorder, Seq("0", "0"), "tick:1")
                    _ <- showsEventually(recorder, Seq("0", "1"), "runs:1")
                    _ <- tick.set(2)
                    _ <- showsEventually(recorder, Seq("0", "0"), "tick:2")
                    _ <- showsEventually(recorder, Seq("0", "1"), "runs:1")
                yield ()
            }
            total <- runs.get
        yield assert(total == 1)
    }

    /** Runs `change`, waits for the next paint at `path`, and returns what the path shows then. */
    private def afterRepaint(recorder: Recorder, path: Seq[String])(change: => Unit < Async)(using
        Frame,
        kyo.test.AssertScope
    ): String < Async =
        for
            before <- recorder.paintsAt(path)
            _      <- change
            _      <- assertEventually(recorder.paintsAt(path).map(_ > before))
            shown  <- recorder.shown(path)
        yield shown

    "a keyed node keeps its instance when an outer region re-renders the region around it" in {
        for
            runs  <- AtomicInt.init(0)
            outer <- Signal.initRef(0)
            inner <- Signal.initRef("x")
            node = UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("box")
            ui   = div(outer.render(n => div(span(s"outer:$n"), inner.render(_ => div(node)))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "runs:1")
                    // Each step waits for the repaint of the region it changes, so a late paint of an earlier step is not taken
                    // for the paint of this one, and then for the instance's content in the repainted range.
                    _      <- afterRepaint(recorder, Seq("0"))(outer.set(1))
                    _      <- showsEventually(recorder, Seq("0", "1", "0"), "runs:")
                    first  <- recorder.shown(Seq("0", "1", "0"))
                    _      <- afterRepaint(recorder, Seq("0", "1"))(inner.set("y"))
                    _      <- afterRepaint(recorder, Seq("0"))(outer.set(2))
                    _      <- showsEventually(recorder, Seq("0", "1", "0"), "runs:")
                    second <- recorder.shown(Seq("0", "1", "0"))
                    total  <- runs.get
                yield
                    assert(first.contains("runs:1") && second.contains("runs:1"), s"shown: $first / $second")
                    assert(total == 1, s"the instance was rebuilt by outer re-renders: $total runs")
            }
        yield succeed
    }

    "a keyless node gets a new instance when an outer region re-renders the region around it" in {
        for
            runs  <- AtomicInt.init(0)
            outer <- Signal.initRef(0)
            node = UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r")))
            ui   = div(outer.render(n => div(span(s"outer:$n"), outer.map(_ => "x").render(_ => div(node)))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _      <- showsEventually(recorder, Seq("0", "1", "0"), "runs:")
                    before <- runs.get
                    _      <- outer.set(1)
                    // The inner region may also render again on its own, so the check is a new run, shown, not an exact count.
                    _ <- assertEventually(runs.get.map(_ > before))
                    _ <- assertEventually(
                        runs.get.map(latest => recorder.shown(Seq("0", "1", "0")).map(_.contains(s"runs:$latest")))
                    )
                yield ()
            }
        yield succeed
    }

    "row keys deeper than 63 levels identify instances without taking positions along" in {
        for
            runs  <- AtomicInt.init(0)
            extra <- Signal.initRef(false)
            items = Signal.initConst(Chunk("a"))
            node  = UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("row")
            row   = (show: Boolean) =>
                if show then UI.Ast.Div(children = Chunk(span("inserted"), node)) else UI.Ast.Div(children = Chunk(node))
            deep = (show: Boolean) =>
                (1 to 70).foldLeft[UI](items.foreachKeyed(identity)(_ => row(show)))((inner, _) => div(inner))
            ui = div(extra.render(deep))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- assertEventually(runs.get.map(_ == 1))
                    _ <- extra.set(true)
                    // The mount now sits second in its row; a paint there means it was subscribed again after the re-render.
                    _     <- assertEventually(recorder.paintsEndingWith(Seq("a", "1")).map(_ >= 1))
                    total <- runs.get
                yield assert(total == 1, s"a sibling deep inside a row rebuilt the instance: $total runs")
            }
        yield succeed
    }

    "a sibling inserted before the region around a keyed node leaves the instance in place" in {
        for
            runs  <- AtomicInt.init(0)
            outer <- Signal.initRef(false)
            inner <- Signal.initRef("x")
            node   = UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("box")
            region = inner.render(_ => div(node))
            ui     = div(outer.render { extra =>
                if extra then UI.Ast.Div(children = Chunk(span("inserted"), region)) else UI.Ast.Div(children = Chunk(region))
            })
            _ <- mount(ui) { (recorder, _) =>
                for
                    _     <- showsEventually(recorder, Seq("0", "0", "0"), "runs:1")
                    shown <- afterRepaint(recorder, Seq("0", "1", "0"))(outer.set(true))
                    total <- runs.get
                yield
                    assert(shown.contains("runs:1"), s"shown: $shown")
                    assert(total == 1, s"the inserted sibling rebuilt the instance: $total runs")
            }
        yield succeed
    }

    "a keyed instance is released when the region around it goes away" in {
        for
            released <- AtomicBoolean.init(false)
            outer    <- Signal.initRef(true)
            inner    <- Signal.initRef("x")
            node = UI.mounted(Scope.ensure(released.set(true)).andThen(span("held"))).keyed("box")
            ui   = div(outer.render(show => if show then div(inner.render(_ => div(node))) else div(span("gone"))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0", "0"), "held")
                    _ <- outer.set(false)
                    _ <- showsEventually(recorder, Seq("0"), "gone")
                    _ <- assertEventually(released.get)
                yield ()
            }
        yield succeed
    }

    "an outer re-render that interrupts a region while it releases its keyed instances still releases all of them" in {
        for
            entered  <- AtomicInt.init(0)
            released <- AtomicInt.init(0)
            gate     <- Promise.init[Unit, Any]
            outer    <- Signal.initRef(0)
            inner    <- Signal.initRef(true)
            node = (name: String) =>
                UI.mounted(
                    Scope.ensure(entered.incrementAndGet.andThen(gate.get).andThen(released.incrementAndGet.unit)).andThen(span(name))
                ).keyed(name)
            ui = div(outer.render(_ => div(inner.render(s => if s then div(node("mount-a"), node("mount-b")) else div(span("empty"))))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0", "0"), "mount-a")
                    _ <- showsEventually(recorder, Seq("0", "0", "1"), "mount-b")
                    _ <- inner.set(false)
                    _ <- assertEventually(entered.get.map(_ == 1))
                    _ <- afterRepaint(recorder, Seq("0"))(outer.set(1))
                    _ <- gate.completeUnitDiscard
                    _ <- assertEventually(released.get.map(_ == 2))
                yield ()
            }
        yield succeed
    }

    private def mountedNodes(rui: ReactiveUI): Chunk[ReactiveUI] =
        Chunk.from(rui.mounted.fold(Seq.empty[ReactiveUI])(_ => Seq(rui)) ++ rui.children.flatMap(mountedNodes))

    // An outer re-render stops the old subscription of a region only once the outer value's scope has drained, while the new
    // subscription of that region already works on the same table. The table follows the subscription born in the pass that last
    // covered the region; the older one, still running a pass, must neither release nor record anything in it.
    private def liveOf(view: ReactiveUI.ClaimView)(using Frame): Maybe[ReactiveUI.MountInstance] < Sync =
        view.state.get.map {
            case ReactiveUI.ClaimState.Live(instance) => Present(instance)
            case _                                    => Absent
        }

    "a region's newer subscription owns its entry in the mount table, and an older one still running leaves the table alone" in {
        Scope.run {
            for
                released <- AtomicBoolean.init(false)
                tick     <- Signal.initRef(0)
                root     <- ReactiveUI.normalize(
                    div(tick.render(_ => div(UI.mounted(Scope.ensure(released.set(true)).andThen(span("held"))).keyed("k")))),
                    Seq.empty
                )
                region   = root.children.head
                node     = mountedNodes(root).head
                spec     = node.mounted.get
                identity = spec.identity.get
                (table, top) <- ReactiveUI.MountTable.root(Seq.empty, new ReactiveUI.MountSession(audited = true))
                outerFirst   <- top.beginWalk(Seq(root))
                older = top.child(outerFirst, region)
                olderPass <- older.beginWalk(region.children)
                first     <- older.claim(node, identity, spec, olderPass).map(liveOf)
                _         <- older.endWalk
                _         <- assertEventually(first.fold(Kyo.lift(false))(_.cell.current.map(_.isInstanceOf[ReactiveUI.MountState.Ready])))
                outerNext <- top.beginWalk(Seq(root))
                newer = top.child(outerNext, region)
                newerPass <- newer.beginWalk(region.children)
                kept      <- newer.claim(node, identity, spec, newerPass).map(liveOf)
                _         <- older.beginWalk(Seq.empty)
                _         <- older.endWalk
                stale     <- older.claim(node, identity, spec, olderPass).map(liveOf)
                _         <- newer.endWalk
                gone      <- released.get
                againPass <- newer.beginWalk(region.children)
                again     <- newer.claim(node, identity, spec, againPass).map(liveOf)
                _         <- newer.endWalk
                found     <- Sync.defer(table.session.audit.fold(Chunk("not audited"))(_.inconsistencies))
                _         <- table.evictAll
                after     <- released.get
            yield
                assert(found.isEmpty, s"mount table states against the invariants: ${found.mkString("; ")}")
                def same(a: Maybe[ReactiveUI.MountInstance]) = a.nonEmpty && first.exists(_ eq a.get)
                assert(same(kept), "the newer subscription did not find the kept instance")
                assert(!gone, "the older subscription released the instance the newer one shows")
                assert(same(stale), "the older subscription's claim did not reuse the live instance")
                assert(same(again), "the table lost the instance after the older subscription's pass")
                assert(after, "evicting the table did not release the instance")
        }
    }

    "an event registration that ends leaves a later one at the same path in place" in {
        for
            dispatch <- MountDispatch.init
            path = Seq("0", "1")
            at   = ReactiveRegion.RegionIdentity(path, 1)
            node = UI.mounted(Kyo.lift(UI.empty))
            (table, _) <- ReactiveUI.MountTable.root(Seq.empty)
            instance   <- ReactiveUI.MountInstance.start(MountedSpec(node, path, Nil), Absent, Nil, table)
            _          <- dispatch.register(at, instance)
            _          <- dispatch.register(at, instance)
            _          <- dispatch.unregister(at, instance)
            found      <- dispatch.instance(at)
            _          <- instance.teardown
        yield assert(found.exists(_ eq instance))
    }

    "two mounts at one path, one the content of the other, made under one frame, are each found by their own content" in {
        // A helper taking a `Frame` makes both under its caller's, so only how deep each one's content sits at the path tells them apart.
        def nested(using Frame)    = (UI.mounted(Kyo.lift(UI.empty)), UI.mounted(Kyo.lift(UI.empty)))
        val (outerNode, innerNode) = nested
        val path                   = Seq("0")
        val outerAt                = ReactiveRegion.RegionIdentity(path, 1)
        val innerAt                = ReactiveRegion.RegionIdentity(path, 3)
        for
            dispatch   <- MountDispatch.init
            (table, _) <- ReactiveUI.MountTable.root(Seq.empty)
            outer      <- ReactiveUI.MountInstance.start(MountedSpec(outerNode, path, Nil), Absent, Nil, table)
            inner      <- ReactiveUI.MountInstance.start(MountedSpec(innerNode, path, Nil), Absent, Nil, table)
            _          <- dispatch.register(outerAt, outer)
            before     <- dispatch.instance(innerAt)
            _          <- dispatch.register(innerAt, inner)
            o          <- dispatch.instance(outerAt)
            i          <- dispatch.instance(innerAt)
            _          <- inner.teardown
            _          <- outer.teardown
        yield
            assert(outerNode.frame == innerNode.frame)
            assert(before.isEmpty, "the inner mount found the outer one's instance before its own registered")
            assert(o.exists(_ eq outer) && i.exists(_ eq inner))
        end for
    }

    "siblings appearing before a keyed node do not change its identity" in {
        for
            runs  <- AtomicInt.init(0)
            extra <- Signal.initRef(false)
            node = UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("box")
            ui   = div(extra.render { show =>
                val before = if show then Chunk[UI](span("first"), span("second")) else Chunk.empty[UI]
                UI.Ast.Div(children = before.append(node))
            })
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0"), "runs:1")
                    _ <- extra.set(true)
                    _ <- showsEventually(recorder, Seq("0", "2"), "runs:1")
                    _ <- extra.set(false)
                    _ <- showsEventually(recorder, Seq("0", "0"), "runs:1")
                yield ()
            }
            total <- runs.get
        yield assert(total == 1)
    }

    "two call sites at the same position with the same key are separate instances" in {
        for
            aRuns  <- AtomicInt.init(0)
            bRuns  <- AtomicInt.init(0)
            choice <- Signal.initRef(true)
            ui = div(choice.render { a =>
                if a then div(UI.mounted(aRuns.incrementAndGet.map(n => span(s"a:$n"))).keyed(1))
                else div(UI.mounted(bRuns.incrementAndGet.map(n => span(s"b:$n"))).keyed(1))
            })
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0"), "a:1")
                    _ <- choice.set(false)
                    _ <- showsEventually(recorder, Seq("0", "0"), "b:1")
                    _ <- choice.set(true)
                    _ <- showsEventually(recorder, Seq("0", "0"), "a:2")
                yield ()
            }
        yield succeed
    }

    // The helpers below take a `(using Frame)` the way application code does, so every mount they build gets the same frame.

    private def counted(runs: AtomicRef[Map[String, Int]], name: String)(using Frame): UI < Sync =
        runs.updateAndGet(m => m.updated(name, m.getOrElse(name, 0) + 1)).map(m => span(s"$name:${m(name)}"))

    private def panels(id: String, saveFirst: Boolean, runs: AtomicRef[Map[String, Int]])(using Frame): UI =
        val cover = UI.mounted(counted(runs, "cover")).keyed(id)
        val save  = UI.mounted(counted(runs, "save")).keyed(id)
        div(if saveFirst then save else cover, if saveFirst then cover else save)
    end panels

    "mounts built under one passed-down Frame are told apart by where .keyed is written" in {
        for
            runs      <- AtomicRef.init(Map.empty[String, Int])
            saveFirst <- Signal.initRef(false)
            ui = div(saveFirst.render(first => panels("album-1", first, runs)))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0"), "cover:1")
                    _ <- showsEventually(recorder, Seq("0", "1"), "save:1")
                    _ <- saveFirst.set(true)
                    _ <- showsEventually(recorder, Seq("0", "0"), "save:1")
                    _ <- showsEventually(recorder, Seq("0", "1"), "cover:1")
                yield ()
            }
            counts <- runs.get
        yield assert(counts == Map("cover" -> 1, "save" -> 1), counts.toString)
    }

    private def tile(ref: String, runs: AtomicInt, released: AtomicInt)(using Frame): UI =
        UI.mounted(Scope.ensure(released.incrementAndGet.unit).andThen(runs.incrementAndGet.map(n => span(s"$ref:$n")))).keyed(ref)

    "the same keyed tile shown twice in one region is reported at both places, and a tile shown once keeps its instance" in {
        for
            runs     <- AtomicInt.init(0)
            released <- AtomicInt.init(0)
            tick     <- Signal.initRef(0)
            ui = div(tick.render(_ => UI.Ast.Div(children = Chunk("x", "y", "x").map(tile(_, runs, released)))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _       <- showsEventually(recorder, Seq("0", "0"), "kyo-mount-error")
                    _       <- showsEventually(recorder, Seq("0", "1"), "y:1")
                    _       <- showsEventually(recorder, Seq("0", "2"), "kyo-mount-error")
                    first   <- recorder.shown(Seq("0", "0"))
                    _       <- afterRepaint(recorder, Seq("0"))(tick.set(1))
                    _       <- showsEventually(recorder, Seq("0", "1"), "y:1")
                    _       <- showsEventually(recorder, Seq("0", "0"), "kyo-mount-error")
                    total   <- runs.get
                    dropped <- released.get
                yield
                    assert(first.contains("shown in 2 places") && first.contains("0.0") && first.contains("0.2"), first)
                    assert(total == 1 && dropped == 0, s"runs: $total, released: $dropped")
            }
        yield succeed
    }

    "when one place of a duplicated identity goes away, the other one gets an instance" in {
        for
            runs     <- AtomicInt.init(0)
            released <- AtomicInt.init(0)
            show     <- Signal.initRef(true)
            ui = div(show.render { s =>
                UI.Ast.Div(children = Chunk("x", "y", "x").zipWithIndex.map { (ref, i) =>
                    if i == 0 && !s then UI.empty else tile(ref, runs, released)
                })
            })
            _ <- mount(ui) { (recorder, _) =>
                for
                    _       <- showsEventually(recorder, Seq("0", "2"), "kyo-mount-error")
                    _       <- showsEventually(recorder, Seq("0", "1"), "y:1")
                    _       <- afterRepaint(recorder, Seq("0"))(show.set(false))
                    _       <- showsEventually(recorder, Seq("0", "2"), "x:2")
                    _       <- showsEventually(recorder, Seq("0", "1"), "y:1")
                    total   <- runs.get
                    dropped <- released.get
                yield assert(total == 2 && dropped == 0, s"runs: $total, released: $dropped")
            }
        yield succeed
    }

    "duplicates whose places all shift stay reported, and run nothing" in {
        for
            runs     <- AtomicInt.init(0)
            released <- AtomicInt.init(0)
            lead     <- Signal.initRef(false)
            ui = div(lead.render { l =>
                UI.Ast.Div(children = Chunk.from((if l then Seq(span("lead"))
                                                  else Seq.empty) ++ Seq("x", "x").map(tile(_, runs, released))))
            })
            _ <- mount(ui) { (recorder, _) =>
                for
                    _     <- showsEventually(recorder, Seq("0", "1"), "kyo-mount-error")
                    _     <- afterRepaint(recorder, Seq("0"))(lead.set(true))
                    _     <- showsEventually(recorder, Seq("0", "1"), "kyo-mount-error")
                    _     <- showsEventually(recorder, Seq("0", "2"), "kyo-mount-error")
                    total <- runs.get
                yield assert(total == 0, s"runs: $total")
            }
        yield succeed
    }

    private def editorOrViewer(editing: Boolean, id: String, runs: AtomicRef[Map[String, Int]])(using Frame): UI =
        if editing then UI.mounted(counted(runs, "editor")).keyed(id)
        else UI.mounted(counted(runs, "viewer")).keyed(id)

    "two .keyed sites in one helper at one position are separate instances" in {
        for
            runs    <- AtomicRef.init(Map.empty[String, Int])
            editing <- Signal.initRef(false)
            ui = div(editing.render(e => div(editorOrViewer(e, "doc-1", runs))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0"), "viewer:1")
                    _ <- editing.set(true)
                    _ <- showsEventually(recorder, Seq("0", "0"), "editor:1")
                    _ <- editing.set(false)
                    _ <- showsEventually(recorder, Seq("0", "0"), "viewer:2")
                yield ()
            }
        yield succeed
    }

    private def card(name: String, runs: AtomicRef[Map[String, Int]])(using Frame, UI.MountSite): UI =
        UI.mounted(counted(runs, name)).keyed("card")

    "a helper that takes the MountSite makes each of its callers a place of its own" in {
        for
            runs   <- AtomicRef.init(Map.empty[String, Int])
            choice <- Signal.initRef(true)
            ui = div(choice.render(a => div(if a then card("a", runs) else card("b", runs))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0"), "a:1")
                    _ <- choice.set(false)
                    _ <- showsEventually(recorder, Seq("0", "0"), "b:1")
                    _ <- choice.set(true)
                    _ <- showsEventually(recorder, Seq("0", "0"), "a:2")
                yield ()
            }
        yield succeed
    }

    private def rowsKeepTheirInstances(keepsRows: Boolean)(using Frame, kyo.test.AssertScope) =
        for
            runs  <- AtomicRef.init(Map.empty[String, Int])
            items <- Signal.initRef(Chunk("a", "b"))
            row = (id: String) =>
                UI.mounted(runs.updateAndGet(m => m.updated(id, m.getOrElse(id, 0) + 1)).map(m => span(s"$id:${m(id)}")))
                    .keyed("row")
            ui = div(items.foreachKeyed(identity)(id => div(row(id))))
            _ <- mount(ui, keepsRows) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "a", "0"), "a:1")
                    _ <- showsEventually(recorder, Seq("0", "b", "0"), "b:1")
                    _ <- items.set(Chunk("c", "a", "b"))
                    _ <- showsEventually(recorder, Seq("0", "c", "0"), "c:1")
                    _ <- showsEventually(recorder, Seq("0", "a", "0"), "a:1")
                    _ <- showsEventually(recorder, Seq("0", "b", "0"), "b:1")
                yield ()
            }
            counts <- runs.get
        yield assert(counts == Map("a" -> 1, "b" -> 1, "c" -> 1))

    "rows of a keyed list keep separate instances of the same keyed node across list changes" in {
        rowsKeepTheirInstances(keepsRows = false)
    }

    "rows a list patch leaves in place keep their keyed instances, and a new row with the same key gets its own" in {
        rowsKeepTheirInstances(keepsRows = true)
    }

    "the same call site shown twice with one key inside a list row is reported in that row" in {
        for
            runs  <- AtomicInt.init(0)
            items <- Signal.initRef(Chunk("a"))
            row = (_: String) =>
                UI.Ast.Div(children =
                    Chunk.from(Seq(1, 2).map(_ => UI.mounted(runs.incrementAndGet.map(n => span(s"runs:$n"))).keyed("same")))
                )
            ui = div(items.foreachKeyed(identity)(row))
            _ <- mount(ui, keepsRows = true) { (recorder, _) =>
                for
                    _      <- showsEventually(recorder, Seq("0", "a", "1"), "kyo-mount-error")
                    _      <- items.set(Chunk("b", "a"))
                    _      <- showsEventually(recorder, Seq("0", "b", "0"), "kyo-mount-error")
                    _      <- showsEventually(recorder, Seq("0", "b", "1"), "kyo-mount-error")
                    second <- recorder.shown(Seq("0", "b", "1"))
                yield assert(second.contains("0.b.0") && second.contains("0.b.1") && !second.contains("0.a."), second)
            }
            total <- runs.get
        yield assert(total == 0, s"runs: $total")
    }

    "the same call site shown twice with one key outside any region is reported at both places" in {
        for
            runs <- AtomicInt.init(0)
            ui = UI.Ast.Div(children =
                Chunk.from(Seq(1, 2).map(_ => UI.mounted(runs.incrementAndGet.map(n => span(s"runs:$n"))).keyed("same")))
            )
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0"), "kyo-mount-error")
                    _ <- showsEventually(recorder, Seq("1"), "kyo-mount-error")
                yield ()
            }
            total <- runs.get
        yield assert(total == 0, s"runs: $total")
    }

    "a key change releases the old instance completely before the new effect starts" in {
        for
            key               <- Signal.initRef("a")
            releasing         <- Promise.init[Unit, Any]
            finishRelease     <- Promise.init[Unit, Any]
            released          <- AtomicBoolean.init(false)
            sawReleaseAtStart <- Promise.init[Boolean, Any]
            effect = (k: String) =>
                if k == "a" then
                    Scope.ensure(releasing.completeUnitDiscard.andThen(finishRelease.get).andThen(released.set(true)))
                        .andThen(span("a"))
                else released.get.map(r => sawReleaseAtStart.completeDiscard(Result.succeed(r))).andThen(span("b"))
            ui = div(key.render(k => div(UI.mounted(effect(k)).keyed(k))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _        <- showsEventually(recorder, Seq("0", "0"), ">a<")
                    _        <- key.set("b")
                    _        <- releasing.get
                    started  <- sawReleaseAtStart.done
                    _        <- finishRelease.completeUnitDiscard
                    observed <- sawReleaseAtStart.get
                    _        <- showsEventually(recorder, Seq("0", "0"), ">b<")
                yield
                    assert(!started)
                    assert(observed)
            }
        yield succeed
    }

    "while a new instance is pending, KeepLatest shows an inert snapshot of the old content" in {
        for
            key        <- Signal.initRef("a")
            gate       <- Promise.init[Unit, Any]
            nestedRuns <- AtomicInt.init(0)
            clicks     <- AtomicInt.init(0)
            effect = (k: String) =>
                if k == "a" then
                    Kyo.lift(div(
                        span("old"),
                        button("hit").onClick(clicks.incrementAndGet.unit),
                        UI.mounted(nestedRuns.incrementAndGet.map(n => span(s"nested:$n")))
                    ))
                else gate.get.andThen(span("new"))
            ui = div(key.render(k => div(UI.mounted(effect(k)).keyed(k))))
            _ <- mount(ui) { (recorder, handle) =>
                for
                    _      <- showsEventually(recorder, Seq("0", "0", "2"), "nested:1")
                    _      <- handle(Seq("0", "0", "1"), click("0", "0", "1"))
                    _      <- assertEventually(clicks.get.map(_ == 1))
                    before <- recorder.paintsAt(Seq("0"))
                    _      <- key.set("b")
                    _      <- assertEventually(recorder.paintsAt(Seq("0")).map(_ > before))
                    _      <- showsEventually(recorder, Seq("0", "0"), "old")
                    _      <- handle(Seq("0", "0", "1"), click("0", "0", "1"))
                    _      <- gate.completeUnitDiscard
                    _      <- showsEventually(recorder, Seq("0", "0"), "new")
                    n      <- nestedRuns.get
                    c      <- clicks.get
                yield
                    assert(n == 1, s"the snapshot re-ran a nested mount: $n runs")
                    assert(c == 1, s"a click on the snapshot reached the old handler: $c clicks")
            }
        yield succeed
    }

    "while a new instance is pending, the KeepLatest snapshot shows what the nested mounts showed" in {
        // A detail view whose thread is a mount of its own: the snapshot is the whole old view, thread included, not the
        // view with an empty hole where the thread was.
        for
            key  <- Signal.initRef("a")
            gate <- Promise.init[Unit, Any]
            effect = (k: String) =>
                if k == "a" then
                    Kyo.lift(div(
                        span("old"),
                        UI.mounted(Kyo.lift(span("keyless:a"))),
                        UI.mounted(Kyo.lift(span("thread:a"))).keyed("thread-a")
                    ))
                else gate.get.andThen(span("new"))
            ui = div(key.render(k => div(UI.mounted(effect(k)).keyed(k))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _      <- showsEventually(recorder, Seq("0", "0", "2"), "thread:a")
                    _      <- showsEventually(recorder, Seq("0", "0", "1"), "keyless:a")
                    before <- recorder.paintsAt(Seq("0"))
                    _      <- key.set("b")
                    _      <- assertEventually(recorder.paintsAt(Seq("0")).map(_ > before))
                    _      <- showsEventually(recorder, Seq("0", "0"), "old")
                    held   <- recorder.shown(Seq("0", "0"))
                    _      <- gate.completeUnitDiscard
                    _      <- showsEventually(recorder, Seq("0", "0"), "new")
                yield assert(held.contains("keyless:a") && held.contains("thread:a"), s"the snapshot lost a nested mount: $held")
            }
        yield succeed
    }

    "the KeepLatest snapshot is held, so nothing in it looks live, until the new content shows" in {
        // The snapshot takes no events, so a field in it must not look like one the reader can type into.
        for
            key  <- Signal.initRef("a")
            gate <- Promise.init[Unit, Any]
            effect = (k: String) => if k == "a" then Kyo.lift(input.id("note")) else gate.get.andThen(span("new"))
            ui     = div(key.render(k => div(UI.mounted(effect(k)).keyed(k))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _       <- showsEventually(recorder, Seq("0", "0"), "note")
                    _       <- key.set("b")
                    _       <- assertEventually(recorder.holds.map(_.map(_._2) == Chunk(true)))
                    whileIn <- recorder.holds
                    _       <- gate.completeUnitDiscard
                    _       <- showsEventually(recorder, Seq("0", "0"), "new")
                    _       <- assertEventually(recorder.holds.map(_.map(_._2) == Chunk(true, false)))
                    all     <- recorder.holds
                yield assert(all.map(_._1).distinct == Chunk(whileIn.head._1), s"held and released different ranges: $all")
            }
        yield succeed
    }

    "a key change under KeepLatest paints the snapshot and then the new content, never an empty slot" in {
        for
            key  <- Signal.initRef("a")
            gate <- Promise.init[Unit, Any]
            effect = (k: String) => if k == "a" then Kyo.lift(span("old")) else gate.get.andThen(span("new"))
            ui     = div(key.render(k => div(UI.mounted(effect(k)).keyed(k))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _      <- showsEventually(recorder, Seq("0", "0"), "old")
                    before <- recorder.paintCount
                    _      <- key.set("b")
                    _      <- assertEventually(recorder.paintsAfter(before).map(_.nonEmpty))
                    _      <- gate.completeUnitDiscard
                    _      <- showsEventually(recorder, Seq("0", "0"), "new")
                    after  <- recorder.paintsAfter(before)
                yield
                    val empty = after.filterNot((_, html) => html.contains("old") || html.contains("new"))
                    assert(empty.isEmpty, s"a paint showed neither the old content nor the new: $empty")
            }
        yield succeed
    }

    "while a new instance is pending, Placeholder shows the placeholder" in {
        for
            key  <- Signal.initRef("a")
            gate <- Promise.init[Unit, Any]
            effect = (k: String) => if k == "a" then Kyo.lift(span("old")) else gate.get.andThen(span("new"))
            ui     = div(key.render(k =>
                div(UI.mounted(effect(k)).keyed(k).placeholder(span("waiting")).whilePending(PendingPolicy.Placeholder))
            ))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _   <- showsEventually(recorder, Seq("0", "0"), "old")
                    _   <- key.set("b")
                    _   <- showsEventually(recorder, Seq("0", "0"), "waiting")
                    now <- recorder.shown(Seq("0", "0"))
                    _   <- gate.completeUnitDiscard
                    _   <- showsEventually(recorder, Seq("0", "0"), "new")
                yield assert(!now.contains("old"))
            }
        yield succeed
    }

    "a failed effect renders onError and the enclosing region keeps working" in {
        for
            tick <- Signal.initRef(0)
            ui = div(tick.render(n =>
                div(
                    span(s"tick:$n"),
                    UI.mounted(Abort.fail(new Exception("boom")))
                        .onError(e => span(s"error:${e.message}:${e.source}"))
                )
            ))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "1"), "error:boom:Effect")
                    _ <- tick.set(1)
                    _ <- showsEventually(recorder, Seq("0", "0"), "tick:1")
                yield ()
            }
        yield succeed
    }

    "a UI.fork fiber that fails after the node published switches it to its error" in {
        for
            feed <- Promise.init[Unit, Any]
            effect = UI.fork(feed.get.andThen(Abort.fail(new Exception("feed died")))).andThen(span("content"))
            ui     = div(UI.mounted(effect).onError(e => span(s"error:${e.message}:${e.source}")))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0"), "content")
                    _ <- feed.completeUnitDiscard
                    _ <- showsEventually(recorder, Seq("0"), "error:feed died:Fork")
                yield ()
            }
        yield succeed
    }

    "a failure is final: a later publish does not replace it" in {
        val failed = ReactiveUI.MountState.Failed(UI.Ast.Text("first")(using Frame.internal))
        val after  = ReactiveUI.MountSupervision.publish(UI.Ast.Text("late")(using Frame.internal))(failed)
        assert(after == failed)
    }

    "a fork failure is not overwritten when the effect publishes afterwards" in {
        for
            effectDone <- Promise.init[Unit, Any]
            effect =
                for
                    fiber <- UI.fork(Abort.fail(new Exception("early")))
                    _     <- fiber.getResult
                    _     <- effectDone.completeUnitDiscard
                yield span("late content")
            ui = div(UI.mounted(effect).onError(e => span(s"error:${e.message}")))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _     <- showsEventually(recorder, Seq("0"), "error:early")
                    _     <- effectDone.get
                    shown <- recorder.shown(Seq("0"))
                yield assert(!shown.contains("late content"))
            }
        yield succeed
    }

    "a plain Fiber.init failure does not switch the node" in {
        for
            done <- Promise.init[Unit, Any]
            effect =
                for
                    fiber <- Fiber.init(Abort.fail(new Exception("ignored")))
                    _     <- fiber.getResult
                    _     <- done.completeUnitDiscard
                yield span("content")
            ui = div(UI.mounted(effect).onError(e => span(s"error:${e.message}")))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _     <- done.get
                    _     <- showsEventually(recorder, Seq("0"), "content")
                    shown <- recorder.shown(Seq("0"))
                yield assert(!shown.contains("error"))
            }
        yield succeed
    }

    "a handler inside the content of a kept instance runs after its region re-renders" in {
        for
            tick   <- Signal.initRef(0)
            clicks <- AtomicInt.init(0)
            ui = div(tick.render(n =>
                div(span(s"tick:$n"), UI.mounted(Kyo.lift(div(button("hit").onClick(clicks.incrementAndGet.unit)))).keyed("k"))
            ))
            _ <- mount(ui) { (recorder, handle) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "1"), "hit")
                    _ <- tick.set(1)
                    _ <- showsEventually(recorder, Seq("0", "0"), "tick:1")
                    _ <- showsEventually(recorder, Seq("0", "1"), "hit")
                    _ <- handle(Seq("0", "1", "0"), click("0", "1", "0"))
                    _ <- assertEventually(clicks.get.map(_ == 1))
                yield ()
            }
        yield succeed
    }

    "a keyed node nested in a kept instance survives the outer region's re-renders" in {
        for
            tick      <- Signal.initRef(0)
            innerRuns <- AtomicInt.init(0)
            inner = UI.mounted(innerRuns.incrementAndGet.map(n => span(s"inner:$n"))).keyed("inner")
            outer = UI.mounted(Kyo.lift(div(inner))).keyed("outer")
            ui    = div(tick.render(n => div(span(s"tick:$n"), outer)))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "inner:1")
                    _ <- tick.set(1)
                    _ <- showsEventually(recorder, Seq("0", "0"), "tick:1")
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "inner:1")
                yield ()
            }
            total <- innerRuns.get
        yield assert(total == 1)
    }

    "a button inside mounted content submits the form around the mount" in {
        for
            submits <- AtomicInt.init(0)
            ui = UI.form.onSubmit(submits.incrementAndGet.unit)(UI.mounted(Kyo.lift(div(button("send")))))
            _ <- mount(ui) { (recorder, handle) =>
                for
                    _ <- showsEventually(recorder, Seq("0"), "send")
                    _ <- handle(Seq("0", "0"), click("0", "0"))
                    _ <- assertEventually(submits.get.map(_ == 1))
                yield ()
            }
        yield succeed
    }

    "a disabled button inside mounted content does not submit the form around the mount" in {
        for
            submits <- AtomicInt.init(0)
            ui = UI.form.onSubmit(submits.incrementAndGet.unit)(UI.mounted(Kyo.lift(div(button("send").disabled(true)))))
            _ <- mount(ui) { (recorder, handle) =>
                for
                    _ <- showsEventually(recorder, Seq("0"), "send")
                    _ <- handle(Seq("0", "0"), click("0", "0"))
                    n <- submits.get
                yield assert(n == 0)
            }
        yield succeed
    }

    "closing the subscription releases what a keyed instance acquired" in {
        for
            released <- AtomicBoolean.init(false)
            ui = div(UI.mounted(Scope.ensure(released.set(true)).andThen(span("held"))).keyed("k"))
            _ <- mount(ui)((recorder, _) => showsEventually(recorder, Seq("0"), "held"))
            r <- released.get
        yield assert(r)
    }

    "an interrupt while a rebuilt row subscribes still releases what the row had subscribed" in {
        // A rebuilt row claims its keyed mount, whose instance has already published, and repaints it inline while the row
        // is still subscribing. The exchange holds that paint, the region around the list closes meanwhile, and the list's
        // fiber is interrupted inside the row: what the row had subscribed by then must still be released.
        val held      = "held-mount"
        val armed     = new java.util.concurrent.atomic.AtomicBoolean(false)
        val published = new java.util.concurrent.atomic.AtomicBoolean(false)
        val recorder  = new Recorder()
        def holding(entered: Promise[Unit, Any], forever: Promise[Unit, Any]): UIExchange = new UIExchange:
            def onChange(
                region: ReactiveRegion,
                path: Seq[String],
                context: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                ui: UI
            )(using Frame): Unit < Async =
                HtmlRenderer.render(ui, path).map { html =>
                    if armed.get() && html.contains(held) then entered.completeUnitDiscard.andThen(forever.get)
                    else
                        if html.contains(held) then published.set(true)
                        recorder.onChange(region, path, context, parentContext, previous, ui)
                }
        for
            open    <- Signal.initRef(true)
            items   <- Signal.initRef(Chunk(("a", 0)))
            probe   <- Signal.initRef(0)
            entered <- Promise.init[Unit, Any]
            forever <- Promise.init[Unit, Any]
            ui = div(open.map(o =>
                if o then
                    div(items.foreachKeyed(_._1)((k, v) =>
                        div(
                            probe.map(p => span(s"$k:$v:$p")),
                            UI.mounted(Kyo.lift[UI, Async](span(held))).keyed("m").placeholder(span("..."))
                        )
                    )): UI
                else span("closed"): UI
            ))
            _ <- Scope.run {
                for
                    _    <- recorder.initial(ui)
                    root <- ReactiveUI.normalize(ui, Seq.empty)
                    sub  <- subscribeAudited(root, holding(entered, forever))
                    _    <- assertEventually(Sync.defer(published.get()))
                    _    <- Sync.defer(armed.set(true))
                    _    <- items.set(Chunk(("a", 1)))
                    _    <- assertEventually(entered.done)
                    _    <- open.set(false)
                    _    <- assertEventually(recorder.shown(Seq("0")).map(_.contains("closed")))
                    _    <- assertEventually(Sync.Unsafe.defer(probe.unsafe.waiters() == 0))
                    _    <- assertConsistent(sub)
                yield ()
            }
        yield succeed
        end for
    }

    // ---- Identity without the Frame: a keyed instance is found by its own identity, regions by their place ----

    // A component that takes a `Frame`, as application code does: both regions it builds get the caller's frame.
    private def twoRegions(show: Boolean, a: Signal[String], b: Signal[String], runs: AtomicInt)(using Frame): UI =
        div(
            if show then a.render(x => span(x)) else span("none"),
            b.render(_ => div(UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("box")))
        )

    "a keyed mount in the second region of a component that takes a Frame survives the first region coming and going" in {
        for
            runs  <- AtomicInt.init(0)
            outer <- Signal.initRef(true)
            a     <- Signal.initRef("a")
            b     <- Signal.initRef("b")
            ui = div(outer.render(show => twoRegions(show, a, b, runs)))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "runs:")
                    _ <- afterRepaint(recorder, Seq("0"))(outer.set(false))
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "runs:")
                    _ <- afterRepaint(recorder, Seq("0"))(outer.set(true))
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "runs:")
                yield ()
            }
            total <- runs.get
        yield assert(total == 1, s"the keyed instance restarted when its neighbour region came and went: $total runs")
    }

    // A helper without a `Frame` of its own: every region it builds gets the one frame derived inside it.
    private def panelRegion(sig: Signal[String], key: String, runs: AtomicRef[Map[String, Int]]): UI =
        sig.render(_ => div(UI.mounted(counted(runs, key)).keyed(key)))

    "two regions from one helper without a Frame keep their keyed instances when the first one goes away" in {
        for
            runs  <- AtomicRef.init(Map.empty[String, Int])
            outer <- Signal.initRef(true)
            a     <- Signal.initRef("a")
            b     <- Signal.initRef("b")
            ui = div(outer.render(show => div(if show then panelRegion(a, "p1", runs) else span("none"), panelRegion(b, "p2", runs))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0", "0"), "p1:1")
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "p2:1")
                    _ <- afterRepaint(recorder, Seq("0"))(outer.set(false))
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "p2:")
                    _ <- afterRepaint(recorder, Seq("0"))(outer.set(true))
                    _ <- showsEventually(recorder, Seq("0", "0", "0"), "p1:")
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "p2:")
                yield ()
            }
            counts <- runs.get
        yield assert(counts == Map("p1" -> 2, "p2" -> 1), s"runs per instance: $counts")
    }

    private def withNeighbour(mode: Int, a: Signal[String], b: Signal[String], runs: AtomicInt)(using Frame): UI =
        val neighbour: UI = mode match
            case 0 => a.render(x => span(x))
            case 1 => span("constant")
            case _ => UI.mounted(Kyo.lift(span("other mount")))
        div(neighbour, b.render(_ => div(UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("box"))))
    end withNeighbour

    "an unrelated neighbour that turns from a region into a constant or a mount does not restart a keyed instance" in {
        for
            runs <- AtomicInt.init(0)
            mode <- Signal.initRef(0)
            a    <- Signal.initRef("a")
            b    <- Signal.initRef("b")
            ui = div(mode.render(m => withNeighbour(m, a, b, runs)))
            // After each change the box shows an instance's content, then the count says whether it was the first instance. Waiting
            // for "runs:1" instead would wait forever on the restart this test guards against.
            _ <- mount(ui) { (recorder, _) =>
                Kyo.foreachDiscard(Seq(1, 2, 0, 2, 1)) { next =>
                    for
                        _     <- afterRepaint(recorder, Seq("0"))(mode.set(next))
                        _     <- showsEventually(recorder, Seq("0", "1", "0"), "runs:")
                        total <- runs.get
                        shown <- recorder.shown(Seq("0", "1", "0"))
                    yield assert(
                        total == 1,
                        s"the neighbour turning into mode $next restarted the keyed instance: $total runs, the box shows $shown"
                    )
                }
            }
            total <- runs.get
        yield assert(total == 1, s"a neighbour changing kind restarted the keyed instance: $total runs")
    }

    "a keyed instance that moves from one region to another in one pass keeps its instance" in {
        for
            runs <- AtomicInt.init(0)
            left <- Signal.initRef(true)
            r1   <- Signal.initRef("r1")
            r2   <- Signal.initRef("r2")
            node = UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("moving")
            ui   = div(left.render(l =>
                div(
                    r1.render(_ => if l then div(node) else div(span("empty-1"))),
                    r2.render(_ => if l then div(span("empty-2")) else div(node))
                )
            ))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _     <- showsEventually(recorder, Seq("0", "0", "0"), "runs:1")
                    _     <- afterRepaint(recorder, Seq("0"))(left.set(false))
                    _     <- showsEventually(recorder, Seq("0", "1", "0"), "runs:")
                    moved <- recorder.shown(Seq("0", "1", "0"))
                    _     <- afterRepaint(recorder, Seq("0"))(left.set(true))
                    _     <- showsEventually(recorder, Seq("0", "0", "0"), "runs:")
                    back  <- recorder.shown(Seq("0", "0", "0"))
                    total <- runs.get
                yield assert(
                    moved.contains("runs:1") && back.contains("runs:1") && total == 1,
                    s"moved: $moved, back: $back, runs: $total"
                )
            }
        yield succeed
    }

    "the same keyed identity shown twice in one pass is reported at both places and runs no effect" in {
        for
            runs <- AtomicInt.init(0)
            both <- Signal.initRef(true)
            node = UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("twice")
            ui   = div(both.render(b => if b then div(node, node) else div(node)))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _      <- showsEventually(recorder, Seq("0", "0"), "kyo-mount-error")
                    _      <- showsEventually(recorder, Seq("0", "1"), "kyo-mount-error")
                    first  <- recorder.shown(Seq("0", "0"))
                    second <- recorder.shown(Seq("0", "1"))
                    before <- runs.get
                    _      <- afterRepaint(recorder, Seq("0"))(both.set(false))
                    _      <- showsEventually(recorder, Seq("0", "0"), "runs:1")
                yield
                    assert(before == 0, s"a duplicated identity ran its effect: $before runs")
                    assert(first.contains("shown in 2 places") && first.contains("0.0") && first.contains("0.1"), first)
                    assert(second.contains("shown in 2 places"), second)
            }
        yield succeed
    }

    "the same keyed identity twice in a dynamic splice is reported like literal arguments" in {
        for
            runs <- AtomicInt.init(0)
            node = UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("spliced-twice")
            kids = Seq[UI.Ast.HtmlChildVal](span("lead"), node, node)
            ui   = div(div(kids*))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "1"), "kyo-mount-error")
                    _ <- showsEventually(recorder, Seq("0", "2"), "kyo-mount-error")
                yield ()
            }
            total <- runs.get
        yield assert(total == 0, s"a duplicated identity ran its effect: $total runs")
    }

    // A splice is a dynamic range: positions in it shift with its length. A keyed instance does not depend on its position, so it
    // stays; a keyless one is new on every re-render of its region, as anywhere else; a region in the splice keeps its keyed mount.
    "a dynamic splice whose length changes keeps keyed instances, also inside a region in it, and renews keyless ones" in {
        for
            keyedRuns  <- AtomicInt.init(0)
            nestedRuns <- AtomicInt.init(0)
            freeRuns   <- AtomicInt.init(0)
            lead       <- Signal.initRef(0)
            inner      <- Signal.initRef("x")
            keyedNode  = UI.mounted(keyedRuns.incrementAndGet.map(r => span(s"keyed:$r"))).keyed("spliced")
            nestedNode = UI.mounted(nestedRuns.incrementAndGet.map(r => span(s"nested:$r"))).keyed("nested")
            freeNode   = UI.mounted(freeRuns.incrementAndGet.map(r => span(s"free:$r")))
            ui         = div(lead.render { n =>
                val leads = Seq.fill[UI.Ast.HtmlChildVal](n)(span("lead"))
                div((leads ++ Seq[UI.Ast.HtmlChildVal](keyedNode, inner.render(_ => div(nestedNode)), freeNode))*)
            })
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0"), "keyed:1")
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "nested:1")
                    _ <- showsEventually(recorder, Seq("0", "2"), "free:1")
                    _ <- afterRepaint(recorder, Seq("0"))(lead.set(2))
                    _ <- showsEventually(recorder, Seq("0", "2"), "keyed:")
                    _ <- showsEventually(recorder, Seq("0", "3", "0"), "nested:")
                    _ <- showsEventually(recorder, Seq("0", "4"), "free:2")
                    _ <- afterRepaint(recorder, Seq("0"))(lead.set(1))
                    _ <- showsEventually(recorder, Seq("0", "1"), "keyed:")
                    _ <- showsEventually(recorder, Seq("0", "2", "0"), "nested:")
                    _ <- showsEventually(recorder, Seq("0", "3"), "free:3")
                    k <- recorder.shown(Seq("0", "1"))
                    m <- recorder.shown(Seq("0", "2", "0"))
                yield assert(k.contains("keyed:1") && m.contains("nested:1"), s"keyed: $k, nested: $m")
            }
            k <- keyedRuns.get
            m <- nestedRuns.get
            f <- freeRuns.get
        yield assert((k, m, f) == (1, 1, 3), s"runs (keyed, nested, keyless): ${(k, m, f)}")
    }

    "a keyed mount inside a region inside keyed rows is told apart by the row key" in {
        for
            runs  <- AtomicRef.init(Map.empty[String, Int])
            items <- Signal.initRef(Chunk("a", "b"))
            tick  <- Signal.initRef(0)
            ui = div(items.foreachKeyed(identity)(id => div(tick.render(_ => div(UI.mounted(counted(runs, id)).keyed("m"))))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "a", "0", "0"), "a:1")
                    _ <- showsEventually(recorder, Seq("0", "b", "0", "0"), "b:1")
                    _ <- items.set(Chunk("c", "a", "b"))
                    _ <- showsEventually(recorder, Seq("0", "c", "0", "0"), "c:1")
                    _ <- tick.set(1)
                    _ <- showsEventually(recorder, Seq("0", "a", "0", "0"), "a:1")
                    _ <- showsEventually(recorder, Seq("0", "b", "0", "0"), "b:1")
                    _ <- showsEventually(recorder, Seq("0", "c", "0", "0"), "c:1")
                yield ()
            }
            counts <- runs.get
        yield assert(counts == Map("a" -> 1, "b" -> 1, "c" -> 1), s"runs per row: $counts")
    }

    "an unkeyed foreach keeps keyed instances by position, as before" in {
        for
            runs  <- AtomicRef.init(Map.empty[String, Int])
            items <- Signal.initRef(Chunk("a", "b"))
            ui = div(items.foreach(id => div(UI.mounted(counted(runs, id)).keyed("m"))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _      <- showsEventually(recorder, Seq("0", "0", "0"), "a:1")
                    _      <- showsEventually(recorder, Seq("0", "1", "0"), "b:1")
                    _      <- items.set(Chunk("c", "a", "b"))
                    _      <- showsEventually(recorder, Seq("0", "2", "0"), "b:2")
                    first  <- recorder.shown(Seq("0", "0", "0"))
                    second <- recorder.shown(Seq("0", "1", "0"))
                yield assert(first.contains("a:1") && second.contains("b:1"), s"row 0: $first, row 1: $second")
            }
            counts <- runs.get
        yield assert(counts == Map("a" -> 1, "b" -> 2), s"runs: $counts")
    }

    private def card(issue: String, refresh: Signal[Int], runs: AtomicRef[Map[String, Int]])(using Frame): UI =
        div(refresh.render(_ => div(UI.mounted(counted(runs, issue)).keyed(issue))))

    // The page takes a `Frame` too, as application code does, so both cards and their regions get the page's caller's frame.
    private def board(pinned: Boolean, refresh: Signal[Int], runs: AtomicRef[Map[String, Int]])(using Frame): UI =
        div(if pinned then card("A", refresh, runs) else span("nothing pinned"), card("B", refresh, runs))

    "two cards on a page that takes a Frame keep the second card's instance while the first comes and goes" in {
        for
            runs    <- AtomicRef.init(Map.empty[String, Int])
            pinned  <- Signal.initRef(true)
            refresh <- Signal.initRef(0)
            ui = div(pinned.render(p => board(p, refresh, runs)))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "1", "0", "0"), "B:1")
                    // A's first instance must have run before it is unpinned: one released before its effect runs is never counted.
                    _ <- showsEventually(recorder, Seq("0", "0", "0", "0"), "A:1")
                    _ <- afterRepaint(recorder, Seq("0"))(pinned.set(false))
                    _ <- showsEventually(recorder, Seq("0", "1", "0", "0"), "B:")
                    _ <- afterRepaint(recorder, Seq("0"))(pinned.set(true))
                    _ <- showsEventually(recorder, Seq("0", "0", "0", "0"), "A:")
                    _ <- showsEventually(recorder, Seq("0", "1", "0", "0"), "B:")
                yield ()
            }
            counts <- runs.get
        yield assert(counts == Map("A" -> 2, "B" -> 1), s"runs per card: $counts")
    }

    // The first instance of A is still in its effect when A is unpinned: its release interrupts that effect, and the instance that
    // shows A again once it is pinned runs at once instead of waiting for anything of the one before it.
    "a keyed instance released while its effect is still pending does not hold back the instance that shows it again" in {
        for
            runs     <- AtomicRef.init(Map.empty[String, Int])
            starts   <- AtomicInt.init(0)
            entered  <- Latch.init(1)
            gate     <- Promise.init[Unit, Any]
            released <- AtomicBoolean.init(false)
            pinned   <- Signal.initRef(true)
            refresh  <- Signal.initRef(0)
            effectA = starts.incrementAndGet.map { n =>
                if n == 1 then Sync.ensure(released.set(true))(entered.release.andThen(gate.get).andThen(counted(runs, "A")))
                else counted(runs, "A")
            }
            cardA = div(refresh.render(_ => div(UI.mounted(effectA).keyed("A"))))
            ui    = div(pinned.render(p => div(if p then cardA else span("nothing pinned"), card("B", refresh, runs))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "1", "0", "0"), "B:1")
                    _ <- entered.await
                    _ <- afterRepaint(recorder, Seq("0"))(pinned.set(false))
                    _ <- afterRepaint(recorder, Seq("0"))(pinned.set(true))
                    _ <- showsEventually(recorder, Seq("0", "0", "0", "0"), "A:1")
                    _ <- assertEventually(released.get)
                yield ()
            }
            counts <- runs.get
            total  <- starts.get
        yield assert(counts == Map("A" -> 1, "B" -> 1) && total == 2, s"runs per card: $counts, effect starts of A: $total")
    }

    /** Keeps the messages logged at error level, so a test can tell whether a mount reported a failure. */
    private class ErrorLog extends Log.Unsafe.ConsoleLogger("MountedTest", Log.Level.error):
        // Unsafe: written by the log's dispatch, read by the test after the releases it waits for are done.
        private val messages = AtomicRef.Unsafe.init(Chunk.empty[String])(using AllowUnsafe.embrace.danger)

        override def emit(event: Log.Event)(using allow: AllowUnsafe): Unit =
            if event.level == Log.Level.error then discard(messages.getAndUpdate(_.append(event.message)))

        def logged(using Frame): Chunk[String] < Async = Log.flush.andThen(Sync.Unsafe.defer(messages.get()))
    end ErrorLog

    // A fiber that waits on a promise interrupts that promise when it is interrupted itself, so an effect awaiting a promise another
    // mount's effect awaits too sees that mount's release as an interrupt of its own. `released` is set once the instance's release
    // has run its effect's finalizers, which comes after the effect's outcome is handled.
    private def awaiting(shared: Promise[Unit, Any], entered: Latch, released: AtomicBoolean, name: String)(using
        Frame
    ): UI < (Async & Scope) =
        Scope.ensure(released.set(true)).andThen(entered.release).andThen(shared.get).andThen(span(s"$name:done"))

    "a mount whose effect is interrupted by the release of another mount shows its error view" in {
        for
            shared    <- Promise.init[Unit, Any]
            entered   <- Latch.init(2)
            releasedA <- AtomicBoolean.init(false)
            releasedB <- AtomicBoolean.init(false)
            showA     <- Signal.initRef(true)
            ui = div(
                showA.render(s => if s then div(UI.mounted(awaiting(shared, entered, releasedA, "A")).keyed("A")) else span("gone")),
                div(UI.mounted(awaiting(shared, entered, releasedB, "B")).keyed("B"))
            )
            _ <- mount(ui) { (recorder, _) =>
                for
                    _     <- entered.await
                    _     <- showA.set(false)
                    _     <- assertEventually(releasedA.get)
                    _     <- showsEventually(recorder, Seq("1", "0"), "kyo-mount-error")
                    gone  <- releasedB.get
                    shown <- recorder.shown(Seq("1", "0"))
                yield assert(!gone, s"B was released though nothing removed it, it shows $shown")
            }
        yield succeed
    }

    private def bothReleased(a: AtomicBoolean, b: AtomicBoolean)(using Frame): Boolean < Sync =
        a.get.map(x => b.get.map(x && _))

    private def reportsNoFailure(errors: ErrorLog)(using Frame, kyo.test.AssertScope): Unit < Async =
        errors.logged.map(logged => assert(!logged.exists(_.contains("Mounted effect")), s"a release was reported as a failure: $logged"))

    "two keyed mounts that await one promise and are released in one pass report no failure" in {
        val errors = new ErrorLog
        Log.let(Log(errors)) {
            for
                shared    <- Promise.init[Unit, Any]
                entered   <- Latch.init(2)
                releasedA <- AtomicBoolean.init(false)
                releasedB <- AtomicBoolean.init(false)
                show      <- Signal.initRef(true)
                ui = div(show.render(s =>
                    if s then
                        div(
                            UI.mounted(awaiting(shared, entered, releasedA, "A")).keyed("A"),
                            UI.mounted(awaiting(shared, entered, releasedB, "B")).keyed("B")
                        )
                    else span("gone")
                ))
                _ <- mount(ui) { (recorder, _) =>
                    for
                        _ <- entered.await
                        _ <- show.set(false)
                        _ <- showsEventually(recorder, Seq("0"), "gone")
                        _ <- assertEventually(bothReleased(releasedA, releasedB))
                        _ <- reportsNoFailure(errors)
                    yield ()
                }
            yield succeed
        }
    }

    "two keyless mounts that await one promise and are released in one re-render leave no error view in the snapshot" in {
        val errors = new ErrorLog
        Log.let(Log(errors)) {
            for
                shared    <- Promise.init[Unit, Any]
                later     <- Promise.init[Unit, Any]
                entered   <- Latch.init(2)
                releasedA <- AtomicBoolean.init(false)
                releasedB <- AtomicBoolean.init(false)
                tick      <- Signal.initRef(0)
                // The instances after the re-render wait on a promise of their own, so the slots keep showing the KeepLatest snapshots.
                effect = (n: Int, name: String, released: AtomicBoolean) =>
                    if n == 0 then awaiting(shared, entered, released, name) else later.get.andThen(span(s"$name:later"))
                ui = div(tick.render(n =>
                    div(span(s"tick:$n"), UI.mounted(effect(n, "A", releasedA)), UI.mounted(effect(n, "B", releasedB)))
                ))
                _ <- mount(ui) { (recorder, _) =>
                    for
                        _ <- entered.await
                        _ <- tick.set(1)
                        _ <- showsEventually(recorder, Seq("0", "0"), "tick:1")
                        _ <- assertEventually(bothReleased(releasedA, releasedB))
                        a <- recorder.shown(Seq("0", "1"))
                        b <- recorder.shown(Seq("0", "2"))
                        _ <- reportsNoFailure(errors)
                    yield assert(
                        !a.contains("kyo-mount-error") && !b.contains("kyo-mount-error"),
                        s"a released instance's snapshot shows an error view: A shows $a, B shows $b"
                    )
                }
            yield succeed
        }
    }

    "a keyless and a keyed mount that await one promise and are released in one pass report no failure" in {
        val errors = new ErrorLog
        Log.let(Log(errors)) {
            for
                shared    <- Promise.init[Unit, Any]
                entered   <- Latch.init(2)
                releasedA <- AtomicBoolean.init(false)
                releasedB <- AtomicBoolean.init(false)
                show      <- Signal.initRef(true)
                ui = div(show.render(s =>
                    if s then
                        div(
                            UI.mounted(awaiting(shared, entered, releasedA, "A")),
                            UI.mounted(awaiting(shared, entered, releasedB, "B")).keyed("B")
                        )
                    else span("gone")
                ))
                // B hears A's release while its region still holds it, so its verdict waits for the pass and the cut that release it.
                _ <- mountSession(ui) { (recorder, sub) =>
                    for
                        _ <- entered.await
                        _ <- show.set(false)
                        _ <- showsEventually(recorder, Seq("0"), "gone")
                        _ <- assertEventually(bothReleased(releasedA, releasedB))
                        _ <- assertEventually(pendingVerdicts(sub.mountSession).map(_ == 0))
                        _ <- reportsNoFailure(errors)
                    yield ()
                }
            yield succeed
        }
    }

    "a keyed mount that moves while an interrupt from elsewhere ends its effect shows its error view at its new place" in {
        val errors = new ErrorLog
        Log.let(Log(errors)) {
            for
                shared    <- Promise.init[Unit, Any]
                entered   <- Latch.init(2)
                releasedK <- AtomicBoolean.init(false)
                releasedX <- AtomicBoolean.init(false)
                where     <- Signal.initRef(1)
                // One call site, so both regions show the same identity: a write that switches the regions moves the instance.
                x  = UI.mounted(awaiting(shared, entered, releasedX, "X")).keyed("X")
                ui = div(
                    where.render(w =>
                        if w == 1 then div(UI.mounted(awaiting(shared, entered, releasedK, "K")), x) else span("first empty")
                    ),
                    where.render(w => if w == 2 then div(x) else span("second empty"))
                )
                _ <- mount(ui) { (recorder, _) =>
                    for
                        _     <- entered.await
                        _     <- where.set(2)
                        _     <- assertEventually(releasedK.get)
                        _     <- showsEventually(recorder, Seq("1", "0"), "kyo-mount-error")
                        gone  <- releasedX.get
                        shown <- recorder.shown(Seq("1", "0"))
                        log   <- errors.logged
                    yield assert(
                        !gone && log.exists(_.contains("Mounted effect panicked")),
                        s"X was released: $gone, it shows $shown, logged $log"
                    )
                }
            yield succeed
        }
    }

    "a keyless mount whose effect is interrupted by the release of a keyed mount shows its error view" in {
        for
            shared    <- Promise.init[Unit, Any]
            entered   <- Latch.init(2)
            releasedA <- AtomicBoolean.init(false)
            releasedB <- AtomicBoolean.init(false)
            showA     <- Signal.initRef(true)
            ui = div(
                showA.render(s => if s then div(UI.mounted(awaiting(shared, entered, releasedA, "A")).keyed("A")) else span("gone")),
                div(UI.mounted(awaiting(shared, entered, releasedB, "B")))
            )
            _ <- mount(ui) { (recorder, _) =>
                for
                    _     <- entered.await
                    _     <- showA.set(false)
                    _     <- assertEventually(releasedA.get)
                    _     <- showsEventually(recorder, Seq("1", "0"), "kyo-mount-error")
                    gone  <- releasedB.get
                    shown <- recorder.shown(Seq("1", "0"))
                yield assert(!gone, s"B was released though nothing removed it, it shows $shown")
            }
        yield succeed
    }

    "a keyless mount in a region nested in a released value reports no failure when its sibling's release reaches it" in {
        val errors = new ErrorLog
        Log.let(Log(errors)) {
            for
                shared    <- Promise.init[Unit, Any]
                entered   <- Latch.init(2)
                releasedA <- AtomicBoolean.init(false)
                releasedB <- AtomicBoolean.init(false)
                show      <- Signal.initRef(true)
                inner     <- Signal.initRef(0)
                // The nested region's subscription is interrupted with the value around it, so its own value releases on a fiber of
                // its own that nothing waits for: B can hear A's release before its own has begun.
                ui = div(show.render(s =>
                    if s then
                        div(
                            UI.mounted(awaiting(shared, entered, releasedA, "A")),
                            inner.render(n => div(span(s"inner:$n"), UI.mounted(awaiting(shared, entered, releasedB, "B"))))
                        )
                    else span("gone")
                ))
                _ <- mount(ui) { (recorder, _) =>
                    for
                        _ <- entered.await
                        _ <- show.set(false)
                        _ <- showsEventually(recorder, Seq("0"), "gone")
                        _ <- assertEventually(bothReleased(releasedA, releasedB))
                        _ <- reportsNoFailure(errors)
                    yield ()
                }
            yield succeed
        }
    }

    "a mount in a boundary whose effect is interrupted by the release of another mount shows the boundary's error view" in {
        for
            shared    <- Promise.init[Unit, Any]
            entered   <- Latch.init(2)
            releasedA <- AtomicBoolean.init(false)
            releasedB <- AtomicBoolean.init(false)
            showA     <- Signal.initRef(true)
            caught: PartialFunction[MountError, UI] = { case e => span(s"caught:${e.source}") }
            ui                                      = div(
                showA.render(s => if s then div(UI.mounted(awaiting(shared, entered, releasedA, "A")).keyed("A")) else span("gone")),
                UI.boundary.onError(caught)(div(UI.mounted(awaiting(shared, entered, releasedB, "B")).keyed("B")))
            )
            _ <- mount(ui) { (recorder, _) =>
                for
                    _     <- entered.await
                    _     <- showA.set(false)
                    _     <- assertEventually(releasedA.get)
                    _     <- showsEventually(recorder, Seq("1", "0"), "caught:Panic")
                    gone  <- releasedB.get
                    shown <- recorder.shown(Seq("1", "0"))
                yield assert(!gone, s"B was released though nothing removed it, it shows $shown")
            }
        yield succeed
    }

    "a verdict that waits for a cut held back by a batch is settled by the end of the session" in {
        val errors = new ErrorLog
        Log.let(Log(errors)) {
            for
                shared    <- Promise.init[Unit, Any]
                entered   <- Latch.init(2)
                hold      <- Promise.init[Unit, Any]
                releasedK <- AtomicBoolean.init(false)
                releasedX <- AtomicBoolean.init(false)
                show      <- Signal.initRef(true)
                ui = div(show.render(s =>
                    if s then
                        div(
                            UI.mounted(awaiting(shared, entered, releasedK, "K")),
                            UI.mounted(awaiting(shared, entered, releasedX, "X")).keyed("X")
                        )
                    else span("gone")
                ))
                recorder = new Recorder()
                _    <- recorder.initial(ui)
                root <- ReactiveUI.normalize(ui, Seq.empty)
                // The write runs in a batch that stays open, so the cut that would decide X is not taken while the session lives.
                (session, batch) <- Scope.run {
                    for
                        sub   <- subscribeAudited(root, recorder)
                        _     <- entered.await
                        batch <- Fiber.initUnscoped(sub.mountSession.within(show.set(false).andThen(hold.get)))
                        _     <- assertEventually(releasedK.get)
                        _     <- assertEventually(pendingVerdicts(sub.mountSession).map(_ == 1))
                        gone  <- releasedX.get
                        _     <- assertConsistent(sub)
                    yield
                        assert(!gone, "X was released while the batch held its cut back")
                        (sub.mountSession, batch)
                }
                _ <- assertEventually(releasedX.get)
                _ <- assertEventually(pendingVerdicts(session).map(_ == 0))
                _ <- hold.completeUnitDiscard
                _ <- batch.get
                _ <- reportsNoFailure(errors)
            yield succeed
        }
    }

    "an instance released while its effect is pending does not end failed" in {
        val errors = new ErrorLog
        Log.let(Log(errors)) {
            for
                entered <- Latch.init(1)
                never   <- Promise.init[Unit, Any]
                node = UI.mounted(entered.release.andThen(never.get).andThen(span("never")))
                (table, _) <- ReactiveUI.MountTable.root(Seq.empty)
                instance   <- ReactiveUI.MountInstance.start(MountedSpec(node, Seq("0"), Nil), Absent, Nil, table)
                _          <- entered.await
                _          <- instance.teardown
                state      <- instance.cell.current
                _          <- reportsNoFailure(errors)
            yield assert(!state.settled, s"the released instance ended as $state")
        }
    }

    private def draftBox(editing: Boolean, draft: SignalRef[String], b: Signal[String], runs: AtomicInt)(using Frame): UI =
        div(
            if editing then input.value(draft) else span("read only"),
            b.render(_ => div(UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("box")))
        )

    "a bound input appearing before a region does not restart the region's keyed instance" in {
        for
            runs    <- AtomicInt.init(0)
            editing <- Signal.initRef(false)
            draft   <- Signal.initRef("draft")
            b       <- Signal.initRef("b")
            ui = div(editing.render(e => draftBox(e, draft, b, runs)))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "runs:")
                    _ <- afterRepaint(recorder, Seq("0"))(editing.set(true))
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "runs:")
                    _ <- afterRepaint(recorder, Seq("0"))(editing.set(false))
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "runs:")
                yield ()
            }
            total <- runs.get
        yield assert(total == 1, s"a bound input before the region restarted its keyed instance: $total runs")
    }

    "rows spliced into a fragment whose mounts share a key without data are reported, not paired by position" in {
        for
            runs  <- AtomicRef.init(Map.empty[String, Int])
            order <- Signal.initRef(Chunk("A", "B"))
            ui = div(order.render(o => ul(fragment(o.toSeq.map(id => li(span(id), UI.mounted(counted(runs, id)).keyed("note")))*))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0", "0", "1"), "kyo-mount-error")
                    _ <- showsEventually(recorder, Seq("0", "0", "1", "1"), "kyo-mount-error")
                    _ <- afterRepaint(recorder, Seq("0"))(order.set(Chunk("B", "A")))
                    _ <- showsEventually(recorder, Seq("0", "0", "0", "1"), "kyo-mount-error")
                    // Each row's error paints on its own after the list's repaint: wait for the second row's too before reading it.
                    _     <- showsEventually(recorder, Seq("0", "0", "1", "1"), "kyo-mount-error")
                    shown <- recorder.shown(Seq("0", "0", "1", "1"))
                yield assert(shown.contains("kyo-mount-error") && !shown.contains("A:") && !shown.contains("B:"), shown)
            }
            counts <- runs.get
        yield assert(counts.isEmpty, s"a duplicated identity ran its effect: $counts")
    }

    "an unkeyed foreach puts the row index into the identity, so a data-keyed mount restarts when its row shifts" in {
        for
            runs  <- AtomicRef.init(Map.empty[String, Int])
            items <- Signal.initRef(Chunk("B"))
            ui = div(items.foreach(id => div(UI.mounted(counted(runs, id)).keyed(id))))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0", "0"), "B:1")
                    _ <- items.set(Chunk("A", "B"))
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "B:2")
                    _ <- showsEventually(recorder, Seq("0", "0", "0"), "A:1")
                yield ()
            }
            counts <- runs.get
        yield assert(counts == Map("A" -> 1, "B" -> 2), s"runs: $counts")
    }

    "a keyed mount inside a keyless one restarts with it, inside a keyed one it is kept" in {
        for
            runs <- AtomicRef.init(Map.empty[String, Int])
            tick <- Signal.initRef(0)
            inner = (name: String) => UI.mounted(counted(runs, name)).keyed("inner")
            ui    = div(tick.render(n =>
                div(
                    span(s"tick:$n"),
                    UI.mounted(Kyo.lift(div(inner("in-keyless")))),
                    UI.mounted(Kyo.lift(div(inner("in-keyed")))).keyed("outer")
                )
            ))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "in-keyless:1")
                    _ <- showsEventually(recorder, Seq("0", "2", "0"), "in-keyed:1")
                    _ <- afterRepaint(recorder, Seq("0"))(tick.set(1))
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "in-keyless:2")
                    _ <- showsEventually(recorder, Seq("0", "2", "0"), "in-keyed:")
                yield ()
            }
            counts <- runs.get
        yield assert(counts == Map("in-keyless" -> 2, "in-keyed" -> 1), s"runs: $counts")
    }

    "a row of nested keyed lists is identified with its group: moving groups restarts it, two groups at once are not a duplicate" in {
        for
            runs   <- AtomicRef.init(Map.empty[String, Int])
            groups <- Signal.initRef(Chunk(("open", Chunk("X")), ("done", Chunk.empty[String])))
            ui = div(groups.foreachKeyed(_._1)((group, ids) =>
                div(
                    span(group),
                    Signal.initConst(ids).foreachKeyed(identity)(id => div(UI.mounted(counted(runs, s"$id@$group")).keyed("row")))
                )
            ))
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "open", "1", "X", "0"), "X@open:1")
                    _ <- groups.set(Chunk(("open", Chunk.empty[String]), ("done", Chunk("X"))))
                    _ <- showsEventually(recorder, Seq("0", "done", "1", "X", "0"), "X@done:1")
                    _ <- groups.set(Chunk(("open", Chunk("X")), ("done", Chunk("X"))))
                    _ <- showsEventually(recorder, Seq("0", "open", "1", "X", "0"), "X@open:2")
                    _ <- showsEventually(recorder, Seq("0", "done", "1", "X", "0"), "X@done:1")
                yield ()
            }
            counts <- runs.get
        yield assert(counts == Map("X@open" -> 2, "X@done" -> 1), s"runs: $counts")
    }

    // ---- Moves between regions that render separately: decided at a cut, never by render order ----

    /** A recorder that, once armed, holds the next paint at `held` until `gate` completes: the region painting there is stuck in the
      * middle of a pass, so any later value of it waits behind that pass, and the other regions run first.
      *
      * The held region must render the value written to hold it (a nonce): its first pass can run after that write, and a pass whose
      * content equals what the page already shows paints nothing, so a write it does not render would never reach the gate.
      */
    private class Gated(held: Seq[String], entered: Promise[Unit, Any], gate: Promise[Unit, Any]) extends Recorder():
        private val armed = new java.util.concurrent.atomic.AtomicBoolean(false)

        def arm(): Unit = armed.set(true)

        override def onChange(
            region: ReactiveRegion,
            path: Seq[String],
            context: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            ui: UI
        )(using Frame): Unit < Async =
            if path == held && armed.compareAndSet(true, false) then
                entered.completeUnitDiscard.andThen(gate.get).andThen(super.onChange(region, path, context, parentContext, previous, ui))
            else super.onChange(region, path, context, parentContext, previous, ui)
        end onChange
    end Gated

    private def mountWith[A](recorder: Recorder, ui: UI)(
        f: ((Seq[String], UIEvent) => Boolean < Async) => A < (Async & Scope)
    )(using Frame, kyo.test.AssertScope): A < Async =
        Scope.run {
            for
                _    <- recorder.initial(ui)
                root <- ReactiveUI.normalize(ui, Seq.empty)
                sub  <- subscribeAudited(root, recorder)
                a    <- f(sub.handle)
                _    <- assertConsistent(sub)
            yield a
        }

    /** A dock: one keyed panel shown either in the left region or in the right one, each region over a signal of its own, both set in
      * one click handler. The region `held` is stuck in a pass of an unrelated value (its nonce) while the click runs, so the other
      * region renders the move first; then it is let go.
      */
    private def dockMove(holdLoser: Boolean, ticks: Int = 0)(using Frame, kyo.test.AssertScope) =
        for
            runs     <- AtomicInt.init(0)
            released <- AtomicInt.init(0)
            left     <- Signal.initRef((true, 0))
            right    <- Signal.initRef((false, 0))
            tick     <- Signal.initRef(0)
            entered  <- Promise.init[Unit, Any]
            gate     <- Promise.init[Unit, Any]
            held     = if holdLoser then Seq("1") else Seq("2")
            recorder = new Gated(held, entered, gate)
            node     = UI.mounted(Scope.ensure(released.incrementAndGet.unit).andThen(runs.incrementAndGet.map(r => span(s"runs:$r"))))
                .keyed("docked")
            ui = div(
                // Two writes on the JVM: one change only inside UI.batch.
                button("move").onClick(UI.batch(left.getAndUpdate(p => (false, p._2)).andThen(right.getAndUpdate(p => (true, p._2)))).unit),
                left.render(p => if p._1 then div(node) else div(span(s"empty-left:${p._2}"))),
                right.render(p => if p._1 then div(node) else div(span(s"empty-right:${p._2}"))),
                tick.render(n => span(s"tick:$n"))
            )
            _ <- mountWith(recorder, ui) { handle =>
                for
                    _ <- showsEventually(recorder, Seq("1", "0"), "runs:1")
                    _ <- Sync.defer(recorder.arm())
                    _ <- (if holdLoser then left else right).getAndUpdate(p => (p._1, p._2 + 1))
                    _ <- entered.get
                    _ <- handle(Seq("0"), click("0"))
                    // The region that was not held renders the click.
                    _ <-
                        if holdLoser then showsEventually(recorder, Seq("2", "0"), "runs:")
                        else showsEventually(recorder, Seq("1"), "empty-left")
                    // The waiting place paints the snapshot, then sends its hold: wait for the hold, while the losing region is still held.
                    _         <- if holdLoser then assertEventually(recorder.holds.map(_.nonEmpty)) else Kyo.unit
                    contested <- recorder.holds
                    _         <- Kyo.foreachDiscard(1 to ticks)(n => tick.set(n))
                    _         <- gate.completeUnitDiscard
                    _         <- showsEventually(recorder, Seq("1"), "empty-left")
                    _         <- assertEventually(recorder.holds.map(h => h.size % 2 == 0))
                    _         <- showsEventually(recorder, Seq("2", "0"), "runs:1")
                    total     <- runs.get
                    gone      <- released.get
                yield
                    if holdLoser then
                        assert(
                            contested.map(_._2) == Chunk(true),
                            s"the gaining place showed live content while the losing one held it: $contested"
                        )
                    end if
                    assert(total == 1 && gone == 0, s"runs: $total, released: $gone")
            }
        yield succeed

    "a panel moved between two separately rendered regions in one handler keeps its instance when the gaining region renders first" in {
        dockMove(holdLoser = true)
    }

    "a panel moved between two separately rendered regions in one handler keeps its instance when the losing region renders first" in {
        dockMove(holdLoser = false)
    }

    "a move is decided once the held region has caught up, even while another region keeps re-rendering" in {
        dockMove(holdLoser = false, ticks = 20)
    }

    "a panel dropped by one region that no other region picks up is released" in {
        for
            released <- AtomicBoolean.init(false)
            left     <- Signal.initRef(true)
            other    <- Signal.initRef(0)
            node = UI.mounted(Scope.ensure(released.set(true)).andThen(span("held"))).keyed("dropped")
            ui   = div(
                button("hide").onClick(left.set(false)),
                left.render(l => if l then div(node) else div(span("empty"))),
                other.render(n => span(s"other:$n"))
            )
            _ <- mount(ui) { (recorder, handle) =>
                for
                    _ <- showsEventually(recorder, Seq("1", "0"), "held")
                    _ <- handle(Seq("0"), click("0"))
                    _ <- showsEventually(recorder, Seq("1"), "empty")
                    _ <- assertEventually(released.get)
                yield ()
            }
        yield succeed
    }

    "an identity two separately rendered regions show at once is reported at both, and the one that stays gets an instance" in {
        for
            runs     <- AtomicInt.init(0)
            released <- AtomicInt.init(0)
            showA    <- Signal.initRef(true)
            showB    <- Signal.initRef(false)
            node = UI.mounted(Scope.ensure(released.incrementAndGet.unit).andThen(runs.incrementAndGet.map(r => span(s"runs:$r"))))
                .keyed("twice-apart")
            ui = div(
                button("both").onClick(showB.set(true)),
                showA.render(a => if a then div(node) else div(span("no a"))),
                showB.render(b => if b then div(node) else div(span("no b")))
            )
            _ <- mount(ui) { (recorder, handle) =>
                for
                    _     <- showsEventually(recorder, Seq("1", "0"), "runs:1")
                    _     <- handle(Seq("0"), click("0"))
                    _     <- showsEventually(recorder, Seq("1", "0"), "kyo-mount-error")
                    _     <- showsEventually(recorder, Seq("2", "0"), "kyo-mount-error")
                    first <- recorder.shown(Seq("1", "0"))
                    _     <- assertEventually(released.get.map(_ == 1))
                    _     <- showA.set(false)
                    _     <- showsEventually(recorder, Seq("2", "0"), "runs:2")
                yield assert(first.contains("1.0") && first.contains("2.0"), first)
            }
        yield succeed
    }

    // The gaining region is held in a pass and is behind on a write it has not read yet, so the cut cannot be reached: the panel
    // the losing region dropped is still in transit, undecided, when the session ends.
    "an instance in transit when the session ends is released" in {
        for
            released <- AtomicBoolean.init(false)
            left     <- Signal.initRef(true)
            right    <- Signal.initRef((false, 0))
            entered  <- Promise.init[Unit, Any]
            gate     <- Promise.init[Unit, Any]
            recorder = new Gated(Seq("2"), entered, gate)
            node     = UI.mounted(Scope.ensure(released.set(true)).andThen(span("panel"))).keyed("transit")
            ui       = div(
                button("hide").onClick(left.set(false)),
                left.render(l => if l then div(node) else div(span("empty-left"))),
                right.render(p => if p._1 then div(node) else div(span(s"empty-right:${p._2}")))
            )
            undecided <- mountWith(recorder, ui) { handle =>
                for
                    _ <- showsEventually(recorder, Seq("1", "0"), "panel")
                    _ <- Sync.defer(recorder.arm())
                    _ <- right.getAndUpdate(p => (p._1, p._2 + 1))
                    _ <- entered.get
                    // A write the held region has not read: it is behind until a later pass of it, which never comes.
                    _ <- right.getAndUpdate(p => (p._1, p._2 + 1))
                    _ <- handle(Seq("0"), click("0"))
                    _ <- showsEventually(recorder, Seq("1"), "empty-left")
                    r <- released.get
                yield !r
            }
            r <- released.get
        yield
            assert(undecided, "the panel was released before the session ended: it was not in transit")
            assert(r, "the instance in transit outlived its session")
        end for
    }

    // A cut has started releasing a panel no region shows any more, and the release waits on the panel's own finalizer when the
    // session closes. The session must not end before the release has: virtual time shows whether its close waited.
    "closing the session waits for a release a cut has already started" in {
        val ri = Signal.defaultRepairInterval
        Clock.withTimeControl { control =>
            Scope.run {
                for
                    _         <- Fiber.init(Loop.forever(control.advance(ri, 1.millis)))
                    releasing <- Latch.init(1)
                    gate      <- Latch.init(1)
                    released  <- AtomicBoolean.init(false)
                    show      <- Signal.initRef(true)
                    node = UI.mounted(
                        Scope.ensure(releasing.release.andThen(gate.await).andThen(released.set(true))).andThen(span("panel"))
                    ).keyed("slow-release")
                    ui = div(show.render(s => if s then div(node) else div(span("hidden"))))
                    bodyDone <- Latch.init(1)
                    session  <- Fiber.init(mount(ui) { (recorder, _) =>
                        showsEventually(recorder, Seq("0", "0"), "panel")
                            .andThen(show.set(false))
                            .andThen(releasing.await)
                            .andThen(bodyDone.release)
                    })
                    _ <- bodyDone.await
                    // Only the close is timed: a close that waits for the release cannot end before the gate opens.
                    closed <- Abort.run[Timeout](Async.timeout(ri * 600)(session.get))
                    _      <- gate.release
                    _      <- assertEventually(released.get)
                yield assert(closed.isFailure, s"the session closed while a release it owned was still running: $closed")
            }
        }
    }

    "an instance in transit when the mount that owns its table goes away is released" in {
        for
            released <- AtomicBoolean.init(false)
            outer    <- Signal.initRef(0)
            left     <- Signal.initRef(true)
            right    <- Signal.initRef((false, 0))
            entered  <- Promise.init[Unit, Any]
            gate     <- Promise.init[Unit, Any]
            recorder = new Gated(Seq("1", "0", "1"), entered, gate)
            inner    = UI.mounted(Scope.ensure(released.set(true)).andThen(span("inner"))).keyed("inner-transit")
            owner    = UI.mounted(Kyo.lift(div(
                left.render(l => if l then div(inner) else div(span("empty-left"))),
                right.render(p => if p._1 then div(inner) else div(span(s"empty-right:${p._2}")))
            )))
            ui = div(button("hide").onClick(left.set(false)), outer.render(n => if n == 0 then div(owner) else div(span("owner gone"))))
            _ <- mountWith(recorder, ui) { handle =>
                for
                    _ <- showsEventually(recorder, Seq("1", "0", "0", "0"), "inner")
                    _ <- Sync.defer(recorder.arm())
                    _ <- right.getAndUpdate(p => (p._1, p._2 + 1))
                    _ <- entered.get
                    _ <- handle(Seq("0"), click("0"))
                    _ <- showsEventually(recorder, Seq("1", "0", "0"), "empty-left")
                    _ <- outer.set(1)
                    _ <- showsEventually(recorder, Seq("1"), "owner gone")
                    _ <- assertEventually(released.get)
                    _ <- gate.completeUnitDiscard
                yield ()
            }
        yield succeed
    }

    private def errorBox(label: String, fail: Promise[Unit, Any])(using Frame): UI =
        UI.mounted(UI.fork(fail.get.andThen(Abort.fail(new Exception("feed died")))).andThen(span("box")))
            .onError(e => span(s"$label-error:${e.message}"))
            .keyed("error-box")

    "a failure while an instance is in transit shows in the error view of the node that adopts it" in {
        for
            fail    <- Promise.init[Unit, Any]
            left    <- Signal.initRef((true, 0))
            right   <- Signal.initRef(false)
            entered <- Promise.init[Unit, Any]
            gate    <- Promise.init[Unit, Any]
            recorder = new Gated(Seq("1"), entered, gate)
            ui       = div(
                button("move").onClick(UI.batch(left.getAndUpdate(p => (false, p._2)).andThen(right.set(true))).unit),
                left.render(p => if p._1 then div(errorBox("left", fail)) else div(span("empty-left"))),
                right.render(r => if r then div(errorBox("right", fail)) else div(span("empty-right")))
            )
            _ <- mountWith(recorder, ui) { handle =>
                for
                    _ <- showsEventually(recorder, Seq("1", "0"), "box")
                    _ <- Sync.defer(recorder.arm())
                    _ <- left.getAndUpdate(p => (p._1, p._2 + 1))
                    _ <- entered.get
                    _ <- handle(Seq("0"), click("0"))
                    _ <- assertEventually(recorder.paintsAt(Seq("2")).map(_ >= 1))
                    _ <- fail.completeUnitDiscard
                    _ <- gate.completeUnitDiscard
                    _ <- showsEventually(recorder, Seq("2", "0"), "right-error:feed died")
                yield ()
            }
        yield succeed
    }

    private def writerDock(batched: Boolean)(using Frame, kyo.test.AssertScope) =
        for
            runs    <- AtomicInt.init(0)
            left    <- Signal.initRef(true)
            right   <- Signal.initRef(false)
            trigger <- Promise.init[Unit, Any]
            between <- Promise.init[Unit, Any]
            node = UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("written")
            move = left.set(false).andThen(between.get).andThen(right.set(true))
            // The writer is a fiber the effect of another mount forked: its writes are no event handler's.
            writer = UI.mounted(UI.fork(trigger.get.andThen(if batched then UI.batch(move) else move)).andThen(span("writer")))
            ui     = div(
                writer,
                left.render(l => if l then div(node) else div(span("empty-left"))),
                right.render(r => if r then div(node) else div(span("empty-right")))
            )
            total <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("1", "0"), "runs:1")
                    _ <- trigger.completeUnitDiscard
                    // The losing region renders while the writer is between its two writes.
                    _ <- showsEventually(recorder, Seq("1"), "empty-left")
                    _ <- between.completeUnitDiscard
                    _ <- showsEventually(recorder, Seq("2", "0"), "runs:")
                    n <- runs.get
                yield n
            }
        yield total

    "a move a background fiber writes in two steps inside UI.batch keeps its instance" in {
        writerDock(batched = true).map(total => assert(total == 1, s"runs: $total"))
    }

    // Two writes with time between them and no batch are two changes: the first is decided before the second is made. This pins the
    // documented behaviour, not a guarantee to keep.
    "a move a background fiber writes in two steps without UI.batch is two changes: released, then started again" in {
        writerDock(batched = false).map(total => assert(total == 2, s"runs: $total"))
    }

    "a large keyed list of regions without mounts records nothing and scans nothing per pass" in {
        val rows = 2000
        for
            items <- Signal.initRef(Chunk.from((1 to rows).map(_.toString)))
            tick  <- Signal.initRef(0)
            ui       = div(items.foreachKeyed(identity)(id => div(tick.render(n => span(s"$id:$n")))))
            recorder = new Recorder(keepsRows = true) // patches the list as the DOM backend does
            counts <- Scope.run {
                for
                    _    <- recorder.initial(ui)
                    root <- ReactiveUI.normalize(ui, Seq.empty)
                    sub  <- subscribeAudited(root, recorder)
                    c = sub.mountSession.counters
                    _ <- assertEventually(Sync.defer(c.registered.get == rows + 1))
                    _ <- assertEventually(Sync.defer(c.skipped.get >= rows + 1))
                    before = (c.registered.get, c.planned.get, c.scanned.get, c.cuts.get, c.skipped.get)
                    _ <- tick.set(1)
                    _ <- assertEventually(Sync.defer(c.skipped.get >= before._5 + rows))
                    _ <- items.set(Chunk.from((1 to rows).map(_.toString)) :+ "new")
                    _ <- showsEventually(recorder, Seq("0", "new", "0"), "new:1")
                    // The new row's region registers as its row subscribes, which may be after the list painted it.
                    _ <- assertEventually(Sync.defer(c.registered.get == before._1 + 1))
                    after = (c.registered.get, c.planned.get, c.scanned.get, c.cuts.get, c.skipped.get)
                    _ <- assertConsistent(sub)
                yield (before, after)
            }
        yield
            val (
                (regBefore, plannedBefore, scannedBefore, cutsBefore, skippedBefore),
                (regAfter, plannedAfter, scannedAfter, cutsAfter, skippedAfter)
            ) =
                counts
            java.lang.System.out.println(s"MOUNTCOST rows=$rows before=${counts._1} after=${counts._2}")
            assert(plannedBefore == 0 && plannedAfter == 0, s"a pass without mounts planned: ${counts}")
            assert(scannedAfter == 0 && cutsAfter == 0, s"a cut scanned regions with nothing in transit: ${counts}")
            assert(regAfter == regBefore + 1, s"a re-render or a new row registered more than the new row's region: ${counts}")
            assert(skippedAfter - skippedBefore >= rows, s"the tick did not reach every row: ${counts}")
        end for
    }

    /** A keyed list of `rows` rows with a keyed mount in each, two rows swapped, then the first row removed: the live instances the
      * planned passes examined for whether a region still shows them, for the swap and for the removal, and how often an effect ran and
      * was released.
      */
    private def examinedForMountedRows(rows: Int)(using Frame, kyo.test.AssertScope) =
        val order   = (0 until rows).map(i => s"r$i").toVector
        val swapped = order.updated(1, order(rows - 2)).updated(rows - 2, order(1))
        for
            runs     <- AtomicInt.init(0)
            released <- AtomicInt.init(0)
            items    <- Signal.initRef(Chunk.from(order))
            row = (id: String) =>
                UI.mounted(Scope.ensure(released.incrementAndGet.unit).andThen(runs.incrementAndGet.andThen(span(id)))).keyed("m")
            ui       = div(items.foreachKeyed(identity)(id => div(row(id))))
            recorder = new Recorder(keepsRows = true) // patches the list as the DOM backend does
            counts <- Scope.run {
                for
                    _    <- recorder.initial(ui)
                    root <- ReactiveUI.normalize(ui, Seq.empty)
                    sub  <- subscribeAudited(root, recorder)
                    c = sub.mountSession.counters
                    _ <- assertEventually(runs.get.map(_ == rows))
                    examinedBefore = c.examined.get
                    plannedBefore  = c.planned.get
                    _ <- items.set(Chunk.from(swapped))
                    _ <- assertEventually(Sync.defer(c.planned.get > plannedBefore))
                    examinedSwap = c.examined.get
                    _ <- items.set(Chunk.from(swapped.drop(1)))
                    _ <- assertEventually(released.get.map(_ == 1))
                    examinedRemove = c.examined.get
                    total <- runs.get
                    gone  <- released.get
                    _     <- assertConsistent(sub)
                yield (examinedSwap - examinedBefore, examinedRemove - examinedSwap, total, gone)
            }
        yield counts
        end for
    end examinedForMountedRows

    "a swap and a removal in a keyed list with a keyed mount in every row examine as many instances for 500 rows as for 50" in {
        for
            small <- examinedForMountedRows(50)
            large <- examinedForMountedRows(500)
        yield
            java.lang.System.out.println(s"MOUNTEXAMINED (swap, remove, runs, released) rows=50 $small rows=500 $large")
            assert(small._3 == 50 && small._4 == 1 && large._3 == 500 && large._4 == 1, s"runs and releases: $small, $large")
            assert(
                small._1 == large._1 && small._2 == large._2,
                s"instances examined (swap, remove): ${(small._1, small._2)} for 50 rows, ${(large._1, large._2)} for 500 rows"
            )
    }

    /** `steps` states of two keyed lists over the ids `r0` to `r15`, each one step from the one before: two rows swapped, a row removed,
      * an absent id inserted, a row moved within its list, or a row moved to the other list. Fixed by `seed`; an id is in one list at most.
      */
    private def listSteps(seed: Long, steps: Int): Vector[(Vector[String], Vector[String])] =
        val random                                                   = new scala.util.Random(seed)
        val pool                                                     = (0 until 16).map(i => s"r$i").toVector
        def insert(rows: Vector[String], id: String): Vector[String] =
            val at = random.nextInt(rows.size + 1)
            (rows.take(at) :+ id) ++ rows.drop(at)
        def next(lists: (Vector[String], Vector[String])): (Vector[String], Vector[String]) =
            val (left, right)                                        = lists
            val toLeft                                               = random.nextBoolean()
            val (from, to)                                           = if toLeft then (right, left) else (left, right)
            def pair(changed: Vector[String], other: Vector[String]) = if toLeft then (other, changed) else (changed, other)
            val absent                                               = pool.filterNot(id => left.contains(id) || right.contains(id))
            random.nextInt(5) match
                case 0 if from.size >= 2 =>
                    val i = random.nextInt(from.size)
                    val j = (i + 1 + random.nextInt(from.size - 1)) % from.size
                    pair(from.updated(i, from(j)).updated(j, from(i)), to)
                case 1 if from.nonEmpty =>
                    pair(from.patch(random.nextInt(from.size), Nil, 1), to)
                case 2 if absent.nonEmpty =>
                    pair(insert(from, absent(random.nextInt(absent.size))), to)
                case 3 if from.size >= 2 =>
                    val i = random.nextInt(from.size)
                    pair(insert(from.patch(i, Nil, 1), from(i)), to)
                case 4 if from.nonEmpty =>
                    val i = random.nextInt(from.size)
                    pair(from.patch(i, Nil, 1), insert(to, from(i)))
                case _ => next(lists)
            end match
        end next
        Vector.iterate(((0 until 6).map(pool).toVector, (6 until 10).map(pool).toVector), steps + 1)(next)
    end listSteps

    "random swaps, removals, insertions and moves within and between two keyed lists keep one instance per row and a consistent table" in {
        val states = listSteps(seed = 20261022L, steps = 60)
        // Each step starts the ids it adds and releases the ids it takes away; one that moves keeps its instance.
        val expected = states.sliding(2).foldLeft(Vector((states.head._1 ++ states.head._2).map(_ -> 1).toMap -> Map.empty[String, Int])) {
            (acc, pair) =>
                val (runs, released) = acc.last
                val before           = pair(0)._1 ++ pair(0)._2
                val after            = pair(1)._1 ++ pair(1)._2
                val started          = after.filterNot(before.contains)
                val stopped          = before.filterNot(after.contains)
                acc :+ (
                    started.foldLeft(runs)((m, id) => m.updated(id, m.getOrElse(id, 0) + 1)),
                    stopped.foldLeft(released)((m, id) => m.updated(id, m.getOrElse(id, 0) + 1))
                )
        }
        def order(html: String, list: String): Vector[String] =
            s"""data-kyo-path="$list\\.([^".]+)"""".r.findAllMatchIn(html).map(_.group(1)).toVector
        for
            runs     <- AtomicRef.init(Map.empty[String, Int])
            released <- AtomicRef.init(Map.empty[String, Int])
            next     <- AtomicRef.init(states.head)
            left     <- Signal.initRef(Chunk.from(states.head._1))
            right    <- Signal.initRef(Chunk.from(states.head._2))
            row = (id: String) =>
                UI.mounted(
                    Scope.ensure(released.updateAndGet(m => m.updated(id, m.getOrElse(id, 0) + 1)).unit)
                        .andThen(runs.updateAndGet(m => m.updated(id, m.getOrElse(id, 0) + 1)).map(m => span(s"[$id:${m(id)}]")))
                ).keyed("row")
            ui = div(
                // Both lists change in one click, as one change: a row moved between them keeps its instance.
                button("go").onClick(next.get.map((l, r) => UI.batch(left.set(Chunk.from(l)).andThen(right.set(Chunk.from(r))))).unit),
                left.foreachKeyed(identity)(id => div(row(id))),
                right.foreachKeyed(identity)(id => div(row(id)))
            )
            _ <- mount(ui) { (recorder, handle) =>
                def settled(at: Int)(using Frame, kyo.test.AssertScope): Unit < Async =
                    val ((l, r), (wantRuns, wantReleased)) = (states(at), expected(at))
                    for
                        _ <- assertEventually(runs.get.map(_ == wantRuns))
                        _ <- assertEventually(released.get.map(_ == wantReleased))
                        _ <- Kyo.foreachDiscard(Seq(("1", l, r), ("2", r, l))) { (list, rows, others) =>
                            for
                                _ <- assertEventually(recorder.shown(Seq(list)).map(html => order(html, list) == rows))
                                _ <- Kyo.foreachDiscard(rows)(id => showsEventually(recorder, Seq(list, id, "0"), s"[$id:${wantRuns(id)}]"))
                                _ <- Kyo.foreachDiscard(others)(id =>
                                    assertEventually(recorder.shown(Seq(list, id, "0")).map(html => !html.contains(s"[$id:")))
                                )
                            yield ()
                        }
                    yield ()
                    end for
                end settled
                for
                    _ <- settled(0)
                    _ <- Kyo.foreachDiscard(1 until states.size) { at =>
                        next.set(states(at)).andThen(handle(Seq("0"), click("0"))).andThen(settled(at))
                    }
                    finalRuns     <- runs.get
                    finalReleased <- released.get
                yield
                    assert(finalRuns == expected.last._1, s"runs: $finalRuns")
                    assert(finalReleased == expected.last._2, s"released: $finalReleased")
                end for
            }
        yield succeed
        end for
    }

    // ---- A handler that waits for what its own writes cause ----

    // The shape of an app that stops its running part and waits until it is gone (Runs.stop in the issue tracker's demo shell): the
    // handler hides a keyed instance and then waits for that instance's release. The release must not wait for the handler.
    "a click handler that hides a keyed instance and waits for its release finishes" in {
        for
            released <- Latch.init(1)
            done     <- Latch.init(1)
            show     <- Signal.initRef(true)
            node = UI.mounted(Scope.ensure(released.release).andThen(span("app"))).keyed("run")
            ui   = div(
                button("stop").onClick(show.set(false).andThen(released.await).andThen(done.release)),
                show.render(s => if s then div(node) else div(span("stopped")))
            )
            _ <- mount(ui) { (recorder, handle) =>
                for
                    _ <- showsEventually(recorder, Seq("1", "0"), "app")
                    _ <- Fiber.init(handle(Seq("0"), click("0")))
                    // The region has dropped the instance: what is left is only its release.
                    _ <- showsEventually(recorder, Seq("1"), "stopped")
                    _ <- done.await
                yield ()
            }
        yield succeed
    }

    // The shape of a mutation handler that stays in flight until the server answers, while the answer is only given once the new
    // keyed content shows (D17 in the issue tracker's tests): the effect of the instance the handler's write shows must not wait for
    // the handler.
    "a click handler in flight does not hold back the effect of the keyed instance its write shows" in {
        for
            key     <- Signal.initRef("a")
            started <- Latch.init(1)
            answer  <- Latch.init(1)
            node = (k: String) => UI.mounted((if k == "b" then started.release else Kyo.unit).andThen(span(s"content-$k"))).keyed(k)
            ui   = div(button("save").onClick(key.set("b").andThen(answer.await)), key.render(k => div(node(k))))
            _ <- mount(ui) { (recorder, handle) =>
                for
                    _      <- showsEventually(recorder, Seq("1", "0"), "content-a")
                    before <- recorder.paintsAt(Seq("1"))
                    _      <- Fiber.init(handle(Seq("0"), click("0")))
                    // The region has rendered the new key while the handler is still in flight, waiting for `answer`.
                    _ <- assertEventually(recorder.paintsAt(Seq("1")).map(_ > before))
                    _ <- started.await
                    _ <- answer.release
                    _ <- showsEventually(recorder, Seq("1", "0"), "content-b")
                yield ()
            }
        yield succeed
    }

    // The issue tracker's tool bar summary: a write takes a region's value away and a second one, made while the region's pass
    // closes, takes it back, so the region runs no pass for it. A cut must still see that region as current, or a release elsewhere
    // waits for good.
    "a region whose value comes back while its pass closes does not hold back a release elsewhere" in {
        for
            released <- Latch.init(1)
            reverted <- Latch.init(1)
            armed    <- AtomicBoolean.init(false)
            value    <- Signal.initRef(0)
            show     <- Signal.initRef(true)
            // A keyless mount is released as its region's pass closes: its finalizer writes the value back then.
            keyless = UI.mounted(
                Scope.ensure(armed.compareAndSet(true, false).map(go => if go then value.set(0).andThen(reverted.release) else Kyo.unit))
                    .andThen(span("keyless"))
            )
            node = UI.mounted(Scope.ensure(released.release).andThen(span("panel"))).keyed("panel")
            ui   = div(
                value.render(n => div(span(s"value:$n"), keyless)),
                show.render(s => if s then div(node) else div(span("hidden")))
            )
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("1", "0"), "panel")
                    // The value region has run its first pass and waits for the next write, and its keyless mount's effect has run, so its
                    // finalizer is registered: the write and its revert are meant for that pass, not for a first pass that would read them
                    // both at once, and the revert only happens if the finalizer exists when the pass closes.
                    _ <- showsEventually(recorder, Seq("0", "1"), "keyless")
                    _ <- assertEventually(Sync.Unsafe.defer(value.unsafe.waiters() == 1))
                    _ <- armed.set(true)
                    _ <- value.set(1)
                    _ <- reverted.await
                    _ <- show.set(false)
                    _ <- showsEventually(recorder, Seq("1"), "hidden")
                    _ <- released.await
                yield ()
            }
        yield succeed
    }

    // ---- Regions the cut cannot read exactly, and a superseded subscription ----

    // A region over a signal built with `Signal.initRaw` is observed through the repairing loop: it has no exact read, so whether a
    // write landed since its pass cannot be told. A cut counts it as behind until its next re-read, which the repair timer brings
    // at the latest. Virtual time drives that timer.
    "a panel moved into a region over an initRaw signal keeps its instance when the losing region renders first" in {
        val ri = Signal.defaultRepairInterval
        Clock.withTimeControl { control =>
            Scope.run {
                for
                    _        <- Fiber.init(Loop.forever(control.advance(ri, 1.millis)))
                    runs     <- AtomicInt.init(0)
                    released <- AtomicInt.init(0)
                    left     <- Signal.initRef(true)
                    right    <- Signal.initRef((false, 0))
                    entered  <- Promise.init[Unit, Any]
                    gate     <- Promise.init[Unit, Any]
                    recorder = new Gated(Seq("2"), entered, gate)
                    rawRight = Signal.initRaw[(Boolean, Int)](
                        [B, S] => (f: ((Boolean, Int)) => B < S) => right.currentWith(f),
                        [B, S] => (f: ((Boolean, Int)) => B < S) => right.nextWith(f)
                    )
                    node = UI.mounted(Scope.ensure(released.incrementAndGet.unit).andThen(runs.incrementAndGet.map(r => span(s"runs:$r"))))
                        .keyed("raw-docked")
                    ui = div(
                        button("move").onClick(UI.batch(left.set(false).andThen(right.getAndUpdate(p => (true, p._2)))).unit),
                        left.render(l => if l then div(node) else div(span("empty-left"))),
                        rawRight.render(p => if p._1 then div(node) else div(span(s"empty-right:${p._2}")))
                    )
                    _ <- mountWith(recorder, ui) { handle =>
                        for
                            _ <- showsEventually(recorder, Seq("1", "0"), "runs:1")
                            // The gaining region is held in a pass of an unrelated value, so the losing one renders the move first.
                            _     <- Sync.defer(recorder.arm())
                            _     <- right.getAndUpdate(p => (p._1, p._2 + 1))
                            _     <- entered.get
                            _     <- handle(Seq("0"), click("0"))
                            _     <- showsEventually(recorder, Seq("1"), "empty-left")
                            _     <- gate.completeUnitDiscard
                            _     <- showsEventually(recorder, Seq("2", "0"), "runs:")
                            total <- runs.get
                            gone  <- released.get
                        yield assert(total == 1 && gone == 0, s"runs: $total, released: $gone")
                    }
                yield succeed
            }
        }
    }

    // A region without a mount has no entry in its table, so nothing records which of its subscriptions is current. Here the old
    // subscription of such a region is in the middle of a pass that gains the region's first keyed mount, held in its paint, when
    // its parent re-renders and subscribes the region again.
    "a superseded region that gains its first keyed mount while its parent re-renders starts no instance of its own" in {
        for
            runs     <- AtomicInt.init(0)
            released <- AtomicInt.init(0)
            outer    <- Signal.initRef(0)
            inner    <- Signal.initRef(false)
            entered  <- Promise.init[Unit, Any]
            gate     <- Promise.init[Unit, Any]
            recorder = new Gated(Seq("0", "1"), entered, gate)
            node     = UI.mounted(Scope.ensure(released.incrementAndGet.unit).andThen(runs.incrementAndGet.map(r => span(s"runs:$r"))))
                .keyed("first-mount")
            ui = div(outer.render(n => div(span(s"outer:$n"), inner.render(v => if v then div(node) else div(span("no mount"))))))
            _ <- mountWith(recorder, ui) { _ =>
                for
                    // Both regions have run their first pass and observe their signals: the inner one is the subscription that will
                    // be superseded.
                    _ <- assertEventually(Sync.Unsafe.defer(outer.unsafe.waiters() == 1 && inner.unsafe.waiters() == 1))
                    _ <- Sync.defer(recorder.arm())
                    _ <- inner.set(true)
                    // The old subscription has recorded its walk and is held in its paint.
                    _ <- entered.get
                    _ <- outer.set(1)
                    _ <- showsEventually(recorder, Seq("0"), "outer:1")
                    _ <- showsEventually(recorder, Seq("0", "1", "0"), "runs:")
                    _ <- gate.completeUnitDiscard
                    // Only the new subscription of the inner region still observes it once the old one is gone.
                    _     <- assertEventually(Sync.Unsafe.defer(inner.unsafe.waiters() == 1))
                    shown <- recorder.shown(Seq("0", "1", "0"))
                    total <- runs.get
                    gone  <- released.get
                yield assert(
                    total == 1 && gone == 0 && shown.contains("runs:1"),
                    s"runs: $total, released: $gone, shown: $shown"
                )
            }
        yield succeed
    }

    // The second place's claim is held in its paint, so the cut that finds the identity in two regions runs before that claim is
    // filed. The report still names both places: they come from what the regions' passes recorded, not from the claims filed so far.
    "a duplicate found across separate regions names every place, also one whose claim is not filed yet" in {
        for
            showA   <- Signal.initRef(true)
            showB   <- Signal.initRef(false)
            entered <- Promise.init[Unit, Any]
            gate    <- Promise.init[Unit, Any]
            recorder = new Gated(Seq("2"), entered, gate)
            node     = UI.mounted(Kyo.lift(span("panel"))).keyed("named-twice")
            ui       = div(
                button("both").onClick(showB.set(true)),
                showA.render(a => if a then div(node) else div(span("no a"))),
                showB.render(b => if b then div(node) else div(span("no b")))
            )
            _ <- mountWith(recorder, ui) { handle =>
                for
                    _     <- showsEventually(recorder, Seq("1", "0"), "panel")
                    _     <- Sync.defer(recorder.arm())
                    _     <- handle(Seq("0"), click("0"))
                    _     <- entered.get
                    _     <- showsEventually(recorder, Seq("1", "0"), "kyo-mount-error")
                    shown <- recorder.shown(Seq("1", "0"))
                    _     <- gate.completeUnitDiscard
                yield assert(shown.contains("shown in 2 places at once: 1.0, 2.0"), shown)
            }
        yield succeed
    }

    // A cut's decision is taken the moment the last region has caught up, not when the table's settling fiber gets to it. Here that
    // fiber is busy, held in the release of another instance, while a keyed node is hidden (a change of its own, decided at once) and
    // shown again: it is a new instance, made but not run until the release before it is done, never the hidden one adopted back.
    "a keyed node hidden and shown again while an earlier release still runs gets a new instance" in {
        for
            xReleasing <- Latch.init(1)
            xGate      <- Latch.init(1)
            runs       <- AtomicInt.init(0)
            kReleased  <- AtomicBoolean.init(false)
            showX      <- Signal.initRef(true)
            showK      <- Signal.initRef(true)
            nodeX = UI.mounted(Scope.ensure(xReleasing.release.andThen(xGate.await)).andThen(span("x-content"))).keyed("slow")
            nodeK = UI.mounted(Scope.ensure(kReleased.set(true)).andThen(runs.incrementAndGet.map(r => span(s"k:$r"))))
                .placeholder(span("k-pending"))
                .whilePending(PendingPolicy.Placeholder)
                .keyed("again")
            ui = div(
                showX.render(s => if s then div(nodeX) else div(span("no x"))),
                showK.render(s => if s then div(nodeK) else div(span("no k")))
            )
            _ <- mount(ui) { (recorder, _) =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0"), "x-content")
                    _ <- showsEventually(recorder, Seq("1", "0"), "k:1")
                    // The settling fiber is now held in the release of the first panel.
                    _ <- showX.set(false)
                    _ <- xReleasing.await
                    _ <- afterRepaint(recorder, Seq("1"))(showK.set(false))
                    _ <- afterRepaint(recorder, Seq("1"))(showK.set(true))
                    // The region's pass, its claim included, has ended: it waits for its next value.
                    _ <- assertEventually(Sync.Unsafe.defer(showK.unsafe.waiters() == 1))
                    _ <- xGate.release
                    // Once the releases have run, the node shows an instance's content: the new one's, never the hidden one's.
                    _     <- showsEventually(recorder, Seq("1", "0"), "k:")
                    shown <- recorder.shown(Seq("1", "0"))
                    _ = assert(shown.contains("k:2"), s"the hidden instance was adopted back instead of a new one: $shown")
                    _     <- assertEventually(kReleased.get)
                    total <- runs.get
                yield assert(total == 2, s"runs: $total")
            }
        yield succeed
    }

end MountedTest
