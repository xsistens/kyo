package kyo.internal

import java.util.concurrent.atomic.AtomicReference
import kyo.*
import kyo.UI.foreachKeyed
import kyo.UI.render

/** What the engine reports to a [[kyo.UI.devtools]] sink.
  *
  * These are the numbers a devtools overlay puts on the screen, so a break here is not a broken feature but a
  * lying one, which is worse. Four things have to hold, and none of them is covered elsewhere:
  *
  *   - a mount does not count as a render. The subscribe path deliberately skips the first emission when it
  *     still equals what the enclosing paint already put in the DOM, and a counter placed above that skip
  *     would report every region as having rendered once before anything happened;
  *   - a region rebuilt by its parent's repaint is reported, and reported as a different thing from a region
  *     whose own signal fired. That distinction is the whole diagnostic value: "this rendered 200 times"
  *     means something entirely different depending on which of the two it was;
  *   - the three cheap writes (attribute, class, bound text) are not repaints. Each is a single DOM
  *     assignment that re-renders nothing, and counting them as renders would point a reader at the one thing
  *     in the engine that is already optimal;
  *   - a keyed list reports the rows it actually repainted, not the rows it holds.
  */
class DevtoolsReportTest extends kyo.test.Test[Any]:

    /** Collects every reported event, and optionally plays the part of a backend that measured its paint.
      *
      * `paint` is what a real backend writes into the probe: bytes that reached the DOM, and whether the
      * render changed nothing. The shared layer's job is to carry that from the exchange into the event, and a
      * stub that never reports would leave exactly that untested.
      */
    final private class Recording(paint: Maybe[(Int, Boolean)] = Absent, textSink: Boolean = false, keepsRows: Boolean = false):
        private val events = new AtomicReference(Vector.empty[UI.RenderEvent])

        val record: UI.RenderEvent => Unit = e => discard(events.updateAndGet(_ :+ e))

        def all: Vector[UI.RenderEvent] = events.get()

        def kinds: Vector[UI.RenderKind] = all.map(_.kind)

        def causes: Vector[UI.RenderCause] = all.map(_.cause)

        def repaints: Vector[UI.RenderEvent] = all.filter(_.kind == UI.RenderKind.Repaint)

        def signalled: Vector[UI.RenderEvent] = all.filter(_.cause == UI.RenderCause.Signal)

        val exchange = new RecordingExchange(
            synchronous = textSink,
            keepsRows = keepsRows,
            beforePaint = (_, _) =>
                paint match
                    case Present((bytes, wasted)) => Devtools.notePaint(bytes, wasted)
                    case Absent                   => Kyo.unit
        )
    end Recording

    /** Mount `ui` under a devtools sink and run `body` inside it, once `ready` holds.
      *
      * Everything a test does belongs inside this block. The sink rides an inheritable `Local`, so it reaches
      * the region fibers precisely because they are forked from here. `ready` says when the mount is through, so
      * that what `body` changes is a later emission and not the first one: see [[parkedOn]].
      */
    private def observed[A](rec: Recording, ui: UI, ready: => Boolean < Sync)(body: => A < Async)(using
        Frame,
        kyo.test.AssertScope
    ): A < (Async & Scope) =
        UI.devtools(rec.record) {
            for
                root   <- ReactiveUI.normalize(ui, Seq.empty)
                _      <- ReactiveUI.subscribe(root, rec.exchange)
                _      <- assertEventually(ready)
                result <- body
            yield result
        }

    /** Whether one observer waits on each of `refs`. An observer waits on its signal again only once it has handled the value
      * before, so for a region just subscribed this is the point where its first emission, the one it skips, is through.
      */
    private def parkedOn(refs: Signal.SignalRef[?]*)(using Frame): Boolean < Sync =
        Kyo.foreach(refs)(_.waiters).map(_.forall(_ == 1))

    "a mount is not a render, and every later emission is one" in {
        Scope.run {
            for
                ref <- Signal.initRef(0)
                rec = new Recording
                events <- observed(rec, UI.div(ref.render(n => UI.span(n.toString))), parkedOn(ref)) {
                    // Each change is awaited before the next is made, and that is not test hygiene but the
                    // subject: a signal is state, not a queue. Two sets landing before the region fiber wakes
                    // produce one render of the latest value, so a test that fired both and expected two
                    // would be asserting something the engine correctly does not do.
                    for
                        _ <- ref.set(1)
                        _ <- assertEventually(Sync.defer(rec.signalled.sizeIs == 1))
                        _ <- ref.set(2)
                        _ <- assertEventually(Sync.defer(rec.signalled.sizeIs == 2))
                    yield rec.all
                }
            yield
                val created = events.filter(_.cause == UI.RenderCause.Created)
                assert(
                    created.sizeIs == 1,
                    s"the mount should report exactly one Created, got ${created.size}: ${events.map(_.cause)}"
                )
                // Zero cost, and deliberately so: the content was produced, by the enclosing paint, and that
                // paint is charged for it. Charging it here too would make every parent look free.
                assert(
                    created.forall(e => e.duration == Duration.Zero && e.bytes == 0 && !e.wasted),
                    s"the skipped first emission carries a cost: ${created.head}"
                )
                assert(events.forall(_.kind == UI.RenderKind.Repaint), s"unexpected kinds: ${events.map(_.kind)}")
            end for
        }
    }

    "a region rebuilt by its parent reports Created, its own signal reports Signal" in {
        Scope.run {
            for
                outer <- Signal.initRef(0)
                inner <- Signal.initRef("a")
                rec = new Recording
                ui  = UI.div(outer.render(n => UI.section(UI.span(n.toString), inner.render(s => UI.p(s)))))
                after <- observed(rec, ui, parkedOn(outer, inner)) {
                    for
                        mounted <- Sync.defer(rec.all.size)
                        _       <- outer.set(1)
                        _       <- assertEventually(Sync.defer(rec.all.sizeIs > mounted + 1))
                    yield rec.all.drop(mounted)
                }
            yield
                // Order is deterministic: a region reports its own repaint before it subscribes the children
                // that paint rebuilt, and both happen on the one region fiber.
                assert(after.sizeIs >= 2, s"expected a repaint and a rebuild, got ${after.map(e => (e.path, e.cause))}")
                assert(
                    after(0).cause == UI.RenderCause.Signal && after(1).cause == UI.RenderCause.Created,
                    s"expected the parent's own repaint then the child's rebuild, got ${after.map(e => (e.path, e.cause))}"
                )
                assert(after(0).path != after(1).path, s"parent and child reported the same path: ${after(0).path}")
            end for
        }
    }

    "a mounted node that publishes reports its content paint as one Signal repaint at its own path" in {
        Scope.run {
            for
                gate <- Promise.init[Unit, Any]
                rec = new Recording(paint = Present((7, false)))
                ui  = UI.div(UI.mounted(gate.get.andThen(UI.span("ready"))).placeholder(UI.span("loading")))
                // The mount waits on the gate once its subscription is through.
                events <- observed(rec, ui, gate.waiters.map(_ == 1)) {
                    for
                        before <- Sync.defer(rec.all.size)
                        _      <- gate.completeUnitDiscard
                        _      <- assertEventually(Sync.defer(rec.signalled.nonEmpty))
                    yield (before, rec.all)
                }
            yield
                val (before, all) = events
                // The placeholder is the enclosing paint's work, so the mount reports nothing until it publishes.
                assert(before == 0, s"the placeholder was reported: ${all.take(before)}")
                assert(all.sizeIs == 1, s"expected one report, got ${all.map(e => (e.path, e.kind, e.cause))}")
                assert(
                    all.head.kind == UI.RenderKind.Repaint && all.head.cause == UI.RenderCause.Signal &&
                        all.head.path == Seq("0") && all.head.bytes == 7,
                    s"unexpected report: ${all.head}"
                )
            end for
        }
    }

    "a sink reaches only what its devtools call encloses" in {
        val mine: UI.RenderEvent => Unit = _ => ()
        for
            during <- UI.devtools(mine)(Devtools.currentSink)
            after  <- Devtools.currentSink
        yield assert(during.exists(_ eq mine) && after.isEmpty, s"during: $during, after: $after")
        end for
    }

    // notNative: drives a session over HttpWebSocket.connect, like UIServerWsTest's session leaves.
    "server-push handlers built inside a devtools call report their sessions to its sink".notNative in {
        // kyo-http runs a session on a fiber that does not descend from whoever built the handlers. The session below runs
        // outside the devtools call on purpose: only the sink the handlers took at build time can reach it.
        for
            ref <- Signal.initRef("before")
            rec = new Recording
            app = UI.div(UI.button("go").onClick(ref.set("after")), ref.map(v => UI.span(v)))
            serve <- UI.devtools(rec.record)(UIServer.sessionServing)
            _     <- Scope.run {
                HttpWebSocket.connect(
                    (serverWs: HttpWebSocket) => serve(serverWs, app),
                    (clientWs: HttpWebSocket) =>
                        for
                            _ <- clientWs.take().andThen(clientWs.take())
                            _ <- clientWs.put(HttpWebSocket.Payload.Text(
                                Json.encode[UIEvent](UIEvent.Click(Seq("0"), MouseEventData(UI.Modifiers.none, Absent)))
                            ))
                            _ <- clientWs.take()
                            _ <- assertEventually(Sync.defer(rec.signalled.nonEmpty))
                        yield ()
                )
            }
        yield assert(
            rec.signalled.exists(_.kind == UI.RenderKind.Repaint),
            s"the session's repaint did not reach the sink: ${rec.all.map(e => (e.path, e.kind, e.cause))}"
        )
        end for
    }

    "an attribute channel is a Channel write, not a repaint" in {
        Scope.run {
            for
                ref <- Signal.initRef("a")
                rec = new Recording
                names <- observed(rec, UI.div(UI.span("x").title(ref: Signal[String])), parkedOn(ref)) {
                    for
                        _ <- ref.set("b")
                        _ <- assertEventually(Sync.defer(rec.kinds.exists {
                            case _: UI.RenderKind.Channel => true
                            case _                        => false
                        }))
                    yield rec.kinds.collect { case UI.RenderKind.Channel(name) => name }
                }
            yield
                assert(names.sizeIs == 1, s"expected one channel write, got $names")
                assert(names.head == "title", s"wrong channel name: ${names.head}")
                // A channel write addresses the element and re-renders nothing, so no region repainted.
                assert(
                    rec.repaints.forall(_.cause == UI.RenderCause.Created),
                    s"a channel write caused a repaint: ${rec.repaints.map(_.cause)}"
                )
            end for
        }
    }

    "a bound text signal is a Text write, not a repaint" in {
        Scope.run {
            for
                ref <- Signal.initRef("a")
                rec = new Recording(textSink = true)
                // A synchronous text sink binds inside `subscribe` itself, so there is nothing to wait for.
                _ <- observed(rec, UI.div(ref: Signal[String]), true) {
                    ref.set("b").andThen(assertEventually(Sync.defer(rec.kinds.contains(UI.RenderKind.Text))))
                }
            yield
                assert(rec.kinds.count(_ == UI.RenderKind.Text) == 1, s"expected one text write, got ${rec.kinds}")
                assert(
                    rec.repaints.forall(_.cause == UI.RenderCause.Created),
                    s"a bound text write caused a repaint: ${rec.repaints.map(_.cause)}"
                )
        }
    }

    "a keyed list reports the rows it repainted, not the rows it holds" in {
        Scope.run {
            for
                rows <- Signal.initRef(Chunk("a", "b", "c"))
                rec = new Recording
                patches <- observed(rec, UI.ul(rows.foreachKeyed(identity)(item => UI.li(item))), parkedOn(rows)) {
                    for
                        _ <- rows.getAndUpdate(_.append("d"))
                        _ <- assertEventually(Sync.defer(rec.signalled.nonEmpty))
                    yield rec.signalled.map(_.kind)
                }
            yield assert(
                patches.contains(UI.RenderKind.ListPatch(1, 4)),
                s"appending one row of four should repaint one, got $patches"
            )
        }
    }

    "a reorder repaints no row and is not wasted" in {
        Scope.run {
            for
                rows <- Signal.initRef(Chunk("a", "b", "c"))
                rec = new Recording
                moved <- observed(rec, UI.ul(rows.foreachKeyed(identity)(item => UI.li(item))), parkedOn(rows)) {
                    for
                        _ <- rows.getAndUpdate(c => Chunk.from(c.toSeq.reverse))
                        _ <- assertEventually(Sync.defer(rec.signalled.nonEmpty))
                    yield rec.signalled
                }
            yield
                assert(moved.sizeIs == 1, s"expected one list emission, got ${moved.map(_.kind)}")
                assert(
                    moved.head.kind == UI.RenderKind.ListPatch(0, 3),
                    s"a reorder should repaint no row: ${moved.head.kind}"
                )
                // The DOM moved, so this is not a render for nothing, even though not one row was repainted.
                assert(!moved.head.wasted, "a reorder was reported as wasted")
            end for
        }
    }

    "what the backend measured about the paint reaches the event" in {
        Scope.run {
            for
                ref <- Signal.initRef(0)
                rec = new Recording(paint = Present((123, true)))
                painted <- observed(rec, UI.div(ref.render(n => UI.span(n.toString))), parkedOn(ref)) {
                    ref.set(1).andThen(assertEventually(Sync.defer(rec.signalled.nonEmpty))).andThen(rec.signalled)
                }
            yield
                assert(painted.sizeIs == 1, s"expected one repaint, got ${painted.size}")
                assert(
                    painted.head.bytes == 123 && painted.head.wasted,
                    s"the probe did not reach the event: ${painted.head}"
                )
        }
    }

    /** The losses reported, by what and where, without the frame. */
    private def losses(events: Vector[UI.RenderEvent]): Vector[String] =
        events.collect {
            case UI.RenderEvent(_, _, _, UI.RenderKind.Lost(UI.Loss.StatedValue(element, attribute, _)), _, _, _, _) =>
                s"$element $attribute"
            case UI.RenderEvent(_, _, _, UI.RenderKind.Lost(UI.Loss.UnkeyedMount(_)), _, _, _, _) => "unkeyed mount"
        }

    "a region that runs again reports a default it moves and a mount it starts over, never a binding, and its first draw reports nothing" in {
        Scope.run {
            for
                tick    <- Signal.initRef(0)
                follows <- Signal.initRef("followed")
                bound   <- Signal.initRef("bound")
                rec = new Recording
                ui  = UI.div(tick.render(n =>
                    UI.div(
                        UI.span(n.toString),
                        UI.input.id("plain"),
                        UI.input.id("start").value("start"),
                        UI.input.id("follows").value(follows.readOnly),
                        UI.input.id("bound").value(bound),
                        UI.textarea.cssClass("note").value("note"),
                        UI.checkbox.id("box").checked(false),
                        UI.checkbox.id("free"),
                        UI.select.id("pick")(UI.option.value("a").selected(true)("a"), UI.option.value("b")("b")),
                        // Defaults: the reader owns these, so only a run that moves the default takes something from the reader.
                        UI.input.id("kept").defaultValue("same"),
                        UI.input.id("moved").defaultValue(s"start $n"),
                        UI.checkbox.id("ticked").defaultChecked(n % 2 == 1),
                        UI.select.id("chosen")(UI.option.value("a").defaultSelected(true)("a"), UI.option.value("b")("b")),
                        UI.mounted(Sync.defer(UI.span("unkeyed"))),
                        UI.mounted(Sync.defer(UI.span("keyed"))).keyed("k")
                    )
                ))
                seen <- observed(rec, ui, parkedOn(tick)) {
                    for
                        first <- Sync.defer(losses(rec.all))
                        _     <- tick.set(1)
                        _     <- assertEventually(Sync.defer(rec.signalled.nonEmpty).map(painted => parkedOn(tick).map(painted && _)))
                    yield (first, losses(rec.all))
                }
            yield
                assert(seen._1.isEmpty, s"the first draw reported ${seen._1}")
                assert(
                    seen._2.sorted == Vector("input#moved value", "input#ticked checked", "unkeyed mount"),
                    s"after one run: ${seen._2}"
                )
            end for
        }
    }

    /** The losses a keyed list reports when row `a` changes and row `c` is added, on a backend that patches rows or not. */
    private def listLosses(keepsRows: Boolean)(using Frame, kyo.test.AssertScope): Vector[String] < (Async & Scope) =
        for
            rows <- Signal.initRef(Chunk("a" -> "1", "b" -> "1"))
            rec = new Recording(keepsRows = keepsRows)
            ui  = UI.ul(rows.foreachKeyed(_._1)((key, text) =>
                UI.li(UI.input.id(key).defaultValue(text), UI.input.id(s"$key-bound").value(text))
            ))
            seen <- observed(rec, ui, parkedOn(rows)) {
                for
                    _ <- rows.set(Chunk("a" -> "2", "b" -> "1", "c" -> "1"))
                    _ <- assertEventually(Sync.defer(rec.signalled.nonEmpty).map(painted => parkedOn(rows).map(painted && _)))
                yield losses(rec.all)
            }
        yield seen

    "a keyed row drawn again for a new value reports the default it moves, and a new row reports none" in {
        Scope.run {
            for
                patched <- listLosses(keepsRows = true)
                whole   <- listLosses(keepsRows = false)
            yield
                assert(patched == Vector("input#a value"), s"a backend that patches rows, after a changed and c added: $patched")
                // A backend with no row patch paints the whole list, so the unchanged row b is drawn again too, with the default it had.
                assert(whole == Vector("input#a value"), s"a backend that paints the whole list: $whole")
        }
    }

end DevtoolsReportTest
