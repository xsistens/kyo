package kyo.internal

import java.util.concurrent.atomic.AtomicReference
import kyo.*
import kyo.UI.foreachKeyed
import kyo.UI.render

/** What the engine reports to a [[kyo.UI.devtools]] sink.
  *
  * These are the numbers a devtools overlay puts on the screen, so a break here is not a broken feature but a
  * lying one — which is worse. Four things have to hold, and none of them is covered elsewhere:
  *
  *   - a mount does not count as a render. The subscribe path deliberately SKIPS the first emission when it
  *     still equals what the enclosing paint already put in the DOM, and a counter placed above that skip
  *     would report every region as having rendered once before anything happened;
  *   - a region rebuilt by its parent's repaint is reported, and reported as a different thing from a region
  *     whose own signal fired. That distinction is the whole diagnostic value: "this rendered 200 times"
  *     means something entirely different depending on which of the two it was;
  *   - the three cheap writes — attribute, class, bound text — are NOT repaints. Each is a single DOM
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
    final private class Recording(paint: Maybe[(Int, Boolean)] = Absent, textSink: Boolean = false):
        private val events = new AtomicReference(Vector.empty[UI.RenderEvent])

        val record: UI.RenderEvent => Unit = e => discard(events.updateAndGet(_ :+ e))

        def all: Vector[UI.RenderEvent] = events.get()

        def kinds: Vector[UI.RenderKind] = all.map(_.kind)

        def causes: Vector[UI.RenderCause] = all.map(_.cause)

        def repaints: Vector[UI.RenderEvent] = all.filter(_.kind == UI.RenderKind.Repaint)

        def signalled: Vector[UI.RenderEvent] = all.filter(_.cause == UI.RenderCause.Signal)

        val exchange = new UIExchange:
            def onChange(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                changed: UI
            )(using Frame): Unit < Async =
                paint match
                    case Present((bytes, wasted)) => Devtools.notePaint(bytes, wasted)
                    case Absent                   => Kyo.unit
            override val textPatcherNow: Maybe[(Seq[String], String) => Unit] =
                if textSink then Present((_, _) => ()) else Absent
    end Recording

    /** Mount `ui` under a devtools sink and run `body` inside it.
      *
      * Everything a test does belongs INSIDE this block. The sink rides an inheritable `Local`, so it reaches
      * the region fibers precisely because they are forked from here.
      */
    private def observed[A](rec: Recording, ui: UI)(body: => A < Async)(using Frame): A < (Async & Scope) =
        UI.devtools(rec.record) {
            for
                root   <- ReactiveUI.normalize(ui, Seq.empty)
                _      <- ReactiveUI.subscribe(root, rec.exchange)
                _      <- Async.sleep(100.millis)
                result <- body
            yield result
        }

    "a mount is not a render, and every later emission is one" in {
        Scope.run {
            for
                ref <- Signal.initRef(0)
                rec = new Recording
                events <- observed(rec, UI.div(ref.render(n => UI.span(n.toString)))) {
                    // Each change is awaited before the next is made, and that is not test hygiene but the
                    // subject: a signal is state, not a queue. Two sets landing before the region fiber wakes
                    // produce ONE render of the latest value, so a test that fired both and expected two
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
                // Zero cost, and deliberately so: the content WAS produced, by the enclosing paint, and that
                // paint is charged for it. Charging it here too would make every parent look free.
                assert(
                    created.forall(e => e.durationNanos == 0L && e.bytes == 0 && !e.wasted),
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
                after <- observed(rec, ui) {
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

    "an attribute channel is a Channel write, not a repaint" in {
        Scope.run {
            for
                ref <- Signal.initRef("a")
                rec = new Recording
                names <- observed(rec, UI.div(UI.span("x").title(ref: Signal[String]))) {
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
                _ <- observed(rec, UI.div(ref: Signal[String])) {
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
                patches <- observed(rec, UI.ul(rows.foreachKeyed(identity)(item => UI.li(item)))) {
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
                moved <- observed(rec, UI.ul(rows.foreachKeyed(identity)(item => UI.li(item)))) {
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
                // The DOM moved, so this is not a render for nothing — even though not one row was repainted.
                assert(!moved.head.wasted, "a reorder was reported as wasted")
            end for
        }
    }

    "what the backend measured about the paint reaches the event" in {
        Scope.run {
            for
                ref <- Signal.initRef(0)
                rec = new Recording(paint = Present((123, true)))
                painted <- observed(rec, UI.div(ref.render(n => UI.span(n.toString)))) {
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

end DevtoolsReportTest
