package kyo

import java.util.concurrent.atomic.AtomicReference
import kyo.UI.*
import kyo.internal.DrawContext
import kyo.internal.HtmlRenderer
import kyo.internal.MountDispatch
import kyo.internal.MountDraw
import kyo.internal.MountedSpec
import kyo.internal.MountInstance
import kyo.internal.MountState
import kyo.internal.MountTable
import kyo.internal.MouseEventData
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.RecordingExchange
import kyo.internal.UICommands
import kyo.internal.UIEvent
import scala.compiletime.testing.typeChecks

/** A mount whose effect is `Sync`: what it allocates lives with its instance, and what it draws is on the page from the first paint. */
class UIDrawnTest extends kyo.test.Test[Any]:

    /** Waits until the region over `ref` is parked on it again, which it is only once it has handled the value before. */
    private def parked(ref: SignalRef[?])(using Frame, kyo.test.AssertScope): Unit < Async =
        assertEventually(ref.waiters.map(_ == 1))

    /** The instances a node has drawn so far, oldest first, each its own ref. */
    final private class Instances:
        private val all                                = new AtomicReference(Chunk.empty[SignalRef[Int]])
        def seen: Chunk[SignalRef[Int]]                = all.get
        def draw(ref: SignalRef[Int])(using Frame): UI =
            discard(all.updateAndGet(_.append(ref)))
            ref.render(v => span(v.toString))
    end Instances

    /** Waits until the node has drawn its first instance. */
    private def drawn(instances: Instances)(using Frame, kyo.test.AssertScope): SignalRef[Int] < Async =
        assertEventually(Sync.defer(instances.seen.nonEmpty)).andThen(instances.seen.head)

    private def click(path: String*): UIEvent = UIEvent.Click(path, MouseEventData(UI.Modifiers.none, Absent))

    /** `ui` as a browser mount runs it: normalized, rendered into the session's table, then subscribed. */
    private def painted[A](ui: UI, exchange: RecordingExchange)(f: (String, ReactiveUI.Subscription) => A < (Async & Scope))(using
        Frame
    ): A < Async =
        Scope.run {
            UI.Commands.init(_ => Kyo.unit).map { commands =>
                UICommands.commands.let(Present(commands)) {
                    for
                        root <- ReactiveUI.normalize(ui, Seq.empty)
                        html <- MountDispatch.live.let(root.mountDispatch)(HtmlRenderer.render(ui, Seq.empty))
                        sub  <- ReactiveUI.subscribe(root, exchange)
                        a    <- f(html, sub)
                    yield a
                }
            }
        }

    "the static render shows what the effect draws, and runs it once" in {
        for
            draws <- AtomicInt.init(0)
            ui = div(UI.mounted(draws.incrementAndGet.map(n => span(s"drawn $n"))))
            html <- HtmlRenderer.render(ui, Seq.empty)
            n    <- draws.get
        yield
            assert(html.contains("drawn 1"))
            assert(n == 1)
        end for
    }

    "a static render on a boundary around it draws it once too" in {
        for
            draws <- AtomicInt.init(0)
            ui = UI.boundary.placeholder(span("wait"))(div(UI.mounted(draws.incrementAndGet.map(n => span(s"drawn $n")))))
            html <- HtmlRenderer.render(ui, Seq.empty)
            n    <- draws.get
        yield
            assert(html.contains("drawn 1"))
            assert(!html.contains("wait"))
            assert(n == 1)
        end for
    }

    "a waiting mount inside what it draws is caught by the boundary around it" in {
        for
            gate <- Promise.init[Unit, Any]
            ui = UI.boundary.placeholder(span("wait"))(
                div(UI.mounted(Sync.defer(div(UI.mounted(gate.get.andThen(span("late")))))))
            )
            html <- HtmlRenderer.render(ui, Seq.empty)
        yield assert(html.contains("wait"))
        end for
    }

    "the live page takes the draw of the first paint: one draw, no paint of its own, and its state" in {
        val rec = new RecordingExchange
        for
            draws <- AtomicInt.init(0)
            count <- Signal.initRef(0)
            ui = div(UI.mounted(draws.incrementAndGet.andThen(Signal.initRef(5)).map(own =>
                button(own.map(_.toString)).onClick(own.updateAndGet(_ + 1).unit)
            )))
            out <- painted(ui, rec) { (html, sub) =>
                for
                    before <- draws.get
                    paints <- rec.paints
                    _      <- sub.handle(Seq("0", "0"), click("0", "0"))
                    after  <- draws.get
                yield (html, before, paints.map(_.path), after)
            }
            (html, before, paintsAtStart, after) = out
        yield
            assert(html.contains(">5<"))
            assert(before == 1)
            assert(after == 1)
            assert(!paintsAtStart.contains(Seq("0")), s"the first paint already shows the mount, yet it painted at ${paintsAtStart}")
        end for
    }

    "an event before any subscription reaches what the first paint shows, with the state the instance then holds" in {
        val rec = new RecordingExchange
        for
            clicks <- AtomicRef.init(Chunk.empty[Int])
            ui = div(UI.mounted(Signal.initRef(1).map(own =>
                button(own.map(_.toString)).onClick(own.updateAndGet(_ + 1).map(v => clicks.getAndUpdate(_.append(v))).unit)
            )))
            shown <- Scope.run {
                for
                    root  <- ReactiveUI.normalize(ui, Seq.empty)
                    _     <- MountDispatch.live.let(root.mountDispatch)(HtmlRenderer.render(ui, Seq.empty))
                    _     <- Abort.run[Any](root.handle(Seq("0", "0"), click("0", "0")))
                    sub   <- ReactiveUI.subscribe(root, rec)
                    _     <- sub.handle(Seq("0", "0"), click("0", "0"))
                    heard <- clicks.get
                yield heard
            }
        yield assert(shown == Chunk(2, 3))
        end for
    }

    "a server session over a page an earlier render drew paints its own instance, with the same ids" in {
        val rec                   = new RecordingExchange
        def tree(using Frame): UI = div(UI.mounted(UI.id.map(i => span("x").id(i))))
        for
            page <- HtmlRenderer.render(tree, Seq.empty)
            id = """id="([^"]+)"""".r.findFirstMatchIn(page).map(_.group(1)).get
            painted <- Scope.run {
                for
                    root <- ReactiveUI.normalize(tree, Seq.empty).map(ReactiveUI.unpainted)
                    _    <- ReactiveUI.subscribe(root, rec)
                    _    <- rec.awaitOp(_ => true)
                    ps   <- rec.paints
                yield ps
            }
            live <- HtmlRenderer.render(painted.last.ui, Seq("0"))
        yield assert(live.contains(s"""id="$id""""), s"the page has $id, the session painted $live")
        end for
    }

    "a static render does not start what runs while it is shown" in {
        for
            runs <- AtomicInt.init(0)
            ui = div(UI.mounted(UI.whileMounted(runs.incrementAndGet).andThen(span("x"))))
            html <- HtmlRenderer.render(ui, Seq.empty)
            n    <- runs.get
        yield
            assert(html.contains("x"))
            assert(n == 0)
        end for
    }

    "what runs while it is shown starts on the live page and ends with the node" in {
        val rec = new RecordingExchange
        for
            started <- Promise.init[Unit, Any]
            closed  <- Promise.init[Unit, Any]
            show    <- Signal.initRef(true)
            ui = div(show.render(on =>
                if on then
                    UI.mounted(
                        UI.whileMounted(Scope.ensure(closed.completeUnitDiscard).andThen(started.completeUnitDiscard)).andThen(span("x"))
                    )
                else span("none")
            ))
            _ <- painted(ui, rec) { (_, _) =>
                for
                    _ <- started.get
                    d <- closed.done
                    _ <- show.set(false)
                    _ <- closed.get
                yield assert(!d)
            }
        yield succeed
        end for
    }

    "a failure of what runs while it is shown shows the node's error view" in {
        val rec = new RecordingExchange
        val ui  =
            div(UI.mounted(UI.whileMounted(Abort.fail("feed died")).andThen(span("content"))).onError(e => span(s"failed: ${e.failure}")))
        painted(ui, rec) { (html, _) =>
            assert(html.contains("content"))
            rec.awaitOp {
                case RecordingExchange.Op.Paint(_, _, ui) => ui.toString.contains("failed: feed died")
                case _                                    => false
            }.andThen(succeed)
        }
    }

    "an instance whose effect failed before it is shown leaves its first paint to the page only while it shows what the page drew" in {
        // The page shows what the render pass drew. What runs while the node is shown starts with its instance, so it can fail before the
        // subscription that shows the instance reads its state: that state is the error view, which is not on the page, so that
        // subscription has to paint it rather than take the pass's paint as its own.
        val at = ReactiveRegion.RegionIdentity(Seq("0"), 1)
        def drawnOnPage(node: UI.Ast.Drawn, table: MountTable)(using Frame): MountInstance < Sync =
            for
                draw     <- MountDraw.run(node, at, DrawContext.IdScope.root, 0)
                _        <- Sync.Unsafe.defer(draw.painted(Seq.empty))
                instance <- MountInstance.start(MountedSpec(node, at.path, Nil, at), Absent, Nil, table, drawn = Present(draw))
            yield instance
        def failed(state: MountState): Maybe[String] =
            state match
                case MountState.Failed(ui) => Present(ui.toString)
                case _                     => Absent
        for
            (table, _) <- MountTable.root(Seq.empty)
            failing    <- drawnOnPage(
                UI.mounted(UI.whileMounted(Abort.fail("feed died")).andThen(span("content"))).onError(e => span(s"failed: ${e.failure}")),
                table
            )
            _          <- assertEventually(failing.cell.current.map(failed(_).nonEmpty))
            failure    <- failing.cell.current
            afterFail  <- failing.takeOnPage(failure)
            healthy    <- drawnOnPage(UI.mounted(UI.whileMounted(Kyo.unit).andThen(span("content"))), table)
            drawnState <- healthy.cell.current
            asDrawn    <- healthy.takeOnPage(drawnState)
            _          <- failing.teardown
            _          <- healthy.teardown
        yield
            assert(failed(failure).exists(_.contains("failed: feed died")), s"the failing instance holds $failure")
            assert(afterFail.isEmpty, "the subscription left the failure's paint to a page that shows the draw")
            assert(asDrawn.exists(_.isEmpty), s"the subscription painted again what the page shows: $asDrawn")
        end for
    }

    "a failure of what runs while it is shown that comes before the subscription shows the node still reaches the page" in {
        // The effect runs from the instance's launch and may fail before the subscription that shows the instance reads its state. The
        // page then shows the draw, not that state, so the failure has to be painted. A race between those two fibers, so the case runs
        // 320 times, 16 side by side: before the fix about one run in sixty never painted the error view, and the case hung.
        Async.foreach(Chunk.from(1 to 320), 16) { _ =>
            val rec = new RecordingExchange
            val ui  =
                div(UI.mounted(UI.whileMounted(Abort.fail("feed died")).andThen(span("content"))).onError(e =>
                    span(s"failed: ${e.failure}")
                ))
            painted(ui, rec) { (html, _) =>
                rec.awaitOp {
                    case RecordingExchange.Op.Paint(_, _, ui) => ui.toString.contains("failed: feed died")
                    case _                                    => false
                }.andThen(html.contains("content"))
            }
        }.map(outs => assert(outs.forall(identity), s"${outs.count(!_)} pages did not show the draw first"))
    }

    "a failure of what runs while it is shown reaches the onFailure rules around it" in {
        val rec = new RecordingExchange
        for
            heard <- Promise.init[Any, Any]
            ui = div(UI.boundary.onFailure { case failure =>
                UI.Reaction.Run(heard.complete(Result.succeed(failure)).unit)
            }(UI.mounted(UI.whileMounted(Abort.fail("feed died")).andThen(span("content")))))
            _ <- painted(ui, rec)((_, _) => heard.get.map(failure => assert(failure.equals("feed died"))))
        yield succeed
        end for
    }

    "a panic in the draw shows the node's error view in the first paint" in {
        val rec = new RecordingExchange
        val ui  = div(UI.mounted(Sync.defer[UI, Any](throw new IllegalStateException("boom"))).onError(e => span(s"failed: ${e.message}")))
        painted(ui, rec)((html, _) => assert(html.contains("failed: boom")))
    }

    "UI.id is the same in a static render and on the live page, and another per call" in {
        val rec                   = new RecordingExchange
        def tree(using Frame): UI =
            div(UI.mounted(for
                a <- UI.id
                b <- UI.id
            yield div(span("a").id(a), span("b").id(b))))
        for
            stat <- HtmlRenderer.render(tree, Seq.empty)
            live <- painted(tree, rec)((html, _) => html)
            ids = """id="([^"]+)"""".r.findAllMatchIn(stat).map(_.group(1)).toList
        yield
            assert(ids.size == 2 && ids.distinct.size == 2, s"ids: $ids")
            assert(ids.forall(live.contains))
        end for
    }

    "UI.id in a list row names that row" in {
        for
            rows <- Signal.initRef(Chunk("a", "b"))
            ui = div(rows.foreachKeyed(identity)(r => UI.mounted(UI.id.map(i => span(r).id(i)))))
            html <- HtmlRenderer.render(ui, Seq.empty)
            ids = """id="([^"]+)"""".r.findAllMatchIn(html).map(_.group(1)).toList
        yield assert(ids.size == 2 && ids.distinct.size == 2, s"ids: $ids")
        end for
    }

    // Decided 2026-11-05: an id is an address. A keyed instance's id comes from its identity (where it was made, its key, the rows around
    // it, and the instance that holds it), never from its place, so a spliced sibling before it does not change it; an unkeyed one keeps the
    // place as its base.
    /** The ids the draws of one run handed out, by what each draw names: a static render's, then a server session's over the same tree. The
      * draws say it themselves, so the session's are read from the instances it holds, not from a render of what it painted.
      */
    private def idsByDraw(tree: AtomicRef[Chunk[(String, String)]] => UI)(using
        Frame,
        kyo.test.AssertScope
    )
        : (Map[String, String], Map[String, String]) < Async =
        val rec = new RecordingExchange
        for
            seen   <- AtomicRef.init(Chunk.empty[(String, String)])
            _      <- HtmlRenderer.render(tree(seen), Seq.empty)
            onPage <- seen.getAndSet(Chunk.empty)
            live   <- Scope.run {
                for
                    root <- ReactiveUI.normalize(tree(seen), Seq.empty).map(ReactiveUI.unpainted)
                    _    <- ReactiveUI.subscribe(root, rec)
                    _    <- assertEventually(seen.get.map(_.size == onPage.size))
                    got  <- seen.get
                yield got
            }
        yield (onPage.toMap, live.toMap)
        end for
    end idsByDraw

    private def named(seen: AtomicRef[Chunk[(String, String)]], name: String)(using Frame): UI =
        UI.mounted(UI.id.map(i => seen.getAndUpdate(_.append(name -> i)).andThen(span(name).id(i)))).keyed("cell")

    // `named` keyed on one site instead of a key: every call of it is a call of that site, as calls of one component in a `map` are.
    private def siteNamed(seen: AtomicRef[Chunk[(String, String)]], name: String)(using Frame): UI =
        UI.mounted(UI.id.map(i => seen.getAndUpdate(_.append(name -> i)).andThen(span(name).id(i)))).keyedOnSite(summon[UI.MountSite])

    private val idPattern                       = """id="([^"]+)"""".r
    private def ids(html: String): List[String] = idPattern.findAllMatchIn(html).map(_.group(1)).toList

    "UI.id of a keyed instance is the same whether or not a spliced sibling stands before it" in {
        val node                                    = UI.mounted(UI.id.map(i => span("kept").id(i))).keyed("kept")
        def page(sibling: Boolean)(using Frame): UI = div(fragment(((if sibling then List[UI](span("first")) else Nil) :+ node)*))
        for
            alone  <- HtmlRenderer.render(page(false), Seq.empty)
            behind <- HtmlRenderer.render(page(true), Seq.empty)
        yield assert(ids(alone) == ids(behind) && ids(alone).size == 1, s"alone: ${ids(alone)}, behind a sibling: ${ids(behind)}")
        end for
    }

    "a server session gives a keyed instance in a list row, behind a spliced sibling, the id the page has" in {
        idsByDraw(seen =>
            div(fragment(span("first"), div(Signal.initConst(Chunk("a", "b")).foreachKeyed(identity)(r => named(seen, r)))))
        ).map { (onPage, live) =>
            assert(onPage.keySet == Set("a", "b") && onPage.values.toSet.size == 2, s"the page: $onPage")
            assert(live == onPage, s"the page: $onPage, the session: $live")
        }
    }

    "a keyed instance inside a keyed one is named by the one that holds it, the same on the page and in a server session" in {
        // The same inner identity under two holders: only the holder tells the two ids apart.
        idsByDraw(seen =>
            div(
                UI.mounted(Kyo.lift(div(named(seen, "left")))).keyed("left"),
                UI.mounted(Kyo.lift(div(named(seen, "right")))).keyed("right")
            )
        ).map { (onPage, live) =>
            assert(onPage.keySet == Set("left", "right") && onPage.values.toSet.size == 2, s"the page: $onPage")
            assert(live == onPage, s"the page: $onPage, the session: $live")
        }
    }

    // Decided 2026-11-06: one site shown twice in one range is told apart by the order of the calls, and the order goes into the id from
    // the second call on, so every id there was before stays as it was.
    "the second call of one site-keyed component in a range gets the first one's id with its order after it, and the first keeps its id" in {
        for
            seen  <- AtomicRef.init(Chunk.empty[(String, String)])
            _     <- HtmlRenderer.render(div(siteNamed(seen, "one")), Seq.empty)
            alone <- seen.getAndSet(Chunk.empty)
            _     <- HtmlRenderer.render(div(siteNamed(seen, "one"), siteNamed(seen, "two")), Seq.empty)
            both  <- seen.get.map(_.toMap)
            first = alone.toMap.getOrElse("one", "")
        yield
            assert(both.get("one").contains(first), s"alone: $alone, beside a second call: $both")
            assert(both.get("two").contains(first + "-o1"), s"the first: $first, the second: ${both.get("two")}")
        end for
    }

    "a server session gives the second call of one site-keyed component the id the page has" in {
        val rec                                                             = new RecordingExchange
        def tree(seen: AtomicRef[Chunk[(String, String)]])(using Frame): UI = div(siteNamed(seen, "one"), siteNamed(seen, "two"))
        for
            seen   <- AtomicRef.init(Chunk.empty[(String, String)])
            _      <- HtmlRenderer.render(tree(seen), Seq.empty)
            onPage <- seen.getAndSet(Chunk.empty).map(_.toMap)
            live   <- Scope.run {
                for
                    root <- ReactiveUI.normalize(tree(seen), Seq.empty).map(ReactiveUI.unpainted)
                    _    <- ReactiveUI.subscribe(root, rec)
                    // Both drew, or a place shows the report instead: either way the session has decided.
                    _ <- assertEventually(seen.get.map(_.size == 2).map(drew =>
                        drew || rec.recorded.exists(_.toString.contains("kyo-mount-error"))
                    ))
                    got <- seen.get
                yield got.toMap
            }
        yield
            assert(onPage.keySet == Set("one", "two") && onPage.values.toSet.size == 2, s"the page: $onPage")
            assert(live == onPage, s"the page: $onPage, the session: $live")
        end for
    }

    "a number key reaches the id the same way on the JVM and in JavaScript" in {
        def idFor(key: Any)(using Frame): String < Sync =
            HtmlRenderer.render(div(UI.mounted(UI.id.map(i => span("n").id(i))).keyed(key)(using CanEqual.derived)), Seq.empty)
                .map(html => ids(html).head)
        def hex(text: String): String = text.flatMap(c => Integer.toHexString(c.toInt))
        for
            whole <- idFor(1.0)
            int   <- idFor(1)
            half  <- idFor(1.5)
        yield
            // A whole number is written as one; in JavaScript a Double 1.0 is the Int 1, so the JVM writes it the same way.
            assert(whole.endsWith("-k" + hex("1")) && int.endsWith("-k" + hex("1")), s"1.0: $whole, 1: $int")
            // Any other is written by its bits, which both platforms agree on, unlike its text.
            assert(half.endsWith("-k" + hex("d" + java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(1.5)))), s"1.5: $half")
        end for
    }

    "an event that reaches a keyed node nothing has drawn yet draws it with the id the page has, in the instance the session then holds" in {
        val rec = new RecordingExchange
        for
            seen   <- AtomicRef.init(Chunk.empty[String])
            clicks <- AtomicRef.init(Chunk.empty[Int])
            tree = (_: Unit) =>
                div(
                    span("first"),
                    UI.mounted(Kyo.lift(div(UI.mounted(for
                        i   <- UI.id
                        _   <- seen.getAndUpdate(_.append(i))
                        own <- Signal.initRef(0)
                    yield button("in").id(i).onClick(own.updateAndGet(_ + 1).map(v => clicks.getAndUpdate(_.append(v))).unit))
                        .keyed("inner")))).keyed("outer")
                )
            page   <- HtmlRenderer.render(tree(()), Seq.empty)
            onPage <- seen.getAndSet(Chunk.empty)
            pagePath = """<button[^>]*data-kyo-path="([^"]+)"""".r.findFirstMatchIn(page).map(_.group(1).split('.').toSeq).get
            live <- Scope.run {
                for
                    // No paint: the session's own tree, which the click reaches before the subscription has drawn anything.
                    root  <- ReactiveUI.normalize(tree(()), Seq.empty).map(ReactiveUI.unpainted)
                    _     <- Abort.run[Any](root.handle(pagePath, click(pagePath*)))
                    sub   <- ReactiveUI.subscribe(root, rec)
                    _     <- assertEventually(rec.paints.map(_.nonEmpty))
                    _     <- sub.handle(pagePath, click(pagePath*))
                    drawn <- seen.get
                    heard <- clicks.get
                yield (drawn, heard)
            }
        yield
            val (drawn, heard) = live
            assert(onPage.size == 1, s"the page drew: $onPage")
            // One draw in the session, the click's, with the page's id, and the instance the session holds is that draw's: its state goes on.
            assert(drawn == onPage, s"the page: $onPage, the session: $drawn")
            assert(heard == Chunk(1, 2), s"clicks heard: $heard")
        end for
    }

    "an event that comes while the subscription takes a keyed node's draw lands in the state the instance then shows" in {
        // The subscription takes the draw the first click made and only then shows its instance there; a click in between found the
        // place empty and drew the node again. That window is a race between the click and the subscription's fiber, so the case runs
        // 320 times, 16 side by side: before the fix about one run in ten drew twice.
        Async.foreach(Chunk.from(1 to 320), 16) { _ =>
            val rec = new RecordingExchange
            for
                seen   <- AtomicRef.init(Chunk.empty[String])
                clicks <- AtomicRef.init(Chunk.empty[Int])
                tree = (_: Unit) =>
                    div(
                        span("first"),
                        UI.mounted(Kyo.lift(div(UI.mounted(for
                            i   <- UI.id
                            _   <- seen.getAndUpdate(_.append(i))
                            own <- Signal.initRef(0)
                        yield button("in").id(i).onClick(own.updateAndGet(_ + 1).map(v => clicks.getAndUpdate(_.append(v))).unit))
                            .keyed("inner")))).keyed("outer")
                    )
                page   <- HtmlRenderer.render(tree(()), Seq.empty)
                onPage <- seen.getAndSet(Chunk.empty)
                pagePath = """<button[^>]*data-kyo-path="([^"]+)"""".r.findFirstMatchIn(page).map(_.group(1).split('.').toSeq).get
                live <- Scope.run {
                    for
                        root  <- ReactiveUI.normalize(tree(()), Seq.empty).map(ReactiveUI.unpainted)
                        _     <- Abort.run[Any](root.handle(pagePath, click(pagePath*)))
                        sub   <- ReactiveUI.subscribe(root, rec)
                        _     <- assertEventually(rec.paints.map(_.nonEmpty))
                        _     <- sub.handle(pagePath, click(pagePath*))
                        drawn <- seen.get
                        heard <- clicks.get
                    yield (drawn == onPage, heard)
                }
            yield live
            end for
        }.map { outcomes =>
            assert(
                outcomes.forall(_ == (true, Chunk(1, 2))),
                s"what the runs saw: ${outcomes.groupBy(identity).view.mapValues(_.size).toMap}"
            )
        }
    }

    "an event that first draws a keyed node in a list row gives it the id the page has, with the row's key in it" in {
        val rec = new RecordingExchange
        for
            seen   <- AtomicRef.init(Chunk.empty[(String, String)])
            clicks <- AtomicRef.init(Chunk.empty[Int])
            tree = (_: Unit) =>
                div(
                    span("first"),
                    div(Signal.initConst(Chunk("a", "b")).foreachKeyed(identity)(r =>
                        UI.mounted(for
                            i   <- UI.id
                            _   <- seen.getAndUpdate(_.append(r -> i))
                            own <- Signal.initRef(0)
                        yield button(r).id(i).onClick(own.updateAndGet(_ + 1).map(v => clicks.getAndUpdate(_.append(v))).unit))
                            .keyed("cell")
                    ))
                )
            page   <- HtmlRenderer.render(tree(()), Seq.empty)
            onPage <- seen.getAndSet(Chunk.empty)
            pathOfB = """<button[^>]*data-kyo-path="([^"]+)"[^>]*>b<""".r.findFirstMatchIn(page).map(_.group(1).split('.').toSeq).get
            live <- Scope.run {
                for
                    // No paint: the click reaches row b's cell before the subscription has drawn anything.
                    root  <- ReactiveUI.normalize(tree(()), Seq.empty).map(ReactiveUI.unpainted)
                    _     <- Abort.run[Any](root.handle(pathOfB, click(pathOfB*)))
                    first <- seen.get
                    sub   <- ReactiveUI.subscribe(root, rec)
                    _     <- assertEventually(seen.get.map(_.size == 2))
                    _     <- sub.handle(pathOfB, click(pathOfB*))
                    drawn <- seen.get
                    heard <- clicks.get
                yield (first, drawn, heard)
            }
        yield
            val (first, drawn, heard) = live
            assert(first.map(_._1) == Chunk("b"), s"the click drew: $first")
            assert(drawn.toMap == onPage.toMap, s"the page: $onPage, the session: $drawn")
            assert(heard == Chunk(1, 2), s"clicks heard: $heard")
        end for
    }

    "UI.id outside a mount panics" in {
        Abort.run[Any](UI.id).map(r => assert(r.isPanic))
    }

    "UI.whileMounted outside a mount panics" in {
        Abort.run[Any](UI.whileMounted(Kyo.unit)).map(r => assert(r.isPanic))
    }

    "a drawn mount has no placeholder" in {
        assert(typeChecks("""{ given kyo.Frame = kyo.Frame.internal; kyo.UI.mounted(kyo.UI.span("x")).onError(_ => kyo.UI.span("y")) }"""))
        assert(!typeChecks("""{ given kyo.Frame = kyo.Frame.internal; kyo.UI.mounted(kyo.UI.span("x")).placeholder(kyo.UI.span("y")) }"""))
    }

    "starts at its initial value, and a write repaints" in {
        val instances = new Instances
        val rec       = new RecordingExchange
        ReactiveUI.subscribed(div(UI.mounted(Signal.initRef(7).map(instances.draw))), rec) { _ =>
            for
                ref    <- drawn(instances)
                start  <- ref.get
                _      <- parked(ref)
                before <- rec.paints
                _      <- ref.set(8)
                _      <- rec.awaitOp(_ => true).andThen(parked(ref))
                after  <- rec.paints
            yield
                assert(start == 7)
                assert(after.size == before.size + 1)
            end for
        }
    }

    "closing the scope releases it" in {
        val instances = new Instances
        val rec       = new RecordingExchange
        for
            ref <- ReactiveUI.subscribed(div(UI.mounted(Signal.initRef(1).map(instances.draw))), rec) { _ =>
                drawn(instances).map(ref => parked(ref).andThen(ref))
            }
            parks <- ref.waiters
        yield assert(parks == 0)
        end for
    }

    "a re-render of the region around it gives a fresh instance at the initial value" in {
        val instances = new Instances
        val rec       = new RecordingExchange
        for
            parent <- Signal.initRef(0)
            ui = div(parent.render(_ => UI.mounted(Signal.initRef(0).map(instances.draw))))
            held <- ReactiveUI.subscribed(ui, rec) { _ =>
                for
                    first <- drawn(instances)
                    _     <- parked(first)
                    _     <- first.set(5)
                    _     <- parked(first)
                    _     <- parent.set(1)
                    _     <- assertEventually(Sync.defer(instances.seen.size == 2))
                    value <- instances.seen.last.get
                yield value
            }
        yield assert(held == 0)
        end for
    }

    "a keyed instance keeps its value across that re-render, and draws once" in {
        val instances = new Instances
        val rec       = new RecordingExchange
        for
            parent <- Signal.initRef(0)
            ui = div(parent.render(_ => UI.mounted(Signal.initRef(0).map(instances.draw)).keyed("k")))
            held <- ReactiveUI.subscribed(ui, rec) { _ =>
                for
                    first <- drawn(instances)
                    _     <- parked(first)
                    _     <- first.set(5)
                    _     <- parked(first)
                    _     <- parent.set(1)
                    _     <- parked(parent)
                    value <- first.get
                yield (instances.seen.size, value)
            }
        yield assert(held == (1, 5))
        end for
    }

    "a repaint around a keyed instance does not draw it again when a paint renders the tree" in {
        for
            draws  <- AtomicInt.init(0)
            parent <- Signal.initRef(0)
            ui        = div(parent.render(p => div(span(p.toString), UI.mounted(draws.incrementAndGet.andThen(span("k"))).keyed("k"))))
            rendering = new RecordingExchange:
                override def onChange(
                    region: kyo.internal.ReactiveRegion,
                    path: Seq[String],
                    context: kyo.internal.ReactiveRegion.RegionIdentity,
                    parentContext: kyo.internal.ReactiveRegion.ParentContext,
                    previous: Maybe[UI],
                    ui: UI
                )(using Frame): Unit < Async =
                    HtmlRenderer.renderRegion(ui, path, context).andThen(super.onChange(region, path, context, parentContext, previous, ui))
            out <- painted(ui, rendering) { (_, sub) =>
                for
                    _ <- assertEventually(parent.waiters.map(_ == 1))
                    _ <- parent.set(1)
                    _ <- rendering.awaitOp(_ => true)
                    _ <- assertEventually(parent.waiters.map(_ == 1))
                    n <- draws.get
                yield n
            }
        yield assert(out == 1)
        end for
    }

    "a keyed instance that moves from one region to another in one pass keeps its state" in {
        val rec = new RecordingExchange
        for
            side <- Signal.initRef(true)
            own  <- AtomicRef.init(Chunk.empty[SignalRef[Int]])
            node = (_: Int) =>
                UI.mounted(Signal.initRef(0).map(r => own.getAndUpdate(_.append(r)).andThen(r.render(v => span(v.toString))))).keyed("k")
            ui = div(side.render(s => if s then node(0): UI else span("none")), side.render(s => if s then span("none"): UI else node(1)))
            held <- painted(ui, rec) { (_, _) =>
                for
                    _     <- assertEventually(side.waiters.map(_ == 2))
                    refs  <- own.get
                    _     <- refs.head.set(5)
                    _     <- side.set(false)
                    _     <- assertEventually(side.waiters.map(_ == 2))
                    after <- own.get
                    value <- after.last.get
                yield (after.size, value)
            }
        yield assert(held == (1, 5))
        end for
    }

    "one identity in two regions at once shows the report in both, and each keeps its error view" in {
        // Decided 2026-11-06: an identity shown at two places at once is an error, in two regions as in one, when the caller chose its key.
        val rec                                  = new RecordingExchange
        def twice(using Frame, UI.MountSite): UI =
            def node: UI = UI.mounted(Kyo.lift(span("x"))).keyed("same")(using summon, summon[UI.MountSite])
            div(Signal.initConst(1).render(_ => div(node)), Signal.initConst(2).render(_ => div(node)))
        painted(twice, rec) { (_, _) =>
            assertEventually(rec.paints.map(_.count(_.ui.toString.contains("shown in 2 places")) == 2)).andThen(succeed)
        }
    }

    "one caller key twice in one region shows the report at both places in a session" in {
        val rec  = new RecordingExchange
        val node = UI.mounted(Kyo.lift(span("drawn"))).keyed("same")
        painted(div(Signal.initConst(1).render(_ => div(node, node))), rec) { (_, _) =>
            // The message at each place names both: 0.0 and 0.1.
            assertEventually(rec.paints.map(_.exists(p =>
                p.ui.toString.contains("is shown in 2 places at once: 0.0, 0.1.")
            ))).andThen(succeed)
        }
    }

    "rows of a keyed list hold separate instances of one keyed node" in {
        val rec = new RecordingExchange
        for
            rows <- Signal.initRef(Chunk("a", "b"))
            seen <- AtomicRef.init(Map.empty[String, SignalRef[Int]])
            ui = div(rows.foreachKeyed(identity)(r =>
                UI.mounted(Signal.initRef(0).map(own =>
                    seen.getAndUpdate(_.updated(r, own)).andThen(own.render(v => span(s"$r:$v")))
                )).keyed("k")
            ))
            held <- painted(ui, rec) { (html, _) =>
                for
                    _      <- assertEventually(rows.waiters.map(_ == 1))
                    first  <- seen.get
                    _      <- first("a").set(7)
                    _      <- rows.set(Chunk("b", "a", "c"))
                    _      <- assertEventually(seen.get.map(_.contains("c")))
                    _      <- assertEventually(rows.waiters.map(_ == 1))
                    second <- seen.get
                    a      <- second("a").get
                yield (html.contains("a:0") && html.contains("b:0"), second.size, first("a") eq second("a"), a)
            }
        yield assert(held == (true, 3, true, 7))
        end for
    }

    "a keyed mount in a keyed one is kept with it, in a keyless one it restarts with it" in {
        val rec = new RecordingExchange
        for
            tick <- Signal.initRef(0)
            runs <- AtomicRef.init(Map.empty[String, Int])
            count = (name: String) => runs.getAndUpdate(m => m.updated(name, m.getOrElse(name, 0) + 1)).andThen(span(name))
            inner = (name: String) => UI.mounted(count(name)).keyed("inner")
            ui    = div(tick.render(n =>
                div(
                    span(s"tick:$n"),
                    UI.mounted(Kyo.lift(div(inner("in-keyless")))),
                    UI.mounted(Kyo.lift(div(inner("in-keyed")))).keyed("outer")
                )
            ))
            counts <- painted(ui, rec) { (_, _) =>
                for
                    _ <- assertEventually(tick.waiters.map(_ == 1))
                    _ <- assertEventually(runs.get.map(_.size == 2))
                    _ <- tick.set(1)
                    _ <- assertEventually(runs.get.map(_.get("in-keyless").contains(2)))
                    _ <- assertEventually(tick.waiters.map(_ == 1))
                    c <- runs.get
                yield c
            }
        yield assert(counts == Map("in-keyless" -> 2, "in-keyed" -> 1), s"runs: $counts")
        end for
    }

    // ---- mountedWith: props read through Live ----

    private case class Labeled(label: Prop[String]) derives CanEqual

    /** A node that reads its label through `Live` and keeps what it read in `read`, drawn once per instance. */
    private def labeled(props: Labeled, read: AtomicReference[Chunk[Prop[String]]])(using Frame): UI.Ast.Drawn =
        UI.mountedWith(props) { live =>
            val label = live(_.label)
            discard(read.updateAndGet(_.append(label)))
            span(label)
        }

    private case class Optional(label: Maybe[Prop[String]], size: Prop[Int]) derives CanEqual

    "an optional field reads as one prop of an option, a plain field as its prop" in {
        val set                        = new UI.Live(Optional(Present("a"), 1), Absent)
        val unset                      = new UI.Live(Optional(Absent, 1), Absent)
        val label: Prop[Maybe[String]] = set(_.label)
        val size: Prop[Int]            = set(_.size)
        for
            l <- Prop.signal(label).current
            n <- Prop.signal(unset(_.label)).current
            z <- Prop.signal(size).current
        yield
            assert(!label.isInstanceOf[Signal[?]] && !size.isInstanceOf[Signal[?]], s"$label $size")
            assert(l == Present("a") && n == Absent && z == 1, s"$l $n $z")
        end for
    }

    "an optional field follows the latest run and a signal inside it" in {
        for
            inner <- Signal.initRef("x")
            runs  <- Signal.initRef(Optional(Absent, 1))
            live = new UI.Live(Optional(Absent, 1), Present(runs))
            read = Prop.signal(live(_.label))
            first <- read.current
            _     <- runs.set(Optional(Present(inner), 1))
            _     <- assertEventually(read.current.map(_ == Present("x")))
            _     <- inner.set("y")
            _     <- assertEventually(read.current.map(_ == Present("y")))
        yield assert(first == Absent, s"$first")
    }

    "an instance no run can reach again reads a constant prop as it is, with no signal" in {
        val read = new AtomicReference(Chunk.empty[Prop[String]])
        val rec  = new RecordingExchange
        val ui   = div(labeled(Labeled("plain"), read), labeled(Labeled("kept"), read).keyed("k"))
        painted(ui, rec) { (html, _) =>
            assert(html.contains("plain") && html.contains("kept"), html)
            assert(read.get.nonEmpty && read.get.forall(!_.isInstanceOf[Signal[?]]), s"read: ${read.get}")
        }
    }

    "a keyed instance in a region that runs again reads its props as a signal that follows the latest run" in {
        val read = new AtomicReference(Chunk.empty[Prop[String]])
        val rec  = new RecordingExchange
        for
            tick <- Signal.initRef(0)
            ui = div(tick.render(n => labeled(Labeled(s"run $n"), read).keyed("k")))
            seen <- painted(ui, rec) { (_, _) =>
                for
                    _ <- assertEventually(tick.waiters.map(_ == 1))
                    _ <- tick.set(1)
                    _ <- assertEventually(tick.waiters.map(_ == 1))
                    label = read.get.head.asInstanceOf[Signal[String]]
                    _   <- assertEventually(label.current.map(_ == "run 1"))
                    now <- label.current
                yield (read.get.size, read.get.head.isInstanceOf[Signal[?]], now)
            }
        yield assert(seen == (1, true, "run 1"))
        end for
    }

    "a mount inside a kept instance drawn in the Solid shape keeps its instance and state while the host runs again with new props" in {
        val instances                     = new Instances
        val read                          = new AtomicReference(Chunk.empty[Prop[String]])
        val rec                           = new RecordingExchange
        def host(n: Int)(using Frame): UI =
            UI.mountedWith(Labeled(s"run $n")) { live =>
                val label = live(_.label)
                discard(read.updateAndGet(_.append(label)))
                div(span(label), UI.mounted(Signal.initRef(0).map(instances.draw)))
            }.keyed("host")
        for
            tick <- Signal.initRef(0)
            ui = div(tick.render(host))
            held <- painted(ui, rec) { (_, _) =>
                for
                    first <- drawn(instances)
                    _     <- parked(first)
                    _     <- first.set(5)
                    _     <- parked(first)
                    _     <- tick.set(1)
                    _     <- assertEventually(tick.waiters.map(_ == 1))
                    label = read.get.head.asInstanceOf[Signal[String]]
                    _     <- assertEventually(label.current.map(_ == "run 1"))
                    value <- first.get
                yield (instances.seen.size, read.get.size, value)
            }
        yield assert(held == (1, 1, 5), s"(inner instances, host draws, inner value): $held")
        end for
    }

    "in a kept instance a mount without a key is held by its place, and one in a region inside it is new with each run of that region" in {
        val rec = new RecordingExchange
        for
            tick  <- Signal.initRef(0)
            local <- Signal.initRef(0)
            runs  <- AtomicRef.init(Map.empty[String, Int])
            count = (name: String) => runs.getAndUpdate(m => m.updated(name, m.getOrElse(name, 0) + 1)).andThen(span(name))
            host  = (n: Int) =>
                UI.mountedWith(Labeled(s"run $n")) { live =>
                    div(span(live(_.label)), UI.mounted(count("placed")), local.render(_ => UI.mounted(count("in-region"))))
                }.keyed("host")
            ui = div(tick.render(host))
            counts <- painted(ui, rec) { (_, _) =>
                for
                    _      <- assertEventually(runs.get.map(_.size == 2))
                    _      <- tick.set(1)
                    _      <- assertEventually(tick.waiters.map(_ == 1))
                    before <- runs.get.map(_.getOrElse("in-region", 0))
                    _      <- local.set(1)
                    _      <- assertEventually(runs.get.map(_.getOrElse("in-region", 0) > before))
                    _      <- assertEventually(local.waiters.map(_ == 1))
                    c      <- runs.get
                yield (c.getOrElse("placed", 0), c.getOrElse("in-region", 0) > before)
            }
        yield assert(counts == (1, true), s"(placed runs, in-region ran again): $counts")
        end for
    }

    // ---- whileMounted on an element ----

    "an element's effect does not run in a static render" in {
        for
            runs <- AtomicInt.init(0)
            html <- HtmlRenderer.render(div(span("x").whileMounted(runs.incrementAndGet)), Seq.empty)
            n    <- runs.get
        yield
            assert(html.contains("x"))
            assert(n == 0)
        end for
    }

    /** The element's effect under a region, in the first paint's tree and in a session, which has no paint of its own to rely on. */
    private def runsOnce(tree: (AtomicInt, Int) => UI, sessionOnly: Boolean)(using Frame, kyo.test.AssertScope): Int < Async =
        val rec = new RecordingExchange
        for
            runs <- AtomicInt.init(0)
            tick <- Signal.initRef(0)
            ui = div(tick.render(n => tree(runs, n)))
            out <- Scope.run {
                for
                    cmds <- UI.Commands.init(_ => Kyo.unit)
                    r    <- UICommands.commands.let(Present(cmds)) {
                        for
                            root0 <- ReactiveUI.normalize(ui, Seq.empty)
                            root  <-
                                if sessionOnly then Kyo.lift(ReactiveUI.unpainted(root0))
                                else MountDispatch.live.let(root0.mountDispatch)(HtmlRenderer.render(ui, Seq.empty)).andThen(root0)
                            _ <- ReactiveUI.subscribe(root, rec)
                            _ <- assertEventually(runs.get.map(_ == 1))
                            _ <- assertEventually(tick.waiters.map(_ == 1))
                            _ <- tick.set(1)
                            _ <- rec.awaitOp(_ => true)
                            _ <- assertEventually(tick.waiters.map(_ == 1))
                            n <- runs.get
                        yield n
                    }
                yield r
            }
        yield out
        end for
    end runsOnce

    // The rule decided on 2026-11-05: a region that runs again has new content, and what has to survive says so with a key. These cases
    // said before that an unkeyed element effect survived its region running again, by its place.
    "an unkeyed element effect starts again with each run of its region, as an unkeyed mount does" - {
        def restarts(tree: (AtomicInt, Int) => UI, sessionOnly: Boolean)(using Frame, kyo.test.AssertScope): Int < Async =
            val rec = new RecordingExchange
            for
                runs <- AtomicInt.init(0)
                tick <- Signal.initRef(0)
                ui = div(tick.render(n => tree(runs, n)))
                out <- Scope.run {
                    for
                        cmds <- UI.Commands.init(_ => Kyo.unit)
                        r    <- UICommands.commands.let(Present(cmds)) {
                            for
                                root0 <- ReactiveUI.normalize(ui, Seq.empty)
                                root  <-
                                    if sessionOnly then Kyo.lift(ReactiveUI.unpainted(root0))
                                    else MountDispatch.live.let(root0.mountDispatch)(HtmlRenderer.render(ui, Seq.empty)).andThen(root0)
                                _ <- ReactiveUI.subscribe(root, rec)
                                _ <- assertEventually(runs.get.map(_ == 1))
                                _ <- assertEventually(tick.waiters.map(_ == 1))
                                _ <- tick.set(1)
                                _ <- assertEventually(runs.get.map(_ == 2))
                                n <- runs.get
                            yield n
                        }
                    yield r
                }
            yield out
            end for
        end restarts
        "the element below the region's root" in {
            restarts((runs, n) => div(span(s"t$n"), span("x").whileMounted(runs.incrementAndGet)), sessionOnly = false).map(n =>
                assert(n == 2)
            )
        }
        "the element at the region's root" in {
            restarts((runs, n) => div(s"t$n").whileMounted(runs.incrementAndGet), sessionOnly = false).map(n => assert(n == 2))
        }
        "in a session over a page an earlier render drew" in {
            restarts((runs, n) => div(span(s"t$n"), span("x").whileMounted(runs.incrementAndGet)), sessionOnly = true).map(n =>
                assert(n == 2)
            )
        }
        "whatever id the element carries: an id is no key" in {
            restarts((runs, n) => div(span(s"t$n"), span("x").id("carried").whileMounted(runs.incrementAndGet)), sessionOnly = false)
                .map(n => assert(n == 2))
        }
    }

    "an element's effect ends with the element, and its Scope with it" in {
        val rec = new RecordingExchange
        for
            started <- Promise.init[Unit, Any]
            closed  <- Promise.init[Unit, Any]
            show    <- Signal.initRef(true)
            ui = div(show.render(on =>
                if on then span("x").whileMounted(Scope.ensure(closed.completeUnitDiscard).andThen(started.completeUnitDiscard)): UI
                else span("none")
            ))
            _ <- painted(ui, rec) { (_, _) =>
                for
                    _ <- started.get
                    d <- closed.done
                    _ <- assertEventually(show.waiters.map(_ == 1))
                    _ <- show.set(false)
                    _ <- closed.get
                yield assert(!d)
            }
        yield succeed
        end for
    }

    // A client that patches a keyed list in place keeps the rows that stay, as both live clients do (`onListPatch`); one that repaints the
    // whole list replaced what those rows' instances had painted and subscribes them again. This case asked the second kind for the
    // first's answer and passed on the JVM only when it looked before the restart of the kept row came through.
    "each row of a keyed list keeps its unkeyed instance while another row goes, and the row that goes ends its own" - {
        def keeps(kind: String, keepsRows: Boolean = true)(using Frame, kyo.test.AssertScope): Unit < Async =
            val rec = new RecordingExchange(keepsRows = keepsRows)
            for
                started <- AtomicRef.init(Chunk.empty[String])
                ended   <- AtomicRef.init(Chunk.empty[String])
                aEnded  <- Promise.init[Unit, Any]
                rows    <- Signal.initRef(Chunk("a", "b"))
                run = (r: String) =>
                    Scope.ensure(ended.getAndUpdate(_.append(r)).andThen(if r == "a" then aEnded.completeUnitDiscard else Kyo.unit))
                        .andThen(started.getAndUpdate(_.append(r)).unit)
                ui = div(rows.foreachKeyed(identity)(r =>
                    div(
                        span(r),
                        kind match
                            case "element" => span("x").whileMounted(run(r).andThen(Async.never))
                            case "drawn"   => UI.mounted(UI.whileMounted(run(r).andThen(Async.never)).andThen(span("m")))
                            case _         => waitingMount(run(r).andThen(span("m")))
                    )
                ))
                out <- painted(ui, rec) { (_, _) =>
                    for
                        _ <- assertEventually(started.get.map(_.size == 2))
                        _ <- parked(rows)
                        _ <- rows.set(Chunk("b"))
                        // The emission has begun once a has ended; once its region waits again it is over, and the end of a restarted b,
                        // which comes before the restart, would have come with it.
                        _ <- aEnded.get
                        _ <- parked(rows)
                        e <- ended.get
                        s <- started.get
                    yield (s.sorted, e)
                }
            yield assert(out == (Chunk("a", "b"), Chunk("a")), s"started, then ended: $out")
            end for
        end keeps
        "an element's effect" in keeps("element")
        "a mount that draws" in keeps("drawn")
        "a mount that waits" in keeps("waiting")
    }

    "a key starts an element's effect again when it changes, and keeps it when it does not" in {
        val rec = new RecordingExchange
        for
            runs <- AtomicRef.init(Chunk.empty[Int])
            tick <- Signal.initRef(0)
            ui = div(tick.render(n => span(s"t$n").whileMountedKeyed(n / 2)(runs.getAndUpdate(_.append(n / 2)).unit)))
            out <- painted(ui, rec) { (_, _) =>
                for
                    _ <- assertEventually(runs.get.map(_.size == 1))
                    _ <- assertEventually(tick.waiters.map(_ == 1))
                    _ <- tick.set(1)
                    _ <- assertEventually(tick.waiters.map(_ == 1))
                    a <- runs.get
                    _ <- tick.set(2)
                    _ <- assertEventually(runs.get.map(_.size == 2))
                    b <- runs.get
                yield (a, b)
            }
        yield assert(out == (Chunk(0), Chunk(0, 1)))
        end for
    }

    /** An element with its effect, with a spliced sibling that appears before it and goes again: how often the effect started, and whether a
      * run of it ended.
      */
    private def splicedBefore(watched: (AtomicInt, Promise[Unit, Any]) => UI)(using Frame, kyo.test.AssertScope): (Int, Boolean) < Async =
        val rec = new RecordingExchange
        for
            runs   <- AtomicInt.init(0)
            closed <- Promise.init[Unit, Any]
            where  <- Signal.initRef(0)
            el = watched(runs, closed)
            ui = div(where.render(n => fragment(((if n == 1 then List[UI](span("first")) else Nil) :+ el)*)))
            out <- painted(ui, rec) { (_, _) =>
                for
                    _ <- assertEventually(runs.get.map(_ == 1))
                    _ <- assertEventually(where.waiters.map(_ == 1))
                    _ <- where.set(1)
                    _ <- rec.awaitOp(_ => true)
                    _ <- assertEventually(where.waiters.map(_ == 1))
                    _ <- where.set(0)
                    _ <- rec.awaitOp(_ => true)
                    _ <- assertEventually(where.waiters.map(_ == 1))
                    n <- runs.get
                    d <- closed.done
                yield (n, d)
            }
        yield out
        end for
    end splicedBefore

    "a spliced sibling that appears before a keyed node and goes again changes nothing" - {
        "a keyed element effect" in {
            splicedBefore((runs, closed) =>
                span(
                    "x"
                ).whileMountedKeyed("watched")(runs.incrementAndGet.andThen(Scope.ensure(closed.completeUnitDiscard)).andThen(Async.never))
            ).map(out => assert(out == (1, false), s"started, and a run ended: $out"))
        }
        "a keyed mount keeps its instance and its state" in {
            val rec       = new RecordingExchange
            val instances = new Instances
            for
                before <- Signal.initRef(false)
                node = UI.mounted(Signal.initRef(0).map(instances.draw)).keyed("kept")
                ui   = div(before.render(b => fragment(((if b then List[UI](span("first")) else Nil) :+ node)*)))
                out <- painted(ui, rec) { (_, _) =>
                    for
                        first <- drawn(instances)
                        _     <- first.set(5)
                        _     <- parked(before)
                        _     <- before.set(true)
                        _     <- rec.awaitOp(_ => true)
                        _     <- parked(before)
                        _     <- before.set(false)
                        _     <- rec.awaitOp(_ => true)
                        _     <- parked(before)
                        last  <- instances.seen.last.get
                    yield (instances.seen.size, last)
                }
            yield assert(out == (1, 5), s"instances and the state the last one holds: $out")
            end for
        }
    }

    "an element's failure meets the rules around it" in {
        val rec = new RecordingExchange
        for
            heard <- Promise.init[Any, Any]
            ui = div(UI.boundary.onFailure { case failure =>
                UI.Reaction.Run(heard.complete(Result.succeed(failure)).unit)
            }(span("x").whileMounted(Abort.fail("feed died"))))
            _ <- painted(ui, rec)((_, _) => heard.get.map(failure => assert(failure.equals("feed died"))))
        yield succeed
        end for
    }

    "an element's failure no rule takes fails the mount around it" in {
        val rec = new RecordingExchange
        val ui  =
            div(UI.mounted(Kyo.lift(div(span("content").whileMounted(Abort.fail("feed died"))))).onError(e =>
                span(s"failed: ${e.failure}")
            ))
        painted(ui, rec) { (html, _) =>
            assert(html.contains("content"))
            rec.awaitOp {
                case RecordingExchange.Op.Paint(_, _, ui) => ui.toString.contains("failed: feed died")
                case _                                    => false
            }.andThen(succeed)
        }
    }

    "the region's first pass does not repaint a value the enclosing paint already showed" in {
        val rec = new RecordingExchange
        for
            renders <- AtomicInt.init(0)
            value   <- Signal.initRef(1)
            ui = div(value.render(v => span(s"v$v").onClick(renders.incrementAndGet.unit)))
            paints <- painted(ui, rec) { (_, _) =>
                assertEventually(value.waiters.map(_ == 1)).andThen(rec.paints)
            }
        yield assert(paints.isEmpty, s"painted again: ${paints.map(_.path)}")
        end for
    }

    "a value that changed between the paint and the first pass is painted, and draws anew" in {
        val rec = new RecordingExchange
        for
            draws <- AtomicInt.init(0)
            value <- Signal.initRef(1)
            ui = div(value.render(v => UI.mounted(draws.incrementAndGet.andThen(span(s"v$v")))))
            n <- Scope.run {
                for
                    root <- ReactiveUI.normalize(ui, Seq.empty)
                    _    <- MountDispatch.live.let(root.mountDispatch)(HtmlRenderer.render(ui, Seq.empty))
                    _    <- value.set(2)
                    _    <- ReactiveUI.subscribe(root, rec)
                    _    <- assertEventually(rec.paints.map(_.exists(_.ui.toString.contains("v2"))))
                    n    <- draws.get
                yield n
            }
        yield assert(n >= 2)
        end for
    }

end UIDrawnTest
