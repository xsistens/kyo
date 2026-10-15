package kyo

import kyo.internal.MouseEventData
import kyo.internal.UIEvent

/** What happens to a failure nothing handles: an event handler's, a [[kyo.UI.fork]] fiber's, a [[kyo.UI.report]]. The `onFailure`
  * rules on the elements and boundaries around it pick a [[kyo.UI.Reaction]], innermost first; `Notify` goes to the nearest
  * boundary's `onNotice` sink, `Escalate` fails the enclosing mount, and `Run` runs an action.
  */
class HandlerFailureTest extends UIScreen:

    import UI.*

    /** A typed failure that is not a `Throwable`, as a service layer would declare it. */
    private case class ApiError(code: Int) derives CanEqual

    // A failure arrives as raised, typed `Any`.
    private given CanEqual[Any, Any] = CanEqual.derived

    private def click(handle: (Seq[String], UIEvent) => Boolean < Async, path: Seq[String])(using Frame): Boolean < Async =
        handle(path, UIEvent.Click(path, MouseEventData(UI.Modifiers.none, Absent)))

    /** A sink that records the failures it receives, as raised, and completes `first` with the first one. */
    private class Notices(seen: AtomicRef[Chunk[Any]], first: Promise[Any, Any]):
        def sink(using Frame): Throwable => Unit < Async =
            case UI.Failed(failure) => seen.getAndUpdate(_.append(failure)).andThen(first.completeDiscard(Result.succeed(failure)))
        def all(using Frame): Chunk[Any] < Sync = seen.get
        def next(using Frame, kyo.test.AssertScope): Any < Async = within("a notice")(first.get)
    end Notices

    private def notices(using Frame): Notices < Sync =
        for
            seen  <- AtomicRef.init(Chunk.empty[Any])
            first <- Promise.init[Any, Any]
        yield Notices(seen, first)

    private val errorView: PartialFunction[MountError, UI] = {
        case e @ MountError.Failed(ApiError(code)) => span(s"error:${e.source}:$code")
    }

    "a handler whose effect can fail with a typed error that is not a Throwable is accepted" in {
        val save: Unit < (Async & Abort[ApiError]) = Abort.fail(ApiError(409))
        discard(button("save").onClick(save))
        discard(input.onInput(_ => save))
        succeed
    }

    "with no rule, a handler's failure is notified to the boundary's sink and the content stays" in {
        for
            n <- notices
            ui = div(UI.boundary.onNotice(n.sink)(div(button("save").onClick(Abort.fail(ApiError(409))))))
            got <- mountWith(ui) { (screen, handle) =>
                for
                    _     <- screen.await("save")(_.contains("save"))
                    _     <- click(handle, Seq("0", "0"))
                    got   <- n.next
                    shown <- screen.now
                yield (got, shown)
            }
        yield
            assert(got._1 == ApiError(409), s"the sink received: ${got._1}")
            assert(got._2.contains("save"), s"the screen after the failure: ${got._2}")
    }

    "a notice with no sink anywhere is logged" in {
        val logged = new UIScreen.Warnings
        for
            ran <- Promise.init[Unit, Any]
            ui = div(button("save").onClick(ran.completeUnitDiscard.andThen(Abort.fail(ApiError(409)))))
            _ <- Log.let(logged.safe) {
                mountWith(ui)((_, handle) => click(handle, Seq("0")).andThen(within("the handler")(ran.get)))
            }.andThen(Log.flush)
        yield assert(logged.lines.exists(_.contains("no onNotice sink")), s"logged: ${logged.lines}")
        end for
    }

    "Escalate fails the enclosing mount, which shows its error view with the failure as raised" in {
        val ui = div(UI.boundary.onFailure { case ApiError(409) => Reaction.Escalate }.onError(errorView)(div(
            UI.mounted(Kyo.lift(div(button("save").onClick(Abort.fail(ApiError(409))))))
        )))
        mountWith(ui) { (screen, handle) =>
            for
                _ <- screen.await("save")(_.contains("save"))
                _ <- click(handle, Seq("0", "0", "0"))
                _ <- screen.await("the mount's error view")(_.contains("error:Handler:409"))
            yield succeed
        }
    }

    "Run runs its action" in {
        for
            route <- Signal.initRef("issues")
            ui = div(UI.boundary.onFailure { case ApiError(401) => Reaction.Run(route.set("login")) }(div(
                button("save").onClick(Abort.fail(ApiError(401))),
                route.render(r => span(s"route:$r"))
            )))
            _ <- mountWith(ui) { (screen, handle) =>
                for
                    _ <- screen.await("the issues route")(_.contains("route:issues"))
                    _ <- click(handle, Seq("0", "0"))
                    _ <- screen.await("the login route")(_.contains("route:login"))
                yield ()
            }
        yield succeed
    }

    "an element's rules come before the boundary's, and a failure they do not match goes on outward" in {
        for
            n <- notices
            ui = div(UI.boundary.onFailure { case _: ApiError => Reaction.Escalate }.onError(errorView).onNotice(n.sink)(div(
                UI.mounted(Kyo.lift(form(
                    button("conflict").onClick(Abort.fail(ApiError(409))),
                    button("broken").onClick(Abort.fail(ApiError(500)))
                ).onFailure { case ApiError(409) => Reaction.Notify }))
            )))
            _ <- mountWith(ui) { (screen, handle) =>
                for
                    _         <- screen.await("the form")(_.contains("conflict"))
                    _         <- click(handle, Seq("0", "0", "0"))
                    notified  <- n.next
                    afterFirst <- screen.now
                    _         <- click(handle, Seq("0", "0", "1"))
                    _         <- screen.await("the mount's error view")(_.contains("error:Handler:500"))
                yield
                    assert(notified == ApiError(409), s"the sink received: $notified")
                    assert(afterFirst.contains("conflict"), s"the form after the notified failure: $afterFirst")
            }
        yield succeed
    }

    "an inner boundary's rules come before an outer one's" in {
        for
            inner <- AtomicRef.init(Chunk.empty[Int])
            outer <- AtomicRef.init(Chunk.empty[Int])
            ui = div(UI.boundary.onFailure { case ApiError(c) => Reaction.Run(outer.getAndUpdate(_.append(c)).unit) }(div(
                UI.boundary.onFailure { case ApiError(409) => Reaction.Run(inner.getAndUpdate(_.append(409)).unit) }(div(
                    button("conflict").onClick(Abort.fail(ApiError(409))),
                    button("broken").onClick(Abort.fail(ApiError(500)))
                ))
            )))
            got <- mountWith(ui) { (screen, handle) =>
                for
                    _ <- screen.await("the buttons")(_.contains("conflict"))
                    _ <- click(handle, Seq("0", "0", "0"))
                    _ <- click(handle, Seq("0", "0", "1"))
                    i <- inner.get
                    o <- outer.get
                yield (i, o)
            }
        yield
            assert(got._1 == Chunk(409), s"the inner boundary ran for: ${got._1}")
            assert(got._2 == Chunk(500), s"the outer boundary ran for: ${got._2}")
    }

    "the innermost sink takes a notice" in {
        for
            inner <- notices
            outer <- notices
            ui = div(UI.boundary.onNotice(outer.sink)(div(UI.boundary.onNotice(inner.sink)(div(
                button("save").onClick(Abort.fail(ApiError(409)))
            )))))
            got <- mountWith(ui)((screen, handle) => screen.await("save")(_.contains("save")).andThen(click(handle, Seq("0", "0", "0"))).andThen(inner.next))
            o   <- outer.all
        yield
            assert(got == ApiError(409), s"the inner sink received: $got")
            assert(o.isEmpty, s"the outer sink received: $o")
    }

    "Escalate with no mount around the handler is notified" in {
        for
            n <- notices
            ui = div(UI.boundary.onFailure { case _ => Reaction.Escalate }.onNotice(n.sink)(div(
                button("save").onClick(Abort.fail(ApiError(409)))
            )))
            got <- mountWith(ui)((screen, handle) => screen.await("save")(_.contains("save")).andThen(click(handle, Seq("0", "0"))).andThen(n.next))
        yield assert(got == ApiError(409), s"the sink received: $got")
    }

    "a Run action that fails in turn is notified" in {
        for
            n <- notices
            ui = div(UI.boundary.onFailure { case ApiError(401) => Reaction.Run(Abort.fail(ApiError(503))) }.onNotice(n.sink)(div(
                button("save").onClick(Abort.fail(ApiError(401)))
            )))
            got <- mountWith(ui)((screen, handle) => screen.await("save")(_.contains("save")).andThen(click(handle, Seq("0", "0"))).andThen(n.next))
        yield assert(got == ApiError(503), s"the sink received: $got")
    }

    "a panic in a handler goes through the same rules" in {
        for
            hit <- Promise.init[String, Any]
            ui = div(UI.boundary.onFailure { case e: IllegalStateException => Reaction.Run(hit.completeDiscard(Result.succeed(e.getMessage))) }(
                div(button("save").onClick(Sync.defer(throw new IllegalStateException("broken"))))
            ))
            got <- mountWith(ui)((screen, handle) =>
                screen.await("save")(_.contains("save")).andThen(click(handle, Seq("0", "0"))).andThen(within("the rule")(hit.get))
            )
        yield assert(got == "broken")
    }

    "the handlers around a failed one still run" in {
        for
            n      <- notices
            parent <- AtomicInt.init(0)
            ui = div(UI.boundary.onNotice(n.sink)(div(button("save").onClick(Abort.fail(ApiError(409)))).onClick(parent.incrementAndGet.unit)))
            got <- mountWith(ui) { (screen, handle) =>
                for
                    _ <- screen.await("save")(_.contains("save"))
                    _ <- click(handle, Seq("0", "0"))
                    _ <- n.next
                    p <- parent.get
                yield p
            }
        yield assert(got == 1, s"the parent's handler ran $got times")
    }

    "a mount that fails with a typed error keeps it as raised and wraps it as the error" in {
        val ui = div(UI.mounted(Abort.fail(ApiError(7))).onError(e =>
            span(s"failure:${e.failure}|wrapped:${e.error.isInstanceOf[UI.FailureException]}|${e.source}")
        ))
        mountWith(ui) { (screen, _) =>
            screen.await("the error view")(_.contains("failure:ApiError(7)|wrapped:true|Effect")).andThen(succeed)
        }
    }

    "a failed UI.fork fails its mount when no rule matches, and is notified with the content kept under a Notify rule" in {
        def feed(gate: Promise[Unit, Any], text: String)(using Frame) =
            UI.mounted(UI.fork(gate.get.andThen(Abort.fail(ApiError(3)))).andThen(span(text)))
        for
            n     <- notices
            gateA <- Promise.init[Unit, Any]
            gateB <- Promise.init[Unit, Any]
            ui = div(
                UI.boundary.onError(errorView)(div(feed(gateA, "plain"))),
                UI.boundary.onFailure { case ApiError(3) => Reaction.Notify }.onNotice(n.sink)(div(feed(gateB, "kept")))
            )
            _ <- mountWith(ui) { (screen, _) =>
                for
                    _     <- screen.await("both feeds")(v => v.contains("plain") && v.contains("kept"))
                    _     <- gateA.completeUnitDiscard
                    _     <- screen.await("the plain feed's error view")(_.contains("error:Fork:3"))
                    _     <- gateB.completeUnitDiscard
                    got   <- n.next
                    shown <- screen.now
                yield
                    assert(got == ApiError(3), s"the sink received: $got")
                    assert(shown.contains("kept"), s"the screen after the notified feed failure: $shown")
            }
        yield succeed
        end for
    }

    "UI.report keeps the reporting fiber running, unless a rule escalates" in {
        for
            n    <- notices
            done <- Promise.init[Unit, Any]
            poll = UI.mounted(
                UI.fork(UI.report(ApiError(1), Reaction.Notify).andThen(UI.report(ApiError(2), Reaction.Notify)).andThen(done.completeUnitDiscard))
                    .andThen(span("polling"))
            )
            escalated = UI.mounted(UI.fork(UI.report(ApiError(9), Reaction.Notify)).andThen(span("escalating")))
            ui = div(
                UI.boundary.onNotice(n.sink)(div(poll)),
                UI.boundary.onFailure { case ApiError(9) => Reaction.Escalate }.onError(errorView)(div(escalated))
            )
            _ <- mountWith(ui) { (screen, _) =>
                for
                    _   <- within("the poll to finish")(done.get)
                    all <- n.all
                    _   <- screen.await("the escalated report's error view")(_.contains("error:Fork:9"))
                yield assert(all == Chunk(ApiError(1), ApiError(2)), s"the sink received: $all")
            }
        yield succeed
    }

    "UI.notify from a mount effect reaches the boundary's sink" in {
        for
            n <- notices
            ui = div(UI.boundary.onNotice(n.sink)(div(UI.mounted(UI.notify(ApiError(5)).andThen(span("content"))))))
            got <- mountWith(ui)((_, _) => n.next)
        yield assert(got == ApiError(5), s"the sink received: $got")
    }

end HandlerFailureTest
