package kyo.internal

import kyo.*

/** Direct-engine tests for the session handler-error policy (`normalize(onHandlerError = …)` →
  * `safeDispatch`): an event handler's Abort/panic is offered to the policy, bubbling continues, success never
  * invokes it, and with no policy the behavior is the log-only status quo.
  */
class HandlerErrorPolicyTest extends kyo.test.Test[Any]:

    private val boom = new RuntimeException("action failed")

    private val silentExchange = new UIExchange:
        def onChange(path: Seq[String], changed: UI)(using Frame): Unit < Async = ()

    private def click(path: Seq[String]): UIEvent =
        UIEvent.Click(path, MouseEventData(UI.Modifiers.none, Absent))

    "a handler Abort reaches the policy and the event still dispatches (bubbling continues)" in {
        Scope.run {
            for
                seen <- AtomicRef.init(Maybe.empty[Throwable])
                ui = UI.div(
                    UI.button("go").id("go").onClick(Abort.fail(boom))
                )
                root <- ReactiveUI.normalize(
                    ui,
                    Seq.empty,
                    onHandlerError = Present(err => seen.set(Present(err)))
                )
                dispatch <- ReactiveUI.subscribe(root, silentExchange)
                handled  <- dispatch.handle(Seq("0"), click(Seq("0")))
                s        <- seen.get
            yield
                assert(handled)
                assert(s == Present(boom))
        }
    }

    "a handler panic reaches the policy too" in {
        Scope.run {
            for
                seen <- AtomicRef.init(Maybe.empty[Throwable])
                ui = UI.div(
                    UI.button("go").id("go").onClick(Sync.defer((throw boom): Unit))
                )
                root <- ReactiveUI.normalize(
                    ui,
                    Seq.empty,
                    onHandlerError = Present(err => seen.set(Present(err)))
                )
                dispatch <- ReactiveUI.subscribe(root, silentExchange)
                handled  <- dispatch.handle(Seq("0"), click(Seq("0")))
                s        <- seen.get
            yield
                assert(handled)
                assert(s == Present(boom))
        }
    }

    "a successful handler never invokes the policy" in {
        Scope.run {
            for
                seen    <- AtomicRef.init(false)
                clicked <- AtomicInt.init(0)
                ui = UI.div(
                    UI.button("go").id("go").onClick(clicked.incrementAndGet.unit)
                )
                root <- ReactiveUI.normalize(
                    ui,
                    Seq.empty,
                    onHandlerError = Present(_ => seen.set(true))
                )
                dispatch <- ReactiveUI.subscribe(root, silentExchange)
                _        <- dispatch.handle(Seq("0"), click(Seq("0")))
                n        <- clicked.get
                s        <- seen.get
            yield
                assert(n == 1)
                assert(!s)
        }
    }

    "a handler that consumes its own failure keeps it from the policy; Abort.tap lets it bubble" in {
        Scope.run {
            for
                policySaw <- AtomicRef.init(List.empty[String])
                tapSaw    <- AtomicRef.init(false)
                ui = UI.div(
                    // consumed locally: never reaches the policy
                    UI.button("a").id("a").onClick(Abort.recover[Throwable](_ => ())(Abort.fail(boom): Unit < Abort[Throwable])),
                    // observed via tap: bubbles on to the policy
                    UI.button("b").id("b").onClick(Abort.tap[Throwable](_ => tapSaw.set(true))(Abort.fail(boom): Unit < Abort[Throwable]))
                )
                root <- ReactiveUI.normalize(
                    ui,
                    Seq.empty,
                    onHandlerError = Present(err => policySaw.getAndUpdate(err.getMessage :: _).unit)
                )
                dispatch <- ReactiveUI.subscribe(root, silentExchange)
                _        <- dispatch.handle(Seq("0"), click(Seq("0")))
                _        <- dispatch.handle(Seq("1"), click(Seq("1")))
                p        <- policySaw.get
                t        <- tapSaw.get
            yield
                assert(p == List("action failed")) // only button b's failure arrived
                assert(t)                          // and the tap observed it on the way
        }
    }

    "no policy installed: dispatch behaves as before (log-only, bubbling continues)" in {
        Scope.run {
            for
                ui       <- Kyo.lift(UI.div(UI.button("go").id("go").onClick(Abort.fail(boom))))
                root     <- ReactiveUI.normalize(ui, Seq.empty)
                dispatch <- ReactiveUI.subscribe(root, silentExchange)
                handled  <- dispatch.handle(Seq("0"), click(Seq("0")))
            yield assert(handled)
        }
    }
end HandlerErrorPolicyTest
