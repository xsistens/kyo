package kyo

import kyo.Browser.*

/** Behavior tests for [[kyo.UI.mounted]] — the effectful component mount node.
  *
  * Runs against the LiveView transport (UITest.withUI = runHandlers + real Chrome over the WebSocket
  * exchange), i.e. the shared ReactiveUI engine: mount effects execute server-side on fibers descending
  * from the session, which is also what makes the Env test meaningful (context flows through the
  * HttpServer.init ancestry into the mount effect at run time, with no Env in any UI-facing type).
  */
class MountedTest extends UITest:

    "mounted effect renders its content" in {
        val app: UI < Async =
            Kyo.lift(UI.div(
                UI.mounted(Kyo.lift(UI.span("hello-from-mount").id("content")))
                    .placeholder(UI.span("loading...").id("ph"))
            ))
        withUI(app) {
            Browser.assertText(Selector.id("content"), "hello-from-mount")
        }
    }

    "placeholder shows while the effect is pending, content after it publishes" in {
        val app: (Promise[Unit, Any], UI < Async) < Sync =
            for gate <- Promise.init[Unit, Any]
            yield (
                gate,
                Kyo.lift(UI.div(
                    UI.mounted(gate.get.andThen(Kyo.lift(UI.span("ready").id("content"))))
                        .placeholder(UI.span("loading...").id("ph"))
                ))
            )
        app.map { (gate, ui) =>
            withUI(ui) {
                for
                    _ <- Browser.assertText(Selector.id("ph"), "loading...")
                    _ <- gate.completeUnitDiscard
                    _ <- Browser.assertText(Selector.id("content"), "ready")
                yield ()
            }
        }
    }

    "Env provided at the mount root flows into the mount effect (runMount-style fiber ancestry)" in {
        // Direct engine test: ReactiveUI.subscribe is invoked from THIS computation, so every renderer fiber —
        // including the mount runner — descends from it, exactly like the browser runMount path (DomBackend calls
        // subscribe inline). Env is a ContextEffect copied across fiber forks, so the erased Env[String] inside the
        // mounted effect resolves at run time with zero Env in any UI-facing type.
        //
        // NOTE deliberately NOT via withUI/runHandlers: in the LiveView transport the session fibers descend from
        // the kyo-http server internals, not from the runHandlers caller, so root-provided Env does not reach mount
        // effects there today (documented follow-up: context capture in UIServer or kyo-http).
        val greetEffect: UI < (Async & Scope & Env[String]) =
            Env.get[String].map(greeting => UI.span(greeting).id("content"))
        val ui: UI = UI.div(UI.mounted(greetEffect))
        Env.run("hello-env") {
            Scope.run {
                for
                    seen <- Promise.init[String, Any]
                    exchange = new kyo.internal.UIExchange:
                        def onChange(path: Seq[String], changed: UI)(using Frame): Unit < Async =
                            kyo.internal.HtmlRenderer.render(changed, path).map { html =>
                                if html.contains("hello-env") then seen.completeDiscard(Result.succeed(html))
                                else ()
                            }
                    root <- kyo.internal.ReactiveUI.normalize(ui, Seq.empty)
                    _    <- kyo.internal.ReactiveUI.subscribe(root, exchange)
                    html <- seen.get
                yield assert(html.contains("hello-env"))
            }
        }
    }

    "keyless mounted remounts (effect re-runs) when the enclosing region re-renders" in {
        val app: UI < Async =
            for
                runs <- AtomicInt.init(0)
                tick <- Signal.initRef(0)
            yield UI.div(
                UI.button("Tick").id("tick").onClick(tick.getAndUpdate(_ + 1).unit),
                tick.map(_ => UI.div(UI.mounted(runs.incrementAndGet.map(n => UI.span(s"runs:$n").id("content")))))
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("content"), "runs:1")
                _ <- Browser.click(Selector.id("tick"))
                _ <- Browser.assertText(Selector.id("content"), "runs:2")
                _ <- Browser.click(Selector.id("tick"))
                _ <- Browser.assertText(Selector.id("content"), "runs:3")
            yield ()
        }
    }

    "keyed mounted keeps its live instance across region re-renders" in {
        val app: UI < Async =
            for
                runs <- AtomicInt.init(0)
                tick <- Signal.initRef(0)
            yield UI.div(
                UI.button("Tick").id("tick").onClick(tick.getAndUpdate(_ + 1).unit),
                tick.map(n =>
                    UI.div(
                        UI.span(s"tick:$n").id("tickview"),
                        UI.mounted(runs.incrementAndGet.map(r => UI.span(s"runs:$r").id("content"))).keyed("box")
                    )
                )
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("content"), "runs:1")
                _ <- Browser.click(Selector.id("tick"))
                // The region re-rendered (tickview changed) but the keyed instance was adopted, not re-run.
                _ <- Browser.assertText(Selector.id("tickview"), "tick:1")
                _ <- Browser.assertText(Selector.id("content"), "runs:1")
                _ <- Browser.click(Selector.id("tick"))
                _ <- Browser.assertText(Selector.id("tickview"), "tick:2")
                _ <- Browser.assertText(Selector.id("content"), "runs:1")
            yield ()
        }
    }

    "keyed mounted keeps component-local signal state across region re-renders" in {
        def counterComponent(runs: AtomicInt): UI < (Async & Scope) =
            for
                _     <- runs.incrementAndGet
                count <- Signal.initRef(0)
            yield UI.div(
                UI.button("Inc").id("inc").onClick(count.getAndUpdate(_ + 1).unit),
                count.map(c => UI.span(s"count:$c").id("count"))
            )
        val app: UI < Async =
            for
                runs <- AtomicInt.init(0)
                tick <- Signal.initRef(0)
            yield UI.div(
                UI.button("Tick").id("tick").onClick(tick.getAndUpdate(_ + 1).unit),
                tick.map(_ => UI.div(UI.mounted(counterComponent(runs)).keyed("counter")))
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("count"), "count:0")
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.assertText(Selector.id("count"), "count:2")
                _ <- Browser.click(Selector.id("tick"))                  // enclosing region re-renders wholesale
                _ <- Browser.assertText(Selector.id("count"), "count:2") // state survived: same instance
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.assertText(Selector.id("count"), "count:3") // and it is still interactive
            yield ()
        }
    }

    "key change tears down the old instance (Scope closed) and mounts a fresh one" in {
        def component(key: Int, runs: AtomicInt, released: AtomicInt): UI < (Async & Scope) =
            for
                _ <- runs.incrementAndGet
                _ <- Scope.ensure(released.incrementAndGet.unit)
            yield UI.span(s"instance:$key").id("content")
        val app: (AtomicInt, AtomicInt, UI < Async) < Sync =
            for
                runs     <- AtomicInt.init(0)
                released <- AtomicInt.init(0)
                sel      <- Signal.initRef(1)
            yield (
                runs,
                released,
                Kyo.lift(UI.div(
                    UI.button("Next").id("next").onClick(sel.getAndUpdate(_ + 1).unit),
                    sel.map(k => UI.div(UI.mounted(component(k, runs, released)).keyed(k)))
                ))
            )
        app.map { (runs, released, ui) =>
            withUI(ui) {
                for
                    _ <- Browser.assertText(Selector.id("content"), "instance:1")
                    _ <- Browser.click(Selector.id("next"))
                    _ <- Browser.assertText(Selector.id("content"), "instance:2")
                    r <- released.get
                    n <- runs.get
                yield
                    assert(r == 1) // old instance's Scope was closed on key change
                    assert(n == 2) // and the effect ran exactly once per key
            }
        }
    }

    "failed mount renders the error state while the enclosing region stays alive" in {
        val app: UI < Async =
            for other <- Signal.initRef("sibling-0")
            yield UI.div(
                UI.button("Bump").id("bump").onClick(other.set("sibling-1")),
                other.map(v => UI.span(v).id("sibling")),
                UI.mounted(Abort.fail(new RuntimeException("boom-mount")))
                    .onError(t => UI.span(s"error:${t.getMessage}").id("content"))
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("content"), "error:boom-mount")
                _ <- Browser.click(Selector.id("bump"))
                _ <- Browser.assertText(Selector.id("sibling"), "sibling-1") // page did not die with the mount
            yield ()
        }
    }

    "duplicate mounted key: the first slot mounts, the duplicate slot renders an inline error card" in {
        val app: UI < Async =
            for
                runs <- AtomicInt.init(0)
            yield UI.div(
                UI.mounted(runs.incrementAndGet.map(r => UI.span(s"runs:$r").id("first"))).keyed("dup"),
                UI.mounted(runs.incrementAndGet.map(r => UI.span(s"runs:$r").id("second"))).keyed("dup")
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("first"), "runs:1")
                // The duplicate slot never runs its effect — it paints the loud error card instead of
                // silently sharing the first slot's instance (the log warning names key and path).
                _ <- Browser.assertText(Selector.css(".kyo-mount-error"), "kyo-ui: duplicate mounted key 'dup'")
            yield ()
        }
    }

    "unstable key (fresh per re-render) re-creates the instance each pass (the churn-hint path)" in {
        val app: UI < Async =
            for
                runs <- AtomicInt.init(0)
                tick <- Signal.initRef(0)
            yield UI.div(
                UI.button("Tick").id("tick").onClick(tick.getAndUpdate(_ + 1).unit),
                tick.map(n =>
                    UI.div(
                        UI.mounted(runs.incrementAndGet.map(r => UI.span(s"runs:$r").id("content"))).keyed(("box", n))
                    )
                )
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("content"), "runs:1")
                _ <- Browser.click(Selector.id("tick"))
                _ <- Browser.assertText(Selector.id("content"), "runs:2")
                _ <- Browser.click(Selector.id("tick"))
                _ <- Browser.assertText(Selector.id("content"), "runs:3")
                // Third consecutive key change — the unstable-key dev hint logs on this pass.
                _ <- Browser.click(Selector.id("tick"))
                _ <- Browser.assertText(Selector.id("content"), "runs:4")
            yield ()
        }
    }

end MountedTest
