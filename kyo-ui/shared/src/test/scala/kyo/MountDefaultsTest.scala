package kyo

import kyo.internal.HtmlRenderer
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.UIEvent
import kyo.internal.UIExchange

/** What [[kyo.UI.defaults]] has to do for an app to set its mount error view and placeholder once, at the root, and change them in a
  * subtree: reach every mount below it, wherever that mount is subscribed, and be part of the first paint of the mount.
  */
class MountDefaultsTest extends kyo.test.Test[Any]:

    import UI.*

    private class Recorder extends UIExchange:
        private val paints = AtomicRef.Unsafe.init(Chunk.empty[(Seq[String], String)])(using AllowUnsafe.embrace.danger)

        def onChange(
            region: ReactiveRegion,
            path: Seq[String],
            context: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            ui: UI
        )(using Frame): Unit < Async =
            HtmlRenderer.render(ui, path).map(html => Sync.Unsafe.defer(discard(paints.getAndUpdate(_.append((path, html))))))

        def initial(ui: UI)(using Frame): Unit < Sync =
            HtmlRenderer.render(ui, Seq.empty).map(html => Sync.Unsafe.defer(discard(paints.getAndUpdate(_.append((Seq.empty, html))))))

        def shown(path: Seq[String])(using Frame): String < Sync =
            Sync.Unsafe.defer(paints.get()).map { all =>
                all.reverseIterator.find((p, _) => path.startsWith(p)).map(_._2).getOrElse("")
            }

        /** The last paint at exactly `path`. */
        def lastAt(path: Seq[String])(using Frame): String < Sync =
            Sync.Unsafe.defer(paints.get()).map(_.reverseIterator.find((p, _) => p == path).map(_._2).getOrElse(""))
    end Recorder

    private def mount[A](ui: UI)(f: Recorder => A < (Async & Scope))(using Frame): A < Async =
        Scope.run {
            val recorder = new Recorder
            for
                _    <- recorder.initial(ui)
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, recorder)
                a    <- f(recorder)
            yield a
            end for
        }

    private def showsEventually(recorder: Recorder, path: Seq[String], text: String)(using Frame, kyo.test.AssertScope): Unit < Async =
        assertEventually(recorder.shown(path).map(_.contains(text)))

    private case class Boom(msg: String) extends Exception(msg)
    private case object Missing          extends Exception("missing")

    private def failing(msg: String)(using Frame): UI.Ast.Mounted = UI.mounted(Abort.fail(Boom(msg)))

    private val card: PartialFunction[MountError, UI] = { case e => span(s"card:${e.message}") }

    "an error view reaches a mount below it" in {
        val ui = div(UI.defaults.onError(card)(div(failing("boom"))))
        mount(ui)(recorder => showsEventually(recorder, Seq("0", "0"), "card:boom")).andThen(succeed)
    }

    "an error view reaches a mount in a region below it, also after the region re-renders" in {
        for
            tick <- Signal.initRef(0)
            ui = div(UI.defaults.onError(card)(div(tick.render(n => div(failing(s"boom$n"))))))
            _ <- mount(ui) { recorder =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0", "0"), "card:boom0")
                    _ <- tick.set(1)
                    _ <- showsEventually(recorder, Seq("0", "0", "0"), "card:boom1")
                yield ()
            }
        yield succeed
    }

    "an error view reaches a mount in the content of another mount" in {
        val ui = div(UI.defaults.onError(card)(div(UI.mounted(Kyo.lift(div(failing("inner")))))))
        mount(ui)(recorder => showsEventually(recorder, Seq("0", "0", "0"), "card:inner")).andThen(succeed)
    }

    "an error view reaches a mount in a keyed list row" in {
        for
            items <- Signal.initRef(Chunk("a", "b"))
            ui = div(UI.defaults.onError(card)(ul(items.foreachKeyed(identity)(k => li(failing(k))))))
            _ <- mount(ui) { recorder =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0", "a", "0"), "card:a")
                    _ <- items.set(Chunk("a", "b", "c"))
                    _ <- showsEventually(recorder, Seq("0", "0", "c", "0"), "card:c")
                yield ()
            }
        yield succeed
    }

    "an error view reaches a mount that fails after it published" in {
        for
            feed <- Promise.init[Unit, Any]
            ui = div(UI.defaults.onError(card)(div(UI.mounted(UI.fork(feed.get.andThen(Abort.fail(Boom("late")))).andThen(span("up"))))))
            _ <- mount(ui) { recorder =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0"), "up")
                    _ <- feed.completeUnitDiscard
                    _ <- showsEventually(recorder, Seq("0", "0"), "card:late")
                yield ()
            }
        yield succeed
    }

    "an error view does not reach a sibling outside it" in {
        val ui = div(UI.defaults.onError(card)(div()), div(failing("outside")))
        mount(ui) { recorder =>
            for
                _     <- showsEventually(recorder, Seq("1", "0"), "outside")
                shown <- recorder.shown(Seq("1", "0"))
            yield assert(shown.contains("kyo-mount-error") && !shown.contains("card:"), shown)
        }
    }

    "the inner error view wins, and a failure it does not match goes to the outer one" in {
        val inner: PartialFunction[MountError, UI] = { case e if e.error == Missing => span("inner:missing") }
        val outer: PartialFunction[MountError, UI] = { case e => span(s"outer:${e.message}") }
        val ui = div(UI.defaults.onError(outer)(div(UI.defaults.onError(inner)(div(UI.mounted(Abort.fail(Missing)), failing("x"))))))
        mount(ui) { recorder =>
            for
                _ <- showsEventually(recorder, Seq("0", "0", "0"), "inner:missing")
                _ <- showsEventually(recorder, Seq("0", "0", "1"), "outer:x")
            yield ()
        }.andThen(succeed)
    }

    "a failure no error view matches gets the built-in error node" in {
        val only: PartialFunction[MountError, UI] = { case e if e.error == Missing => span("missing") }
        val ui                                    = div(UI.defaults.onError(only)(div(failing("other"))))
        mount(ui) { recorder =>
            for
                _     <- showsEventually(recorder, Seq("0", "0"), "other")
                shown <- recorder.shown(Seq("0", "0"))
            yield assert(shown.contains("kyo-mount-error"), shown)
        }
    }

    "a mount's own onError wins over the default" in {
        val ui = div(UI.defaults.onError(card)(div(failing("boom").onError(_ => span("own")))))
        mount(ui)(recorder => showsEventually(recorder, Seq("0", "0"), "own")).andThen(succeed)
    }

    "a default placeholder is in the server-rendered page" in {
        for
            gate <- Promise.init[Unit, Any]
            ui = div(UI.defaults.placeholder(span("spin"))(div(UI.mounted(gate.get.andThen(span("ready"))))))
            html <- HtmlRenderer.render(ui, Seq.empty)
        yield assert(html.contains("spin"), html)
    }

    "a default placeholder is in the paint of the region that shows the mount" in {
        for
            gate <- Promise.init[Unit, Any]
            tick <- Signal.initRef(0)
            ui = div(
                UI.defaults.placeholder(span("spin"))(div(tick.render(n => div(span(s"tick:$n"), UI.mounted(gate.get.andThen(span("x")))))))
            )
            _ <- mount(ui) { recorder =>
                for
                    _       <- tick.set(1)
                    _       <- showsEventually(recorder, Seq("0", "0", "0"), "tick:1")
                    painted <- recorder.lastAt(Seq("0", "0"))
                yield assert(painted.contains("spin"), painted)
            }
        yield succeed
    }

    "a mount's own placeholder wins over the default" in {
        for
            gate <- Promise.init[Unit, Any]
            ui = div(UI.defaults.placeholder(span("spin"))(div(UI.mounted(gate.get.andThen(span("x"))).placeholder(span("own")))))
            html <- HtmlRenderer.render(ui, Seq.empty)
        yield assert(html.contains("own") && !html.contains("spin"), html)
    }

    "a keyed mount below a default keeps its instance while its region re-renders" in {
        for
            runs <- AtomicInt.init(0)
            tick <- Signal.initRef(0)
            ui = div(UI.defaults.onError(card)(div(tick.render(n =>
                div(span(s"tick:$n"), UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("box"))
            ))))
            _ <- mount(ui) { recorder =>
                for
                    _ <- showsEventually(recorder, Seq("0", "0", "1"), "runs:1")
                    _ <- tick.set(1)
                    _ <- showsEventually(recorder, Seq("0", "0", "0"), "tick:1")
                    _ <- tick.set(2)
                    _ <- showsEventually(recorder, Seq("0", "0", "0"), "tick:2")
                    _ <- showsEventually(recorder, Seq("0", "0", "1"), "runs:1")
                yield ()
            }
            total <- runs.get
        yield assert(total == 1, s"runs: $total")
    }
end MountDefaultsTest
