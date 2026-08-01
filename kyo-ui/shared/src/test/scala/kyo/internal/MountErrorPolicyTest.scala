package kyo.internal

import java.util.concurrent.CopyOnWriteArrayList
import kyo.*

/** Direct-engine tests for the terminal station of mount-failure routing (no browser): the session's
  * [[kyo.UI.MountErrorPolicy]], installed with [[kyo.UI.mountErrors]] and read once at the runner entry.
  *
  * The engine renders nothing of its own for a failure any more. What these tests pin: the policy answers exactly
  * when nothing below it did (no node-local `.onError`, no matching boundary), it answers at most once per node
  * instance, and the [[kyo.UI.MountError]] it receives classifies the failure and names the node — including the
  * `Fork` case, which no effect type can describe because the mount effect already succeeded.
  *
  * With no policy installed the default answers, and the tree's unanswered mounts are announced once by source
  * position — the compile-time obligation's replacement, and the reason a tree with no mounted nodes pays nothing.
  *
  * Driven via `ReactiveUI.normalize`/`subscribe` with a recording `UIExchange` (the MountedTest direct-test idiom).
  */
class MountErrorPolicyTest extends kyo.test.Test[Any]:

    /** Records every emission's rendered HTML and completes marker promises on first match. */
    final private class Recording(markers: Map[String, Fiber.Promise.Unsafe[String, Any]]):
        val emissions = new CopyOnWriteArrayList[String]()
        val exchange = new UIExchange:
            def onChange(path: Seq[String], changed: UI, mount: Boolean)(using Frame): Unit < Async =
                HtmlRenderer.render(changed, path).map { html =>
                    discard(emissions.add(html))
                    markers.foreach { (marker, p) =>
                        if html.contains(marker) then
                            import AllowUnsafe.embrace.danger
                            discard(p.complete(Result.succeed(html)))
                    }
                }
        def count(marker: String): Int =
            import scala.jdk.CollectionConverters.*
            emissions.asScala.count(_.contains(marker))
    end Recording

    private def recording(markers: String*)(using Frame): (Recording, Map[String, Fiber[String, Any]]) < Sync =
        Sync.Unsafe.defer {
            val ps  = markers.map(m => m -> Fiber.Promise.Unsafe.init[String, Any]()).toMap
            val rec = new Recording(ps)
            (rec, ps.view.mapValues(_.safe: Fiber[String, Any]).toMap)
        }

    /** A policy standing in for the app's: records every failure it is asked to answer for and paints a marker. */
    final private class Spy:
        val seen = new CopyOnWriteArrayList[UI.MountError]()
        val policy: UI.MountErrorPolicy = UI.MountErrorPolicy { e =>
            given Frame = e.frame
            discard(seen.add(e))
            UI.span("POLICY").cssClass("kyo-mount-error")
        }
        def size: Int            = seen.size
        def first: UI.MountError = seen.get(0)
    end Spy

    private def keyOf(e: UI.MountError): String = e.key.map(_.toString).getOrElse("none")

    private def run(ui: UI, rec: Recording, spy: Spy)(using Frame): Unit < (Async & Scope) =
        ReactiveUI.normalize(ui, Seq.empty, Present(spy.policy)).map(root => ReactiveUI.subscribe(root, rec.exchange).unit)

    private def boom = new RuntimeException("feed died")

    "an unclaimed failure reaches the policy, classified as Effect and naming the node" in {
        Scope.run {
            for
                (rec, marks) <- recording("POLICY")
                spy = new Spy
                _ <- run(UI.div(UI.mounted(Abort.fail(boom)).keyed("k")), rec, spy)
                _ <- marks("POLICY").get
            yield
                assert(spy.size == 1)
                val e = spy.first
                assert(e.source == UI.MountErrorSource.Effect)
                assert(e.message == "feed died")
                assert(keyOf(e) == "k")
                assert(e.path.nonEmpty)
                assert(e.frame.position.show.contains("MountErrorPolicyTest"))
            end for
        }
    }

    "a panic of the mount effect classifies as Panic" in {
        Scope.run {
            for
                (rec, marks) <- recording("POLICY")
                spy = new Spy
                _ <- run(UI.div(UI.mounted(Abort.panic(boom))), rec, spy)
                _ <- marks("POLICY").get
            yield
                assert(spy.size == 1)
                assert(spy.first.source == UI.MountErrorSource.Panic)
            end for
        }
    }

    "a UI.fork fiber dying after publish classifies as Fork" in {
        Scope.run {
            for
                gate         <- Promise.init[Unit, Any]
                (rec, marks) <- recording("ready", "POLICY")
                spy = new Spy
                effect = UI.fork(gate.get.andThen(Abort.fail(boom)))
                    .map(_ => UI.span("ready").id("content"))
                _ <- run(UI.div(UI.mounted(effect).keyed("k")), rec, spy)
                _ <- marks("ready").get
                _ <- gate.completeUnitDiscard
                _ <- marks("POLICY").get
            yield
                assert(spy.size == 1)
                assert(spy.first.source == UI.MountErrorSource.Fork)
            end for
        }
    }

    "a node-local .onError claims the failure: the policy is never consulted" in {
        Scope.run {
            for
                (rec, marks) <- recording("custom:feed died")
                spy  = new Spy
                node = UI.mounted(Abort.fail(boom)).keyed("k").onError(t => UI.span(s"custom:${t.getMessage}"))
                _ <- run(UI.div(node), rec, spy)
                _ <- marks("custom:feed died").get
            yield assert(spy.size == 0)
        }
    }

    "a matching boundary claims the failure: the policy is never consulted" in {
        Scope.run {
            for
                (rec, marks) <- recording("boundary:feed died")
                spy = new Spy
                ui = UI.div(
                    UI.boundary.onError { case t: RuntimeException => UI.span(s"boundary:${t.getMessage}") }(
                        UI.mounted(Abort.fail(boom)).keyed("k")
                    )
                )
                _ <- run(ui, rec, spy)
                _ <- marks("boundary:feed died").get
            yield assert(spy.size == 0)
        }
    }

    "a non-matching boundary falls through to the policy" in {
        Scope.run {
            for
                (rec, marks) <- recording("POLICY")
                spy = new Spy
                ui = UI.div(
                    UI.boundary.onError { case _: IllegalStateException => UI.span("never") }(
                        UI.mounted(Abort.fail(boom)).keyed("k")
                    )
                )
                _ <- run(ui, rec, spy)
                _ <- marks("POLICY").get
            yield
                assert(spy.size == 1)
                assert(spy.first.source == UI.MountErrorSource.Effect)
            end for
        }
    }

    "the policy answers at most once per instance — two failing feeds, one answer" in {
        Scope.run {
            for
                gate1        <- Promise.init[Unit, Any]
                gate2        <- Promise.init[Unit, Any]
                (rec, marks) <- recording("ready", "POLICY")
                spy = new Spy
                effect =
                    for
                        _ <- UI.fork(gate1.get.andThen(Abort.fail(new RuntimeException("first"))))
                        _ <- UI.fork(gate2.get.andThen(Abort.fail(new RuntimeException("second"))))
                    yield UI.span("ready").id("content")
                _ <- run(UI.div(UI.mounted(effect).keyed("k")), rec, spy)
                _ <- marks("ready").get
                _ <- gate1.completeUnitDiscard
                _ <- marks("POLICY").get
                _ <- gate2.completeUnitDiscard
                _ <- Async.sleep(100L.millis)
            yield
                assert(spy.size == 1)
                assert(rec.count("POLICY") == 1)
            end for
        }
    }

    "a duplicate key renders through the policy and does NOT consult the node's .onError" in {
        // Two nodes claim one key in the same render pass: the second slot never mounts, so its `.onError`
        // (which answers for a FAILED MOUNT) must not fire — the caller bug goes straight to the policy.
        Scope.run {
            for
                (rec, marks) <- recording("POLICY")
                spy     = new Spy
                content = Kyo.lift(UI.span("ready").id("content"): UI)
                ui = UI.div(
                    UI.mounted(content).keyed("dup").onError(_ => UI.span("custom:never")),
                    UI.mounted(content).keyed("dup").onError(_ => UI.span("custom:never"))
                )
                _ <- run(ui, rec, spy)
                _ <- marks("POLICY").get
            yield
                assert(spy.size == 1)
                val e = spy.first
                assert(e.source == UI.MountErrorSource.DuplicateKey)
                assert(e.error.isInstanceOf[DuplicateMountKey])
                // The rendered text is the undecorated one-liner, NOT KyoException.getMessage (which is the
                // frame-decorated development block — right for a log, wrong for a page).
                assert(e.message == "kyo-ui: duplicate mounted key 'dup'")
                assert(rec.count("duplicate mounted key") == 0) // the Spy renders POLICY, not the message
                assert(rec.count("custom:never") == 0)
            end for
        }
    }

    "under MountErrorPolicy.message a duplicate key renders the undecorated one-liner" in {
        // Regression: routing the duplicate-key slot through an exception made the rendered text
        // KyoException.getMessage, which in development is a multi-line frame-decorated block.
        Scope.run {
            for
                // No apostrophes in the marker: the renderer escapes them, so the raw HTML carries `&#39;dup&#39;`.
                (rec, marks) <- recording("kyo-ui: duplicate mounted key")
                content = Kyo.lift(UI.span("ready").id("content"): UI)
                ui      = UI.div(UI.mounted(content).keyed("dup"), UI.mounted(content).keyed("dup"))
                root <- ReactiveUI.normalize(ui, Seq.empty, Present(UI.MountErrorPolicy.message))
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                html <- marks("kyo-ui: duplicate mounted key").get
            yield
                assert(!html.contains("KyoException"))
                assert(!html.contains("MountErrorPolicyTest.scala"))
            end for
        }
    }

    // ---- the default: what happens when the app installed nothing ----

    "with no policy installed the default marker answers, not the failure's own text" in {
        Scope.run {
            for
                (rec, marks) <- recording("mount failed")
                root         <- ReactiveUI.normalize(UI.div(UI.mounted(Abort.fail(boom))), Seq.empty)
                _            <- ReactiveUI.subscribe(root, rec.exchange)
                html         <- marks("mount failed").get
            yield
                assert(html.contains("kyo-mount-error"))
                assert(!html.contains("feed died"))
            end for
        }
    }

    /** Captures log lines so the startup announcement can be asserted on. Mirrors LogTest's capture sink. */
    private def capturingLog(into: AtomicRef.Unsafe[List[String]]): Log =
        import AllowUnsafe.embrace.danger
        Log(new Log.Unsafe:
            val level: Log.Level                = Log.Level.debug
            val name: String                    = "mount-error-policy-test"
            def withName(n: String): Log.Unsafe = this
            private def record(pfx: String, msg: => String)(using AllowUnsafe): Unit =
                discard(into.getAndUpdate(s"$pfx:$msg" :: _))
            def trace(msg: => String)(using Frame, AllowUnsafe): Unit                  = record("trace", msg)
            def trace(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit = record("trace", msg)
            def debug(msg: => String)(using Frame, AllowUnsafe): Unit                  = record("debug", msg)
            def debug(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit = record("debug", msg)
            def info(msg: => String)(using Frame, AllowUnsafe): Unit                   = record("info", msg)
            def info(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit  = record("info", msg)
            def warn(msg: => String)(using Frame, AllowUnsafe): Unit                   = record("warn", msg)
            def warn(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit  = record("warn", msg)
            def error(msg: => String)(using Frame, AllowUnsafe): Unit                  = record("error", msg)
            def error(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit = record("error", msg))
    end capturingLog

    "with no policy installed, mounts with no answer of their own are announced once, by source position" in {
        import AllowUnsafe.embrace.danger
        val captured = AtomicRef.Unsafe.init(List.empty[String])
        val ui = UI.div(
            UI.mounted(Kyo.lift(UI.span("a"): UI)),                              // unanswered
            UI.mounted(Kyo.lift(UI.span("b"): UI)),                              // unanswered
            UI.mounted(Kyo.lift(UI.span("c"): UI)).onError(_ => UI.span("own")), // answers itself
            UI.boundary.onError { case _: RuntimeException => UI.span("bnd") }( // answered by boundary
                UI.mounted(Kyo.lift(UI.span("d"): UI))
            )
        )
        for
            _ <- Log.let(capturingLog(captured))(ReactiveUI.normalize(ui, Seq.empty).unit)
            _ <- Log.flush
        yield
            val warns = captured.get().filter(_.startsWith("warn:"))
            assert(warns.size == 1, s"expected exactly one announcement, got $warns")
            val w = warns.head
            assert(w.contains("2 mounted node(s) have no answer"), w)
            assert(w.contains("MountErrorPolicyTest"), w)
        end for
    }

    "a tree with no mounted nodes announces nothing" in {
        import AllowUnsafe.embrace.danger
        val captured = AtomicRef.Unsafe.init(List.empty[String])
        for
            _ <- Log.let(capturingLog(captured))(ReactiveUI.normalize(UI.div(UI.span("plain")), Seq.empty).unit)
            _ <- Log.flush
        yield assert(captured.get().forall(!_.startsWith("warn:")))
        end for
    }

    "an installed policy silences the announcement" in {
        import AllowUnsafe.embrace.danger
        val captured = AtomicRef.Unsafe.init(List.empty[String])
        val spy      = new Spy
        for
            _ <- Log.let(capturingLog(captured))(
                ReactiveUI.normalize(UI.div(UI.mounted(Kyo.lift(UI.span("a"): UI))), Seq.empty, Present(spy.policy)).unit
            )
            _ <- Log.flush
        yield assert(captured.get().forall(!_.startsWith("warn:")))
        end for
    }

    "UI.mountErrors installs the policy the runner reads" in {
        Scope.run {
            val spy = new Spy
            UI.mountErrors(spy.policy) {
                ReactiveUI.mountErrorPolicy.get.map(p => assert(p.map(_ eq spy.policy).getOrElse(false)))
            }
        }
    }

end MountErrorPolicyTest
