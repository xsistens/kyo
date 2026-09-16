package kyo.internal

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import kyo.*

/** The callback binding of a lone-text region (`ReactiveUI.bindTextRegion`).
  *
  * A `Signal[String]` in a child position renders one text node, so its patch is one DOM write and needs no
  * fiber — the same trade the attribute channels make in [[ReactiveChannelBindingTest]], one level up in the
  * tree. What differs, and is what these leaves pin down:
  *
  *   - a region has a fallback the channels do not have. Where a backend offers no text sink, the region path
  *     repaints it, and that repaint IS the patch. So "no sink" must mean the OLD behaviour exactly, not a
  *     dropped update;
  *   - the value reaches the sink RAW. The region path renders HTML, so its text goes through escaping; a bound
  *     region writes the string into a text node, where `&` and `<` are literal characters. A binding that
  *     escaped here would double-escape;
  *   - only a region that is statically text binds. `Signal[UI]` regions render arbitrary subtrees and must keep
  *     the render-walk-paint loop.
  */
class ReactiveTextBindingTest extends kyo.test.Test[Any]:

    /** Counts the two paths separately, so a leaf can tell WHICH one ran. `fast = false` reproduces a backend
      * with no synchronous text sink (the server transport).
      */
    final private class Recording(fast: Boolean):
        val patches                        = new AtomicInteger(0)
        val repaints                       = new AtomicInteger(0)
        val lastPatch                      = new AtomicReference[String]("")
        val lastRepaint                    = new AtomicReference[UI](null)
        private def bump(c: AtomicInteger) = discard(c.incrementAndGet())
        val exchange = new UIExchange:
            def onChange(path: Seq[String], changed: UI, mount: Boolean)(using Frame): Unit < Async =
                Sync.defer { bump(repaints); lastRepaint.set(changed) }
            override val textPatcherNow: Maybe[(Seq[String], String) => Unit] =
                if fast then
                    Present((_, v) =>
                        bump(patches); lastPatch.set(v)
                    )
                else Absent
    end Recording

    private def subscribed(ui: UI, rec: Recording)(using Frame): Unit < (Async & Scope) =
        ReactiveUI.normalize(ui, Seq.empty).map(ReactiveUI.subscribe(_, rec.exchange).unit)

    "a text region patches inside the writer's set, with no scheduler hop" in {
        Scope.run {
            val rec = new Recording(fast = true)
            for
                ref <- Signal.initRef("before")
                _   <- subscribed(UI.div(ref: Signal[String]), rec)
                before = rec.patches.get
                _ <- ref.set("after")
                // Read with NO sleep and NO assertEventually: on the region path the repaint would still be
                // queued here and this would read the same value as `before`.
                after = rec.patches.get
            yield
                assert(before == 0 && after == 1, s"before=$before after=$after")
                assert(rec.lastPatch.get == "after")
                assert(rec.repaints.get == 0, "the region path ran alongside the binding")
            end for
        }
    }

    "the value reaches the sink unescaped" in {
        Scope.run {
            val rec = new Recording(fast = true)
            for
                ref <- Signal.initRef("plain")
                _   <- subscribed(UI.div(ref: Signal[String]), rec)
                _   <- ref.set("a & b < c")
            yield assert(rec.lastPatch.get == "a & b < c")
            end for
        }
    }

    "an unchanged first value is not written" in {
        // The enclosing render already painted it; normalize happens-before that render, so the snapshot the
        // binding starts from is what the DOM shows.
        Scope.run {
            val rec = new Recording(fast = true)
            for
                ref <- Signal.initRef("same")
                _   <- subscribed(UI.div(ref: Signal[String]), rec)
                _   <- ref.set("same")
            yield assert(rec.patches.get == 0, s"patches=${rec.patches.get}")
            end for
        }
    }

    "closing the scope releases the registration" in {
        for
            ref <- Signal.initRef("before")
            rec = new Recording(fast = true)
            _      <- Scope.run(subscribed(UI.div(ref: Signal[String]), rec))
            parked <- ref.waiters
            // Moving the signal AFTER the scope closed is the whole leaf: the next-promise is masked and
            // nothing interrupts it, so a binding that forgets to release keeps patching a subtree that is gone.
            _ <- ref.set("after")
        yield assert(parked == 0 && rec.patches.get == 0, s"parked=$parked patches=${rec.patches.get}")
        end for
    }

    "a backend without a text sink keeps the region on the render path" in {
        Scope.run {
            val rec = new Recording(fast = false)
            for
                ref <- Signal.initRef("before")
                _   <- subscribed(UI.div(ref: Signal[String]), rec)
                _   <- ref.set("after")
                _   <- assertEventually(Sync.defer(rec.repaints.get == 1))
            yield
                assert(rec.patches.get == 0, "took the fast path with no sink offered")
                assert(rec.lastRepaint.get == UI.Ast.Text("after"), s"repainted with ${rec.lastRepaint.get}")
            end for
        }
    }

    "a Signal[UI] region is not bound, even when it renders text" in {
        // Same painted output, but the signal is over UI: nothing guarantees the next value is text, so the
        // region keeps the render-walk-paint loop.
        Scope.run {
            val rec = new Recording(fast = true)
            for
                ref <- Signal.initRef("before")
                _   <- subscribed(UI.div(ref.map(v => UI.span(v))), rec)
                _   <- ref.set("after")
                _   <- assertEventually(Sync.defer(rec.repaints.get == 1))
            yield assert(rec.patches.get == 0, "a UI-valued region took the text binding")
            end for
        }
    }

end ReactiveTextBindingTest
