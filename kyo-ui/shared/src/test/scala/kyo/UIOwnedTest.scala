package kyo

import java.util.concurrent.atomic.AtomicReference
import kyo.UI.*
import kyo.internal.HtmlRenderer
import kyo.internal.ReactiveUI
import kyo.internal.RecordingExchange

class UIOwnedTest extends kyo.test.Test[Any]:

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

    "starts at its initial value, and a write repaints" in {
        val instances = new Instances
        val rec       = new RecordingExchange
        ReactiveUI.subscribed(div(UI.owned(7)(instances.draw)), rec) { _ =>
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

    "an effectful initial value is read when the node attaches, once per instance" in {
        val instances = new Instances
        val rec       = new RecordingExchange
        for
            reads  <- AtomicInt.init(0)
            parent <- Signal.initRef(0)
            ui = div(parent.render(_ => UI.owned(reads.incrementAndGet)(instances.draw)))
            _        <- HtmlRenderer.render(ui, Seq.empty)
            atRender <- reads.get
            counts   <- ReactiveUI.subscribed(ui, rec) { _ =>
                for
                    _     <- drawn(instances).map(parked)
                    first <- reads.get
                    _     <- parent.set(1)
                    _     <- assertEventually(Sync.defer(instances.seen.size == 2))
                    next  <- reads.get
                yield (first, next)
            }
        yield
            assert(atRender == 0)
            assert(counts == (1, 2))
        end for
    }

    "the placeholder is what a static page shows" in {
        val instances = new Instances
        HtmlRenderer.render(div(UI.owned(1)(instances.draw).placeholder(span("wait"))), Seq.empty).map { html =>
            assert(html.contains("wait"))
            assert(instances.seen.isEmpty)
        }
    }

    "closing the scope releases it" in {
        val instances = new Instances
        val rec       = new RecordingExchange
        for
            ref <- ReactiveUI.subscribed(div(UI.owned(1)(instances.draw)), rec) { _ =>
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
            ui = div(parent.render(_ => UI.owned(0)(instances.draw)))
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

    "a keyed instance keeps its value across that re-render" in {
        val instances = new Instances
        val rec       = new RecordingExchange
        for
            parent <- Signal.initRef(0)
            ui = div(parent.render(_ => UI.owned(0)(instances.draw).keyed("k")))
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

end UIOwnedTest
