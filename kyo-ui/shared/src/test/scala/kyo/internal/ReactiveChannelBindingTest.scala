package kyo.internal

import java.util.concurrent.atomic.AtomicInteger
import kyo.*

/** The callback binding of the reactive channels (`ReactiveUI.bindChannel`) for attribute, boolean attribute and
  * class channels.
  *
  * Where the exchange offers a synchronous sink, a channel binds as a callback on its signal instead of a fiber.
  * Three things have to hold for each channel:
  *
  *   - the patch lands inside the writer's own `set`;
  *   - closing the owning Scope releases the registration, which sits on a masked promise that nothing
  *     interrupts, so a missing release retains the callback and the subtree behind it;
  *   - an exchange without a synchronous sink (the server transport) keeps the fiber path.
  *
  * The three channels share one binding, so they are asserted together.
  */
class ReactiveChannelBindingTest extends kyo.test.Test[Any]:

    /** Counts the two sinks separately, so a test can tell which path ran. `fast = false` is an exchange
      * without a synchronous sink.
      */
    final private class Recording(fast: Boolean):
        val sync                                       = new AtomicInteger(0)
        val async                                      = new AtomicInteger(0)
        private def bump(counter: AtomicInteger): Unit = discard(counter.incrementAndGet())
        val exchange                                   = new UIExchange:
            def onChange(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                changed: UI
            )(using Frame): Unit < Async                                                                        = Kyo.unit
            override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
                Sync.defer(bump(async))
            override def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async =
                Sync.defer(bump(async))
            override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
                Sync.defer(bump(async))
            override val attrPatcherNow: Maybe[(Seq[String], String, String) => Unit] =
                if fast then Present((_, _, _) => bump(sync)) else Absent
            override val boolAttrPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit] =
                if fast then Present((_, _, _) => bump(sync)) else Absent
            override val classPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit] =
                if fast then Present((_, _, _) => bump(sync)) else Absent
    end Recording

    /** One channel under test, built outside any scope the test opens so that `move` and `waiters` outlive it: a
      * release test that can only reach its signal from inside the scope proves nothing.
      *
      * The rendered snapshot equals each signal's initial value, so the initial emission is skipped on either
      * path and every count below describes a real change.
      *
      * @param move
      *   changes the channel's signal once
      * @param waiters
      *   registrations currently parked on that signal
      */
    final private case class Channel(name: String, ui: UI, move: Unit < Async, waiters: Int < Sync)

    private def attrChannel(using Frame): Channel < Sync =
        Signal.initRef("a").map(ref =>
            Channel("attr", UI.div(UI.span("x").title(ref: Signal[String])), ref.set("b").unit, ref.waiters)
        )

    private def boolChannel(using Frame): Channel < Sync =
        Signal.initRef(false).map(ref =>
            Channel("bool-attr", UI.div(UI.input.disabled(ref: Signal[Boolean])), ref.set(true).unit, ref.waiters)
        )

    private def classChannel(using Frame): Channel < Sync =
        Signal.initRef(false).map(ref =>
            Channel("class", UI.div(UI.span("x").cssClass("hot", ref: Signal[Boolean])), ref.set(true).unit, ref.waiters)
        )

    private def channels(using Frame): Seq[Channel] < Sync =
        Kyo.foreach(Chunk(attrChannel, boolChannel, classChannel))(identity).map(_.toSeq)

    "every channel patches inside the writer's set, with no scheduler hop" in {
        Scope.run {
            channels.map(Kyo.foreachDiscard(_) { ch =>
                val rec = new Recording(fast = true)
                for
                    root <- ReactiveUI.normalize(ch.ui, Seq.empty)
                    _    <- ReactiveUI.subscribe(root, rec.exchange)
                    before = rec.sync.get
                    _ <- ch.move
                    // On the fiber path the patch would still be queued here.
                    after = rec.sync.get
                yield assert(before == 0 && after == 1 && rec.async.get == 0, s"${ch.name}: before=$before after=$after")
                end for
            })
        }
    }

    "closing the scope releases every channel's registration" in {
        channels.map(Kyo.foreachDiscard(_) { ch =>
            val rec = new Recording(fast = true)
            for
                _      <- Scope.run(ReactiveUI.normalize(ch.ui, Seq.empty).map(ReactiveUI.subscribe(_, rec.exchange).unit))
                parked <- ch.waiters
                // The signal moves after the scope closed: a binding that forgot to release would still patch
                // a subtree that is gone.
                _ <- ch.move
            yield assert(parked == 0 && rec.sync.get == 0, s"${ch.name}: parked=$parked patches=${rec.sync.get}")
            end for
        })
    }

    "an exchange without synchronous sinks keeps every channel on the fiber path" in {
        Scope.run {
            channels.map(Kyo.foreachDiscard(_) { ch =>
                val rec = new Recording(fast = false)
                for
                    root <- ReactiveUI.normalize(ch.ui, Seq.empty)
                    _    <- ReactiveUI.subscribe(root, rec.exchange)
                    _    <- ch.move
                    _    <- assertEventually(Sync.defer(rec.async.get == 1))
                yield assert(rec.sync.get == 0, s"${ch.name} took the fast path with no sink offered")
                end for
            })
        }
    }

end ReactiveChannelBindingTest
