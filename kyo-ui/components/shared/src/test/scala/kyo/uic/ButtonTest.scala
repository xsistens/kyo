package kyo.uic

import kyo.*

/** A button reads its three switches as one region, whichever of them follow a signal, and with none it draws no region at all. */
class ButtonTest extends UicTest:

    private def button(using Frame) = uic.Button("Save")

    private def regions(b: uic.Button)(using Frame): Int < Sync = regionsAbove(b.render, "p-button")

    "two signals are one region" in {
        for
            on <- Signal.initRef(false)
            n  <- regions(button.disabled(on).loading(on))
        yield assert(n == 1, s"$n regions")
    }

    "a signal in aria-disabled and one in loading are one region" in {
        for
            on <- Signal.initRef(false)
            n  <- regions(button.ariaDisabled(on).loading(on))
        yield assert(n == 1, s"$n regions")
    }

    "three signals are one region" in {
        for
            on <- Signal.initRef(false)
            n  <- regions(button.disabled(on).ariaDisabled(on).loading(on))
        yield assert(n == 1, s"$n regions")
    }

    "one signal is one region" in {
        for
            on <- Signal.initRef(false)
            d  <- regions(button.disabled(on))
            ad <- regions(button.ariaDisabled(on))
            l  <- regions(button.loading(on))
        yield assert(d == 1 && ad == 1 && l == 1, s"disabled $d, ariaDisabled $ad, loading $l")
    }

    "constants draw no region" in {
        regions(button.disabled(true).ariaDisabled(false).loading(false)).map(n => assert(n == 0, s"$n regions"))
    }

    "nothing set draws no region" in {
        regions(button).map(n => assert(n == 0, s"$n regions"))
    }

    "a signal beside a constant keeps the constant" in {
        for
            on <- Signal.initRef(false)
            ui = button.disabled(true).loading(on).render
            btn <- elementWithClass(ui, "p-button")
        yield btn match
            case b: UI.Ast.Button => assert(b.disabled.contains(true), s"disabled ${b.disabled}")
            case other            => fail(s"not a button: $other")
    }

    /** Runs a button with `disabled` and `loading` over refs, hands `drive` the refs once it reads them, and says whether the page
      * ended on both set: its observers parked again, so what it read is what the region left, and the last paint shows both.
      */
    private def endsOnBoth(log: kyo.internal.RecordingExchange)(drive: (SignalRef[Boolean], SignalRef[Boolean]) => Any < Async)(using
        Frame
    ): Boolean < Async =
        def held(t: UI): Boolean < Sync =
            elementWithClass(t, "p-button").map {
                case b: UI.Ast.Button => b.disabled.contains(true) && b.attrs.ariaAttrs.get("busy").contains("true")
                case _                => false
            }
        for
            disabled <- Signal.initRef(false)
            busy     <- Signal.initRef(false)
            cmds     <- headlessCommands
            ends <- kyo.internal.ReactiveUI.subscribed(button.disabled(disabled).loading(busy).render, log, commands = Present(cmds)) { _ =>
                waitFor(busy.waiters.map(_ > 0)).andThen(drive(disabled, busy)).andThen(
                    waitFor(
                        for
                            parked <- disabled.waiters.map(_ > 0)
                            also   <- busy.waiters.map(_ > 0)
                            drawn  <- Sync.defer(UicTest.publishedIn(log).headOption)
                            both   <- drawn.fold(Kyo.lift(false))(held)
                        yield parked && also && both
                    ).andThen(true)
                )
            }
        yield ends
        end for
    end endsOnBoth

    "one effect writing disabled and loading ends on both" in {
        val log = new kyo.internal.RecordingExchange
        endsOnBoth(log)((disabled, busy) => disabled.set(true).andThen(busy.set(true)))
            .map(ends => assert(ends, s"the last paint: ${UicTest.publishedIn(log).headOption}"))
    }

    "a write that lands while the older state is still painting ends on both" in {
        // The paint `busy` brought is paused at the transport, a slow one, while `disabled` is written: however that overlaps, the
        // page ends on both. The pause is a latch, and no wait here has a time limit.
        for
            painting <- Latch.init(1)
            resume   <- Latch.init(1)
            first    <- AtomicBoolean.init(true)
            log = new kyo.internal.RecordingExchange(beforePaint =
                (_, _) =>
                    first.getAndSet(false).map(isFirst => if isFirst then painting.release.andThen(resume.await) else Kyo.unit)
            )
            ends <- endsOnBoth(log)((disabled, busy) =>
                busy.set(true).andThen(painting.await).andThen(setSettled(disabled, true)).andThen(resume.release)
            )
        yield assert(ends, s"the last paint: ${UicTest.publishedIn(log).headOption}")
    }

end ButtonTest
