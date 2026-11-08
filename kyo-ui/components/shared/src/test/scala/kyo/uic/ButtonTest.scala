package kyo.uic

import kyo.*
import kyo.UI.render

/** A button reads its three switches as one region, whichever of them follow a signal, and with none it draws no region at all. */
class ButtonTest extends UicTest:

    // Freed of every loss in its wave (T19): a region that takes the reader's state again fails the leaf.
    override private[uic] def armed: Boolean = true

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

    "a signal in disabled or aria-disabled patches the button in place; one in loading, which shapes it, is its one region" in {
        for
            on <- Signal.initRef(false)
            d  <- regions(button.disabled(on))
            ad <- regions(button.ariaDisabled(on))
            l  <- regions(button.loading(on))
        yield assert((d, ad, l) == (0, 0, 1), s"disabled $d, ariaDisabled $ad, loading $l")
    }

    "a variant, size or shape from a signal toggles its classes in place" in {
        for
            variant <- Signal.initRef[ButtonVariant](ButtonVariant.Filled)
            size    <- Signal.initRef[Size](Size.Normal)
            round   <- Signal.initRef(false)
            // No region above the button (asserted below), so nothing draws it again: the element built once carries channels,
            // and each reads its signal's value now.
            b       <- elementWithClass(button.variant(variant).size(size).rounded(round).raised(round).fluid(round).render, "p-button")
            _       <- variant.set(ButtonVariant.Outlined).andThen(size.set(Size.Small)).andThen(round.set(true))
            classes <- Kyo.foreach(Seq(
                "p-button-outlined",
                "p-button-text",
                "p-button-sm",
                "p-button-lg",
                "p-button-rounded",
                "p-button-raised",
                "p-button-fluid"
            ))(carriesClass(b, _))
            n <- regions(button.variant(variant).size(size).rounded(round).raised(round).fluid(round))
        yield assert(
            classes == Seq(true, false, true, false, true, true, true) && n == 0,
            s"classes $classes, $n regions"
        )
        end for
    }

    "a click while a disabled signal says on reaches nothing, and once it says off it reaches the handler" in {
        for
            off   <- Signal.initRef(true)
            heard <- AtomicInt.init(0)
            ui = button.disabled(off).onClick(heard.incrementAndGet.unit).render
            _      <- elementWithClass(ui, "p-button").map(click)
            before <- heard.get
            _      <- off.set(false)
            _      <- elementWithClass(ui, "p-button").map(click)
            after  <- heard.get
        yield assert((before, after) == (0, 1), s"heard while disabled $before, after $after")
    }

    "aria-disabled from a signal says so in place, and not while the button is natively disabled" in {
        for
            ad  <- Signal.initRef(false)
            off <- Signal.initRef(false)
            el  <- elementWithClass(button.ariaDisabled(ad).disabled(off).render, "p-button")
            a0  <- ariaNow(el, "disabled")
            _   <- ad.set(true)
            a1  <- ariaNow(el, "disabled")
            c1  <- carriesClass(el, "p-uic-aria-disabled")
            _   <- off.set(true)
            a2  <- ariaNow(el, "disabled")
            c2  <- carriesClass(el, "p-uic-aria-disabled")
            n   <- regions(button.ariaDisabled(ad).disabled(off))
        yield assert(
            !a0.contains("true") && a1.contains("true") && c1 && !a2.contains("true") && !c2 && n == 0,
            s"aria-disabled off $a0, on $a1 (class $c1), natively disabled $a2 (class $c2), $n regions"
        )
        end for
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
      * ended on both set: the region's observer of `loading` parked again, so what it read is what the region left, and the last
      * paint shows both. Only `loading` is the region's read: `disabled` is a channel, and a loading button draws itself disabled
      * and reads it no more, so nothing waits on it then.
      */
    private def endsOnBoth(log: kyo.internal.RecordingExchange)(drive: (SignalRef[Boolean], SignalRef[Boolean]) => Any < Async)(using
        Frame
    ): Boolean < Async =
        // Disabled as the page shows it: the attribute, or its channel holding true (`disabled` follows a signal through it).
        def held(t: UI): Boolean < Sync =
            elementWithClass(t, "p-button").map {
                case b: UI.Ast.Button =>
                    val off = b.disabled.contains(true) match
                        case true  => Kyo.lift(true)
                        case false => b.attrs.reactiveBoolAttrs.get("disabled").fold(Kyo.lift(false))(_.current)
                    off.map(_ && b.attrs.ariaAttrs.get("busy").contains("true"))
                case _ => Kyo.lift(false)
            }
        for
            disabled <- Signal.initRef(false)
            busy     <- Signal.initRef(false)
            cmds     <- headlessCommands
            ends <- kyo.internal.ReactiveUI.subscribed(button.disabled(disabled).loading(busy).render, log, commands = Present(cmds)) { _ =>
                waitFor(busy.waiters.map(_ > 0)).andThen(drive(disabled, busy)).andThen(
                    waitFor(
                        for
                            parked <- busy.waiters.map(_ > 0)
                            drawn  <- Sync.defer(UicTest.publishedIn(log).headOption)
                            both   <- drawn.fold(Kyo.lift(false))(held)
                        yield parked && both
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

    "a host region that runs again draws the run's props, with no instance of its own" in {
        def labelled(n: Int)(t: UI): Boolean < Sync = elementsWithClass(t, "p-button-label").map(_.exists(textOf(_) == s"Save $n"))
        for
            tick <- Signal.initRef(0)
            ui = UI.div(tick.render(n => UI.div(uic.Button(s"Save $n").disabled(n > 0))))
            after <- publishedAfterReady(ui, _ => true, until = labelled(1))(_ => setSettled(tick, 1))
            shown = after.getOrElse(ui)
            el    <- elementWithClass(shown, "p-button")
            mount <- holdsMount(uic.Button("Save").render)
        yield
            val off = el match
                case b: UI.Ast.Button => b.disabled.contains(true)
                case _                => false
            assert(off && !mount, s"disabled $off, mount $mount")
        end for
    }

end ButtonTest
