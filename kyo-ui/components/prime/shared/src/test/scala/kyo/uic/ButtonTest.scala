package kyo.uic

import kyo.*
import kyo.UI.Href
import kyo.UI.render
import kyo.internal.MouseEventData
import kyo.internal.ReactiveUI
import kyo.internal.UIEvent

/** A button is drawn once: its three switches patch it in place, whichever of them follow a signal, and only the spinner of a
  * loading button is a region, inside it.
  */
class ButtonTest extends UicTest:

    // Armed: a region that takes the reader's state again fails the leaf.
    override private[uic] def armed: Boolean = true

    private def button(using Frame) = uic.Button("Save")

    private def regions(b: uic.Button)(using Frame): Int < Sync = regionsAbove(b.render, "p-button")

    "no signal draws a region around the button: disabled, aria-disabled and loading patch it in place" in {
        for
            on  <- Signal.initRef(false)
            d   <- regions(button.disabled(on))
            ad  <- regions(button.ariaDisabled(on))
            l   <- regions(button.loading(on))
            all <- regions(button.disabled(on).ariaDisabled(on).loading(on))
        yield assert((d, ad, l, all) == (0, 0, 0, 0), s"disabled $d, ariaDisabled $ad, loading $l, all three $all")
    }

    "loading from a signal draws one region, inside the button: the spinner that stands where the icon does" in {
        for
            on      <- Signal.initRef(true)
            spinner <- regionsAbove(button.loading(on).render, "p-button-loading-icon")
            fixed   <- regionsAbove(button.loading(true).render, "p-button-loading-icon")
        yield assert((spinner, fixed) == (1, 0), s"regions above the spinner: from a signal $spinner, from a constant $fixed")
    }

    "a button that starts to load keeps its element: the type, aria-busy, the inert marker and the classes follow in place" in {
        for
            on <- Signal.initRef(false)
            el <- elementWithClass(uic.Button("Save").buttonType(ButtonType.Submit).loading(on).render, "p-button")
            a    = el.attrs
            kind = a.reactiveAttrs.get("type")
            idle <- kind.fold(Kyo.lift(""))(_.current)
            _    <- on.set(true)
            busy <- kind.fold(Kyo.lift(""))(_.current)
            says <- a.reactiveMaybeAttrs.get("aria-busy").fold(Kyo.lift(Absent: Maybe[String]))(_.current)
        yield
            assert((idle, busy) == ("submit", "button"), s"type while idle $idle, while loading $busy")
            assert(says.contains("true"), s"aria-busy while loading: $says")
            assert(
                a.reactiveMaybeAttrs.contains("data-kyo-inert") && a.reactiveAttrs.contains("aria-disabled") &&
                    a.reactiveClasses.contains("p-uic-aria-disabled") && a.jsProps.get("type").isEmpty,
                s"$a"
            )
        end for
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

    "an icon from a signal is its own region below the button, and the button draws no region for it" in {
        for
            glyph <- Signal.initRef[IconGlyph](Icons.check)
            ui = button.icon(glyph).endIcon(glyph).render
            above  <- regionsAbove(ui, "p-button")
            onIcon <- regionsAbove(ui, "p-button-icon-left")
        yield assert(above == 0 && onIcon == 1, s"above the button $above, above the icon $onIcon")
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

    /** Runs a button with `disabled` and `loading` over refs, hands `drive` the refs once the page reads them, and says whether the
      * page ended on both set: each follows its own channel on the button drawn once, so neither write waits on the other, and the
      * spinner's region, the one thing `loading` draws, has painted.
      */
    private def endsOnBoth(log: kyo.internal.RecordingExchange)(drive: (SignalRef[Boolean], SignalRef[Boolean]) => Any < Async)(using
        Frame
    ): Boolean < Async =
        // Disabled and busy as the page shows them: each attribute's channel holding true.
        def held(t: UI): Boolean < Sync =
            elementWithClass(t, "p-button").map {
                case b: UI.Ast.Button =>
                    for
                        off  <- b.attrs.reactiveBoolAttrs.get("disabled").fold(Kyo.lift(b.disabled.contains(true)))(_.current)
                        busy <- b.attrs.reactiveMaybeAttrs.get("aria-busy").fold(Kyo.lift(false))(_.current.map(_.contains("true")))
                    yield off && busy
                case _ => Kyo.lift(false)
            }
        def spins(t: UI): Boolean < Sync = elementsWithClass(t, "p-button-loading-icon").map(_.nonEmpty)
        for
            disabled <- Signal.initRef(false)
            busy     <- Signal.initRef(false)
            cmds     <- headlessCommands
            ui = button.disabled(disabled).loading(busy).render
            ends <- kyo.internal.ReactiveUI.subscribed(ui, log, commands = Present(cmds)) { _ =>
                waitFor(busy.waiters.map(_ > 0)).andThen(drive(disabled, busy)).andThen(
                    waitFor(
                        for
                            parked <- busy.waiters.map(_ > 0)
                            drawn  <- Sync.defer(UicTest.publishedIn(log).headOption)
                            spun   <- drawn.fold(Kyo.lift(false))(spins)
                            both   <- held(ui)
                        yield parked && spun && both
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

    // ---- loading is aria-disabled, not native disabled ----

    "a loading button is aria-disabled and greyed, not natively disabled, and stays in the tab order" in {
        for
            el     <- elementWithClass(uic.Button("Save").loading(true).render, "p-button")
            ad     <- ariaNow(el, "disabled")
            greyed <- carriesClass(el, "p-uic-aria-disabled")
        yield el match
            case b: UI.Ast.Button =>
                assert(
                    !b.disabled.contains(true) && ad.contains("true") && greyed && b.attrs.tabIndex.isEmpty,
                    s"disabled ${b.disabled}, aria-disabled $ad, greyed $greyed, tabindex ${b.attrs.tabIndex}"
                )
            case other => fail(s"not a button: $other")
        end for
    }

    "a loading button's handler runs nothing while the button loads" in {
        for
            heard <- AtomicInt.init(0)
            el    <- elementWithClass(uic.Button("Save").loading(true).onClick(heard.incrementAndGet.unit).render, "p-button")
            _     <- el match
                case b: UI.Ast.Button if b.attrs.onClick.nonEmpty => b.attrs.onClick.getOrElse(Kyo.unit)
                case other                                        => fail(s"not a button with a handler: $other")
            n <- heard.get
        yield assert(n == 0, s"the handler ran $n times while loading")
        end for
    }

    "a loading link keeps its href, is aria-disabled and greyed, and stays in the tab order" in {
        for
            el     <- elementWithClass(uic.Button("Go").href("/go").loading(true).render, "p-button")
            ad     <- ariaNow(el, "disabled")
            greyed <- carriesClass(el, "p-uic-aria-disabled")
        yield el match
            case a: UI.Ast.Anchor =>
                assert(
                    a.href.contains(Href.Path("/go")) && ad.contains("true") && greyed && a.attrs.tabIndex.contains(0),
                    s"href ${a.href}, aria-disabled $ad, greyed $greyed, tabindex ${a.attrs.tabIndex}"
                )
            case other => fail(s"not a link: $other")
        end for
    }

    "a loading link's handler runs nothing while it loads" in {
        for
            heard <- AtomicInt.init(0)
            el    <- elementWithClass(
                uic.Button("Go").href("/go").loading(true).onClick(heard.incrementAndGet.unit).render,
                "p-button"
            )
            _ <- el match
                case a: UI.Ast.Anchor if a.attrs.onClick.nonEmpty => a.attrs.onClick.getOrElse(Kyo.unit)
                case other                                        => fail(s"not a link with a handler: $other")
            n <- heard.get
        yield assert(n == 0, s"the handler ran $n times while the link loads")
        end for
    }

    "a loading button declines its browser default, and a loading or aria-disabled link is marked for the client to decline its click" in {
        def declined(e: UI.Ast.Element): Boolean = e.attrs.dataAttrs.get("kyo-inert").contains("1")
        def marked(e: UI.Ast.Element): Boolean   = e.attrs.blocked.contains(true)
        for
            submit  <- elementWithClass(uic.Button("Save").buttonType(ButtonType.Submit).loading(true).render, "p-button")
            loading <- elementWithClass(uic.Button("Go").href("/go").loading(true).render, "p-button")
            inert   <- elementWithClass(uic.Button("Go").href("/go").ariaDisabled(true).render, "p-button")
            idle    <- elementWithClass(uic.Button("Go").href("/go").render, "p-button")
        yield assert(
            declined(submit) && marked(loading) && marked(inert) && !marked(idle) && !declined(loading) && !declined(idle),
            s"loading button declined ${declined(submit)}, loading link marked ${marked(loading)} declined ${declined(loading)}, " +
                s"inert link marked ${marked(inert)}, idle link marked ${marked(idle)}"
        )
        end for
    }

    /** Clicks a submit button inside a form and says how many times the form's `onSubmit` ran. The idle button is the positive
      * control: the same path must reach the form when nothing holds the button back.
      */
    private def submitsOnClick(loading: Boolean)(using Frame): Int < (Async & Scope) =
        for
            submits <- AtomicInt.init(0)
            ui =
                UI.form.onSubmit(submits.getAndUpdate(_ + 1).unit)(uic.Button("Save").buttonType(ButtonType.Submit).loading(loading).render)
            _ <- ReactiveUI.subscribed(ui) { subscription =>
                subscription.handle(Seq("0"), UIEvent.Click(Seq("0"), MouseEventData(UI.Modifiers.none, Absent)))
            }
            n <- submits.get
        yield n

    "a submit button in a form submits on click when idle, and submits nothing while it loads" in {
        for
            idle    <- submitsOnClick(false)
            loading <- submitsOnClick(true)
        yield assert(idle == 1 && loading == 0, s"idle submitted $idle time(s), loading $loading")
    }

    "an aria-disabled link runs nothing on click, as the link it stands in for does" in {
        for
            heard <- AtomicInt.init(0)
            el    <- elementWithClass(
                uic.Button("Go").href("/go").ariaDisabled(true).onClick(heard.incrementAndGet.unit).render,
                "p-button"
            )
            _ <- el match
                case a: UI.Ast.Anchor if a.attrs.onClick.nonEmpty => a.attrs.onClick.getOrElse(Kyo.unit)
                case other                                        => fail(s"not a link with a handler: $other")
            n <- heard.get
        yield assert(n == 0, s"the handler ran $n times on an inert link")
    }

    "a normal click runs a button's action and a link's once each, and a disabled link's click runs nothing" in {
        for
            heard <- AtomicInt.init(0)
            saved <- elementWithClass(uic.Button("Save").onClick(heard.incrementAndGet.unit).render, "p-button")
            went  <- elementWithClass(uic.Button("Go").href("/go").onClick(heard.incrementAndGet.unit).render, "p-button")
            held  <- elementWithClass(uic.Button("Go").href("/go").disabled(true).onClick(heard.incrementAndGet.unit).render, "p-button")
            _     <- saved match
                case b: UI.Ast.Button if b.attrs.onClick.nonEmpty => b.attrs.onClick.getOrElse(Kyo.unit)
                case other                                        => fail(s"not a button with a handler: $other")
            afterButton <- heard.get
            _           <- went match
                case a: UI.Ast.Anchor if a.attrs.onClick.nonEmpty => a.attrs.onClick.getOrElse(Kyo.unit)
                case other                                        => fail(s"not a link with a handler: $other")
            afterLink <- heard.get
            _         <- held match
                case a: UI.Ast.Anchor if a.attrs.onClick.nonEmpty => a.attrs.onClick.getOrElse(Kyo.unit)
                case other                                        => fail(s"not a link with a handler: $other")
            afterHeld <- heard.get
        yield assert(
            (afterButton, afterLink, afterHeld) == (1, 2, 2),
            s"handler counts after button $afterButton, link $afterLink, disabled link $afterHeld"
        )
        end for
    }

    // ---- an aria-disabled link is disabled as a link ----

    "an aria-disabled link keeps its href, and is aria-disabled, out of the tab order and styled as disabled" in {
        for
            el  <- elementWithClass(uic.Button("Go").href("/go").ariaDisabled(true).render, "p-button")
            ad  <- ariaNow(el, "disabled")
            dis <- carriesClass(el, "p-disabled")
        yield el match
            case a: UI.Ast.Anchor =>
                assert(
                    a.href.contains(Href.Path("/go")) && ad.contains("true") && dis && a.attrs.tabIndex.contains(-1),
                    s"href ${a.href}, aria-disabled $ad, disabled class $dis, tabindex ${a.attrs.tabIndex}"
                )
            case other => fail(s"not a link: $other")
        end for
    }

    "a disabled link drops its href, and is aria-disabled, out of the tab order and styled as disabled" in {
        for
            el  <- elementWithClass(uic.Button("Go").href("/go").disabled(true).render, "p-button")
            ad  <- ariaNow(el, "disabled")
            dis <- carriesClass(el, "p-disabled")
        yield el match
            case a: UI.Ast.Anchor =>
                assert(
                    a.href.isEmpty && ad.contains("true") && dis && a.attrs.tabIndex.contains(-1),
                    s"href ${a.href}, aria-disabled $ad, disabled class $dis, tabindex ${a.attrs.tabIndex}"
                )
            case other => fail(s"not a link: $other")
        end for
    }

    // ---- icon-only: any icon, and an empty label is no label ----

    "an end icon alone makes the button icon-only" in {
        elementWithClass(uic.Button().endIcon(Icons.check).render, "p-button").flatMap(carriesClass(_, "p-button-icon-only"))
            .map(icon => assert(icon, "an end icon alone did not make the button icon-only"))
    }

    "an empty label with an icon is icon-only and renders no empty label span" in {
        val ui = uic.Button("").icon(Icons.check).render
        for
            el    <- elementWithClass(ui, "p-button")
            icon  <- carriesClass(el, "p-button-icon-only")
            spans <- elementsWithClass(ui, "p-button-label")
        yield assert(icon && spans.isEmpty, s"icon-only $icon, label spans ${spans.size}")
        end for
    }

    "a label signal that becomes empty gains the icon-only class in place" in {
        for
            label  <- Signal.initRef("Save")
            el     <- elementWithClass(uic.Button(label).icon(Icons.check).render, "p-button")
            before <- carriesClass(el, "p-button-icon-only")
            _      <- label.set("")
            after  <- carriesClass(el, "p-button-icon-only")
        yield assert((before, after) == (false, true), s"icon-only before $before, after $after")
        end for
    }

    // ---- form: the native form attribute ----

    "form names the form the button submits, as the native form attribute" in {
        renderHtml(UI.div(uic.Button("Save").form("profile").render)).map { html =>
            assert(html.contains(""" form="profile""""), html)
            assert(!html.contains("data-uic-form"), html)
        }
    }

end ButtonTest
