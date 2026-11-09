package kyo.uic

import kyo.*
import kyo.UI.render

/** The `checked` slot holds to rule A ([[ReactiveValueContract]]). The button draws its state itself, so it is a click on
  * the button that asks for the other state, and `aria-disabled` (a `role="button"` has no readonly state) that says a
  * fixed one changes nothing.
  */
class ToggleButtonTest extends ReactiveValueContract:

    // Freed of every loss in its wave (T19): a region that takes the reader's state again fails the leaf.
    override private[uic] def armed: Boolean = true

    /** What the button shows: `aria-pressed`, which the checked class must agree with. */
    private def pressed(t: UI)(using Frame): Boolean < Sync =
        for
            el      <- elementWithClass(t, "p-togglebutton")
            checked <- carriesClass(el, "p-togglebutton-checked")
            aria    <- ariaNow(el, "pressed")
        yield
            val says = aria.contains("true")
            if says != checked then throw new AssertionError(s"aria-pressed says $says, the checked class $checked")
            says

    holds(new SlotCase[uic.ToggleButton, Boolean](
        name = "checked",
        bare = Present(uic.ToggleButton()),
        bind = uic.ToggleButton().checked(_),
        hear = (b, f) => b.onChange(f),
        event = "onChange",
        render = _.render,
        start = false,
        next = true,
        control = elementWithClass(_, "p-togglebutton"),
        shows = pressed(_),
        // The button is built once and its state rides the class and `aria-pressed` channels, so a change is a patch.
        nextPatch = Present(_ == ("aria-pressed", "true")),
        saysFixed = {
            case b: UI.Ast.Button => b.attrs.ariaAttrs.get("disabled").contains("true") && !b.disabled.contains(true)
            case _                => false
        },
        // As an app places it, which keys the instance on the place.
        placed = Present(b => b.toUI)
    ))

    "a signal nobody hears says nothing where it reaches somebody or nobody can toggle it: a ref, readonly, disabled" in {
        def errors(c: uic.ToggleButton): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-diagnostic")
        for
            held   <- Signal.initRef(false)
            twoWay <- errors(uic.ToggleButton().checked(held))
            ro     <- errors(uic.ToggleButton().checked(held.readOnly).readonly(true))
            off    <- errors(uic.ToggleButton().checked(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can toggle")
        end for
    }

    "a host region that runs again keeps what the reader pressed and shows the run's props" in {
        def labelled(t: UI): Boolean < Sync =
            elementsWithClass(t, "p-togglebutton-label").map(_.headOption.exists(l => textOf(l) == "on 1"))
        for
            tick <- Signal.initRef(0)
            ui = UI.div(tick.render(n => UI.div(uic.ToggleButton().onLabel(s"on $n"))))
            (every, since, prior) <- paintedAround(ui, t => elementsWithClass(t, "p-togglebutton").map(_.nonEmpty), labelled)(
                t => elementWithClass(t, "p-togglebutton").map(click),
                _ => setSettled(tick, 1)
            )
            held    <- pressed(every)
            redrawn <- drawnAgain(since, prior, "p-togglebutton")
        yield assert(
            held && redrawn.isEmpty,
            s"after the host's run: pressed $held, the button drawn again at ${redrawn.mkString(", ")} (a new label patches the label alone)"
        )
        end for
    }

    "a run that stops binding checked carries on from what the button showed" in {
        def ranOnce(t: UI): Boolean < Sync =
            elementsWithClass(t, "p-togglebutton-label").map(_.headOption.exists(l => textOf(l).endsWith(" 1")))
        for
            tick <- Signal.initRef(0)
            held <- Signal.initRef(true)
            ui = UI.div(tick.render { n =>
                val b = uic.ToggleButton().onLabel(s"on $n").offLabel(s"off $n")
                // One placement: a conversion in each branch of an `if` would be two places, so two instances.
                val button: uic.ToggleButton = if n == 0 then b.checked(held) else b
                UI.div(button)
            })
            (every, _, _) <-
                paintedAround(ui, t => elementsWithClass(t, "p-togglebutton").map(_.nonEmpty), ranOnce)(_ => setSettled(tick, 1))
            shown  <- pressed(every)
            labels <- elementsWithClass(every, "p-togglebutton-label").map(_.map(textOf))
            ref    <- held.get
        yield assert(
            shown,
            s"the run that stopped binding dropped the button to where its own state started, not what it showed: labels $labels, app ref $ref"
        )
        end for
    }

    "a click after a host run writes the ref and reaches the onChange of that run" in {
        for
            tick  <- Signal.initRef(0)
            a     <- Signal.initRef(false)
            b     <- Signal.initRef(false)
            heard <- AtomicRef.init(Chunk.empty[String])
            ui = UI.div(tick.render(n =>
                UI.div(uic.ToggleButton().checked(if n == 0 then a else b).offLabel(s"off $n")
                    .onChange(v => heard.getAndUpdate(_.append(s"run $n: $v")).unit))
            ))
            // The click comes once the button shows the run: the host region settling is not yet the instance having its props.
            reached = (t: UI) =>
                elementsWithClass(t, "p-togglebutton-label").map(ls =>
                    if ls.exists(textOf(_) == "off 1") then elementWithClass(t, "p-togglebutton").map(click)
                    else throw new AssertionError("the button does not show the run yet")
                )
            _  <- actedOn(ui, _ => true)(_ => setSettled(tick, 1), reached)
            va <- a.get
            vb <- b.get
            hs <- heard.get
        yield assert((va, vb, hs) == (false, true, Chunk("run 1: true")), s"a $va, b $vb, heard $hs")
        end for
    }

    "a host run that brings only a new onChange draws nothing again, and the click reaches that run" in {
        for
            tick  <- Signal.initRef(0)
            heard <- AtomicRef.init(Chunk.empty[Int])
            ui = UI.div(tick.render(n => UI.div(uic.ToggleButton().onChange(_ => heard.getAndUpdate(_.append(n)).unit))))
            // The run reaches the instance when it sends its tree again into the host's new slot.
            (every, since, prior) <- paintedAround(
                ui,
                t => elementsWithClass(t, "p-togglebutton").map(_.nonEmpty),
                _ => true,
                sent = (since, _) => since.exists(_._1.size > 1)
            )(_ => setSettled(tick, 1))
            redrawn <- drawnAgain(since, prior, "p-togglebutton")
            _       <- elementWithClass(every, "p-togglebutton").map(click)
            hs      <- heard.get
        yield assert(redrawn.isEmpty && hs == Chunk(1), s"drawn again at ${redrawn.mkString(", ")}, heard $hs")
        end for
    }

    "a size or width from a signal toggles its classes in place" in {
        for
            size  <- Signal.initRef[Size](Size.Normal)
            fluid <- Signal.initRef(false)
            ui = uic.ToggleButton().size(size).fluid(fluid).render
            (every, since, prior) <- paintedAround(
                ui,
                t => elementsWithClass(t, "p-togglebutton").map(_.nonEmpty),
                t => elementWithClass(t, "p-togglebutton").map(b => carriesClass(b, "p-togglebutton-sm"))
            )(_ => setSettled(size, Size.Small).andThen(setSettled(fluid, true)))
            b       <- elementWithClass(every, "p-togglebutton")
            classes <-
                Kyo.foreach(Seq("p-togglebutton-sm", "p-inputfield-sm", "p-togglebutton-lg", "p-togglebutton-fluid"))(carriesClass(b, _))
            redrawn <- drawnAgain(since, prior, "p-togglebutton")
        yield assert(
            classes == Seq(true, true, false, true) && redrawn.isEmpty,
            s"classes $classes, drawn again at ${redrawn.mkString(", ")}"
        )
        end for
    }

    "readonly from a signal: on, the button says so and is silent; off again, it answers; nothing is drawn again" in {
        for
            ro <- Signal.initRef(false)
            ui = uic.ToggleButton().readonly(ro).render
            (every, since, prior) <- paintedAround(
                ui,
                t => elementsWithClass(t, "p-togglebutton").map(_.nonEmpty),
                t => elementWithClass(t, "p-togglebutton").map(silentNow)
            )(_ => setSettled(ro, true))
            b        <- elementWithClass(every, "p-togglebutton")
            onQuiet  <- silentNow(b)
            onAria   <- ariaNow(b, "disabled")
            _        <- setSettled(ro, false)
            offQuiet <- silentNow(b)
            offAria  <- ariaNow(b, "disabled")
            redrawn  <- drawnAgain(since, prior, "p-togglebutton")
        yield assert(
            onQuiet && onAria.contains("true") && !offQuiet && !offAria.contains("true") && redrawn.isEmpty,
            s"on: silent $onQuiet aria $onAria; off: silent $offQuiet aria $offAria; drawn again at ${redrawn.mkString(", ")}"
        )
        end for
    }

    "drawn with nothing that can change, the button sits under no region; its own state is one region, the content" in {
        for
            fixedRoot    <- regionsAbove(uic.ToggleButton().checked(true).render, "p-togglebutton")
            fixedLabel   <- regionsAbove(uic.ToggleButton().checked(true).render, "p-togglebutton-label")
            unboundRoot  <- regionsAbove(uic.ToggleButton().render, "p-togglebutton")
            unboundLabel <- regionsAbove(uic.ToggleButton().render, "p-togglebutton-label")
        yield assert(
            (fixedRoot, fixedLabel, unboundRoot, unboundLabel) == (0, 0, 0, 1),
            s"regions above (fixed root, fixed label, unbound root, unbound label): ${(fixedRoot, fixedLabel, unboundRoot, unboundLabel)}"
        )
        end for
    }

    "a fixed constant of true is drawn pressed" in {
        for p <- pressed(uic.ToggleButton().checked(true).render)
        yield assert(p, "the constant is not shown")
    }

end ToggleButtonTest
