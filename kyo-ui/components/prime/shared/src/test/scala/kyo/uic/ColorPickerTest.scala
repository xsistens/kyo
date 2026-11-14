package kyo.uic

import kyo.*
import kyo.UI.*

/** The keyboard of `uic.ColorPicker`, which had none: the plane and the hue bar were pointer
  * surfaces with no role, no tab stop and no key, so a colour could only be picked with a mouse.
  *
  * One visible handle is one control, so the plane is ONE slider answering all four arrows. It was
  * two, one per axis, which announced each number exactly and left the handle answering half the
  * arrows on each of two tab stops. `aria-valuenow` carries saturation because a slider needs one
  * number, and `aria-valuetext` carries both. Both sliders are visually hidden because the visible
  * thing is the handle, which is also why the ring is drawn on the surface through `:focus-within`.
  */
class ColorPickerTest extends ReactiveValueContract:

    /** Pure red: hue 0, saturation 100%, brightness 100%. */
    private val red = "#ff0000"

    private def picker(hex: String)(using Frame): (SignalRef[String], UI) < Async =
        Signal.initRef(hex).map(r => (r, uic.ColorPicker().inline(true).value(r).render))

    /** An OPEN overlay picker, with the panel refs its mount would have minted. */
    private def overlay(hex: String)(using Frame): (SignalRef[String], uic.ColorPicker.Panel, UI) < Async =
        for
            ref    <- Signal.initRef(hex)
            open   <- Signal.initRef(true)
            opened <- Signal.initRef(hex)
            panel = uic.ColorPicker.Panel(open, opened)
        yield (ref, panel, uic.ColorPicker().value(ref).wired(panel, Present(ref), hex))

    private def sliders(ui: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elements(ui).map(_.filter(_.attrs.role.contains("slider")))

    private val plane = "Saturation and brightness"
    private val hue   = "Hue"

    /** Presses `key` on the slider labelled `label` and reports the resulting hex. */
    private def after(label: String, key: UI.Keyboard, from: String = red)(using Frame): String < Async =
        for
            (ref, ui) <- picker(from)
            all       <- sliders(ui)
            el = all.find(_.attrs.ariaAttrs.get("label").contains(label)).get
            _   <- press(el, key)
            got <- ref.get
        yield got

    "the plane is one slider and the hue bar is another, two tab stops for two handles" in {
        for
            (_, ui) <- picker(red)
            all     <- sliders(ui)
        yield
            assert(all.map(_.attrs.ariaAttrs("label")).toList == List("Saturation and brightness", "Hue"))
            assert(all.forall(_.attrs.tabIndex.contains(0)), "each handle takes the Tab")
            assert(all.forall(_.attrs.cssClasses.contains("p-hidden-accessible")), "and neither of them is seen")
    }

    "the plane announces one number as its value and both of them in words" in {
        for
            (_, ui) <- picker("#804040")
            all     <- sliders(ui)
            plane = all.head
        yield
            assert(plane.attrs.ariaAttrs.get("valuemax").contains("100"))
            assert(plane.attrs.reactiveAttrs.contains("aria-valuenow"), "the value rides the signal")
            assert(plane.attrs.reactiveAttrs.contains("aria-valuetext"))
    }

    "each slider announces its own range" in {
        for
            (_, ui) <- picker(red)
            all     <- sliders(ui)
        yield
            val hue = all.find(_.attrs.ariaAttrs.get("label").contains("Hue")).get
            assert(hue.attrs.ariaAttrs.get("valuemin").contains("0"))
            assert(hue.attrs.ariaAttrs.get("valuemax").contains("359"), "degrees, not percent")
            val plane = all.head
            assert(plane.attrs.ariaAttrs.get("valuemax").contains("100"), "percent, and saturation is the number")
    }

    "the horizontal arrows move saturation and leave the rest of the colour alone" in {
        for
            down <- after(plane, UI.Keyboard.ArrowLeft)
            up   <- after(plane, UI.Keyboard.ArrowRight, from = "#ff8080")
        yield
            assert(uic.ColorPicker.hsvOf(down)._2 < 1.0 && uic.ColorPicker.hsvOf(down)._1 == 0.0)
            assert(uic.ColorPicker.hsvOf(up)._2 > uic.ColorPicker.hsvOf("#ff8080")._2)
    }

    "and the vertical arrows move brightness, on the SAME handle the reader is standing on" in {
        for
            darker  <- after(plane, UI.Keyboard.ArrowDown)
            lighter <- after(plane, UI.Keyboard.ArrowUp, from = "#800000")
        yield
            assert(uic.ColorPicker.hsvOf(darker)._3 < 1.0, "one tab stop answers both axes")
            assert(uic.ColorPicker.hsvOf(lighter)._3 > uic.ColorPicker.hsvOf("#800000")._3)
            assert(uic.ColorPicker.hsvOf(darker)._2 == 1.0, "and leaves the other one where it was")
    }

    "the hue bar takes both axes, which is what a vertical slider asks" in {
        for
            up   <- after(hue, UI.Keyboard.ArrowUp)
            side <- after(hue, UI.Keyboard.ArrowRight)
        yield assert(uic.ColorPicker.hsvOf(up)._1 > 0.0 && uic.ColorPicker.hsvOf(side)._1 > 0.0)
    }

    "the page keys move ten at a time, on the plane's vertical axis" in {
        for
            one <- after(plane, UI.Keyboard.ArrowDown)
            ten <- after(plane, UI.Keyboard.PageDown)
        yield
            val oneV = uic.ColorPicker.hsvOf(one)._3
            val tenV = uic.ColorPicker.hsvOf(ten)._3
            assert(tenV < oneV && math.abs((1.0 - tenV) - 10 * (1.0 - oneV)) < 0.02)
    }

    "Home and End reach the ends of the plane's horizontal axis" in {
        for
            none <- after(plane, UI.Keyboard.Home)
            full <- after(plane, UI.Keyboard.End, from = "#804040")
        yield
            assert(uic.ColorPicker.hsvOf(none)._2 == 0.0, "no saturation left")
            assert(uic.ColorPicker.hsvOf(full)._2 == 1.0)
    }

    "an inline picker seeds no focus, since nothing opened for the reader to be taken into" in {
        for
            (_, ui) <- picker(red)
            all     <- sliders(ui)
        yield assert(all.forall(_.attrs.focusAuto.isEmpty))
    }

    "the overlay's plane slider is what focus is seeded onto, and what hands it back" in {
        for
            (_, _, ui) <- overlay(red)
            all        <- sliders(ui)
        yield
            assert(all.head.attrs.focusAuto.contains(true), "opening takes the reader to the plane")
            assert(all.head.attrs.focusRestore.contains(true), "and closing gives focus back to the swatch")
            assert(all(1).attrs.focusAuto.isEmpty, "only one, or the panel fights itself")
    }

    "the panel takes no focus of its own, so Tab out of it has somewhere to go" in {
        for
            (_, _, ui) <- overlay(red)
            floating   <- elementWithClass(ui, "p-uic-overlay-panel")
        yield
            assert(floating.attrs.focusAuto.isEmpty)
            assert(floating.attrs.focusTrap.isEmpty, "a colour picker is not a modal")
    }

    "Enter confirms the colour the reader arrived at, and closes" in {
        for
            (ref, panel, ui) <- overlay(red)
            all              <- sliders(ui)
            _                <- press(all.head, UI.Keyboard.ArrowLeft)
            _                <- press(all.head, UI.Keyboard.Enter)
            got              <- ref.get
            still            <- panel.open.get
        yield assert(!still && got != red, "the arrow's colour stays")
    }

    "Space says the same thing as Enter" in {
        for
            (_, panel, ui) <- overlay(red)
            all            <- sliders(ui)
            _              <- press(all.head, UI.Keyboard.Space)
            still          <- panel.open.get
        yield assert(!still)
    }

    "Escape closes and puts back the colour the panel opened with" in {
        for
            (ref, panel, ui) <- overlay(red)
            all              <- sliders(ui)
            _                <- press(all.head, UI.Keyboard.ArrowLeft)
            moved            <- ref.get
            _                <- press(all.head, UI.Keyboard.Escape)
            got              <- ref.get
            still            <- panel.open.get
        yield assert(!still && moved != red && got == red, "cancel is what makes confirm mean something")
    }

    "the swatch toggles, where it used to only ever open, and remembers what it opened with" in {
        for
            (ref, panel, ui) <- overlay(red)
            swatch           <- elementWithClass(ui, "p-colorpicker-preview")
            _                <- click(swatch)
            closed           <- panel.open.get
            _                <- ref.set("#00ff00")
            _                <- click(swatch)
            opened           <- panel.open.get
            remembered       <- panel.opened.get
        yield assert(!closed && opened && remembered == "#00ff00")
    }

    /** The colour the inline picker shows, read off what its two sliders announce: the hue bar's degrees, and the
      * plane's saturation and brightness in words. Both ride the value signal as attributes, so the reading is what a
      * screen reader hears now.
      */
    private def shown(t: UI)(using Frame): String < Sync =
        def announced(el: UI.Ast.Element, name: String): String < Sync =
            el.attrs.reactiveAttrs.get(name).fold(throw new AssertionError(s"the slider announces no $name"))(_.current)
        for
            all <- sliders(t)
            planeEl = all.find(_.attrs.ariaAttrs.get("label").contains(plane))
                .getOrElse(throw new AssertionError("no plane slider: the picker offers the reader nothing to change"))
            hueEl = all.find(_.attrs.ariaAttrs.get("label").contains(hue)).getOrElse(throw new AssertionError("no hue slider"))
            degrees <- announced(hueEl, "aria-valuenow")
            words   <- announced(planeEl, "aria-valuetext")
        yield words match
            case s"Saturation $s%, brightness $v%" => uic.ColorPicker.hexOf(degrees.toDouble, s.toDouble / 100, v.toDouble / 100)
            case other                             => throw new AssertionError(s"the plane says $other")
        end for
    end shown

    /** The colour a disabled picker draws, which announces nothing: read off where its two handles sit. */
    private def drawn(t: UI)(using Frame): String < Sync =
        def at(cls: String)(pick: PartialFunction[Style.Prop, Length]): Double < Sync =
            elementWithClass(t, cls).map(_.attrs.uiStyle.props.collectFirst(pick) match
                case Some(Length.Pct(v)) => v
                case other               => throw new AssertionError(s"$cls sits at $other"))
        for
            s <- at("p-colorpicker-color-handle") { case Style.Prop.Left(v) => v }
            v <- at("p-colorpicker-color-handle") { case Style.Prop.Top(v) => v }
            h <- at("p-colorpicker-hue-handle") { case Style.Prop.Top(v) => v }
        yield uic.ColorPicker.hexOf((1 - h / 100) * 360.0, s / 100, 1 - v / 100)
        end for
    end drawn

    private def planeOf(t: UI)(using Frame): UI.Ast.Element < Sync =
        sliders(t).map(_.find(_.attrs.ariaAttrs.get("label").contains(plane))
            .getOrElse(throw new AssertionError("no plane slider: the picker offers the reader nothing to change")))

    // The colour is held to rule A: unbound the picker keeps it itself (it draws it entirely), a constant is a binding (a
    // constant nobody hears is fixed: read-only, nothing to drag), and Home on the plane (no saturation left) is the gesture.
    // Where the colour starts from a default is the case after it.
    holds(new SlotCase[uic.ColorPicker, String](
        name = "value",
        bare = Present(uic.ColorPicker().inline(true)),
        bind = uic.ColorPicker().inline(true).value(_),
        hear = (c, f) => c.onChange(f),
        event = "onChange",
        render = _.render,
        start = red,
        next = "#ffffff",
        control = planeOf(_),
        shows = t => sliders(t).map(s => if s.isEmpty then drawn(t) else shown(t)),
        gesture = (c: UI.Ast.Element) => press(c, UI.Keyboard.Home),
        answers = _.attrs.onKeyDown.isDefined,
        ready = Present(t => Abort.run[AssertionError](Abort.catching[AssertionError](planeOf(t))).map(_.isSuccess)),
        // The sliders stay where they are while the colour moves (a rebuilt one would lose the focus that pressed it), so
        // what says the reader's colour is shown is the plane announcing it in place.
        nextPatch = Present((name, value) => name == "aria-valuetext" && value == "Saturation 0%, brightness 100%"),
        // Fixed, the picker draws no slider at all, so it says so on its root.
        isFixed = Present(t =>
            sliders(t).map(s =>
                if s.nonEmpty then Kyo.lift(false)
                else elementWithClass(t, "p-colorpicker").map(_.attrs.ariaAttrs.get("readonly").contains("true"))
            )
        ),
        disable = Present((c, on) => c.disabled(on))
    ))

    "a default colour is where the picker starts, and a gesture moves the colour the picker keeps" in {
        def started(using Frame)      = uic.ColorPicker().inline(true).defaultValue("#00ff00")
        def ready(t: UI)(using Frame) = Abort.run[AssertionError](Abort.catching[AssertionError](planeOf(t))).map(_.isSuccess)
        for
            first <- readyTree(started.render)(ready(_))
            from  <- shown(first)
            moved <- attrPatchedAfter(started.render, ready(_))(t => planeOf(t).map(press(_, UI.Keyboard.Home)))((name, value) =>
                name == "aria-valuetext" && value == "Saturation 0%, brightness 100%"
            )
        yield assert(from == "#00ff00" && moved, s"started at $from, moved $moved")
        end for
    }

    "a color bound one way follows its owner" in {
        for
            held <- Signal.initRef(red)
            ui = uic.ColorPicker().inline(true).value(held.readOnly).onChange(_ => ()).render
            _     <- held.set("#00ff00")
            again <- shown(ui)
        yield assert(again == "#00ff00", s"shows $again")
    }

    "a color bound one way says nothing where it reaches somebody or nobody can pick: a ref, disabled" in {
        def errors(c: uic.ColorPicker): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-diagnostic")
        for
            held   <- Signal.initRef(red)
            twoWay <- errors(uic.ColorPicker().inline(true).value(held))
            off    <- errors(uic.ColorPicker().inline(true).value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && off.isEmpty, "somebody hears, or nobody can pick")
        end for
    }

end ColorPickerTest
