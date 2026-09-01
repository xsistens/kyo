package kyo.uic

import kyo.*
import kyo.UI.*

/** The keyboard of `uic.ColorPicker`, which had none: the plane and the hue bar were pointer
  * surfaces with no role, no tab stop and no key, so a colour could only be picked with a mouse.
  *
  * The plane carries TWO values, and a `role="slider"` carries exactly one `aria-valuenow`, so it
  * is two sliders rather than one: a single role over both would announce a number that is only
  * half the truth. They are visually hidden because the visible thing is the handle, which is
  * also why the ring is drawn on the surface through `:focus-within`.
  */
class ColorPickerTest extends UicTest:

    /** Pure red: hue 0, saturation 100%, brightness 100%. */
    private val red = "#ff0000"

    private def picker(hex: String)(using Frame): (SignalRef[String], UI) < Async =
        Signal.initRef(hex).map(r => (r, uic.ColorPicker().inline(true).value(r).render))

    private def sliders(ui: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elements(ui).map(_.filter(_.attrs.role.contains("slider")))

    /** Presses `key` on the slider labelled `label` and reports the resulting hex. */
    private def after(label: String, key: UI.Keyboard, from: String = red)(using Frame): String < Async =
        for
            (ref, ui) <- picker(from)
            all       <- sliders(ui)
            el = all.find(_.attrs.ariaAttrs.get("label").contains(label)).get
            _   <- press(el, key)
            got <- ref.get
        yield got

    "the plane is two sliders and the hue bar is one, all three real tab stops" in {
        for
            (_, ui) <- picker(red)
            all     <- sliders(ui)
            plane   <- elementWithClass(ui, "p-colorpicker-color-selector")
        yield
            assert(all.map(_.attrs.ariaAttrs("label")).toList == List("Saturation", "Brightness", "Hue"))
            assert(all.forall(_.attrs.tabIndex.contains(0)), "each axis takes the Tab")
            assert(all.forall(_.attrs.cssClasses.contains("p-hidden-accessible")), "and none of them is seen")
            assert(plane.attrs.role.contains("group"), "the plane groups the two axes it controls")
            assert(plane.attrs.ariaAttrs.get("label").contains("Saturation and brightness"))
    }

    "each axis announces its own range" in {
        for
            (_, ui) <- picker(red)
            all     <- sliders(ui)
        yield
            val hue = all.find(_.attrs.ariaAttrs.get("label").contains("Hue")).get
            assert(hue.attrs.ariaAttrs.get("valuemin").contains("0"))
            assert(hue.attrs.ariaAttrs.get("valuemax").contains("359"), "degrees, not percent")
            val sat = all.find(_.attrs.ariaAttrs.get("label").contains("Saturation")).get
            assert(sat.attrs.ariaAttrs.get("valuemax").contains("100"))
    }

    "the horizontal arrows move saturation and leave the rest of the colour alone" in {
        for
            down <- after("Saturation", UI.Keyboard.ArrowLeft)
            up   <- after("Saturation", UI.Keyboard.ArrowRight, from = "#ff8080")
        yield
            assert(uic.ColorPicker.hsvOf(down)._2 < 1.0 && uic.ColorPicker.hsvOf(down)._1 == 0.0)
            assert(uic.ColorPicker.hsvOf(up)._2 > uic.ColorPicker.hsvOf("#ff8080")._2)
    }

    "the vertical arrows move brightness on the plane and hue on the bar" in {
        for
            darker <- after("Brightness", UI.Keyboard.ArrowDown)
            hued   <- after("Hue", UI.Keyboard.ArrowUp)
        yield
            assert(uic.ColorPicker.hsvOf(darker)._3 < 1.0, "the plane's vertical axis is brightness")
            assert(uic.ColorPicker.hsvOf(hued)._1 > 0.0, "the bar's is hue")
    }

    "an axis takes only its own arrows, so a reader is never surprised by the other" in {
        for
            sideways <- after("Saturation", UI.Keyboard.ArrowUp)
            straight <- after("Brightness", UI.Keyboard.ArrowRight)
        yield assert(sideways == red && straight == red)
    }

    "the page keys move ten at a time" in {
        for
            one <- after("Brightness", UI.Keyboard.ArrowDown)
            ten <- after("Brightness", UI.Keyboard.PageDown)
        yield
            val oneV = uic.ColorPicker.hsvOf(one)._3
            val tenV = uic.ColorPicker.hsvOf(ten)._3
            assert(tenV < oneV && math.abs((1.0 - tenV) - 10 * (1.0 - oneV)) < 0.02)
    }

    "Home and End reach the ends of the axis" in {
        for
            none <- after("Saturation", UI.Keyboard.Home)
            full <- after("Saturation", UI.Keyboard.End, from = "#804040")
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

    "the overlay's saturation slider is what focus is seeded onto, and what hands it back" in {
        for
            ref  <- Signal.initRef(red)
            open <- Signal.initRef(true)
            ui = uic.ColorPicker().value(ref).wired(open, Present(ref), red)
            all <- sliders(ui)
            sat = all.find(_.attrs.ariaAttrs.get("label").contains("Saturation")).get
            hue = all.find(_.attrs.ariaAttrs.get("label").contains("Hue")).get
        yield
            assert(sat.attrs.focusAuto.contains(true), "opening takes the reader to the first axis")
            assert(sat.attrs.focusRestore.contains(true), "and closing gives focus back to the swatch")
            assert(hue.attrs.focusAuto.isEmpty, "only one, or the panel fights itself")
    }

    "the panel takes no focus of its own, so Tab out of it has somewhere to go" in {
        for
            ref  <- Signal.initRef(red)
            open <- Signal.initRef(true)
            ui = uic.ColorPicker().value(ref).wired(open, Present(ref), red)
            overlay <- elementWithClass(ui, "p-uic-overlay-panel")
        yield
            assert(overlay.attrs.focusAuto.isEmpty)
            assert(overlay.attrs.focusTrap.isEmpty, "a colour picker is not a modal")
    }

    "Escape from an axis closes the panel" in {
        for
            ref  <- Signal.initRef(red)
            open <- Signal.initRef(true)
            ui = uic.ColorPicker().value(ref).wired(open, Present(ref), red)
            all   <- sliders(ui)
            _     <- press(all.head, UI.Keyboard.Escape)
            still <- open.get
        yield assert(!still)
    }

    "the swatch toggles, where it used to only ever open" in {
        for
            ref  <- Signal.initRef(red)
            open <- Signal.initRef(true)
            ui = uic.ColorPicker().value(ref).wired(open, Present(ref), red)
            swatch <- elementWithClass(ui, "p-colorpicker-preview")
            _      <- click(swatch)
            closed <- open.get
            _      <- click(swatch)
            opened <- open.get
        yield assert(!closed && opened)
    }

end ColorPickerTest
