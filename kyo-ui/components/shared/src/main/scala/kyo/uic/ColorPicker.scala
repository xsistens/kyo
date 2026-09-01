package kyo.uic

import kyo.*
import kyo.UI.*

/** ColorPicker — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ColorPicker anatomy: `div.p-colorpicker.p-component` > (overlay) the
  * `input.p-colorpicker-preview` swatch trigger + the floating
  * `div.p-colorpicker-panel` OR (inline) `div.p-colorpicker-panel.p-colorpicker-panel-inline`
  * > `div.p-colorpicker-content` > `div.p-colorpicker-color-selector` >
  * `div.p-colorpicker-color-background` (the saturation × brightness plane,
  * tinted to the current hue) > `div.p-colorpicker-color-handle` + the
  * `div.p-colorpicker-hue` bar > `div.p-colorpicker-hue-handle`), so the
  * extracted `@primeuix` colorpicker CSS applies verbatim.
  *
  * A [[TextFormControl]] over that hex string, so `uic.ColorPicker().bind(field)`
  * wires value, validity, message and the blur trigger in one call.
  *
  * Server-honest: the value is a single hex string bound to a
  * `SignalRef[String]`. HSB is DERIVED from the hex on every render (pure math
  * in [[ColorPicker.hsvOf]]) — the plane's tint, both handle positions, and the
  * preview swatch all follow from the bound hex, so a ref write from anywhere
  * moves the picker.
  *
  * Keyboard: the plane and the hue bar are operated through real sliders. The
  * plane carries TWO values and a `role="slider"` carries exactly one
  * `aria-valuenow`, so it is a `role="group"` holding two of them — Saturation on
  * the horizontal arrows, Brightness on the vertical, each with its own Home,
  * End, PageUp and PageDown. The hue bar IS one axis, so it wears the role
  * itself. All three are visually hidden (Prime's `.p-hidden-accessible`, as
  * Rating's radios are) because the visible thing is the handle, and the ring is
  * drawn on the surface through `:focus-within`. Opening the overlay seeds focus
  * onto the saturation axis and closing hands it back to the swatch; Escape
  * closes. The panel traps nothing: a colour picker is not a modal, and Tab out
  * of it has somewhere to go.
  *
  * Interaction is kyo-ui's pointer-drag: the 2D plane and the hue
  * bar each carry `onPointerDown` + `onPointerMove` (drag session with
  * setPointerCapture, rAF-coalesced). A press/drag on the plane maps
  * `(x/rectW, 1 − y/rectH) → (saturation, brightness)`; on the hue bar,
  * `(1 − y/rectH)·360 → hue`. The new HSB is converted back to hex ON THE
  * SERVER, written into the ref, and the normal re-render moves the handles and
  * repaints the swatch.
  *
  * HONEST cost: this is a WAN round-trip PER FRAME while dragging. The client
  * throttles pointer-move to one message per animation frame, but each of those
  * still crosses the wire, recomputes hex server-side, and re-renders — so a
  * drag is only as smooth as the round-trip latency (crisp locally / on the
  * SPA, visibly stepped over a slow link). This is the deliberate trade for a
  * zero-client-logic, server-authoritative color value.
  *
  * HONEST limit: only the hex is stored, not full HSB. For a grayscale value
  * (saturation 0, e.g. black/white) hue is not represented in hex, so it reads
  * back as 0 — dragging the hue bar on pure black has no visible effect until
  * brightness/saturation lift it off the achromatic axis.
  */
final case class ColorPicker private (
    valueBinding: Maybe[Input.Value] = Absent,
    inlineFlag: Boolean = false,
    disabledFlag: Maybe[BoolValue] = Absent,
    anchorV: OverlayAnchor = OverlayAnchor.BottomStart,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    onChangeF: Maybe[String => Any < Async] = Absent,
    onBlurF: Maybe[String => Any < Async] = Absent,
    idV: Maybe[String] = Absent
) extends Node, TextFormControl, HasAccessibleName:
    type Self = ColorPicker

    /** Native `id` on the root — pair with `Label.forId`; the form layer stamps the bound
      * field's id here so focus-first-invalid can target it.
      */
    def id(v: String): ColorPicker = copy(idV = Present(v))

    /** Sets a constant color (renders the plane/handles/swatch statically). */
    def value(v: String): ColorPicker = copy(valueBinding = Present(Input.Value.Const(v)))

    /** Binds two-way to `ref`: drags write the recomputed hex back, ref changes move the picker. */
    def value(ref: SignalRef[String]): ColorPicker = copy(valueBinding = Present(Input.Value.Ref(ref)))

    /** Renders the panel inline (`.p-colorpicker-panel-inline`) instead of the
      * default overlay behind a swatch trigger (Prime's `inline`).
      */
    def inline(v: Boolean): ColorPicker = copy(inlineFlag = v)

    /** Disables the control (`.p-disabled`, no pointer interaction); a `Signal[Boolean]`
      * toggles it reactively (re-render).
      */
    def disabled(v: Boolean | Signal[Boolean]): ColorPicker = copy(disabledFlag = Present(ReactiveValue(v)))

    /** Which corner the overlay panel opens from (overlay mode only; default below). */
    def anchor(v: OverlayAnchor): ColorPicker = copy(anchorV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): ColorPicker = copy(accNameV = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): ColorPicker                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): ColorPicker                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): ColorPicker = copy(invalidMsgDynV = v)

    /** Fired with the NEW hex after the ref write-back. */
    def onChange(f: String => Any < Async): ColorPicker = copy(onChangeF = Present(f))

    /** Fires on focus loss with the current hex — unlike onChange it fires even when the
      * color was not changed. The validation layer's Blur trigger.
      */
    def onBlur(f: String => Any < Async): ColorPicker = copy(onBlurF = Present(f))

    private def interactive: Boolean = !disabledFlag.constTrue

    /** Resolves the reactive validity slots and hands `build` the `(red, message)`
      * snapshot. Called INSIDE the overlay mount rather than around it: a reactive
      * boundary wrapping `UI.mounted` would re-mount on every emission and close an open
      * panel (see [[FieldInvalid.reactive]]).
      */
    private def withValidity(build: (Boolean, Maybe[String]) => UI)(using Frame): UI =
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => build(invalidV.constTrue, invalidMsgV)
            case _                => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)(build)

    private[uic] def render(using Frame): UI =
        BoolValue.reactive(disabledFlag): d =>
            copy(disabledFlag = d).renderResolved

    private def renderResolved(using Frame): UI =
        valueBinding match
            case Present(Input.Value.Ref(ref)) => body(Present(ref), ColorPicker.DefaultHex)
            case Present(Input.Value.Const(v)) => body(Absent, v)
            case Absent                        => body(Absent, ColorPicker.DefaultHex)

    private def commit(hex: String, ref: Maybe[SignalRef[String]])(using Frame): Any < Async =
        val write: Any < Async = ref match
            case Present(r) => r.set(hex)
            case Absent     => ()
        val fire: Any < Async = onChangeF match
            case Present(f) => f(hex)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end commit

    /** The saturation × brightness plane + the hue bar.
      *
      * CRITICAL for the drag: the pointer HANDLERS sit on the STABLE surface
      * elements (`.p-colorpicker-color-selector` / `.p-colorpicker-hue`), which
      * are OUTSIDE the reactive region — a per-frame re-render would otherwise
      * detach the pointer-captured element and break the drag stream. Only the
      * tint + handle positions live in the inner `ref.render`. The handlers read
      * the LIVE value via `ref.get` (not a closed-over render-time hex), so a
      * plane drag keeps the current hue and a hue drag keeps the current
      * saturation/brightness. The surface's reactive child + the tint background
      * are `pointer-events: none` (glue) so the pointer lands on the stable
      * surface whose rect defines the coordinate space.
      */
    /** One axis of the picker as a real slider.
      *
      * The two axes of the PLANE are two sliders and not one, because a `role="slider"` carries
      * exactly one `aria-valuenow` and the plane carries two values: a single role over both
      * would announce a number that is only half the truth. They are visually hidden (Prime's own
      * `.p-hidden-accessible`, the same helper Rating's per-option radios use) because the visible
      * thing is the handle, and the ring is drawn on the surface through `:focus-within` instead.
      *
      * The element is STABLE: `aria-valuenow` rides the value signal as an attribute rather than
      * a re-render, and the handler reads the value live. A focusable element rebuilt on every
      * arrow press would lose the focus that pressed it, which is the same reason the pointer
      * handlers sit outside the reactive region.
      */
    private def axisSlider(
        label: String,
        r: SignalRef[String],
        read: ((Double, Double, Double)) => Double,
        write: ((Double, Double, Double), Double) => String,
        max: Double,
        unit: String,
        step: Double,
        decrease: Set[Keyboard],
        increase: Set[Keyboard],
        ref: Maybe[SignalRef[String]],
        open: Maybe[SignalRef[Boolean]],
        seed: Boolean
    )(using Frame): UI =
        def at(hex: String): Double = read(ColorPicker.hsvOf(hex))
        var el = span
            .cssClass("p-hidden-accessible")
            .role("slider")
            .aria("label", label)
            .aria("valuemin", "0")
            .aria("valuemax", ColorPicker.short(max))
            .aria("valuenow", r.map(hex => ColorPicker.short(at(hex))))
            .aria("valuetext", r.map(hex => ColorPicker.short(at(hex)) + unit))
            .tabIndex(0)
            .preventScrollKeys
        if seed then el = el.focusAuto(true).focusRestore(true)
        el = el.onKeyDown { e =>
            def move(delta: Double): Any < Async =
                r.get.map { cur =>
                    val hsv = ColorPicker.hsvOf(cur)
                    commit(write(hsv, math.max(0.0, math.min(max, read(hsv) + delta))), ref)
                }
            val eff: Any < Async = e.key match
                case k if decrease.contains(k) => move(-step)
                case k if increase.contains(k) => move(step)
                case Keyboard.PageDown         => move(-step * 10)
                case Keyboard.PageUp           => move(step * 10)
                case Keyboard.Home             => r.get.map(cur => commit(write(ColorPicker.hsvOf(cur), 0.0), ref))
                case Keyboard.End              => r.get.map(cur => commit(write(ColorPicker.hsvOf(cur), max), ref))
                case Keyboard.Escape =>
                    open match
                        case Present(o) => o.set(false)
                        case Absent     => ()
                case _ => ()
            eff
        }
        el
    end axisSlider

    private def content(ref: Maybe[SignalRef[String]], fallbackHex: String, open: Maybe[SignalRef[Boolean]] = Absent)(
        using Frame
    ): UI =
        def planeVisual(hex: String): UI =
            val (h, s, v)    = ColorPicker.hsvOf(hex)
            val (hr, hg, hb) = ColorPicker.hsvToRgb(h, 1.0, 1.0) // pure hue tint
            val handle = div.cssClass("p-colorpicker-color-handle").aria("hidden", "true")
                .style(_.left((s * 100).pct).top(((1 - v) * 100).pct))
            div.cssClass("p-colorpicker-color-background").style(_.bg(_.rgb(hr, hg, hb)))(toChild(handle))
        end planeVisual
        def hueVisual(hex: String): UI =
            val (h, _, _) = ColorPicker.hsvOf(hex)
            div.cssClass("p-colorpicker-hue-handle").aria("hidden", "true")
                .style(_.top(((1 - h / 360.0) * 100).pct))
        end hueVisual

        val planeInner: UI = ref match
            case Present(r) => r.render(planeVisual)
            case Absent     => planeVisual(fallbackHex)
        val hueInner: UI = ref match
            case Present(r) => r.render(hueVisual)
            case Absent     => hueVisual(fallbackHex)

        var selector = div.cssClass("p-colorpicker-color-selector")
        var hue      = div.cssClass("p-colorpicker-hue")
        ref.foreach { r =>
            if interactive then
                val onPlane = (e: PointerEvent) =>
                    r.get.map { cur =>
                        val (h, _, _) = ColorPicker.hsvOf(cur)
                        val ns        = ColorPicker.clamp01(if e.rectW <= 0 then 0 else e.x / e.rectW)
                        val nv        = ColorPicker.clamp01(if e.rectH <= 0 then 1 else 1 - e.y / e.rectH)
                        commit(ColorPicker.hexOf(h, ns, nv), ref)
                    }
                val onHue = (e: PointerEvent) =>
                    r.get.map { cur =>
                        val (_, s, v) = ColorPicker.hsvOf(cur)
                        val nh        = ColorPicker.clamp01(if e.rectH <= 0 then 0 else 1 - e.y / e.rectH) * 360.0
                        commit(ColorPicker.hexOf(nh, s, v), ref)
                    }
                selector = selector.onPointerDown(onPlane).onPointerMove(onPlane)
                hue = hue.onPointerDown(onHue).onPointerMove(onHue)
        }
        // The plane groups the two axes it controls; the hue bar IS one axis, so it wears the
        // slider role itself rather than hiding one inside.
        val axes: List[UI] = ref.toList.filter(_ => interactive).flatMap { r =>
            List(
                axisSlider(
                    "Saturation",
                    r,
                    _._2 * 100.0,
                    (hsv, x) => ColorPicker.hexOf(hsv._1, x / 100.0, hsv._3),
                    100.0,
                    "%",
                    1.0,
                    Set(Keyboard.ArrowLeft),
                    Set(Keyboard.ArrowRight),
                    ref,
                    open,
                    seed = open.isDefined
                ),
                axisSlider(
                    "Brightness",
                    r,
                    _._3 * 100.0,
                    (hsv, y) => ColorPicker.hexOf(hsv._1, hsv._2, y / 100.0),
                    100.0,
                    "%",
                    1.0,
                    Set(Keyboard.ArrowDown),
                    Set(Keyboard.ArrowUp),
                    ref,
                    open,
                    seed = false
                )
            )
        }
        if axes.nonEmpty then selector = selector.role("group").aria("label", "Saturation and brightness")
        val hueAxis: List[UI] = ref.toList.filter(_ => interactive).map { r =>
            axisSlider(
                "Hue",
                r,
                _._1,
                (hsv, h) => ColorPicker.hexOf(h, hsv._2, hsv._3),
                359.0,
                " degrees",
                1.0,
                Set(Keyboard.ArrowDown),
                Set(Keyboard.ArrowUp),
                ref,
                open,
                seed = false
            )
        }
        div.cssClass("p-colorpicker-content")(
            toChild(selector((planeInner :: axes).map(toChild)*)),
            toChild(hue((hueInner :: hueAxis).map(toChild)*))
        )
    end content

    private def body(ref: Maybe[SignalRef[String]], fallbackHex: String)(using Frame): UI =
        if inlineFlag then
            withValidity { (red, msg) =>
                FieldInvalid.withMessage(
                    shellRoot(red)(
                        toChild(
                            div.cssClass("p-colorpicker-panel").cssClass("p-colorpicker-panel-inline")(
                                toChild(content(ref, fallbackHex))
                            )
                        )
                    ),
                    red,
                    msg
                )
            }
        else overlayBody(ref, fallbackHex)
    end body

    /** The root box shared by the inline and overlay paths, with the resolved red state
      * stamped on it.
      */
    private def shellRoot(red: Boolean, extraClasses: String*)(using Frame): Ast.Div =
        var root = div.cssClass("p-colorpicker").cssClass("p-component")
        extraClasses.foreach(c => root = root.cssClass(c))
        idV.foreach(v => root = root.id(v))
        if disabledFlag.constTrue then root = root.cssClass("p-disabled")
        if red then root = root.cssClass("p-invalid").aria("invalid", "true")
        accNameV match
            case Present(TextValue.Const(n)) => root = root.aria("label", n)
            case Present(TextValue.Dyn(s))   => root = root.aria("label", s)
            case Absent                      => ()
        end match
        onBlurF.foreach { f =>
            root = root.onBlur(valueBinding match
                case Present(Input.Value.Ref(r))   => r.use(f)
                case Present(Input.Value.Const(v)) => f(v)
                case Absent                        => f(ColorPicker.DefaultHex))
        }
        root
    end shellRoot

    /** The preview swatch trigger (`input.p-colorpicker-preview` per Prime — here a
      * `button` so it needs no value plumbing); its background IS the current hex.
      * Bound to `ref` it re-renders reactively as the color changes.
      */
    private def previewSwatchOf(swatch: String => UI, ref: Maybe[SignalRef[String]], fallbackHex: String)(using Frame): UI =
        ref match
            case Present(r) => r.render(swatch)
            case Absent     => swatch(fallbackHex)

    private def previewSwatch(hex: String, onClickEff: Any < Async)(using Frame): UI =
        var preview = button
            .cssClass("p-colorpicker-preview")
            .jsProp("type", "button")
            .style(_.bg(Style.Color.hex(hex).getOrElse(Style.Color.rgb(0, 0, 0))))
            .onClick(onClickEff)
        if disabledFlag.constTrue then preview = preview.disabled(true).cssClass("p-disabled")
        accNameV match
            case Present(TextValue.Const(n)) => preview = preview.aria("label", n)
            case Present(TextValue.Dyn(s))   => preview = preview.aria("label", s)
            case Absent                      => ()
        end match
        preview
    end previewSwatch

    /** Overlay mode: a preview swatch trigger + the floating panel. The open state
      * is an internal ref owned by a `UI.mounted` region (Select's pattern); the
      * static placeholder is the closed swatch.
      */
    private def overlayBody(ref: Maybe[SignalRef[String]], fallbackHex: String)(using Frame): UI =
        val staticRoot: UI =
            withValidity { (red, msg) =>
                FieldInvalid.withMessage(
                    shellRoot(red, "p-uic-overlay-anchor")(
                        toChild(previewSwatchOf(hex => previewSwatch(hex, ()), ref, fallbackHex))
                    ),
                    red,
                    msg
                )
            }
        UI.mounted {
            for open <- Signal.initRef(false)
            yield wired(open, ref, fallbackHex)
        }.placeholder(staticRoot)
    end overlayBody

    /** The subscription tree the overlay mount publishes — the seam the tests drive, since a
      * golden render shows a mount only as its placeholder.
      */
    private[uic] def wired(open: SignalRef[Boolean], ref: Maybe[SignalRef[String]], fallbackHex: String)(using
        Frame
    ): UI =
        // Toggles rather than opens. A trigger that only ever opens is a trigger a reader cannot
        // close from, and it is the one control of the component that is reachable when the panel
        // is shut.
        val toggle: Any < Async = if interactive then open.getAndUpdate(!_) else ()
        // The Overlay primitive floats the panel; the .p-colorpicker-panel skin (sheet:
        // position:absolute; top:0; left:0) rides INSIDE it as a static child (see [[Theme]]'s
        // `position: static` glue) so it does not re-anchor to the panel origin.
        //
        // The panel seeds no focus of its own: the saturation slider does, which is what the
        // reader is being taken to, and its `focusRestore` hands focus back to the swatch on
        // close. A trap here would be worse than none — this panel is not modal, and Tab out of a
        // colour picker has somewhere to go.
        val panel = Overlay(open)
            .anchor(anchorV)
            .seedFocus(false)
            .matchWidth(false)(
                div.cssClass("p-colorpicker-panel")(toChild(content(ref, fallbackHex, Present(open))))
            )
        // The validity boundary sits INSIDE the mount: around it, an emission would re-mount and
        // drop `open`, closing the panel mid-interaction.
        withValidity { (red, msg) =>
            FieldInvalid.withMessage(
                shellRoot(red, "p-uic-overlay-anchor")(
                    toChild(previewSwatchOf(hex => previewSwatch(hex, toggle), ref, fallbackHex)),
                    toChild(panel)
                ),
                red,
                msg
            )
        }
    end wired
end ColorPicker

object ColorPicker:
    /** A color picker whose hex value binds two-way to `ref`. */
    def apply(ref: SignalRef[String]): ColorPicker = new ColorPicker(valueBinding = Present(Input.Value.Ref(ref)))

    /** An unbound color picker — set the value via [[ColorPicker.value]]. */
    def apply(): ColorPicker = new ColorPicker()

    private[uic] val DefaultHex = "#ff0000"

    private[uic] def clamp01(v: Double): Double = math.max(0.0, math.min(1.0, v))

    /** A percentage or degree count as a whole number, for the ARIA value attributes. */
    private[uic] def short(v: Double): String = math.round(v).toString

    /** Normalizes any accepted hex spelling to lowercase `#rrggbb`. */
    private[uic] def normalizeHex(raw: String): String =
        val (r, g, b) = hexToRgb(raw)
        f"#$r%02x$g%02x$b%02x"

    /** Parses `#rgb` / `#rrggbb` (with or without `#`) to 0–255 channels; falls
      * back to red on anything unparseable.
      */
    private[uic] def hexToRgb(raw: String): (Int, Int, Int) =
        val h = raw.trim.stripPrefix("#")
        def hx(s: String): Option[Int] =
            try Some(Integer.parseInt(s, 16))
            catch case _: Throwable => None
        val parsed =
            if h.length == 3 then
                for
                    r <- hx(s"${h(0)}${h(0)}")
                    g <- hx(s"${h(1)}${h(1)}")
                    b <- hx(s"${h(2)}${h(2)}")
                yield (r, g, b)
            else if h.length == 6 then
                for
                    r <- hx(h.substring(0, 2))
                    g <- hx(h.substring(2, 4))
                    b <- hx(h.substring(4, 6))
                yield (r, g, b)
            else None
        parsed.getOrElse((255, 0, 0))
    end hexToRgb

    /** HSB of a hex color: hue in [0,360), saturation/brightness in [0,1]. */
    private[uic] def hsvOf(hex: String): (Double, Double, Double) =
        val (ri, gi, bi) = hexToRgb(hex)
        rgbToHsv(ri, gi, bi)

    /** Hex `#rrggbb` for the given HSB. */
    private[uic] def hexOf(h: Double, s: Double, v: Double): String =
        val (r, g, b) = hsvToRgb(h, s, v)
        f"#$r%02x$g%02x$b%02x"

    private[uic] def rgbToHsv(ri: Int, gi: Int, bi: Int): (Double, Double, Double) =
        val r  = ri / 255.0
        val g  = gi / 255.0
        val b  = bi / 255.0
        val mx = math.max(r, math.max(g, b))
        val mn = math.min(r, math.min(g, b))
        val d  = mx - mn
        val v  = mx
        val s  = if mx <= 0 then 0.0 else d / mx
        val h =
            if d <= 0 then 0.0
            else if mx == r then 60.0 * (((g - b) / d) % 6)
            else if mx == g then 60.0 * (((b - r) / d) + 2)
            else 60.0 * (((r - g) / d) + 4)
        ((h + 360.0) % 360.0, s, v)
    end rgbToHsv

    private[uic] def hsvToRgb(h: Double, s: Double, v: Double): (Int, Int, Int) =
        val hh = ((h % 360.0) + 360.0) % 360.0 / 60.0
        val c  = v * s
        val x  = c * (1 - math.abs(hh % 2 - 1))
        val (r1, g1, b1) =
            if hh < 1 then (c, x, 0.0)
            else if hh < 2 then (x, c, 0.0)
            else if hh < 3 then (0.0, c, x)
            else if hh < 4 then (0.0, x, c)
            else if hh < 5 then (x, 0.0, c)
            else (c, 0.0, x)
        val m                     = v - c
        def to255(d: Double): Int = math.round((d + m) * 255).toInt.max(0).min(255)
        (to255(r1), to255(g1), to255(b1))
    end hsvToRgb
end ColorPicker
