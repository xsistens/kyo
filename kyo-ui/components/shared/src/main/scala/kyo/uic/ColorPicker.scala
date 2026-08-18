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
) extends Node, TextFormControl:
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

    /** Accessible name → `aria-label` on the preview swatch / inline root. */
    def accessibleName(v: String): ColorPicker = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): ColorPicker = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Marks the picker invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): ColorPicker = copy(invalidV = Present(BoolValue.Const(v)))

    /** Reactive validity: the bound signal toggles the invalid state on emission. */
    def invalid(sig: Signal[Boolean]): ColorPicker = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Message rendered below the picker while it is invalid (kyo extension —
      * `div.p-uic-invalid-message`).
      */
    def invalidMessage(v: String): ColorPicker = copy(invalidMsgV = Present(v))

    /** Reactive invalid message — `Present` shows the row and (by default) marks the
      * picker invalid; `Absent` clears both.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): ColorPicker = copy(invalidMsgDynV = Present(sig))

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
    private def content(ref: Maybe[SignalRef[String]], fallbackHex: String)(using Frame): UI =
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
        div.cssClass("p-colorpicker-content")(
            toChild(selector(toChild(planeInner))),
            toChild(hue(toChild(hueInner)))
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
            yield
                val toggle: Any < Async = if interactive then open.set(true) else ()
                // The Overlay primitive floats the panel; the .p-colorpicker-panel skin
                // (sheet: position:absolute; top:0; left:0) rides INSIDE it as a static
                // child (see [[Theme]]'s `position: static` glue) so it does not re-anchor
                // to the panel origin.
                val panel = Overlay(open)
                    .anchor(anchorV)
                    .matchWidth(false)(
                        div.cssClass("p-colorpicker-panel")(toChild(content(ref, fallbackHex)))
                    )
                // The validity boundary sits INSIDE the mount: around it, an emission would
                // re-mount and drop `open`, closing the panel mid-interaction.
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
        }.placeholder(staticRoot)
    end overlayBody
end ColorPicker

object ColorPicker:
    /** A color picker whose hex value binds two-way to `ref`. */
    def apply(ref: SignalRef[String]): ColorPicker = new ColorPicker(valueBinding = Present(Input.Value.Ref(ref)))

    /** An unbound color picker — set the value via [[ColorPicker.value]]. */
    def apply(): ColorPicker = new ColorPicker()

    private[uic] val DefaultHex = "#ff0000"

    private[uic] def clamp01(v: Double): Double = math.max(0.0, math.min(1.0, v))

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
