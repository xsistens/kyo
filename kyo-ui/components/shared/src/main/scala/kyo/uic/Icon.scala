package kyo.uic

import kyo.*
import kyo.UI.*

/** Icon modes: `Image` exposes the icon (`role="img"`), `Decorative` hides it
  * from assistive technology, `Interactive` makes it a focusable button.
  */
enum IconMode derives CanEqual:
    case Image, Decorative, Interactive

    private[uic] def token: String = this.toString
end IconMode

/** Icon — native kyo-ui, PrimeOne design. Renders the glyph's SVG path as an
  * inline `<svg><path/></svg>` with `fill=currentColor` inside a
  * `span.p-icon.p-uic-icon`, so it needs no icon font, works in SSR string
  * renders, and inherits the surrounding text color (Prime icons carry no
  * color of their own).
  *
  * Glyphs come from the generated [[Icons]] object: `uic.Icon(uic.Icons.check)`.
  * Decorative by default (`aria-hidden`); give it an `accessibleName` to expose
  * it, or an `onClick` to make it interactive (button role + focusability).
  * An explicit [[mode]] overrides that implicit behaviour.
  */
final case class Icon private (
    glyph: IconGlyph,
    sizePx: Int = 16,
    accessibleName: Maybe[TextValue] = Absent,
    showTooltipFlag: Boolean = false,
    modeV: Maybe[IconMode] = Absent,
    onClickEff: Maybe[Any < Async] = Absent
) extends Node:
    type Self = Icon

    def size(px: Int): Icon             = copy(sizePx = px)
    def accessibleName(v: String): Icon = copy(accessibleName = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Icon = copy(accessibleName = Present(TextValue.Dyn(sig)))

    /** Shows the glyph name as a native tooltip (the `title` attribute). */
    def showTooltip(v: Boolean): Icon = copy(showTooltipFlag = v)

    /** Explicit mode override. When unset, the mode is implicit: `Decorative` by
      * default, `Interactive` when an `onClick` is set (and `Image` semantics when
      * only an `accessibleName` is set).
      */
    def mode(v: IconMode): Icon = copy(modeV = Present(v))

    /** Runs `action` when the icon is activated; makes the icon focusable with a button role. */
    def onClick(action: => Any < Async)(using Frame): Icon =
        copy(onClickEff = Present(Sync.defer(action)))

    private[uic] def render(using Frame): UI =
        val svgIcon = Svg.svg
            .viewBox(Svg.ViewBox(0, 0, glyph.viewBoxWidth, glyph.viewBoxHeight))
            .width(sizePx)
            .height(sizePx)(
                Svg.path.d(Svg.PathData.raw(glyph.pathData)).fill(Svg.Paint.CurrentColor)
            )
        val base = span
            .cssClass("p-icon")
            .cssClass("p-uic-icon")
            .data("uic-icon", glyph.name)
        val withTip = if showTooltipFlag then base.jsProp("title", glyph.name) else base
        val withMode = modeV match
            case Present(m) => applyMode(withTip, m)
            case Absent     => implicitMode(withTip)
        withMode(svgIcon)
    end render

    /** Explicit `mode` semantics. */
    private def applyMode(el: Ast.SpanElement, m: IconMode)(using Frame): Ast.SpanElement =
        m match
            case IconMode.Image =>
                val img = el.role("img")
                accessibleName match
                    case Present(TextValue.Const(n)) => img.aria("label", n)
                    case Present(TextValue.Dyn(s))   => img.aria("label", s)
                    case Absent                      => img
                end match
            case IconMode.Decorative =>
                el.role("presentation").aria("hidden", "true")
            case IconMode.Interactive =>
                val btn = el.role("button").tabIndex(0)
                val withName = accessibleName match
                    case Present(TextValue.Const(n)) => btn.aria("label", n)
                    case Present(TextValue.Dyn(s))   => btn.aria("label", s)
                    case Absent                      => btn
                onClickEff.map(e => withName.onClick(e)).getOrElse(withName)

    /** Implicit default behaviour (no explicit `mode`): decorative, image when an
      * `accessibleName` is set, interactive when an `onClick` is set.
      */
    private def implicitMode(el: Ast.SpanElement)(using Frame): Ast.SpanElement =
        val withA11y = accessibleName match
            case Present(TextValue.Const(n)) => el.role("img").aria("label", n)
            case Present(TextValue.Dyn(s))   => el.role("img").aria("label", s)
            case Absent                      => el.aria("hidden", "true")
        onClickEff match
            case Present(e) => withA11y.role("button").tabIndex(0).onClick(e)
            case Absent     => withA11y
    end implicitMode
end Icon

object Icon:
    def apply(glyph: IconGlyph): Icon = new Icon(glyph)
