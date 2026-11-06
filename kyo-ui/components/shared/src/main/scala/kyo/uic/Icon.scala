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

/** Icon: native kyo-ui, PrimeOne design. Renders the glyph's SVG path as an
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
final class Icon private[uic] (private[uic] val p: Icon.Props) extends Node, HasAccessibleName, HasProps:
    import p.*

    type Self = Icon

    def size(px: Int): Icon = new Icon(p.copy(sizePx = px))

    private[uic] def accessibility: Accessibility              = accessV
    private[uic] def withAccessibility(v: Accessibility): Self = new Icon(p.copy(accessV = v))

    /** Shows the glyph name as a native tooltip (the `title` attribute). */
    def showTooltip(v: Boolean): Icon = new Icon(p.copy(showTooltipFlag = v))

    /** Explicit mode override. When unset, the mode is implicit: `Decorative` by
      * default, `Interactive` when an `onClick` is set (and `Image` semantics when
      * only an `accessibleName` is set).
      */
    def mode(v: IconMode): Icon = new Icon(p.copy(modeV = Present(v)))

    /** Runs `action` when the icon is activated; makes the icon focusable with a button role. */
    def onClick(action: => UI.Action)(using Frame): Icon =
        new Icon(p.copy(onClickEff = Present(Callback(action))))

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
        val withTip  = if showTooltipFlag then base.jsProp("title", glyph.name) else base
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
                accessV.nameV match
                    case Present(TextValue.Const(n)) => img.aria("label", n)
                    case Present(TextValue.Dyn(s))   => img.aria("label", s)
                    case Absent                      => img
                end match
            case IconMode.Decorative =>
                el.role("presentation").aria("hidden", "true")
            case IconMode.Interactive =>
                val btn      = el.role("button").tabIndex(0)
                val withName = accessV.nameV match
                    case Present(TextValue.Const(n)) => btn.aria("label", n)
                    case Present(TextValue.Dyn(s))   => btn.aria("label", s)
                    case Absent                      => btn
                onClickEff.map(e => operable(withName, e)).getOrElse(withName)

    /** Implicit default behaviour (no explicit `mode`): decorative, image when an
      * `accessibleName` is set, interactive when an `onClick` is set.
      */
    private def implicitMode(el: Ast.SpanElement)(using Frame): Ast.SpanElement =
        val withA11y = accessV.nameV match
            case Present(TextValue.Const(n)) => el.role("img").aria("label", n)
            case Present(TextValue.Dyn(s))   => el.role("img").aria("label", s)
            case Absent                      => el.aria("hidden", "true")
        onClickEff match
            case Present(e) => operable(withA11y.role("button").tabIndex(0), e)
            case Absent     => withA11y
    end implicitMode

    /** Runs `action` on a click and on the two keys a `role="button"` owes the reader.
      *
      * A span is not a button, so the browser gives it neither: without this the icon was a tab
      * stop that answered nothing at all.
      */
    private def operable(el: Ast.SpanElement, action: UI.Action)(using Frame): Ast.SpanElement =
        el.onClick(action).onKeyDown { evt =>
            evt.key match
                case Keyboard.Enter | Keyboard.Space => action
                case _                               => ()
        }
end Icon

object Icon:
    final private[uic] case class Props(
        glyph: IconGlyph,
        sizePx: Int = 16,
        accessV: Accessibility = Accessibility.none,
        showTooltipFlag: Boolean = false,
        modeV: Maybe[IconMode] = Absent,
        onClickEff: Maybe[UI.Action] = Absent
    )

    def apply(glyph: IconGlyph): Icon = new Icon(Props(glyph))
end Icon
