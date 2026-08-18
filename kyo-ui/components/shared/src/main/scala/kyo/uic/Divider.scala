package kyo.uic

import kyo.*
import kyo.UI.*

/** Divider flow (`.p-divider-horizontal` vs `.p-divider-vertical`). */
enum DividerLayout derives CanEqual:
    case Horizontal, Vertical

/** Divider line style (`.p-divider-solid|-dashed|-dotted`). */
enum DividerLineStyle derives CanEqual:
    case Solid, Dashed, Dotted

/** Divider content alignment: `Left`/`Center`/`Right` apply to horizontal
  * dividers, `Top`/`Center`/`Bottom` to vertical ones (Prime's `align`
  * vocabulary — a value from the other axis is ignored, like Prime).
  */
enum DividerAlign derives CanEqual:
    case Left, Center, Right, Top, Bottom

/** Divider — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Divider anatomy: `div.p-divider.p-component.p-divider-horizontal|-vertical
  * .p-divider-solid|-dashed|-dotted[.p-divider-left|-center|-right|-top|-bottom]`
  * > optional `div.p-divider-content`), so the extracted `@primeuix` divider
  * CSS applies verbatim.
  *
  * The line is the root's `::before`; default-slot children render inside
  * `div.p-divider-content` sitting on the line. Content positioning mirrors
  * Prime exactly, including its quirk: alignment classes follow the class map
  * (an unset horizontal align gets `.p-divider-left`) while the actual
  * position comes from the inline `justify-content`/`align-items` Prime's
  * `inlineStyles` emit (unset → centered). Renders `role="separator"` with
  * `aria-orientation` like Prime.
  */
final case class Divider private (
    layoutV: DividerLayout = DividerLayout.Horizontal,
    lineStyleV: DividerLineStyle = DividerLineStyle.Solid,
    alignV: Maybe[DividerAlign] = Absent,
    kids: List[UI] = Nil
) extends Node:
    type Self = Divider

    /** `Horizontal` (default) or `Vertical` flow. */
    def layout(v: DividerLayout): Divider = copy(layoutV = v)

    /** Line style: `Solid` (default) / `Dashed` / `Dotted` (Prime's `type`). */
    def lineStyle(v: DividerLineStyle): Divider = copy(lineStyleV = v)

    /** Content alignment along the divider (Prime's `align`). */
    def align(v: DividerAlign): Divider = copy(alignV = Present(v))

    /** Adds default-slot children, rendered as `div.p-divider-content` on the line. */
    def apply(cs: UI*): Divider = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        val layoutToken = layoutV match
            case DividerLayout.Horizontal => "horizontal"
            case DividerLayout.Vertical   => "vertical"
        val lineToken = lineStyleV match
            case DividerLineStyle.Solid  => "solid"
            case DividerLineStyle.Dashed => "dashed"
            case DividerLineStyle.Dotted => "dotted"
        var el = div
            .cssClass("p-divider")
            .cssClass("p-component")
            .cssClass(s"p-divider-$layoutToken")
            .cssClass(s"p-divider-$lineToken")
            .role("separator")
            .aria("orientation", layoutToken)
        // Alignment class + inline position, both exactly per PrimeVue's class map
        // and inlineStyles (including the unset-horizontal → .p-divider-left quirk).
        (layoutV, alignV) match
            case (DividerLayout.Horizontal, Absent | Present(DividerAlign.Left)) =>
                el = el.cssClass("p-divider-left")
                el = el.style(_.justify(if alignV.isEmpty then Style.Justification.center else Style.Justification.start))
            case (DividerLayout.Horizontal, Present(DividerAlign.Center)) =>
                el = el.cssClass("p-divider-center").style(_.justify(_.center))
            case (DividerLayout.Horizontal, Present(DividerAlign.Right)) =>
                el = el.cssClass("p-divider-right").style(_.justify(_.end))
            case (DividerLayout.Vertical, Absent | Present(DividerAlign.Center)) =>
                el = el.cssClass("p-divider-center").style(_.align(_.center))
            case (DividerLayout.Vertical, Present(DividerAlign.Top)) =>
                el = el.cssClass("p-divider-top").style(_.align(_.start))
            case (DividerLayout.Vertical, Present(DividerAlign.Bottom)) =>
                el = el.cssClass("p-divider-bottom").style(_.align(_.end))
            case _ => () // cross-axis align value: no class, no inline position (Prime behaviour)
        end match
        val content: List[UI] =
            if kids.isEmpty then Nil
            else List(div.cssClass("p-divider-content")(kids.map(toChild)*))
        el(content.map(toChild)*)
    end render
end Divider

object Divider:
    def apply(): Divider = new Divider()
