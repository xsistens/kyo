package kyo.uic

import kyo.*
import kyo.UI.*

/** Which side of the target the tooltip box opens on. */
enum TooltipPosition derives CanEqual:
    case Top, Bottom, Left, Right

    /** The `.p-tooltip-<token>` positional class suffix. */
    private[uic] def token: String = this match
        case TooltipPosition.Top    => "top"
        case TooltipPosition.Bottom => "bottom"
        case TooltipPosition.Left   => "left"
        case TooltipPosition.Right  => "right"
end TooltipPosition

/** Tooltip: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Tooltip anatomy: `div.p-tooltip.p-component.p-tooltip-top|-bottom|-left|-right`
  * > `div.p-tooltip-arrow` + `div.p-tooltip-text`), so the extracted `@primeuix`
  * tooltip CSS applies verbatim.
  *
  * Deliberate deviation from Prime: Prime's tooltip is a JS directive that
  * mounts/positions the box on pointer events with timers. This one is
  * server-honest and CSS-driven: the box is always in the DOM (the sheet ships
  * `display: none`) and the `.p-uic-tooltip` wrapper's `:hover`/`:focus-within`
  * shows it, with zero state and zero round-trips. Consequences: no
  * showDelay/hideDelay/autoHide (those need JS timers), the box is not
  * interactive (`pointer-events: none`, Prime's autoHide default), and it
  * positions against the wrapped target (no viewport flip; pick the position
  * that fits).
  *
  * Usage: `uic.Tooltip("Saved!").position(TooltipPosition.Bottom)(uic.Button("Save"))`.
  * The target children render inside the relative wrapper; keyboard users get
  * the same box via `:focus-within` when the target is focusable.
  */
final class Tooltip private[uic] (private[uic] val p: Tooltip.Props) extends Node, HasProps:
    import p.*

    type Self = Tooltip

    /** Which side the box opens on (default [[TooltipPosition.Top]]). */
    def position(v: TooltipPosition): Tooltip = new Tooltip(p.copy(positionV = v))

    /** Adds target children (the elements the tooltip attaches to). */
    def apply(cs: UI*): Tooltip = new Tooltip(p.copy(kids = kids ++ cs))

    private[uic] def render(using Frame): UI =
        val textUI: UI = contentV match
            case Present(c) => c
            case Absent     =>
                textV match
                    case Present(TextValue.Const(t)) => stringToUI(t)
                    case Present(TextValue.Dyn(s))   => signalStringToUI(s)
                    case Absent                      => stringToUI("")
        val box: UI =
            div
                .cssClass("p-tooltip")
                .cssClass("p-component")
                .cssClass(s"p-tooltip-${positionV.token}")
                .role("tooltip")(
                    div.cssClass("p-tooltip-arrow"),
                    div.cssClass("p-tooltip-text")(textUI)
                )
        span.cssClass("p-uic-tooltip")(UI.children(kids :+ box)*)
    end render
end Tooltip

object Tooltip:
    final private[uic] case class Props(
        textV: Maybe[TextValue],
        contentV: Maybe[UI],
        positionV: TooltipPosition = TooltipPosition.Top,
        kids: List[UI] = Nil
    )

    /** A tooltip showing `text`. A `Signal[String]` re-renders it in place on emission (e.g. a locale-
      * driven `I18n.t` leaf).
      */
    def apply(text: String | Signal[String]): Tooltip = new Tooltip(Props(Present(ReactiveValue(text)), Absent))

    /** A tooltip with arbitrary `UI` content. */
    def apply(content: UI): Tooltip = new Tooltip(Props(Absent, Present(content)))
end Tooltip
