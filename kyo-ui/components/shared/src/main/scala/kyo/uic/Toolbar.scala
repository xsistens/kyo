package kyo.uic

import kyo.*
import kyo.UI.*

/** Toolbar — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Toolbar anatomy: `div.p-toolbar[role=toolbar]` > `div.p-toolbar-start` +
  * `div.p-toolbar-center` + `div.p-toolbar-end`), so the extracted `@primeuix`
  * toolbar CSS applies verbatim. The three sections replace the previous flow/spacer
  * model: `start`/`center`/`end` each append content to their region, and the
  * root's `justify-content: space-between` spreads them.
  */
final case class Toolbar private (
    startKids: List[UI] = Nil,
    centerKids: List[UI] = Nil,
    endKids: List[UI] = Nil,
    accessibleNameV: Maybe[TextValue] = Absent,
    accessibleNameRefV: Maybe[String] = Absent
) extends Node:
    type Self = Toolbar

    /** Appends content to the leading section (`.p-toolbar-start`). */
    def start(cs: UI*): Toolbar = copy(startKids = startKids ++ cs)

    /** Appends content to the middle section (`.p-toolbar-center`). */
    def center(cs: UI*): Toolbar = copy(centerKids = centerKids ++ cs)

    /** Appends content to the trailing section (`.p-toolbar-end`). */
    def end(cs: UI*): Toolbar = copy(endKids = endKids ++ cs)

    /** `aria-label` for the toolbar. */
    def accessibleName(v: String): Toolbar = copy(accessibleNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Toolbar = copy(accessibleNameV = Present(TextValue.Dyn(sig)))

    /** `aria-labelledby` id reference for the toolbar. */
    def accessibleNameRef(v: String): Toolbar = copy(accessibleNameRefV = Present(v))

    private[uic] def render(using Frame): UI =
        var bar = div.cssClass("p-toolbar").cssClass("p-component").role("toolbar")
        accessibleNameV match
            case Present(TextValue.Const(n)) => bar = bar.aria("label", n)
            case Present(TextValue.Dyn(s))   => bar = bar.aria("label", s)
            case Absent                      => ()
        end match
        accessibleNameRefV.foreach(r => bar = bar.aria("labelledby", r))
        bar(
            div.cssClass("p-toolbar-start")(startKids.map(toChild)*),
            div.cssClass("p-toolbar-center")(centerKids.map(toChild)*),
            div.cssClass("p-toolbar-end")(endKids.map(toChild)*)
        )
    end render
end Toolbar

object Toolbar:
    def apply(): Toolbar = new Toolbar()
