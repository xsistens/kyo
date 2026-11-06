package kyo.uic

import kyo.*
import kyo.UI.*

/** Toolbar: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Toolbar anatomy: `div.p-toolbar[role=toolbar]` > `div.p-toolbar-start` +
  * `div.p-toolbar-center` + `div.p-toolbar-end`), so the extracted `@primeuix`
  * toolbar CSS applies verbatim. The three sections replace the previous flow/spacer
  * model: `start`/`center`/`end` each append content to their region, and the
  * root's `justify-content: space-between` spreads them.
  */
final class Toolbar private[uic] (private[uic] val p: Toolbar.Props) extends Node, HasAccessibleNameRef, HasProps:
    import p.*

    type Self = Toolbar

    /** Appends content to the leading section (`.p-toolbar-start`). */
    def start(cs: UI*): Toolbar = new Toolbar(p.copy(startKids = startKids ++ cs))

    /** Appends content to the middle section (`.p-toolbar-center`). */
    def center(cs: UI*): Toolbar = new Toolbar(p.copy(centerKids = centerKids ++ cs))

    /** Appends content to the trailing section (`.p-toolbar-end`). */
    def end(cs: UI*): Toolbar = new Toolbar(p.copy(endKids = endKids ++ cs))

    private[uic] def accessibility: Accessibility              = accessV
    private[uic] def withAccessibility(v: Accessibility): Self = new Toolbar(p.copy(accessV = v))
    private[uic] def render(using Frame): UI                   =
        var bar = div.cssClass("p-toolbar").cssClass("p-component").role("toolbar")
        accessV.nameV match
            case Present(TextValue.Const(n)) => bar = bar.aria("label", n)
            case Present(TextValue.Dyn(s))   => bar = bar.aria("label", s)
            case Absent                      => ()
        end match
        accessV.nameRefV.foreach(r => bar = bar.aria("labelledby", r))
        bar(
            div.cssClass("p-toolbar-start")(UI.children(startKids)*),
            div.cssClass("p-toolbar-center")(UI.children(centerKids)*),
            div.cssClass("p-toolbar-end")(UI.children(endKids)*)
        )
    end render
end Toolbar

object Toolbar:
    final private[uic] case class Props(
        startKids: List[UI] = Nil,
        centerKids: List[UI] = Nil,
        endKids: List[UI] = Nil,
        accessV: Accessibility = Accessibility.none
    )

    def apply(): Toolbar = new Toolbar(Props())
end Toolbar
