package kyo.uic

import kyo.*
import kyo.UI.*

/** ScrollPanel — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ScrollPanel anatomy: `div.p-scrollpanel.p-component` >
  * `div.p-scrollpanel-content-container` > `div.p-scrollpanel-content`), so the
  * extracted `@primeuix` scrollpanel CSS applies verbatim.
  *
  * DEFERRAL (server-honest): Prime's ScrollPanel drives CUSTOM scrollbar
  * elements (`.p-scrollpanel-bar-x/-y`) from JS scroll measuring — that
  * lifecycle has no server-side equivalent, so this version scrolls NATIVELY:
  * the kyo remainder re-enables the native scrollbars Prime's content CSS
  * hides and styles them thin with the `--p-scrollpanel-bar-*` tokens
  * (`scrollbar-width: thin` + WebKit thumb rules). The JS-managed bars can
  * layer on later without changing this anatomy.
  *
  * Size the viewport via `width`/`height` (inline styles, like Prime's
  * showcase does via `style`).
  */
final case class ScrollPanel private (
    widthV: Maybe[String] = Absent,
    heightV: Maybe[String] = Absent,
    kids: List[UI] = Nil
) extends Node:
    type Self = ScrollPanel

    /** CSS width of the scroll viewport (inline style). */
    def width(v: String): ScrollPanel = copy(widthV = Present(v))

    /** CSS height of the scroll viewport (inline style). */
    def height(v: String): ScrollPanel = copy(heightV = Present(v))

    /** Adds content children. */
    def apply(cs: UI*): ScrollPanel = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        var st = Style.empty
        widthV.foreach(w => st = st.width(CssValue.length(w)))
        heightV.foreach(h => st = st.height(CssValue.length(h)))
        div.cssClass("p-scrollpanel").cssClass("p-component").style(st)(
            div.cssClass("p-scrollpanel-content-container")(
                div.cssClass("p-scrollpanel-content")(kids.map(toChild)*)
            )
        )
    end render
end ScrollPanel

object ScrollPanel:
    def apply(): ScrollPanel = new ScrollPanel()

    /** A scroll panel with the given content — size it via `width`/`height`. */
    def apply(cs: UI*): ScrollPanel = new ScrollPanel(kids = cs.toList)
end ScrollPanel
