package kyo.uic

import kyo.*
import kyo.UI.*

/** Text — native kyo-ui typographic primitive (Prime ships no text component;
  * the `p-uic-text` remainder CSS provides body color and the clamp/empty
  * helpers). Renders a `span.p-uic-text` so it flows inline and inherits the
  * page font.
  *
  * `maxLines(n)` clamps the paragraph after `n` lines (CSS line-clamp via the
  * `data-uic-max-lines` hook, 1–10); `emptyIndicatorMode(On)` shows a dash when
  * the text is empty.
  */
final case class Text private (
    emptyIndicatorModeV: TextEmptyIndicatorMode = TextEmptyIndicatorMode.Off,
    maxLinesV: Maybe[Int] = Absent,
    kids: List[UI] = Nil
) extends Node:
    type Self = Text

    /** Specifies if an empty indicator (dash) should be displayed when there is no text. */
    def emptyIndicatorMode(v: TextEmptyIndicatorMode): Text = copy(emptyIndicatorModeV = v)

    /** Clamps the text after `v` lines before truncating (1–10, CSS line-clamp). */
    def maxLines(v: Int): Text = copy(maxLinesV = Present(math.max(1, math.min(10, v))))

    def apply(cs: UI*): Text = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        var el = span.cssClass("p-uic-text")
        if emptyIndicatorModeV == TextEmptyIndicatorMode.On then
            el = el.cssClass("p-uic-text--empty-indicator")
        maxLinesV.foreach { n =>
            el = el.cssClass("p-uic-text--clamp").data("uic-max-lines", n.toString)
        }
        el(kids.map(toChild)*)
    end render
end Text

object Text:
    def apply(): Text = new Text()
