package kyo.uic

import kyo.*
import kyo.UI.*

/** Title — native kyo-ui typographic primitive (Prime ships no title component;
  * the `p-uic-title` remainder CSS provides the rem scale — h1 2rem/600 down to
  * h6 0.875rem/600, Inter inherited). Renders a heading-role `div.p-uic-title`
  * with `p-uic-title--h<n>` for the visual size.
  *
  * The semantic `level` (ARIA `aria-level`) is decoupled from the visual
  * `size`; `size` defaults to the level when unset. `wrap(false)` keeps the
  * title on a single line.
  */
final case class Title private (
    levelV: TitleLevel = TitleLevel.H2,
    sizeV: Maybe[TitleLevel] = Absent,
    wrapFlag: Boolean = true,
    kids: List[UI] = Nil
) extends Node:
    type Self = Title

    /** Semantic heading level (`role="heading"` + `aria-level`, default H2). */
    def level(v: TitleLevel): Title = copy(levelV = v)

    /** Visual appearance (`p-uic-title--h<n>`); defaults to the semantic level. */
    def size(v: TitleLevel): Title = copy(sizeV = Present(v))

    /** `wrap(false)` keeps the title on one line (`p-uic-title--nowrap`); default wraps. */
    def wrap(v: Boolean): Title = copy(wrapFlag = v)

    def apply(cs: UI*): Title = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        val visual = sizeV.getOrElse(levelV)
        var el = div
            .cssClass("p-uic-title")
            .cssClass(s"p-uic-title--${visual.token.toLowerCase}")
            .role("heading")
            .aria("level", levelV.token.stripPrefix("H"))
        if !wrapFlag then el = el.cssClass("p-uic-title--nowrap")
        el(kids.map(toChild)*)
    end render
end Title

object Title:
    def apply(): Title = new Title()
