package kyo.uic

/** A single icon glyph: the raw SVG path data of one icon, rendered inline as
  * `<svg><path/></svg>` with `fill=currentColor`. Sets: [[Icons]] (PrimeIcons,
  * primary) and [[FioriIcons]] (legacy SAP-icons-v5).
  *
  * Values come from the generated [[Icons]] object (`uic.FioriIcons.save`, ...); each
  * icon is a `def` there, so Scala.js method-level dead-code elimination removes
  * the path strings of unused icons from downstream bundles.
  */
final case class IconGlyph(
    name: String,
    pathData: String,
    viewBoxWidth: Int,
    viewBoxHeight: Int
)

/** Package-internal inline-SVG renderer for a glyph with caller-chosen classes
  * (components attach their own structural classes, e.g. `p-button-icon`).
  */
private[uic] object GlyphSvg:
    import kyo.*
    import kyo.UI.*

    def apply(glyph: IconGlyph, classes: String*)(using Frame): UI =
        build(span.aria("hidden", "true"), glyph, classes)

    /** Variant with an inline style on the wrapping span (MeterGroup colors its
      * label icons with the meter color, as Prime does via an inline style).
      */
    def styled(glyph: IconGlyph, styleF: Style.type => Style, classes: String*)(using Frame): UI =
        build(span.aria("hidden", "true").style(styleF), glyph, classes)

    private def build(base: UI.Ast.SpanElement, glyph: IconGlyph, classes: Seq[String])(using Frame): UI =
        val svg = Svg.svg
            .viewBox(Svg.ViewBox(0, 0, glyph.viewBoxWidth, glyph.viewBoxHeight))
            .width(16)
            .height(16)(
                Svg.path.d(Svg.PathData.raw(glyph.pathData)).fill(Svg.Paint.CurrentColor)
            )
        classes
            .foldLeft(base)((el, c) => el.cssClass(c))
            .data("uic-icon", glyph.name)(svg)
    end build
end GlyphSvg
