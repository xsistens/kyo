package kyo.uic

import kyo.*
import kyo.UI.*

/** MeterGroup — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * MeterGroup anatomy: `div.p-metergroup.p-component
  * .p-metergroup-horizontal|-vertical[role=meter]` > optional label list /
  * start slot / `div.p-metergroup-meters` > `span.p-metergroup-meter` (inline
  * size % + background color) / end slot / optional label list —
  * `ol.p-metergroup-label-list.p-metergroup-label-list-horizontal|-vertical` >
  * `li.p-metergroup-label` > `span.p-metergroup-label-marker` (or the meter's
  * icon as `.p-metergroup-label-icon`) + `span.p-metergroup-label-text`), so
  * the extracted `@primeuix` metergroup CSS applies verbatim.
  *
  * Values scale against `max` (default 100); like Prime, a meter that rounds
  * to 0% renders no bar segment (its label still lists), and the label text is
  * `Label (NN%)`. Colors: pass a CSS color per meter (`#hex` or
  * `var(--token)` — kyo's typed styles carry no other color forms), or leave
  * it unset to draw from the rotating default palette
  * (`--p-primary-color`, then `--p-orange-500`, `--p-cyan-500`,
  * `--p-purple-500`, `--p-teal-500`, `--p-pink-500`, `--p-indigo-500`,
  * `--p-yellow-500`, cycling).
  *
  * Orientation follows Prime's two independent props: `orientation` flips the
  * meter track (vertical segments size by height; give the group a height from
  * the surrounding layout), `labelOrientation` stacks the label list. The
  * label list renders after the meters by default (Prime's
  * `labelPosition="end"`); `labelPosition(Start)` moves it before them.
  * Template slots mirror Prime's: `startTemplate`/`endTemplate` render around
  * the meter track, `meterTemplate` replaces each segment, `labelTemplate`
  * replaces each label row's content (Prime's label slot swaps the whole list;
  * per-item functions fit the library's typed template idiom — see
  * `Timeline.content`).
  */
final case class MeterGroup private (
    metersV: List[MeterGroup.Meter] = Nil,
    maxV: Double = 100.0,
    orientationV: Orientation = Orientation.Horizontal,
    labelOrientationV: Orientation = Orientation.Horizontal,
    labelPositionV: LabelPosition = LabelPosition.End,
    startTemplateV: Maybe[UI] = Absent,
    endTemplateV: Maybe[UI] = Absent,
    meterTemplateV: Maybe[(MeterGroup.Meter, Double) => UI] = Absent,
    labelTemplateV: Maybe[(MeterGroup.Meter, Double) => UI] = Absent
) extends Node:
    type Self = MeterGroup

    /** Appends one meter drawing its color from the default palette. A `Signal[String]` label
      * re-renders in place on emission; the trailing `(NN%)` follows the value either way.
      */
    def meter(label: String | Signal[String], value: Double): MeterGroup =
        copy(metersV = metersV :+ MeterGroup.Meter(label, value))

    /** Appends one meter with an explicit CSS color (`#hex` or `var(--token)`). */
    def meter(label: String | Signal[String], value: Double, color: String): MeterGroup =
        copy(metersV = metersV :+ MeterGroup.Meter(label, value, Present(color)))

    /** Appends one meter whose label leads with `icon` instead of the color marker. */
    def meter(label: String | Signal[String], value: Double, icon: IconGlyph): MeterGroup =
        copy(metersV = metersV :+ MeterGroup.Meter(label, value, Absent, Present(icon)))

    /** Appends one meter with an explicit CSS color and a leading label icon. */
    def meter(label: String | Signal[String], value: Double, color: String, icon: IconGlyph): MeterGroup =
        copy(metersV = metersV :+ MeterGroup.Meter(label, value, Present(color), Present(icon)))

    /** Appends the given meters. */
    def meters(ms: Seq[MeterGroup.Meter]): MeterGroup = copy(metersV = metersV ++ ms.toList)

    /** The scale ceiling (default 100) — each meter's size is `value / max`. */
    def max(v: Double): MeterGroup = copy(maxV = v)

    /** Meter-track orientation: `Vertical` renders `.p-metergroup-vertical` — the
      * track becomes a column and segments size by HEIGHT percent (the group needs
      * a height from the surrounding layout). Default `Horizontal`.
      */
    def orientation(v: Orientation): MeterGroup = copy(orientationV = v)

    /** Label-list orientation (independent of the track, like Prime's
      * `labelOrientation`): `Vertical` stacks the labels. Default `Horizontal`.
      */
    def labelOrientation(v: Orientation): MeterGroup = copy(labelOrientationV = v)

    /** Places the label list before (`Start`) or after (`End`, default) the meter
      * track — Prime's `labelPosition`.
      */
    def labelPosition(v: LabelPosition): MeterGroup = copy(labelPositionV = v)

    /** Content rendered directly BEFORE the meter track (Prime's `start` slot). */
    def startTemplate(content: UI): MeterGroup = copy(startTemplateV = Present(content))

    /** Content rendered directly AFTER the meter track (Prime's `end` slot). */
    def endTemplate(content: UI): MeterGroup = copy(endTemplateV = Present(content))

    /** Replaces each meter segment with `(meter, percent) => UI` (Prime's `meter`
      * slot); the template renders for 0% meters too — skipping is its call.
      */
    def meterTemplate(f: (MeterGroup.Meter, Double) => UI): MeterGroup =
        copy(meterTemplateV = Present(f))

    /** Replaces each label row's CONTENT with `(meter, percent) => UI`, keeping the
      * `li.p-metergroup-label` rows (Prime's `label` slot swaps the whole list).
      */
    def labelTemplate(f: (MeterGroup.Meter, Double) => UI): MeterGroup =
        copy(labelTemplateV = Present(f))

    private def pct(v: Double): Double =
        if maxV <= 0 then 0.0 else math.max(0.0, math.min(100.0, v / maxV * 100.0))

    private def colorOf(m: MeterGroup.Meter, index: Int): Style.Color =
        m.color.flatMap(CssValue.color).getOrElse(MeterGroup.palette(index % MeterGroup.palette.size))

    private[uic] def render(using Frame): UI =
        val totalPct = math.round(pct(metersV.map(_.value).sum))
        val root = div
            .cssClass("p-metergroup")
            .cssClass("p-component")
            .cssClass(s"p-metergroup-${orientationV.token}")
            .role("meter")
            .aria("valuemin", "0")
            .aria("valuemax", NumberFmt(maxV))
            .aria("valuenow", totalPct.toString)

        // Like Prime, default segments rounding to 0% render no span at all; a
        // custom meterTemplate always renders (skipping is the template's call).
        val segments: List[UI] = metersV.zipWithIndex.collect {
            case (m, i) if meterTemplateV.isDefined || math.round(pct(m.value)) != 0L =>
                meterTemplateV match
                    case Present(f) => f(m, pct(m.value))
                    case Absent =>
                        val sized: Style.type => Style = orientationV match
                            case Orientation.Horizontal => _.width(Length.Pct(pct(m.value))).bg(colorOf(m, i))
                            case Orientation.Vertical   => _.height(Length.Pct(pct(m.value))).bg(colorOf(m, i))
                        span.cssClass("p-metergroup-meter").style(sized)
        }
        val meters: UI = div.cssClass("p-metergroup-meters")(segments.map(toChild)*)

        val labels: UI = ol
            .cssClass("p-metergroup-label-list")
            .cssClass(s"p-metergroup-label-list-${labelOrientationV.token}")(
                metersV.zipWithIndex.map { (m, i) =>
                    val content: List[UI] = labelTemplateV match
                        case Present(f) => List(f(m, pct(m.value)))
                        case Absent =>
                            val lead: UI = m.icon match
                                case Present(g) =>
                                    GlyphSvg.styled(g, _.color(colorOf(m, i)), "p-metergroup-label-icon")
                                case Absent =>
                                    span.cssClass("p-metergroup-label-marker").style(_.bg(colorOf(m, i)))
                            val labelText: UI = m.label match
                                case TextValue.Dyn(sig) =>
                                    // The percentage is build-time (the meter value is not reactive), so the
                                    // whole label is a projection of the one string signal and goes in as a
                                    // text child — patched in place instead of rebuilding the span.
                                    span.cssClass("p-metergroup-label-text")(
                                        sig.map(t => s"$t (${math.round(pct(m.value))}%)")
                                    )
                                case TextValue.Const(t) =>
                                    span.cssClass("p-metergroup-label-text")(s"$t (${math.round(pct(m.value))}%)")
                            List(lead, labelText)
                    toChild(li.cssClass("p-metergroup-label")(content.map(toChild)*))
                }*
            )

        val startLabels: List[UI] = if labelPositionV == LabelPosition.Start then List(labels) else Nil
        val endLabels: List[UI]   = if labelPositionV == LabelPosition.End then List(labels) else Nil
        val body: List[UI] =
            startLabels ++ startTemplateV.toList ++ List(meters) ++ endTemplateV.toList ++ endLabels
        root(body.map(toChild)*)
    end render

    /** Formats the max for `aria-valuemax` without a trailing `.0`. */
    private def NumberFmt(v: Double): String =
        if v == math.rint(v) then v.toLong.toString else v.toString
end MeterGroup

object MeterGroup:
    def apply(): MeterGroup = new MeterGroup()

    /** One meter: `label` + `value` (scaled against the group's `max`), an
      * optional CSS color (`#hex` or `var(--token)`; unset draws from the
      * rotating default palette), and an optional label icon (replaces the color
      * marker, tinted with the meter color — Prime semantics). A reactive label
      * re-renders only its own text region; the `(NN%)` suffix follows the value.
      */
    final case class Meter private[uic] (
        label: TextValue,
        value: Double,
        color: Maybe[String],
        icon: Maybe[IconGlyph]
    ):
        /** The label as plain text, `""` for a reactive one. What a [[meterTemplate]] or
          * [[labelTemplate]] reads when it composes its own label string: the carrier itself is
          * package-private, and a template that wants the reactive text should take the signal it
          * built the meter from rather than read it back out here.
          */
        def labelText: String = label.constOrEmpty
    end Meter

    object Meter:
        /** Construct a meter. A `Signal[String]` label re-renders in place on emission. */
        def apply(
            label: String | Signal[String],
            value: Double,
            color: Maybe[String] = Absent,
            icon: Maybe[IconGlyph] = Absent
        ): Meter = new Meter(ReactiveValue(label), value, color, icon)
    end Meter

    /** The rotating default segment palette (Prime theme tokens). */
    private val palette: Vector[Style.Color] = Vector(
        Style.Color.variable("p-primary-color"),
        Style.Color.variable("p-orange-500"),
        Style.Color.variable("p-cyan-500"),
        Style.Color.variable("p-purple-500"),
        Style.Color.variable("p-teal-500"),
        Style.Color.variable("p-pink-500"),
        Style.Color.variable("p-indigo-500"),
        Style.Color.variable("p-yellow-500")
    )
end MeterGroup
