package kyo.uic

import kyo.*
import kyo.UI.*

/** Timeline content alignment relative to the line: `Left`/`Right` for the
  * vertical layout, `Top`/`Bottom` for the horizontal one, `Alternate` zigzags
  * in both.
  */
enum TimelineAlign derives CanEqual:
    case Left, Right, Alternate, Top, Bottom

    private[uic] def token: String = this match
        case TimelineAlign.Left      => "left"
        case TimelineAlign.Right     => "right"
        case TimelineAlign.Alternate => "alternate"
        case TimelineAlign.Top       => "top"
        case TimelineAlign.Bottom    => "bottom"
end TimelineAlign

/** Timeline orientation. */
enum TimelineLayout derives CanEqual:
    case Vertical, Horizontal

    private[uic] def token: String = this match
        case TimelineLayout.Vertical   => "vertical"
        case TimelineLayout.Horizontal => "horizontal"
end TimelineLayout

/** Timeline: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Timeline anatomy: `div.p-timeline.p-component.p-timeline-<align>
  * .p-timeline-<layout>` > per event `div.p-timeline-event` >
  * `div.p-timeline-event-opposite` + `div.p-timeline-event-separator`
  * (> `div.p-timeline-event-marker` + `div.p-timeline-event-connector`) +
  * `div.p-timeline-event-content`), so the extracted `@primeuix` timeline CSS
  * applies verbatim. The last event renders no connector (Prime semantics).
  *
  * Events are TYPED: `events(Seq[A])` supplies the data, `content`/`opposite`
  * render each side of the line, and `marker` replaces the default token dot:
  * {{{
  * uic.Timeline[Order]()
  *   .events(orders)
  *   .content(o => span(o.status))
  *   .opposite(o => span(o.date))
  *   .align(uic.TimelineAlign.Alternate)
  * }}}
  */
final case class Timeline[A] private (
    eventSources: List[List[A] | Signal[Seq[A]]] = Nil,
    contentF: Maybe[A => UI] = Absent,
    oppositeF: Maybe[A => UI] = Absent,
    markerF: Maybe[A => UI] = Absent,
    alignV: TimelineAlign = TimelineAlign.Left,
    layoutV: TimelineLayout = TimelineLayout.Vertical
) extends Node:
    type Self = Timeline[A]

    /** Appends events, or a signal of events that change while the timeline is shown (the history of a record that is
      * still being written to). The events keep the order the calls gave them.
      */
    def events(es: Iterable[A] | Signal[Seq[A]]): Timeline[A] =
        val source: List[A] | Signal[Seq[A]] = es match
            case sig: Signal[?] => sig.asInstanceOf[Signal[Seq[A]]]
            case is             => is.asInstanceOf[Iterable[A]].toList
        copy(eventSources = eventSources :+ source)
    end events

    /** The events as ONE reading: fixed unless one of them was handed over as a signal. */
    private[uic] def entries(using Frame): ReactiveValue[List[A]] = ReactiveValue.concat(eventSources)

    /** Content template, rendered in `div.p-timeline-event-content`. */
    def content(f: A => UI): Timeline[A] = copy(contentF = Present(f))

    /** Opposite-side template, rendered in `div.p-timeline-event-opposite`. */
    def opposite(f: A => UI): Timeline[A] = copy(oppositeF = Present(f))

    /** Custom marker replacing Prime's default token dot in
      * `div.p-timeline-event-marker`.
      */
    def marker(f: A => UI): Timeline[A] = copy(markerF = Present(f))

    /** Alignment: `Left` (default) / `Right` / `Alternate` for vertical,
      * `Top` / `Bottom` / `Alternate` for horizontal.
      */
    def align(v: TimelineAlign): Timeline[A] = copy(alignV = v)

    /** Orientation: `Vertical` (default) or `Horizontal`. */
    def layout(v: TimelineLayout): Timeline[A] = copy(layoutV = v)

    private[uic] def render(using Frame): UI =
        // The events of a live timeline share one region, so the last-child and odd/even rules still see one run of them.
        div
            .cssClass("p-timeline")
            .cssClass("p-component")
            .cssClass(s"p-timeline-${alignV.token}")
            .cssClass(s"p-timeline-${layoutV.token}")(UI.children(entries.children(eventEls))*)
    end render

    /** One `div.p-timeline-event` per event; the last one draws no connector. */
    private def eventEls(events: List[A])(using Frame): List[UI] =
        val n = events.length
        events.zipWithIndex.map { (a, i) =>
            val oppositeEl: UI = div.cssClass("p-timeline-event-opposite")(
                UI.children(oppositeF.toList.map(f => f(a)))*
            )
            // Custom marker content replaces the default dot; the default marker is
            // an empty div; Prime paints it entirely via ::before/::after.
            val markerEl: UI = markerF match
                case Present(f) => div.cssClass("p-timeline-event-marker")(f(a))
                case Absent     => div.cssClass("p-timeline-event-marker")
            val connectorSlot: List[UI] =
                if i < n - 1 then List(div.cssClass("p-timeline-event-connector")) else Nil
            val separatorEl: UI = div.cssClass("p-timeline-event-separator")(
                UI.children(markerEl :: connectorSlot)*
            )
            val contentEl: UI = div.cssClass("p-timeline-event-content")(
                UI.children(contentF.toList.map(f => f(a)))*
            )
            div.cssClass("p-timeline-event")(oppositeEl, separatorEl, contentEl)
        }
    end eventEls
end Timeline

object Timeline:
    def apply[A](): Timeline[A] = new Timeline[A]()
