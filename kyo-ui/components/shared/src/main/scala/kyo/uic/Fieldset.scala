package kyo.uic

import kyo.*
import kyo.UI.*

/** Fieldset: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Fieldset anatomy: `.p-fieldset.p-component[.p-fieldset-toggleable]` >
  * `.p-fieldset-legend` (plain `span.p-fieldset-legend-label`, or, when toggleable,
  * `button.p-fieldset-toggle-button` with the `.p-fieldset-toggle-icon` glyph +
  * label span) + `.p-fieldset-content-container` > `.p-fieldset-content-wrapper`
  * > `.p-fieldset-content`), so the extracted `@primeuix` fieldset CSS applies
  * verbatim.
  *
  * The root is a real `<fieldset>` and the legend a real `<legend>`, so the
  * group's accessible name comes from the legend by STRUCTURE. That matters
  * beyond tidiness: the `role="group"` + `aria-label` pair this used to render
  * could only carry a constant, so a fieldset whose legend was a `Signal[String]`
  * (a locale-driven `I18n.t` leaf, say) announced as an unnamed group. No
  * `role` is set: `<fieldset>` already has `group` implicitly.
  *
  * A fieldset is fixed by default; `toggleable(true)` renders the legend as
  * Prime's toggle button (plus glyph collapsed, minus expanded). The fieldset
  * keeps the collapse state itself unless [[collapsed]] binds it, and reports
  * every press to [[onCollapsedChange]].
  *
  * Collapse ANIMATION: the content container is a `.p-uic-collapse` grid,
  * reactive on the `collapsed` ref. Under DOM morphing a class-only re-render
  * patches `.p-uic-collapsed` onto the same element in place (no `outerHTML`
  * swap), so the grid-rows transition plays. The content is ALWAYS present; the
  * grid 0fr row hides it when collapsed.
  */
final case class Fieldset private (
    legendV: Maybe[TextValue] = Absent,
    toggleableFlag: Boolean = false,
    collapsedSlot: Slot[Boolean] = Slot(),
    idV: Maybe[String] = Absent,
    kids: List[UI] = Nil
) extends Node, HasElementId:
    type Self = Fieldset

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): Fieldset = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Legend text, rendered inside `.p-fieldset-legend`. A `Signal[String]` re-renders it in place on
      * emission, e.g. a locale-driven `I18n.t` leaf.
      */
    def legend(v: String | Signal[String]): Fieldset = copy(legendV = Present(ReactiveValue(v)))

    /** Renders the legend as the toggle button (`.p-fieldset-toggleable`); pair
      * with [[collapsed]] to bind the collapse state. Default false.
      */
    def toggleable(v: Boolean): Fieldset = copy(toggleableFlag = v)

    /** Binds the collapse state (only effective with `toggleable(true)`). A `SignalRef` binds two way: a press of the
      * toggle writes it, then [[onCollapsedChange]] hears the new state. A plain `Signal` binds one way: the fieldset
      * follows it and only reports a press. A constant nobody hears is fixed: the fieldset keeps it and the toggle,
      * still reachable, changes nothing. Unbound, a toggleable fieldset keeps the state itself, starting expanded. A
      * signal nothing hears is reported in a card.
      */
    def collapsed(v: Boolean | Signal[Boolean]): Fieldset = copy(collapsedSlot = collapsedSlot.bind(v))

    /** Fired with the new collapse state after a press of the toggle, after a two-way [[collapsed]] is written. */
    def onCollapsedChange(f: Boolean => UI.Action)(using Frame): Fieldset = copy(collapsedSlot = collapsedSlot.hear(f))

    /** Adds content children. */
    def apply(cs: UI*): Fieldset = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        // Unbound, a toggleable fieldset keeps its collapse state itself, starting expanded.
        Slot.draw(collapsedSlot, Fieldset.collapsedName, start = false, on = ReactiveValue.Const(toggleableFlag))(s =>
            copy(collapsedSlot = s).body
        )

    private def body(using Frame): UI =
        // The toggle button's own aria-label is an attribute channel, and a channel can only
        // carry a static string: there is no attribute-level reactive patch. So it reads the
        // constant legend, falling back to "Toggle". The GROUP's name does not depend on this:
        // that comes from the <legend> element, which a reactive legend drives as a text node.
        val legendConst: Maybe[String] = legendV match
            case Present(TextValue.Const(v)) => Present(v)
            case _                           => Absent

        var shell = fieldset.cssClass("p-fieldset").cssClass("p-component")
        idV.foreach(v => shell = shell.id(v))
        if toggleableFlag then shell = shell.cssClass("p-fieldset-toggleable")

        val toggleSlot: Maybe[ReactiveValue[Boolean]] = if toggleableFlag then collapsedSlot.binding else Absent
        // A constant nobody hears is fixed: the toggle stays a tab stop and says it changes nothing.
        val fixed = collapsedSlot.fixed

        def labelSpan: UI = legendV match
            case Present(TextValue.Const(v)) => span.cssClass("p-fieldset-legend-label")(v)
            case Present(TextValue.Dyn(s))   => span.cssClass("p-fieldset-legend-label")(s)
            case Absent                      => span.cssClass("p-fieldset-legend-label")("")

        val legendUI: List[UI] =
            toggleSlot match
                case Present(slot) =>
                    // The whole button re-renders on toggle so the plus/minus glyph and
                    // aria-expanded track the state (a DIFFERENT element from the content,
                    // so replacing it never resets the content's collapse transition).
                    List(
                        // `UI.legend`, not the bare name: this class has its own `legend` setter,
                        // which shadows the element factory inside it.
                        UI.legend.cssClass("p-fieldset-legend")(
                            Reading.of(slot).render { c =>
                                val b = button
                                    .cssClass("p-fieldset-toggle-button")
                                    .jsProp("type", "button")
                                    .aria("expanded", (!c).toString)
                                    .aria("label", legendConst.getOrElse("Toggle"))
                                val pressable =
                                    if fixed then b.aria("disabled", "true")
                                    else b.onClick(collapsedSlot.update(!_))
                                pressable(
                                    GlyphSvg(if c then Icons.plus else Icons.minus, "p-fieldset-toggle-icon"),
                                    labelSpan
                                )
                            }
                        )
                    )
                case Absent if legendV.isDefined =>
                    List(UI.legend.cssClass("p-fieldset-legend")(labelSpan))
                case Absent => Nil

        val containerBase =
            div.cssClass("p-fieldset-content-container").role("region")(
                div.cssClass("p-fieldset-content-wrapper")(
                    div.cssClass("p-fieldset-content")(UI.children(kids)*)
                )
            )
        val container: UI = toggleSlot match
            case Present(slot) => Reading.of(slot).render(collapseContainer(_))
            case Absent        => containerBase

        shell(UI.children(legendUI :+ container)*)
    end body

    /** The animated content container: `containerBase`'s twin carrying
      * `.p-uic-collapse` (the grid-rows transition) with the current collapse state
      * in its class. Reactive on the collapse ref: morph patches `.p-uic-collapsed`
      * onto the same element in place, so the transition fires.
      */
    private def collapseContainer(collapsed: Boolean)(using Frame): UI =
        var c = div.cssClass("p-fieldset-content-container").role("region").cssClass("p-uic-collapse")
        if collapsed then c = c.cssClass("p-uic-collapsed")
        c(
            div.cssClass("p-fieldset-content-wrapper")(
                div.cssClass("p-fieldset-content")(UI.children(kids)*)
            )
        )
    end collapseContainer
end Fieldset

object Fieldset:
    def apply(): Fieldset = new Fieldset()

    private val collapsedName = Slot.Name("Fieldset", "the collapse state", "a press", "onCollapsedChange")
end Fieldset
