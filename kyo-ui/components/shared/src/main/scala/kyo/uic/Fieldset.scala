package kyo.uic

import kyo.*
import kyo.UI.*

/** Fieldset — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Fieldset anatomy: `.p-fieldset.p-component[.p-fieldset-toggleable]` >
  * `.p-fieldset-legend` (plain `span.p-fieldset-legend-label`, or — toggleable —
  * `button.p-fieldset-toggle-button` with the `.p-fieldset-toggle-icon` glyph +
  * label span) + `.p-fieldset-content-container` > `.p-fieldset-content-wrapper`
  * > `.p-fieldset-content`), so the extracted `@primeuix` fieldset CSS applies
  * verbatim.
  *
  * The root is a real `<fieldset>` and the legend a real `<legend>`, so the
  * group's accessible name comes from the legend by STRUCTURE. That matters
  * beyond tidiness: the `role="group"` + `aria-label` pair this used to render
  * could only carry a constant, so a fieldset whose legend was a `Signal[String]`
  * — a locale-driven `I18n.t` leaf, say — announced as an unnamed group. No
  * `role` is set: `<fieldset>` already has `group` implicitly.
  *
  * A fieldset is fixed by default; `toggleable(true)` renders the legend as
  * Prime's toggle button (plus glyph collapsed, minus expanded) whose state
  * binds two-way to the `collapsed` `SignalRef[Boolean]`: pressing the button
  * toggles the ref, external ref writes collapse/expand the content.
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
    collapsedRef: Maybe[SignalRef[Boolean]] = Absent,
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

    /** Binds the collapse state two-way to `ref` (only effective with
      * `toggleable(true)`).
      */
    def collapsed(ref: SignalRef[Boolean]): Fieldset = copy(collapsedRef = Present(ref))

    /** Adds content children. */
    def apply(cs: UI*): Fieldset = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        // The toggle button's own aria-label is an attribute channel, and a channel can only
        // carry a static string — there is no attribute-level reactive patch. So it reads the
        // constant legend, falling back to "Toggle". The GROUP's name does not depend on this:
        // that comes from the <legend> element, which a reactive legend drives as a text node.
        val legendConst: Maybe[String] = legendV match
            case Present(TextValue.Const(v)) => Present(v)
            case _                           => Absent

        var shell = fieldset.cssClass("p-fieldset").cssClass("p-component")
        idV.foreach(v => shell = shell.id(v))
        if toggleableFlag then shell = shell.cssClass("p-fieldset-toggleable")

        val toggleRefV: Maybe[SignalRef[Boolean]] =
            if toggleableFlag then collapsedRef else Absent

        def labelSpan: UI = legendV match
            case Present(TextValue.Const(v)) => span.cssClass("p-fieldset-legend-label")(v)
            case Present(TextValue.Dyn(s))   => span.cssClass("p-fieldset-legend-label")(s)
            case Absent                      => span.cssClass("p-fieldset-legend-label")("")

        val legendUI: List[UI] =
            toggleRefV match
                case Present(ref) =>
                    // The whole button re-renders on toggle so the plus/minus glyph and
                    // aria-expanded track the state (a DIFFERENT element from the content,
                    // so replacing it never resets the content's collapse transition).
                    List(
                        // `UI.legend`, not the bare name: this class has its own `legend` setter,
                        // which shadows the element factory inside it.
                        UI.legend.cssClass("p-fieldset-legend")(
                            toChild(ref.render { c =>
                                button
                                    .cssClass("p-fieldset-toggle-button")
                                    .jsProp("type", "button")
                                    .aria("expanded", (!c).toString)
                                    .aria("label", legendConst.getOrElse("Toggle"))
                                    .onClick(ref.getAndUpdate(!_))(
                                        toChild(GlyphSvg(if c then Icons.plus else Icons.minus, "p-fieldset-toggle-icon")),
                                        toChild(labelSpan)
                                    )
                            })
                        )
                    )
                case Absent if toggleableFlag || legendV.isDefined =>
                    List(UI.legend.cssClass("p-fieldset-legend")(toChild(labelSpan)))
                case Absent => Nil

        val containerBase =
            div.cssClass("p-fieldset-content-container").role("region")(
                div.cssClass("p-fieldset-content-wrapper")(
                    div.cssClass("p-fieldset-content")(kids.map(toChild)*)
                )
            )
        val container: UI = toggleRefV match
            case Present(ref) => ref.render(collapseContainer(_))
            case Absent       => containerBase

        shell((legendUI :+ container).map(toChild)*)
    end render

    /** The animated content container: `containerBase`'s twin carrying
      * `.p-uic-collapse` (the grid-rows transition) with the current collapse state
      * in its class. Reactive on the collapse ref — morph patches `.p-uic-collapsed`
      * onto the same element in place, so the transition fires.
      */
    private def collapseContainer(collapsed: Boolean)(using Frame): UI =
        var c = div.cssClass("p-fieldset-content-container").role("region").cssClass("p-uic-collapse")
        if collapsed then c = c.cssClass("p-uic-collapsed")
        c(
            div.cssClass("p-fieldset-content-wrapper")(
                div.cssClass("p-fieldset-content")(kids.map(toChild)*)
            )
        )
    end collapseContainer
end Fieldset

object Fieldset:
    def apply(): Fieldset = new Fieldset()
