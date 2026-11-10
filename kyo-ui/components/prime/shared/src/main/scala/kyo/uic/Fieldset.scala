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
final class Fieldset private[uic] (private[uic] val p: Fieldset.Props) extends Node, HasElementId, HasProps:
    import p.*

    type Self = Fieldset

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new Fieldset(p.copy(access = v))

    /** Legend text, rendered inside `.p-fieldset-legend`. A `Signal[String]` re-renders it in place on
      * emission, e.g. a locale-driven `I18n.t` leaf.
      */
    def legend(v: String | Signal[String]): Fieldset = new Fieldset(p.copy(legendV = Present(ReactiveValue(v))))

    /** Renders the legend as the toggle button (`.p-fieldset-toggleable`). Set, it decides alone, and a signal switches the
      * button and the collapse in place. Without it a binding of [[collapsed]] turns them on, and an unbound fieldset is plain.
      *
      * On and unbound, the fieldset keeps the collapse state itself. Under a signal it holds the state while the switch is off,
      * when the fieldset is plain and its content shows, and comes back to it.
      */
    def toggleable(v: Boolean | Signal[Boolean]): Fieldset = new Fieldset(p.copy(toggleableV = Present(ReactiveValue(v))))

    /** Binds the collapse state, which turns the toggle on unless [[toggleable]] says otherwise. A `SignalRef` binds two way: a press of the
      * toggle writes it, then [[onCollapsedChange]] hears the new state. A plain `Signal` binds one way: the fieldset
      * follows it and only reports a press. A constant nobody hears is fixed: the fieldset keeps it and the toggle,
      * still reachable, changes nothing. Unbound, a toggleable fieldset keeps the state itself, starting expanded. A
      * signal nothing hears is reported in a card.
      */
    def collapsed(v: Boolean | Signal[Boolean]): Fieldset = new Fieldset(p.copy(collapsedSlot = collapsedSlot.bind(v)))

    /** Fired with the new collapse state after a press of the toggle, after a two-way [[collapsed]] is written. */
    def onCollapsedChange(f: Boolean => UI.Action)(using Frame): Fieldset = new Fieldset(p.copy(collapsedSlot = collapsedSlot.hear(f)))

    /** Adds content children. */
    def apply(cs: UI*): Fieldset = new Fieldset(p.copy(kids = kids ++ cs))

    private[uic] def render(using Frame): UI =
        // Unbound, a toggleable fieldset keeps its collapse state itself, starting expanded.
        Controlled.draw(collapsedSlot, Fieldset.collapsedName, start = false, on = toggleOn)(s =>
            new Fieldset(this.p.copy(collapsedSlot = s)).body
        )

    /** The switch as this fieldset reads it: set, it decides alone; unset, a binding of [[collapsed]] turns the toggle on. */
    private def toggleOn: BoolValue = toggleableV.getOrElse(ReactiveValue.Const(collapsedSlot.binding.isDefined))

    private def body(using Frame): UI =
        // The toggle button's own aria-label is an attribute channel, and a channel can only
        // carry a static string: there is no attribute-level reactive patch. So it reads the
        // constant legend, falling back to "Toggle". The GROUP's name does not depend on this:
        // that comes from the <legend> element, which a reactive legend drives as a text node.
        val legendConst: Maybe[String] = legendV match
            case Present(TextValue.Const(v)) => Present(v)
            case _                           => Absent

        var shell = fieldset.cssClass("p-fieldset").cssClass("p-component")
        access.id.foreach(v => shell = shell.id(v))
        val on = toggleOn
        on match
            case ReactiveValue.Const(b) => if b then shell = shell.cssClass("p-fieldset-toggleable")
            case ReactiveValue.Dyn(sig) => shell = shell.when(sig)(_.cssClass("p-fieldset-toggleable"))
        // A constant nobody hears is fixed: the toggle stays a tab stop and says it changes nothing.
        val fixed = collapsedSlot.fixed

        def labelSpan: UI = legendV match
            case Present(TextValue.Const(v)) => span.cssClass("p-fieldset-legend-label")(v)
            case Present(TextValue.Dyn(s))   => span.cssClass("p-fieldset-legend-label")(s)
            case Absent                      => span.cssClass("p-fieldset-legend-label")("")

        // The whole button re-renders on toggle so the plus/minus glyph and aria-expanded track the state (a DIFFERENT element
        // from the content, so replacing it never resets the content's collapse transition).
        def toggleButton(c: Boolean): UI =
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
        end toggleButton

        val collapse = collapsedSlot.reading(false)
        // `UI.legend`, not the bare name: this class has its own `legend` setter, which shadows the element factory inside it.
        val legendUI: List[UI] =
            on match
                case ReactiveValue.Const(true) => List(UI.legend.cssClass("p-fieldset-legend")(collapse.render(toggleButton)))
                case ReactiveValue.Dyn(sig)    =>
                    // The switch and the collapse are read as ONE region: off, the legend is the plain label.
                    List(UI.legend.cssClass("p-fieldset-legend")(
                        ReactiveValue.Dyn(sig).and(collapse).render((isOn, c) => if isOn then toggleButton(c) else labelSpan)
                    ))
                case _ if legendV.isDefined => List(UI.legend.cssClass("p-fieldset-legend")(labelSpan))
                case _                      => Nil

        val containerBase =
            div.cssClass("p-fieldset-content-container").role("region")(
                div.cssClass("p-fieldset-content-wrapper")(
                    div.cssClass("p-fieldset-content")(UI.children(kids)*)
                )
            )
        val container: UI = on match
            case ReactiveValue.Const(false) => containerBase
            case ReactiveValue.Const(true)  => collapse.render(collapseContainer(_))
            case ReactiveValue.Dyn(sig)     =>
                // Off the fieldset is plain and its content shows; the collapse state is held, not applied.
                ReactiveValue.Dyn(sig).and(collapse).render((isOn, c) => if isOn then collapseContainer(c) else containerBase)

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
    final private[uic] case class Props(
        legendV: Maybe[TextValue] = Absent,
        toggleableV: Maybe[BoolValue] = Absent,
        collapsedSlot: Controlled[Boolean] = Controlled(),
        access: Accessibility = Accessibility.none,
        kids: List[UI] = Nil
    )

    def apply(): Fieldset = new Fieldset(Props())

    private val collapsedName = Controlled.Name("Fieldset", "the collapse state", "a press", "onCollapsedChange")
end Fieldset
