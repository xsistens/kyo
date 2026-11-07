package kyo.uic

import kyo.*
import kyo.UI.*

/** ARIA role of a [[Panel]] region. */
enum PanelAccessibleRole derives CanEqual:
    case Form, Region, Complementary

    /** The rendered `role` attribute value. */
    private[uic] def roleToken: String = this match
        case Form          => "form"
        case Region        => "region"
        case Complementary => "complementary"
end PanelAccessibleRole

/** Panel: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Panel
  * anatomy: `div.p-panel[.p-panel-toggleable]` > `div.p-panel-header`
  * (`span.p-panel-title` + `div.p-panel-header-actions` with the
  * `button.p-panel-toggle-button`) + `div.p-panel-content-container` >
  * `div.p-panel-content-wrapper` > `div.p-panel-content` + optional
  * `div.p-panel-footer`), so the extracted `@primeuix` panel CSS applies
  * verbatim.
  *
  * A panel is fixed by default; `toggleable(true)` adds the toggle button
  * (plus icon collapsed, minus expanded, Prime's glyphs). The panel keeps the
  * collapse state itself unless [[collapsed]] binds it, and reports every press
  * to [[onCollapsedChange]].
  *
  * `id(...)` lands on the panel root, and the toggle takes `s"$id-toggle"`. That
  * derivation is the only way to address the button: it belongs to the panel, so
  * wrapping the panel in a `div` with an id does not reach it: the wrapper is
  * not that button.
  *
  * Collapse ANIMATION: the content container is a `.p-uic-collapse` grid,
  * reactive on the `collapsed` ref. Under DOM morphing a class-only re-render
  * patches `.p-uic-collapsed` onto the same element in place (no `outerHTML`
  * swap), so the grid-rows transition plays. The content (and footer) is ALWAYS
  * present; the grid 0fr row hides it when collapsed.
  */
final class Panel private[uic] (private[uic] val p: Panel.Props) extends Node, HasElementId, HasAccessibleName, HasProps:
    import p.*

    type Self = Panel

    /** Header title. A `Signal[String]` re-renders it in place on emission, e.g. a
      * locale-driven `I18n.t` leaf.
      */
    def header(v: String | Signal[String]): Panel = new Panel(p.copy(headerText = Present(ReactiveValue(v))))

    /** Custom header slot (wins over the `header(String)` title). */
    def header(u: UI): Panel = new Panel(p.copy(headerUIV = Present(u)))

    /** Renders the toggle button (`.p-panel-toggleable`). Set, it decides alone, and a signal switches the button and the
      * collapse in place. Without it a binding of [[collapsed]] turns them on, and an unbound panel is plain.
      *
      * On and unbound, the panel keeps the collapse state itself. Under a signal it holds the state while the switch is off,
      * when the panel is plain and its content shows, and comes back to it.
      */
    def toggleable(v: Boolean | Signal[Boolean]): Panel = new Panel(p.copy(toggleableV = Present(ReactiveValue(v))))

    /** Binds the collapse state, which turns the toggle on unless [[toggleable]] says otherwise. A `SignalRef` binds two way: a press of the
      * toggle writes it, then [[onCollapsedChange]] hears the new state. A plain `Signal` binds one way: the panel
      * follows it and only reports a press. A constant nobody hears is fixed: the panel keeps it and the toggle,
      * still reachable, changes nothing. Unbound, a toggleable panel keeps the state itself, starting expanded. A
      * signal nothing hears is reported in a card.
      */
    def collapsed(v: Boolean | Signal[Boolean]): Panel = new Panel(p.copy(collapsedSlot = collapsedSlot.bind(v)))

    /** Fired with the new collapse state after a press of the toggle, after a two-way [[collapsed]] is written. */
    def onCollapsedChange(f: Boolean => UI.Action)(using Frame): Panel = new Panel(p.copy(collapsedSlot = collapsedSlot.hear(f)))

    /** Heading level of the header title (rendered as `aria-level`, default H2). */
    def headerLevel(v: TitleLevel): Panel = new Panel(p.copy(headerLevelV = v))

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new Panel(p.copy(access = v))

    /** ARIA role of the panel (default [[PanelAccessibleRole.Region]]). */
    def accessibleRole(v: PanelAccessibleRole): Panel = new Panel(p.copy(accessibleRoleV = v))

    /** Footer slot rendered as `div.p-panel-footer` below the content. */
    def footer(v: UI): Panel = new Panel(p.copy(footerV = Present(v)))

    /** Adds content children. */
    def apply(cs: UI*): Panel = new Panel(p.copy(kids = kids ++ cs))

    private[uic] def render(using Frame): UI =
        // Unbound, a toggleable panel keeps its collapse state itself, starting expanded.
        Slot.draw(collapsedSlot, Panel.collapsedName, start = false, on = toggleOn)(s => new Panel(this.p.copy(collapsedSlot = s)).body)

    /** The switch as this panel reads it: set, it decides alone; unset, a binding of [[collapsed]] turns the toggle on. */
    private def toggleOn: BoolValue = toggleableV.getOrElse(ReactiveValue.Const(collapsedSlot.binding.isDefined))

    private def body(using Frame): UI =
        var shell = div.cssClass("p-panel").cssClass("p-component").role(accessibleRoleV.roleToken)
        access.id.foreach(v => shell = shell.id(v))
        val on = toggleOn
        on match
            case ReactiveValue.Const(b) => if b then shell = shell.cssClass("p-panel-toggleable")
            case ReactiveValue.Dyn(sig) => shell = shell.cssClass("p-panel-toggleable", sig)
        access.name.foreach(n => shell = shell.aria("label", n))

        // The header slot content: a custom UI wins over the plain title text.
        // The shell without its child, so a signal title can go in as a text CHILD (kyo-ui's
        // text channel patches it in place) instead of a region rebuilding the heading span.
        def titleShell =
            span
                .cssClass("p-panel-title")
                .role("heading")
                .aria("level", headerLevelV.token.drop(1))
        val titleSlot: List[UI] = (headerUIV, headerText) match
            case (Present(u), _)                       => List(span.cssClass("p-panel-title")(u))
            case (Absent, Present(TextValue.Const(t))) => List(titleShell(t))
            case (Absent, Present(TextValue.Dyn(s)))   => List(titleShell(s))
            case _                                     => Nil

        // A constant nobody hears is fixed: the toggle stays a tab stop and says it changes nothing.
        val fixed = collapsedSlot.fixed

        // The whole button re-renders on toggle so the plus/minus glyph and aria-expanded track the state (a DIFFERENT element
        // from the content, so replacing it never resets the content's collapse transition).
        def toggleButton(c: Boolean): UI =
            var b = button
                .cssClass("p-panel-toggle-button")
                .cssClass("p-button")
                .cssClass("p-component")
                .cssClass("p-button-icon-only")
                .cssClass("p-button-rounded")
                .cssClass("p-button-text")
                .cssClass("p-button-secondary")
                .jsProp("type", "button")
                .aria("expanded", (!c).toString)
                .aria("label", "Toggle")
            b =
                if fixed then b.aria("disabled", "true")
                else b.onClick(collapsedSlot.update(!_))
            // The toggle is Panel's own button, so it is not reachable by
            // wrapping the panel: the wrapper is not that button. It takes
            // its id from the panel's, per the derivation HasElementId
            // describes.
            access.id.foreach(base => b = b.id(s"$base-toggle"))
            b(GlyphSvg(if c then Icons.plus else Icons.minus, "p-button-icon"))
        end toggleButton

        val collapse          = collapsedSlot.reading(false)
        val actions: List[UI] = on match
            case ReactiveValue.Const(false) => Nil
            case ReactiveValue.Const(true)  => List(div.cssClass("p-panel-header-actions")(collapse.render(toggleButton)))
            case ReactiveValue.Dyn(sig)     =>
                // The switch and the collapse are read as ONE region: the actions come and go with the switch.
                List(ReactiveValue.Dyn(sig).and(collapse).render((isOn, c) =>
                    if isOn then div.cssClass("p-panel-header-actions")(toggleButton(c)) else UI.empty
                ))

        val headerUI: List[UI] =
            if titleSlot.isEmpty && actions.isEmpty then Nil
            else List(div.cssClass("p-panel-header")(UI.children(titleSlot ++ actions)*))

        val footerUI: List[UI]     = footerV.toList.map(f => div.cssClass("p-panel-footer")(f))
        val contentInner: List[UI] =
            (div.cssClass("p-panel-content")(UI.children(kids)*): UI) :: footerUI
        val containerBase =
            div.cssClass("p-panel-content-container")(
                div.cssClass("p-panel-content-wrapper")(UI.children(contentInner)*)
            )
        val container: UI = on match
            case ReactiveValue.Const(false) => containerBase
            case ReactiveValue.Const(true)  => collapse.render(collapseContainer(_, contentInner))
            case ReactiveValue.Dyn(sig)     =>
                // Off the panel is plain and its content shows; the collapse state is held, not applied.
                ReactiveValue.Dyn(sig).and(collapse).render((isOn, c) => if isOn then collapseContainer(c, contentInner) else containerBase)

        shell(UI.children(headerUI :+ container)*)
    end body

    /** The animated content container: `containerBase`'s twin carrying
      * `.p-uic-collapse` (the grid-rows transition) with the current collapse state
      * in its class. Reactive on the collapse ref: morph patches `.p-uic-collapsed`
      * onto the same element in place, so the transition fires.
      */
    private def collapseContainer(collapsed: Boolean, inner: List[UI])(using Frame): UI =
        var c = div.cssClass("p-panel-content-container").cssClass("p-uic-collapse")
        if collapsed then c = c.cssClass("p-uic-collapsed")
        c(div.cssClass("p-panel-content-wrapper")(UI.children(inner)*))
    end collapseContainer
end Panel

object Panel:
    final private[uic] case class Props(
        headerText: Maybe[TextValue] = Absent,
        headerUIV: Maybe[UI] = Absent,
        collapsedSlot: Slot[Boolean] = Slot(),
        toggleableV: Maybe[BoolValue] = Absent,
        headerLevelV: TitleLevel = TitleLevel.H2,
        access: Accessibility = Accessibility.none,
        accessibleRoleV: PanelAccessibleRole = PanelAccessibleRole.Region,
        footerV: Maybe[UI] = Absent,
        kids: List[UI] = Nil
    )

    def apply(): Panel = new Panel(Props())

    private val collapsedName = Slot.Name("Panel", "the collapse state", "a press", "onCollapsedChange")
end Panel
