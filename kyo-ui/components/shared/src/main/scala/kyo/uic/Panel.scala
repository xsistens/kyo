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

/** Panel — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Panel
  * anatomy: `div.p-panel[.p-panel-toggleable]` > `div.p-panel-header`
  * (`span.p-panel-title` + `div.p-panel-header-actions` with the
  * `button.p-panel-toggle-button`) + `div.p-panel-content-container` >
  * `div.p-panel-content-wrapper` > `div.p-panel-content` + optional
  * `div.p-panel-footer`), so the extracted `@primeuix` panel CSS applies
  * verbatim.
  *
  * A panel is fixed by default; `toggleable(true)` adds the toggle button
  * (plus icon collapsed, minus expanded — Prime's glyphs) whose state binds
  * two-way to the `collapsed` `SignalRef[Boolean]`: pressing the button toggles
  * the ref, and external ref writes collapse/expand the content.
  *
  * Collapse ANIMATION: the content container is a `.p-uic-collapse` grid,
  * reactive on the `collapsed` ref. Under DOM morphing a class-only re-render
  * patches `.p-uic-collapsed` onto the same element in place (no `outerHTML`
  * swap), so the grid-rows transition plays. The content (and footer) is ALWAYS
  * present; the grid 0fr row hides it when collapsed.
  */
final case class Panel private (
    headerText: Maybe[TextValue] = Absent,
    headerUIV: Maybe[UI] = Absent,
    collapsedRef: Maybe[SignalRef[Boolean]] = Absent,
    toggleableFlag: Boolean = false,
    headerLevelV: TitleLevel = TitleLevel.H2,
    accessibleNameV: Maybe[TextValue] = Absent,
    accessibleRoleV: PanelAccessibleRole = PanelAccessibleRole.Region,
    footerV: Maybe[UI] = Absent,
    kids: List[UI] = Nil
) extends Node:
    type Self = Panel

    def header(v: String): Panel = copy(headerText = Present(TextValue.Const(v)))

    /** Reactive `header` tracking `sig` — re-renders in place on emission, e.g. a
      * locale-driven `I18n.t` leaf.
      */
    def header(sig: Signal[String]): Panel = copy(headerText = Present(TextValue.Dyn(sig)))

    /** Custom header slot (wins over the `header(String)` title). */
    def header(u: UI): Panel = copy(headerUIV = Present(u))

    /** Renders the toggle button (`.p-panel-toggleable`); pair with [[collapsed]]
      * to bind the collapse state. Default false — a plain fixed panel.
      */
    def toggleable(v: Boolean): Panel = copy(toggleableFlag = v)

    /** Binds the collapse state two-way to `ref` (only effective with
      * `toggleable(true)`).
      */
    def collapsed(ref: SignalRef[Boolean]): Panel = copy(collapsedRef = Present(ref))

    /** Heading level of the header title (rendered as `aria-level`, default H2). */
    def headerLevel(v: TitleLevel): Panel = copy(headerLevelV = v)

    /** `aria-label` for the panel region. */
    def accessibleName(v: String): Panel = copy(accessibleNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Panel = copy(accessibleNameV = Present(TextValue.Dyn(sig)))

    /** ARIA role of the panel (default [[PanelAccessibleRole.Region]]). */
    def accessibleRole(v: PanelAccessibleRole): Panel = copy(accessibleRoleV = v)

    /** Footer slot rendered as `div.p-panel-footer` below the content. */
    def footer(v: UI): Panel = copy(footerV = Present(v))

    /** Adds content children. */
    def apply(cs: UI*): Panel = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        var shell = div.cssClass("p-panel").cssClass("p-component").role(accessibleRoleV.roleToken)
        if toggleableFlag then shell = shell.cssClass("p-panel-toggleable")
        accessibleNameV match
            case Present(TextValue.Const(n)) => shell = shell.aria("label", n)
            case Present(TextValue.Dyn(s))   => shell = shell.aria("label", s)
            case Absent                      => ()
        end match

        // The header slot content: a custom UI wins over the plain title text.
        def titleEl(t: String): UI =
            span
                .cssClass("p-panel-title")
                .role("heading")
                .aria("level", headerLevelV.token.drop(1))(t)
        val titleSlot: List[UI] = (headerUIV, headerText) match
            case (Present(u), _)                       => List(span.cssClass("p-panel-title")(toChild(u)))
            case (Absent, Present(TextValue.Const(t))) => List(titleEl(t))
            case (Absent, Present(TextValue.Dyn(s)))   => List(s.render(titleEl))
            case _                                     => Nil

        val toggleRefV: Maybe[SignalRef[Boolean]] =
            if toggleableFlag then collapsedRef else Absent

        val actions: List[UI] = toggleRefV match
            case Present(ref) =>
                // The whole button re-renders on toggle so the plus/minus glyph and
                // aria-expanded track the state (a DIFFERENT element from the content,
                // so replacing it never resets the content's collapse transition).
                List(
                    div.cssClass("p-panel-header-actions")(
                        toChild(ref.render { c =>
                            button
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
                                .onClick(ref.getAndUpdate(!_))(
                                    toChild(GlyphSvg(if c then Icons.plus else Icons.minus, "p-button-icon"))
                                )
                        })
                    )
                )
            case Absent if toggleableFlag =>
                List(div.cssClass("p-panel-header-actions")())
            case Absent => Nil

        val headerUI: List[UI] =
            if titleSlot.isEmpty && actions.isEmpty then Nil
            else List(div.cssClass("p-panel-header")((titleSlot ++ actions).map(toChild)*))

        val footerUI: List[UI] = footerV.toList.map(f => div.cssClass("p-panel-footer")(toChild(f)))
        val contentInner: List[UI] =
            (div.cssClass("p-panel-content")(kids.map(toChild)*): UI) :: footerUI
        val containerBase =
            div.cssClass("p-panel-content-container")(
                div.cssClass("p-panel-content-wrapper")(contentInner.map(toChild)*)
            )
        val container: UI = toggleRefV match
            case Present(ref) => ref.render(collapseContainer(_, contentInner))
            case Absent       => containerBase

        shell((headerUI :+ container).map(toChild)*)
    end render

    /** The animated content container: `containerBase`'s twin carrying
      * `.p-uic-collapse` (the grid-rows transition) with the current collapse state
      * in its class. Reactive on the collapse ref — morph patches `.p-uic-collapsed`
      * onto the same element in place, so the transition fires.
      */
    private def collapseContainer(collapsed: Boolean, inner: List[UI])(using Frame): UI =
        var c = div.cssClass("p-panel-content-container").cssClass("p-uic-collapse")
        if collapsed then c = c.cssClass("p-uic-collapsed")
        c(div.cssClass("p-panel-content-wrapper")(inner.map(toChild)*))
    end collapseContainer
end Panel

object Panel:
    def apply(): Panel = new Panel()
