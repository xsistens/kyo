package kyo.uic

import kyo.*
import kyo.UI.*

/** Button form types: plain button, form submit, or form reset. Maps onto the
  * native `type` attribute. The default, `Button`, does not submit, unlike a bare HTML button.
  */
enum ButtonType derives CanEqual:
    case Button, Submit, Reset
end ButtonType

/** Button accessible roles (`role` attribute override). */
enum ButtonAccessibleRole derives CanEqual:
    case Button, Link

end ButtonAccessibleRole

/** Button: native kyo-ui, PrimeOne design. Renders a real `<button>` with
  * Prime's own anatomy (`.p-button` > `.p-button-icon`/`.p-button-label`), so
  * the extracted `@primeuix` component CSS applies verbatim.
  *
  * Severity defaults to `Primary` (Prime's default button IS the primary
  * action); `variant` switches Filled/Outlined/Text/Link; while `loading` the
  * button shows a spinning glyph, is marked `aria-disabled` and ignores its
  * clicks, while staying in the tab order.
  */
final case class Button private (
    label: Maybe[TextValue] = Absent,
    severityV: SeverityValue = SeverityValue.Const(Severity.Primary),
    variantV: ButtonVariant = ButtonVariant.Filled,
    sizeV: Size = Size.Normal,
    roundedFlag: Boolean = false,
    raisedFlag: Boolean = false,
    fluidFlag: Boolean = false,
    icon: Maybe[IconGlyph] = Absent,
    endIconV: Maybe[IconGlyph] = Absent,
    idV: Maybe[String] = Absent,
    hrefV: Maybe[String] = Absent,
    disabledV: Maybe[BoolValue] = Absent,
    ariaDisabledV: Maybe[BoolValue] = Absent,
    loadingV: Maybe[BoolValue] = Absent,
    buttonTypeV: ButtonType = ButtonType.Button,
    accessibleRoleV: ButtonAccessibleRole = ButtonAccessibleRole.Button,
    accessibleNameV: Maybe[TextValue] = Absent,
    accessibleNameRefV: Maybe[String] = Absent,
    accessibleDescriptionV: Maybe[TextValue] = Absent,
    tooltipV: Maybe[TextValue] = Absent,
    formV: Maybe[String] = Absent,
    onClickEff: Maybe[Any < (Abort[Throwable] & Async)] = Absent,
    onKeyDownF: Maybe[KeyboardEvent => Any < (Abort[Throwable] & Async)] = Absent,
    extraChildren: List[UI] = Nil,
    extraClassesV: List[String] = Nil,
    extraAriasV: List[(String, String)] = Nil,
    roleRawV: Maybe[String] = Absent,
    tabIndexV: Maybe[Int] = Absent,
    focusSeedV: Boolean = false
) extends Node, HasElementId, HasTooltip, HasAccessibleNameRef, HasAccessibleDescription:
    type Self = Button

    /** Package-internal class hook: hosts (SplitButton, SpeedDial) stamp Prime's
      * contextual classes (`p-splitbutton-button`, `p-speeddial-button`) onto the
      * button element.
      */
    private[uic] def extraClass(cls: String): Button = copy(extraClassesV = extraClassesV :+ cls)

    /** Package-internal aria hook: hosts add popup semantics (`aria-haspopup`,
      * `aria-expanded`) not part of Button's own public surface.
      */
    private[uic] def ariaRaw(name: String, value: String): Button =
        copy(extraAriasV = extraAriasV :+ (name -> value))

    /** Package-internal composite-widget hooks: the attributes a button needs when it is a CHILD
      * of a composite widget rather than a control of its own. A standalone button decides none
      * of these (it is its own tab stop, it is a button, and the browser gives it Enter and
      * Space), so they stay off the public surface and belong to the host that owns the widget:
      * SpeedDial's fan is one `role="menu"` whose actions rove a single tab stop, are announced
      * as `menuitem`, answer arrows, and take the focus the fan seeds when it opens.
      */
    private[uic] def roleRaw(v: String): Button       = copy(roleRawV = Present(v))
    private[uic] def tabIndexRaw(v: Int): Button      = copy(tabIndexV = Present(v))
    private[uic] def focusSeedRaw(v: Boolean): Button = copy(focusSeedV = v)

    private[uic] def onKeyDownRaw(f: KeyboardEvent => Any < (Abort[Throwable] & Async)): Button =
        copy(onKeyDownF = Present(f))

    /** Semantic accent (`.p-button-<severity>`); `Primary` is the unsuffixed default. A
      * `Signal[Severity]` swaps the `.p-button-<token>` class IN PLACE via kyo-ui's class channel on
      * emission (no re-render), e.g. a save button going neutral to success.
      */
    def severity(v: Severity | Signal[Severity]): Button = copy(severityV = ReactiveValue(v))

    /** Rendering variant: `Filled` (default), `Outlined`, `Text`, or `Link`. */
    def variant(v: ButtonVariant): Button = copy(variantV = v)

    /** Size: `Small` / `Normal` (default) / `Large`. */
    def size(v: Size): Button = copy(sizeV = v)

    /** Fully rounded corners (`.p-button-rounded`). */
    def rounded(v: Boolean): Button = copy(roundedFlag = v)

    /** Adds a shadow to indicate elevation (`.p-button-raised`). */
    def raised(v: Boolean): Button = copy(raisedFlag = v)

    /** Spans the full width of its container (`.p-button-fluid`). */
    def fluid(v: Boolean): Button = copy(fluidFlag = v)

    def icon(glyph: IconGlyph): Button = copy(icon = Present(glyph))

    /** Icon rendered after the label. */
    def endIcon(glyph: IconGlyph): Button = copy(endIconV = Present(glyph))

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): Button = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Disables the button. A `Signal[Boolean]` enables/disables it in place on emission (e.g. a
      * mutation-in-flight signal).
      */
    def disabled(v: Boolean | Signal[Boolean]): Button = copy(disabledV = Present(ReactiveValue(v)))

    /** Accessible-disabled: unlike the native `disabled` attribute (which removes the button
      * from the tab order and drops its click), this keeps the button focusable and IN the tab
      * sequence, marking it `aria-disabled="true"` for assistive tech plus a greyed style, and
      * keeps the click wired. Route that click through `form.submit`, which validates and moves
      * focus to the first invalid field. This is the fix for the focus trap where a natively
      * `disabled` submit button is skipped by Tab: the reactive re-enable lands a tick after the
      * Tab keypress, so focus jumps PAST the button to the next control. With `aria-disabled` the
      * button never leaves the tab order, so the race disappears. On a link (`href`) it drops the
      * `href` and leaves the tab order (`tabindex="-1"`), the way a disabled link does.
      */
    def ariaDisabled(v: Boolean | Signal[Boolean]): Button = copy(ariaDisabledV = Present(ReactiveValue(v)))

    /** Binds a form's submit gate. [[SubmitGate]] is a distinct type from `Signal[Boolean]`
      * precisely so it fits HERE and not on [[disabled]]: a natively disabled submit button
      * leaves the tab order and the re-enable lands a tick late, which is the focus trap
      * [[ariaDisabled]] exists to avoid. `uic.Button("Save").ariaDisabled(gate)` is therefore
      * the whole binding, with no accessibility decision left to make.
      */
    def ariaDisabled(gate: SubmitGate): Button = ariaDisabled(gate.signal)

    /** Busy state: shows a spinning glyph, marks the button `aria-disabled` and blocks its clicks. Unlike
      * a disabled button it stays in the tab order, so focus does not jump past it while a request is in
      * flight. Bind a `Signal[Boolean]` to an in-flight request; the button re-renders its own boundary on
      * emission, because loading is fused with the spinner, aria-busy, icon-only layout and aria-disabled
      * state rather than patching a single attribute.
      */
    def loading(v: Boolean | Signal[Boolean]): Button = copy(loadingV = Present(ReactiveValue(v)))

    /** Form behaviour (native `type` attribute): `Button` (default, non-submitting),
      * `Submit`, or `Reset`.
      */
    def buttonType(v: ButtonType): Button = copy(buttonTypeV = v)

    /** ARIA role override: `Link` renders `role="link"` on the native button. */
    def accessibleRole(v: ButtonAccessibleRole): Button = copy(accessibleRoleV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): Button = copy(accessibleNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): Button = copy(accessibleNameRefV = v)

    private[uic] def withAccessibleDescription(v: Maybe[TextValue]): Button = copy(accessibleDescriptionV = v)

    /** Renders the button as a client-routed navigation anchor (`<a>` with kyo-ui's
      * SPA `Href.Path`) that keeps the full button skin, Prime's Button link mode.
      * Link semantics (native anchor role, focus) with button styling; form `type`
      * does not apply.
      */
    def href(v: String): Button = copy(hrefV = Present(v))

    private[uic] def withTooltip(v: Maybe[TextValue]): Button = copy(tooltipV = v)

    /** ID of the `<form>` the button submits or resets, rendered as the native `form` attribute. The button
      * may sit outside that form's markup and the browser still associates it. Has no effect in link mode.
      */
    def form(v: String): Button = copy(formV = Present(v))

    /** Runs `action` when the button is pressed. Mirrors kyo-ui's native event effect
      * (`Abort[Throwable] & Async`), so a handler may fail with a domain error that
      * bubbles to the enclosing mount's error policy; pure `Async` handlers still fit.
      */
    def onClick(action: => Any < (Abort[Throwable] & Async))(using Frame): Button =
        copy(onClickEff = Present(Sync.defer(action)))

    /** Adds arbitrary extra children (e.g. a custom badge) after the label/icon. */
    def apply(cs: UI*): Button = copy(extraChildren = extraChildren ++ cs)

    // Resolve any reactive (Dyn) disabled / aria-disabled slots to their current boolean first.
    // Each re-renders its own boundary on emission (like TextValue.Dyn), then the element is built.
    private[uic] def render(using Frame): UI =
        if isDyn(disabledV) || isDyn(ariaDisabledV) || isDyn(loadingV) then
            BoolValue.reactive(disabledV): d =>
                BoolValue.reactive(ariaDisabledV): ad =>
                    BoolValue.reactive(loadingV): l =>
                        copy(disabledV = d, ariaDisabledV = ad, loadingV = l).renderResolved
        else renderResolved

    /** A reactive boolean slot: only these need the resolving copy in `render`. */
    private def isDyn(bv: Maybe[BoolValue]): Boolean = bv match
        case Present(BoolValue.Dyn(_)) => true
        case _                         => false

    /** The resolved busy state, read after the reactive slots are pinned to their current constants. */
    private def loadingNow: Boolean  = loadingV.constTrue
    private def disabledNow: Boolean = disabledV.constTrue
    private def effDisabled: Boolean = loadingNow || disabledNow

    private def renderResolved(using Frame): UI =
        hrefV match
            case Present(h) => renderAnchor(h)
            case Absent     => renderButton

    /** No label at all: absent, or a constant empty string. A reactive label is judged in `shell`. */
    private def labelEmpty: Boolean = label match
        case Absent                      => true
        case Present(TextValue.Const(t)) => t.isEmpty
        case Present(TextValue.Dyn(_))   => false

    /** Carries an icon (or the spinner) and no children, so the label alone decides icon-only. */
    private def iconsOnly: Boolean =
        extraChildren.isEmpty && (icon.isDefined || endIconV.isDefined || loadingNow)

    private def iconOnly: Boolean = labelEmpty && iconsOnly

    /** The `.p-button` class chain, shared verbatim by the button and anchor renderings. */
    private def buttonClasses: List[String] =
        val bs = List.newBuilder[String]
        bs += "p-button"
        bs += "p-component"
        extraClassesV.foreach(bs += _)
        // A constant non-primary severity joins the static class list; a reactive one is bound on the
        // element in the render sites (reactive class channel) so it swaps in place without a re-render.
        severityV.const.foreach(s => if s != Severity.Primary then bs += Button.severityClasses(s.ordinal))
        variantV match
            case ButtonVariant.Filled   => ()
            case ButtonVariant.Outlined => bs += "p-button-outlined"
            case ButtonVariant.Text     => bs += "p-button-text"
            case ButtonVariant.Link     => bs += "p-button-link"
        end match
        sizeV match
            case Size.Small  => bs += "p-button-sm"
            case Size.Large  => bs += "p-button-lg"
            case Size.Normal => ()
        end match
        if roundedFlag then bs += "p-button-rounded"
        if raisedFlag then bs += "p-button-raised"
        if fluidFlag then bs += "p-button-fluid"
        if iconOnly then bs += "p-button-icon-only"
        bs.result()
    end buttonClasses

    /** The attributes both renderings open with, in order: the class chain, the reactive severity
      * accent, the id and the extra aria pairs.
      */
    private def shell[E <: Ast.Element { type Self = E }](base: E)(using Frame): E =
        var el = buttonClasses.foldLeft(base)((e, c) => e.cssClass(c))
        severityV.dyn.foreach(sig =>
            Severity.values.foreach(s => if s != Severity.Primary then el = el.cssClass(Button.severityClasses(s.ordinal), sig.map(_ == s)))
        )
        // A reactive label that empties out turns the button icon-only in place, like the severity swap.
        if iconsOnly then label.flatMap(_.dyn).foreach(s => el = el.cssClass("p-button-icon-only", s.map(_.isEmpty)))
        idV.foreach(v => el = el.id(v))
        extraAriasV.foreach((n, v) => el = el.aria(n, v))
        el
    end shell

    /** The accessible name, its labelledby reference, the description and the tooltip, shared by both renderings. */
    private def described[E <: Ast.Element { type Self = E }](base: E)(using Frame): E =
        var el = base
        accessibleNameV.flatMap(_.const).foreach(n => el = el.aria("label", n))
        accessibleNameV.flatMap(_.dyn).foreach(s => el = el.aria("label", s))
        accessibleNameRefV.foreach(r => el = el.aria("labelledby", r))
        accessibleDescriptionV.flatMap(_.const).foreach(d => el = el.aria("description", d))
        accessibleDescriptionV.flatMap(_.dyn).foreach(s => el = el.aria("description", s))
        tooltipV.flatMap(_.const).foreach(t => el = el.jsProp("title", t))
        tooltipV.flatMap(_.dyn).foreach(s => el = el.title(s))
        el
    end described

    private def renderButton(using Frame): UI =
        var el = shell(button)
        el = el.jsProp(
            "type",
            buttonTypeV match
                case ButtonType.Button => "button"
                case ButtonType.Submit => "submit"
                case ButtonType.Reset  => "reset"
        )
        if accessibleRoleV == ButtonAccessibleRole.Link then el = el.role("link")
        roleRawV.foreach(r => el = el.role(r))
        tabIndexV.foreach(t => el = el.tabIndex(t))
        if focusSeedV then el = el.focusAuto(true).focusRestore(true)
        onKeyDownF.foreach(f => el = el.onKeyDown(f))
        el = described(el)
        formV.foreach(f => el = el.form(f))
        if loadingNow then el = el.aria("busy", "true")
        // A disabled button leaves the tab order natively. A loading or aria-disabled button stays in
        // it: aria-disabled and the greyed style, its click routed through the handler (form.submit
        // → focus-first-invalid) and blocked there while loading.
        if disabledNow then el = el.disabled(true)
        else if loadingNow || ariaDisabledV.constTrue then el = el.aria("disabled", "true").cssClass("p-uic-aria-disabled")
        if !disabledNow then
            onClickEff.foreach { e =>
                val handler: Any < (Abort[Throwable] & Async) = if loadingNow then Kyo.unit else e
                el = el.onClick(handler)
            }
        end if
        el(contentChildren.map(toChild)*)
    end renderButton

    /** Button skin on a client-routed anchor (SPA `Href.Path`). The anchor keeps its
      * native link role; form `type` and the native `disabled` attribute do not apply.
      * A disabled link drops href/click and gets `.p-disabled` + `aria-disabled` +
      * `tabIndex(-1)`. An aria-disabled link drops its href and leaves the tab order the
      * same way, keeping its click wired as the button does.
      */
    private def renderAnchor(h: String)(using Frame): UI =
        var el = described(shell(a))
        if loadingNow then el = el.aria("busy", "true")
        roleRawV.foreach(r => el = el.role(r))
        if focusSeedV then el = el.focusAuto(true).focusRestore(true)
        onKeyDownF.foreach(f => el = el.onKeyDown(f))
        // A disabled or aria-disabled link leaves the tab order outright and drops its href, so the
        // host's roving tabindex has nothing left to say about it.
        if effDisabled then el = el.cssClass("p-disabled").aria("disabled", "true").tabIndex(-1)
        else if ariaDisabledV.constTrue then
            el = el.cssClass("p-uic-aria-disabled").aria("disabled", "true").tabIndex(-1)
        else
            tabIndexV.foreach(t => el = el.tabIndex(t))
            el = el.href(Href.Path(h))
        end if
        if !effDisabled then onClickEff.foreach(e => el = el.onClick(e))
        el(contentChildren.map(toChild)*)
    end renderAnchor

    private def contentChildren(using Frame): List[UI] =
        val leading: List[UI] =
            if loadingNow then
                List(GlyphSvg(Icons.spinner, "p-button-icon", "p-button-icon-left", "p-button-loading-icon", "p-icon-spin"))
            else icon.toList.map(g => GlyphSvg(g, "p-button-icon", "p-button-icon-left"))
        val textChild: List[UI] = label match
            case Present(TextValue.Const(t)) if t.nonEmpty => List(span.cssClass("p-button-label")(t))
            case Present(TextValue.Dyn(s))                 => List(span.cssClass("p-button-label")(s))
            case _                                         => Nil
        leading ++ textChild ++ endIconV.toList.map(g => GlyphSvg(g, "p-button-icon", "p-button-icon-right")) ++ extraChildren
    end contentChildren
end Button

object Button:
    /** The `p-button-<token>` class of each severity, indexed by ordinal, built once rather than per render. */
    private val severityClasses: Span[String] = Span.from(Severity.values.map(s => s"p-button-${s.token}"))

    /** A button labelled `label`. A `Signal[String]` re-renders the label in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def apply(label: String | Signal[String]): Button = new Button(label = Present(ReactiveValue(label)))

    /** An empty button: add an icon and/or children via the setters. */
    def apply(): Button = new Button()
end Button
