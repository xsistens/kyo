package kyo.uic

import kyo.*
import kyo.UI.*

/** Button form types: plain button, form submit, or form reset. Maps onto the
  * native `type` attribute (default `Button`: non-submitting, unlike bare HTML).
  */
enum ButtonType derives CanEqual:
    case Button, Submit, Reset

    private[uic] def token: String = this.toString
end ButtonType

/** Button accessible roles (`role` attribute override). */
enum ButtonAccessibleRole derives CanEqual:
    case Button, Link

    private[uic] def token: String = this.toString
end ButtonAccessibleRole

/** Button: native kyo-ui, PrimeOne design. Renders a real `<button>` with
  * Prime's own anatomy (`.p-button` > `.p-button-icon`/`.p-button-label`), so
  * the extracted `@primeuix` component CSS applies verbatim.
  *
  * Severity defaults to `Primary` (Prime's default button IS the primary
  * action); `variant` switches Filled/Outlined/Text/Link; while `loading` the
  * button shows a spinning glyph and is non-clickable like a disabled button.
  */
final class Button private[uic] (private[uic] val p: Button.Props) extends Node, HasElementId, HasTooltip, HasAccessibleNameRef,
      HasAccessibleDescription, PopupTrigger, HasProps:
    type Self = Button

    /** Package-internal class hook: hosts (SplitButton, SpeedDial) stamp Prime's
      * contextual classes (`p-splitbutton-button`, `p-speeddial-button`) onto the
      * button element.
      */
    private[uic] def extraClass(cls: String): Button = new Button(p.copy(extraClasses = p.extraClasses :+ cls))

    /** Package-internal aria hook: hosts add popup semantics (`aria-haspopup`,
      * `aria-expanded`) not part of Button's own public surface. A signal is a channel,
      * patched in place.
      */
    private[uic] def ariaRaw(name: String, value: Prop[String]): Button =
        new Button(p.copy(extraArias = p.extraArias :+ (name -> value)))

    // A button is a native control: its keys are its click, so `activate` has nothing left to answer.
    private[uic] def asPopupTrigger(
        popup: String,
        expanded: Prop[String],
        fixed: Boolean,
        activate: Maybe[UI.Action]
    )(using Frame): UI =
        val trigger = ariaRaw("haspopup", popup).ariaRaw("expanded", expanded)
        (if fixed then trigger.ariaDisabled(true) else trigger).render
    end asPopupTrigger

    /** Package-internal composite-widget hooks: the attributes a button needs when it is a CHILD
      * of a composite widget rather than a control of its own. A standalone button decides none
      * of these (it is its own tab stop, it is a button, and the browser gives it Enter and
      * Space), so they stay off the public surface and belong to the host that owns the widget:
      * SpeedDial's fan is one `role="menu"` whose actions rove a single tab stop, are announced
      * as `menuitem`, answer arrows, and take the focus the fan seeds when it opens.
      */
    private[uic] def roleRaw(v: String): Button       = new Button(p.copy(role = Present(v)))
    private[uic] def tabIndexRaw(v: Int): Button      = new Button(p.copy(tabIndex = Present(v)))
    private[uic] def focusSeedRaw(v: Boolean): Button = new Button(p.copy(focusSeed = v))

    private[uic] def onKeyDownRaw(f: KeyboardEvent => UI.Action): Button =
        new Button(p.copy(onKeyDown = Present(f)))

    /** Semantic accent (`.p-button-<severity>`); `Primary` is the unsuffixed default. A
      * `Signal[Severity]` swaps the `.p-button-<token>` class IN PLACE via kyo-ui's class channel on
      * emission (no re-render), e.g. a save button going neutral to success.
      */
    def severity(v: Prop[Severity]): Button = new Button(p.copy(severity = v))

    /** Rendering variant: `Filled` (default), `Outlined`, `Text`, or `Link`. A signal swaps the variant's class in place. */
    def variant(v: Prop[ButtonVariant]): Button = new Button(p.copy(variant = v))

    /** Size: `Small` / `Normal` (default) / `Large`. A signal swaps the size's class in place. */
    def size(v: Prop[Size]): Button = new Button(p.copy(size = v))

    /** Fully rounded corners (`.p-button-rounded`), a signal toggling the class in place. */
    def rounded(v: Prop[Boolean]): Button = new Button(p.copy(rounded = v))

    /** Adds a shadow to indicate elevation (`.p-button-raised`), a signal toggling the class in place. */
    def raised(v: Prop[Boolean]): Button = new Button(p.copy(raised = v))

    /** Spans the full width of its container (`.p-button-fluid`), a signal toggling the class in place. */
    def fluid(v: Prop[Boolean]): Button = new Button(p.copy(fluid = v))

    /** Icon rendered before the label. A signal swaps the glyph in its own region, below the button. */
    def icon(glyph: Prop[IconGlyph]): Button = new Button(p.copy(icon = Present(glyph)))

    /** Icon rendered after the label. A signal swaps the glyph in its own region, below the button. */
    def endIcon(glyph: Prop[IconGlyph]): Button = new Button(p.copy(endIcon = Present(glyph)))

    private[uic] def accessibility: Accessibility              = p.access
    private[uic] def withAccessibility(v: Accessibility): Self = new Button(p.copy(access = v))

    /** Disables the button. A `Signal[Boolean]` enables/disables it in place on emission (e.g. a
      * mutation-in-flight signal), and a click reads it when it comes.
      */
    def disabled(v: Prop[Boolean]): Button = new Button(p.copy(disabled = v))

    /** Accessible-disabled: unlike the native `disabled` attribute (which removes the button
      * from the tab order and drops its click), this keeps the button focusable and IN the tab
      * sequence, marking it `aria-disabled="true"` for assistive tech plus a greyed style, and
      * keeps the click wired. Route that click through `form.submit`, which validates and moves
      * focus to the first invalid field. This is the fix for the focus trap where a natively
      * `disabled` submit button is skipped by Tab: the reactive re-enable lands a tick after the
      * Tab keypress, so focus jumps PAST the button to the next control. With `aria-disabled` the
      * button never leaves the tab order, so the race disappears.
      */
    def ariaDisabled(v: Prop[Boolean]): Button = new Button(p.copy(ariaDisabled = v))

    /** Binds a form's submit gate. [[SubmitGate]] is a distinct type from `Signal[Boolean]`
      * precisely so it fits HERE and not on [[disabled]]: a natively disabled submit button
      * leaves the tab order and the re-enable lands a tick late, which is the focus trap
      * [[ariaDisabled]] exists to avoid. `uic.Button("Save").ariaDisabled(gate)` is therefore
      * the whole binding, with no accessibility decision left to make.
      */
    def ariaDisabled(gate: SubmitGate): Button = ariaDisabled(gate.signal)

    /** Busy state: shows a spinning glyph and blocks clicks (like disabled). Bind a `Signal[Boolean]`
      * to an in-flight request; the button is drawn again on emission, its one region, because loading
      * shapes the spinner, aria-busy, icon-only layout and the effective disabled state rather than
      * patching a single attribute.
      */
    def loading(v: Prop[Boolean]): Button = new Button(p.copy(loading = v))

    /** Form behaviour (native `type` attribute): `Button` (default, non-submitting),
      * `Submit`, or `Reset`.
      */
    def buttonType(v: ButtonType): Button = new Button(p.copy(buttonType = v))

    /** ARIA role override: `Link` renders `role="link"` on the native button. */
    def accessibleRole(v: ButtonAccessibleRole): Button = new Button(p.copy(accessibleRole = v))

    private[uic] def withAccessibleDescription(v: Maybe[Prop[String]]): Button = new Button(p.copy(description = v))

    /** Renders the button as a client-routed navigation anchor (`<a>` with kyo-ui's
      * SPA `Href.Path`) that keeps the full button skin (Prime's Button link mode).
      * Link semantics (native anchor role, focus) with button styling; form `type`
      * does not apply.
      */
    def href(v: String): Button = new Button(p.copy(href = Present(v)))

    private[uic] def withTooltip(v: Maybe[Prop[String]]): Button = new Button(p.copy(tooltip = v))

    /** ID of the `<form>` to associate with. kyo-ui 1.0.0-RC5 exposes no `form`
      * attribute setter and the DOM `form` property is read-only, so this is
      * surfaced as `data-uic-form`: an association hint, not a native browser
      * form association.
      */
    def form(v: String): Button = new Button(p.copy(form = Present(v)))

    /** Runs `action` when the button is pressed. Mirrors kyo-ui's native event effect
      * (`Async`): a handler that can fail handles its own failure, the way a native
      * `onClick` does.
      */
    def onClick(action: => UI.Action)(using Frame): Button =
        new Button(p.copy(onClick = Present(Callback(action))))

    /** Adds arbitrary extra children (e.g. a custom badge) after the label/icon. */
    def apply(cs: UI*): Button = new Button(p.copy(extraChildren = p.extraChildren ++ cs))

    /** Draws the button, or the link when `href` is set. A stateless component has no instance: a host repaint has nothing of it
      * to lose. Each place reads the field it shows: `disabled`, `aria-disabled`, the severity, the variant, the size and the
      * shapes reach the element through its channels, and a click reads `disabled` when it comes. Only `loading` shapes the
      * button (spinner, icon-only, effective disabled), so it is the one read of the button's region; a link reads `disabled`
      * there too, since a disabled link is a different element. With neither a signal the block draws no region.
      */
    private[uic] def render(using Frame): UI =
        p.href match
            case Present(h) => UI.reactive(renderAnchor(h, p.loading.read, p.disabled.read))
            case Absent     =>
                UI.reactive {
                    val busy = p.loading.read
                    // While loading the button is non-clickable, exactly like disabled.
                    val off = busy || p.disabled.read
                    // aria-disabled keeps the button focusable and clickable (the click routes through the handler, e.g.
                    // form.submit to focus-first-invalid); it says so only while the button is not natively disabled.
                    val says = p.ariaDisabled.read && !off
                    renderButton(busy).disabled(off)
                        .when(p.onClick)((e, click) => e.onClick(if off then Kyo.unit else click))
                        .ariaFlag("disabled", says)
                        .cssClass("p-uic-aria-disabled", says)(UI.children(contentChildren(busy))*)
                }
        end match
    end render

    /** The `.p-button` class chain, shared by the button and anchor renderings: a constant is a class, a signal a class channel. */
    private def classed[E <: UI.Ast.Element { type Self = E }](el: E, loading: Boolean)(using Frame): E =
        p.extraClasses.foldLeft(el.cssClass("p-button").cssClass("p-component"))(_.cssClass(_))
            .classFor(p.severity)(_.cls("p-button")).classFor(p.variant)(_.cls).classFor(p.size)(_.cls("p-button"))
            .cssClass("p-button-rounded", p.rounded).cssClass("p-button-raised", p.raised).cssClass("p-button-fluid", p.fluid)
            .cssClass("p-button-icon-only", p.label.isEmpty && p.extraChildren.isEmpty && (p.icon.isDefined || loading))
    end classed

    /** What the button and the link share, on either element: the class chain, the id, the host's aria, the accessible name and
      * description, the tooltip, the host's role, focus seeding and key handler, and `aria-busy` while loading.
      */
    private def shared[E <: (UI.Ast.Element & UI.Ast.Interactive) { type Self = E }](el: E, loading: Boolean)(using Frame): E =
        p.extraArias.foldLeft(classed(el, loading))((e, aria) => e.aria(aria._1, aria._2))
            .when(p.access.id)(_.id(_))
            .when(p.role)(_.role(_))
            .when(p.focusSeed)(_.focusAuto(true).focusRestore(true))
            .when(p.onKeyDown)(_.onKeyDown(_))
            .aria("label", p.access.name)
            .aria("labelledby", p.access.nameRef)
            .aria("description", p.description)
            .title(p.tooltip)
            .when(loading)(_.aria("busy", "true"))
    end shared

    /** The `<button>` with what only it carries besides its flags: the form type, its own role, the tab stop and the form. */
    private def renderButton(loading: Boolean)(using Frame): UI.Ast.Button =
        // The form type and the button's own role come first, so the host's role (in `shared`) wins over `accessibleRole`.
        val typed = button.jsProp("type", p.buttonType.token.toLowerCase)
            .when(p.accessibleRole == ButtonAccessibleRole.Link)(_.role("link"))
        shared(typed, loading).when(p.tabIndex)(_.tabIndex(_)).when(p.form)(_.data("uic-form", _))
    end renderButton

    /** Button skin on a client-routed anchor (SPA `Href.Path`). The anchor keeps its
      * native link role; form `type` and the native `disabled` attribute do not apply,
      * so a disabled link drops href/click and gets `.p-disabled` + `aria-disabled` +
      * `tabIndex(-1)` (the same treatment as [[Link]]).
      */
    private def renderAnchor(h: String, loading: Boolean, disabled: Boolean)(using Frame): UI =
        val el = shared(a, loading)
        // A disabled link leaves the tab order outright, so the host's roving tabindex has
        // nothing left to say about it.
        val linked =
            if loading || disabled then el.cssClass("p-disabled").aria("disabled", "true").tabIndex(-1)
            else el.when(p.tabIndex)(_.tabIndex(_)).href(Href.Path(h)).when(p.onClick)(_.onClick(_))
        linked(UI.children(contentChildren(loading))*)
    end renderAnchor

    private def contentChildren(loading: Boolean)(using Frame): List[UI] =
        val leading: Maybe[UI] =
            if loading then Present(GlyphSvg(Icons.spinner, "p-button-icon", "p-button-icon-left", "p-button-loading-icon", "p-icon-spin"))
            else p.icon.map(GlyphSvg(_, "p-button-icon", "p-button-icon-left"))
        List(
            leading,
            p.label.map(l => span.cssClass("p-button-label")(l)),
            p.endIcon.map(GlyphSvg(_, "p-button-icon", "p-button-icon-right"))
        ).flatMap(_.toList) ++ p.extraChildren
    end contentChildren
end Button

object Button:
    final private[uic] case class Props(
        label: Maybe[Prop[String]] = Absent,
        severity: Prop[Severity] = Severity.Primary,
        variant: Prop[ButtonVariant] = ButtonVariant.Filled,
        size: Prop[Size] = Size.Normal,
        rounded: Prop[Boolean] = false,
        raised: Prop[Boolean] = false,
        fluid: Prop[Boolean] = false,
        icon: Maybe[Prop[IconGlyph]] = Absent,
        endIcon: Maybe[Prop[IconGlyph]] = Absent,
        access: Accessibility = Accessibility.none,
        href: Maybe[String] = Absent,
        disabled: Prop[Boolean] = false,
        ariaDisabled: Prop[Boolean] = false,
        loading: Prop[Boolean] = false,
        buttonType: ButtonType = ButtonType.Button,
        accessibleRole: ButtonAccessibleRole = ButtonAccessibleRole.Button,
        description: Maybe[Prop[String]] = Absent,
        tooltip: Maybe[Prop[String]] = Absent,
        form: Maybe[String] = Absent,
        onClick: Maybe[UI.Action] = Absent,
        onKeyDown: Maybe[KeyboardEvent => UI.Action] = Absent,
        extraChildren: List[UI] = Nil,
        extraClasses: List[String] = Nil,
        extraArias: List[(String, Prop[String])] = Nil,
        role: Maybe[String] = Absent,
        tabIndex: Maybe[Int] = Absent,
        focusSeed: Boolean = false
    ) derives CanEqual

    /** A button labelled `label`. A `Signal[String]` re-renders the label in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def apply(label: Prop[String]): Button = new Button(Props(label = Present(label)))

    /** An empty button; add an icon and/or children via the setters. */
    def apply(): Button = new Button(Props())
end Button
