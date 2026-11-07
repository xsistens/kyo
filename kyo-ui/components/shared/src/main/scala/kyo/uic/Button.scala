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
    import p.*

    type Self = Button

    /** Package-internal class hook: hosts (SplitButton, SpeedDial) stamp Prime's
      * contextual classes (`p-splitbutton-button`, `p-speeddial-button`) onto the
      * button element.
      */
    private[uic] def extraClass(cls: String): Button = new Button(p.copy(extraClassesV = extraClassesV :+ cls))

    /** Package-internal aria hook: hosts add popup semantics (`aria-haspopup`,
      * `aria-expanded`) not part of Button's own public surface. A signal is a channel,
      * patched in place.
      */
    private[uic] def ariaRaw(name: String, value: String | Signal[String]): Button =
        new Button(p.copy(extraAriasV = extraAriasV :+ (name -> value)))

    // A button is a native control: its keys are its click, so `activate` has nothing left to answer.
    private[uic] def asPopupTrigger(
        popup: String,
        expanded: String | Signal[String],
        fixed: Boolean,
        activate: Maybe[UI.Action]
    )(using Frame): UI =
        val trigger = ariaRaw("haspopup", popup).ariaRaw("expanded", expanded)
        (if fixed then trigger.ariaDisabled(true) else trigger).render
    end asPopupTrigger

    /** The host-added aria attributes, a constant set as it stands and a signal as a channel. */
    private def withExtraArias[E <: UI.Ast.Element { type Self = E }](el: E): E =
        extraAriasV.foldLeft(el) {
            case (e, (n, v: String))                    => e.aria(n, v)
            case (e, (n, v: Signal[String] @unchecked)) => e.aria(n, v)
        }

    /** Package-internal composite-widget hooks: the attributes a button needs when it is a CHILD
      * of a composite widget rather than a control of its own. A standalone button decides none
      * of these (it is its own tab stop, it is a button, and the browser gives it Enter and
      * Space), so they stay off the public surface and belong to the host that owns the widget:
      * SpeedDial's fan is one `role="menu"` whose actions rove a single tab stop, are announced
      * as `menuitem`, answer arrows, and take the focus the fan seeds when it opens.
      */
    private[uic] def roleRaw(v: String): Button       = new Button(p.copy(roleRawV = Present(v)))
    private[uic] def tabIndexRaw(v: Int): Button      = new Button(p.copy(tabIndexV = Present(v)))
    private[uic] def focusSeedRaw(v: Boolean): Button = new Button(p.copy(focusSeedV = v))

    private[uic] def onKeyDownRaw(f: KeyboardEvent => UI.Action): Button =
        new Button(p.copy(onKeyDownF = Present(f)))

    /** Semantic accent (`.p-button-<severity>`); `Primary` is the unsuffixed default. A
      * `Signal[Severity]` swaps the `.p-button-<token>` class IN PLACE via kyo-ui's class channel on
      * emission (no re-render), e.g. a save button going neutral to success.
      */
    def severity(v: Severity | Signal[Severity]): Button = new Button(p.copy(severityV = v))

    /** Rendering variant: `Filled` (default), `Outlined`, `Text`, or `Link`. */
    def variant(v: ButtonVariant): Button = new Button(p.copy(variantV = v))

    /** Size: `Small` / `Normal` (default) / `Large`. */
    def size(v: Size): Button = new Button(p.copy(sizeV = v))

    /** Fully rounded corners (`.p-button-rounded`). */
    def rounded(v: Boolean): Button = new Button(p.copy(roundedFlag = v))

    /** Adds a shadow to indicate elevation (`.p-button-raised`). */
    def raised(v: Boolean): Button = new Button(p.copy(raisedFlag = v))

    /** Spans the full width of its container (`.p-button-fluid`). */
    def fluid(v: Boolean): Button = new Button(p.copy(fluidFlag = v))

    def icon(glyph: IconGlyph): Button = new Button(p.copy(icon = Present(glyph)))

    /** Icon rendered after the label. */
    def endIcon(glyph: IconGlyph): Button = new Button(p.copy(endIconV = Present(glyph)))

    private[uic] def accessibility: Accessibility              = accessV
    private[uic] def withAccessibility(v: Accessibility): Self = new Button(p.copy(accessV = v))

    /** Disables the button. A `Signal[Boolean]` enables/disables it in place on emission (e.g. a
      * mutation-in-flight signal).
      */
    def disabled(v: Boolean | Signal[Boolean]): Button = new Button(p.copy(disabledV = Present(v)))

    /** Accessible-disabled: unlike the native `disabled` attribute (which removes the button
      * from the tab order and drops its click), this keeps the button focusable and IN the tab
      * sequence, marking it `aria-disabled="true"` for assistive tech plus a greyed style, and
      * keeps the click wired. Route that click through `form.submit`, which validates and moves
      * focus to the first invalid field. This is the fix for the focus trap where a natively
      * `disabled` submit button is skipped by Tab: the reactive re-enable lands a tick after the
      * Tab keypress, so focus jumps PAST the button to the next control. With `aria-disabled` the
      * button never leaves the tab order, so the race disappears.
      */
    def ariaDisabled(v: Boolean | Signal[Boolean]): Button = new Button(p.copy(ariaDisabledV = Present(v)))

    /** Binds a form's submit gate. [[SubmitGate]] is a distinct type from `Signal[Boolean]`
      * precisely so it fits HERE and not on [[disabled]]: a natively disabled submit button
      * leaves the tab order and the re-enable lands a tick late, which is the focus trap
      * [[ariaDisabled]] exists to avoid. `uic.Button("Save").ariaDisabled(gate)` is therefore
      * the whole binding, with no accessibility decision left to make.
      */
    def ariaDisabled(gate: SubmitGate): Button = ariaDisabled(gate.signal)

    /** Busy state: shows a spinning glyph and blocks clicks (like disabled). Bind a `Signal[Boolean]`
      * to an in-flight request; the button re-renders its own boundary on emission, because loading is
      * fused with the spinner, aria-busy, icon-only layout and the effective disabled state rather than
      * patching a single attribute.
      */
    def loading(v: Boolean | Signal[Boolean]): Button = new Button(p.copy(loadingV = Present(v)))

    /** Form behaviour (native `type` attribute): `Button` (default, non-submitting),
      * `Submit`, or `Reset`.
      */
    def buttonType(v: ButtonType): Button = new Button(p.copy(buttonTypeV = v))

    /** ARIA role override: `Link` renders `role="link"` on the native button. */
    def accessibleRole(v: ButtonAccessibleRole): Button = new Button(p.copy(accessibleRoleV = v))

    private[uic] def withAccessibleDescription(v: Maybe[TextValue]): Button = new Button(p.copy(accessibleDescriptionV = v))

    /** Renders the button as a client-routed navigation anchor (`<a>` with kyo-ui's
      * SPA `Href.Path`) that keeps the full button skin (Prime's Button link mode).
      * Link semantics (native anchor role, focus) with button styling; form `type`
      * does not apply.
      */
    def href(v: String): Button = new Button(p.copy(hrefV = Present(v)))

    private[uic] def withTooltip(v: Maybe[TextValue]): Button = new Button(p.copy(tooltipV = v))

    /** ID of the `<form>` to associate with. kyo-ui 1.0.0-RC5 exposes no `form`
      * attribute setter and the DOM `form` property is read-only, so this is
      * surfaced as `data-uic-form`: an association hint, not a native browser
      * form association.
      */
    def form(v: String): Button = new Button(p.copy(formV = Present(v)))

    /** Runs `action` when the button is pressed. Mirrors kyo-ui's native event effect
      * (`Async`): a handler that can fail handles its own failure, the way a native
      * `onClick` does.
      */
    def onClick(action: => UI.Action)(using Frame): Button =
        new Button(p.copy(onClickEff = Present(Callback(action))))

    /** Adds arbitrary extra children (e.g. a custom badge) after the label/icon. */
    def apply(cs: UI*): Button = new Button(p.copy(extraChildren = extraChildren ++ cs))

    // A stateless component draws no instance: a host repaint has nothing of it to lose. The three switches are read as ONE
    // region, pinned back into the props as constants so the element reads plain values. With none of them a signal the button
    // draws no region and pays for no reading.
    private[uic] def render(using Frame): UI =
        Prop.render(Prop.zip(Prop.zip(Button.switch(disabledV), Button.switch(ariaDisabledV)), Button.switch(loadingV))) {
            case ((d, ad), l) => new Button(p.copy(disabledV = d, ariaDisabledV = ad, loadingV = l)).renderResolved
        }

    /** The resolved busy state, read after the switches are pinned to their current constants. */
    private def loadingNow: Boolean = Button.on(loadingV)

    private def renderResolved(using Frame): UI =
        hrefV match
            case Present(h) => renderAnchor(h)
            case Absent     => renderButton

    private def iconOnly: Boolean =
        label.isEmpty && extraChildren.isEmpty && (p.icon.isDefined || loadingNow)

    /** The `.p-button` class chain, shared verbatim by the button and anchor renderings. */
    private def buttonClasses: List[String] =
        val bs = List.newBuilder[String]
        bs += "p-button"
        bs += "p-component"
        extraClassesV.foreach(bs += _)
        // A constant non-primary severity joins the static class list; a reactive one is bound on the
        // element in the render sites (reactive class channel) so it swaps in place without a re-render.
        severityV match
            case s: Severity => if s != Severity.Primary then bs += s"p-button-${s.token}"
            case _           => ()
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

    private def renderButton(using Frame): UI =
        var el = buttonClasses.foldLeft(button)((e, c) => e.cssClass(c))
        severityV match
            case sig: Signal[Severity] @unchecked =>
                Severity.values.foreach(s => if s != Severity.Primary then el = el.cssClass(s"p-button-${s.token}", sig.map(_ == s)))
            case _ => ()
        end match
        accessV.idV.foreach(v => el = el.id(v))
        el = withExtraArias(el)
        el = el.jsProp("type", buttonTypeV.token.toLowerCase)
        if accessibleRoleV == ButtonAccessibleRole.Link then el = el.role("link")
        roleRawV.foreach(r => el = el.role(r))
        tabIndexV.foreach(t => el = el.tabIndex(t))
        if focusSeedV then el = el.focusAuto(true).focusRestore(true)
        onKeyDownF.foreach(f => el = el.onKeyDown(f))
        accessV.nameV match
            case Present(TextValue.Const(n)) => el = el.aria("label", n)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        accessV.nameRefV.foreach(r => el = el.aria("labelledby", r))
        accessibleDescriptionV match
            case Present(TextValue.Const(d)) => el = el.aria("description", d)
            case Present(TextValue.Dyn(s))   => el = el.aria("description", s)
            case Absent                      => ()
        end match
        tooltipV match
            case Present(TextValue.Const(t)) => el = el.jsProp("title", t)
            case Present(TextValue.Dyn(s))   => el = el.title(s)
            case Absent                      => ()
        end match
        formV.foreach(f => el = el.data("uic-form", f))
        if loadingNow then el = el.aria("busy", "true")
        // While loading the button is non-clickable, exactly like disabled.
        val effDisabled = loadingNow || Button.on(disabledV)
        if effDisabled then el = el.disabled(true)
        else Button.constant(disabledV).foreach(d => el = el.disabled(d))
        if !effDisabled then onClickEff.foreach(e => el = el.onClick(e))
        // aria-disabled keeps the button focusable + clickable (the click routes through the
        // handler, e.g. form.submit → focus-first-invalid); only apply when not natively disabled.
        if !effDisabled then
            Button.constant(ariaDisabledV).foreach(ad => if ad then el = el.aria("disabled", "true").cssClass("p-uic-aria-disabled"))
        el(UI.children(contentChildren)*)
    end renderButton

    /** Button skin on a client-routed anchor (SPA `Href.Path`). The anchor keeps its
      * native link role; form `type` and the native `disabled` attribute do not apply,
      * so a disabled link drops href/click and gets `.p-disabled` + `aria-disabled` +
      * `tabIndex(-1)` (the same treatment as [[Link]]).
      */
    private def renderAnchor(h: String)(using Frame): UI =
        var el = buttonClasses.foldLeft(a)((e, c) => e.cssClass(c))
        severityV match
            case sig: Signal[Severity] @unchecked =>
                Severity.values.foreach(s => if s != Severity.Primary then el = el.cssClass(s"p-button-${s.token}", sig.map(_ == s)))
            case _ => ()
        end match
        accessV.idV.foreach(v => el = el.id(v))
        el = withExtraArias(el)
        accessV.nameV match
            case Present(TextValue.Const(n)) => el = el.aria("label", n)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        accessV.nameRefV.foreach(r => el = el.aria("labelledby", r))
        accessibleDescriptionV match
            case Present(TextValue.Const(d)) => el = el.aria("description", d)
            case Present(TextValue.Dyn(s))   => el = el.aria("description", s)
            case Absent                      => ()
        end match
        tooltipV match
            case Present(TextValue.Const(t)) => el = el.jsProp("title", t)
            case Present(TextValue.Dyn(s))   => el = el.title(s)
            case Absent                      => ()
        end match
        if loadingNow then el = el.aria("busy", "true")
        roleRawV.foreach(r => el = el.role(r))
        if focusSeedV then el = el.focusAuto(true).focusRestore(true)
        onKeyDownF.foreach(f => el = el.onKeyDown(f))
        val effDisabled = loadingNow || Button.on(disabledV)
        // A disabled link leaves the tab order outright, so the host's roving tabindex has
        // nothing left to say about it.
        if effDisabled then el = el.cssClass("p-disabled").aria("disabled", "true").tabIndex(-1)
        else
            tabIndexV.foreach(t => el = el.tabIndex(t))
            el = el.href(Href.Path(h))
            onClickEff.foreach(e => el = el.onClick(e))
        end if
        el(UI.children(contentChildren)*)
    end renderAnchor

    private def contentChildren(using Frame): List[UI] =
        val leading: List[UI] =
            if loadingNow then
                List(GlyphSvg(Icons.spinner, "p-button-icon", "p-button-icon-left", "p-button-loading-icon", "p-icon-spin"))
            else p.icon.toList.map(g => GlyphSvg(g, "p-button-icon", "p-button-icon-left"))
        val textChild: List[UI]    = label.toList.map(l => span.cssClass("p-button-label")(l))
        val endIconChild: List[UI] =
            endIconV.toList.map(g => GlyphSvg(g, "p-button-icon", "p-button-icon-right"))
        leading ++ textChild ++ endIconChild ++ extraChildren
    end contentChildren
end Button

object Button:
    final private[uic] case class Props(
        label: Maybe[Prop[String]] = Absent,
        severityV: Prop[Severity] = Severity.Primary,
        variantV: ButtonVariant = ButtonVariant.Filled,
        sizeV: Size = Size.Normal,
        roundedFlag: Boolean = false,
        raisedFlag: Boolean = false,
        fluidFlag: Boolean = false,
        icon: Maybe[IconGlyph] = Absent,
        endIconV: Maybe[IconGlyph] = Absent,
        accessV: Accessibility = Accessibility.none,
        hrefV: Maybe[String] = Absent,
        disabledV: Maybe[Prop[Boolean]] = Absent,
        ariaDisabledV: Maybe[Prop[Boolean]] = Absent,
        loadingV: Maybe[Prop[Boolean]] = Absent,
        buttonTypeV: ButtonType = ButtonType.Button,
        accessibleRoleV: ButtonAccessibleRole = ButtonAccessibleRole.Button,
        accessibleDescriptionV: Maybe[TextValue] = Absent,
        tooltipV: Maybe[TextValue] = Absent,
        formV: Maybe[String] = Absent,
        onClickEff: Maybe[UI.Action] = Absent,
        onKeyDownF: Maybe[KeyboardEvent => UI.Action] = Absent,
        extraChildren: List[UI] = Nil,
        extraClassesV: List[String] = Nil,
        extraAriasV: List[(String, String | Signal[String])] = Nil,
        roleRawV: Maybe[String] = Absent,
        tabIndexV: Maybe[Int] = Absent,
        focusSeedV: Boolean = false
    )

    /** A button labelled `label`. A `Signal[String]` re-renders the label in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def apply(label: String | Signal[String]): Button = new Button(Props(label = Present(label)))

    /** A switch the button may have, as one prop: absent stays absent, a constant a constant, a signal a signal of it. */
    private def switch(v: Maybe[Prop[Boolean]])(using Frame): Prop[Maybe[Boolean]] =
        v match
            case Present(sw) => Prop.map(sw)(Present(_))
            case _           => Absent

    /** A switch pinned to a constant, and on. */
    private def on(v: Maybe[Prop[Boolean]]): Boolean = constant(v).getOrElse(false)

    /** A switch pinned to a constant, as that constant. */
    private def constant(v: Maybe[Prop[Boolean]]): Maybe[Boolean] =
        v match
            case Present(b: Boolean) => Present(b)
            case _                   => Absent

    /** An empty button; add an icon and/or children via the setters. */
    def apply(): Button = new Button(Props())
end Button
