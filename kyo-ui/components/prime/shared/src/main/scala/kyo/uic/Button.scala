package kyo.uic

import kyo.*
import kyo.UI.*

/** Button form types: plain button, form submit, or form reset. Maps onto the
  * native `type` attribute (default `Button`: non-submitting, unlike bare HTML).
  */
enum ButtonType(private[uic] val token: String) derives CanEqual:
    case Button extends ButtonType("button")
    case Submit extends ButtonType("submit")
    case Reset  extends ButtonType("reset")
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
  * button shows a spinning glyph, is `aria-disabled` and greyed, and blocks its
  * click in the handler, so it stays in the tab order.
  */
final class Button private[uic] (private[uic] val p: Button.Props) extends Component(Setters[Button.Props, Button]), Node,
      HasElementId, HasTooltip, HasAccessibleNameRef, HasAccessibleDescription, PopupTrigger:
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
    private[uic] def ariaRaw(name: Aria, value: Prop[String]): Button =
        new Button(p.copy(extraArias = p.extraArias :+ (name -> value)))

    // A button is a native control: its keys are its click, so `activate` has nothing left to answer.
    private[uic] def asPopupTrigger(
        popup: String,
        expanded: Prop[String],
        fixed: Boolean,
        activate: Maybe[UI.Action]
    )(using Frame): UI =
        val trigger = ariaRaw(Aria.haspopup, popup).ariaRaw(Aria.expanded, expanded)
        trigger.when(fixed)(_.ariaDisabled(true)).render
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

    private[uic] def onKeyDownRaw(f: KeyboardEvent => UI.Action): Button = new Button(p.copy(onKeyDown = Present(f)))

    private[uic] def accessibility: Accessibility                              = p.access
    private[uic] def withAccessibility(v: Accessibility): Self                 = new Button(p.copy(access = v))
    private[uic] def withAccessibleDescription(v: Maybe[Prop[String]]): Button = new Button(p.copy(description = v))
    private[uic] def withTooltip(v: Maybe[Prop[String]]): Button               = new Button(p.copy(tooltip = v))

    /** Accessible-disabled: unlike the native `disabled` attribute (which removes the button
      * from the tab order and drops its click), this keeps the button focusable and IN the tab
      * sequence, marking it `aria-disabled="true"` for assistive tech plus a greyed style, and
      * keeps the click wired. Route that click through `form.submit`, which validates and moves
      * focus to the first invalid field. This is the fix for the focus trap where a natively
      * `disabled` submit button is skipped by Tab: the reactive re-enable lands a tick after the
      * Tab keypress, so focus jumps PAST the button to the next control. With `aria-disabled` the
      * button never leaves the tab order, so the race disappears. On a link (`href`) the inert state keeps the href, which a click
      * declines, and takes the link out of the tab order (`tabIndex(-1)`). [[Link]] drops the href of a disabled link instead.
      */
    def ariaDisabled(v: Prop[Boolean]): Button = new Button(p.copy(ariaDisabled = v))

    /** Binds a form's submit gate. [[SubmitGate]] is a distinct type from `Signal[Boolean]`
      * precisely so it fits HERE and not on `disabled`: a natively disabled submit button
      * leaves the tab order and the re-enable lands a tick late, which is the focus trap
      * [[ariaDisabled]] exists to avoid. `uic.Button("Save").ariaDisabled(gate)` is therefore
      * the whole binding, with no accessibility decision left to make.
      */
    def ariaDisabled(gate: SubmitGate): Button = ariaDisabled(gate.signal)

    /** Adds arbitrary extra children (e.g. a custom badge) after the label/icon. */
    def apply(cs: UI*): Button = new Button(p.copy(extraChildren = p.extraChildren ++ cs))

    /** Draws the button, or the link when `href` is set. A stateless component has no instance: a host repaint has nothing of it
      * to lose. Each place reads the field it shows: `disabled`, `aria-disabled`, the severity, the variant, the size and the
      * shapes reach the element through its channels, and a click reads `disabled` and `loading` when it comes. Only `loading`
      * shapes the button (spinner, icon-only, aria-disabled), so it is the one read of the button's region; a link reads `disabled`
      * and `ariaDisabled` there too, since an inert link is a different element. With neither a signal the block draws no region.
      */
    private[uic] def render(using Frame): UI =
        // The gated click actions are built once per render, not once per run of the reactive block below.
        p.href match
            case Present(h) =>
                val pressed = p.onClick.map(a => answering(a, link = true))
                UI.reactive(renderAnchor(h, p.loading.read, p.disabled.read, p.ariaDisabled.read, pressed))
            case Absent =>
                val pressed = p.onClick.map(a => answering(a, link = false))
                UI.reactive {
                    val busy = p.loading.read
                    // A loading button is aria-disabled, not natively disabled: it stays in the tab order, and `answering`
                    // blocks its click.
                    val off = p.disabled.read
                    // aria-disabled keeps the button focusable and clickable (the click routes through the handler, e.g.
                    // form.submit to focus-first-invalid); it says so only while the button is not natively disabled.
                    val says = (busy || p.ariaDisabled.read) && !off
                    renderButton(busy).disabled(off)
                        .when(pressed)((el, act) => el.onClick(act))
                        .aria(Aria.disabled -> says)
                        .when(says)(_.cssClass("p-uic-aria-disabled"))(contentChildren(busy)*)
                }
        end match
    end render

    /** The click handler: nothing runs while the button is loading or natively disabled, and a link also runs nothing while it is
      * aria-disabled, as [[Link]] does. Each value is read when the click comes, not when the button renders, so a click that lands
      * as the button starts to load does not run its action.
      */
    private def answering(a: UI.Action, link: Boolean)(using Frame): UI.Action =
        direct {
            val inert = if link then p.ariaDisabled.read.now else false
            if inert || p.loading.read.now || p.disabled.read.now then () else a.now
        }
    end answering

    /** The `.p-button` class chain, shared by the button and anchor renderings: a constant is a class, a signal a class channel. */
    private def classed[E <: UI.Ast.Element { type Self = E }](el: E, loading: Boolean)(using Frame): E =
        p.extraClasses.foldLeft(el.cssClass("p-button", "p-component"))(_.cssClass(_))
            .cssClass.matching(p.severity)(s => if s == Severity.Primary then Absent else Button.severityClass(s.ordinal))
            .cssClass.matching(p.variant)(v => if v == ButtonVariant.Filled then Absent else Button.variantClass(v.ordinal))
            .cssClass.matching(p.size) {
                case Size.Small  => "p-button-sm"
                case Size.Normal => Absent
                case Size.Large  => "p-button-lg"
            }
            .when(p.rounded)(_.cssClass("p-button-rounded")).when(p.raised)(_.cssClass("p-button-raised")).when(p.fluid)(_.cssClass(
                "p-button-fluid"
            ))
            .when(iconOnly(loading))(_.cssClass("p-button-icon-only"))
    end classed

    /** Icon-only: no children and no label, with an icon (leading or trailing) or the spinner in its place. An empty label is no label,
      * and a label signal that empties gains the class in place.
      */
    private def iconOnly(loading: Boolean)(using Frame): Prop[Boolean] =
        if p.extraChildren.nonEmpty || !(p.icon.isDefined || p.endIcon.isDefined || loading) then false
        else p.label.fold[Prop[Boolean]](true)(l => Prop.map(l)(_.isEmpty))
    end iconOnly

    /** What the button and the link share, on either element: the class chain, the id, the host's aria, the accessible name and
      * description, the tooltip, the host's role, focus seeding and key handler, and `aria-busy` while loading.
      */
    private def shared[E <: (UI.Ast.Element & UI.Ast.Interactive) { type Self = E }](el: E, loading: Boolean)(using Frame): E =
        p.extraArias.foldLeft(classed(el, loading))((e, aria) => e.aria(aria))
            .when(p.access.id)(_.id(_))
            .when(p.role)(_.role(_))
            .when(p.focusSeed)(_.focusAuto(true).focusRestore(true))
            .when(p.onKeyDown)(_.onKeyDown(_))
            .aria(
                Aria.label       -> p.access.name,
                Aria.labelledby  -> p.access.nameRef,
                Aria.description -> p.description
            )
            .title(p.tooltip)
            .when(loading)(_.aria(Aria.busy -> true))
    end shared

    /** The `<button>` with what only it carries besides its flags: the form type, its own role, the tab stop and the form. A loading
      * button is `type="button"`, so its click submits or resets nothing, and it declines the browser's default for the click.
      */
    private def renderButton(loading: Boolean)(using Frame): UI.Ast.Button =
        // The form type and the button's own role come first, so the host's role (in `shared`) wins over `accessibleRole`.
        val typed = button.jsProp("type", if loading then ButtonType.Button.token else p.buttonType.token)
            .when(p.accessibleRole == ButtonAccessibleRole.Link)(_.role("link"))
        shared(typed, loading).when(loading)(_.preventActivation).when(p.tabIndex)(_.tabIndex(_)).when(p.form)(_.form(_))
    end renderButton

    /** Button skin on a client-routed anchor (SPA `Href.Path`). The anchor keeps its native link role; form `type` and the native
      * `disabled` attribute do not apply. An inert link (disabled or aria-disabled) gets `.p-disabled`, `aria-disabled`
      * and `tabIndex(-1)`, the treatment [[Link]] gives. A natively disabled link drops its href (it navigates nowhere); an
      * aria-disabled one keeps it. A loading link keeps its href and its place in the tab order,
      * as a loading button does. A blocked link (loading, disabled or aria-disabled) is marked `data-kyo-blocked`, so the client
      * declines the browser's default for its plain, modified and middle clicks (it navigates nowhere), and [[answering]] runs
      * no host action. The browser's context menu is not covered: its "open link in new tab" still follows the href.
      */
    private def renderAnchor(
        h: String,
        loading: Boolean,
        disabled: Boolean,
        ariaDisabled: Boolean,
        pressed: Maybe[UI.Action]
    )(using Frame): UI =
        // The marker the clients read to decline a blocked link's clicks. aria-disabled is only a hint for assistive technology, so an
        // anchor a caller marks aria-disabled on its own still navigates.
        val el = shared(a, loading).blocked(loading || disabled || ariaDisabled)
        // An inert link leaves the tab order outright, so the host's roving tabindex has nothing left to say about it.
        val linked =
            if disabled || ariaDisabled then
                (if disabled then el else el.href(Href.Path(h))).cssClass("p-disabled").aria(Aria.disabled -> true).tabIndex(-1)
            else if loading then
                el.href(Href.Path(h)).cssClass("p-uic-aria-disabled").aria(Aria.disabled -> true).tabIndex(p.tabIndex.getOrElse(0))
            else el.when(p.tabIndex)(_.tabIndex(_)).href(Href.Path(h))
        linked.when(pressed)((node, act) => node.onClick(act))(contentChildren(loading)*)
    end renderAnchor

    private def contentChildren(loading: Boolean)(using Frame): UI.Children =
        val leading: Maybe[UI] =
            if loading then Present(GlyphSvg(Icons.spinner, "p-button-icon", "p-button-icon-left", "p-button-loading-icon", "p-icon-spin"))
            else p.icon.map(GlyphSvg(_, "p-button-icon", "p-button-icon-left"))
        // An empty label is no label: a constant "" draws no span, while a label signal keeps its span and empties it in place.
        val label: Maybe[UI] = p.label.fold(Absent: Maybe[UI]) {
            case text: String if text.isEmpty => Absent
            case text                         => Present(span.cssClass("p-button-label")(text))
        }
        UI.children(
            leading,
            label,
            p.endIcon.map(GlyphSvg(_, "p-button-icon", "p-button-icon-right")),
            p.extraChildren
        )
    end contentChildren
end Button

object Button:
    /** The class each severity and variant adds, built once from its token: `Primary` and `Filled` add none, the classes are
      * indexed by ordinal so a render reads them without building a string.
      */
    private val severityClass: Array[String] = Severity.values.map(s => s"p-button-${s.token}")
    private val variantClass: Array[String]  = ButtonVariant.values.map(v => s"p-button-${v.token}")

    /** What a button holds. Each field is a setter of its name (`uic.Button("Save").size(Size.Small)`), which takes a constant or
      * a signal where the field is a `Prop` and the value itself where it is an `Opt`, and `onClick` its action by name;
      * `Button` writes the setter of `ariaDisabled` itself, and the fields marked `@noSetter` are written by hosts and the shared
      * traits.
      */
    final private[uic] case class Props(
        /** The label. A signal re-renders the label in place on emission (e.g. a locale-driven `I18n.t` leaf). */
        label: Opt[Prop[String]] = Opt.unset,
        /** Semantic accent (`.p-button-<severity>`); `Primary` is the unsuffixed default. A signal swaps the class in place,
          * e.g. a save button going neutral to success.
          */
        severity: Prop[Severity] = Severity.Primary,
        /** Rendering variant: `Filled` (default), `Outlined`, `Text`, or `Link`. A signal swaps the variant's class in place. */
        variant: Prop[ButtonVariant] = ButtonVariant.Filled,
        /** Size: `Small` / `Normal` (default) / `Large`. A signal swaps the size's class in place. */
        size: Prop[Size] = Size.Normal,
        /** Fully rounded corners (`.p-button-rounded`), a signal toggling the class in place. */
        rounded: Prop[Boolean] = false,
        /** Adds a shadow to indicate elevation (`.p-button-raised`), a signal toggling the class in place. */
        raised: Prop[Boolean] = false,
        /** Spans the full width of its container (`.p-button-fluid`), a signal toggling the class in place. */
        fluid: Prop[Boolean] = false,
        /** Icon rendered before the label. A signal swaps the glyph in its own region, below the button. */
        icon: Opt[Prop[IconGlyph]] = Opt.unset,
        /** Icon rendered after the label. A signal swaps the glyph in its own region, below the button. */
        endIcon: Opt[Prop[IconGlyph]] = Opt.unset,
        /** Renders the button as a client-routed navigation anchor (`<a>` with kyo-ui's SPA `Href.Path`) that keeps the full
          * button skin (Prime's Button link mode): link semantics with button styling; form `type` does not apply.
          */
        href: Opt[String] = Opt.unset,
        /** Disables the button. A signal enables and disables it in place on emission (e.g. a mutation-in-flight signal), and a
          * click reads it when it comes.
          */
        disabled: Prop[Boolean] = false,
        ariaDisabled: Prop[Boolean] = false,
        /** Busy state: shows a spinning glyph, marks the button `aria-disabled` and greyed (it stays in the tab order), and blocks its
          * click in the handler. A signal is the button's one region, because loading shapes the spinner, aria-busy, the icon-only
          * layout and the aria-disabled state.
          */
        loading: Prop[Boolean] = false,
        /** Form behaviour (native `type` attribute): `Button` (default, non-submitting), `Submit`, or `Reset`. */
        buttonType: ButtonType = ButtonType.Button,
        /** ARIA role override: `Link` renders `role="link"` on the native button. */
        accessibleRole: ButtonAccessibleRole = ButtonAccessibleRole.Button,
        /** ID of the `<form>` this button submits, written as the native `form` attribute, so the button submits that form from
          * anywhere in the page.
          */
        form: Opt[String] = Opt.unset,
        /** What a press runs, kyo-ui's native handler type: its setter takes the action by name, built each time the button is
          * pressed, and a failure goes to the `onFailure` rules where it ran.
          */
        onClick: Maybe[UI.Action] = Absent,
        @noSetter access: Accessibility = Accessibility.none,
        @noSetter description: Maybe[Prop[String]] = Absent,
        @noSetter tooltip: Maybe[Prop[String]] = Absent,
        @noSetter onKeyDown: Maybe[KeyboardEvent => UI.Action] = Absent,
        @noSetter extraChildren: List[UI] = Nil,
        @noSetter extraClasses: List[String] = Nil,
        @noSetter extraArias: List[(Aria, Prop[String])] = Nil,
        @noSetter role: Maybe[String] = Absent,
        @noSetter tabIndex: Maybe[Int] = Absent,
        @noSetter focusSeed: Boolean = false
    ) derives CanEqual

    /** A button labelled `label`. */
    def apply(label: Prop[String]): Button = new Button(Props(label = label))

    /** An empty button; add an icon and/or children via the setters. */
    def apply(): Button = new Button(Props())
end Button
