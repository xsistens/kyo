package kyo.uic

import kyo.*
import kyo.UI.*

/** SplitButton: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * SplitButton anatomy: `div.p-splitbutton.p-component
  * [.p-splitbutton-raised|-rounded|-fluid]` > primary
  * `button.p-splitbutton-button.p-button` + attached dropdown trigger
  * `button.p-splitbutton-dropdown.p-button.p-button-icon-only`), so the
  * extracted `@primeuix` splitbutton CSS applies verbatim: the sheet fuses the
  * two buttons' radii and drops the inner border.
  *
  * The primary segment runs `onClick`; the chevron segment toggles a popup
  * [[Menu]] of the given [[MenuItem]]s, the real [[Overlay]] panel anchored on
  * the splitbutton root (the sheet's `position: relative`), with Prime's
  * `.p-splitbutton .p-menu { min-width: 100% }` sizing, outside-click/Escape
  * dismissal, focus seeding, and the Menu's ArrowDown/ArrowUp + Enter keyboard
  * navigation. `severity`/`variant`/`size`/`rounded`/`raised`/`fluid`/
  * `disabled` pass through to BOTH segments (Button precedent). `open(ref)`
  * optionally exposes the panel visibility (self-managed otherwise).
  */
final class SplitButton private[uic] (private[uic] val p: SplitButton.Props) extends Node, HasProps:
    import p.*

    type Self = SplitButton

    /** Semantic accent, passed to both segments. A `Signal[Severity]` is forwarded to the inner [[Button]]
      * and swapped IN PLACE via the class channel.
      */
    def severity(v: Severity | Signal[Severity]): SplitButton = new SplitButton(p.copy(severityV = ReactiveValue(v)))

    /** Rendering variant (Filled/Outlined/Text/Link), passed to both segments. */
    def variant(v: ButtonVariant): SplitButton = new SplitButton(p.copy(variantV = v))

    /** Size, passed to both segments. */
    def size(v: Size): SplitButton = new SplitButton(p.copy(sizeV = v))

    /** Fully rounded outer corners (`.p-splitbutton-rounded`). */
    def rounded(v: Boolean): SplitButton = new SplitButton(p.copy(roundedFlag = v))

    /** Elevation shadow (`.p-splitbutton-raised`). */
    def raised(v: Boolean): SplitButton = new SplitButton(p.copy(raisedFlag = v))

    /** Spans the full container width (`.p-splitbutton-fluid`). */
    def fluid(v: Boolean): SplitButton = new SplitButton(p.copy(fluidFlag = v))

    /** Disables both segments. */
    def disabled(v: Boolean): SplitButton = new SplitButton(p.copy(disabledFlag = v))

    /** Leading icon on the primary segment. */
    def icon(glyph: IconGlyph): SplitButton = new SplitButton(p.copy(iconV = Present(glyph)))

    /** Glyph of the dropdown segment (default chevron-down). */
    def dropdownIcon(glyph: IconGlyph): SplitButton = new SplitButton(p.copy(dropdownIconV = Present(glyph)))

    /** The popup menu rows ([[MenuItem]], incl. `MenuItem.separator`), each built directly or from `_` (see [[MenuItemScope]]), or a
      * signal of rows for a list that changes while the page shows it (the requests a server holds, say). Read here, the list is
      * part of the button's one region: building the button again inside a region over the list would start its mount again with
      * every change, and with it its open state and highlight, and a click landing while it starts reaches nothing.
      */
    def items(is: MenuItemLiveEntry*): SplitButton = new SplitButton(p.copy(entriesV = entriesV ++ is))

    /** Binds the menu visibility. A `SignalRef` binds two way: the dropdown segment, a pick, Tab, Escape and an outside
      * click write it, then [[onOpenChange]] hears the new state. A plain `Signal` binds one way: the menu follows it
      * and only reports. A constant nobody hears is fixed: the menu stays as it is and the dropdown segment, still
      * reachable, changes nothing. Unbound, the button keeps the state itself, starting closed. A signal nothing hears
      * is reported in a card.
      */
    def open(v: Boolean | Signal[Boolean]): SplitButton = new SplitButton(p.copy(openSlot = openSlot.bind(v)))

    /** Fired with the menu visibility the reader asks for, after a two-way [[open]] is written. */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): SplitButton = new SplitButton(p.copy(openSlot = openSlot.hear(f)))

    /** Runs `action` when the PRIMARY segment is pressed. */
    def onClick(action: => UI.Action)(using Frame): SplitButton =
        new SplitButton(p.copy(onClickEff = Present(Callback(action))))

    private[uic] def render(using Frame): UI =
        // The panel visibility + keyboard highlight live in signals allocated by
        // this effectful mount; static projections render the same closed anatomy.
        val button = UI.mounted {
            for
                // Unbound, the button keeps the menu state itself, starting closed.
                open <- openSlot.owned(false)
                hi   <- Anchored.init(-1)
                base <- UI.id
            yield new SplitButton(p.copy(openSlot = open)).wiredOver(hi, base)
        }
        // A signal nobody hears: every toggle and pick the reader makes is painted away again.
        Diagnostics.before(button)("SplitButton")(Diagnostic.unheard(openSlot, SplitButton.openName))
    end render

    /** The subscription tree the mount publishes, over a two-way ref (golden-test seam). */
    private[uic] def wired(open: SignalRef[Boolean], hi: Anchored[Int], base: String)(using Frame): UI =
        new SplitButton(p.copy(openSlot = openSlot.bind(open))).wiredOver(hi, base)

    /** The subscription tree over the menu state however it is bound. */
    private def wiredOver(hi: Anchored[Int], base: String)(using Frame): UI =
        // The menu's items, its open state and its highlight are read here as ONE region (see [[ReactiveValue.and]]) and handed to
        // the hosted menu: leaving the menu writes the open state and the highlight together, and the menu's own region
        // nested in this one would repaint what it closed over.
        // The highlight follows its row over the roots shown ([[Anchored]], [[Menu.rowRebase]]).
        MenuRender.readRooted(entriesV).and(openSlot.reading(false)).and(hi.reading).render { case ((Rooted(roots, items), o), h) =>
            val hv = hi.view(h, roots)(using Menu.rowRebase)
            body(o, Present(hv), base, items, hv.value)
        }
    end wiredOver

    private def body(
        isOpen: Boolean,
        hi: Maybe[Anchored.View[Int]],
        base: String,
        items: List[MenuItem] = Nil,
        h: Int = -1
    )(using Frame): UI =
        def styled(b: Button): Button =
            val withSev = severityV match
                case SeverityValue.Const(s) => b.severity(s)
                case SeverityValue.Dyn(sig) => b.severity(sig)
            withSev.variant(variantV).size(sizeV).disabled(disabledFlag)
        end styled

        val primaryBase = labelV match
            case Present(TextValue.Const(l)) => Button(l)
            case Present(TextValue.Dyn(s))   => Button(s)
            case Absent                      => Button()
        var primary = styled(primaryBase.extraClass("p-splitbutton-button"))
        iconV.foreach(g => primary = primary.icon(g))
        if fluidFlag then primary = primary.fluid(true)
        onClickEff.foreach(e => primary = primary.onClick(e))

        // The placeholder has no state to change (`hi` Absent): the toggle is then a no-op.
        val toggle: UI.Action = if hi.isDefined then openSlot.update(!_) else ()
        // A constant nobody hears is fixed: the dropdown stays a tab stop and says it changes nothing.
        val fixed = openSlot.fixed

        var dropdown = styled(Button())
            .icon(dropdownIconV.getOrElse(Icons.chevronDown))
            .extraClass("p-splitbutton-dropdown")
            .accessibleName("Open menu")
            .ariaRaw("haspopup", "menu")
            .ariaRaw("expanded", isOpen.toString)
        if fixed then dropdown = dropdown.ariaDisabled(true)
        else if !disabledFlag then dropdown = dropdown.onClick(toggle)

        val panel: List[UI] = (openSlot.bound, hi) match
            case (Present(bound), Present(hiRef)) =>
                // The hosted menu announces its highlighted row off an id of its own, minted
                // from this button's, since nothing outside knows the panel is there to name it. It
                // closes through this button's binding, so its close is heard where this button's is.
                val menu = Menu().rows(items).popup(bound)
                List(openSlot.hearer.fold(menu)(f => menu.onPopupChange(f)).hosted(h, hiRef, s"$base-menu", isOpen))
            case _ => Nil

        var el = div.cssClass("p-splitbutton").cssClass("p-component")
        if roundedFlag then el = el.cssClass("p-splitbutton-rounded")
        if raisedFlag then el = el.cssClass("p-splitbutton-raised")
        if fluidFlag then el = el.cssClass("p-splitbutton-fluid")
        el(UI.children((primary: UI) :: (dropdown: UI) :: panel)*)
    end body
end SplitButton

object SplitButton:
    final private[uic] case class Props(
        labelV: Maybe[TextValue] = Absent,
        iconV: Maybe[IconGlyph] = Absent,
        severityV: SeverityValue = SeverityValue.Const(Severity.Primary),
        variantV: ButtonVariant = ButtonVariant.Filled,
        sizeV: Size = Size.Normal,
        roundedFlag: Boolean = false,
        raisedFlag: Boolean = false,
        fluidFlag: Boolean = false,
        disabledFlag: Boolean = false,
        dropdownIconV: Maybe[IconGlyph] = Absent,
        entriesV: List[MenuItemLiveEntry] = Nil,
        openSlot: Slot[Boolean] = Slot(),
        onClickEff: Maybe[UI.Action] = Absent
    )

    /** A split button with `label` as its primary label. A `Signal[String]` re-renders it in place on
      * emission (e.g. a locale-driven `I18n.t` leaf).
      */
    def apply(label: String | Signal[String]): SplitButton = new SplitButton(Props(labelV = Present(ReactiveValue(label))))

    /** An empty split button; add an icon and items via the setters. */
    def apply(): SplitButton = new SplitButton(Props())

    private val openName = Slot.Name("SplitButton", "the menu", "a toggle or a pick", "onOpenChange")
end SplitButton
