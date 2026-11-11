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

    /** Semantic accent, passed to both segments. A `Signal[Severity]` swaps the class IN PLACE through the inner [[Button]]s'
      * class channels.
      */
    def severity(v: Prop[Severity]): SplitButton = new SplitButton(p.copy(severityV = v))

    /** Rendering variant (Filled/Outlined/Text/Link), passed to both segments; a signal swaps the class in place. */
    def variant(v: Prop[ButtonVariant]): SplitButton = new SplitButton(p.copy(variantV = v))

    /** Size, passed to both segments; a signal swaps the class in place. */
    def size(v: Prop[Size]): SplitButton = new SplitButton(p.copy(sizeV = v))

    /** Fully rounded outer corners (`.p-splitbutton-rounded`), a signal toggling the class in place. */
    def rounded(v: Prop[Boolean]): SplitButton = new SplitButton(p.copy(roundedFlag = v))

    /** Elevation shadow (`.p-splitbutton-raised`), a signal toggling the class in place. */
    def raised(v: Prop[Boolean]): SplitButton = new SplitButton(p.copy(raisedFlag = v))

    /** Spans the full container width (`.p-splitbutton-fluid`), a signal toggling the class in place. */
    def fluid(v: Prop[Boolean]): SplitButton = new SplitButton(p.copy(fluidFlag = v))

    /** Disables both segments; a signal toggles them in place, and a click reads it when it comes. */
    def disabled(v: Prop[Boolean]): SplitButton = new SplitButton(p.copy(disabledFlag = v))

    /** Leading icon on the primary segment. */
    def icon(glyph: IconGlyph): SplitButton = new SplitButton(p.copy(iconV = Present(glyph)))

    /** Glyph of the dropdown segment (default chevron-down). */
    def dropdownIcon(glyph: IconGlyph): SplitButton = new SplitButton(p.copy(dropdownIconV = Present(glyph)))

    /** The popup menu rows ([[MenuItem]], incl. `MenuItem.separator`), each built directly or from `_` (see [[MenuItemScope]]), or a
      * signal of rows for a list that changes while the page shows it (the requests a server holds, say). Read here, the list is
      * part of the menu panel's one region, with the open state and the highlight, so a change of rows draws the panel again and
      * keeps the open state, the highlight and both segments.
      */
    def items(is: MenuItemLiveEntry*): SplitButton = new SplitButton(p.copy(entriesV = entriesV ++ is))

    /** Binds the menu visibility. A `SignalRef` binds two way: the dropdown segment, a pick, Tab, Escape and an outside
      * click write it, then [[onOpenChange]] hears the new state. A plain `Signal` binds one way: the menu follows it
      * and only reports. A constant nobody hears is fixed: the menu stays as it is and the dropdown segment, still
      * reachable, changes nothing. Unbound, the button keeps the state itself, starting closed. A signal nothing hears
      * is reported in a card.
      */
    def open(v: Prop[Boolean]): SplitButton = new SplitButton(p.copy(openSlot = openSlot.bind(v)))

    /** Fired with the menu visibility the reader asks for, after a two-way [[open]] is written. */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): SplitButton = new SplitButton(p.copy(openSlot = openSlot.hear(f)))

    /** Runs `action` when the PRIMARY segment is pressed. */
    def onClick(action: => UI.Action)(using Frame): SplitButton =
        new SplitButton(p.copy(onClickEff = Present(Callback(action))))

    /** One instance per place, drawn once: it holds the menu's open state where nothing binds it, its keyboard highlight and its
      * ids, and a host region that runs again hands it the run's props instead of starting it over (see [[SplitButton.drawn]]).
      */
    private[uic] def render(using Frame): UI =
        UI.mountedWith(p) { live =>
            for
                hi   <- Anchored.init(-1)
                base <- UI.id
            yield SplitButton.drawn(live, hi, base)
        }

    /** The button over a two-way ref with no instance of its own (golden-test seam): no run follows, so every prop is read as it is. */
    private[uic] def wired(open: SignalRef[Boolean], hi: Anchored[Int], base: String)(using Frame): UI =
        SplitButton.drawn(new UI.Live(p.copy(openSlot = openSlot.bind(open)), Absent), hi, base)
end SplitButton

object SplitButton:
    final private[uic] case class Props(
        labelV: Maybe[Prop[String]] = Absent,
        iconV: Maybe[IconGlyph] = Absent,
        severityV: Prop[Severity] = Severity.Primary,
        variantV: Prop[ButtonVariant] = ButtonVariant.Filled,
        sizeV: Prop[Size] = Size.Normal,
        roundedFlag: Prop[Boolean] = false,
        raisedFlag: Prop[Boolean] = false,
        fluidFlag: Prop[Boolean] = false,
        disabledFlag: Prop[Boolean] = false,
        dropdownIconV: Maybe[IconGlyph] = Absent,
        entriesV: List[MenuItemLiveEntry] = Nil,
        openSlot: Controlled[Boolean] = Controlled(start = Present(false)),
        onClickEff: Maybe[UI.Action] = Absent
    ) derives CanEqual

    /** A split button with `label` as its primary label. A `Signal[String]` patches it in place on emission (e.g. a locale-driven
      * `I18n.t` leaf).
      */
    def apply(label: Prop[String]): SplitButton = new SplitButton(Props(labelV = Present(label)))

    /** An empty split button; add an icon and items via the setters. */
    def apply(): SplitButton = new SplitButton(Props())

    private val openName = Controlled.Name("SplitButton", "the menu", "a toggle or a pick", "onOpenChange")

    /** What of the props builds the two segments and has no channel of its own, read as one value: no hearer and no click is in
      * it, so a run that only brings new ones leaves it equal and draws nothing again.
      *
      * @param clicks
      *   somebody hears the primary segment
      * @param fixed
      *   the open state is a constant nobody hears, so the dropdown says it changes nothing
      * @param deaf
      *   the open state is a signal bound one way that nobody hears
      */
    final private[uic] case class Base(
        icon: Maybe[IconGlyph],
        dropdownIcon: Maybe[IconGlyph],
        labelled: Boolean,
        clicks: Boolean,
        fixed: Boolean,
        deaf: Boolean
    ) derives CanEqual

    private object Base:
        def apply(q: Props): Base =
            Base(q.iconV, q.dropdownIconV, q.labelV.isDefined, q.onClickEff.isDefined, q.openSlot.fixed, q.openSlot.deaf)

    /** The split button, drawn once over `props`, which hand it `open` bound: to the instance's own ref where a run binds nothing,
      * to the host's binding where one does.
      *
      * Each place reads the field it shows: the label, severity, variant, size and `disabled` reach both inner [[Button]]s through
      * their channels, the shapes the root's class channels, and the dropdown's `aria-expanded` follows the open state through a
      * channel, so a run that changes one of them patches it alone. What builds the segments (the [[Base]]) is one `reactive`
      * block, drawn again only when a run changes it. The menu panel is the one region over the rows, the open state and the
      * highlight, which leaving the menu writes together. A click, a toggle and the menu's report read the run's binding and
      * hearers when they come, so they write that run's ref and reach that run's `onOpenChange`.
      */
    private def drawn(props: UI.Live[Props], hi: Anchored[Int], base: String)(using Frame): UI =
        given CanEqual[List[MenuItemLiveEntry], List[MenuItemLiveEntry]] = CanEqual.derived
        val text                                                         = Prop.map(props(_.labelV))(_.getOrElse(""))
        val severity                                                     = props(_.severityV)
        val variant                                                      = props(_.variantV)
        val size                                                         = props(_.sizeV)
        val disabled                                                     = props(_.disabledFlag)
        val fluid                                                        = props(_.fluidFlag)
        val open                                                         = props(_.openSlot.value(false))
        val expanded                                                     = Prop.map(open)(_.toString)
        val shape                                                        = props(Base(_))
        val entries                                                      = props(_.entriesV)
        val toggle: UI.Action                                            = props.call(_.openSlot).map(_.update(!_))
        val click: UI.Action                                             = props.call(_.onClickEff).map(_.getOrElse(Kyo.unit: UI.Action))
        // The menu reports what the reader asks for, and the run's binding takes it: its ref, then its hearer.
        val asked: Boolean => UI.Action = v => props.call(_.openSlot).map(_.change(v))

        def styled(b: Button): Button = b.severity(severity).variant(variant).size(size).disabled(disabled)

        // The highlight follows its row over the roots shown ([[Anchored]], [[Menu.rowRebase]]).
        val panel: UI = Prop.render(entries) { es =>
            MenuRender.readRooted(es).and(ReactiveValue[Boolean](open)).and(hi.reading).render { case ((Rooted(roots, items), o), h) =>
                val hv = hi.view(h, roots)(using Menu.rowRebase)
                Menu().rows(items).popup(open).onPopupChange(asked).hosted(hv.value, hv, s"$base-menu", o)
            }
        }

        UI.reactive {
            val b       = shape.read
            val labeled = styled((if b.labelled then Button(text) else Button()).extraClass("p-splitbutton-button")).fluid(fluid)
            val iconed  = b.icon.fold(labeled)(labeled.icon(_))
            val primary = if b.clicks then iconed.onClick(click) else iconed
            val drop    = styled(Button())
                .icon(b.dropdownIcon.getOrElse(Icons.chevronDown))
                .extraClass("p-splitbutton-dropdown")
                .accessibleName("Open menu")
                .ariaRaw(Aria.haspopup, "menu")
                .ariaRaw(Aria.expanded, expanded)
            // A constant nobody hears is fixed: the dropdown stays a tab stop and says it changes nothing.
            val dropdown = if b.fixed then drop.ariaDisabled(true) else drop.onClick(toggle)
            val root     = div.cssClass("p-splitbutton").cssClass("p-component")
                .when(props(_.roundedFlag))(_.cssClass("p-splitbutton-rounded"))
                .when(props(_.raisedFlag))(_.cssClass("p-splitbutton-raised"))
                .when(fluid)(_.cssClass("p-splitbutton-fluid"))
            // A signal nobody hears: every toggle and pick the reader makes is painted away again.
            Diagnostics.before(root((primary: UI), (dropdown: UI), panel))("SplitButton")(Diagnostic(b.deaf, openName.advice))
        }
    end drawn
end SplitButton
