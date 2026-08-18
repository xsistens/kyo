package kyo.uic

import kyo.*
import kyo.UI.*

/** SplitButton — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * SplitButton anatomy: `div.p-splitbutton.p-component
  * [.p-splitbutton-raised|-rounded|-fluid]` > primary
  * `button.p-splitbutton-button.p-button` + attached dropdown trigger
  * `button.p-splitbutton-dropdown.p-button.p-button-icon-only`), so the
  * extracted `@primeuix` splitbutton CSS applies verbatim — the sheet fuses the
  * two buttons' radii and drops the inner border.
  *
  * The primary segment runs `onClick`; the chevron segment toggles a popup
  * [[Menu]] of the given [[MenuItem]]s — the real [[Overlay]] panel anchored on
  * the splitbutton root (the sheet's `position: relative`), with Prime's
  * `.p-splitbutton .p-menu { min-width: 100% }` sizing, outside-click/Escape
  * dismissal, focus seeding, and the Menu's ArrowDown/ArrowUp + Enter keyboard
  * navigation. `severity`/`variant`/`size`/`rounded`/`raised`/`fluid`/
  * `disabled` pass through to BOTH segments (Button precedent). `open(ref)`
  * optionally exposes the panel visibility (self-managed otherwise).
  */
final case class SplitButton private (
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
    itemsV: List[MenuItem] = Nil,
    openRefV: Maybe[SignalRef[Boolean]] = Absent,
    onClickEff: Maybe[Any < Async] = Absent
) extends Node:
    type Self = SplitButton

    /** Semantic accent — passed to both segments. */
    def severity(v: Severity): SplitButton = copy(severityV = SeverityValue.Const(v))

    /** Reactive accent — forwarded to the inner [[Button]], swapped IN PLACE via the class channel. */
    def severity(sig: Signal[Severity]): SplitButton = copy(severityV = SeverityValue.Dyn(sig))

    /** Rendering variant (Filled/Outlined/Text/Link) — passed to both segments. */
    def variant(v: ButtonVariant): SplitButton = copy(variantV = v)

    /** Size — passed to both segments. */
    def size(v: Size): SplitButton = copy(sizeV = v)

    /** Fully rounded outer corners (`.p-splitbutton-rounded`). */
    def rounded(v: Boolean): SplitButton = copy(roundedFlag = v)

    /** Elevation shadow (`.p-splitbutton-raised`). */
    def raised(v: Boolean): SplitButton = copy(raisedFlag = v)

    /** Spans the full container width (`.p-splitbutton-fluid`). */
    def fluid(v: Boolean): SplitButton = copy(fluidFlag = v)

    /** Disables both segments. */
    def disabled(v: Boolean): SplitButton = copy(disabledFlag = v)

    /** Leading icon on the primary segment. */
    def icon(glyph: IconGlyph): SplitButton = copy(iconV = Present(glyph))

    /** Glyph of the dropdown segment (default chevron-down). */
    def dropdownIcon(glyph: IconGlyph): SplitButton = copy(dropdownIconV = Present(glyph))

    /** The popup menu rows ([[MenuItem]], incl. `MenuItem.separator`). */
    def items(is: MenuItem*): SplitButton = copy(itemsV = itemsV ++ is.toList)

    /** Binds the panel visibility two-way to `ref` (optional — self-managed
      * otherwise).
      */
    def open(ref: SignalRef[Boolean]): SplitButton = copy(openRefV = Present(ref))

    /** Runs `action` when the PRIMARY segment is pressed. */
    def onClick(action: => Any < Async)(using Frame): SplitButton =
        copy(onClickEff = Present(Sync.defer(action)))

    private[uic] def render(using Frame): UI =
        // The panel visibility + keyboard highlight live in signals allocated by
        // this effectful mount; static projections render the same closed anatomy.
        val stat: UI = body(false, Absent, Absent)
        UI.mounted {
            for
                open <- openRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef(false)
                hi <- Signal.initRef(-1)
            yield wired(open, hi)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam). */
    private[uic] def wired(open: SignalRef[Boolean], hi: SignalRef[Int])(using Frame): UI =
        open.render(o => body(o, Present(open), Present(hi)))

    private def body(
        isOpen: Boolean,
        open: Maybe[SignalRef[Boolean]],
        hi: Maybe[SignalRef[Int]]
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

        // Explicitly typed effect: an untyped match would infer `Any` and land the
        // effect inert in the by-name handler.
        val toggle: Any < Async =
            open match
                case Present(r) => r.getAndUpdate(!_)
                case Absent     => ()

        var dropdown = styled(Button())
            .icon(dropdownIconV.getOrElse(Icons.chevronDown))
            .extraClass("p-splitbutton-dropdown")
            .accessibleName("Open menu")
            .ariaRaw("haspopup", "menu")
            .ariaRaw("expanded", isOpen.toString)
        if !disabledFlag then dropdown = dropdown.onClick(toggle)

        val panel: List[UI] = (open, hi) match
            case (Present(o), Present(h)) =>
                List(Menu().items(itemsV*).popup(o).wired(h))
            case _ => Nil

        var el = div.cssClass("p-splitbutton").cssClass("p-component")
        if roundedFlag then el = el.cssClass("p-splitbutton-rounded")
        if raisedFlag then el = el.cssClass("p-splitbutton-raised")
        if fluidFlag then el = el.cssClass("p-splitbutton-fluid")
        el(((primary: UI) :: (dropdown: UI) :: panel).map(toChild)*)
    end body
end SplitButton

object SplitButton:
    /** A split button whose primary segment is labelled `label`. */
    def apply(label: String): SplitButton = new SplitButton(labelV = Present(TextValue.Const(label)))

    /** A split button whose primary label tracks `label` — re-renders in place on
      * emission (e.g. a locale-driven `I18n.t` leaf).
      */
    def apply(label: Signal[String]): SplitButton = new SplitButton(labelV = Present(TextValue.Dyn(label)))

    /** An empty split button — add an icon and items via the setters. */
    def apply(): SplitButton = new SplitButton()
end SplitButton
