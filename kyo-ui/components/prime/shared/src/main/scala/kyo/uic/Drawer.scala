package kyo.uic

import kyo.*
import kyo.UI.*

/** Which viewport edge a [[Drawer]] docks to (Prime's `position` prop);
  * `Full` covers the whole viewport (`.p-drawer-full`).
  */
enum DrawerPosition derives CanEqual:
    case Left, Right, Top, Bottom, Full

    /** The `.p-drawer-<token>` mask modifier class suffix. */
    private[uic] def token: String = this match
        case DrawerPosition.Left   => "left"
        case DrawerPosition.Right  => "right"
        case DrawerPosition.Top    => "top"
        case DrawerPosition.Bottom => "bottom"
        case DrawerPosition.Full   => "full"

    /** The directional slide from-state class the panel enters with: the panel
      * transitions from this edge to its resting `translate3d(0,0,0)` (the sheet's
      * own `.p-drawer { transition: transform }`). `Full` has no directional slide
      * (its sheet rule sets `transition:none`), so it rides the mask fade.
      */
    private[uic] def enterClass: String = this match
        case DrawerPosition.Left   => "p-uic-slide-enter-left"
        case DrawerPosition.Right  => "p-uic-slide-enter-right"
        case DrawerPosition.Top    => "p-uic-slide-enter-top"
        case DrawerPosition.Bottom => "p-uic-slide-enter-bottom"
        case DrawerPosition.Full   => "p-uic-enter-fade"
end DrawerPosition

/** Drawer: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Drawer/Sidebar anatomy: the modal `div.p-drawer-mask.p-overlay-mask
  * .p-drawer-open.p-drawer-<position>` backdrop (base.css styles
  * `.p-overlay-mask`; a `.p-uic-*` remainder expresses Prime's inline mask
  * positioning as CSS) over `div[role=dialog].p-drawer.p-component` >
  * `div.p-drawer-header` (title + icon-only `button.p-drawer-close-button`) +
  * `div.p-drawer-content` + optional `div.p-drawer-footer`), so the extracted
  * `@primeuix` drawer CSS applies verbatim: the panel is sized and docked by
  * the sheet's positional rules (`.p-drawer-left .p-drawer { width: 20rem;
  * height: 100% }`, ...).
  *
  * [[Dialog]]'s edge-docked sibling, NOT an [[Overlay]] consumer: visibility
  * is a `when(open)` reactive boundary over a full-viewport mask, exactly
  * the proven Dialog machinery, and an unbound drawer keeps it itself and
  * starts open. Every internal close path (close button, Escape and, with
  * `dismissable`, default true, a mask click) asks for false and reports it to
  * [[onOpenChange]]. Focus story
  * like Dialog (kyo's declarative `data-kyo-focus-*` contract): the panel
  * carries `focusAuto` (opening seeds focus, so Escape dismisses without any
  * prior click) and `focusRestore` (closing returns focus to the opener),
  * plus the renderer's element-level `focusTrap`.
  *
  * Animations: the mask fades in on open and plays Prime's own
  * `p-overlay-mask-leave-active` keyframe on close (leave ghost). The panel
  * SLIDES directionally: on open a per-edge `p-uic-slide-enter-<pos>` from-state
  * animates it in via the sheet's own `.p-drawer { transition: transform 0.3s }`;
  * on close it slides back out its docking edge from a position-keyed
  * `.p-overlay-mask-leave-active .p-drawer` rule that rides the mask's leave
  * ghost (the panel is inside the mask subtree, so only the mask ghosts; the
  * rule slides the cloned panel as the ghost fades). `Full` fades (its sheet rule
  * sets `transition:none`). Note the sheet keeps Prime's identity `transform` on
  * the panel at rest, so the
  * drawer box is the containing block for `position: fixed` DESCENDANTS:
  * overlay-bearing components inside the drawer content confine their
  * backdrops to the drawer box. Non-modal mode (`modal(false)`, mask-less) is
  * deferred too: the mask always renders.
  */
final class Drawer private[uic] (private[uic] val p: Drawer.Props) extends Node, HasAccessibleNameRef, HasProps:
    import p.*

    type Self = Drawer

    /** Binds visibility. A `SignalRef` binds two way: writes open and close the drawer, and every close the reader
      * asks for (the close button, Escape, a mask click) writes false, then [[onOpenChange]] hears it. A plain `Signal` binds one way: the drawer
      * follows it and only reports a close. A constant nobody hears is fixed: an open drawer stays, its close button
      * reachable and inert. Unbound, the drawer keeps the state itself and starts open, so it shows until the reader
      * closes it. A signal nothing hears is reported in a card.
      */
    def open(v: Boolean | Signal[Boolean]): Drawer = new Drawer(p.copy(openSlot = openSlot.bind(v)))

    /** Fired with the visibility the reader asks for (false for every close), after a two-way [[open]] is written.
      * External writes fire nothing.
      */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): Drawer = new Drawer(p.copy(openSlot = openSlot.hear(f)))

    /** Which edge the panel docks to (default [[DrawerPosition.Left]], as in Prime). */
    def position(v: DrawerPosition): Drawer = new Drawer(p.copy(positionV = v))

    /** Header title. A `Signal[String]` re-renders it in place on emission, e.g. a
      * locale-driven `I18n.t` leaf.
      */
    def header(v: String | Signal[String]): Drawer = new Drawer(p.copy(headerText = Present(ReactiveValue(v))))

    /** Arbitrary header content replacing the default title (the close button is
      * still composed after it).
      */
    def header(content: UI): Drawer = new Drawer(p.copy(headerContent = Present(content)))

    /** Footer slot: Prime's `div.p-drawer-footer`. */
    def footer(v: UI): Drawer = new Drawer(p.copy(footerV = Present(v)))

    /** Whether clicking the mask closes the drawer (default true, Prime's
      * `dismissable`).
      */
    def dismissable(v: Boolean): Drawer = new Drawer(p.copy(dismissableFlag = v))

    /** Whether the header renders the icon-only close button (default true). */
    def showCloseIcon(v: Boolean): Drawer = new Drawer(p.copy(showCloseIconFlag = v))

    /** Suppresses the panel's automatic focus seeding on open (omits
      * `data-kyo-focus-auto`). Escape then needs focus inside the drawer first.
      */
    def preventInitialFocus(v: Boolean): Drawer = new Drawer(p.copy(preventInitialFocusFlag = v))

    /** Suppresses the focus return to the opener after close (omits
      * `data-kyo-focus-restore`).
      */
    def preventFocusRestore(v: Boolean): Drawer = new Drawer(p.copy(preventFocusRestoreFlag = v))

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new Drawer(p.copy(access = v))

    /** Adds content children. */
    def apply(cs: UI*): Drawer = new Drawer(p.copy(kids = kids ++ cs))

    private[uic] def render(using Frame): UI =
        // Unbound, the drawer keeps its visibility itself and starts open. Before the mount runs it draws from a stand-in, so
        // the static page does not call its close button fixed.
        Controlled.draw(openSlot, Drawer.openName, start = true)(s => new Drawer(this.p.copy(openSlot = s)).drawn)

    private def drawn(using Frame): UI =
        openSlot.binding match
            case Absent        => UI.empty
            case Present(slot) =>
                // A constant nobody hears is fixed: the drawer stays, and no close path is wired.
                val fixed = openSlot.fixed
                // Every internal close path funnels through here: a two-way binding is written, then the close reported.
                val close: UI.Action = openSlot.change(false)
                // Shown while the binding says so: a constant needs no region, a signal is one boundary.
                def shown(ui: => UI): UI = slot.const match
                    case Present(v) => if v then ui else UI.empty
                    case Absent     => when(slot.signal)(ui)

                val titleChildren: List[UI] = headerContent match
                    case Present(c) => List(c)
                    case Absent     =>
                        headerText.toList.map {
                            case TextValue.Const(t) => div.cssClass("p-drawer-title")(t): UI
                            case TextValue.Dyn(s)   => div.cssClass("p-drawer-title")(s)
                        }

                val closeBtn: List[UI] =
                    if showCloseIconFlag then
                        val shell = button
                            .cssClass("p-drawer-close-button")
                            .cssClass("p-button")
                            .cssClass("p-component")
                            .cssClass("p-button-icon-only")
                            .cssClass("p-button-secondary")
                            .cssClass("p-button-rounded")
                            .cssClass("p-button-text")
                            .jsProp("type", "button")
                            .aria(Aria.label -> "Close")
                        val pressable = if fixed then shell.aria(Aria.disabled -> "true") else shell.onClick(close)
                        List(pressable(GlyphSvg(Icons.times, "p-icon")))
                    else Nil

                // Prime renders the header row even without a title, since it docks the
                // close button; both absent renders no header at all.
                val headerUI: List[UI] =
                    if titleChildren.isEmpty && closeBtn.isEmpty then Nil
                    else List(div.cssClass("p-drawer-header")(UI.children(titleChildren ++ closeBtn)*))

                val contentUI: List[UI] = List(div.cssClass("p-drawer-content")(UI.children(kids)*))
                val footerUI: List[UI]  = footerV.toList.map(f => div.cssClass("p-drawer-footer")(f))

                var box = div
                    .cssClass("p-drawer")
                    .cssClass("p-component")
                    .role("dialog")
                    .aria(Aria.modal -> "true")
                    .focusTrap(true)
                    .tabIndex(-1)
                    // Open slides the panel in from its docking edge (Full fades); its exit
                    // slides back out on the mask's leave ghost (the position-keyed
                    // `.p-overlay-mask-leave-active .p-drawer` rule in Theme).
                    .enterTransition(positionV.enterClass)
                if positionV == DrawerPosition.Full then box = box.cssClass("p-drawer-full")
                if !preventInitialFocusFlag then box = box.focusAuto(true)
                if !preventFocusRestoreFlag then box = box.focusRestore(true)
                // A text header names the drawer when the caller gave it no name, as in Dialog.
                val headerName: Maybe[Prop[String]] =
                    if access.nameRef.isDefined || headerContent.isDefined then Absent
                    else
                        headerText.map {
                            case TextValue.Const(t) => t
                            case TextValue.Dyn(s)   => s
                        }
                access.name.orElse(headerName).foreach(n => box = box.aria(Aria.label -> n))
                access.nameRef.foreach(n => box = box.aria(Aria.labelledby -> n))
                if !fixed then
                    box = box.onKeyDown { e =>
                        e.key match
                            case Keyboard.Escape => close
                            case _               => ()
                    }
                end if
                val boxUI = box(UI.children(headerUI ++ contentUI ++ footerUI)*)

                // Enter/leave: the mask fades in via a transient from-state
                // class (NOT Prime's permanent `p-overlay-mask-enter-active`: the Dialog
                // transparent-mask trap); on close the leave ghost plays Prime's own
                // `p-overlay-mask-leave-active` keyframe. `onClickSelf`: only clicks
                // landing on the mask itself dismiss; clicks inside the panel must not.
                val mask =
                    val base = div
                        .cssClass("p-drawer-mask")
                        .cssClass("p-overlay-mask")
                        .cssClass("p-drawer-open")
                        .cssClass(s"p-drawer-${positionV.token}")
                        .enterTransition("p-uic-enter-fade")
                        .leaveTransition("p-overlay-mask-leave-active")
                    if dismissableFlag && !fixed then base.onClickSelf(close)(boxUI) else base(boxUI)
                end mask

                shown(mask)
        end match
    end drawn
end Drawer

object Drawer:
    final private[uic] case class Props(
        openSlot: Controlled[Boolean] = Controlled(),
        positionV: DrawerPosition = DrawerPosition.Left,
        headerText: Maybe[TextValue] = Absent,
        headerContent: Maybe[UI] = Absent,
        footerV: Maybe[UI] = Absent,
        dismissableFlag: Boolean = true,
        showCloseIconFlag: Boolean = true,
        preventInitialFocusFlag: Boolean = false,
        preventFocusRestoreFlag: Boolean = false,
        access: Accessibility = Accessibility.none,
        kids: List[UI] = Nil
    )

    def apply(): Drawer = new Drawer(Props())

    private val openName = Controlled.Name("Drawer", "the visibility", "a close", "onOpenChange")
end Drawer
