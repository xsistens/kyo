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

    /** The directional slide from-state class the panel enters with — the panel
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

/** Drawer — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Drawer/Sidebar anatomy: the modal `div.p-drawer-mask.p-overlay-mask
  * .p-drawer-open.p-drawer-<position>` backdrop (base.css styles
  * `.p-overlay-mask`; a `.p-uic-*` remainder expresses Prime's inline mask
  * positioning as CSS) over `div[role=dialog].p-drawer.p-component` >
  * `div.p-drawer-header` (title + icon-only `button.p-drawer-close-button`) +
  * `div.p-drawer-content` + optional `div.p-drawer-footer`), so the extracted
  * `@primeuix` drawer CSS applies verbatim — the panel is sized and docked by
  * the sheet's positional rules (`.p-drawer-left .p-drawer { width: 20rem;
  * height: 100% }`, ...).
  *
  * [[Dialog]]'s edge-docked sibling — NOT an [[Overlay]] consumer: visibility
  * is a `when(openRef)` reactive boundary over a full-viewport mask, exactly
  * the proven Dialog machinery. Escape and (with `dismissable`, default true)
  * a mask click write `false` back; every internal close path (close button,
  * Escape, mask click) also fires `onClose` after the ref write. Focus story
  * like Dialog (kyo's declarative `data-kyo-focus-*` contract): the panel
  * carries `focusAuto` — opening seeds focus, so Escape dismisses without any
  * prior click — and `focusRestore` — closing returns focus to the opener —
  * plus the renderer's element-level `focusTrap`.
  *
  * Animations: the mask fades in on open and plays Prime's own
  * `p-overlay-mask-leave-active` keyframe on close (leave ghost). The panel
  * SLIDES directionally: on open a per-edge `p-uic-slide-enter-<pos>` from-state
  * animates it in via the sheet's own `.p-drawer { transition: transform 0.3s }`;
  * on close it slides back out its docking edge from a position-keyed
  * `.p-overlay-mask-leave-active .p-drawer` rule that rides the mask's leave
  * ghost (the panel is inside the mask subtree, so only the mask ghosts — the
  * rule slides the cloned panel as the ghost fades). `Full` fades (its sheet rule
  * sets `transition:none`). Note the sheet keeps Prime's identity `transform` on
  * the panel at rest, so the
  * drawer box is the containing block for `position: fixed` DESCENDANTS:
  * overlay-bearing components inside the drawer content confine their
  * backdrops to the drawer box. Non-modal mode (`modal(false)`, mask-less) is
  * deferred too — the mask always renders.
  */
final case class Drawer private (
    openRef: Maybe[SignalRef[Boolean]] = Absent,
    positionV: DrawerPosition = DrawerPosition.Left,
    headerText: Maybe[TextValue] = Absent,
    headerContent: Maybe[UI] = Absent,
    footerV: Maybe[UI] = Absent,
    dismissableFlag: Boolean = true,
    showCloseIconFlag: Boolean = true,
    preventInitialFocusFlag: Boolean = false,
    preventFocusRestoreFlag: Boolean = false,
    accessibleNameV: Maybe[TextValue] = Absent,
    accessibleNameRefV: Maybe[String] = Absent,
    onCloseEff: Maybe[Any < Async] = Absent,
    kids: List[UI] = Nil
) extends Node:
    type Self = Drawer

    /** Binds visibility two-way to `ref` — the only way to open/close the drawer. */
    def open(ref: SignalRef[Boolean]): Drawer = copy(openRef = Present(ref))

    /** Which edge the panel docks to (default [[DrawerPosition.Left]] — Prime). */
    def position(v: DrawerPosition): Drawer = copy(positionV = v)

    def header(v: String): Drawer = copy(headerText = Present(TextValue.Const(v)))

    /** Reactive `header` tracking `sig` — re-renders in place on emission, e.g. a
      * locale-driven `I18n.t` leaf.
      */
    def header(sig: Signal[String]): Drawer = copy(headerText = Present(TextValue.Dyn(sig)))

    /** Arbitrary header content replacing the default title (the close button is
      * still composed after it).
      */
    def header(content: UI): Drawer = copy(headerContent = Present(content))

    /** Footer slot — Prime's `div.p-drawer-footer`. */
    def footer(v: UI): Drawer = copy(footerV = Present(v))

    /** Whether clicking the mask closes the drawer (default true — Prime's
      * `dismissable`).
      */
    def dismissable(v: Boolean): Drawer = copy(dismissableFlag = v)

    /** Whether the header renders the icon-only close button (default true). */
    def showCloseIcon(v: Boolean): Drawer = copy(showCloseIconFlag = v)

    /** Suppresses the panel's automatic focus seeding on open (omits
      * `data-kyo-focus-auto`). Escape then needs focus inside the drawer first.
      */
    def preventInitialFocus(v: Boolean): Drawer = copy(preventInitialFocusFlag = v)

    /** Suppresses the focus return to the opener after close (omits
      * `data-kyo-focus-restore`).
      */
    def preventFocusRestore(v: Boolean): Drawer = copy(preventFocusRestoreFlag = v)

    /** Accessible name, emitted as `aria-label`. */
    def accessibleName(v: String): Drawer = copy(accessibleNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Drawer = copy(accessibleNameV = Present(TextValue.Dyn(sig)))

    /** Id(s) of labelling element(s), emitted as `aria-labelledby`. */
    def accessibleNameRef(v: String): Drawer = copy(accessibleNameRefV = Present(v))

    /** Runs `action` whenever the drawer closes itself (close button, Escape,
      * mask click) — after the `open` ref is written to `false`. External ref
      * writes do NOT fire it.
      */
    def onClose(action: => Any < Async)(using Frame): Drawer =
        copy(onCloseEff = Present(Sync.defer(action)))

    /** Adds content children. */
    def apply(cs: UI*): Drawer = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        openRef match
            case Absent       => UI.empty
            case Present(ref) =>
                // Every internal close path funnels through here: ref write, then callback.
                val close: Any < Async =
                    onCloseEff match
                        case Present(e) => ref.set(false).map(_ => e)
                        case Absent     => ref.set(false)

                val titleChildren: List[UI] = headerContent match
                    case Present(c) => List(c)
                    case Absent =>
                        headerText.toList.map {
                            case TextValue.Const(t) => div.cssClass("p-drawer-title")(t): UI
                            case TextValue.Dyn(s)   => s.render(t => div.cssClass("p-drawer-title")(t))
                        }

                val closeBtn: List[UI] =
                    if showCloseIconFlag then
                        List(
                            button
                                .cssClass("p-drawer-close-button")
                                .cssClass("p-button")
                                .cssClass("p-component")
                                .cssClass("p-button-icon-only")
                                .cssClass("p-button-secondary")
                                .cssClass("p-button-rounded")
                                .cssClass("p-button-text")
                                .jsProp("type", "button")
                                .aria("label", "Close")
                                .onClick(close)(toChild(GlyphSvg(Icons.times, "p-icon")))
                        )
                    else Nil

                // Prime renders the header row even without a title — it docks the
                // close button; both absent renders no header at all.
                val headerUI: List[UI] =
                    if titleChildren.isEmpty && closeBtn.isEmpty then Nil
                    else List(div.cssClass("p-drawer-header")((titleChildren ++ closeBtn).map(toChild)*))

                val contentUI: List[UI] = List(div.cssClass("p-drawer-content")(kids.map(toChild)*))
                val footerUI: List[UI]  = footerV.toList.map(f => div.cssClass("p-drawer-footer")(toChild(f)))

                var box = div
                    .cssClass("p-drawer")
                    .cssClass("p-component")
                    .role("dialog")
                    .aria("modal", "true")
                    .focusTrap(true)
                    .tabIndex(-1)
                    // Open slides the panel in from its docking edge (Full fades); its exit
                    // slides back out on the mask's leave ghost (the position-keyed
                    // `.p-overlay-mask-leave-active .p-drawer` rule in Theme).
                    .enterTransition(positionV.enterClass)
                if positionV == DrawerPosition.Full then box = box.cssClass("p-drawer-full")
                if !preventInitialFocusFlag then box = box.focusAuto(true)
                if !preventFocusRestoreFlag then box = box.focusRestore(true)
                accessibleNameV match
                    case Present(TextValue.Const(n)) => box = box.aria("label", n)
                    case Present(TextValue.Dyn(s))   => box = box.aria("label", s)
                    case Absent                      => ()
                end match
                accessibleNameRefV.foreach(n => box = box.aria("labelledby", n))
                box = box.onKeyDown { e =>
                    e.key match
                        case Keyboard.Escape => close
                        case _               => ()
                }
                val boxUI = box((headerUI ++ contentUI ++ footerUI).map(toChild)*)

                // Enter/leave: the mask fades in via a transient from-state
                // class (NOT Prime's permanent `p-overlay-mask-enter-active` — the Dialog
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
                    if dismissableFlag then base.onClickSelf(close)(boxUI) else base(boxUI)
                end mask

                when(ref)(mask)
end Drawer

object Drawer:
    def apply(): Drawer = new Drawer()
