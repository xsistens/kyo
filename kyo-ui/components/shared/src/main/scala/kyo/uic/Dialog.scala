package kyo.uic

import kyo.*
import kyo.UI.*

/** Accessible role of a popup. Hand-authored; lives with [[Dialog]], the only
  * popup that exposes it. [[PopupAccessibleRole.None]] suppresses the `role`
  * attribute entirely.
  */
enum PopupAccessibleRole derives CanEqual:
    case None, Dialog, AlertDialog

    /** The ARIA `role` attribute value ([[PopupAccessibleRole.None]] emits no role). */
    private[uic] def roleAttr: Maybe[String] = this match
        case PopupAccessibleRole.None        => Absent
        case PopupAccessibleRole.Dialog      => Present("dialog")
        case PopupAccessibleRole.AlertDialog => Present("alertdialog")
end PopupAccessibleRole

/** Dialog: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Dialog
  * anatomy: the modal `div.p-dialog-mask.p-overlay-mask.p-overlay-mask-enter-active`
  * backdrop (base.css styles `.p-overlay-mask`; a `.p-uic-*` remainder centers
  * it, since Prime centers via inline styles) over `div[role=dialog].p-dialog.p-component
  * [.p-dialog-maximized]` > `div.p-dialog-header` (title +
  * `div.p-dialog-header-actions` with the icon-only close button) +
  * `div.p-dialog-content` + optional `div.p-dialog-footer`), so the extracted
  * `@primeuix` dialog CSS applies verbatim. The dialog keeps its proven fixed
  * backdrop approach: no floating-overlay primitive is involved.
  *
  * Visibility is the dialog's own unless [[open]] binds it, and an unbound
  * dialog starts open. Every internal close path (header close button, Escape,
  * a backdrop click) asks for false and reports it to [[onOpenChange]]. The hard
  * parts are delegated to kyo-ui primitives rather than rebuilt: visibility is a
  * `when(open)`
  * reactive boundary, and keyboard containment uses the element-level `focusTrap`
  * the kyo renderer already implements.
  *
  * `severity` accents the header (kyo extension: a subtle colored bottom border
  * plus a tinted leading icon under `.p-uic-dialog-<token>`); it is only visible
  * when a header is present. `maximized` renders Prime's `.p-dialog-maximized`
  * full-viewport state.
  *
  * Focus story (kyo's declarative `data-kyo-focus-*` client contract): the
  * dialog box carries `focusAuto` (opening seeds focus onto the freshly
  * inserted box, so Escape dismisses immediately without any prior click) and
  * `focusRestore` (closing returns focus to the element focused before the
  * open, typically the opener button). `preventInitialFocus(true)` /
  * `preventFocusRestore(true)` omit the respective attribute. To seed a
  * SPECIFIC child instead of the box, combine `preventInitialFocus(true)` with
  * `.focusAuto(true).focusRestore(true)` stamped directly on that child (the
  * client seeds the first `data-kyo-focus-auto` element of the inserted
  * region, root first; the box must therefore stand down).
  */
final case class Dialog private (
    openSlot: Maybe[ReactiveValue[Boolean]] = Absent,
    onOpenF: Maybe[Boolean => UI.Action] = Absent,
    headerText: Maybe[TextValue] = Absent,
    headerContent: Maybe[UI] = Absent,
    footerV: Maybe[UI] = Absent,
    closeOnBackdrop: Boolean = true,
    maximizedFlag: Boolean = false,
    draggableFlag: Boolean = false,
    resizableFlag: Boolean = false,
    severityV: Maybe[Severity] = Absent,
    initialFocusV: Maybe[String] = Absent,
    preventInitialFocusFlag: Boolean = false,
    preventFocusRestoreFlag: Boolean = false,
    accessibleNameV: Maybe[TextValue] = Absent,
    accessibleNameRefV: Maybe[String] = Absent,
    accessibleDescriptionV: Maybe[TextValue] = Absent,
    accessibleDescriptionRefV: Maybe[String] = Absent,
    accessibleRoleV: PopupAccessibleRole = PopupAccessibleRole.Dialog,
    boxClassesV: List[String] = Nil,
    quietFlag: Boolean = false,
    kids: List[UI] = Nil
) extends Node, HasAccessibleNameRef, HasAccessibleDescription:
    type Self = Dialog

    /** Binds visibility. A `SignalRef` binds two way: writes open and close the dialog, and every close the reader
      * asks for (the close button, Escape, a backdrop click) writes false, then [[onOpenChange]] hears it. A plain
      * `Signal` binds one way: the dialog follows it and only reports a close. A constant nobody hears is fixed: an open
      * dialog stays, its close button reachable and inert. Unbound, the dialog keeps the state itself and starts open,
      * the way an HTML `dialog` with `open` does, so it shows until the reader closes it. A signal nothing hears is
      * reported in a card.
      */
    def open(v: Boolean | Signal[Boolean]): Dialog = copy(openSlot = Present(ReactiveValue(v)))

    /** Fired with the visibility the reader asks for (false for every close), after a two-way [[open]] is written.
      * External writes fire nothing.
      */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): Dialog = copy(onOpenF = Present(Callback.of(f)))

    /** Header title. A `Signal[String]` re-renders it in place on emission, e.g. a
      * locale-driven `I18n.t` leaf.
      */
    def header(v: String | Signal[String]): Dialog = copy(headerText = Present(ReactiveValue(v)))

    /** Arbitrary header content replacing the default title span (the severity icon
      * and the close button are still composed around it).
      */
    def header(content: UI): Dialog = copy(headerContent = Present(content))

    /** Footer slot (typically right-aligned action buttons), Prime's
      * `div.p-dialog-footer`.
      */
    def footer(v: UI): Dialog = copy(footerV = Present(v))

    /** Whether clicking the backdrop closes the dialog (default true). */
    def closeOnBackdrop(v: Boolean): Dialog = copy(closeOnBackdrop = v)

    /** Grows the dialog to the full viewport: Prime's `.p-dialog-maximized`. */
    def maximized(v: Boolean): Dialog = copy(maximizedFlag = v)

    /** Makes the dialog draggable by its header (Prime's `draggable`, driven by
      * pointer-drag). The header becomes a stable pointer-capture surface; a drag
      * translates the box via an internal position ref. Reset to centered on each
      * open.
      */
    def draggable(v: Boolean): Dialog = copy(draggableFlag = v)

    /** Adds a bottom-right resize handle (Prime's `resizable`, driven by
      * pointer-drag): dragging it sets an explicit box width/height that follows
      * the cursor (seeded from a one-shot self-measure on grab).
      */
    def resizable(v: Boolean): Dialog = copy(resizableFlag = v)

    /** Semantic header accent (kyo extension: colored border + tinted icon under
      * `.p-uic-dialog-<token>`); only visible when a header is present.
      */
    def severity(v: Severity): Dialog = copy(severityV = Present(v))

    /** Superseded hint (kept for source compatibility, no attribute emitted): the
      * box seeds focus on open and the named child is reachable by Tab inside the
      * trap. To seed the child DIRECTLY, use `preventInitialFocus(true)` on the
      * dialog plus `.focusAuto(true).focusRestore(true)` on the child element.
      */
    def initialFocus(id: String): Dialog = copy(initialFocusV = Present(id))

    /** Suppresses the box's automatic focus seeding on open (omits
      * `data-kyo-focus-auto`). Escape then needs focus inside the dialog first.
      */
    def preventInitialFocus(v: Boolean): Dialog = copy(preventInitialFocusFlag = v)

    /** Suppresses the focus return to the opener after close (omits
      * `data-kyo-focus-restore`).
      */
    def preventFocusRestore(v: Boolean): Dialog = copy(preventFocusRestoreFlag = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): Dialog = copy(accessibleNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): Dialog = copy(accessibleNameRefV = v)

    private[uic] def withAccessibleDescription(v: Maybe[TextValue]): Dialog = copy(accessibleDescriptionV = v)

    /** Id(s) of describing element(s), emitted as `aria-describedby`. */
    def accessibleDescriptionRef(v: String): Dialog = copy(accessibleDescriptionRefV = Present(v))

    /** ARIA role of the dialog box (default [[PopupAccessibleRole.Dialog]]). */
    def accessibleRole(v: PopupAccessibleRole): Dialog = copy(accessibleRoleV = v)

    /** Package-internal class hook: hosts (ConfirmDialog) stamp Prime's skin
      * class (`p-confirmdialog`) onto the dialog box element.
      */
    private[uic] def boxClass(cls: String): Dialog = copy(boxClassesV = boxClassesV :+ cls)

    /** Package-internal: a host that reports its own binding mistakes (ConfirmDialog, whose choices are heard apart)
      * keeps the dialog from reporting the same binding a second time.
      */
    private[uic] def quiet: Dialog = copy(quietFlag = true)

    /** Adds content children. */
    def apply(cs: UI*): Dialog = copy(kids = kids ++ cs)

    /** The name a text header gives the dialog when the caller gave it none.
      *
      * A dialog needs an accessible name, and the header is the one Prime points `aria-labelledby`
      * at. The text itself is used rather than a reference to the title span, since a reference
      * needs an id and minting one needs a mount this path does not have; the name a reader hears
      * is the same. Custom header content carries no text to take, and a caller's own name or
      * reference always wins.
      */
    private def headerName: Maybe[TextValue] =
        if accessibleNameRefV.isDefined || headerContent.isDefined then Absent else headerText

    /** The severity's leading header glyph (shared vocabulary with Message's
      * design-derived defaults).
      */
    private def severityGlyph(s: Severity): IconGlyph = s match
        case Severity.Success => Icons.checkCircle
        case Severity.Warn    => Icons.exclamationTriangle
        case Severity.Danger  => Icons.timesCircle
        case _                => Icons.infoCircle

    private[uic] def render(using Frame): UI =
        openSlot match
            // Unbound, the dialog keeps its visibility itself and starts open. Before the mount runs it draws from a
            // signal, so the static page does not call its close button fixed.
            case Absent =>
                ReactiveValue.owned(true)(own => copy(openSlot = Present(own)).drawn)(
                    copy(openSlot = Present(ReactiveValue[Boolean](Signal.initConst(true)))).drawn
                )
            // A signal nobody hears: every close the reader asks for is painted away again.
            case _ if !quietFlag && ReactiveValue.deaf(openSlot, onOpenF) =>
                fragment(
                    KeyDiagnostics.card(
                        "Dialog",
                        "the visibility is bound one way and nothing hears a close, so the reader cannot close it; add " +
                            "onOpenChange, or bind a SignalRef",
                        Nil
                    ),
                    drawn
                )
            case _ => drawn
    end render

    private def drawn(using Frame): UI =
        openSlot match
            case Absent        => UI.empty
            case Present(slot) =>
                // A constant nobody hears is fixed: the dialog stays, and no close path is wired.
                val fixed = ReactiveValue.fixed(openSlot, onOpenF)
                // Every internal close path funnels through here: a two-way binding is written, then the close reported.
                val close: UI.Action = ReactiveValue.change(openSlot, false, onOpenF)
                // Shown while the binding says so: a constant needs no region, a signal is one boundary.
                def shown(ui: => UI): UI = slot.const match
                    case Present(v) => if v then ui else UI.empty
                    case Absent     => when(slot.signal)(ui)

                val titleChildren: List[UI] = headerContent match
                    case Present(c) => List(c)
                    case Absent     =>
                        headerText.toList.map {
                            case TextValue.Const(t) => span.cssClass("p-dialog-title")(t): UI
                            case TextValue.Dyn(s)   => span.cssClass("p-dialog-title")(s)
                        }

                // The header, optionally wired as a draggable pointer surface. When drag
                // handlers are present the header carries them directly: it is a STABLE
                // child of the box (built once), so pointer capture survives the per-frame
                // translate re-render; the handlers read the position ref
                // LIVE, never a closed-over render value.
                def headerBlock(drag: Maybe[(PointerEvent => UI.Action, PointerEvent => UI.Action)]): List[UI] =
                    if titleChildren.isEmpty then Nil
                    else
                        val severityIcon: List[UI] = severityV.toList.map { s =>
                            span.cssClass("p-uic-dialog-severity-icon")(
                                toChild(GlyphSvg(severityGlyph(s), "p-icon"))
                            )
                        }
                        val closeShell = button
                            .cssClass("p-dialog-close-button")
                            .cssClass("p-button")
                            .cssClass("p-component")
                            .cssClass("p-button-icon-only")
                            .cssClass("p-button-secondary")
                            .cssClass("p-button-rounded")
                            .cssClass("p-button-text")
                            .jsProp("type", "button")
                            .aria("label", "Close")
                        val closeBtn: UI =
                            (if fixed then closeShell.aria("disabled", "true") else closeShell.onClick(close)) (
                                toChild(GlyphSvg(Icons.times, "p-icon"))
                            )
                        val actions: UI = div.cssClass("p-dialog-header-actions")(toChild(closeBtn))
                        var hdr         = div.cssClass("p-dialog-header")
                        drag.foreach { (down, move) =>
                            hdr = hdr.cssClass("p-uic-dialog-draggable-header").onPointerDown(down).onPointerMove(move)
                        }
                        List(hdr(((severityIcon ++ titleChildren) :+ actions).map(toChild)*))

                val contentUI: List[UI] = List(div.cssClass("p-dialog-content")(kids.map(toChild)*))
                val footerUI: List[UI]  = footerV.toList.map(f => div.cssClass("p-dialog-footer")(toChild(f)))

                // Builds the whole box element. `animate` adds the open scale (dropped on
                // the draggable/resizable path, where the box re-renders per drag frame, so
                // re-triggering the from-state each frame would flicker); `pos`/`size`
                // apply the live translate/geometry; `inner` are the stable children.
                def buildBox(
                    idMaybe: Maybe[String],
                    animate: Boolean,
                    pos: Maybe[(Double, Double)],
                    size: Maybe[(Double, Double)],
                    inner: List[UI]
                ): UI =
                    var box = div.cssClass("p-dialog").cssClass("p-component").aria("modal", "true").focusTrap(true).tabIndex(-1)
                    // The reactive (movable) box carries an id + a no-transition class so the
                    // `.p-dialog` transform transition never smears the live drag translate.
                    idMaybe.foreach(id => box = box.id(id).cssClass("p-uic-dialog-movable"))
                    if animate then box = box.enterTransition("p-uic-enter-scale")
                    boxClassesV.foreach(c => box = box.cssClass(c))
                    accessibleRoleV.roleAttr.foreach(r => box = box.role(r))
                    if maximizedFlag then box = box.cssClass("p-dialog-maximized")
                    severityV.foreach { s =>
                        box = box.cssClass(s"p-uic-dialog-${s.token}").data("uic-severity", s.token)
                    }
                    if !preventInitialFocusFlag then box = box.focusAuto(true)
                    if !preventFocusRestoreFlag then box = box.focusRestore(true)
                    val name: Maybe[TextValue] = accessibleNameV.orElse(headerName)
                    name match
                        case Present(TextValue.Const(n)) => box = box.aria("label", n)
                        case Present(TextValue.Dyn(s))   => box = box.aria("label", s)
                        case Absent                      => ()
                    end match
                    accessibleNameRefV.foreach(n => box = box.aria("labelledby", n))
                    accessibleDescriptionV match
                        case Present(TextValue.Const(n)) => box = box.aria("description", n)
                        case Present(TextValue.Dyn(s))   => box = box.aria("description", s)
                        case Absent                      => ()
                    end match
                    accessibleDescriptionRefV.foreach(n => box = box.aria("describedby", n))
                    if !fixed then
                        box = box.onKeyDown { e =>
                            e.key match
                                case Keyboard.Escape => close
                                case _               => ()
                        }
                    end if
                    var sty = Style.empty
                    pos.foreach((x, y) => sty = sty.translate(x.px, y.px))
                    size.foreach((w, h) => sty = sty.width(w.px).height(h.px))
                    if pos.isDefined || size.isDefined then box = box.style(sty)
                    box(inner.map(toChild)*)
                end buildBox

                // Enter/leave: the mask fades in via a transient from-state
                // class (NOT Prime's `p-overlay-mask-enter-active`: applied permanently,
                // its fill-forwards keyframe paints the mask transparent, the documented
                // Chromium trap); on close the body-level leave ghost plays Prime's own
                // `p-overlay-mask-leave-active` keyframe, carrying the box out with it.
                // `onClickSelf`: only a click on the mask itself closes.
                def maskAround(boxUI: UI): UI =
                    val base = div
                        .cssClass("p-dialog-mask")
                        .cssClass("p-overlay-mask")
                        .enterTransition("p-uic-enter-fade")
                        .leaveTransition("p-overlay-mask-leave-active")
                    if closeOnBackdrop && !fixed then base.onClickSelf(close)(boxUI) else base(boxUI)
                end maskAround

                if !draggableFlag && !resizableFlag then
                    shown(maskAround(buildBox(Absent, animate = true, Absent, Absent, headerBlock(Absent) ++ contentUI ++ footerUI)))
                else
                    // Draggable/resizable (pointer-drag plus a self-measure on grab).
                    // The box position/size live in refs owned by this per-open mount; the
                    // reactive region re-styles the box element each frame while the header
                    // and resize handle (the pointer-capture surfaces) are STABLE children
                    // reused by the positional diff, so capture never detaches.
                    val staticPlaceholder =
                        maskAround(buildBox(Absent, animate = true, Absent, Absent, headerBlock(Absent) ++ contentUI ++ footerUI))
                    shown(
                        UI.mounted {
                            for
                                cmds   <- UI.commands
                                boxId  <- cmds.freshId
                                pos    <- Signal.initRef((0.0, 0.0))
                                grab   <- Signal.initRef((0.0, 0.0))
                                size   <- Signal.initRef[Maybe[(Double, Double)]](Absent)
                                origin <- Signal.initRef[Maybe[(Double, Double)]](Absent)
                            yield
                                val drag: Maybe[(PointerEvent => UI.Action, PointerEvent => UI.Action)] =
                                    if draggableFlag then
                                        Present(
                                            (
                                                (e: PointerEvent) =>
                                                    pos.get.map(p => grab.set((e.rectX + e.x - p._1, e.rectY + e.y - p._2))),
                                                (e: PointerEvent) =>
                                                    grab.get.map(g => pos.set((e.rectX + e.x - g._1, e.rectY + e.y - g._2)))
                                            )
                                        )
                                    else Absent
                                val resizeHandle: List[UI] =
                                    if resizableFlag then
                                        List(
                                            div
                                                .cssClass("p-uic-dialog-resize-handle")
                                                .aria("hidden", "true")
                                                // Seed the resize origin (box top-left) + current size from a
                                                // one-shot self-measure on grab; then follow the cursor.
                                                .onPointerDown((_: PointerEvent) =>
                                                    cmds.requestMeasureById(boxId).map {
                                                        case Present(r) =>
                                                            origin.set(Present((r.x, r.y))).andThen(size.set(Present((r.width, r.height))))
                                                        case Absent => ()
                                                    }
                                                )
                                                .onPointerMove((e: PointerEvent) =>
                                                    origin.get.map {
                                                        case Present((ox, oy)) =>
                                                            size.set(Present((
                                                                math.max(Dialog.MinW, e.rectX + e.x - ox),
                                                                math.max(Dialog.MinH, e.rectY + e.y - oy)
                                                            )))
                                                        case Absent => ()
                                                    }
                                                )
                                        )
                                    else Nil
                                val staticInner: List[UI] = headerBlock(drag) ++ contentUI ++ footerUI ++ resizeHandle
                                val boxUI                 = pos.render { p =>
                                    size.render(sz => buildBox(Present(boxId), animate = false, Present(p), sz, staticInner))
                                }
                                maskAround(boxUI)
                        }.placeholder(staticPlaceholder)
                    )
                end if
        end match
    end drawn
end Dialog

object Dialog:
    def apply(): Dialog = new Dialog()

    /** Minimum box dimensions (px) while resizing. */
    private[uic] val MinW = 200.0
    private[uic] val MinH = 120.0
end Dialog
