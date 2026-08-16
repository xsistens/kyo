package kyo.uic

import kyo.*
import kyo.UI.*

/** Accessible role of a popup. Hand-authored — lives with [[Dialog]], the only
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

/** Dialog — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Dialog
  * anatomy: the modal `div.p-dialog-mask.p-overlay-mask.p-overlay-mask-enter-active`
  * backdrop (base.css styles `.p-overlay-mask`; a `.p-uic-*` remainder centers
  * it — Prime centers via inline styles) over `div[role=dialog].p-dialog.p-component
  * [.p-dialog-maximized]` > `div.p-dialog-header` (title +
  * `div.p-dialog-header-actions` with the icon-only close button) +
  * `div.p-dialog-content` + optional `div.p-dialog-footer`), so the extracted
  * `@primeuix` dialog CSS applies verbatim. The dialog keeps its proven fixed
  * backdrop approach — no floating-overlay primitive is involved.
  *
  * Visibility binds two-way to a `SignalRef[Boolean]`: writes to the ref open and
  * close the dialog, Escape and (optionally) a backdrop click write `false` back.
  * Every internal close path (header close button, Escape, backdrop click) also
  * fires the `onClose` callback after the ref write. The hard parts are delegated
  * to kyo-ui primitives rather than rebuilt: visibility is a `when(openRef)`
  * reactive boundary, and keyboard containment uses the element-level `focusTrap`
  * the kyo renderer already implements.
  *
  * `severity` accents the header (kyo extension — a subtle colored bottom border
  * plus a tinted leading icon under `.p-uic-dialog-<token>`); it is only visible
  * when a header is present. `maximized` renders Prime's `.p-dialog-maximized`
  * full-viewport state.
  *
  * Focus story (kyo's declarative `data-kyo-focus-*` client contract): the
  * dialog box carries `focusAuto` — opening seeds focus onto the freshly
  * inserted box, so Escape dismisses immediately without any prior click — and
  * `focusRestore` — closing returns focus to the element focused before the
  * open (typically the opener button). `preventInitialFocus(true)` /
  * `preventFocusRestore(true)` omit the respective attribute. To seed a
  * SPECIFIC child instead of the box, combine `preventInitialFocus(true)` with
  * `.focusAuto(true).focusRestore(true)` stamped directly on that child (the
  * client seeds the first `data-kyo-focus-auto` element of the inserted
  * region, root first — the box must therefore stand down).
  */
final case class Dialog private (
    openRef: Maybe[SignalRef[Boolean]] = Absent,
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
    onCloseEff: Maybe[Any < Async] = Absent,
    boxClassesV: List[String] = Nil,
    kids: List[UI] = Nil
) extends Node:
    type Self = Dialog

    /** Binds visibility two-way to `ref` — the only way to open/close the dialog. */
    def open(ref: SignalRef[Boolean]): Dialog = copy(openRef = Present(ref))

    def header(v: String): Dialog = copy(headerText = Present(TextValue.Const(v)))

    /** Reactive `header` tracking `sig` — re-renders in place on emission, e.g. a
      * locale-driven `I18n.t` leaf.
      */
    def header(sig: Signal[String]): Dialog = copy(headerText = Present(TextValue.Dyn(sig)))

    /** Arbitrary header content replacing the default title span (the severity icon
      * and the close button are still composed around it).
      */
    def header(content: UI): Dialog = copy(headerContent = Present(content))

    /** Footer slot (typically right-aligned action buttons) — Prime's
      * `div.p-dialog-footer`.
      */
    def footer(v: UI): Dialog = copy(footerV = Present(v))

    /** Whether clicking the backdrop closes the dialog (default true). */
    def closeOnBackdrop(v: Boolean): Dialog = copy(closeOnBackdrop = v)

    /** Grows the dialog to the full viewport — Prime's `.p-dialog-maximized`. */
    def maximized(v: Boolean): Dialog = copy(maximizedFlag = v)

    /** Makes the dialog draggable by its header (Prime's `draggable`, mounted6
      * pointer-drag). The header becomes a stable pointer-capture surface; a drag
      * translates the box via an internal position ref. Reset to centered on each
      * open.
      */
    def draggable(v: Boolean): Dialog = copy(draggableFlag = v)

    /** Adds a bottom-right resize handle (Prime's `resizable`, mounted6
      * pointer-drag): dragging it sets an explicit box width/height that follows
      * the cursor (seeded from a one-shot self-measure on grab).
      */
    def resizable(v: Boolean): Dialog = copy(resizableFlag = v)

    /** Semantic header accent (kyo extension — colored border + tinted icon under
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

    /** Accessible name, emitted as `aria-label`. */
    def accessibleName(v: String): Dialog = copy(accessibleNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Dialog = copy(accessibleNameV = Present(TextValue.Dyn(sig)))

    /** Id(s) of labelling element(s), emitted as `aria-labelledby`. */
    def accessibleNameRef(v: String): Dialog = copy(accessibleNameRefV = Present(v))

    /** Accessible description, emitted as `aria-description`. */
    def accessibleDescription(v: String): Dialog = copy(accessibleDescriptionV = Present(TextValue.Const(v)))

    /** Reactive accessible description — `aria-description` patched IN PLACE via kyo-ui's
      * attribute channel (`setAttribute`, no re-render).
      */
    def accessibleDescription(sig: Signal[String]): Dialog = copy(accessibleDescriptionV = Present(TextValue.Dyn(sig)))

    /** Id(s) of describing element(s), emitted as `aria-describedby`. */
    def accessibleDescriptionRef(v: String): Dialog = copy(accessibleDescriptionRefV = Present(v))

    /** ARIA role of the dialog box (default [[PopupAccessibleRole.Dialog]]). */
    def accessibleRole(v: PopupAccessibleRole): Dialog = copy(accessibleRoleV = v)

    /** Runs `action` whenever the dialog closes itself (close button, Escape,
      * backdrop click) — after the `open` ref is written to `false`. External ref
      * writes do NOT fire it.
      */
    def onClose(action: => Any < Async)(using Frame): Dialog =
        copy(onCloseEff = Present(Sync.defer(action)))

    /** Package-internal class hook: hosts (ConfirmDialog) stamp Prime's skin
      * class (`p-confirmdialog`) onto the dialog box element.
      */
    private[uic] def boxClass(cls: String): Dialog = copy(boxClassesV = boxClassesV :+ cls)

    /** Adds content children. */
    def apply(cs: UI*): Dialog = copy(kids = kids ++ cs)

    /** The severity's leading header glyph (shared vocabulary with Message's
      * design-derived defaults).
      */
    private def severityGlyph(s: Severity): IconGlyph = s match
        case Severity.Success => Icons.checkCircle
        case Severity.Warn    => Icons.exclamationTriangle
        case Severity.Danger  => Icons.timesCircle
        case _                => Icons.infoCircle

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
                            case TextValue.Const(t) => span.cssClass("p-dialog-title")(t): UI
                            case TextValue.Dyn(s)   => s.render(t => span.cssClass("p-dialog-title")(t))
                        }

                // The header, optionally wired as a draggable pointer surface. When drag
                // handlers are present the header carries them directly — it is a STABLE
                // child of the box (built once), so pointer capture survives the per-frame
                // translate re-render (gotcha #1); the handlers read the position ref
                // LIVE, never a closed-over render value.
                def headerBlock(drag: Maybe[(PointerEvent => Any < Async, PointerEvent => Any < Async)]): List[UI] =
                    if titleChildren.isEmpty then Nil
                    else
                        val severityIcon: List[UI] = severityV.toList.map { s =>
                            span.cssClass("p-uic-dialog-severity-icon")(
                                toChild(GlyphSvg(severityGlyph(s), "p-icon"))
                            )
                        }
                        val closeBtn: UI = button
                            .cssClass("p-dialog-close-button")
                            .cssClass("p-button")
                            .cssClass("p-component")
                            .cssClass("p-button-icon-only")
                            .cssClass("p-button-secondary")
                            .cssClass("p-button-rounded")
                            .cssClass("p-button-text")
                            .jsProp("type", "button")
                            .aria("label", "Close")
                            .onClick(close)(toChild(GlyphSvg(Icons.times, "p-icon")))
                        val actions: UI = div.cssClass("p-dialog-header-actions")(toChild(closeBtn))
                        var hdr         = div.cssClass("p-dialog-header")
                        drag.foreach { (down, move) =>
                            hdr = hdr.cssClass("p-uic-dialog-draggable-header").onPointerDown(down).onPointerMove(move)
                        }
                        List(hdr(((severityIcon ++ titleChildren) :+ actions).map(toChild)*))

                val contentUI: List[UI] = List(div.cssClass("p-dialog-content")(kids.map(toChild)*))
                val footerUI: List[UI]  = footerV.toList.map(f => div.cssClass("p-dialog-footer")(toChild(f)))

                // Builds the whole box element. `animate` adds the open scale (dropped on
                // the draggable/resizable path, where the box re-renders per drag frame —
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
                    accessibleNameV match
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
                    box = box.onKeyDown { e =>
                        e.key match
                            case Keyboard.Escape => close
                            case _               => ()
                    }
                    var sty = Style.empty
                    pos.foreach((x, y) => sty = sty.translate(x.px, y.px))
                    size.foreach((w, h) => sty = sty.width(w.px).height(h.px))
                    if pos.isDefined || size.isDefined then box = box.style(sty)
                    box(inner.map(toChild)*)
                end buildBox

                // Enter/leave (Welle P): the mask fades in via a transient from-state
                // class (NOT Prime's `p-overlay-mask-enter-active` — applied permanently
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
                    if closeOnBackdrop then base.onClickSelf(close)(boxUI) else base(boxUI)
                end maskAround

                if !draggableFlag && !resizableFlag then
                    when(ref)(maskAround(buildBox(Absent, animate = true, Absent, Absent, headerBlock(Absent) ++ contentUI ++ footerUI)))
                else
                    // Draggable/resizable (mounted6 pointer-drag + mounted7 self-measure).
                    // The box position/size live in refs owned by this per-open mount; the
                    // reactive region re-styles the box element each frame while the header
                    // and resize handle — the pointer-capture surfaces — are STABLE children
                    // reused by the positional diff, so capture never detaches.
                    val staticPlaceholder =
                        maskAround(buildBox(Absent, animate = true, Absent, Absent, headerBlock(Absent) ++ contentUI ++ footerUI))
                    when(ref)(
                        UI.mounted {
                            for
                                cmds   <- UI.commands
                                boxId  <- cmds.freshId
                                pos    <- Signal.initRef((0.0, 0.0))
                                grab   <- Signal.initRef((0.0, 0.0))
                                size   <- Signal.initRef[Maybe[(Double, Double)]](Absent)
                                origin <- Signal.initRef[Maybe[(Double, Double)]](Absent)
                            yield
                                val drag: Maybe[(PointerEvent => Any < Async, PointerEvent => Any < Async)] =
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
                                                    cmds.requestMeasureById(boxId).map(r =>
                                                        origin.set(Present((r.x, r.y))).andThen(size.set(Present((r.width, r.height))))
                                                    )
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
                                val boxUI = pos.render { p =>
                                    size.render(sz => buildBox(Present(boxId), animate = false, Present(p), sz, staticInner))
                                }
                                maskAround(boxUI)
                        }.placeholder(staticPlaceholder)
                    )
                end if
end Dialog

object Dialog:
    def apply(): Dialog = new Dialog()

    /** Minimum box dimensions (px) while resizing. */
    private[uic] val MinW = 200.0
    private[uic] val MinH = 120.0
end Dialog
