package kyo.uic

import kyo.*
import kyo.UI.*

/** Popover: native kyo-ui, PrimeOne design (mirrors PrimeVue's Popover /
  * PrimeReact v10's OverlayPanel anatomy: floating `div.p-popover.p-component` >
  * `div.p-popover-content`, with the arrow drawn by the sheet's
  * `.p-popover:before/:after` border triangles), so the extracted `@primeuix`
  * popover CSS applies verbatim.
  *
  * Built ON the [[Overlay]] primitive: visibility is the popover's own unless
  * [[open]] binds it, an outside click or Escape closes (`dismissable(false)` turns both off), and the
  * panel seeds focus on open / restores it on close (`seedFocus(false)` opts
  * out). `trigger(...)` renders the toggling trigger and the panel together
  * inside the anchor wrapper (`div.p-uic-overlay-anchor.p-uic-popover-anchor`);
  * while open, a click on the trigger area lands on the Overlay backdrop and
  * closes (Prime's toggle semantics). Without a trigger the popover renders just
  * the floating panel: place it inside your own `position: relative` anchor
  * (stamp `p-uic-overlay-anchor`) and bind [[open]] yourself.
  *
  * The panel opens below the anchor by default; `anchor(TopStart|TopEnd)` opens
  * it above and stamps Prime's `.p-popover-flipped` (the arrow moves to the
  * bottom edge, pointing down). The arrow sits at the sheet's
  * `--p-popover-arrow-offset` from the panel's start edge (Prime positions it
  * under the pointer via JS measuring; server-side the offset is fixed).
  *
  * Honest limitations (inherited from [[Overlay]]): no flip/shift at viewport
  * edges, no portal, no enter/leave animation. Additionally the sheet's
  * `.p-popover { will-change: transform }` makes the PANEL the containing block
  * for `position: fixed` descendants: do not nest another overlay's backdrop
  * expectation inside popover content that must escape it.
  */
final class Popover private[uic] (private[uic] val p: Popover.Props) extends Node, HasProps:
    import p.*

    type Self = Popover

    /** Binds visibility. A `SignalRef` binds two way: the trigger, Escape and an outside click write it, then
      * [[onOpenChange]] hears the new state. A plain `Signal` binds one way: the popover follows it and only reports. A
      * constant nobody hears is fixed: the panel stays as it is and neither the trigger nor a dismissal changes it.
      * Unbound, the popover keeps the state itself: closed with a [[trigger]] to open it, open without one, so it shows
      * until the reader dismisses it. A signal nothing hears is reported in a card.
      */
    def open(v: Boolean | Signal[Boolean]): Popover = new Popover(p.copy(openSlot = openSlot.bind(v)))

    /** Fired with the visibility the reader asks for, after a two-way [[open]] is written. */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): Popover = new Popover(p.copy(openSlot = openSlot.hear(f)))

    /** The control that opens the popover: a click on it asks for the other state, and it says the popover's state to
      * the reader (that it opens a dialog, whether the dialog is open, and that it changes nothing where the state is
      * fixed). An element that is no control of its own (a `span`, a `div`) is made one, with a button role, a tab stop
      * and Enter and Space. See [[PopupTrigger]] for what can be a trigger.
      */
    def trigger(t: UI.Ast.Interactive | PopupTrigger): Popover = new Popover(p.copy(triggerV = Present(t)))

    /** Which anchor corner the panel opens from (default
      * [[OverlayAnchor.BottomStart]]); `Top*` flips the arrow to the bottom edge
      * (Prime's `.p-popover-flipped`).
      */
    def anchor(v: OverlayAnchor): Popover = new Popover(p.copy(anchorV = v))

    /** Whether an outside click or Escape closes the popover (default true,
      * Prime's `dismissable`).
      */
    def dismissable(v: Boolean): Popover = new Popover(p.copy(dismissableV = v))

    /** Whether the panel seeds focus on open and restores it on close (default
      * true).
      */
    def seedFocus(v: Boolean): Popover = new Popover(p.copy(seedFocusV = v))

    /** Whether the panel (and backdrop) re-home to `document.body` ([[Overlay.portal]]), the
      * escape from ancestor `overflow` clipping and `transform` containing blocks (default false).
      */
    def portal(v: Boolean): Popover = new Popover(p.copy(portalV = v))

    /** How the open panel responds to scroll outside it (default [[Overlay.Scroll.Close]],
      * Prime's hide-on-scroll). See [[Overlay.Scroll]].
      */
    def scroll(v: Overlay.Scroll): Popover = new Popover(p.copy(scrollV = v))

    /** Adds panel content. */
    def apply(cs: UI*): Popover = new Popover(p.copy(kids = kids ++ cs))

    private[uic] def render(using Frame): UI =
        // Unbound, the popover keeps its visibility itself: closed when a trigger can open it, open otherwise.
        Controlled.owning(openSlot, start = triggerV.isEmpty)(s => new Popover(this.p.copy(openSlot = s)).drawn)
    end render

    private def drawn(using Frame): UI =
        val flipped = anchorV == OverlayAnchor.TopStart || anchorV == OverlayAnchor.TopEnd
        var overlay = Overlay.withOpen(openSlot)
            .reportedAs("Popover")
            .anchor(anchorV)
            .portal(portalV)
            .scroll(scrollV)
            .matchWidth(false)
            .dismissOnOutsideClick(dismissableV)
            .dismissOnEscape(dismissableV)
            .seedFocus(seedFocusV)
            .panelClass("p-popover")
            .panelClass("p-component")
        if flipped then overlay = overlay.panelClass("p-popover-flipped")
        val panel: UI =
            overlay(div.cssClass("p-popover-content")(UI.children(kids)*)).render
        triggerV match
            case Present(t) =>
                // A constant nobody hears is fixed: the trigger stays, says so, and toggles nothing.
                val fixed                    = openSlot.fixed
                val expanded                 = PopupTrigger.expanded(openSlot.binding)
                val toggle: Maybe[UI.Action] = if fixed then Absent else Present(openSlot.update(!_))
                val control                  = PopupTrigger.stamped(t, "dialog", expanded, fixed, toggle)
                // The wrapper takes the click, which reaches it from the control, so a handler the page put on its own
                // control still runs.
                var shell = span.cssClass("p-uic-popover-trigger")
                toggle.foreach(act => shell = shell.onClick(act))
                div.cssClass("p-uic-overlay-anchor").cssClass("p-uic-popover-anchor")(
                    shell(control),
                    panel
                )
            case _ => panel
        end match
    end drawn
end Popover

object Popover:
    final private[uic] case class Props(
        openSlot: Controlled[Boolean] = Controlled(),
        triggerV: Maybe[UI.Ast.Interactive | PopupTrigger] = Absent,
        anchorV: OverlayAnchor = OverlayAnchor.BottomStart,
        dismissableV: Boolean = true,
        seedFocusV: Boolean = true,
        portalV: Boolean = false,
        scrollV: Overlay.Scroll = Overlay.Scroll.Close,
        kids: List[UI] = Nil
    )

    /** A popover over the visibility `open` (see [[Popover.open]]). */
    def apply(open: Boolean | Signal[Boolean]): Popover = new Popover(Props(openSlot = Controlled().bind(open)))

    /** An unbound popover: it keeps its visibility itself (see [[Popover.open]]). */
    def apply(): Popover = new Popover(Props())
end Popover
