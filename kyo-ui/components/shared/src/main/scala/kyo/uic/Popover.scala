package kyo.uic

import kyo.*
import kyo.UI.*

/** Popover — native kyo-ui, PrimeOne design (mirrors PrimeVue's Popover /
  * PrimeReact v10's OverlayPanel anatomy: floating `div.p-popover.p-component` >
  * `div.p-popover-content`, with the arrow drawn by the sheet's
  * `.p-popover:before/:after` border triangles), so the extracted `@primeuix`
  * popover CSS applies verbatim.
  *
  * Built ON the [[Overlay]] primitive: visibility binds two-way to `open`, an
  * outside click or Escape closes (`dismissable(false)` turns both off), and the
  * panel seeds focus on open / restores it on close (`seedFocus(false)` opts
  * out). `trigger(ui)` renders the toggling trigger and the panel together
  * inside the anchor wrapper (`div.p-uic-overlay-anchor.p-uic-popover-anchor`);
  * while open, a click on the trigger area lands on the Overlay backdrop and
  * closes — Prime's toggle semantics. Without a trigger the popover renders just
  * the floating panel: place it inside your own `position: relative` anchor
  * (stamp `p-uic-overlay-anchor`) and open the ref yourself.
  *
  * The panel opens below the anchor by default; `anchor(TopStart|TopEnd)` opens
  * it above and stamps Prime's `.p-popover-flipped` (the arrow moves to the
  * bottom edge, pointing down). The arrow sits at the sheet's
  * `--p-popover-arrow-offset` from the panel's start edge (Prime positions it
  * under the pointer via JS measuring — server-side the offset is fixed).
  *
  * Honest limitations (inherited from [[Overlay]]): no flip/shift at viewport
  * edges, no portal, no enter/leave animation. Additionally the sheet's
  * `.p-popover { will-change: transform }` makes the PANEL the containing block
  * for `position: fixed` descendants — do not nest another overlay's backdrop
  * expectation inside popover content that must escape it.
  */
final case class Popover private (
    openRef: SignalRef[Boolean],
    triggerV: Maybe[UI] = Absent,
    anchorV: OverlayAnchor = OverlayAnchor.BottomStart,
    dismissableV: Boolean = true,
    seedFocusV: Boolean = true,
    portalV: Boolean = false,
    scrollV: Overlay.Scroll = Overlay.Scroll.Close,
    kids: List[UI] = Nil
) extends Node:
    type Self = Popover

    /** The toggling trigger: renders `ui` inside the anchor wrapper with a click
      * handler that toggles the bound ref (open ⇄ closed).
      */
    def trigger(ui: UI): Popover = copy(triggerV = Present(ui))

    /** Which anchor corner the panel opens from (default
      * [[OverlayAnchor.BottomStart]]); `Top*` flips the arrow to the bottom edge
      * (Prime's `.p-popover-flipped`).
      */
    def anchor(v: OverlayAnchor): Popover = copy(anchorV = v)

    /** Whether an outside click or Escape closes the popover (default true —
      * Prime's `dismissable`).
      */
    def dismissable(v: Boolean): Popover = copy(dismissableV = v)

    /** Whether the panel seeds focus on open and restores it on close (default
      * true).
      */
    def seedFocus(v: Boolean): Popover = copy(seedFocusV = v)

    /** Whether the panel (and backdrop) re-home to `document.body` ([[Overlay.portal]]) — the
      * escape from ancestor `overflow` clipping and `transform` containing blocks (default false).
      */
    def portal(v: Boolean): Popover = copy(portalV = v)

    /** How the open panel responds to scroll outside it (default [[Overlay.Scroll.Close]] —
      * Prime's hide-on-scroll). See [[Overlay.Scroll]].
      */
    def scroll(v: Overlay.Scroll): Popover = copy(scrollV = v)

    /** Adds panel content. */
    def apply(cs: UI*): Popover = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        val flipped = anchorV == OverlayAnchor.TopStart || anchorV == OverlayAnchor.TopEnd
        var overlay = Overlay(openRef)
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
            overlay(div.cssClass("p-popover-content")(kids.map(toChild)*)).render

        triggerV match
            case Present(t) =>
                div.cssClass("p-uic-overlay-anchor").cssClass("p-uic-popover-anchor")(
                    toChild(
                        span
                            .cssClass("p-uic-popover-trigger")
                            .onClick(openRef.getAndUpdate(!_))(toChild(t))
                    ),
                    toChild(panel)
                )
            case Absent => panel
        end match
    end render
end Popover

object Popover:
    /** A popover whose visibility binds two-way to `open`: writes to the ref open
      * and close the panel; Escape and outside clicks write `false` back.
      */
    def apply(open: SignalRef[Boolean]): Popover = new Popover(open)
end Popover
