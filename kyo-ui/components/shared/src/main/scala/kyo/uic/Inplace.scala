package kyo.uic

import kyo.*
import kyo.UI.*

/** Inplace: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Inplace anatomy: `div.p-inplace.p-component[aria-live=polite]` > EITHER the
  * clickable `div.p-inplace-display[role=button]` OR the active
  * `div.p-inplace-content`, never both), so the extracted `@primeuix` inplace
  * CSS applies verbatim.
  *
  * Which side shows is the inplace's own unless [[active]] binds it:
  * clicking the display (or pressing Enter or Space on it) asks for the
  * content, and the close button for the display. `closable(true)` appends
  * that icon-only close button to the content (PrimeReact's `closable`). `disabled(true)` renders the display side inert (`.p-disabled`,
  * Prime's open guard). [[onActiveChange]] hears the reader ask for either side
  * (external ref writes fire nothing).
  */
final class Inplace private[uic] (private[uic] val p: Inplace.Props) extends Node, HasProps:
    import p.*

    type Self = Inplace

    /** The inactive side (`.p-inplace-display`); clicking it activates the content. */
    def display(u: UI): Inplace = new Inplace(p.copy(displayV = Present(u)))

    /** The active side (`.p-inplace-content`), shown while the ref is true. */
    def content(u: UI): Inplace = new Inplace(p.copy(contentV = Present(u)))

    /** Binds which side shows. A `SignalRef` binds two way: activating the display writes true and the close button
      * false, then [[onActiveChange]] hears it. A plain `Signal` binds one way: the inplace follows it and only reports.
      * A constant nobody hears is fixed: the side it names stays, reachable and inert. Unbound, the inplace keeps the
      * state itself, starting on the display side. A signal nothing hears is reported in a card.
      */
    def active(v: Boolean | Signal[Boolean]): Inplace = new Inplace(p.copy(activeSlot = activeSlot.bind(v)))

    /** Fired with the side the reader asks for (true for the content, false for the display), after a two-way
      * [[active]] is written. One channel for both ways, since an open and a close are never both.
      */
    def onActiveChange(f: Boolean => UI.Action)(using Frame): Inplace = new Inplace(p.copy(activeSlot = activeSlot.hear(f)))

    /** Appends a close button to the content that deactivates (sets the ref false). */
    def closable(v: Boolean): Inplace = new Inplace(p.copy(closableFlag = v))

    /** Renders the display side inert: `.p-disabled` look, no activation (Prime's
      * `disabled` open guard; the content side is unaffected).
      */
    def disabled(v: Boolean): Inplace = new Inplace(p.copy(disabledFlag = v))

    private def displaySide(using Frame): UI =
        var d = div.cssClass("p-inplace-display").role("button").tabIndex(0)
        if disabledFlag then d = d.cssClass("p-disabled")
        else if activeSlot.fixed then d = d.aria("disabled", "true")
        else
            val open = activeSlot.change(true)
            // A `role="button"` owes the reader both keys, and a div is not a button, so the
            // browser supplies neither. Enter alone was half of the contract.
            d = d.onClick(open).onKeyDown { evt =>
                evt.key match
                    case Keyboard.Enter | Keyboard.Space => open
                    case _                               => ()
            }
        end if
        d(UI.children(displayV.toList)*)
    end displaySide

    private def contentSide(using Frame): UI =
        val closeBtn: List[UI] =
            if !closableFlag then Nil
            else
                val b = button
                    .cssClass("p-button")
                    .cssClass("p-component")
                    .cssClass("p-button-icon-only")
                    .jsProp("type", "button")
                    .aria("label", "Close")
                val pressable = if activeSlot.fixed then b.aria("disabled", "true") else b.onClick(activeSlot.change(false))
                List(pressable(GlyphSvg(Icons.times, "p-button-icon")))
        div.cssClass("p-inplace-content")(UI.children(contentV.toList ++ closeBtn)*)
    end contentSide

    private[uic] def render(using Frame): UI =
        // Unbound, the inplace keeps the side itself, starting on the display. Before the mount runs it draws the display from a
        // stand-in, so the static page does not call it fixed.
        Slot.draw(activeSlot, Inplace.activeName, start = false)(s => new Inplace(this.p.copy(activeSlot = s)).drawn)

    private def drawn(using Frame): UI =
        div.cssClass("p-inplace").cssClass("p-component").aria("live", "polite")(
            activeSlot.reading(false).render(a => if a then contentSide else displaySide)
        )
end Inplace

object Inplace:
    final private[uic] case class Props(
        displayV: Maybe[UI] = Absent,
        contentV: Maybe[UI] = Absent,
        activeSlot: Slot[Boolean] = Slot(),
        closableFlag: Boolean = false,
        disabledFlag: Boolean = false
    )

    def apply(): Inplace = new Inplace(Props())

    private val activeName = Slot.Name("Inplace", "the active side", "the other side", "onActiveChange")
end Inplace
