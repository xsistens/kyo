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
final case class Inplace private (
    displayV: Maybe[UI] = Absent,
    contentV: Maybe[UI] = Absent,
    activeSlot: Maybe[ReactiveValue[Boolean]] = Absent,
    onActiveF: Maybe[Boolean => UI.Action] = Absent,
    closableFlag: Boolean = false,
    disabledFlag: Boolean = false
) extends Node:
    type Self = Inplace

    /** The inactive side (`.p-inplace-display`); clicking it activates the content. */
    def display(u: UI): Inplace = copy(displayV = Present(u))

    /** The active side (`.p-inplace-content`), shown while the ref is true. */
    def content(u: UI): Inplace = copy(contentV = Present(u))

    /** Binds which side shows. A `SignalRef` binds two way: activating the display writes true and the close button
      * false, then [[onActiveChange]] hears it. A plain `Signal` binds one way: the inplace follows it and only reports.
      * A constant nobody hears is fixed: the side it names stays, reachable and inert. Unbound, the inplace keeps the
      * state itself, starting on the display side. A signal nothing hears is reported in a card.
      */
    def active(v: Boolean | Signal[Boolean]): Inplace = copy(activeSlot = Present(ReactiveValue(v)))

    /** Fired with the side the reader asks for (true for the content, false for the display), after a two-way
      * [[active]] is written. One channel for both ways, since an open and a close are never both.
      */
    def onActiveChange(f: Boolean => UI.Action)(using Frame): Inplace = copy(onActiveF = Present(Callback.of(f)))

    /** Appends a close button to the content that deactivates (sets the ref false). */
    def closable(v: Boolean): Inplace = copy(closableFlag = v)

    /** Renders the display side inert: `.p-disabled` look, no activation (Prime's
      * `disabled` open guard; the content side is unaffected).
      */
    def disabled(v: Boolean): Inplace = copy(disabledFlag = v)

    /** Whether anything hears the reader ask for the other side. */
    private def heard: Maybe[Unit] = if onActiveF.isDefined then Present(()) else Absent

    /** A constant nobody hears: the side it names stays, reachable, and nothing swaps it. */
    private def fixed: Boolean = ReactiveValue.fixed(activeSlot, heard)

    /** Asks for side `v`: written where the binding is two way, then reported. */
    private def ask(v: Boolean)(using Frame): UI.Action = ReactiveValue.change(activeSlot, v, onActiveF)

    private def displaySide(using Frame): UI =
        var d = div.cssClass("p-inplace-display").role("button").tabIndex(0)
        if disabledFlag then d = d.cssClass("p-disabled")
        else if fixed then d = d.aria("disabled", "true")
        else
            val open = ask(true)
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
                val pressable = if fixed then b.aria("disabled", "true") else b.onClick(ask(false))
                List(pressable(GlyphSvg(Icons.times, "p-button-icon")))
        div.cssClass("p-inplace-content")(UI.children(contentV.toList ++ closeBtn)*)
    end contentSide

    private[uic] def render(using Frame): UI =
        activeSlot match
            // Unbound, the inplace keeps the side itself, starting on the display. Before the mount runs it draws the
            // display from a signal, so the static page does not call it fixed.
            case Absent =>
                ReactiveValue.owned(false)(own => copy(activeSlot = Present(own)).drawn)(
                    copy(activeSlot = Present(ReactiveValue[Boolean](Signal.initConst(false)))).drawn
                )
            // A signal nobody hears: every side the reader asks for is painted away again.
            case _ if ReactiveValue.deaf(activeSlot, heard) =>
                fragment(
                    Diagnostics.card(
                        "Inplace",
                        "the active side is bound one way and nothing hears the reader ask for the other; add " +
                            "onActiveChange, or bind a SignalRef",
                        Nil
                    ),
                    drawn
                )
            case _ => drawn
    end render

    private def drawn(using Frame): UI =
        val root = div.cssClass("p-inplace").cssClass("p-component").aria("live", "polite")
        activeSlot match
            case Present(slot) => root(Reading.of(slot).render(a => if a then contentSide else displaySide))
            case Absent        => root(displaySide)
    end drawn
end Inplace

object Inplace:
    def apply(): Inplace = new Inplace()
