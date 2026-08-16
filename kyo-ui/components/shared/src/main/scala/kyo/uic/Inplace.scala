package kyo.uic

import kyo.*
import kyo.UI.*

/** Inplace — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Inplace anatomy: `div.p-inplace.p-component[aria-live=polite]` > EITHER the
  * clickable `div.p-inplace-display[role=button]` OR the active
  * `div.p-inplace-content` — never both), so the extracted `@primeuix` inplace
  * CSS applies verbatim.
  *
  * The active state binds two-way to the `active` `SignalRef[Boolean]`
  * ([[Panel]]'s reactive pattern): clicking the display (or pressing Enter on
  * it — Prime's key) sets it true and swaps in the content; external ref
  * writes swap the sides reactively. `closable(true)` appends an icon-only
  * close button to the content that sets the ref back to false (PrimeReact's
  * `closable`). `disabled(true)` renders the display side inert (`.p-disabled`
  * — Prime's open guard). `onOpen`/`onClose` fire AFTER the ref write when the
  * user activates the display or the close button (Prime's open/close events;
  * external ref writes fire neither). Without a bound ref the display side
  * renders statically.
  */
final case class Inplace private (
    displayV: Maybe[UI] = Absent,
    contentV: Maybe[UI] = Absent,
    activeRef: Maybe[SignalRef[Boolean]] = Absent,
    closableFlag: Boolean = false,
    disabledFlag: Boolean = false,
    onOpenEff: Maybe[Any < Async] = Absent,
    onCloseEff: Maybe[Any < Async] = Absent
) extends Node:
    type Self = Inplace

    /** The inactive side (`.p-inplace-display`) — clicking it activates the content. */
    def display(u: UI): Inplace = copy(displayV = Present(u))

    /** The active side (`.p-inplace-content`), shown while the ref is true. */
    def content(u: UI): Inplace = copy(contentV = Present(u))

    /** Binds the active state two-way to `ref`: display click sets it true,
      * external writes swap the sides reactively.
      */
    def active(ref: SignalRef[Boolean]): Inplace = copy(activeRef = Present(ref))

    /** Appends a close button to the content that deactivates (sets the ref false). */
    def closable(v: Boolean): Inplace = copy(closableFlag = v)

    /** Renders the display side inert: `.p-disabled` look, no activation (Prime's
      * `disabled` open guard — the content side is unaffected).
      */
    def disabled(v: Boolean): Inplace = copy(disabledFlag = v)

    /** Runs `action` after the display side is activated (ref already true). */
    def onOpen(action: => Any < Async)(using Frame): Inplace =
        copy(onOpenEff = Present(Sync.defer(action)))

    /** Runs `action` after the close button deactivates (ref already false). */
    def onClose(action: => Any < Async)(using Frame): Inplace =
        copy(onCloseEff = Present(Sync.defer(action)))

    private def displaySide(ref: Maybe[SignalRef[Boolean]])(using Frame): UI =
        var d = div.cssClass("p-inplace-display").role("button").tabIndex(0)
        if disabledFlag then d = d.cssClass("p-disabled")
        else
            ref.foreach { r =>
                val open: Any < Async =
                    onOpenEff match
                        case Present(e) =>
                            for
                                _ <- r.set(true)
                                x <- e
                            yield x
                        case Absent => r.set(true)
                d = d.onClick(open).onKeyDown { evt =>
                    evt.key match
                        case Keyboard.Enter => open
                        case _              => ()
                }
            }
        end if
        d(displayV.toList.map(toChild)*)
    end displaySide

    private def contentSide(ref: SignalRef[Boolean])(using Frame): UI =
        val closeBtn: List[UI] =
            if !closableFlag then Nil
            else
                val close: Any < Async =
                    onCloseEff match
                        case Present(e) =>
                            for
                                _ <- ref.set(false)
                                x <- e
                            yield x
                        case Absent => ref.set(false)
                List(
                    button
                        .cssClass("p-button")
                        .cssClass("p-component")
                        .cssClass("p-button-icon-only")
                        .jsProp("type", "button")
                        .aria("label", "Close")
                        .onClick(close)(
                            toChild(GlyphSvg(Icons.times, "p-button-icon"))
                        )
                )
        div.cssClass("p-inplace-content")((contentV.toList ++ closeBtn).map(toChild)*)
    end contentSide

    private[uic] def render(using Frame): UI =
        val root = div.cssClass("p-inplace").cssClass("p-component").aria("live", "polite")
        activeRef match
            case Present(ref) =>
                root(toChild(ref.render(a => if a then contentSide(ref) else displaySide(Present(ref)))))
            case Absent =>
                root(toChild(displaySide(Absent)))
        end match
    end render
end Inplace

object Inplace:
    def apply(): Inplace = new Inplace()
