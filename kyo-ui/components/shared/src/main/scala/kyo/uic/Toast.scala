package kyo.uic

import kyo.*
import kyo.UI.*

/** Toast: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Toast
  * anatomy: the fixed `div.p-toast.p-component.p-toast-<position>` region (Prime
  * positions it via inline styles; here a `.p-uic-*` remainder pins each
  * position token) > `div.p-toast-message.p-toast-message-<severity>` >
  * `div.p-toast-message-content` > severity icon + `div.p-toast-message-text`
  * (`span.p-toast-summary` + `div.p-toast-detail`) + optional
  * `button.p-toast-close-button`), so the extracted `@primeuix` toast CSS
  * applies verbatim.
  *
  * Visibility is gated exactly like [[Dialog]] (a `when(open)` reactive
  * boundary), and an unbound toast keeps it itself and starts open. One `Toast`
  * owns no fiber at all (see [[Node]]
  * on fiber ownership): the auto-dismiss `duration` is emitted as
  * `data-uic-duration` (ms) for the client runtime, and the dismissal timer, which
  * has to outlive the render, is the caller's: `ref.set(true)` then
  * `Fiber.initUnscoped(Async.sleep(..).andThen(ref.set(false)))`. [[ToastService]]
  * exists so that timer has an owner.
  *
  * One toast renders ONE message; for the multi-message stacking store (Prime's
  * ToastService with `add(...)` queueing) use [[ToastService]], a kyo
  * Env/Layer service whose `render` region stacks queued messages.
  */
final case class Toast private (
    openSlot: Maybe[ReactiveValue[Boolean]] = Absent,
    onOpenF: Maybe[Boolean => UI.Action] = Absent,
    durationMs: Int = 3000,
    positionV: OverlayPosition = OverlayPosition.TopRight,
    severityV: Severity = Severity.Info,
    summaryV: Maybe[TextValue] = Absent,
    detailV: Maybe[TextValue] = Absent,
    closableFlag: Boolean = false,
    kids: List[UI] = Nil
) extends Node:
    type Self = Toast

    /** Binds visibility. A `SignalRef` binds two way: writes open and close the toast, and every close the reader
      * asks for (the close button) writes false, then [[onOpenChange]] hears it. A plain `Signal` binds one way: the toast
      * follows it and only reports a close. A constant nobody hears is fixed: an open toast stays, its close button
      * reachable and inert. Unbound, the toast keeps the state itself and starts open, so it shows until the reader
      * closes it. A signal nothing hears is reported in a card.
      */
    def open(v: Boolean | Signal[Boolean]): Toast = copy(openSlot = Present(ReactiveValue(v)))

    /** Fired with the visibility the reader asks for (false for every close), after a two-way [[open]] is written.
      * External writes fire nothing.
      */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): Toast = copy(onOpenF = Present(Callback.of(f)))

    /** Auto-dismiss duration in milliseconds (emitted as `data-uic-duration`;
      * Prime's `life`).
      */
    def duration(ms: Int): Toast = copy(durationMs = ms)

    /** Screen position (`.p-toast-<token>`; Prime's default is top-right). */
    def position(v: OverlayPosition): Toast = copy(positionV = v)

    /** Semantic tint of the message (`.p-toast-message-<token>`; Prime's toast
      * vocabulary is info/success/warn/error/secondary/contrast; `Danger` maps to
      * `error`, `Primary`/`Help` fall back to the `info` skin).
      */
    def severity(v: Severity): Toast = copy(severityV = v)

    /** Bold first line (`span.p-toast-summary`). A `Signal[String]` re-renders it in place on emission
      * (e.g. a locale-driven `I18n.t` leaf).
      */
    def summary(v: String | Signal[String]): Toast = copy(summaryV = Present(ReactiveValue(v)))

    /** Detail line below the summary (`div.p-toast-detail`). A `Signal[String]` re-renders it in place on
      * emission.
      */
    def detail(v: String | Signal[String]): Toast = copy(detailV = Present(ReactiveValue(v)))

    /** Renders Prime's close button; pressing it asks for `false` (see [[open]]). */
    def closable(v: Boolean): Toast = copy(closableFlag = v)

    /** Adds custom body children, rendered after summary/detail inside the text slot. */
    def apply(cs: UI*): Toast = copy(kids = kids ++ cs)

    /** The `.p-toast-message-<token>` suffix (Prime's toast set; `Danger` is `error`). */
    private def token: String = Toast.severityToken(severityV)

    /** Design-derived leading icon per severity (Prime's Toast defaults). */
    private def severityIcon: IconGlyph = Toast.severityIcon(severityV)

    private[uic] def render(using Frame): UI =
        openSlot match
            // Unbound, the toast keeps its visibility itself and starts open. Before the mount runs it draws from a
            // signal, so the static page does not call its close button fixed.
            case Absent =>
                ReactiveValue.owned(true)(own => copy(openSlot = Present(own)).drawn)(
                    copy(openSlot = Present(ReactiveValue[Boolean](Signal.initConst(true)))).drawn
                )
            // A signal nobody hears, on a toast the reader can close: every close is painted away again. A toast with no
            // close button has nothing to hear.
            case _ if closableFlag && ReactiveValue.deaf(openSlot, onOpenF) =>
                fragment(
                    KeyDiagnostics.card(
                        "Toast",
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
                // A constant nobody hears is fixed: the toast stays, and its close button closes nothing.
                val fixed            = ReactiveValue.fixed(openSlot, onOpenF)
                val close: UI.Action = ReactiveValue.change(openSlot, false, onOpenF)
                // Shown while the binding says so: a constant needs no region, a signal is one boundary.
                def shown(ui: => UI): UI = slot.const match
                    case Present(v) => if v then ui else UI.empty
                    case Absent     => when(slot.signal)(ui)

                val icon: UI            = GlyphSvg(severityIcon, "p-toast-message-icon")
                val textParts: List[UI] =
                    summaryV.toList.map {
                        case TextValue.Const(t) => span.cssClass("p-toast-summary")(t): UI
                        case TextValue.Dyn(s)   => span.cssClass("p-toast-summary")(s)
                    } ++
                        detailV.toList.map {
                            case TextValue.Const(t) => div.cssClass("p-toast-detail")(t): UI
                            case TextValue.Dyn(s)   => div.cssClass("p-toast-detail")(s)
                        } ++
                        kids
                val text: UI           = div.cssClass("p-toast-message-text")(UI.children(textParts)*)
                val closeBtn: List[UI] =
                    if !closableFlag then Nil
                    else
                        val shell     = button.cssClass("p-toast-close-button").jsProp("type", "button").aria("label", "Close")
                        val pressable = if fixed then shell.aria("disabled", "true") else shell.onClick(close)
                        List(pressable(GlyphSvg(Icons.times, "p-toast-close-icon")))

                shown(
                    div
                        .cssClass("p-toast")
                        .cssClass("p-component")
                        .cssClass(s"p-toast-${positionV.token}")
                        .data("uic-duration", durationMs.toString)
                        .role("alert")
                        .aria("live", "polite")(
                            div
                                .cssClass("p-toast-message")
                                .cssClass(s"p-toast-message-$token")
                                // Enter slides the message in from its screen edge and fades;
                                // close plays Prime's own `p-toast-message-leave-active` keyframe
                                // (ending in translateY(-100%) scale(.6)) on the leave ghost.
                                .enterTransition(Toast.enterClass(positionV))
                                .leaveTransition("p-toast-message-leave-active")(
                                    div.cssClass("p-toast-message-content")(
                                        UI.children((icon :: text :: closeBtn))*
                                    )
                                )
                        )
                )
        end match
    end drawn
end Toast

object Toast:
    def apply(): Toast = new Toast()

    /** The `.p-toast-message-<token>` suffix (Prime's toast vocabulary; `Danger`
      * maps to `error`, `Primary`/`Help` fall back to the `info` skin), shared
      * with [[ToastService]]'s stacked region.
      */
    private[uic] def severityToken(s: Severity): String = s match
        case Severity.Success                                 => "success"
        case Severity.Warn                                    => "warn"
        case Severity.Danger                                  => "error"
        case Severity.Secondary                               => "secondary"
        case Severity.Contrast                                => "contrast"
        case Severity.Info | Severity.Primary | Severity.Help => "info"

    /** The message ENTER transition per screen position (a kyo enhancement over
      * Prime's uniform scale-in): side positions slide in horizontally,
      * top/bottom-center vertically, `Center` fades, each space-joined with the
      * fade from-state so the message slides AND fades. The region carries the
      * centering transform, not the message, so the slide never fights it. Shared
      * with [[ToastService]]'s stacked region.
      */
    private[uic] def enterClass(p: OverlayPosition): String = p match
        case OverlayPosition.TopLeft | OverlayPosition.BottomLeft   => "p-uic-slide-enter-left p-uic-enter-fade"
        case OverlayPosition.TopRight | OverlayPosition.BottomRight => "p-uic-slide-enter-right p-uic-enter-fade"
        case OverlayPosition.TopCenter                              => "p-uic-slide-enter-top p-uic-enter-fade"
        case OverlayPosition.BottomCenter                           => "p-uic-slide-enter-bottom p-uic-enter-fade"
        case OverlayPosition.Center                                 => "p-uic-enter-fade"

    /** Design-derived leading icon per severity (Prime's Toast defaults), shared
      * with [[ToastService]]'s stacked region.
      */
    private[uic] def severityIcon(s: Severity): IconGlyph = s match
        case Severity.Success => Icons.check
        case Severity.Warn    => Icons.exclamationTriangle
        case Severity.Danger  => Icons.timesCircle
        case _                => Icons.infoCircle
end Toast
