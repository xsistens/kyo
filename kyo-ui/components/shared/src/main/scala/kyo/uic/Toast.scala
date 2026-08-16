package kyo.uic

import kyo.*
import kyo.UI.*

/** Toast — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Toast
  * anatomy: the fixed `div.p-toast.p-component.p-toast-<position>` region (Prime
  * positions it via inline styles; here a `.p-uic-*` remainder pins each
  * position token) > `div.p-toast-message.p-toast-message-<severity>` >
  * `div.p-toast-message-content` > severity icon + `div.p-toast-message-text`
  * (`span.p-toast-summary` + `div.p-toast-detail`) + optional
  * `button.p-toast-close-button`), so the extracted `@primeuix` toast CSS
  * applies verbatim.
  *
  * Visibility is gated by a `SignalRef[Boolean]` exactly like [[Dialog]] (a
  * `when(ref)` reactive boundary). The render is pure — no timers run at render
  * time; the auto-dismiss `duration` is emitted as `data-uic-duration` (ms) for
  * the client runtime, and app code drives timed dismissal from a forked fiber
  * (`ref.set(true)` then `Fiber.initUnscoped(Async.sleep(..).andThen(ref.set(false)))`).
  *
  * One toast renders ONE message; for the multi-message stacking store (Prime's
  * ToastService with `add(...)` queueing) use [[ToastService]] — a kyo
  * Env/Layer service whose `render` region stacks queued messages.
  */
final case class Toast private (
    openRef: Maybe[SignalRef[Boolean]] = Absent,
    durationMs: Int = 3000,
    positionV: OverlayPosition = OverlayPosition.TopRight,
    severityV: Severity = Severity.Info,
    summaryV: Maybe[TextValue] = Absent,
    detailV: Maybe[TextValue] = Absent,
    closableFlag: Boolean = false,
    onCloseEff: Maybe[Any < Async] = Absent,
    kids: List[UI] = Nil
) extends Node:
    type Self = Toast

    /** Binds visibility to `ref` — the only way to show/hide the toast. */
    def open(ref: SignalRef[Boolean]): Toast = copy(openRef = Present(ref))

    /** Auto-dismiss duration in milliseconds (emitted as `data-uic-duration`;
      * Prime's `life`).
      */
    def duration(ms: Int): Toast = copy(durationMs = ms)

    /** Screen position (`.p-toast-<token>`; Prime's default is top-right). */
    def position(v: OverlayPosition): Toast = copy(positionV = v)

    /** Semantic tint of the message (`.p-toast-message-<token>`; Prime's toast
      * vocabulary is info/success/warn/error/secondary/contrast — `Danger` maps to
      * `error`, `Primary`/`Help` fall back to the `info` skin).
      */
    def severity(v: Severity): Toast = copy(severityV = v)

    /** Bold first line (`span.p-toast-summary`). */
    def summary(v: String): Toast = copy(summaryV = Present(TextValue.Const(v)))

    /** Reactive summary that tracks `sig` — re-renders in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def summary(sig: Signal[String]): Toast = copy(summaryV = Present(TextValue.Dyn(sig)))

    /** Detail line below the summary (`div.p-toast-detail`). */
    def detail(v: String): Toast = copy(detailV = Present(TextValue.Const(v)))

    /** Reactive detail that tracks `sig` — re-renders in place on emission. */
    def detail(sig: Signal[String]): Toast = copy(detailV = Present(TextValue.Dyn(sig)))

    /** Renders Prime's close button; pressing it writes the bound ref to `false`. */
    def closable(v: Boolean): Toast = copy(closableFlag = v)

    /** Runs `action` after the close button writes the ref to `false`. */
    def onClose(action: => Any < Async)(using Frame): Toast =
        copy(onCloseEff = Present(Sync.defer(action)))

    /** Adds custom body children, rendered after summary/detail inside the text slot. */
    def apply(cs: UI*): Toast = copy(kids = kids ++ cs)

    /** The `.p-toast-message-<token>` suffix (Prime's toast set; `Danger` is `error`). */
    private def token: String = Toast.severityToken(severityV)

    /** Design-derived leading icon per severity (Prime's Toast defaults). */
    private def severityIcon: IconGlyph = Toast.severityIcon(severityV)

    private[uic] def render(using Frame): UI =
        openRef match
            case Absent => UI.empty
            case Present(ref) =>
                val close: Any < Async =
                    onCloseEff match
                        case Present(e) => ref.set(false).map(_ => e)
                        case Absent     => ref.set(false)

                val icon: UI = GlyphSvg(severityIcon, "p-toast-message-icon")
                val textParts: List[UI] =
                    summaryV.toList.map {
                        case TextValue.Const(t) => span.cssClass("p-toast-summary")(t): UI
                        case TextValue.Dyn(s)   => s.render(t => span.cssClass("p-toast-summary")(t))
                    } ++
                        detailV.toList.map {
                            case TextValue.Const(t) => div.cssClass("p-toast-detail")(t): UI
                            case TextValue.Dyn(s)   => s.render(t => div.cssClass("p-toast-detail")(t))
                        } ++
                        kids
                val text: UI = div.cssClass("p-toast-message-text")(textParts.map(toChild)*)
                val closeBtn: List[UI] =
                    if !closableFlag then Nil
                    else
                        List(
                            button
                                .cssClass("p-toast-close-button")
                                .jsProp("type", "button")
                                .aria("label", "Close")
                                .onClick(close)(toChild(GlyphSvg(Icons.times, "p-toast-close-icon")))
                        )

                when(ref)(
                    div
                        .cssClass("p-toast")
                        .cssClass("p-component")
                        .cssClass(s"p-toast-${positionV.token}")
                        .data("uic-duration", durationMs.toString)
                        .role("alert")
                        .aria("live", "polite")(
                            toChild(
                                div
                                    .cssClass("p-toast-message")
                                    .cssClass(s"p-toast-message-$token")
                                    // Enter slides the message in from its screen edge and fades;
                                    // close plays Prime's own `p-toast-message-leave-active` keyframe
                                    // (ending in translateY(-100%) scale(.6)) on the leave ghost.
                                    .enterTransition(Toast.enterClass(positionV))
                                    .leaveTransition("p-toast-message-leave-active")(
                                        toChild(
                                            div.cssClass("p-toast-message-content")(
                                                ((icon :: text :: closeBtn)).map(toChild)*
                                            )
                                        )
                                    )
                            )
                        )
                )
end Toast

object Toast:
    def apply(): Toast = new Toast()

    /** The `.p-toast-message-<token>` suffix (Prime's toast vocabulary; `Danger`
      * maps to `error`, `Primary`/`Help` fall back to the `info` skin) — shared
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
      * top/bottom-center vertically, `Center` fades — each space-joined with the
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

    /** Design-derived leading icon per severity (Prime's Toast defaults) — shared
      * with [[ToastService]]'s stacked region.
      */
    private[uic] def severityIcon(s: Severity): IconGlyph = s match
        case Severity.Success => Icons.check
        case Severity.Warn    => Icons.exclamationTriangle
        case Severity.Danger  => Icons.timesCircle
        case _                => Icons.infoCircle
end Toast
