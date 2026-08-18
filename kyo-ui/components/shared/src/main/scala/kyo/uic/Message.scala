package kyo.uic

import kyo.*
import kyo.UI.*

/** Message rendering variant (`.p-message-outlined` / `.p-message-simple`). */
enum MessageVariant derives CanEqual:
    case Outlined, Simple

/** Message — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Message anatomy: `div.p-message.p-message-<severity>` > `.p-message-content-wrapper`
  * > `.p-message-content` > icon + `.p-message-text` + optional
  * `button.p-message-close-button`), so the extracted `@primeuix` message CSS
  * applies verbatim.
  *
  * `severity` defaults to `Info` (Prime's default); each severity carries a
  * design-derived default leading icon from the Prime icon set — override it via
  * `icon(...)` or suppress it entirely with `hideIcon`. The close button only
  * renders with `closable(true)` (Prime semantics — messages are not closable by
  * default).
  *
  * A Message does NOT own its visibility: it has no ref to write, so pressing the
  * close button notifies and nothing disappears until the app removes the message
  * from its own state. That is why the callback is [[onDismissed]] and not
  * `onClose` — `onClose` is reserved across this module for the surfaces that own
  * a visibility ref and close themselves ([[Dialog]], [[Drawer]], [[Toast]]).
  */
final case class Message private (
    severityV: SeverityValue = SeverityValue.Const(Severity.Info),
    variantV: Maybe[MessageVariant] = Absent,
    sizeV: Size = Size.Normal,
    iconV: Maybe[IconGlyph] = Absent,
    hideIconFlag: Boolean = false,
    closableFlag: Boolean = false,
    onDismissedEff: Maybe[Any < Async] = Absent,
    kids: List[UI] = Nil
) extends Node:
    type Self = Message

    /** Semantic accent (`.p-message-<token>`; Prime's message vocabulary is
      * info/success/warn/error/secondary/contrast — `Danger` maps to `error`,
      * `Primary`/`Help` fall back to the `info` skin).
      */
    def severity(v: Severity): Message = copy(severityV = SeverityValue.Const(v))

    /** Reactive accent — the `.p-message-<token>` class swaps IN PLACE via the class channel and the
      * default leading icon re-renders in its own small sub-region on emission (no whole re-render).
      */
    def severity(sig: Signal[Severity]): Message = copy(severityV = SeverityValue.Dyn(sig))

    /** Rendering variant: `Outlined` (transparent body, colored border) or
      * `Simple` (borderless text-only); unset renders Prime's filled default.
      */
    def variant(v: MessageVariant): Message = copy(variantV = Present(v))

    /** Size: `.p-message-sm` / default / `.p-message-lg`. */
    def size(v: Size): Message = copy(sizeV = v)

    /** Custom leading icon, replacing the severity's default glyph. */
    def icon(glyph: IconGlyph): Message = copy(iconV = Present(glyph))

    /** Hides the leading icon entirely (both an explicit one and the severity default). */
    def hideIcon(v: Boolean): Message = copy(hideIconFlag = v)

    /** Renders the close button (`.p-message-close-button`, hidden by default). */
    def closable(v: Boolean): Message = copy(closableFlag = v)

    /** Runs `action` when the close button is pressed. Past tense on purpose: the
      * message has been dismissed by the user, and nothing has been hidden — remove
      * it from your own state if you want it gone. Contrast [[Dialog.onClose]],
      * which fires after the component has already written its own visibility ref.
      */
    def onDismissed(action: => Any < Async)(using Frame): Message =
        copy(onDismissedEff = Present(Sync.defer(action)))

    /** Adds default-slot children (the message text). */
    def apply(cs: UI*): Message = copy(kids = kids ++ cs)

    /** The `.p-message-<token>` suffix for a severity (Prime's message set; `Danger` is `error`). */
    private def messageToken(s: Severity): String = s match
        case Severity.Success                                 => "success"
        case Severity.Warn                                    => "warn"
        case Severity.Danger                                  => "error"
        case Severity.Secondary                               => "secondary"
        case Severity.Contrast                                => "contrast"
        case Severity.Info | Severity.Primary | Severity.Help => "info"

    /** Design-derived default leading icon per severity (Prime icon set). */
    private def defaultIconFor(s: Severity): IconGlyph = s match
        case Severity.Success  => Icons.checkCircle
        case Severity.Warn     => Icons.exclamationTriangle
        case Severity.Danger   => Icons.timesCircle
        case Severity.Contrast => Icons.infoCircle
        case _                 => Icons.infoCircle

    private[uic] def render(using Frame): UI =
        var el = div
            .cssClass("p-message")
            .cssClass("p-component")
            .role("alert")
            .aria("live", "assertive")
            .aria("atomic", "true")
            // Enter/leave (Welle P): fades in when inserted, and — when the app removes
            // the message from its own reactive state — plays Prime's own
            // `p-message-leave-active` keyframe on the leave ghost.
            .enterTransition("p-uic-enter-fade")
            .leaveTransition("p-message-leave-active")
        // Constant severity joins the class list; a reactive one binds one class per DISTINCT token
        // (Info/Primary/Help all map to "info"), condition = the current severity maps to that token —
        // so many-to-one severities never toggle the same class against each other.
        severityV match
            case SeverityValue.Const(s) => el = el.cssClass(s"p-message-${messageToken(s)}")
            case SeverityValue.Dyn(sig) =>
                Severity.values.map(messageToken).distinct.foreach(tok =>
                    el = el.cssClass(s"p-message-$tok", sig.map(s => messageToken(s) == tok))
                )
        end match
        variantV match
            case Present(MessageVariant.Outlined) => el = el.cssClass("p-message-outlined")
            case Present(MessageVariant.Simple)   => el = el.cssClass("p-message-simple")
            case Absent                           => ()
        end match
        sizeV match
            case Size.Small  => el = el.cssClass("p-message-sm")
            case Size.Large  => el = el.cssClass("p-message-lg")
            case Size.Normal => ()
        end match

        val iconChild: List[UI] =
            if hideIconFlag then Nil
            else
                iconV match
                    // An explicit icon is fixed. Otherwise the default glyph follows the severity: constant is
                    // built once; reactive re-renders ONLY the icon in its own sub-region on severity change.
                    case Present(g) => List(GlyphSvg(g, "p-message-icon"))
                    case Absent =>
                        severityV match
                            case SeverityValue.Const(s) => List(GlyphSvg(defaultIconFor(s), "p-message-icon"))
                            case SeverityValue.Dyn(sig) => List(sig.render(s => GlyphSvg(defaultIconFor(s), "p-message-icon")))
        val textChild: List[UI] = List(span.cssClass("p-message-text")(kids.map(toChild)*))
        val closeChild: List[UI] =
            if !closableFlag then Nil
            else
                var btn = button
                    .cssClass("p-message-close-button")
                    .jsProp("type", "button")
                    .aria("label", "Close")
                onDismissedEff.foreach(e => btn = btn.onClick(e))
                List(btn(toChild(GlyphSvg(Icons.times, "p-message-close-icon"))))

        el(
            div.cssClass("p-message-content-wrapper")(
                div.cssClass("p-message-content")((iconChild ++ textChild ++ closeChild).map(toChild)*)
            )
        )
    end render
end Message

object Message:
    def apply(): Message = new Message()
