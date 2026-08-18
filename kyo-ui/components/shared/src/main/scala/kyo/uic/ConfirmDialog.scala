package kyo.uic

import kyo.*
import kyo.UI.*

/** ConfirmDialog — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ConfirmDialog anatomy: a [[Dialog]] whose box carries `.p-confirmdialog`, its
  * content pairing `span.p-confirmdialog-icon` with the
  * `span.p-confirmdialog-message` text, its footer holding the
  * `.p-confirmdialog-reject-button` (text/secondary — Prime's default) and the
  * `.p-confirmdialog-accept-button`), so the extracted `@primeuix` confirmdialog
  * CSS applies verbatim on top of the dialog sheet.
  *
  * Built entirely on the [[Dialog]] machinery, whose focus story it inherits:
  * opening seeds focus on the box (Escape works with no prior click), the box
  * focus-traps Tab, closing restores focus to the opener. Visibility binds
  * two-way to the `open` ref like Dialog.
  *
  * Choice semantics: the accept button runs `onAccept`; the reject button runs
  * `onReject` — and so does EVERY dismissal (Escape, the header close button,
  * a backdrop click): dismissing a confirmation means declining it (Prime's
  * reject semantics). All paths write `false` into the ref first.
  *
  * This is the declarative form. Prime's imperative service form
  * (`ConfirmationService.require(...)` — an Env service resolving the choice as
  * an effect) is a documented follow-up; the declarative ref + callbacks cover
  * the anatomy and the interaction story.
  */
final case class ConfirmDialog private (
    openRef: Maybe[SignalRef[Boolean]] = Absent,
    messageV: Maybe[TextValue] = Absent,
    headerV: Maybe[TextValue] = Absent,
    iconV: Maybe[IconGlyph] = Absent,
    acceptLabelV: TextValue = TextValue.Const("Yes"),
    rejectLabelV: TextValue = TextValue.Const("No"),
    acceptSeverityV: Maybe[Severity] = Absent,
    onAcceptEff: Maybe[Any < Async] = Absent,
    onRejectEff: Maybe[Any < Async] = Absent
) extends Node:
    type Self = ConfirmDialog

    /** Binds visibility two-way to `ref` — the only way to open/close the dialog. */
    def open(ref: SignalRef[Boolean]): ConfirmDialog = copy(openRef = Present(ref))

    /** The confirmation question (`span.p-confirmdialog-message`). */
    def message(v: String): ConfirmDialog = copy(messageV = Present(TextValue.Const(v)))

    /** Reactive `message` tracking `sig` — re-renders in place on emission, e.g. a
      * locale-driven `I18n.t` leaf.
      */
    def message(sig: Signal[String]): ConfirmDialog = copy(messageV = Present(TextValue.Dyn(sig)))

    /** Header title (Prime's `header` option). */
    def header(v: String): ConfirmDialog = copy(headerV = Present(TextValue.Const(v)))

    /** Reactive `header` tracking `sig` — re-renders in place on emission, e.g. a
      * locale-driven `I18n.t` leaf.
      */
    def header(sig: Signal[String]): ConfirmDialog = copy(headerV = Present(TextValue.Dyn(sig)))

    /** Leading icon beside the message (`span.p-confirmdialog-icon`). */
    def icon(glyph: IconGlyph): ConfirmDialog = copy(iconV = Present(glyph))

    /** Accept button label (default "Yes"). */
    def acceptLabel(v: String): ConfirmDialog = copy(acceptLabelV = TextValue.Const(v))

    /** Reactive `acceptLabel` tracking `sig` — re-renders in place on emission, e.g. a
      * locale-driven `I18n.t` leaf.
      */
    def acceptLabel(sig: Signal[String]): ConfirmDialog = copy(acceptLabelV = TextValue.Dyn(sig))

    /** Reject button label (default "No"). */
    def rejectLabel(v: String): ConfirmDialog = copy(rejectLabelV = TextValue.Const(v))

    /** Reactive `rejectLabel` tracking `sig` — re-renders in place on emission, e.g. a
      * locale-driven `I18n.t` leaf.
      */
    def rejectLabel(sig: Signal[String]): ConfirmDialog = copy(rejectLabelV = TextValue.Dyn(sig))

    /** Accept button severity (default primary — Prime's default; a destructive
      * confirmation typically passes [[Severity.Danger]]).
      */
    def acceptSeverity(v: Severity): ConfirmDialog = copy(acceptSeverityV = Present(v))

    /** Runs `action` when the accept button is pressed (after the ref write). */
    def onAccept(action: => Any < Async)(using Frame): ConfirmDialog =
        copy(onAcceptEff = Present(Sync.defer(action)))

    /** Runs `action` on the reject button AND on every dismissal — Escape, the
      * header close button, a backdrop click (after the ref write).
      */
    def onReject(action: => Any < Async)(using Frame): ConfirmDialog =
        copy(onRejectEff = Present(Sync.defer(action)))

    private[uic] def render(using Frame): UI =
        openRef match
            case Absent => UI.empty
            case Present(ref) =>
                def run(eff: Maybe[Any < Async]): Any < Async =
                    eff match
                        case Present(e) => ref.set(false).map(_ => e)
                        case Absent     => ref.set(false)

                def labelButton(tv: TextValue): Button = tv match
                    case TextValue.Const(t) => Button(t)
                    case TextValue.Dyn(s)   => Button(s)

                val iconUI: List[UI] =
                    iconV.toList.map(g => GlyphSvg(g, "p-confirmdialog-icon"))
                val messageUI: List[UI] =
                    messageV.toList.map {
                        case TextValue.Const(t) => span.cssClass("p-confirmdialog-message")(t): UI
                        case TextValue.Dyn(s)   => s.render(t => span.cssClass("p-confirmdialog-message")(t))
                    }

                var accept = labelButton(acceptLabelV)
                    .extraClass("p-confirmdialog-accept-button")
                    .onClick(run(onAcceptEff))
                acceptSeverityV.foreach(s => accept = accept.severity(s))
                val reject = labelButton(rejectLabelV)
                    .severity(Severity.Secondary)
                    .variant(ButtonVariant.Text)
                    .extraClass("p-confirmdialog-reject-button")
                    .onClick(run(onRejectEff))

                var dialog = Dialog()
                    .open(ref)
                    .boxClass("p-confirmdialog")
                    .accessibleRole(PopupAccessibleRole.AlertDialog)
                    .footer(fragment(reject, accept))
                headerV.foreach {
                    case TextValue.Const(t) => dialog = dialog.header(t)
                    case TextValue.Dyn(s)   => dialog = dialog.header(s)
                }
                // Dismissal = declining: Dialog fires onClose on Escape, the header
                // close button, and backdrop clicks — route all of them to onReject.
                onRejectEff.foreach(e => dialog = dialog.onClose(e))
                dialog((iconUI ++ messageUI)*)
end ConfirmDialog

object ConfirmDialog:
    /** A confirm dialog whose visibility binds two-way to `open`. */
    def apply(open: SignalRef[Boolean]): ConfirmDialog = new ConfirmDialog(openRef = Present(open))

    /** An unbound confirm dialog — bind visibility via [[ConfirmDialog.open]]. */
    def apply(): ConfirmDialog = new ConfirmDialog()
end ConfirmDialog
