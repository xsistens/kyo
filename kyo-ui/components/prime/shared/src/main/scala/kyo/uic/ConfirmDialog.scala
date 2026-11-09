package kyo.uic

import kyo.*
import kyo.UI.*

/** ConfirmDialog: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ConfirmDialog anatomy: a [[Dialog]] whose box carries `.p-confirmdialog`, its
  * content pairing `span.p-confirmdialog-icon` with the
  * `span.p-confirmdialog-message` text, its footer holding the
  * `.p-confirmdialog-reject-button` (text/secondary, Prime's default) and the
  * `.p-confirmdialog-accept-button`), so the extracted `@primeuix` confirmdialog
  * CSS applies verbatim on top of the dialog sheet.
  *
  * Built entirely on the [[Dialog]] machinery, whose focus story it inherits:
  * opening seeds focus on the box (Escape works with no prior click), the box
  * focus-traps Tab, closing restores focus to the opener. Visibility binds
  * like Dialog's, and an unbound confirm dialog starts open.
  *
  * Choice semantics: the accept button runs `onAccept`; the reject button runs
  * `onReject`, and so does EVERY dismissal (Escape, the header close button,
  * a backdrop click): dismissing a confirmation means declining it (Prime's
  * reject semantics). Every path asks for `false`, written into a two-way
  * binding first and reported to [[onOpenChange]] before the choice.
  *
  * This is the declarative form. Prime's imperative service form
  * (`ConfirmationService.require(...)`, an Env service resolving the choice as
  * an effect) is a documented follow-up; the declarative ref + callbacks cover
  * the anatomy and the interaction story.
  */
final class ConfirmDialog private[uic] (private[uic] val p: ConfirmDialog.Props) extends Node, HasProps:
    import p.*

    type Self = ConfirmDialog

    /** Binds visibility the way [[Dialog.open]] does: a `SignalRef` is written by every choice and dismissal, a plain
      * `Signal` is followed and a close only reported, a constant nobody hears is fixed, and unbound the dialog keeps
      * the state itself and starts open. A signal no choice is heard for is reported in a card.
      */
    def open(v: Boolean | Signal[Boolean]): ConfirmDialog = new ConfirmDialog(p.copy(openSlot = openSlot.bind(v)))

    /** Fired with the visibility the reader asks for (false for every choice and dismissal), after a two-way [[open]]
      * is written and before [[onAccept]] or [[onReject]].
      */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): ConfirmDialog = new ConfirmDialog(p.copy(openSlot = openSlot.hear(f)))

    /** The confirmation question (`span.p-confirmdialog-message`). A `Signal[String]` re-renders it in
      * place on emission, e.g. a locale-driven `I18n.t` leaf.
      */
    def message(v: String | Signal[String]): ConfirmDialog = new ConfirmDialog(p.copy(messageV = Present(ReactiveValue(v))))

    /** Header title (Prime's `header` option). A `Signal[String]` re-renders it in place on emission, e.g.
      * a locale-driven `I18n.t` leaf.
      */
    def header(v: String | Signal[String]): ConfirmDialog = new ConfirmDialog(p.copy(headerV = Present(ReactiveValue(v))))

    /** Leading icon beside the message (`span.p-confirmdialog-icon`). */
    def icon(glyph: IconGlyph): ConfirmDialog = new ConfirmDialog(p.copy(iconV = Present(glyph)))

    /** Accept button label (default "Yes"). A `Signal[String]` re-renders it in place on emission, e.g. a
      * locale-driven `I18n.t` leaf.
      */
    def acceptLabel(v: String | Signal[String]): ConfirmDialog = new ConfirmDialog(p.copy(acceptLabelV = ReactiveValue(v)))

    /** Reject button label (default "No"). A `Signal[String]` re-renders it in place on emission, e.g. a
      * locale-driven `I18n.t` leaf.
      */
    def rejectLabel(v: String | Signal[String]): ConfirmDialog = new ConfirmDialog(p.copy(rejectLabelV = ReactiveValue(v)))

    /** Accept button severity (default primary, Prime's default; a destructive
      * confirmation typically passes [[Severity.Danger]]).
      */
    def acceptSeverity(v: Severity): ConfirmDialog = new ConfirmDialog(p.copy(acceptSeverityV = Present(v)))

    /** Runs `action` when the accept button is pressed (after the ref write). */
    def onAccept(action: => UI.Action)(using Frame): ConfirmDialog =
        new ConfirmDialog(p.copy(onAcceptEff = Present(Callback(action))))

    /** Runs `action` on the reject button AND on every dismissal: Escape, the
      * header close button, a backdrop click (after the ref write).
      */
    def onReject(action: => UI.Action)(using Frame): ConfirmDialog =
        new ConfirmDialog(p.copy(onRejectEff = Present(Callback(action))))

    /** The slot as an accept hears it: [[onOpenChange]], or [[onAccept]]. */
    private def accepting: Controlled[Boolean] = if onAcceptEff.isDefined then openSlot.heardToo else openSlot

    /** The slot as a reject and every dismissal hear it: [[onOpenChange]], or [[onReject]]. */
    private def rejecting: Controlled[Boolean] = if onRejectEff.isDefined then openSlot.heardToo else openSlot

    private[uic] def render(using Frame): UI =
        // Unbound, the confirm dialog keeps its visibility itself and starts open, like [[Dialog]].
        val drawing = Controlled.owning(
            openSlot,
            start = true
        )(s => new ConfirmDialog(this.p.copy(openSlot = s)).drawn)
        // A signal nobody hears for a choice: that choice is painted away again. One card for both, since the dialog
        // underneath is kept quiet.
        val unheard = Seq("an accept").filter(_ => accepting.deaf) ++ Seq("a reject or a dismissal").filter(_ => rejecting.deaf)
        Diagnostics.before(drawing)("ConfirmDialog")(
            Diagnostic(unheard.nonEmpty, ConfirmDialog.openName(unheard.mkString(" or ")).advice)
        )
    end render

    private def drawn(using Frame): UI =
        openSlot.binding match
            case Absent     => UI.empty
            case Present(_) =>
                // A constant nobody hears for a choice is fixed for that choice: its button stays, inert.
                val acceptFixed = accepting.fixed
                val rejectFixed = rejecting.fixed
                // Every choice asks for false: a two-way binding is written, the close reported, then the choice heard.
                def choose(eff: Maybe[UI.Action]): UI.Action =
                    val asked = openSlot.change(false)
                    eff match
                        case Present(e) => asked.andThen(e)
                        case Absent     => asked
                end choose

                def labelButton(tv: TextValue): Button = tv match
                    case TextValue.Const(t) => Button(t)
                    case TextValue.Dyn(s)   => Button(s)

                val iconUI: List[UI] =
                    iconV.toList.map(g => GlyphSvg(g, "p-confirmdialog-icon"))
                val messageUI: List[UI] =
                    messageV.toList.map {
                        case TextValue.Const(t) => span.cssClass("p-confirmdialog-message")(t): UI
                        case TextValue.Dyn(s)   => span.cssClass("p-confirmdialog-message")(s)
                    }

                val acceptShell = labelButton(acceptLabelV).extraClass("p-confirmdialog-accept-button")
                var accept      = if acceptFixed then acceptShell.ariaDisabled(true) else acceptShell.onClick(choose(onAcceptEff))
                acceptSeverityV.foreach(s => accept = accept.severity(s))
                val rejectShell = labelButton(rejectLabelV)
                    .severity(Severity.Secondary)
                    .variant(ButtonVariant.Text)
                    .extraClass("p-confirmdialog-reject-button")
                val reject = if rejectFixed then rejectShell.ariaDisabled(true) else rejectShell.onClick(choose(onRejectEff))

                var dialog = Dialog()
                    .withOpen(Controlled(openSlot.bound, quiet = true))
                    .boxClass("p-confirmdialog")
                    .accessibleRole(PopupAccessibleRole.AlertDialog)
                    .footer(fragment(reject, accept))
                headerV.foreach {
                    case TextValue.Const(t) => dialog = dialog.header(t)
                    case TextValue.Dyn(s)   => dialog = dialog.header(s)
                }
                // Dismissal = declining: Escape, the header close button and a backdrop click are rejects. Heard, the
                // dialog wires them; unheard, a constant leaves them inert, the dialog's own fixed state.
                if openSlot.hearer.isDefined || onRejectEff.isDefined then
                    dialog = dialog.onOpenChange(v =>
                        val reported: UI.Action = openSlot.hearer.fold[UI.Action](())(f => f(v))
                        onRejectEff.fold[UI.Action](reported)(e => reported.andThen(e))
                    )
                end if
                dialog((iconUI ++ messageUI)*)
        end match
    end drawn
end ConfirmDialog

object ConfirmDialog:
    final private[uic] case class Props(
        openSlot: Controlled[Boolean] = Controlled(),
        messageV: Maybe[TextValue] = Absent,
        headerV: Maybe[TextValue] = Absent,
        iconV: Maybe[IconGlyph] = Absent,
        acceptLabelV: TextValue = TextValue.Const("Yes"),
        rejectLabelV: TextValue = TextValue.Const("No"),
        acceptSeverityV: Maybe[Severity] = Absent,
        onAcceptEff: Maybe[UI.Action] = Absent,
        onRejectEff: Maybe[UI.Action] = Absent
    )

    /** A confirm dialog whose visibility binds to `open` (see [[ConfirmDialog.open]]). */
    def apply(open: Boolean | Signal[Boolean]): ConfirmDialog = new ConfirmDialog(Props(openSlot = Controlled().bind(open)))

    /** An unbound confirm dialog: it keeps its visibility itself and starts open. */
    def apply(): ConfirmDialog = new ConfirmDialog(Props())

    private def openName(gesture: String) = Controlled.Name("ConfirmDialog", "the visibility", gesture, "onOpenChange")
end ConfirmDialog
