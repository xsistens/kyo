package kyo.uic.form

import kyo.*
import kyo.UI.*
import kyo.uic.KeyDiagnostics

/** What a form can say for CERTAIN about its own ids, computed once when it mounts.
  *
  * The form owns the id slot: `focusFirstInvalid` and the error summary both reach a field
  * through `Commands.focusId(domId)`, so an id that is not where the form thinks it is turns
  * both of them into no-ops — silently, and only for the reader who submits an invalid form.
  * The three checks below each rest on a fact rather than a guess:
  *
  *   - a control that already carried an id when `bind` stamped the field's own over it;
  *   - two fields of one form tree claiming the same id;
  *   - a `<label for="x">` with no `#x` anywhere in the form's tree.
  *
  * What is deliberately NOT checked: whether a field's own id appears in the tree at all.
  * That reads as the most useful check of the three and is the one that cannot be trusted — a
  * field bound inside a reactive branch that happens to be closed at mount is absent for a
  * perfectly good reason, and a card a reader is told to sometimes ignore is worse than no
  * card. The dangling-`for` check catches the same mistake from the side that IS certain.
  *
  * Field-array rows are outside all of this: rows arrive and leave after mount, and a card
  * computed once cannot follow them.
  */
private[form] object FormDiagnostics:

    /** The cards for one mounted form tree; empty for every correct program. */
    def cards(form: Form, ui: UI)(using Frame): Chunk[UI] < Sync =
        for
            fields <- form.declaredFields
            seen   <- scan(ui)
        yield
            val conflicts = fields.collect { case f if f.idConflict.isDefined => f.idConflict.get }
            val dupes     = KeyDiagnostics.duplicates(fields.map(_.domId))
            val dangling  = seen.labelFors.distinct.filterNot(seen.ids.contains)

            val conflictCard =
                if conflicts.isEmpty then Chunk.empty
                else
                    Chunk(KeyDiagnostics.card(
                        "kyo.uic.form",
                        "a control bound to a field already carried an id, which bind replaced — the form owns " +
                            "that slot, so choose it at declaration with form.field(...).domId(...)",
                        conflicts
                    ))

            val dupeCard =
                if dupes.isEmpty then Chunk.empty
                else
                    Chunk(KeyDiagnostics.card(
                        "kyo.uic.form",
                        "two fields declare the same domId — getElementById answers with whichever came first, so " +
                            "focus-first-invalid and the error summary will reach the wrong field",
                        dupes
                    ))

            val danglingCard =
                if dangling.isEmpty then Chunk.empty
                else
                    Chunk(KeyDiagnostics.card(
                        "kyo.uic.form",
                        "a label points at an id no element in this form carries — the usual cause is an id set on " +
                            "a control AFTER bind, which the label's forId was read before",
                        dangling
                    ))

            conflictCard ++ dupeCard ++ danglingCard
        end for
    end cards

    /** Element ids and label targets, with reactive regions resolved to what they currently
      * render. A list region is skipped: its rows are dynamic, so a one-shot reading of them
      * would be a reading of one moment.
      */
    private def scan(node: UI)(using Frame): Seen < Sync =
        def descend(children: Chunk[UI], here: Seen)(using Frame): Seen < Sync =
            Kyo.foreach(children)(scan).map(_.foldLeft(here)(_ ++ _))
        node match
            case l: Ast.Label =>
                descend(l.children, Seen(ids(l.attrs.identifier), ids(l.forId)))
            case e: Ast.Element =>
                descend(e.children, Seen(ids(e.attrs.identifier), Chunk.empty))
            case r: Ast.Reactive[?]   => r.signal.current(using r.frame).map(scan)
            case f: Ast.Fragment[?]   => descend(f.children, Seen.empty)
            case k: Ast.KeyedChild[?] => scan(k.child)
            case m: Ast.Mounted =>
                m.placeholderUI match
                    case Present(u) => scan(u)
                    case Absent     => Seen.empty
            case _ => Seen.empty
        end match
    end scan

    private def ids(v: Maybe[String]): Chunk[String] = v.map(Chunk(_)).getOrElse(Chunk.empty)

    final private case class Seen(ids: Chunk[String], labelFors: Chunk[String]):
        infix def ++(o: Seen): Seen = Seen(ids ++ o.ids, labelFors ++ o.labelFors)

    private object Seen:
        val empty: Seen = Seen(Chunk.empty, Chunk.empty)
end FormDiagnostics
