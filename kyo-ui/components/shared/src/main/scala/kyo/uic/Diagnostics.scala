package kyo.uic

import kyo.*
import kyo.UI.*

/** What a component says, on the page, about a mistake it cannot fail loudly about on its own.
  *
  * A wrong key or a slot nobody hears does not crash: it quietly applies a selection to the wrong record, or paints the
  * reader's change away, and only with the data that triggers it. kyo-ui's engine sets the precedent: `foreachKeyed` warns
  * on duplicate keys and a duplicate mounted key paints an inline error card. A component `render` here is pure (`UI`, no
  * `Sync`), so a log is not reachable; the card is.
  */
private[uic] object Diagnostics:

    /** The keys that appear more than once, in first-seen order. Empty means the projection is a real identity for this
      * data.
      */
    def duplicates(keys: Seq[String]): Seq[String] =
        keys.groupBy(identity).collect { case (k, occurrences) if occurrences.sizeIs > 1 => k }
            .toSeq.sortBy(keys.indexOf)

    /** The loud inline card. `component` names the component, `advice` the setter to reach for; the offending keys are
      * listed so the data problem is findable.
      */
    def card(component: String, advice: String, keys: Seq[String])(using Frame): UI =
        val listed = if keys.isEmpty then "" else keys.take(5).mkString(" (", ", ", if keys.sizeIs > 5 then ", ...)" else ")")
        div
            .cssClass("p-uic-key-error")
            .role("alert")(s"$component: $advice$listed")
    end card

    /** The card for a typed binding that holds a value no option carries: the data and the binding disagree, and the value
      * stays where it is until the app changes it. `loading` is the way to say the options are not all there yet.
      */
    def unknown(component: String, labels: Seq[String])(using Frame): UI =
        card(
            component,
            "the bound value is not among the options, so no row shows it chosen; set loading while the options arrive",
            labels
        )
    end unknown

    /** The card for a value a reader edits, bound one way with nobody to hear the edit: every change is put back. `setter`
      * names the binding, `events` the events that would hear it.
      */
    def unheard(component: String, setter: String, events: String)(using Frame): UI =
        card(
            component,
            s"$setter is bound one way and nothing hears a change, so the reader cannot change it; add $events, or bind a SignalRef",
            Nil
        )
    end unheard
end Diagnostics
