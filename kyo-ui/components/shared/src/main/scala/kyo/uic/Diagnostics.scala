package kyo.uic

import kyo.*
import kyo.UI.*

/** One thing a component can say about a mistake, and whether it holds right now. Its arguments are evaluated whether it
  * fires or not, so a costly key stays behind a plain `if` around [[Diagnostics.cards]].
  */
final private[uic] case class Diagnostic(when: Boolean, advice: String, keys: Seq[String] = Nil)

private[uic] object Diagnostic:

    /** Fires when there is something to name. */
    def naming(advice: String, keys: Seq[String]): Diagnostic = Diagnostic(keys.nonEmpty, advice, keys)

    /** A slot bound one way that nobody hears, while `on` says the slot does anything at all. */
    def unheard[A](slot: Slot[A], name: Slot.Name, on: Boolean = true): Diagnostic = Diagnostic(on && slot.deaf, name.advice)

    /** A typed binding that holds a value no option carries: the data and the binding disagree, and the value stays where it
      * is until the app changes it. `loading` is the way to say the options are not all there yet.
      */
    def unknown(labels: Seq[String]): Diagnostic =
        naming("the bound value is not among the options, so no row shows it chosen; set loading while the options arrive", labels)
end Diagnostic

/** What a component says, on the page, about a mistake it cannot fail loudly about on its own.
  *
  * A wrong key or a slot nobody hears does not crash: it quietly applies a selection to the wrong record, or paints the
  * reader's change away, and only with the data that triggers it. kyo-ui's engine sets the precedent: `foreachKeyed` warns
  * on duplicate keys and a duplicate mounted key paints an inline error card. A component `render` here is pure (`UI`, no
  * `Sync`), so a log is not reachable; the card is.
  *
  * Where a card goes follows the component, and the three places are not equal (see "Diagnostics" in this module's
  * CLAUDE.md): in front of a container ([[before]]), behind a field with its message row between ([[after]]), inside the root
  * of a table or a list ([[cards]], as children).
  */
private[uic] object Diagnostics:

    /** The keys that appear more than once, in first-seen order. Empty means the projection is a real identity for this
      * data.
      */
    def duplicates[K](keys: Seq[K]): Seq[K] =
        keys.groupBy(identity).collect { case (k, occurrences) if occurrences.sizeIs > 1 => k }
            .toSeq.sortBy(keys.indexOf)

    /** The cards of what fires, in the order given. */
    def cards(component: String)(ds: Diagnostic*)(using Frame): Chunk[UI] =
        Chunk.from(ds.collect { case d if d.when => card(component, d.advice, d.keys) })

    /** `ui` with the cards of what fires in front of it. With none it is `ui` itself, so the bytes stay. */
    def before(ui: UI)(component: String)(ds: Diagnostic*)(using Frame): UI =
        val cs = cards(component)(ds*)
        if cs.isEmpty then ui else fragment((cs :+ ui)*)

    /** `ui` with the cards of what fires behind it. With none it is `ui` itself, so the bytes stay. */
    def after(ui: UI)(component: String)(ds: Diagnostic*)(using Frame): UI =
        val cs = cards(component)(ds*)
        if cs.isEmpty then ui else fragment((ui +: cs)*)

    /** [[cards]] whose diagnoses depend on a switch: a region of its own over a signal, so the cards come and go with it and
      * nothing around them is drawn again.
      */
    def cardsWhile(on: BoolValue)(component: String)(ds: Boolean => Seq[Diagnostic])(using Frame): Chunk[UI] =
        on match
            case ReactiveValue.Const(b) => cards(component)(ds(b)*)
            case ReactiveValue.Dyn(sig) => Chunk(sig.render(b => fragment(cards(component)(ds(b)*)*)))

    /** [[before]] whose diagnoses depend on a switch. */
    def beforeWhile(ui: UI)(on: BoolValue)(component: String)(ds: Boolean => Seq[Diagnostic])(using Frame): UI =
        val cs = cardsWhile(on)(component)(ds)
        if cs.isEmpty then ui else fragment((cs :+ ui)*)

    /** The loud inline card. `component` names the component, `advice` the setter to reach for; the offending keys are
      * listed so the data problem is findable.
      */
    def card(component: String, advice: String, keys: Seq[String])(using Frame): UI =
        val listed = if keys.isEmpty then "" else keys.take(5).mkString(" (", ", ", if keys.sizeIs > 5 then ", ...)" else ")")
        div
            .cssClass("p-uic-key-error")
            .role("alert")(s"$component: $advice$listed")
    end card

    /** The card for a typed binding that holds a value no option carries. */
    def unknown(component: String, labels: Seq[String])(using Frame): UI =
        card(component, Diagnostic.unknown(labels).advice, labels)

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
