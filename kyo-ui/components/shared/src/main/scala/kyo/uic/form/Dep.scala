package kyo.uic.form

import kyo.*

/** A signal a rule reads, and what a change of it does to the rule's verdict: the argument of [[Validator.reading]] and
  * [[Rule.reading]].
  *
  * A rule that reads another field judges a pair, its field's value and what it read, so a verdict it found is only good
  * while both stay. A change of the field's own value is one of the field's events, and the rule's [[Rule.on]] decides
  * whether it runs. A change of a dependency is an event of its own, and the dependency decides:
  *
  *   - [[Dep.recheck]]: the rule runs at once with the new value, whatever its `on`. What a cheap check wants, so the error
  *     appears and goes while the reader edits the OTHER field.
  *   - [[Dep.discard]]: nothing runs. The verdict about the old value is dropped and the rule is NOT YET CHECKED until one
  *     of its own events, or the submit, runs it. What an expensive check wants: no request goes out that the reader did
  *     not ask for. A dependency changed back to the value the verdict was kept for gets that verdict back without a run.
  *
  * The reaction and `on` never override each other: `on` governs the field's own events, the reaction the dependency's.
  * A submit, and a DataTable cell's commit, run every rule with the dependencies' current values either way.
  *
  * A bare signal passed to the one-dependency `reading` takes the default of the rule it builds: `recheck` for a sync
  * rule, `discard` for an async one. Several dependencies each name their reaction.
  * {{{
  * Validator.reading(Dep.discard(country), Dep.recheck(minAge)).async((birthday: LocalDate, d: (String, Int)) => ...)
  * }}}
  */
final class Dep[D] private[form] (private[form] val signal: Signal[D], private[form] val reaction: Maybe[Dep.Reaction])

object Dep:

    /** A change of `sig` runs the rule at once with the new value. */
    def recheck[D](sig: Signal[D]): Dep[D] = new Dep(sig, Present(Reaction.Recheck))

    /** A change of `sig` drops the rule's verdict and runs nothing: the rule is not yet checked until its own event. */
    def discard[D](sig: Signal[D]): Dep[D] = new Dep(sig, Present(Reaction.Discard))

    /** `sig` with the reaction of the rule it ends up in: what a bare signal in the one-dependency `reading` is. */
    private[form] def default[D](sig: Signal[D]): Dep[D] = new Dep(sig, Absent)

    private[form] enum Reaction derives CanEqual:
        case Recheck, Discard
end Dep
