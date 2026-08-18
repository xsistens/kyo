package kyo.uic.form

import kyo.*

/** A typed, optionally zone-aware date field. It is a thin typed façade over a
  * `String`-valued [[FormField]] (which owns the two-way ISO ref the picker binds):
  * the string stays the single source of truth, and the typed value is derived through
  * a [[DateCodec]]. This sidesteps a second `SignalRef[A]` and the lossy round-tripping
  * that binding a `ZonedDateTime` straight through a date-only control would need.
  *
  * The value is `Maybe[A]` because a picker can be empty; validators supplied to
  * [[Form.dateField]] run on the parsed `A` and are simply skipped while empty.
  *
  * Constructed by [[Form.dateField]] — never directly.
  */
final class DateField[A] private[form] (private[form] val underlying: FormField[String], codec: DateCodec[A]):

    private given CanEqual[A, A] = codec.equality

    /** The typed value, reactively derived: `Absent` while no date is selected. */
    def value(using Frame): Signal[Maybe[A]] = underlying.value.map(codec.fromInput)

    /** True once the field has been entered and left (first blur/commit). */
    val touched: Signal[Boolean] = underlying.touched

    /** True while an async validator is in flight. */
    val validating: Signal[Boolean] = underlying.validating

    /** Set/replace a server error (value-scoped, delegated to the underlying field). */
    def setError(e: Maybe[FieldError])(using Frame): Unit < Sync = underlying.setError(e)

    /** The raw (ungated) error, delegated to the underlying field. */
    def error(using Frame): Signal[Maybe[FieldError]] = underlying.error

    /** The reveal-gated error, delegated to the underlying field. */
    def visibleError(using Frame): Signal[Maybe[FieldError]] = underlying.visibleError

    /** The gated, translated inline message, delegated to the underlying field. */
    def message(using Frame): Signal[Maybe[String]] = underlying.message

    /** Choose when this field's error is DISPLAYED (delegates to the underlying field).
      * Chainable — returns this façade.
      */
    def revealWhen(mode: Reveal): DateField[A] =
        discard(underlying.revealWhen(mode))
        this

    /** Append another rule over the parsed value `A` — adapted onto the underlying `String`
      * field through the [[DateCodec]] (skipped while the picker is empty, like the constructor
      * rules). Chainable.
      */
    def addRule(v: Validator[A])(using Frame): DateField[A] =
        discard(underlying.addRule(Validator.async: s =>
            codec.fromInput(s) match
                case Present(a) => v.run(a)
                case Absent     => (Absent: Maybe[FieldError])))
        this
    end addRule

    /** Predicate rule over the parsed value `A`, mirroring [[Form.satisfy]] — sugar for
      * `addRule(Validator.satisfy(code, args)(p))` (skipped while the picker is empty). Chainable.
      */
    def satisfy(code: String, args: Map[String, String] = Map.empty)(p: A => Boolean)(using Frame): DateField[A] =
        addRule(Validator.satisfy(code, args)(p))

    /** This field must equal the live value of `other` — one call for `addRule` +
      * [[dependsOn]] (see [[FormField.matches]]).
      */
    def matches(other: Signal[A], code: String = "must-match")(using CanEqual[A, A], Frame): Unit < (Async & Scope) =
        discard(addRule(Validator.matchesField(other, code)))
        dependsOn(other)

    /** Re-validate whenever any of `sigs` emits (cross-field dependency), delegated to the
      * underlying field.
      */
    def dependsOn(sigs: Signal[?]*)(using Frame): Unit < (Async & Scope) = underlying.dependsOn(sigs*)

    /** Include/exclude the picker as a focus-first-invalid target (delegates to the
      * underlying field). Returns this façade for chaining.
      */
    def focusable(v: Boolean): DateField[A] =
        discard(underlying.focusable(v))
        this

    /** True while the picked value differs from the live baseline (compares the underlying
      * ISO string).
      */
    def isDirty(using Frame): Signal[Boolean] = underlying.isDirty

    /** Snapshot the current value as the new clean baseline (delegates to the underlying field). */
    def rebase(using Frame): Unit < Sync = underlying.rebase

end DateField
