package kyo.uic.form

import kyo.*

/** A typed numeric field (`Int` / `Long` / `Double`). A thin typed façade over a
  * `Double`-valued [[FormField]] (which owns the two-way ref `uic.InputNumber` binds):
  * the `Double` stays the single source of truth and the typed value is derived through a
  * [[NumberCodec]] — sidestepping a second `SignalRef[A]` (which `SignalRef`, having no
  * bidirectional map, cannot provide).
  *
  * The value is a total `Signal[A]` (not `Maybe`): `InputNumber` always holds a `Double`,
  * and integer masking (see [[NumberCodec.integer]]) keeps whole-valued fields exact.
  *
  * Constructed by [[Form.numberField]] — never directly.
  */
final class NumberField[A] private[form] (
    private[form] val underlying: FormField[Double],
    private[form] val codec: NumberCodec[A]
):

    private given CanEqual[A, A] = codec.equality

    /** The typed value, reactively derived from the bound `Double`. */
    def value(using Frame): Signal[A] = underlying.value.map(codec.fromDouble)

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
    def revealWhen(mode: Reveal): NumberField[A] =
        discard(underlying.revealWhen(mode))
        this

    /** Append another rule over the typed value `A` — adapted onto the underlying `Double`
      * field through the [[NumberCodec]]. Chainable.
      */
    def addRule(v: Validator[A])(using Frame): NumberField[A] =
        discard(underlying.addRule(Validator.async(d => v.run(codec.fromDouble(d)))))
        this

    /** Predicate rule over the typed value `A`, mirroring [[Form.satisfy]] — sugar for
      * `addRule(Validator.satisfy(code, args)(p))`. Chainable.
      */
    def satisfy(code: String, args: Map[String, String] = Map.empty)(p: A => Boolean)(using Frame): NumberField[A] =
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

    /** Include/exclude the field as a focus-first-invalid target (delegates to the
      * underlying field). Returns this façade for chaining.
      */
    def focusable(v: Boolean): NumberField[A] =
        discard(underlying.focusable(v))
        this

    /** True while the value differs from the live baseline (compares the underlying Double). */
    def isDirty(using Frame): Signal[Boolean] = underlying.isDirty

    /** Snapshot the current value as the new clean baseline (delegates to the underlying field). */
    def rebase(using Frame): Unit < Sync = underlying.rebase

end NumberField
