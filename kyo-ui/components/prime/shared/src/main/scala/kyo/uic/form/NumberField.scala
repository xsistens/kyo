package kyo.uic.form

import kyo.*

/** A typed numeric field (`Int` / `Long` / `Double`). A thin typed façade over a
  * `Double`-valued [[FormField]] (which owns the two-way ref `uic.InputNumber` binds):
  * the `Double` stays the single source of truth and the typed value is derived through a
  * [[NumberCodec]], sidestepping a second `SignalRef[A]` (which `SignalRef`, having no
  * bidirectional map, cannot provide).
  *
  * The value is a total `Signal[A]` (not `Maybe`): `InputNumber` always holds a `Double`,
  * and integer masking (see [[NumberCodec.integer]]) keeps whole-valued fields exact.
  *
  * Its rules are stated over `A` and may refine it like a field's ([[FieldSpec.rules]]): `Valid` is what
  * they yield, which is what [[Form.submit]] hands its handler, `A` when there are none.
  *
  * Constructed by [[Form.numberField]], never directly.
  */
sealed abstract class NumberField[A] private[form] (
    private[form] val codec: NumberCodec[A]
) extends Validated:

    /** The `Double` field the control binds, carrying this field's rules adapted through the codec, and so validating
      * to the same `Valid`.
      */
    private[form] def underlying: FormField[Double] { type Valid = NumberField.this.Valid }

    private[form] def submitField: FormField[?] = underlying

    private[form] def owner: Form = underlying.owner

    private[form] def verdict(using Frame): Result.Partial[FieldError, Valid] < Async = underlying.verdict

    private given CanEqual[A, A] = codec.equality

    /** The typed value, reactively derived from the bound `Double`. */
    def value(using Frame): Signal[A] = underlying.value.map(codec.fromDouble)

    /** True once the field has been entered and left (first blur/commit). */
    def touched: Signal[Boolean] = underlying.touched

    /** True while an async validator is in flight; drive a spinner off this. */
    def validating: Signal[Boolean] = underlying.validating

    /** Set/replace a server error (value-scoped, delegated to the underlying field). */
    def setError(e: Maybe[FieldError])(using Frame): Unit < Sync = underlying.setError(e)

    /** Set a server error as text (value-scoped, delegated to the underlying field). */
    def setError(message: String)(using Frame): Unit < Sync = underlying.setError(message)

    /** The raw (ungated) error, delegated to the underlying field. */
    def error(using Frame): Signal[Maybe[FieldError]] = underlying.error

    /** The reveal-gated error, delegated to the underlying field. */
    def visibleError(using Frame): Signal[Maybe[FieldError]] = underlying.visibleError

    /** The gated, translated inline message, delegated to the underlying field. */
    def message(using Frame): Signal[Maybe[String]] = underlying.message

    /** This field typed with its rules' result, which is what a chained call hands back: `this.type` cannot be lifted
      * into a computation, and the refinement is what a chain must not lose.
      */
    private def self: NumberField[A] { type Valid = NumberField.this.Valid } = this

    /** Append another rule over the typed value `A`, adapted onto the underlying `Double`
      * field through the [[NumberCodec]]. As with [[FormField.addRule]], what it yields is not
      * kept, and a rule that reads another field is followed from the moment it is appended.
      * Chainable.
      */
    def addRule(v: Rule[A, Any])(using Frame): (NumberField[A] { type Valid = NumberField.this.Valid }) < Sync =
        underlying.addRule(v.contramap(codec.fromDouble)).map(_ => self)

    /** Predicate rule over the typed value `A`, mirroring [[Form.satisfy]]: sugar for
      * `addRule(Validator.satisfy(code, args)(p))`. Chainable.
      */
    def satisfy(code: String, args: Map[String, String] = Map.empty)(p: A => Boolean)(using
        Frame
    ): (NumberField[A] { type Valid = NumberField.this.Valid }) < Sync =
        addRule(Validator.satisfy(code, args)(p))

    /** This field must equal the live value of `other`: `addRule(Validator.matchesField(other, code))`, a rule that
      * carries its dependency (see [[FormField.matches]]). Chainable.
      */
    def matches(other: Signal[A], code: String = "must-match")(using
        CanEqual[A, A],
        Frame
    ): (NumberField[A] { type Valid = NumberField.this.Valid }) < Sync =
        addRule(Validator.matchesField(other, code))

    /** Re-validate whenever any of `sigs` emits (cross-field dependency), delegated to the
      * underlying field. [[Validator.reading]] is the preferred shape for a rule that reads another field.
      */
    def dependsOn(sigs: Signal[?]*)(using Frame): Unit < (Async & Scope) = underlying.dependsOn(sigs*)

    /** True while the value differs from the live baseline (compares the underlying Double). */
    def isDirty(using Frame): Signal[Boolean] = underlying.isDirty

    /** Snapshot the current value as the new clean baseline (delegates to the underlying field). */
    def rebase(using Frame): Unit < Sync = underlying.rebase

end NumberField

object NumberField:
    private[form] def of[A, B](field: FormField[Double] { type Valid = B }, codec: NumberCodec[A]): NumberField[A] { type Valid = B } =
        new NumberField[A](codec):
            type Valid = B
            private[form] def underlying: FormField[Double] { type Valid = B } = field
end NumberField
