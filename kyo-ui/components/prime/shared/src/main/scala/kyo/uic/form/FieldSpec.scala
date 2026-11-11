package kyo.uic.form

import kyo.*

/** How a field is DECLARED: a pure description with a default for every slot, turned into a
  * live [[FormField]] by [[declare]].
  *
  * Every slot here is a decision made once, before the field exists, and the defaults are the
  * answer most fields want, so the common declaration is
  * `form.field("").declare` and nothing else. What the older constructor form made mandatory,
  * this makes optional: an empty rule chain was spelled `Validator.all[String]()` and a field
  * that re-checks only at submit was spelled `Activation.Submit`, both of which read as
  * omissions rather than as choices.
  *
  * Nothing here runs an effect. The spec is a value you can build, pass around and hand to
  * `declare` at the point where the form scope wants it, which is what lets the id, the reveal
  * mode and the trigger set be settled BEFORE anything reads them: the ordering hazard that a
  * post-hoc setter on the live field would keep open.
  *
  * `Valid` is the type the rule chain yields, `A` until [[rules]] says otherwise, and every
  * other setter keeps it, so the declared field knows what [[Form.submit]] will hand over.
  *
  * The chain reads in declaration order and ends in a terminal:
  * {{{
  * username <- form.field("chris")
  *               .rules(Validator.required() and Validator.minLength(2))
  *               .domId("login-username")
  *               .declare
  * }}}
  */
sealed abstract class FieldSpec[A] private[form] (
    private[form] val form: Form,
    private[form] val initialV: A,
    private[form] val onV: Maybe[Set[Activation.Field]],
    private[form] val domIdV: Maybe[String],
    private[form] val debounceV: Duration,
    private[form] val revealV: Reveal,
    private[form] val focusableV: Boolean
)(using CanEqual[A, A]):

    /** What the rule chain yields once it passes: `A` with no rules, the last rule's result type otherwise. */
    type Valid

    private[form] def rulesV: Rule[A, Valid]

    /** This spec with its rules and any slot replaced. One place builds a spec, so a new slot is added once. */
    private def rebuild[B](
        rules: Rule[A, B],
        on: Maybe[Set[Activation.Field]] = this.onV,
        domId: Maybe[String] = this.domIdV,
        debounce: Duration = this.debounceV,
        reveal: Reveal = this.revealV,
        focusable: Boolean = this.focusableV
    ): FieldSpec[A] { type Valid = B } =
        FieldSpec.of(form, initialV, rules, on, domId, debounce, reveal, focusable)

    /** The rule chain this field STARTS with (default: [[Validator.none]], every value passes), and so the type it
      * validates to: a chain that refines (`Validator.chosen()` on a `Maybe[A]` field) makes the declared field's
      * [[FormField.Valid]] the refined type, which is what [[Form.submit]] hands its handler.
      *
      * Compose several with `a and b`, which short-circuits on the first failure, or
      * [[Validator.all]]. This SETS the chain; [[FormField.addRule]] appends to it later, which
      * is what a cross-field rule needs, since the field it compares against does not exist yet
      * at declaration.
      */
    def rules[B](r: Rule[A, B]): FieldSpec[A] { type Valid = B } = rebuild(r)

    /** The field's own events, which do two things.
      *
      * They are the FALLBACK for when its rules run: a rule with its own [[Rule.on]] keeps it,
      * every other rule runs on these instead of its default (every change and the blur, the
      * blur alone for [[Validator.async]]). Without this, each rule runs on its own default.
      *
      * And they are what marks the field touched (default: `Blur` + `Change`), which is what
      * [[Reveal.WhenTouched]] waits for. That pairing gives the shape most forms want: silent
      * while the reader first types, live once they have left the field. When a failure is
      * SHOWN is [[revealWhen]]'s question, not this one's.
      *
      * `on(Activation.Submit)` is how a field opts out of both: no event touches it, and its rules
      * without an `on` of their own wait for the submit. See [[Activation.Field]] for why that
      * reads as a choice rather than as an empty set.
      */
    def on(ts: Activation.Field*): FieldSpec[A] { type Valid = FieldSpec.this.Valid } = rebuild(rulesV, on = Present(ts.toSet))

    /** The control's DOM id (default: minted, `kyo-id-N`).
      *
      * A minted id is unique and stable within a render, and unreferenceable from anything
      * authored outside the form: a hand-written `<label for>`, an `aria-describedby` on a
      * sibling hint, a deep link that focuses a field (`/settings#email`), an e2e selector.
      * Give one here and `bind` stamps it, `Label.forId(field.domId)` follows it, and the focus
      * jumps that address the field (`focusFirstInvalid`, the error summary) keep working.
      *
      * Yours to keep unique: the mint could not collide, a chosen id can. Two fields of one
      * form claiming the same id is reported at [[declare]].
      */
    def domId(id: String): FieldSpec[A] { type Valid = FieldSpec.this.Valid } = rebuild(rulesV, domId = Present(id))

    /** Wait this long after a change before running the rules (default: none).
      *
      * For an expensive rule, such as a round-trip that asks a server whether a name is taken. Only
      * the RUN is delayed; the value itself is not. A run that a later run with a rule to run
      * (the next keystroke, a submit, a reset) overtakes during the wait runs no rule at all, so a
      * reader typing faster than the debounce has the rules asked once, about the last value. An
      * event no rule is due for (a blur the field's rules ignore) overtakes nothing.
      */
    def debounce(d: Duration): FieldSpec[A] { type Valid = FieldSpec.this.Valid } = rebuild(rulesV, debounce = d)

    /** When a failure is DISPLAYED (default: [[Reveal.WhenTouched]], after this field's first
      * blur/change, or after a submit).
      *
      * Independent of [[on]]: `Reveal.OnSubmit` stays quiet while editing and flags only at
      * submit, `Reveal.Immediate` flags an invalid initial value before any interaction, and
      * `Reveal.Manual` never shows automatically while still feeding `isValid` and blocking
      * submit.
      */
    def revealWhen(mode: Reveal): FieldSpec[A] { type Valid = FieldSpec.this.Valid } = rebuild(rulesV, reveal = mode)

    /** Whether a failed submit may move focus here (default: true).
      *
      * A hidden or programmatically-managed field opts out with `focusable(false)`: it is still
      * validated and still blocks submit, it is just skipped when submit picks which invalid
      * field to focus.
      */
    def focusable(v: Boolean): FieldSpec[A] { type Valid = FieldSpec.this.Valid } = rebuild(rulesV, focusable = v)

    /** Allocate the field's state, register it with the form, and hand back the live handle, which knows the type its
      * rules validate to.
      *
      * Registration order is declaration order, which is the order `focusFirstInvalid` and the
      * error summary walk, so a field declared here is reachable even if it is never bound.
      */
    def declare(using Frame): (FormField[A] { type Valid = FieldSpec.this.Valid }) < Sync = declareIn(form)

    /** [[declare]] into `scope` rather than the form the spec was made from: how a field group declares its members,
      * since a group's fields belong to the group's scope ([[Form.fields]]).
      */
    private[form] def declareIn(scope: Form)(using Frame): (FormField[A] { type Valid = FieldSpec.this.Valid }) < Sync =
        // Explicitly typed rather than folded: a match with one effectful and one plain branch
        // infers `Any` and lands the effect inert (the module's effect-as-value trap).
        val id: String < Sync = domIdV match
            case Present(i) => i
            case Absent     => scope.mintId()
        for
            id            <- id
            valueRef      <- Signal.initRef(initialV)
            baselineRef   <- Signal.initRef(initialV)
            touchedRef    <- Signal.initRef(false)
            validatingRef <- Signal.initRef(false)
            clientRef     <- Signal.initRef(ClientVerdict.Checked(Absent): ClientVerdict)
            serverErrRef  <- Signal.initRef(Absent: Maybe[ServerError[A]])
            epoch         <- AtomicInt.init(0)
            resetWrite    <- AtomicRef.init(Maybe.empty[A])
            namedRef      <- Signal.initRef(false)
            f             <- FormField.declared[A, Valid](
                valueRef,
                initialV,
                baselineRef,
                id,
                touchedRef,
                validatingRef,
                clientRef,
                serverErrRef,
                epoch,
                resetWrite,
                rulesV,
                onV,
                debounceV,
                revealV,
                focusableV,
                scope.translator,
                scope.submitCount,
                namedRef,
                scope
            )
            _ <- scope.register(f)
        yield f
        end for
    end declareIn
end FieldSpec

object FieldSpec:

    /** The events that touch a field that declares none: focus loss, and every change. See
      * [[Activation.Field]] for why this and `Reveal.WhenTouched` together are the shape most forms
      * want. It is also every rule's default, except [[Validator.async]]'s.
      */
    val defaultActivation: Set[Activation.Field] = Rule.defaultOn

    private[form] def init[A](form: Form, initial: A)(using CanEqual[A, A]): FieldSpec[A] { type Valid = A } =
        of(form, initial, Validator.none[A], Absent, Absent, Duration.Zero, Reveal.WhenTouched, true)

    private def of[A, B](
        form: Form,
        initial: A,
        chainRules: Rule[A, B],
        on: Maybe[Set[Activation.Field]],
        domId: Maybe[String],
        debounce: Duration,
        reveal: Reveal,
        focusable: Boolean
    )(using CanEqual[A, A]): FieldSpec[A] { type Valid = B } =
        new FieldSpec[A](form, initial, on, domId, debounce, reveal, focusable):
            type Valid = B
            private[form] val rulesV: Rule[A, B] = chainRules
end FieldSpec

/** [[FieldSpec]] for a numeric field, over the typed value `A` rather than the `Double` the
  * control actually binds. The rules are stated in `A` and adapted through the
  * [[NumberCodec]] at [[declare]] time, which is the only reason this is a separate spec
  * rather than a `FieldSpec[Double]`.
  *
  * Its rules refine the way a field's do: `Valid` is what the chain yields, so a numeric field
  * hands [[Form.submit]] the type its rules validate to, `A` with none.
  */
sealed abstract class NumberFieldSpec[A] private[form] (
    private[form] val under: FieldSpec[Double],
    private[form] val codec: NumberCodec[A]
):
    /** What the rule chain yields once it passes: `A` with no rules. */
    type Valid

    private[form] def rulesV: Rule[A, Valid]

    private def rebuild[B](under: FieldSpec[Double], rules: Rule[A, B]): NumberFieldSpec[A] { type Valid = B } =
        NumberFieldSpec.of(under, codec, rules)

    /** @see [[FieldSpec.rules]], stated over `A`, run on the parsed value. */
    def rules[B](r: Rule[A, B]): NumberFieldSpec[A] { type Valid = B } = rebuild(under, r)

    /** @see [[FieldSpec.on]] */
    def on(ts: Activation.Field*): NumberFieldSpec[A] { type Valid = NumberFieldSpec.this.Valid } =
        rebuild(under.on(ts*), rulesV)

    /** @see [[FieldSpec.domId]] */
    def domId(id: String): NumberFieldSpec[A] { type Valid = NumberFieldSpec.this.Valid } = rebuild(under.domId(id), rulesV)

    /** @see [[FieldSpec.debounce]] */
    def debounce(d: Duration): NumberFieldSpec[A] { type Valid = NumberFieldSpec.this.Valid } = rebuild(under.debounce(d), rulesV)

    /** @see [[FieldSpec.revealWhen]] */
    def revealWhen(mode: Reveal): NumberFieldSpec[A] { type Valid = NumberFieldSpec.this.Valid } =
        rebuild(under.revealWhen(mode), rulesV)

    /** @see [[FieldSpec.focusable]] */
    def focusable(v: Boolean): NumberFieldSpec[A] { type Valid = NumberFieldSpec.this.Valid } = rebuild(under.focusable(v), rulesV)

    /** @see [[FieldSpec.declare]] */
    def declare(using Frame): (NumberField[A] { type Valid = NumberFieldSpec.this.Valid }) < Sync = declareIn(under.form)

    /** @see [[FieldSpec.declareIn]] */
    private[form] def declareIn(scope: Form)(using Frame): (NumberField[A] { type Valid = NumberFieldSpec.this.Valid }) < Sync =
        under.rules(rulesV.contramap(codec.fromDouble)).declareIn(scope)
            .map(f => NumberField.of(f, codec))
end NumberFieldSpec

object NumberFieldSpec:
    private[form] def of[A, B](under: FieldSpec[Double], codec: NumberCodec[A], chainRules: Rule[A, B]): NumberFieldSpec[A] {
        type Valid = B
    } =
        new NumberFieldSpec[A](under, codec):
            type Valid = B
            private[form] val rulesV: Rule[A, B] = chainRules
end NumberFieldSpec
