package kyo.uic.form

import kyo.*

/** How a field is DECLARED: a pure description with a default for every slot, turned into a
  * live [[FormField]] by [[declare]].
  *
  * Every slot here is a decision made once, before the field exists, and the defaults are the
  * answer most fields want — so the common declaration is
  * `form.field("").declare` and nothing else. What the older constructor form made mandatory,
  * this makes optional: an empty rule chain was spelled `Validator.all[String]()` and a field
  * that re-checks only at submit was spelled `Activation.Submit`, both of which read as
  * omissions rather than as choices.
  *
  * Nothing here runs an effect. The spec is a value you can build, pass around and hand to
  * `declare` at the point where the form scope wants it, which is what lets the id, the reveal
  * mode and the trigger set be settled BEFORE anything reads them — the ordering hazard that a
  * post-hoc setter on the live field would keep open.
  *
  * The chain reads in declaration order and ends in a terminal:
  * {{{
  * username <- form.field("chris")
  *               .rules(Validator.required() and Validator.minLength(2))
  *               .domId("login-username")
  *               .declare
  * }}}
  */
final case class FieldSpec[A] private[form] (
    private[form] val form: Form,
    private[form] val initialV: A,
    private[form] val rulesV: Validator[A],
    private[form] val onV: Set[Activation.Field],
    private[form] val domIdV: Maybe[String],
    private[form] val debounceV: Duration,
    private[form] val revealV: Reveal,
    private[form] val focusableV: Boolean
)(using CanEqual[A, A]):

    /** The rule chain this field STARTS with (default: [[Validator.none]] — every value passes).
      *
      * Compose several with `a and b`, which short-circuits on the first failure, or
      * [[Validator.all]]. This SETS the chain; [[FormField.addRule]] appends to it later, which
      * is what a cross-field rule needs, since the field it compares against does not exist yet
      * at declaration.
      */
    def rules(v: Validator[A]): FieldSpec[A] = copy(rulesV = v)

    /** When the rule chain is RE-COMPUTED (default: `Blur` + `Change`).
      *
      * Combinable, and independent of when a failure is SHOWN — that is [[revealWhen]]. The
      * default pairs with `Reveal.WhenTouched` to give the shape most forms want: silent while
      * the reader first types, live once they have left the field. `on(Activation.Submit)` is
      * how you opt out of both triggers; see [[Activation.Field]] for why that reads as a
      * choice rather than as an empty set.
      */
    def on(ts: Activation.Field*): FieldSpec[A] = copy(onV = ts.toSet)

    /** The control's DOM id (default: minted, `kyo-uic-N`).
      *
      * A minted id is unique and stable within a render, and unreferenceable from anything
      * authored outside the form: a hand-written `<label for>`, an `aria-describedby` on a
      * sibling hint, a deep link that focuses a field (`/settings#email`), an e2e selector.
      * Give one here and `bind` stamps it, `Label.forId(field.domId)` follows it, and the focus
      * jumps that address the field (`focusFirstInvalid`, the error summary) keep working.
      *
      * Yours to keep unique — the mint could not collide, a chosen id can. Two fields of one
      * form claiming the same id is reported at [[declare]].
      */
    def domId(id: String): FieldSpec[A] = copy(domIdV = Present(id))

    /** Wait this long after a change before running the rules (default: none).
      *
      * For an expensive rule — a round-trip that asks a server whether a name is taken. Only
      * the RUN is delayed; the value itself is not. A superseded run is dropped rather than
      * published, so the last edit is what the reader is told about.
      */
    def debounce(d: Duration): FieldSpec[A] = copy(debounceV = d)

    /** When a failure is DISPLAYED (default: [[Reveal.WhenTouched]] — after this field's first
      * blur/change, or after a submit).
      *
      * Independent of [[on]]: `Reveal.OnSubmit` stays quiet while editing and flags only at
      * submit, `Reveal.Immediate` flags an invalid initial value before any interaction, and
      * `Reveal.Manual` never shows automatically while still feeding `isValid` and blocking
      * submit.
      */
    def revealWhen(mode: Reveal): FieldSpec[A] = copy(revealV = mode)

    /** Whether a failed submit may move focus here (default: true).
      *
      * A hidden or programmatically-managed field opts out with `focusable(false)`: it is still
      * validated and still blocks submit, it is just skipped when submit picks which invalid
      * field to focus.
      */
    def focusable(v: Boolean): FieldSpec[A] = copy(focusableV = v)

    /** Allocate the field's state, register it with the form, and hand back the live handle.
      *
      * Registration order is declaration order, which is the order `focusFirstInvalid` and the
      * error summary walk — so a field declared here is reachable even if it is never bound.
      */
    def declare(using Frame): FormField[A] < Sync =
        // Explicitly typed rather than folded: a match with one effectful and one plain branch
        // infers `Any` and lands the effect inert (the module's effect-as-value trap).
        val id: String < Sync = domIdV match
            case Present(i) => i
            case Absent     => form.mintId()
        for
            id            <- id
            valueRef      <- Signal.initRef(initialV)
            baselineRef   <- Signal.initRef(initialV)
            touchedRef    <- Signal.initRef(false)
            validatingRef <- Signal.initRef(false)
            clientErrRef  <- Signal.initRef(Absent: Maybe[FieldError])
            serverErrRef  <- Signal.initRef(Absent: Maybe[ServerError[A]])
            epoch         <- AtomicInt.init(0)
            f = new FormField[A](
                valueRef,
                initialV,
                baselineRef,
                id,
                touchedRef,
                validatingRef,
                clientErrRef,
                serverErrRef,
                epoch,
                rulesV,
                onV,
                debounceV,
                revealV,
                focusableV,
                form.translator,
                form.submitCount
            )
            _ <- form.register(f)
        yield f
        end for
    end declare
end FieldSpec

object FieldSpec:

    /** The trigger set of a field that declares none: re-check on focus loss, and on every
      * change after that. See [[Activation.Field]] for why this and `Reveal.WhenTouched`
      * together are the shape most forms want.
      */
    val defaultActivation: Set[Activation.Field] = Set(Activation.Blur, Activation.Change)

    private[form] def init[A](form: Form, initial: A)(using CanEqual[A, A]): FieldSpec[A] =
        FieldSpec(form, initial, Validator.none[A], defaultActivation, Absent, Duration.Zero, Reveal.WhenTouched, true)
end FieldSpec

/** [[FieldSpec]] for a numeric field, over the typed value `A` rather than the `Double` the
  * control actually binds. The rules are stated in `A` and adapted through the
  * [[NumberCodec]] at [[declare]] time, which is the only reason this is a separate spec
  * rather than a `FieldSpec[Double]`.
  */
final case class NumberFieldSpec[A] private[form] (
    private[form] val under: FieldSpec[Double],
    private[form] val codec: NumberCodec[A],
    private[form] val rulesV: Validator[A]
):
    /** @see [[FieldSpec.rules]] — stated over `A`, run on the parsed value. */
    def rules(v: Validator[A]): NumberFieldSpec[A] = copy(rulesV = v)

    /** @see [[FieldSpec.on]] */
    def on(ts: Activation.Field*): NumberFieldSpec[A] = copy(under = under.on(ts*))

    /** @see [[FieldSpec.domId]] */
    def domId(id: String): NumberFieldSpec[A] = copy(under = under.domId(id))

    /** @see [[FieldSpec.debounce]] */
    def debounce(d: Duration): NumberFieldSpec[A] = copy(under = under.debounce(d))

    /** @see [[FieldSpec.revealWhen]] */
    def revealWhen(mode: Reveal): NumberFieldSpec[A] = copy(under = under.revealWhen(mode))

    /** @see [[FieldSpec.focusable]] */
    def focusable(v: Boolean): NumberFieldSpec[A] = copy(under = under.focusable(v))

    /** @see [[FieldSpec.declare]] */
    def declare(using Frame): NumberField[A] < Sync =
        under.rules(Validator.async(d => rulesV.run(codec.fromDouble(d)))).declare
            .map(f => new NumberField[A](f, codec))
end NumberFieldSpec

/** [[FieldSpec]] for a date field. The picker's ISO string is the source of truth; the rules
  * are stated over the parsed value and are SKIPPED while the picker is empty, so `required`
  * belongs on the emptiness, not on the parse.
  */
final case class DateFieldSpec[A] private[form] (
    private[form] val under: FieldSpec[String],
    private[form] val codec: DateCodec[A],
    private[form] val rulesV: Validator[A]
):
    /** @see [[FieldSpec.rules]] — stated over `A`, skipped while the picker is empty. */
    def rules(v: Validator[A]): DateFieldSpec[A] = copy(rulesV = v)

    /** @see [[FieldSpec.on]] */
    def on(ts: Activation.Field*): DateFieldSpec[A] = copy(under = under.on(ts*))

    /** @see [[FieldSpec.domId]] */
    def domId(id: String): DateFieldSpec[A] = copy(under = under.domId(id))

    /** @see [[FieldSpec.debounce]] */
    def debounce(d: Duration): DateFieldSpec[A] = copy(under = under.debounce(d))

    /** @see [[FieldSpec.revealWhen]] */
    def revealWhen(mode: Reveal): DateFieldSpec[A] = copy(under = under.revealWhen(mode))

    /** @see [[FieldSpec.focusable]] */
    def focusable(v: Boolean): DateFieldSpec[A] = copy(under = under.focusable(v))

    /** @see [[FieldSpec.declare]] */
    def declare(using Frame): DateField[A] < Sync =
        val adapted: Validator[String] = Validator.async: s =>
            codec.fromInput(s) match
                case Present(a) => rulesV.run(a)
                case Absent     => (Absent: Maybe[FieldError])
        under.rules(adapted).declare.map(f => new DateField[A](f, codec))
    end declare
end DateFieldSpec
