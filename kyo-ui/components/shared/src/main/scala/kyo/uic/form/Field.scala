package kyo.uic.form

import kyo.*
import kyo.uic.BooleanFormControl
import kyo.uic.FileFormControl
import kyo.uic.MultiSelectFormControl
import kyo.uic.NumberFormControl
import kyo.uic.TextFormControl

/** A single validated field of value type `A`. Owns its value/touched/validating/error
  * signals and a mutable rule chain; the owning [[Form]] holds a reference and validates
  * it on submit. Display is gated: the error surfaces only once the field is `touched`
  * (entered and left) OR the form has been submitted (`submitCount > 0`).
  *
  * `A` is the field's value type — `String` for text controls, `Boolean` for
  * checkbox/switch/radio, and so on. The gating, message, and aggregation logic is
  * value-agnostic (it only ever handles `Maybe[FieldError]`); only value binding and the
  * blur trigger are type-specific, which is why `bind` splits by control family below.
  * `CanEqual[A, A]` is required because a server error is scoped to the value it judged
  * ([[serverVisible]]) — and a `Signal[A]` demands it anyway.
  *
  * Two independent error channels feed the display:
  *   - the CLIENT error (`clientErrorRef`) — the rule chain's verdict, re-derived on
  *     every trigger and at submit;
  *   - the SERVER error (`serverErrorRef`) — an out-of-band result from `setError`,
  *     [[ServerError]]-scoped to the value it was set against. It is shown only
  *     while the value is unchanged, so the first edit hides it — which re-enables the
  *     submit button (the only way to re-check a server-side rule). Without this the
  *     button that produced the error would be the one it disables: a deadlock.
  *
  * Constructed by [[Form.field]] — never directly.
  */
final class FormField[A] private[form] (
    private[form] val valueRef: SignalRef[A],
    initial: A,
    baselineRef: SignalRef[A],
    val domId: String,
    touchedRef: SignalRef[Boolean],
    validatingRef: SignalRef[Boolean],
    clientErrorRef: SignalRef[Maybe[FieldError]],
    serverErrorRef: SignalRef[Maybe[ServerError[A]]],
    epoch: AtomicInt,
    private var rules: Validator[A],
    triggers: Set[Activation.Field],
    debounce: Duration,
    translator: ErrorTranslator,
    submitCount: Signal[Int]
)(using CanEqual[A, A]) derives CanEqual:

    /** The two-way value cell — bind it to a control, or read it at submit. */
    val value: Signal[A] = valueRef

    /** True once the field has been entered and left (first blur/commit). */
    val touched: Signal[Boolean] = touchedRef

    /** True while an async validator is in flight — drive a spinner off this. */
    val validating: Signal[Boolean] = validatingRef

    /** The server error, but only while the value still matches the one it was set
      * against. As soon as the user edits the field the value diverges and this reads
      * `Absent`, so a stale server verdict cannot outlive the value it judged.
      */
    private def serverVisible(using Frame): Signal[Maybe[FieldError]] =
        valueRef.combineLatest(serverErrorRef).map((v, se) => se.collect { case ServerError(e, v0) if v0 == v => e })

    /** The raw error state, ungated: the client rule error, else the still-current
      * server error.
      */
    def error(using Frame): Signal[Maybe[FieldError]] =
        clientErrorRef.combineLatest(serverVisible).map((c, s) => c.orElse(s))

    /** How this field's error is DISPLAYED — see [[revealWhen]]. Defaults to the touched-gate. */
    private var revealMode: Reveal = Reveal.WhenTouched

    /** Choose when this field's error is DISPLAYED (inline + in a summary), independent of when
      * it is re-validated (its [[Activation]] triggers): [[Reveal.WhenTouched]] (default — after
      * this field's first blur/change, or submit), [[Reveal.OnSubmit]] (quiet while editing,
      * flags only at submit), [[Reveal.Immediate]] (flags an invalid value before any
      * interaction), or [[Reveal.Manual]] (never auto-shown; still feeds `isValid` / blocks
      * submit). Pure + chainable, like [[addRule]] / [[focusable]].
      */
    def revealWhen(mode: Reveal): FormField[A] =
        revealMode = mode
        this

    /** The error as it should be DISPLAYED, gated by [[revealMode]]: the client rule error,
      * else the still-current server error, once the reveal condition opens. Feeds both inline
      * and the summary.
      */
    def visibleError(using Frame): Signal[Maybe[FieldError]] =
        def gated(open: Signal[Boolean]): Signal[Maybe[FieldError]] =
            open.combineLatest(error).map((o, e) => if o then e else Absent)
        revealMode match
            case Reveal.Immediate => error
            case Reveal.Manual    => Signal.initConst(Absent)
            case Reveal.OnSubmit  => gated(submitCount.map(_ > 0))
            case Reveal.WhenTouched =>
                gated(touchedRef.combineLatest(submitCount).map((t, c) => t || c > 0))
        end match
    end visibleError

    /** The gated, translated message for inline display — `Present` shows the row and
      * turns the field red. Re-renders on locale change (translator returns a Signal).
      */
    def message(using Frame): Signal[Maybe[String]] =
        visibleError.switchMap {
            case Present(e) => translator.translate(e).map(m => Present(m))
            case Absent     => Signal.initConst(Absent)
        }

    /** Set/replace the error from outside (a server-side validation result). It is
      * value-scoped: recorded against the field's current value, so the next edit hides
      * it (see [[serverVisible]]) and the submit button re-enables for a fresh round-trip.
      */
    def setError(e: Maybe[FieldError])(using Frame): Unit < Sync =
        valueRef.get.map(v => serverErrorRef.set(e.map(err => ServerError(err, v))))

    /** Append another rule (ordered short-circuit) — e.g. a cross-field rule that
      * reads a sibling handle. Returns this field for chaining.
      */
    def addRule(v: Validator[A])(using Frame): FormField[A] =
        rules = rules and v
        this

    /** Append a predicate rule in the same `satisfy(code)(pred)` shape used at the form level
      * ([[Form.satisfy]]) and to build a standalone [[Validator]] ([[Validator.satisfy]]) — so a
      * field's post-hoc rule reads exactly like a form's. Sugar for
      * `addRule(Validator.satisfy(code, args)(p))`; `true` = valid. Reach for [[addRule]] directly
      * for built-in rules (`required`/`email`) or compositions (`a and b`). Chainable.
      */
    def satisfy(code: String, args: Map[String, String] = Map.empty)(p: A => Boolean)(using Frame): FormField[A] =
        addRule(Validator.satisfy(code, args)(p))

    /** Include (default) or exclude this field as a focus target for the form's
      * focus-first-invalid-on-submit behaviour. A hidden or programmatically-managed
      * field can opt out with `focusable(false)`; it is still validated, just skipped
      * when submit picks which invalid field to focus. Returns this field for chaining.
      */
    def focusable(v: Boolean): FormField[A] =
        focusableFlag = v
        this

    /** The field's original default value — the baseline it was constructed with (also
      * where [[Form.reset]] lands a never-rebaselined field). The *live* baseline that
      * [[isDirty]] compares against can move via [[rebase]] / [[Form.markPristine]].
      */
    def initialValue: A = initial

    /** True while the current value differs from the live baseline (initially [[initialValue]],
      * moved by [[rebase]]). Reactive — gate a "discard" button or an unsaved-changes
      * indicator off it.
      */
    def isDirty(using Frame): Signal[Boolean] =
        valueRef.combineLatest(baselineRef).map((v, b) => v != b)

    /** Snapshot the current value as the new clean baseline (e.g. after a successful save),
      * so [[isDirty]] reads `false` and [[Form.reset]] returns here — without reverting the
      * value now.
      */
    def rebase(using Frame): Unit < Sync =
        valueRef.get.map(baselineRef.set)

    /** Re-validate this field whenever any of `sigs` emits (cross-field dependency).
      * Launches a scoped observer per signal; torn down with the enclosing mount.
      */
    def dependsOn(sigs: Signal[?]*)(using Frame): Unit < (Async & Scope) =
        Kyo.foreach(sigs)(s => Fiber.init(s.observe(_ => revalidate)).unit).unit

    /** This field must equal the live value of `other` (the password-confirmation pattern) —
      * one call for what is otherwise two coupled statements: it appends
      * [[Validator.matchesField]] AND wires [[dependsOn]]`(other)` so the field re-validates as
      * soon as `other` changes. Forgetting the dependency (the common footgun) is impossible.
      * For rules that read a sibling WITHOUT equality, use `addRule` + `dependsOn` directly.
      */
    def matches(other: Signal[A], code: String = "must-match")(using CanEqual[A, A], Frame): Unit < (Async & Scope) =
        discard(addRule(Validator.matchesField(other, code)))
        dependsOn(other)

    // --- internals -------------------------------------------------------------

    /** True when this field validates on focus-loss — the `bind` extensions read it to
      * decide whether to wire `onBlur`.
      */
    private[form] def wantsBlur: Boolean = triggers(Activation.Blur)

    /** True when this field validates on every value change — [[wireChange]] launches the
      * observer that drives it.
      */
    private[form] def wantsChange: Boolean = triggers(Activation.Change)

    /** Wire the [[Activation.Change]] behaviour: observe the bound value and, on every change,
      * validate. The FIRST emission is the current (mount) value — validate it SILENTLY (no
      * reveal) so [[isValid]] is correct from the start without flagging an untouched field;
      * every subsequent (real) change runs [[onEvent]], which marks the field touched so the
      * error surfaces inline as the user types (RHF `onChange` mode). A no-op unless the
      * field declared `Activation.Change`. Launched in the mount scope by `Form.mountedWith`, so
      * the observer is torn down with the form; array-row fields (built without a `Scope`) do
      * not get it — use `Activation.Blur` / `Submit` there.
      */
    private[form] def wireChange(using Frame): Unit < (Async & Scope) =
        if !wantsChange then Kyo.unit
        else
            for
                seen <- AtomicInt.init(0)
                _    <- Fiber.init(value.observe(v => seen.getAndIncrement.map(n => if n == 0 then revalidate else onEvent(v)))).unit
            yield ()

    /** Whether submit may focus this field when it is the first invalid one — toggled
      * by [[focusable]], read by `Form.submit`.
      */
    private[form] var focusableFlag: Boolean = true

    /** A trigger fired: mark touched and validate. */
    private[form] def onEvent(v: A)(using Frame): Unit < Async =
        touchedRef.set(true).andThen(runAndSet(v))

    /** Re-run the current rule chain against the latest value and publish the error,
      * guarded against stale async runs by the epoch counter.
      */
    private[form] def revalidate(using Frame): Unit < Async =
        valueRef.use(runAndSet)

    private def runAndSet(v: A)(using Frame): Unit < Async =
        for
            mine <- epoch.incrementAndGet
            _    <- validatingRef.set(true)
            _    <- if debounce > Duration.Zero then Async.sleep(debounce) else Kyo.unit
            res  <- rules.run(v)
            cur  <- epoch.get
            _ <- if cur == mine then clientErrorRef.set(res).andThen(validatingRef.set(false))
            else Kyo.unit
        yield ()

    /** Validate unconditionally for submit (ignores triggers), publish, and report. */
    private[form] def validateForSubmit(using Frame): Maybe[FieldError] < Async =
        for
            v    <- valueRef.get
            mine <- epoch.incrementAndGet
            res  <- rules.run(v)
            cur  <- epoch.get
            _    <- if cur == mine then clientErrorRef.set(res) else Kyo.unit
        yield res

    /** For summary aggregation: the gated error as a (0-or-1) chunk. */
    private[form] def visibleErrorChunk(using Frame): Signal[Chunk[FieldError]] =
        visibleError.map(e => e.map(Chunk(_)).getOrElse(Chunk.empty))

    /** The UNGATED error as a (0-or-1) chunk — the raw client/server verdict regardless of
      * touched/submit-reveal. Feeds `Form.isValid(onlyVisible = false)`.
      */
    private[form] def errorChunk(using Frame): Signal[Chunk[FieldError]] =
        error.map(e => e.map(Chunk(_)).getOrElse(Chunk.empty))

    /** The gated error as a (0-or-1) chunk of [[Form.ErrorEntry]], tagged with this field's
      * [[domId]] so a summary can jump focus to it. Feeds `Form.errorEntries`.
      */
    private[form] def visibleErrorEntries(using Frame): Signal[Chunk[Form.ErrorEntry]] =
        visibleError.map(e => e.map(err => Chunk(Form.ErrorEntry(err, Present(domId)))).getOrElse(Chunk.empty))

    /** Revert value to the live baseline (the last [[rebase]], else [[initialValue]]),
      * and clear touched, error (client + server), and validating (form reset).
      */
    private[form] def resetField(using Frame): Unit < Sync =
        baselineRef.get.map(valueRef.set)
            .andThen(touchedRef.set(false))
            .andThen(clientErrorRef.set(Absent))
            .andThen(serverErrorRef.set(Absent))
            .andThen(validatingRef.set(false))

end FormField

/** A server-set error paired with the field value it was judged against. Kept as a
  * value so the display can hide it the moment the value changes ([[FormField.serverVisible]]).
  * `derives CanEqual` is load-bearing: it rides a `Signal`, which requires `CanEqual`.
  */
final private[form] case class ServerError[A](err: FieldError, value: A) derives CanEqual

/** Postfix binding: `uic.Input()...bind(field)` returns the same concrete control
  * type (`C` is inferred from the receiver, so no type ascription is needed). The
  * extension is split by control family — a `String` field binds to a text control,
  * a `Boolean` field to a checkbox/switch — because only the value binding and the
  * blur trigger are value-typed; everything gated stays shared on [[FormField]].
  */
/** The `String`-control binding, as a named helper so a wrapping control ([[DateField]]'s
  * `DatePicker`) can reuse it without the `bind` name colliding with its own extension.
  */
private[form] def wireText[C <: TextFormControl { type Self <: C }](control: C, field: FormField[String])(using Frame): C =
    var b: C = control.value(field.valueRef).id(field.domId)
    b = b.invalidMessage(field.message)
    if field.wantsBlur then b = b.onBlur(v => field.onEvent(v))
    b
end wireText

private[form] def wireTextControlOnly[C <: TextFormControl { type Self <: C }](control: C, field: FormField[String])(using Frame): C =
    var b: C = control.value(field.valueRef).id(field.domId)
    b = b.invalid(field.visibleError.map(_.isDefined))
    if field.wantsBlur then b = b.onBlur(v => field.onEvent(v))
    b
end wireTextControlOnly

extension [C <: TextFormControl { type Self <: C }](control: C)
    def bind(field: FormField[String])(using Frame): C            = wireText(control, field)
    def bindControlOnly(field: FormField[String])(using Frame): C = wireTextControlOnly(control, field)
    // Typed date façade (`uic.DatePicker().bind(dateField)`): binds the underlying String
    // field, so it reuses the reactive value + gated message + Blur path. Co-located as an
    // overload in THIS clause — overloaded extension `bind`s must share one receiver group.
    def bind(field: DateField[?])(using Frame): C            = wireText(control, field.underlying)
    def bindControlOnly(field: DateField[?])(using Frame): C = wireTextControlOnly(control, field.underlying)
end extension

extension [C <: BooleanFormControl { type Self <: C }](control: C)
    def bind(field: FormField[Boolean])(using Frame): C =
        var b: C = control.checked(field.valueRef).id(field.domId)
        b = b.invalidMessage(field.message)
        if field.wantsBlur then b = b.onBlur(v => field.onEvent(v))
        b
    end bind

    def bindControlOnly(field: FormField[Boolean])(using Frame): C =
        var b: C = control.checked(field.valueRef).id(field.domId)
        b = b.invalid(field.visibleError.map(_.isDefined))
        if field.wantsBlur then b = b.onBlur(v => field.onEvent(v))
        b
    end bindControlOnly
end extension

/** The `Double`-control binding, as a named helper so a wrapping control ([[NumberField]]'s
  * `InputNumber`) can reuse it without the `bind` name colliding with its own extension.
  */
private[form] def wireNumber[C <: NumberFormControl { type Self <: C }](control: C, field: FormField[Double])(using Frame): C =
    var b: C = control.value(field.valueRef).id(field.domId)
    b = b.invalidMessage(field.message)
    if field.wantsBlur then b = b.onBlur(v => field.onEvent(v))
    b
end wireNumber

private[form] def wireNumberControlOnly[C <: NumberFormControl { type Self <: C }](control: C, field: FormField[Double])(using Frame): C =
    var b: C = control.value(field.valueRef).id(field.domId)
    b = b.invalid(field.visibleError.map(_.isDefined))
    if field.wantsBlur then b = b.onBlur(v => field.onEvent(v))
    b
end wireNumberControlOnly

extension [C <: NumberFormControl { type Self <: C }](control: C)
    def bind(field: FormField[Double])(using Frame): C            = wireNumber(control, field)
    def bindControlOnly(field: FormField[Double])(using Frame): C = wireNumberControlOnly(control, field)
    // Typed numeric façade (`uic.InputNumber().bind(numberField)`): binds the underlying
    // Double field and flips on the integer keystroke mask for whole-valued codecs, so a
    // fraction can't be entered. Co-located as an overload in THIS clause.
    def bind(field: NumberField[?])(using Frame): C =
        wireNumber(control.integer(field.codec.integer), field.underlying)
    def bindControlOnly(field: NumberField[?])(using Frame): C =
        wireNumberControlOnly(control.integer(field.codec.integer), field.underlying)
end extension

extension [C <: FileFormControl { type Self <: C }](control: C)
    def bind(field: FormField[Seq[UI.FilePayload]])(using Frame): C =
        var b: C = control.value(field.valueRef).id(field.domId)
        b = b.invalidMessage(field.message)
        if field.wantsBlur then b = b.onBlur(v => field.onEvent(v))
        b
    end bind

    def bindControlOnly(field: FormField[Seq[UI.FilePayload]])(using Frame): C =
        var b: C = control.value(field.valueRef).id(field.domId)
        b = b.invalid(field.visibleError.map(_.isDefined))
        if field.wantsBlur then b = b.onBlur(v => field.onEvent(v))
        b
    end bindControlOnly
end extension

extension [C <: MultiSelectFormControl { type Self <: C }](control: C)
    def bind(field: FormField[Set[String]])(using Frame): C =
        var b: C = control.value(field.valueRef).id(field.domId)
        b = b.invalidMessage(field.message)
        if field.wantsBlur then b = b.onBlur(v => field.onEvent(v))
        b
    end bind

    def bindControlOnly(field: FormField[Set[String]])(using Frame): C =
        var b: C = control.value(field.valueRef).id(field.domId)
        b = b.invalid(field.visibleError.map(_.isDefined))
        if field.wantsBlur then b = b.onBlur(v => field.onEvent(v))
        b
    end bindControlOnly
end extension
