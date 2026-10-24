package kyo.uic.form

import kyo.*
import kyo.UI.*
import kyo.uic

/** A form-validation scope. Forms nest: [[child]] creates a sub-scope, so a
  * reusable sub-form owns its own fields and its errors bubble into the parent's
  * aggregate. Any scope can host a [[Form.errorSummary]], so summaries can sit at
  * the root and at sub-forms at once. Created via [[Form.mounted]] /
  * [[Form.mountedWith]] (allocates the state inside a `UI.mounted` region).
  */
final class Form private[form] (
    private[form] val translator: ErrorTranslator,
    fieldsRef: AtomicRef[Chunk[FormField[?]]],
    childrenRef: AtomicRef[Chunk[Form]],
    formErrorsRef: SignalRef[Chunk[Form.ErrorEntry]],
    submitCountRef: SignalRef[Int],
    submittingRef: SignalRef[Boolean],
    arraysRef: AtomicRef[Chunk[FieldArray[?]]],
    focusInvalidOnSubmit: Boolean,
    private[form] val mintId: () => String < Sync,
    focusField: String => Unit < Async,
    ownsSubmit: Boolean,
    outerV: Maybe[Form] = Absent
):

    /** The scope this one was made in ([[child]], or an array's row), a root form being its own. */
    private[form] def outer: Form = outerV.getOrElse(this)

    /** Declare a typed field in this scope, starting from `initial` (which is also the reset
      * baseline). Returns a [[FieldSpec]] (a pure description with a default for every other
      * slot), which `.declare` turns into the live handle:
      * {{{
      * stock <- form.field(true).declare
      * name  <- form.field("").rules(Validator.required()).domId("product-name").declare
      * }}}
      * Bind the handle onto a matching control with `uic.CheckBox().bind(field)`.
      */
    def field[A](initial: A)(using CanEqual[A, A]): FieldSpec[A] { type Valid = A } =
        FieldSpec.init(this, initial)

    /** Zero-config `String` field, value starting empty: the common case, so text forms
      * declare `form.field.declare` and say nothing else.
      */
    def field: FieldSpec[String] { type Valid = String } = FieldSpec.init(this, "")

    /** Declare a typed numeric field (`Int` / `Long` / `Double`) bound to a `uic.InputNumber`.
      * The bound `Double` is the source of truth; rules stated on the spec validate the typed
      * value (via the given [[NumberCodec]]). Whole-valued types mask decimal entry
      * automatically at bind time. `.declare` yields a [[NumberField]] whose `value` is a
      * `Signal[A]`; bind it with `uic.InputNumber().bind(field)`.
      */
    def numberField[A](initial: A)(using codec: NumberCodec[A]): NumberFieldSpec[A] { type Valid = A } =
        NumberFieldSpec.of(FieldSpec.init(this, codec.toDouble(initial)), codec, Validator.none[A])

    /** Every field declared in this scope and its child scopes, in declaration order.
      *
      * The STATIC tree only: a field array's rows come and go after the form is built, and
      * [[FormDiagnostics]], the one caller, reads this once at mount.
      */
    private[form] def declaredFields(using Frame): Chunk[FormField[?]] < Sync =
        for
            own  <- fieldsRef.get
            kids <- childrenRef.get
            more <- Kyo.foreach(kids)(_.declaredFields)
        yield own ++ more.foldLeft(Chunk.empty[FormField[?]])(_ ++ _)

    /** Register a declared field. Declaration order IS the order `focusFirstInvalid` and the
      * error summary walk, so the append is the whole of what ordering means here.
      */
    private[form] def register(f: FormField[?])(using Frame): Unit < Sync =
        fieldsRef.updateAndGet(_ :+ f).unit

    /** Create a nested scope (composable sub-form). Shares the submit/submitting
      * state with the root, so submit-reveal opens every gate at once.
      */
    def child()(using Frame): Form < Sync =
        for
            c <- newChildScope
            _ <- childrenRef.updateAndGet(_ :+ c)
        yield c

    /** Allocate a fresh sub-scope that shares this form's submit/submitting state and
      * id/focus channels, but is NOT registered in `childrenRef`: [[child]] registers it,
      * [[FieldArray]] tracks its rows separately (in its own reactive list).
      */
    private[form] def newChildScope(using Frame): Form < Sync =
        for
            fieldsR   <- AtomicRef.init(Chunk.empty[FormField[?]])
            childrenR <- AtomicRef.init(Chunk.empty[Form])
            formErrs  <- Signal.initRef(Chunk.empty[Form.ErrorEntry])
            arraysR   <- AtomicRef.init(Chunk.empty[FieldArray[?]])
        yield new Form(
            translator,
            fieldsR,
            childrenR,
            formErrs,
            submitCountRef,
            submittingRef,
            arraysR,
            focusInvalidOnSubmit,
            mintId,
            focusField,
            ownsSubmit = false,
            outerV = Present(this)
        )

    /** Declare a dynamic array of repeated rows. Each row is its own sub-form scope built
      * by `build`, which receives the row's [[Form]] and a [[FieldArray.Row]] control (self
      * `remove` / `moveUp` / `moveDown` / `index`). `initial` seeds that many rows. Render
      * the live, keyed list with `array.render`; grow it with `array.add`. Rows participate
      * fully in validation, `errorSummary`, `isDirty`, focus-first-invalid, `reset`, and
      * `markPristine`.
      *
      * Note: a row `build` runs with `Async` only (no `Scope`). Everything that does not need
      * one works (fields, `addRule`, `bind`, `errorSummary`), and `Activation.Change` works
      * too: the array wires each row's observer on an unscoped fiber and cancels it when the
      * row is removed or the form unmounts. A rule that reads another field
      * ([[Validator.reading]], [[FormField.matches]]) works the same way: the row subscribes to
      * what its rules read on the same unscoped fibers. `dependsOn` is the exception, and the
      * type says so: it returns `Unit < (Async & Scope)`, so it cannot be called from a row at all.
      */
    def fieldArray(initial: Int = 0)(build: (Form, FieldArray.Row) => UI < Async)(using Frame): FieldArray[Unit] < Async =
        fieldArrayOf(initial)((row, ctl) => build(row, ctl).map(ui => (ui, ())))

    /** Like [[fieldArray]], but each row's builder ALSO returns a typed model `R` (typically
      * a case class of the row's field handles). The array then gives typed, live,
      * removal-correct access to every row via [[FieldArray.rows]] / [[FieldArray.currentRows]]
      * and [[FieldArray.satisfy]], the seam for cross-row validation ("all rows agree on a
      * field", "no duplicates", "sum ≤ N").
      */
    def fieldArrayOf[R](initial: Int = 0)(build: (Form, FieldArray.Row) => (UI, R) < Async)(using
        CanEqual[R, R],
        Frame
    ): FieldArray[R] < Async =
        for
            rowsRef <- Signal.initRef(Chunk.empty[FieldArray.RowEntry[R]])
            arr = new FieldArray[R](rowsRef, this, build)
            _ <- arraysRef.updateAndGet(_ :+ arr)
            _ <- Kyo.foreach(0 until initial)(_ => arr.add)
        yield arr

    /** Declare a field group: a named tuple of specs, declared in one effect and handed back as the named tuple of what
      * each spec declared to.
      * {{{
      * f <- form.fields((
      *        customer = form.field.rules(Validator.required()),
      *        lines    = form.array(initial = 1)(row => (sku = row.field, qty = row.numberField(1))),
      *        shipping = form.child(scope => (street = scope.field, city = scope.field))
      *      ))
      * }}}
      * `f.customer` is the declared `FormField[String] { type Valid = String }`, `f.lines` a [[GroupArray]] and
      * `f.shipping` the child's own named tuple of fields. A group is nothing but its fields: it has no methods, so none
      * can shadow a field. What submits it, finds its scope and lays out its rows are functions that take it
      * ([[submit]], [[Form.scopeOf]], [[GroupArray.render]]).
      *
      * Each member is an UNDECLARED spec: a field or number field spec, an [[array]], or a [[child]]. Every member is
      * declared in this scope, whichever form its spec was made from, in tuple order, which is the order
      * focus-first-invalid and the error summary walk. A name the named tuple itself answers (`toString`, `hashCode`,
      * `equals`, `getClass`, `isInstanceOf`, `asInstanceOf`, `apply`, and the symbolic `==`, `!=`, `##`) would not read
      * the field, so it does not compile; neither does a group of no fields.
      */
    def fields[N <: Tuple, S <: NonEmptyTuple, H <: Tuple](specs: NamedTuple.NamedTuple[N, S])(using
        frame: Frame,
        members: FieldGroup.Specs.Aux[N, S, H]
    ): NamedTuple.NamedTuple[N, H] < Async =
        // `H` is a `NamedTuple[N, H]` by the named tuple's lower bound, so the handles need no conversion.
        members.declare(specs.toTuple, this).map(hs => hs: NamedTuple.NamedTuple[N, H])

    /** An array member for [[fields]]: each row is a fresh scope in which `row` declares its specs, and `initial` rows are
      * added when the group is declared. It declares to a [[GroupArray]], whose rows are named tuples of fields like a
      * group, and a group submit hands it over as the `Chunk` of its rows' validated values. The spec is a pure value,
      * declared into the scope of the group that holds it.
      */
    def array[N <: Tuple, S <: NonEmptyTuple, H <: Tuple](initial: Int = 0)(row: Form => NamedTuple.NamedTuple[N, S])(using
        members: FieldGroup.Specs.Aux[N, S, H]
    ): ArraySpec[N, H] =
        ArraySpec.of(initial, row)

    /** A child group member for [[fields]]: a [[child]] scope (its own summary, anchor and reset) in which `build`
      * declares its specs. It declares to the child's named tuple of fields, read by name like any member, and the
      * child scope is [[Form.scopeOf]] of it. The spec is a pure value; the child scope is made in the scope of the group
      * that holds it.
      */
    def child[N <: Tuple, S <: NonEmptyTuple, H <: Tuple](build: Form => NamedTuple.NamedTuple[N, S])(using
        members: FieldGroup.Specs.Aux[N, S, H]
    ): ChildSpec[N, H] =
        ChildSpec.of(build)

    /** This scope's focus anchor id, set once via [[anchor]], inherited by every form-level
      * error ([[satisfy]] / [[raise]]) on this scope.
      */
    private var anchorId: Maybe[String] = Absent

    /** Set this scope's focus ANCHOR: the id of an element that an [[Form.errorSummary]] row for a
      * form-level error ([[satisfy]] / [[raise]]) jumps focus to. Set it once; every satisfy/raise
      * on this scope inherits it (like a field's id owns all its validators). Stamp the same id
      * on a wrapping element (e.g. `uic.InputGroup().id(theId)(...)`): because a wrapper is not
      * itself focusable, the jump lands on its first field. For a DIFFERENT anchor, group those
      * fields in a [[child]] sub-form and give IT its own anchor. Returns this scope for chaining.
      */
    def anchor(id: String): Form =
        anchorId = Present(id)
        this

    /** Scope-local form-level errors (not tied to a field). */
    def errors(using Frame): Signal[Chunk[FieldError]] = formErrorsRef.map(_.map(_.error))

    /** Raise a form-level error (e.g. "invalid credentials" from a submit handler). Not tied to
      * a field, so it does not gate the submit button; in the summary it jumps focus to this
      * scope's [[anchor]] (if set), else renders as a plain row.
      */
    def raise(e: FieldError)(using Frame): Unit < Sync =
        formErrorsRef.updateAndGet(_ :+ Form.ErrorEntry(e, anchorId)).unit

    /** Raise a form-level error that is its text, with no code to invent: `raise(FieldError.message(message))`. A
      * server's own sentence goes here as it is.
      */
    def raise(message: String)(using Frame): Unit < Sync =
        raise(FieldError.message(message))

    def clearErrors(using Frame): Unit < Sync = formErrorsRef.set(Chunk.empty[Form.ErrorEntry])

    /** This scope's submit-time checks, declared via [[satisfy]] with a thunk predicate. */
    private var checks: List[Form.Check] = Nil

    /** This scope's reactive invariants, declared via [[satisfy]] with a `Signal[Boolean]`. */
    private var invariants: List[Form.Invariant] = Nil

    /** Attach a cross-field / form-level validation to this scope, in the SAME
      * `satisfy(code)(predicate)` shape a field rule uses ([[Validator.satisfy]]), so what you
      * know from writing a field validation carries straight over to the form. `true` = valid
      * (same polarity as a field). The PREDICATE'S TYPE selects when it is activated (illegal
      * combinations stay unrepresentable, see [[Activation]]):
      *
      *   - a `Signal[Boolean]` (this overload) is a REACTIVE invariant: re-evaluated whenever its
      *     inputs change (build it from the fields, e.g.
      *     `Signal.combineLatestAll(fields.map(_.value)).map(...)`); no `dependsOn` needed. While
      *     `false` the form is invalid ([[isValid]] reflects it → live submit-enable) and submit
      *     is blocked.
      *   - a `=> Boolean < Async` thunk (the other overload) is a SUBMIT-pull check: evaluated once
      *     per submit (so it may suspend, e.g. a REST/mutation round-trip); on `false` it blocks submit.
      *
      * Pure + chainable (like [[anchor]]); inherits this scope's [[anchor]] for the summary
      * focus-jump. `reveal` controls only WHEN `code` is auto-shown in an [[Form.errorSummary]]
      * ([[Reveal.OnSubmit]] default / [[Reveal.Immediate]] / [[Reveal.Manual]]).
      */
    def satisfy(code: String, reveal: Reveal)(valid: Signal[Boolean]): Form =
        invariants = invariants :+ Form.Invariant(code, valid, reveal, anchorId)
        this

    /** Reactive `Signal[Boolean]` invariant with the default [[Reveal.OnSubmit]]; see the
      * `(code, reveal)` overload for the full contract.
      */
    def satisfy(code: String)(valid: Signal[Boolean]): Form =
        satisfy(code, Reveal.OnSubmit)(valid)

    /** SUBMIT-pull check: `pred` is evaluated once per submit (it may suspend). On failure it
      * blocks submit; `reveal` chooses between [[Reveal.OnSubmit]] (default: raise `code` into the
      * summary; [[Reveal.Immediate]] coincides, having nothing to show earlier) and [[Reveal.Manual]]
      * (block silently; the consumer surfaces it). See the `Signal[Boolean]` overload for the
      * reactive counterpart. Pure + chainable.
      */
    def satisfy(code: String, reveal: Reveal)(pred: => Boolean < Async): Form =
        checks = checks :+ Form.Check(code, () => pred, reveal)
        this

    /** SUBMIT-pull check with the default [[Reveal.OnSubmit]]; see the `(code, reveal)` overload. */
    def satisfy(code: String)(pred: => Boolean < Async): Form =
        satisfy(code, Reveal.OnSubmit)(pred)

    /** This scope's invariant errors as reveal-gated display entries (own scope only; the
      * subtree is covered by [[errorEntries]]' existing child/array recursion).
      */
    private def invariantDisplayEntries(using Frame): Seq[Signal[Chunk[Form.ErrorEntry]]] =
        invariants.map { inv =>
            val failing: Signal[Chunk[Form.ErrorEntry]] =
                inv.valid.map(ok => if ok then Chunk.empty[Form.ErrorEntry] else Chunk(Form.ErrorEntry(FieldError(inv.code), inv.anchorId)))
            inv.reveal match
                case Reveal.Immediate => failing
                case Reveal.Manual    => Signal.initConst(Chunk.empty[Form.ErrorEntry])
                // A scope-level invariant has nothing to "touch", so WhenTouched behaves as OnSubmit.
                case Reveal.OnSubmit | Reveal.WhenTouched =>
                    submitCountRef.combineLatest(failing).map((c, e) => if c > 0 then e else Chunk.empty[Form.ErrorEntry])
            end match
        }

    /** Whether every reactive invariant in this subtree currently holds: raw validity,
      * independent of `reveal`. Folded into [[isValid]] and checked at submit. Recurses child
      * scopes; array-row invariants are not folded (rows have no `Scope`, as with `Activation.Change`).
      */
    private[form] def invariantsHold(using Frame): Signal[Boolean] < Sync =
        for
            kids    <- childrenRef.get
            kidSigs <- Kyo.foreach(kids)(_.invariantsHold)
        yield
            val parts: Seq[Signal[Boolean]] = invariants.map(_.valid) ++ kidSigs.toSeq
            if parts.isEmpty then Signal.initConst(true)
            else Signal.combineLatestAll(parts).map(_.forall(identity))

    /** How many times submit has run since the form loaded or was last [[reset]]: drives the display gate (`> 0` opens it). */
    val submitCount: Signal[Int] = submitCountRef

    /** True while a submit is validating/running. */
    val submitting: Signal[Boolean] = submittingRef

    /** True while ANY error is currently DISPLAYED in this subtree (gated by
      * touched / submit-reveal). A pristine, untouched form reads `false`.
      */
    def hasVisibleErrors(using Frame): Signal[Boolean] < Sync =
        allErrors.map(_.map(_.nonEmpty))

    /** The usual submit-button gate: blocked while submitting OR while any FIELD error is
      * shown to the user, yet open for a pristine form (so the first click runs
      * submit-reveal, after which the shown errors keep it blocked). Read after the
      * fields/children are declared.
      *
      * It returns a [[kyo.uic.SubmitGate]] rather than a bare `Signal[Boolean]` so that
      * `uic.Button("Save").ariaDisabled(form.submitDisabled)` is the only binding that fits.
      * A native `disabled` here is an accessibility trap: the button leaves the tab order,
      * and the reactive re-enable lands a tick after the value change, so a Tab keypress
      * jumps past it. `aria-disabled` keeps the button focusable and the race disappears,
      * and routing its click through [[submit]] is what moves focus to the first invalid
      * field. Reach for `.signal` when you want the raw boolean for something else.
      *
      * Form-level errors (`raise`) deliberately do NOT gate: they can only be resolved by
      * re-submitting, so blocking submit on them would deadlock the button that produced
      * them; they surface in [[Form.errorSummary]] instead. Field server errors ([[FormField.setError]])
      * do gate, but they are value-scoped, so the first edit hides them and re-opens submit.
      */
    def submitDisabled(using Frame): uic.SubmitGate < Sync =
        fieldErrors.map(errs =>
            new uic.SubmitGate(submittingRef.combineLatest(errs.map(_.nonEmpty)).map(t => t._1 || t._2))
        )

    /** All DISPLAYABLE errors in this subtree, each paired with the id of the field it
      * belongs to (`Absent` for a form-level [[raise]] error). This is the building block a
      * custom error summary reads: it carries enough to render each message AND jump focus to
      * its field (`scope.focus(entry.fieldId)`). Gated per field by touched/submit-reveal;
      * combine with [[submitCount]] to show a summary only after submit. Read after the fields
      * are declared.
      */
    def errorEntries(using Frame): Signal[Chunk[Form.ErrorEntry]] < Sync =
        for
            fields  <- fieldsRef.get
            kids    <- childrenRef.get
            arrays  <- arraysRef.get
            kidSigs <- Kyo.foreach(kids)(_.errorEntries)
        yield
            val parts: Seq[Signal[Chunk[Form.ErrorEntry]]] =
                fields.toSeq.map(_.visibleErrorEntries) ++ Seq(formErrorsRef: Signal[Chunk[Form.ErrorEntry]]) ++
                    invariantDisplayEntries ++ kidSigs.toSeq ++ arrays.toSeq.map(_.entriesSignal)
            Signal.combineLatestAll(parts).map(chunks => chunks.foldLeft(Chunk.empty[Form.ErrorEntry])(_ ++ _))

    /** All DISPLAYABLE errors in this subtree (own gated field errors + form errors +
      * descendants), for a summary: the field-association-free view of [[errorEntries]].
      * Read after the fields are declared.
      */
    def allErrors(using Frame): Signal[Chunk[FieldError]] < Sync =
        errorEntries.map(_.map(_.map(_.error)))

    /** Move DOM focus (and scroll into view) to the field with the given id (the id an
      * [[errorEntries]] entry carries), so a custom summary's rows can jump to their field.
      */
    def focus(fieldId: String)(using Frame): Unit < Async = focusField(fieldId)

    /** FIELD errors in this subtree (field errors + descendants' + array rows'), EXCLUDING
      * form-level errors, as the touched/submit gate shows them (the [[submitDisabled]] view).
      * A subtree with no fields yields the empty chunk.
      */
    private[form] def fieldErrors(using Frame): Signal[Chunk[FieldError]] < Sync =
        for
            fields     <- fieldsRef.get
            kids       <- childrenRef.get
            arrays     <- arraysRef.get
            kidSignals <- Kyo.foreach(kids)(_.fieldErrors)
        yield
            val parts = fields.toSeq.map(_.visibleErrorChunk) ++ kidSignals.toSeq ++ arrays.toSeq.map(_.fieldErrorsSignal)
            if parts.isEmpty then Signal.initConst(Chunk.empty[FieldError])
            else Signal.combineLatestAll(parts).map(chunks => chunks.foldLeft(Chunk.empty[FieldError])(_ ++ _))

    /** True while any field in this subtree (child scopes and array rows included) is not valid, whatever is shown: it
      * holds an error, or a rule has not judged its current value ([[Rule.on]]). The [[isValid]]`(onlyVisible = false)`
      * view.
      */
    private[form] def invalidAgg(using Frame): Signal[Boolean] < Sync =
        for
            fields     <- fieldsRef.get
            kids       <- childrenRef.get
            arrays     <- arraysRef.get
            kidSignals <- Kyo.foreach(kids)(_.invalidAgg)
        yield
            val parts: Seq[Signal[Boolean]] = fields.toSeq.map(_.invalid) ++ kidSignals.toSeq ++ arrays.toSeq.map(_.invalidSignal)
            if parts.isEmpty then Signal.initConst(false)
            else Signal.combineLatestAll(parts).map(_.exists(identity))

    /** Whether every FIELD in this subtree currently holds valid data, as a reactive signal.
      *
      * `onlyVisible = false` (default) is UNGATED: it reflects the raw client/server field
      * verdict even for untouched fields, so a submit button can be enabled live the moment
      * the data becomes valid, before any field is touched. `onlyVisible = true` is the gated
      * view: valid unless an error is currently DISPLAYED (touched or after submit-reveal).
      *
      * A field with a rule that has not judged its current value yet (one waiting for the blur
      * while the reader types, see [[Rule.on]]) shows no error and is NOT valid in the ungated
      * view: nothing is known about the value until that rule has run. The gated view ignores
      * it, as [[submitDisabled]] does, because there is nothing displayed to count.
      *
      * Form-level [[raise]] errors are excluded (they can't be fixed client-side and would
      * deadlock a button gated on this; see [[errors]] / [[hasVisibleErrors]]). Reactive
      * [[satisfy]] invariants ARE included (raw validity, independent of their `reveal`): they are
      * client-fixable, so a failing invariant makes the form invalid in BOTH modes. Read after
      * the fields are declared.
      */
    def isValid(onlyVisible: Boolean = false)(using Frame): Signal[Boolean] < Sync =
        for
            invalid <- if onlyVisible then fieldErrors.map(_.map(_.nonEmpty)) else invalidAgg
            invHold <- invariantsHold
        yield invalid.combineLatest(invHold).map((bad, hold) => !bad && hold)

    /** Validate this subtree and, only if everything is valid, run `onValid` (which
      * may itself write server-side errors via `field.setError` / `raise`). Opens the
      * display gate for ALL fields first (submit-reveal), then validates every field
      * including untouched ones, in parallel. A failing `onValid` goes to the
      * `onFailure` rules around the handler, and `submitting` still ends.
      *
      * The overloads below hand `onValid` the validated values of the fields they name.
      */
    def submit(onValid: => UI.Action)(using Frame): UI.Action =
        submitWith(Chunk.empty)((Chunk.empty, Present(() => onValid)))

    /** [[submit]], handing `onValid` what `field`'s rules yielded: its [[Validated.Valid]], so a field declared with
      * `Validator.chosen()` over a `Maybe[LocalDate]` hands over the `LocalDate`, and a field whose rules only check hands
      * over its value. The value is the one the submit validated, from the same single run of the rules that decided
      * the form was valid, so an edit that lands while an async rule is in flight cannot reach the handler.
      *
      * Every other behaviour is [[submit]]'s: the whole subtree is validated, focus goes to the first invalid field, and
      * `onValid` runs only when everything holds. `field` may belong to this form or to any other; a
      * field of another form gets its own display gate opened by the submit, so the error that blocked it shows, while the
      * unnamed fields of its form stay as they were.
      */
    def submit(field: Validated)(onValid: field.Valid => UI.Action)(using Frame): UI.Action =
        submitWith(Chunk(field.submitField)) {
            field.verdict.map(r => (Chunk(r.failure), r.value.map(v => () => onValid(v))))
        }

    /** [[submit]] handing over the validated values of two fields, in the order named. */
    def submit(a: Validated, b: Validated)(onValid: (a.Valid, b.Valid) => UI.Action)(using Frame): UI.Action =
        submitWith(Chunk(a.submitField, b.submitField)) {
            Async.zip(a.verdict, b.verdict).map { (ra, rb) =>
                val run =
                    for
                        x <- ra.value
                        y <- rb.value
                    yield () => onValid(x, y)
                (Chunk(ra.failure, rb.failure), run)
            }
        }

    /** [[submit]] handing over the validated values of three fields, in the order named. */
    def submit(a: Validated, b: Validated, c: Validated)(onValid: (a.Valid, b.Valid, c.Valid) => UI.Action)(using
        Frame
    ): UI.Action =
        submitWith(Chunk(a.submitField, b.submitField, c.submitField)) {
            Async.zip(a.verdict, b.verdict, c.verdict).map { (ra, rb, rc) =>
                val run =
                    for
                        x <- ra.value
                        y <- rb.value
                        z <- rc.value
                    yield () => onValid(x, y, z)
                (Chunk(ra.failure, rb.failure, rc.failure), run)
            }
        }

    /** [[submit]] handing over the validated values of four fields, in the order named. */
    def submit(a: Validated, b: Validated, c: Validated, d: Validated)(
        onValid: (a.Valid, b.Valid, c.Valid, d.Valid) => UI.Action
    )(using Frame): UI.Action =
        submitWith(Chunk(a.submitField, b.submitField, c.submitField, d.submitField)) {
            Async.zip(a.verdict, b.verdict, c.verdict, d.verdict).map { (ra, rb, rc, rd) =>
                val run =
                    for
                        w <- ra.value
                        x <- rb.value
                        y <- rc.value
                        z <- rd.value
                    yield () => onValid(w, x, y, z)
                (Chunk(ra.failure, rb.failure, rc.failure, rd.failure), run)
            }
        }

    /** [[submit]] handing over the validated values of a whole field group ([[fields]]) as one named tuple with the
      * group's names: each field's [[Validated.Valid]], each [[GroupArray]] as the `Chunk` of its rows' named tuples,
      * each child as its own named tuple.
      * {{{
      * uic.Button("Order").onClick(form.submit(f)(v => order(v.customer, v.due, v.lines, v.shipping)))
      * }}}
      * An array hands over the rows it holds when the submit starts; a row added while an async rule is in flight is the
      * next submit's. Every field of the group is validated once, by the run that both decides validity and yields its
      * value, and the rest is [[submit]]'s: the whole subtree of this scope is validated, focus goes to the first invalid
      * field, and `onValid` runs only when everything holds. As with a single field, the group may belong to this scope
      * or to another; its fields' display gates are opened either way.
      */
    def submit[N <: Tuple, H <: Tuple, V <: Tuple](group: NamedTuple.NamedTuple[N, H])(using
        shape: FieldGroup.Shape.Aux[H, V]
    )(onValid: NamedTuple.NamedTuple[N, V] => UI.Action)(using Frame): UI.Action =
        shape.snapshot(group.toTuple).map { snap =>
            submitWith(snap.fields) {
                // `V` is a `NamedTuple[N, V]` by the named tuple's lower bound, so the value needs no conversion.
                snap.verdict().map((errs, value) => (errs, value.map(v => () => onValid(v))))
            }
        }

    /** The one submit every overload runs. `named` are the fields whose verdicts `runNamed` produces, in its order, with
      * the handler to run if all of them passed; the subtree walk skips exactly those fields, so each field's rules run
      * once per submit, and their verdicts are put back in declaration order for focus-first-invalid. The named run
      * and the walk go in parallel, as the subtree's own fields always have.
      */
    private[form] def submitWith(named: Chunk[FormField[?]])(
        runNamed: => (Chunk[Maybe[FieldError]], Maybe[() => UI.Action]) < Async
    )(using Frame): UI.Action =
        def validate: ((Chunk[Maybe[FieldError]], Maybe[() => UI.Action]), Chunk[(FormField[?], Maybe[FieldError])]) < Async =
            if named.isEmpty then runNamed.map(n => validateSubtree(named).map(rest => (n, rest)))
            else Async.zip(runNamed, validateSubtree(named))
        for
            _                           <- submitCountRef.updateAndGet(_ + 1)
            _                           <- submittingRef.set(true)
            _                           <- Kyo.foreach(named)(_.openGate) // a named field of another form shows why too
            _                           <- clearErrorsSubtree
            ((namedErrs, handler), own) <- validate
            namedResults = named.zip(namedErrs)
            // The named verdicts in the walk's declaration order; a named field from another form follows the walk.
            results = own.map((f, e) => namedResults.find(_._1 eq f).getOrElse((f, e))) ++
                namedResults.filterNot((f, _) => own.exists(_._1 eq f))
            checkFailures <- runChecks
            invHoldSig    <- invariantsHold
            invHold       <- invHoldSig.current // a failing invariant blocks onValid
            ok = results.forall(_._2.isEmpty) && checkFailures.isEmpty && invHold
            // Every named field passed when `ok` holds, which is exactly when `handler` is present.
            run = if ok then handler else Absent
            // `submitting` stays true THROUGH onValid so it also covers the server
            // round-trip (where the handler may write back per-field / form errors).
            // On failure, move focus to the first invalid field (focus-first-invalid).
            r <- run match
                case Present(k) => kyo.uic.Callback(k())
                case Absent     => focusFirstInvalid(results).andThen(Kyo.unit)
            _ <- submittingRef.set(false)
        yield r
        end for
    end submitWith

    /** Revert values to their live baselines (last [[markPristine]], else each field's
      * initial), and clear errors + touched state across the subtree. Reset on the form
      * itself also closes the display gate a submit opened, so the form is as it loaded:
      * a field a successful submit empties does not show its `required` error at once.
      * Reset on a sub-scope leaves the gate alone, since the rest of the form still
      * shows what the last submit found.
      */
    def reset(using Frame): Unit < Sync =
        for
            _      <- if ownsSubmit then submitCountRef.set(0) else Kyo.unit
            fields <- fieldsRef.get
            _      <- Kyo.foreach(fields)(_.resetField)
            _      <- formErrorsRef.set(Chunk.empty)
            kids   <- childrenRef.get
            _      <- Kyo.foreach(kids)(_.reset)
            arrays <- arraysRef.get
            _      <- Kyo.foreach(arrays)(_.resetRows)
        yield ()

    /** True while ANY field in this subtree differs from its live baseline: the form has
      * unsaved changes. A pristine (or freshly [[markPristine]]d) form reads `false`. Gate
      * a "discard" button or an unsaved-changes indicator off it; read after the fields are
      * declared.
      */
    def isDirty(using Frame): Signal[Boolean] < Sync =
        for
            fields  <- fieldsRef.get
            kids    <- childrenRef.get
            arrays  <- arraysRef.get
            kidSigs <- Kyo.foreach(kids)(_.isDirty)
        yield
            val parts: Seq[Signal[Boolean]] = fields.toSeq.map(_.isDirty) ++ kidSigs.toSeq ++ arrays.toSeq.map(_.dirtySignal)
            if parts.isEmpty then Signal.initConst(false)
            else Signal.combineLatestAll(parts).map(_.exists(identity))

    /** Snapshot the whole subtree's current values as the new clean baseline (e.g. after a
      * successful save), so [[isDirty]] reads `false` and [[reset]] returns here, without
      * reverting any values now.
      */
    def markPristine(using Frame): Unit < Sync =
        for
            fields <- fieldsRef.get
            _      <- Kyo.foreach(fields)(_.rebase)
            kids   <- childrenRef.get
            _      <- Kyo.foreach(kids)(_.markPristine)
            arrays <- arraysRef.get
            _      <- Kyo.foreach(arrays)(_.markPristineRows)
        yield ()

    // --- internals -------------------------------------------------------------

    /** Launch the value observer of every field in this static subtree
      * (own fields + nested children). Called once by `Form.mountedWith` after `build`, in
      * the mount scope, so the observers live for the form's lifetime.
      *
      * Field-array ROWS are not reached from here: they are added later, from an event
      * handler with no ambient `Scope`. They wire the same trigger through
      * [[wireRowChangeActivations]] and the array owns the resulting fibers; the
      * `Scope.ensure` below is what tears those down when the form unmounts, so a row
      * observer never outlives the form that declared it.
      */
    private[form] def wireChangeActivations(using Frame): Unit < (Async & Scope) =
        for
            fields <- fieldsRef.get
            _      <- Kyo.foreach(fields)(_.wireChange)
            kids   <- childrenRef.get
            _      <- Kyo.foreach(kids)(_.wireChangeActivations)
            _      <- Scope.ensure(cancelRowChangeActivations)
        yield ()

    /** Row-scope counterpart of [[wireChangeActivations]]: wires this scope's
      * fields (and those of its children and nested arrays' rows)
      * WITHOUT an ambient `Scope`, retaining the observer fibers so [[cancelRowChangeActivations]]
      * can tear exactly this subtree down. Called by `FieldArray.add` for each new row.
      */
    private[form] def wireRowChangeActivations(using Frame): Unit < Async =
        for
            fields <- fieldsRef.get
            wired  <- Kyo.foreach(fields)(_.wireChangeUnscoped)
            _      <- Sync.defer { rowChangeFibers = rowChangeFibers ++ wired }
            kids   <- childrenRef.get
            _      <- Kyo.foreach(kids)(_.wireRowChangeActivations)
            arrays <- arraysRef.get
            _      <- Kyo.foreach(arrays)(_.wireRowActivations)
        yield ()

    /** Cancel every observer fiber [[wireRowChangeActivations]] started in this subtree, and
      * end its fields' subscriptions to what their rules read ([[FormField.unwire]]): on row
      * removal, on form reset of the array, and on unmount.
      */
    private[form] def cancelRowChangeActivations(using Frame): Unit < Async =
        for
            fibers <- Sync.defer { val fs = rowChangeFibers; rowChangeFibers = Chunk.empty; fs }
            _      <- Kyo.foreach(fibers)(_.interrupt)
            fields <- fieldsRef.get
            _      <- Kyo.foreach(fields)(_.unwire)
            kids   <- childrenRef.get
            _      <- Kyo.foreach(kids)(_.cancelRowChangeActivations)
            arrays <- arraysRef.get
            _      <- Kyo.foreach(arrays)(_.cancelRowActivations)
        yield ()

    /** Observer fibers of this scope's row-wired fields. A plain `var`
      * for the same reason as `checks` / `invariants`: a row's wiring and teardown each run
      * single-threaded, once.
      */
    private var rowChangeFibers: Chunk[Fiber[Unit, Any]] = Chunk.empty

    private[form] def clearErrorsSubtree(using Frame): Unit < Sync =
        for
            _      <- formErrorsRef.set(Chunk.empty)
            kids   <- childrenRef.get
            _      <- Kyo.foreach(kids)(_.clearErrorsSubtree)
            arrays <- arraysRef.get
            _      <- Kyo.foreach(arrays)(_.clearErrorRows)
        yield ()

    /** Validate every field in this subtree in declaration order (own fields, then nested
      * children, then array rows), pairing each field with its result, in the order
      * [[focusFirstInvalid]] walks to pick the field to focus.
      */
    private[form] def validateSubtree(skip: Chunk[FormField[?]])(using Frame): Chunk[(FormField[?], Maybe[FieldError])] < Async =
        for
            fields <- fieldsRef.get
            kids   <- childrenRef.get
            arrays <- arraysRef.get
            own    <- Async.foreach(fields) { f =>
                // A skipped field is validated by the submit that named it; its placeholder verdict is replaced there.
                val res: Maybe[FieldError] < Async = if skip.exists(_ eq f) then Absent else f.validateForSubmit
                res
            }
            kidResults   <- Kyo.foreach(kids)(_.validateSubtree(skip))
            arrayResults <- Kyo.foreach(arrays)(_.validateRows(skip))
        yield
            val empty = Chunk.empty[(FormField[?], Maybe[FieldError])]
            fields.zip(own) ++ kidResults.foldLeft(empty)(_ ++ _) ++ arrayResults.foldLeft(empty)(_ ++ _)

    /** Move DOM focus (and scroll into view) to the first invalid, focusable field in
      * declaration order: the accessibility win of focus-first-invalid-on-submit. A
      * no-op when the behaviour is disabled ([[Form.mountedWith]]'s `focusInvalidOnSubmit`)
      * or when no field is invalid (e.g. only a cross-field `check` failed).
      */
    private def focusFirstInvalid(results: Chunk[(FormField[?], Maybe[FieldError])])(using Frame): Unit < Async =
        if !focusInvalidOnSubmit then Kyo.unit
        else
            results.collect { case (f, e) if e.isDefined && f.focusableFlag => f.domId }.headMaybe match
                case Present(id) => focusField(id)
                case Absent      => Kyo.unit

    private[form] def runChecks(using Frame): Chunk[FieldError] < Async =
        for
            ownResults <-
                Async.foreach(checks)(c => c.pred().map(ok => (c, if ok then (Absent: Maybe[FieldError]) else Present(FieldError(c.code)))))
            // A failing check always blocks submit; it is only RAISED into the summary unless it
            // opted out of display with `Reveal.Manual`.
            _ <- Kyo.foreach(ownResults) {
                case (c, Present(e)) if c.reveal != Reveal.Manual => raise(e)
                case _                                            => Kyo.unit
            }
            kids          <- childrenRef.get
            arrays        <- arraysRef.get
            kidFailures   <- Kyo.foreach(kids)(_.runChecks)
            arrayFailures <- Kyo.foreach(arrays)(_.runCheckRows)   // each row's own checks
            crossFailures <- Kyo.foreach(arrays)(_.runCrossChecks) // array-level cross-row checks
        yield
            val empty = Chunk.empty[FieldError]
            ownResults.collect { case (_, Present(e)) => e } ++
                kidFailures.foldLeft(empty)(_ ++ _) ++ arrayFailures.foldLeft(empty)(_ ++ _) ++ crossFailures.foldLeft(empty)(_ ++ _)
end Form

object Form:

    final private[form] case class Check(code: String, pred: () => Boolean < Async, reveal: Reveal)

    /** The scope a field group's fields live in: the form for a group declared by [[Form.fields]], the child scope for a
      * [[Form.child]] member, a row's own scope for a row of a [[GroupArray]].
      * {{{
      * Form.errorSummary(Form.scopeOf(f.shipping))
      * }}}
      * A function rather than a member of the group, because a group's only members are its fields: a field named
      * `scope` reads the field.
      */
    def scopeOf[N <: Tuple, X, T <: Tuple](group: NamedTuple.NamedTuple[N, X *: T])(using head: FieldGroup.Member[X]): Form =
        head.owner(group.toTuple.head)

    /** A reactive cross-field invariant: a live validity `Signal`, its display `reveal` mode,
      * and the scope's anchor id (captured at registration) for its summary focus-jump.
      */
    final private[form] case class Invariant(code: String, valid: Signal[Boolean], reveal: Reveal, anchorId: Maybe[String])

    /** Root form; resolves the [[ErrorTranslator]] from `Env` at build time (mirrors
      * `ToastService`). Install a translator once at the app root. `focusInvalidOnSubmit`
      * defaults on (opt-out): a failed submit focuses the first invalid field. The form is a
      * mount, see [[mountedWith]].
      */
    def mounted(focusInvalidOnSubmit: Boolean = true)(build: Form => UI < (Async & Scope))(using
        Frame
    ): UI.Ast.Mounted < Env[ErrorTranslator] =
        Env.get[ErrorTranslator].map(tr => mountedWith(tr, focusInvalidOnSubmit)(build))

    /** Root form with an explicit translator; no `Env` wiring needed. Use
      * `ErrorTranslator.default` for English fallbacks / no i18n. `focusInvalidOnSubmit`
      * defaults on (opt-out): a failed submit moves focus (and scroll) to the first invalid
      * field in declaration order; pass `false` to disable, or exclude a single field with
      * [[FieldSpec.focusable]]`(false)`.
      *
      * The form is a mount, and its state lives as long as the mount's instance, so everything
      * a `UI.mounted` takes applies to it directly: `.keyed(issueId)` gives each issue a form of
      * its own, and `.placeholder` and `.onError` show while it loads and when it fails.
      */
    def mountedWith(translator: ErrorTranslator, focusInvalidOnSubmit: Boolean = true)(build: Form => UI < (Async & Scope))(using
        Frame
    ): UI.Ast.Mounted =
        UI.mounted {
            for
                // The session's imperative channel, captured once so the form's field-id
                // minting and focus commands need no `UI.commands` lookup downstream (mirrors
                // Overlay/InputOtp's self-addressing pattern).
                cmds <- UI.commands
                form <- create(translator, focusInvalidOnSubmit, cmds)
                ui   <- build(form)
                _    <- form.wireChangeActivations // start the change observers of the declared tree
                // What the form can be certain is wrong about its own ids. Empty for every
                // correct program, so this adds a node only where something is already broken.
                cards <- FormDiagnostics.cards(form, ui)
            yield if cards.isEmpty then ui else fragment((cards :+ ui)*)
        }

    private def create(translator: ErrorTranslator, focusInvalidOnSubmit: Boolean, cmds: UI.Commands)(using Frame): Form < Sync =
        for
            fieldsR     <- AtomicRef.init(Chunk.empty[FormField[?]])
            childrenR   <- AtomicRef.init(Chunk.empty[Form])
            formErrs    <- Signal.initRef(Chunk.empty[Form.ErrorEntry])
            arraysR     <- AtomicRef.init(Chunk.empty[FieldArray[?]])
            countR      <- Signal.initRef(0)
            submittingR <- Signal.initRef(false)
        yield new Form(
            translator,
            fieldsR,
            childrenR,
            formErrs,
            countR,
            submittingR,
            arraysR,
            focusInvalidOnSubmit,
            () => cmds.freshId,
            id => cmds.scrollIntoViewId(id).andThen(cmds.focusId(id)),
            ownsSubmit = true
        )

    /** A displayable error paired with the id of the field it belongs to (`Absent` for a
      * form-level ([[Form.raise]]) error, which has no field to focus). Produced by
      * [[Form.errorEntries]]; consumed by [[errorSummary]] and any custom summary.
      */
    final case class ErrorEntry(error: FieldError, fieldId: Maybe[String]) derives CanEqual

    /** An OPTIONAL, opinionated error-summary view for a scope, NOT shipped with every form:
      * a summary is a use-case-specific View, so it lives here as a helper over the data a
      * [[Form]] exposes ([[Form.errorEntries]] + [[Form.submitCount]] + [[Form.focus]]), not
      * as a method on the data-holding form. Drop ONE near a submit button. It shows only
      * after submit (`submitCount > 0`), and each field error is a button that jumps focus to
      * its field; form-level errors render as plain rows.
      *
      * The jump rows say `type="button"` because the summary is meant to sit inside real
      * markup, and a `<button>` without a type is a SUBMIT button: put this near the submit
      * control of a native `<form>` and the browser would submit on a click meant to move
      * focus, and (being the first submit button in the form) would answer Enter in a text
      * field as well. A [[Form]] is a validation scope and renders no element of its own, so
      * whether a `<form>` is there at all is the caller's choice, which is precisely why this
      * cannot be left to the default.
      *
      * For anything more bespoke (custom layout, grouping, inline placement), build your own
      * from `scope.errorEntries`; this is just a sensible default.
      */
    def errorSummary(scope: Form)(using Frame): UI < Sync =
        scope.errorEntries.map { entriesSig =>
            scope.submitCount.combineLatest(entriesSig).render { (count, entries) =>
                if count == 0 || entries.isEmpty then fragment()
                else
                    div.cssClass("p-uic-form-errors").role("alert")(
                        entries.map { entry =>
                            val msg = scope.translator.translate(entry.error)
                            entry.fieldId match
                                case Present(id) =>
                                    (button.cssClass(
                                        "p-uic-invalid-message"
                                    ).cssClass("p-uic-error-link").jsProp("type", "button").onClick(scope.focus(id))(msg): UI)
                                case Absent =>
                                    (div.cssClass("p-uic-invalid-message")(msg): UI)
                            end match
                        }*
                    )
            }
        }
end Form
