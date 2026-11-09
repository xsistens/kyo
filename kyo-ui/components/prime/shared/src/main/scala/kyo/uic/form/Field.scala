package kyo.uic.form

import kyo.*
import kyo.uic.ChoiceFormControl
import kyo.uic.FileFormControl
import kyo.uic.FormControl
import kyo.uic.MultiChoiceFormControl
import kyo.uic.NumberFormControl
import kyo.uic.OptionalFormControl
import kyo.uic.TextFormControl
import scala.annotation.targetName

/** A form handle that yields a validated value at submit: a [[FormField]] or a [[NumberField]]. What
  * [[Form.submit]] takes, so one handler can receive the validated values of fields of either kind.
  *
  * `Valid` is the type the handle's rule chain yields when it passes: the field's value type when its
  * rules only check, the refined type when they refine (`Validator.chosen()` turns a `Maybe[A]` field into
  * one that validates to `A`). It is a type member rather than a type parameter so that every signature
  * naming `FormField[A]` keeps meaning what it meant; a declared field is a `FormField[A] { type Valid = B }`,
  * which is a `FormField[A]` wherever one is asked for.
  *
  * Its members are package-private, so it cannot be implemented outside this package.
  */
trait Validated:

    /** The type the rule chain yields once it passes. */
    type Valid

    /** The registered field the verdict belongs to: the one submit focuses, and the one it must not validate twice. */
    private[form] def submitField: FormField[?]

    /** The scope the field was declared in, which is the scope a field group it heads belongs to ([[Form.scopeOf]]). */
    private[form] def owner: Form

    /** Validate for submit (publishing the error as [[FormField.validateForSubmit]] does) and hand back what the rules
      * yielded.
      */
    private[form] def verdict(using Frame): Result.Partial[FieldError, Valid] < Async
end Validated

/** A single validated field of value type `A`. Owns its value/touched/validating/error
  * signals and a mutable rule chain; the owning [[Form]] holds a reference and validates
  * it on submit. Display is gated: the error surfaces only once the field is `touched`
  * (entered and left) OR the form has been submitted (`submitCount > 0`), or a submit of
  * any form named this field (`Form.submit(field)`).
  *
  * `A` is the field's value type: `String` for text controls, `Boolean` for
  * checkbox/switch/radio, and so on. The gating, message, and aggregation logic is
  * value-agnostic (it only ever handles `Maybe[FieldError]`); only the value binding is
  * type-specific, which is why `bind` splits by control family below.
  * `CanEqual[A, A]` is required because a server error is scoped to the value it judged
  * ([[serverVisible]]), and a `Signal[A]` demands it anyway. `Valid` (see [[Validated]]) is
  * what the rules yield, and so what [[Form.submit]] hands its handler for this field.
  *
  * Two independent error channels feed the display:
  *   - the CLIENT verdict (`clientRef`): what the rule chain said, re-derived on every
  *     event and at submit. It is either an error or none ([[ClientVerdict.Checked]]), or
  *     [[ClientVerdict.NotChecked]] while a rule the event was not due for has nothing to say
  *     about the current value: no error shows, and the field does not count as valid;
  *   - the SERVER error (`serverErrorRef`): an out-of-band result from `setError`,
  *     [[ServerError]]-scoped to the value it was set against. It is shown only
  *     while the value is unchanged, so the first edit hides it, which re-enables the
  *     submit button (the only way to re-check a server-side rule). Without this the
  *     button that produced the error would be the one it disables: a deadlock.
  *
  * Constructed by [[FieldSpec.declare]], never directly. Everything decided AT declaration
  * (the rules it starts with, its events, its id, its reveal mode, whether submit may focus
  * it) is a parameter of that spec and arrives here already settled; what stays on this class
  * is what a caller can only do AFTERWARDS: [[addRule]] and [[matches]], which need a second
  * field that does not exist yet when the first is declared.
  */
sealed abstract class FormField[A] private[form] (
    private[form] val valueRef: SignalRef[A],
    initial: A,
    baselineRef: SignalRef[A],
    val domId: String,
    touchedRef: SignalRef[Boolean],
    validatingRef: SignalRef[Boolean],
    clientRef: SignalRef[ClientVerdict],
    serverErrorRef: SignalRef[Maybe[ServerError[A]]],
    epoch: AtomicInt,
    dueState: AtomicLong,
    resetWrite: AtomicRef[Maybe[A]],
    store: Rule.Store,
    fieldOn: Maybe[Set[Activation.Field]],
    debounce: Duration,
    revealMode: Reveal,
    private[form] val focusableFlag: Boolean,
    translator: ErrorTranslator,
    submitCount: Signal[Int],
    namedRef: SignalRef[Boolean],
    private[form] val owner: Form
)(using CanEqual[A, A]) extends Validated derives CanEqual:

    /** The rule chain, typed by what it yields. Held in a [[RuleChain]] because [[addRule]] appends to it. */
    private[form] def chain: RuleChain[A, Valid]

    /** The two-way value cell; bind it to a control, or read it at submit. */
    val value: Signal[A] = valueRef

    /** True once the field has been entered and left (first blur/commit). */
    def touched: Signal[Boolean] = touchedRef

    /** True while an async validator is in flight; drive a spinner off this. */
    def validating: Signal[Boolean] = validatingRef

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
        clientRef.combineLatest(serverVisible).map((c, s) => c.error.orElse(s))

    /** True while a rule of the chain has not judged the current value: one that was not due for the last event, and
      * whose last verdict was about another value ([[Rule.on]]). It shows no error and still makes the ungated
      * [[Form.isValid]] false.
      */
    private[form] def notChecked(using Frame): Signal[Boolean] = clientRef.map(_ == ClientVerdict.NotChecked)

    /** The error as it should be DISPLAYED, gated by [[revealMode]]: the client rule error,
      * else the still-current server error, once the reveal condition opens. Feeds both inline
      * and the summary.
      */
    def visibleError(using Frame): Signal[Maybe[FieldError]] =
        def gated(open: Signal[Boolean]): Signal[Maybe[FieldError]] =
            open.combineLatest(error).map((o, e) => if o then e else Absent)
        revealMode match
            case Reveal.Immediate   => error
            case Reveal.Manual      => Signal.initConst(Absent)
            case Reveal.OnSubmit    => gated(submitted)
            case Reveal.WhenTouched =>
                gated(touchedRef.combineLatest(submitted).map((t, s) => t || s))
        end match
    end visibleError

    /** Whether a submit has opened this field's gate: its own form's (`submitCount > 0`), or any form's submit that
      * named it ([[openGate]]). The second is what lets a field of another form, named in `submit(field)`, show the error
      * that blocked that submit, without revealing the unnamed fields of its own form.
      */
    private def submitted(using Frame): Signal[Boolean] =
        submitCount.combineLatest(namedRef).map((c, n) => c > 0 || n)

    /** A submit named this field: open its gate as its own form's submit would, whichever form ran it. Not `touched`,
      * which says the reader was here, and which an [[Reveal.OnSubmit]] field does not read.
      */
    private[form] def openGate(using Frame): Unit < Sync = namedRef.set(true)

    /** The gated, translated message for inline display: `Present` shows the row and
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

    /** Set the error from outside as text, with no code to invent: `setError(Present(FieldError.message(text)))`.
      * Value-scoped exactly like the [[FieldError]] form.
      */
    def setError(message: String)(using Frame): Unit < Sync =
        setError(Present(FieldError.message(message)))

    /** This field typed with its rules' result, which is what a chained call hands back: `this.type` cannot be lifted
      * into a computation, and the refinement is what a chain must not lose.
      */
    private def self: FormField[A] { type Valid = FormField.this.Valid } = this

    /** Append another rule (ordered short-circuit), e.g. a cross-field rule that
      * reads a sibling handle. Hands back this field for chaining.
      *
      * The rule runs on the field's value, after the declared chain has passed, and what it
      * yields is not kept: the field still validates to the type its declared rules yield
      * ([[Valid]]), so a later check can never change what submit hands over.
      *
      * It may be appended at any time, during the build or from a handler after the mount. A
      * rule that reads another field ([[Validator.reading]]) is followed from then on: once the
      * field is wired, appending it subscribes the field to what it reads, for as long as the
      * mount (or the row) lives. Appending does not run it; the field's next event does, or a
      * change of what it reads.
      */
    def addRule(v: Rule[A, Any])(using Frame): (FormField[A] { type Valid = FormField.this.Valid }) < Sync =
        chain.append(v).map(observe).map(_ => self)

    /** Append a predicate rule in the same `satisfy(code)(pred)` shape used at the form level
      * ([[Form.satisfy]]) and to build a standalone [[Validator]] ([[Validator.satisfy]]), so a
      * field's post-hoc rule reads exactly like a form's. Sugar for
      * `addRule(Validator.satisfy(code, args)(p))`; `true` = valid. Reach for [[addRule]] directly
      * for built-in rules (`required`/`email`) or compositions (`a and b`). Chainable.
      */
    def satisfy(code: String, args: Map[String, String] = Map.empty)(p: A => Boolean)(using
        Frame
    ): (FormField[A] { type Valid = FormField.this.Valid }) < Sync =
        addRule(Validator.satisfy(code, args)(p))

    /** The field's original default value: the baseline it was constructed with (also
      * where [[Form.reset]] lands a never-rebaselined field). The *live* baseline that
      * [[isDirty]] compares against can move via [[rebase]] / [[Form.markPristine]].
      */
    def initialValue: A = initial

    /** True while the current value differs from the live baseline (initially [[initialValue]],
      * moved by [[rebase]]). Reactive: gate a "discard" button or an unsaved-changes
      * indicator off it.
      */
    def isDirty(using Frame): Signal[Boolean] =
        valueRef.combineLatest(baselineRef).map((v, b) => v != b)

    /** Snapshot the current value as the new clean baseline (e.g. after a successful save),
      * so [[isDirty]] reads `false` and [[Form.reset]] returns here, without reverting the
      * value now.
      */
    def rebase(using Frame): Unit < Sync =
        valueRef.get.map(baselineRef.set)

    /** Re-validate this field whenever any of `sigs` changes (cross-field dependency).
      * Launches a scoped observer per signal; torn down with the enclosing mount.
      *
      * For a rule that reads another field, [[Validator.reading]] is the preferred shape: the rule carries what it reads,
      * reruns or drops only itself on a change ([[Dep]]), and needs no call here. This is the general fallback, for a rule
      * that reads a signal some other way.
      *
      * A change of a dependency runs EVERY rule of the chain, whatever its [[Rule.on]], as a submit
      * does: this field's own value has not moved, so a rule not due for the event would keep the
      * verdict it found for that value, which was about the dependency's OLD value. The fresh
      * verdicts are remembered, so the events after it reuse them while the value stays. The run
      * does not touch the field: whether its error shows is still up to its own events and the
      * submit. The dependency's value at this call is not a change and runs as the mount does; it is
      * read here rather than when the observer's fiber gets to run, so a change landing in between
      * is still a change.
      */
    def dependsOn(sigs: Signal[?]*)(using Frame): Unit < (Async & Scope) =
        Kyo.foreach(sigs) { s =>
            s.current.map(at => Fiber.init(revalidate.andThen(s.observe(Present(at), Signal.defaultRepairInterval)(_ => revalidateAll))))
        }.unit

    /** This field must equal the live value of `other` (the password-confirmation pattern):
      * `addRule(Validator.matchesField(other, code))`. The rule carries its dependency, so the
      * field subscribes to `other` when the form mounts (or at once, when called after it) and
      * nothing else needs wiring. A change
      * of `other` rechecks the rule at once, whatever its `on`, so a match held to the blur
      * drops its error the moment `other` comes to match, and shows it at once on a touched
      * field when `other` stops matching. The field's other rules are not rerun by it.
      * For a rule that reads a sibling WITHOUT equality, use [[Validator.reading]]. Chainable.
      */
    def matches(other: Signal[A], code: String = "must-match")(using
        CanEqual[A, A],
        Frame
    ): (FormField[A] { type Valid = FormField.this.Valid }) < Sync =
        addRule(Validator.matchesField(other, code))

    // --- internals -------------------------------------------------------------

    /** The field's OWN events: the ones it declared with `on`, else [[FieldSpec.defaultActivation]]. An event among them
      * marks the field touched, which is what [[Reveal.WhenTouched]] waits for; whether a rule RUNS on an event is the
      * rule's own matter ([[Rule.on]]), with this set as the fallback only where `on` was declared.
      */
    private[form] def touchedBy(event: Activation.Field): Boolean = fieldOn.getOrElse(FieldSpec.defaultActivation).contains(event)

    /** Wire the change events: subscribe to what the rules read ([[observeDependencies]]), then observe the bound value
      * and, on every change, run the rules that are due for a change. The FIRST emission is the current (mount) value:
      * validate it SILENTLY (no reveal) so [[isValid]] is correct from the start without flagging an untouched field;
      * every subsequent (real) change runs [[onEvent]], which marks the field touched when [[Activation.Change]] is one of
      * its own events, so the error surfaces inline as the user types (RHF `onChange` mode). Subscribing first leaves no
      * gap between the mount's run and the subscription for a dependency to move in unheard.
      *
      * Every field is wired, whatever it declares: a rule may ask for changes in a field that does not, and a rule that
      * does not run on a change still has to learn that the value it judged is gone. Launched in the mount scope by
      * `Form.mountedWith`, so the observers are torn down with the form. Field-array rows are added long after that mount
      * body ran and have no ambient `Scope`, so they take [[wireChangeUnscoped]] instead; the events behave identically
      * there.
      */
    private[form] def wireChange(using Frame): Unit < (Async & Scope) =
        for
            _        <- wireDependencies
            baseline <- changeBaseline
            _        <- Fiber.init(changeObserver(baseline)).unit
        yield ()

    /** [[wireChange]] for a caller with no ambient `Scope`: the change observer runs on an UNSCOPED fiber, handed back
      * so the caller owns its lifetime, and the subscriptions to what the rules read end with [[unwire]]. `FieldArray` is
      * that caller: it cancels a row's fiber and unwires its fields when the row is removed and when the form's mount
      * scope closes, which is what makes the change events behave on rows exactly as they do on the static tree.
      */
    private[form] def wireChangeUnscoped(using Frame): Fiber[Unit, Any] < Sync =
        observeDependencies.andThen(changeBaseline.map(baseline => Fiber.initUnscoped(changeObserver(baseline))))

    /** [[observeDependencies]] in the enclosing `Scope`, with no change observer and no run of its own: what a test that
      * drives the field's events by hand wires.
      */
    private[form] def wireDependencies(using Frame): Unit < (Async & Scope) =
        Scope.ensure(unwire).andThen(observeDependencies)

    /** Subscribe to every signal the rule chain reads ([[Validator.reading]]), one observer per signal however many rules
      * read it, but never to the field's own value, whose changes are its own events. From then on [[addRule]] subscribes
      * to what an appended rule reads as it is appended.
      */
    private def observeDependencies(using Frame): Unit < Sync =
        chain.wire.map(observe)

    /** End every subscription to what the rules read: the mount or the row is over. */
    private[form] def unwire(using Frame): Unit < Sync = chain.close

    /** Start the observers of `signals`, which the chain has claimed for this field, and hand them to it. What each holds
      * now is noted first ([[Rule.Store.subscribe]]), so a change after this call reaches a rule that has judged nothing
      * yet, however soon it comes.
      */
    private def observe(signals: Chunk[Signal[?]])(using Frame): Unit < Sync =
        if signals.isEmpty then Kyo.unit
        else
            store.subscribe(signals).andThen(
                Kyo.foreach(signals)(s => Fiber.initUnscoped(s.observe(_ => dependencyChanged(s)))).map(chain.keep)
            )

    /** `signal`, which the rules read, has news (every value its observer delivers, the one at subscription included): an event for the field ([[Rule.Ctx.of]]). Each rule decides for itself,
      * against what it reads NOW: one that rechecks on `signal` runs against the field's current value when the value of
      * `signal` it last judged is not the one it reads, unless it already judged exactly this input; the others keep or
      * drop their verdicts. So a delivery behind the signal, or of a value some run has already judged, runs nothing, and
      * a field event that read the new value first does not swallow the change for a rule it was not due for. The field is
      * not touched.
      */
    private[form] def dependencyChanged(signal: Signal[?])(using Frame): Unit < Async =
        onCurrent((v, mine) => decide(v, signal, mine, Absent))

    /** The value the change observer starts from, read when the field is wired and not when the observer's fiber gets to
      * run: an edit landing in between is then a change from it, the reader's edit, rather than the value the field mounted
      * with. A reset before the wiring wrote a value no observer will deliver, so its pending write is spent here.
      */
    private def changeBaseline(using Frame): A < Sync =
        resetWrite.set(Absent).andThen(valueRef.get)

    /** The observer both wirings run: the mount's check first, SILENTLY (correct `isValid` without flagging an untouched
      * field), then every value that differs from `baseline` through [[onEvent]], except the value a reset wrote, which
      * the reset judges itself ([[resetField]]).
      */
    private def changeObserver(baseline: A)(using Frame): Unit < Async =
        revalidate.andThen(value.observe(Present(baseline), Signal.defaultRepairInterval) { v =>
            // Every emission spends a pending reset write, so one this observer never saw cannot be
            // taken for a later edit; and only the value the reset wrote counts as the reset's, which
            // the reset judges itself.
            resetWrite.getAndSet(Absent).map(written => if written.contains(v) then Kyo.unit else onEvent(v, Activation.Change))
        })

    /** An id the control already carried when [[bind]] stamped this field's own over it.
      *
      * `bind` is a pure `C => C` with nowhere to log, so it records the fact here and the
      * form reports it at mount, where an effect and a place to render exist. `Absent` is
      * every correct program.
      */
    private[form] var idConflict: Maybe[String] = Absent

    private[form] def noteIdConflict(previous: String): Unit =
        // The form's own stamp is not a conflict; a field bound twice would otherwise
        // report against itself.
        if previous != domId then idConflict = Present(previous)

    /** `event` happened on `v`: mark the field touched if it is one of its own events, and run the rules due for it on `v`.
      * The change observer is its caller: `v` is the emission it handles, and a write after it is the observer's next
      * emission, handled after this one with a later epoch. A test drives events through it too.
      */
    private[form] def onEvent(v: A, event: Activation.Field)(using Frame): Unit < Async =
        (if touchedBy(event) then touchedRef.set(true) else Kyo.unit).andThen(runAndSet(v, event))

    /** The bound control lost focus: mark the field touched if the blur is one of its own events, and run the rules due
      * for it on the value the field holds once the run has its epoch ([[onCurrent]]), not on the value the control read
      * for its blur. An edit landing between that read and the epoch would take the earlier epoch, and the blur's run on
      * the old value, being the last, would publish its verdict. Every control reads its blur value from the field's own
      * ref, so the value judged is the one it would have handed over.
      */
    private[form] def blurred(using Frame): Unit < Async =
        (if touchedBy(Activation.Blur) then touchedRef.set(true) else Kyo.unit)
            .andThen(onCurrent((v, mine) => decide(v, Activation.Blur, mine, Absent)))

    /** Re-run the rule chain against the latest value as a change event, without touching the field: the mount value, and
      * the value a reset leaves, judged as at the mount. Guarded against stale async runs by the epoch.
      */
    private[form] def revalidate(using Frame): Unit < Async =
        onCurrent((v, mine) => decide(v, Activation.Change, mine, Absent))

    /** Re-run EVERY rule against the latest value, whatever it is due for, without touching the field: a field this one
      * [[dependsOn]] changed, which changes what the rules would say about a value that did not move.
      */
    private def revalidateAll(using Frame): Unit < Async =
        onCurrent((v, mine) => decide(v, Rule.Sweep.Every, mine, Absent))

    /** Start a run on the value the field holds: take the run's epoch, THEN read the value. In this order a write after the
      * read is a later event with a later epoch, so the field's last run always judges its last value. Read first, an edit
      * landing between the read and the epoch took the earlier epoch, and the run on the old value, being the last, published
      * its verdict over the edit's. Every run that is not handed its value (the mount, a reset, a dependency, a settle) starts
      * here; a reset's and a dependency's run on fibers beside the change observer's, which is what lets an edit land.
      */
    private def onCurrent(run: (A, Int) => Unit < Async)(using Frame): Unit < Async =
        epoch.incrementAndGet.map(mine => FormField.startingRun.use(hook => hook).andThen(valueRef.use(v => run(v, mine))))

    /** Run the rules `trigger` is due for on `v`, and publish what the chain says.
      *
      * Every trigger takes an epoch first, so a run learns that something happened while it ran. The first rule the run
      * is due for makes it a due run ([[startDue]]): it claims the field as its latest due run (`dueState`), shows
      * `validating` and waits out the debounce before that rule runs. A run a later due run claimed the field from
      * meanwhile runs no rule at all, so a reader typing faster than the debounce has the rules asked once, about the last
      * value. A run no rule is due for only recalls verdicts (or finds rules not yet checked) and publishes at once, while
      * the epoch is still its own, leaving `validating` to a run still in flight. A due run that finds the epoch still its
      * own publishes its verdict and ends `validating`. One that finds a later due run leaves the field to it. One
      * overtaken only by runs no rule was due for ends as the field's last due run, but cannot publish its own verdict,
      * which may be about a value the field no longer holds: it settles instead ([[settle]]).
      */
    private def runAndSet(v: A, trigger: Rule.Trigger)(using Frame): Unit < Async =
        epoch.incrementAndGet.map(mine => decide(v, trigger, mine, Absent))

    /** Run the chain for `trigger` on `v` as the run whose epoch is `mine`, and publish. `settling` is the epoch of the
      * due run a settle finishes: a settle no rule is due for publishes on its behalf, which ends that run and clears
      * `validating`, and one overtaken in turn settles again.
      */
    private def decide(v: A, trigger: Rule.Trigger, mine: Int, settling: Maybe[Int])(using Frame): Unit < Async =
        for
            res   <- chain.run(v, Rule.Ctx.of(trigger, fieldOn, store, mine, startDue(mine)))
            state <- dueState.get
            // A due run at `mine` or later: this run was due, or a later one is, and then nothing publishes.
            _ <- finish(if FormField.dueRun(state) >= mine then Present(mine) else settling, mine, ClientVerdict.of(res))
        yield ()

    /** Before each rule the run whose epoch is `mine` is due for: whether the run still owns the field. Before the first,
      * it claims the field as its latest due run, showing `validating`, and waits out the debounce. A later due run (a
      * keystroke after it, a submit, a reset) that claimed the field meanwhile, during the debounce included, owns it, and
      * this run runs no rule: it would ask about a value the later run judges in its place. A run no rule is due for
      * never claims anything, so it leaves an earlier due run its field.
      */
    private def startDue(mine: Int)(using Frame): Boolean < Async =
        dueState.getAndUpdate(s => if FormField.dueRun(s) < mine then FormField.dueShowing(mine) else s).map { before =>
            val held                  = FormField.dueRun(before)
            val owns: Boolean < Async =
                if held > mine then false
                else if held == mine then true
                else
                    FormField.afterClaim.use(hook => hook).andThen(syncValidating).andThen(
                        if debounce > Duration.Zero then Async.sleep(debounce) else Kyo.unit
                    ).andThen(dueState.get.map(s => FormField.dueRun(s) == mine))
            owns
        }

    /** Bring `validating` in line with `dueState`: shown while the field's latest due run shows it. Whoever changes
      * `dueState` calls this after, and it writes again when the state moved while it wrote, so the last write agrees with
      * the last state in whatever order the writes land: a run paused between its claim and its write, overtaken by a run
      * that started and ended meanwhile, writes what the state says by then, never a `true` no run stands behind.
      */
    private def syncValidating(using Frame): Unit < Sync =
        dueState.get.map { s =>
            validatingRef.set(FormField.dueShows(s)).andThen(
                dueState.get.map(again => if FormField.dueShows(again) == FormField.dueShows(s) then Kyo.unit else syncValidating)
            )
        }

    /** Claim the field for the run whose epoch is `mine` without showing `validating`: a submit or a reset. It owns the
      * field from here, which ends a `validating` an earlier run showed, and that run, overtaken, publishes nothing.
      */
    private def claimQuietly(mine: Int)(using Frame): Unit < Sync =
        dueState.updateAndGet(s => if FormField.dueRun(s) < mine then FormField.dueQuiet(mine) else s).andThen(syncValidating)

    /** The run whose epoch is `mine` found `published`, on behalf of `owner`: the due run it is, or the due run a settle
      * finishes, or none for a run no rule was due for. Published if nothing happened since, which ends `owner` and so
      * `validating`. Otherwise, while `owner` is still the field's last due run, only runs no rule was due for happened,
      * and none of them could see what it stored: settle. A later due run leaves the field to that run.
      */
    private def finish(owner: Maybe[Int], mine: Int, published: ClientVerdict)(using Frame): Unit < Async =
        epoch.get.map { cur =>
            if cur == mine then
                clientRef.set(published).andThen(owner match
                    case Present(run) =>
                        dueState.compareAndSet(FormField.dueShowing(run), FormField.dueQuiet(run))
                            .map(ended => if ended then syncValidating else Kyo.unit)
                    case Absent => Kyo.unit)
            else
                owner match
                    case Present(run) => dueState.get.map(s => if FormField.dueRun(s) == run then settle(run) else Kyo.unit)
                    case Absent       => Kyo.unit
        }

    /** Finish the run whose epoch is `run`, which runs no rule was due for overtook, on the value the field holds now.
      * Those runs published what the rules had stored then, which this run's verdicts were not yet among, and a change of
      * a dependency among them ran nothing for a rule this run was judging. So every signal counts as changed
      * ([[Rule.Sweep.Settle]]): a rule rechecking on one that moved since it judged runs again, and the rest answer what
      * they hold for the current input.
      */
    private def settle(run: Int)(using Frame): Unit < Async =
        onCurrent((v, mine) => decide(v, Rule.Sweep.Settle, mine, Present(run)))

    /** Validate unconditionally for submit (every rule runs, whatever its activation), publish the error, and hand back
      * what the rules yielded for the value read here: ONE run, so submit decides validity and receives the validated
      * value from the same verdict, and a value written after the read cannot reach the handler. Each rule remembers what
      * it found, so the events after the submit keep its verdict while the value stays. It owns the field as a due run
      * does ([[runAndSet]]), with no debounce and no `validating` of its own ([[claimQuietly]]). It claims the field
      * BEFORE it reads the value, as [[onCurrent]] does: an edit after the read is then a later run, which owns the field
      * and publishes, where read first, the edit took the earlier epoch, lost the field to the submit and ran nothing,
      * and the submit published the old value's verdict with nothing left to correct it.
      */
    private[form] def verdict(using Frame): Result.Partial[FieldError, Valid] < Async =
        for
            mine <- epoch.incrementAndGet
            _    <- claimQuietly(mine)
            _    <- FormField.startingRun.use(hook => hook)
            v    <- valueRef.get
            res  <- chain.run(v, Rule.Ctx.all(store, mine))
            _    <- finish(Present(mine), mine, ClientVerdict.Checked(res.failure))
        yield res

    /** [[verdict]] reduced to the error it publishes. */
    private[form] def validateForSubmit(using Frame): Maybe[FieldError] < Async =
        verdict.map(_.failure)

    private[form] def submitField: FormField[?] = this

    /** For summary aggregation: the gated error as a (0-or-1) chunk. */
    private[form] def visibleErrorChunk(using Frame): Signal[Chunk[FieldError]] =
        visibleError.map(e => e.map(Chunk(_)).getOrElse(Chunk.empty))

    /** True while the field is not valid, regardless of touched/submit-reveal: the raw client/server
      * verdict holds an error, or a rule has not judged the current value ([[notChecked]]). Feeds
      * `Form.isValid(onlyVisible = false)`.
      */
    private[form] def invalid(using Frame): Signal[Boolean] =
        clientRef.combineLatest(serverVisible).map((c, s) => c != ClientVerdict.Checked(Absent) || s.isDefined)

    /** The gated error as a (0-or-1) chunk of [[Form.ErrorEntry]], tagged with this field's
      * [[domId]] so a summary can jump focus to it. Feeds `Form.errorEntries`.
      */
    private[form] def visibleErrorEntries(using Frame): Signal[Chunk[Form.ErrorEntry]] =
        visibleError.map(e => e.map(err => Chunk(Form.ErrorEntry(err, Present(domId)))).getOrElse(Chunk.empty))

    /** Revert value to the live baseline (the last [[rebase]], else [[initialValue]]),
      * and clear touched, error (client + server), validating, the verdicts the rules remember,
      * and the gate a submit naming this field opened (form reset). A run still in flight is
      * dropped rather than published over the reset.
      *
      * Then the field is judged as at the mount: one change run on the value it now holds
      * ([[revalidate]]), started here, whether or not the reset moved the value, so a rule not
      * due on a change is not yet checked after a reset as it is after the mount. It runs on its
      * own fiber, as the mount's does, so the reset returns at once and a debounce or a request
      * runs its normal course. The change observer would read a value the reset writes as the
      * reader's edit, marking the field touched and running the rules a second time; so the
      * reset records the value it writes, and the observer passes over an emission of exactly
      * that value. Only a write that changes the value is recorded, since an unchanged value
      * emits nothing.
      */
    private[form] def resetField(using Frame): Unit < Sync =
        for
            base    <- baselineRef.get
            current <- valueRef.get
            mine    <- epoch.incrementAndGet
            _       <- claimQuietly(mine)
            _       <- store.clear
            _       <- touchedRef.set(false)
            _       <- namedRef.set(false)
            _       <- clientRef.set(ClientVerdict.Checked(Absent))
            _       <- serverErrorRef.set(Absent)
            _       <- if current == base then Kyo.unit else resetWrite.set(Present(base)).andThen(valueRef.set(base))
            _       <- Fiber.initUnscoped(revalidate)
        yield ()

end FormField

/** What a field's rule chain last said about its value, as the field publishes it.
  *
  * `Checked` is a verdict: an error, or none. `NotChecked` is the third state a rule's activation makes possible: a rule
  * the last event was not due for has no verdict about the current value ([[Rule.on]]). It shows no error, because the
  * one it had was about another value, and it is not valid either, so the ungated [[Form.isValid]] reads it as `false`
  * (the gated view counts only what is shown). Kept apart from [[FieldError]] on purpose: it is not a failure a reader
  * could be shown.
  */
private[form] enum ClientVerdict derives CanEqual:
    case Checked(err: Maybe[FieldError])
    case NotChecked

    /** The error this verdict shows: none while not checked. */
    def error: Maybe[FieldError] =
        this match
            case Checked(e) => e
            case NotChecked => Absent
end ClientVerdict

private[form] object ClientVerdict:
    /** What a field publishes for the result of a run on one of its events. */
    def of(r: Result.Partial[FieldError | Rule.NotChecked, Any]): ClientVerdict =
        r.foldPartial[ClientVerdict](
            _ => Checked(Absent),
            {
                case e: FieldError => Checked(Present(e))
                case _             => NotChecked
            }
        )
end ClientVerdict

/** A server-set error paired with the field value it was judged against. Kept as a
  * value so the display can hide it the moment the value changes ([[FormField.serverVisible]]).
  * `derives CanEqual` is load-bearing: it rides a `Signal`, which requires `CanEqual`.
  */
final private[form] case class ServerError[A](err: FieldError, value: A) derives CanEqual

object FormField:

    /** What a run does right after it claimed its field as the latest due run, before it brings `validating` in line: nothing,
      * except in a test that pauses a run there to let another one overtake it.
      */
    private[form] val afterClaim: Local[Unit < Async] = Local.init[Unit < Async](Kyo.unit)

    /** What a run on the value the field holds does between the two steps that start it, taking its epoch and reading the
      * value: nothing, except in a test that pauses a run there to let an edit land in between.
      */
    private[form] val startingRun: Local[Unit < Async] = Local.init[Unit < Async](Kyo.unit)

    // A field's `dueState` holds its latest due run's epoch and whether that run shows `validating` in one atomic word (the
    // epoch shifted left once, plus one while it shows), so claiming the field and deciding what `validating` should read
    // are one step.
    private inline def dueShowing(epoch: Int): Long   = (epoch.toLong << 1) | 1L
    private inline def dueQuiet(epoch: Int): Long     = epoch.toLong << 1
    private inline def dueRun(state: Long): Int       = (state >>> 1).toInt
    private inline def dueShows(state: Long): Boolean = (state & 1L) == 1L

    /** A live field over `A` whose rules yield `B`. The one constructor [[FieldSpec.declare]] (and a test's hand-built
      * field) goes through, so the refinement is stated once.
      */
    private[form] def declared[A, B](
        valueRef: SignalRef[A],
        initial: A,
        baselineRef: SignalRef[A],
        domId: String,
        touchedRef: SignalRef[Boolean],
        validatingRef: SignalRef[Boolean],
        clientRef: SignalRef[ClientVerdict],
        serverErrorRef: SignalRef[Maybe[ServerError[A]]],
        epoch: AtomicInt,
        resetWrite: AtomicRef[Maybe[A]],
        rules: Rule[A, B],
        fieldOn: Maybe[Set[Activation.Field]],
        debounce: Duration,
        revealMode: Reveal,
        focusable: Boolean,
        translator: ErrorTranslator,
        submitCount: Signal[Int],
        namedRef: SignalRef[Boolean],
        owner: Form
    )(using CanEqual[A, A], Frame): (FormField[A] { type Valid = B }) < Sync =
        for
            rulesChain <- RuleChain.init(rules, valueRef)
            dueState   <- AtomicLong.init(0L)
            store      <- Rule.Store.init
        yield new FormField[A](
            valueRef,
            initial,
            baselineRef,
            domId,
            touchedRef,
            validatingRef,
            clientRef,
            serverErrorRef,
            epoch,
            dueState,
            resetWrite,
            store,
            fieldOn,
            debounce,
            revealMode,
            focusable,
            translator,
            submitCount,
            namedRef,
            owner
        ):
            type Valid = B
            private[form] def chain: RuleChain[A, B] = rulesChain
    end declared
end FormField

/** A field's rule chain: what it was declared with, plus whatever [[FormField.addRule]] appended since, together with the
  * subscriptions the field holds to the signals its rules read. Typed by what the declared chain yields, which an
  * appended check keeps (see [[Rule.andCheck]]).
  *
  * The rules and the subscriptions share one atomic state because a rule may be appended at any time, from a handler
  * after the mount as well as during the build, and the two must agree: wiring claims every signal the chain reads at
  * that moment, and an append after it claims the signals of the new rule that are not observed yet, so a signal is
  * observed exactly once and none is missed, whichever comes first. The field's own value is observed from the start:
  * its change observer already reports every edit, and a rule reading it must not hear the edit twice.
  */
final private[form] class RuleChain[A, B] private (own: Signal[?], state: AtomicRef[RuleChain.Props[A, B]]):
    import RuleChain.*

    /** Run the chain under `ctx`. */
    def run[S](a: A, ctx: Rule.Ctx[S])(using Frame): Result.Partial[FieldError | S, B] < Async =
        state.get.map(_.rules.runIn(a, ctx))

    /** Append a check. Once the field is wired, hand back the signals the check reads that the field does not observe yet,
      * now claimed: the caller starts their observers.
      */
    def append(v: Rule[A, Any])(using Frame): Chunk[Signal[?]] < Sync =
        state.getAndUpdate(st =>
            st.wiring match
                case Wiring.Wired(observed, fibers) =>
                    Props(st.rules.andCheck(v), Wiring.Wired(observed ++ unobserved(observed, v.dependencies), fibers))
                case _ => Props(st.rules.andCheck(v), st.wiring)
        ).map(old =>
            old.wiring match
                case Wiring.Wired(observed, _) => unobserved(observed, v.dependencies)
                case _                         => Chunk.empty
        )

    /** Start observing: hand back, claimed, every signal the chain reads but the field's own value. Nothing while the
      * field is wired already.
      */
    def wire(using Frame): Chunk[Signal[?]] < Sync =
        state.getAndUpdate(st =>
            st.wiring match
                case Wiring.Unwired =>
                    Props(st.rules, Wiring.Wired(Chunk(own) ++ unobserved(Chunk(own), st.rules.dependencies), Chunk.empty))
                case _ => st
        ).map(old =>
            old.wiring match
                case Wiring.Unwired => unobserved(Chunk(own), old.rules.dependencies)
                case _              => Chunk.empty
        )

    /** Keep the observers started for claimed signals, or interrupt them if the wiring ended meanwhile. */
    def keep(fibers: Chunk[Fiber[Unit, Any]])(using Frame): Unit < Sync =
        state.getAndUpdate(st =>
            st.wiring match
                case Wiring.Wired(observed, held) => Props(st.rules, Wiring.Wired(observed, held ++ fibers))
                case _                            => st
        ).map(old =>
            old.wiring match
                case Wiring.Wired(_, _) => Kyo.unit
                case _                  => Kyo.foreachDiscard(fibers)(_.interrupt)
        )

    /** End the wiring: interrupt every observer, and observe nothing appended later. */
    def close(using Frame): Unit < Sync =
        state.getAndUpdate(st => Props(st.rules, Wiring.Unwired)).map(old =>
            old.wiring match
                case Wiring.Wired(_, fibers) => Kyo.foreachDiscard(fibers)(_.interrupt)
                case _                       => Kyo.unit
        )
end RuleChain

private[form] object RuleChain:

    def init[A, B](rules: Rule[A, B], own: Signal[?])(using Frame): RuleChain[A, B] < Sync =
        AtomicRef.init(Props(rules, Wiring.Unwired)).map(new RuleChain(own, _))

    /** The rules, and where the field stands with observing what they read. */
    final case class Props[A, B](rules: Rule[A, B], wiring: Wiring)

    enum Wiring derives CanEqual:
        /** Not observing: not wired yet, when the wiring will claim whatever the chain reads by then, or the mount or the
          * row ended.
          */
        case Unwired

        /** Observing `observed` (the field's own value among them) through `fibers`. */
        case Wired(observed: Chunk[Signal[?]], fibers: Chunk[Fiber[Unit, Any]])
    end Wiring

    /** The signals of `signals` (distinct already) that `observed` does not hold. By identity, as [[Rule.dependencies]]. */
    private def unobserved(observed: Chunk[Signal[?]], signals: Chunk[Signal[?]]): Chunk[Signal[?]] =
        signals.filterNot(s => observed.exists(_ eq s))
end RuleChain

/** Stamps the field's id onto the control, and notices when that displaces one the caller
  * had already set.
  *
  * The slot belongs to the form: `focusFirstInvalid` and the error summary both address the
  * field through `Commands.focusId(domId)`, so a control answering to something else is a
  * field they cannot reach. That used to happen in silence. It cannot be refused here (`bind`
  * is a pure `C => C`), so the fact is recorded on the field and the form reports it at mount.
  * The way to choose an id is `form.field(...).domId(...)`, which settles it before anything
  * reads it.
  */
private def stampId[C <: FormControl { type Self <: C }](control: C, field: FormField[?]): C =
    control.elementId.foreach(field.noteIdConflict)
    control.id(field.domId)

/** Postfix binding: `uic.Input()...bind(field)` returns the same concrete control
  * type (`C` is inferred from the receiver, so no type ascription is needed). The
  * extension is split by control family (a `String` field binds to a text control,
  * a `Boolean` field to a checkbox or switch) because only the value binding is
  * value-typed. The blur trigger ignores the value the control hands it and judges what
  * the field holds ([[FormField.blurred]]); everything gated stays shared on [[FormField]].
  */
extension [C <: TextFormControl { type Self <: C }](control: C)
    def bind(field: FormField[String])(using Frame): C =
        var b: C = stampId(control.value(field.valueRef), field)
        b = b.invalidMessage(field.message)
        b = b.onBlur(_ => field.blurred)
        b
    end bind

    def bindControlOnly(field: FormField[String])(using Frame): C =
        var b: C = stampId(control.value(field.valueRef), field)
        b = b.invalid(field.visibleError.map(_.isDefined))
        b = b.onBlur(_ => field.blurred)
        b
    end bindControlOnly
end extension

/** A `Boolean` field binds to any control with a `checked` and an `onBlur` setter, generated or written by hand ([[HasSetter]]). */
extension [C <: FormControl { type Self <: C }](control: C)
    def bind(field: FormField[Boolean])(using
        checked: HasSetter[C, "checked", UI.Prop[Boolean]],
        blur: HasSetter[C, "onBlur", Boolean => UI.Action],
        frame: Frame
    ): C =
        blur(stampId(checked(control, field.valueRef), field).invalidMessage(field.message), _ => field.blurred)

    def bindControlOnly(field: FormField[Boolean])(using
        checked: HasSetter[C, "checked", UI.Prop[Boolean]],
        blur: HasSetter[C, "onBlur", Boolean => UI.Action],
        frame: Frame
    ): C =
        blur(stampId(checked(control, field.valueRef), field).invalid(field.visibleError.map(_.isDefined)), _ => field.blurred)
end extension

/** The `Double`-control binding, as a named helper so a wrapping control ([[NumberField]]'s
  * `InputNumber`) can reuse it without the `bind` name colliding with its own extension.
  */
private[form] def wireNumber[C <: NumberFormControl { type Self <: C }](control: C, field: FormField[Double])(using Frame): C =
    var b: C = stampId(control.value(field.valueRef), field)
    b = b.invalidMessage(field.message)
    b = b.onBlur(_ => field.blurred)
    b
end wireNumber

private[form] def wireNumberControlOnly[C <: NumberFormControl { type Self <: C }](control: C, field: FormField[Double])(using Frame): C =
    var b: C = stampId(control.value(field.valueRef), field)
    b = b.invalid(field.visibleError.map(_.isDefined))
    b = b.onBlur(_ => field.blurred)
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
        var b: C = stampId(control.value(field.valueRef), field)
        b = b.invalidMessage(field.message)
        b = b.onBlur(_ => field.blurred)
        b
    end bind

    def bindControlOnly(field: FormField[Seq[UI.FilePayload]])(using Frame): C =
        var b: C = stampId(control.value(field.valueRef), field)
        b = b.invalid(field.visibleError.map(_.isDefined))
        b = b.onBlur(_ => field.blurred)
        b
    end bindControlOnly
end extension

/** The choice-control binding: the field's value type is the option type, so a field that always holds an option binds as
  * `FormField[A]` and one that may stay empty as `FormField[Maybe[A]]`. The blur trigger validates what the field holds, which
  * is what the control was just showing, since the two share the field's ref.
  */
extension [A, C <: ChoiceFormControl[A] { type Self <: C }](control: C)
    def bind(field: FormField[A])(using Frame): C =
        var b: C = stampId(control.selection(field.valueRef), field)
        b = b.invalidMessage(field.message)
        b = b.onBlur(_ => field.blurred)
        b
    end bind

    def bindControlOnly(field: FormField[A])(using Frame): C =
        var b: C = stampId(control.selection(field.valueRef), field)
        b = b.invalid(field.visibleError.map(_.isDefined))
        b = b.onBlur(_ => field.blurred)
        b
    end bindControlOnly

    @targetName("bindMaybe")
    def bind(field: FormField[Maybe[A]])(using Frame): C =
        var b: C = stampId(control.selection(field.valueRef), field)
        b = b.invalidMessage(field.message)
        b = b.onBlur(_ => field.blurred)
        b
    end bind

    @targetName("bindControlOnlyMaybe")
    def bindControlOnly(field: FormField[Maybe[A]])(using Frame): C =
        var b: C = stampId(control.selection(field.valueRef), field)
        b = b.invalid(field.visibleError.map(_.isDefined))
        b = b.onBlur(_ => field.blurred)
        b
    end bindControlOnly
end extension

extension [A, C <: MultiChoiceFormControl[A] { type Self <: C }](control: C)
    @targetName("bindSet")
    def bind(field: FormField[Set[A]])(using Frame): C =
        var b: C = stampId(control.selection(field.valueRef), field)
        b = b.invalidMessage(field.message)
        b = b.onBlur((_: Set[A]) => field.blurred)
        b
    end bind

    @targetName("bindControlOnlySet")
    def bindControlOnly(field: FormField[Set[A]])(using Frame): C =
        var b: C = stampId(control.selection(field.valueRef), field)
        b = b.invalid(field.visibleError.map(_.isDefined))
        b = b.onBlur((_: Set[A]) => field.blurred)
        b
    end bindControlOnly
end extension

/** The binding of a control holding an optional typed value ([[kyo.uic.DatePicker]]): the field's value type is `Maybe[A]`,
  * the control reads and writes the field's own ref, and the blur trigger validates what the field holds, which is what the
  * control was just showing.
  */
extension [A, C <: OptionalFormControl[A] { type Self <: C }](control: C)
    @targetName("bindOptional")
    def bind(field: FormField[Maybe[A]])(using Frame): C =
        var b: C = stampId(control.value(field.valueRef), field)
        b = b.invalidMessage(field.message)
        b = b.onBlur(_ => field.blurred)
        b
    end bind

    @targetName("bindControlOnlyOptional")
    def bindControlOnly(field: FormField[Maybe[A]])(using Frame): C =
        var b: C = stampId(control.value(field.valueRef), field)
        b = b.invalid(field.visibleError.map(_.isDefined))
        b = b.onBlur(_ => field.blurred)
        b
    end bindControlOnly
end extension
