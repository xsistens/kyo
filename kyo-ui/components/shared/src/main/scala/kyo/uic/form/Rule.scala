package kyo.uic.form

import kyo.*

/** A rule checks a value of type `A` and, when it passes, yields a value of type `B`: "parse, don't validate". A rule that
  * only checks yields its input unchanged and is a [[Validator]]; a rule that also refines the type says so in `B`, as
  * [[Validator.chosen]] does by turning the `Maybe[A]` an optional field holds into the `A` it was required to hold.
  *
  * It runs in the one effect a mounted event handler actually runs in: `Async`. A pure rule widens for free; an async rule
  * (a mutation, a REST call, anything) discharges its OWN failure channel into a [[FieldError]] before it becomes a `Rule`,
  * so the core never depends on any transport library. Failure is a value, not an `Abort`: a `Result.Partial` holds either
  * the refined value or the one error the field shows.
  *
  * Rules compose with [[Rule.and]], left to right, each one receiving what the previous one yielded, and the first failure
  * is the one reported. That is why a field's validated type follows from its rules alone: `form.field(Maybe.empty[LocalDate])
  * .rules(Validator.chosen())` declares a field whose [[FormField.Valid]] is `LocalDate`, and [[Form.submit]] hands the
  * handler that `LocalDate` rather than the `Maybe` the field holds.
  *
  * Each rule also carries WHEN it runs in a field: [[Rule.on]] on the rule, else the field's own `on`, else its default
  * (every change and the blur, only the blur for [[Validator.async]]). A rule that is not due for an event keeps its last
  * verdict while its input is the one it judged, and is otherwise "not yet checked": the chain stops there, the field shows
  * no error from it, and the form is not valid until it runs. A submit runs every rule. See [[Rule.on]].
  *
  * A rule may read other fields too ([[Rule.reading]], [[Validator.reading]]): what it read is part of the input its
  * verdict is kept for, and a change of it reruns the rule or drops its verdict as its [[Dep]] says.
  *
  * Contravariant in what it checks, covariant in what it yields: a rule over `Any` checks a `String`, and a rule yielding a
  * `LocalDate` yields an `AnyRef`.
  */
opaque type Rule[-A, +B] = Rule.Run[A, B]

object Rule:

    /** A rule from its function: `Result.Success(b)` passes and yields `b`, `Result.Failure(e)` fails with `e`. The
      * function may suspend (a server round-trip); fold its own failures into a [[FieldError]] here, which keeps this
      * layer free of any transport. [[Validator.refine]] is the pure shorthand for the common "parse or fail with a code".
      *
      * Runs on every change and on the blur unless [[on]] or its field says otherwise.
      */
    def apply[A, B](f: A => Result.Partial[FieldError, B] < Async): Rule[A, B] = leaf(defaultOn)(f)

    /** A rule that reads `dep` too, and refines: `Rule.reading(offset)((s: String, o: Int) => ...)` is shaped like
      * [[apply]] with what it read as the second argument. A change of `dep` runs it at once; see [[Validator.reading]] for
      * what reading means for a field, and [[Dep]] for the reactions.
      */
    def reading[D](dep: Signal[D])(using Frame): Reading[D] = new Reading(Source.one(Dep.default(dep)))

    /** [[reading]] with the dependency's reaction stated: `Rule.reading(Dep.discard(country))`. */
    def reading[D](dep: Dep[D])(using Frame): Reading[D] = new Reading(Source.one(dep))

    /** A rule that reads two signals, each with its own reaction; the function receives both values as a pair. */
    def reading[D1, D2](d1: Dep[D1], d2: Dep[D2])(using Frame): Reading[(D1, D2)] = new Reading(Source.two(d1, d2))

    /** A rule that reads three signals, each with its own reaction; the function receives the values as a triple. */
    def reading[D1, D2, D3](d1: Dep[D1], d2: Dep[D2], d3: Dep[D3])(using Frame): Reading[(D1, D2, D3)] =
        new Reading(Source.three(d1, d2, d3))

    /** What [[Rule.reading]] returns: the dependencies a refining rule reads, waiting for its function.
      *
      * The function receives the field's value and the dependencies' values read at the start of the run, and those
      * values are part of the input the rule's verdict is kept for: an event that does not run the rule reuses the
      * verdict only while both the value and every dependency are the ones it judged, and is otherwise not yet checked.
      * A dependency given as a bare signal rechecks, like [[Validator.reading]]'s sync form, since `Rule(f)` has no
      * separate async default either.
      *
      * The rule runs on the field's events like [[Rule.apply]] (every change and the blur unless [[Rule.on]] or the field
      * says otherwise), on a change of a dependency as that dependency's [[Dep]] says, and at every submit.
      */
    final class Reading[D] private[form] (source: Source[D]):

        /** The rule from its function, `(value, read) => Result.Success(b)` or `Result.Failure(e)`. */
        def apply[A, B](f: (A, D) => Result.Partial[FieldError, B] < Async)(using Frame): Rule[A, B] =
            leafReading(defaultOn, source, Dep.Reaction.Recheck)(f)
    end Reading

    /** The dependencies a reading rule names, how to read their values in one go, and how to take what was read apart
      * again, one value per dependency in the order of `deps` (what a field compares when one of them changes). Each of the
      * `reading` overloads of [[Rule]] and [[Validator]] builds one, so the two only differ in the function they take.
      */
    final private[form] class Source[D](val deps: Chunk[Dep[?]], val current: D < Sync, val values: D => Chunk[Any]):

        /** Each dependency with its reaction, a bare signal's taken from `default`. */
        def reads(default: Dep.Reaction): Chunk[Read] =
            deps.map(d => Read(d.signal, d.reaction.getOrElse(default) == Dep.Reaction.Recheck))
    end Source

    private[form] object Source:
        def one[D](d: Dep[D])(using Frame): Source[D] = new Source(Chunk(d), d.signal.current, v => Chunk(v))

        def two[D1, D2](d1: Dep[D1], d2: Dep[D2])(using Frame): Source[(D1, D2)] =
            new Source(Chunk(d1, d2), d1.signal.current.map(v1 => d2.signal.current.map(v2 => (v1, v2))), t => Chunk(t._1, t._2))

        def three[D1, D2, D3](d1: Dep[D1], d2: Dep[D2], d3: Dep[D3])(using Frame): Source[(D1, D2, D3)] =
            new Source(
                Chunk(d1, d2, d3),
                d1.signal.current.map(v1 => d2.signal.current.map(v2 => d3.signal.current.map(v3 => (v1, v2, v3)))),
                t => Chunk(t._1, t._2, t._3)
            )
    end Source

    /** A signal a leaf reads, and whether a change of it runs the leaf (`recheck`) or only drops its verdict. */
    final private[form] class Read(val signal: Signal[?], val recheck: Boolean)

    /** The events a rule runs on when neither it nor its field names any: every change and the blur. */
    private[form] val defaultOn: Set[Activation.Field] = Set(Activation.Blur, Activation.Change)

    /** A check as a rule yielding its input: `Absent` passes, `Present(e)` fails with `e`. Runs on `default` unless told
      * otherwise.
      */
    private[form] def check[A](default: Set[Activation.Field])(f: A => Maybe[FieldError] < Async)(using Frame): Rule[A, A] =
        leaf(default)(a => f(a).map(passOr(a)))

    /** [[check]] for a rule that reads `source` too: what [[Validator.reading]] builds. A bare signal reacts as
      * `reaction` says.
      */
    private[form] def checkReading[A, D](default: Set[Activation.Field], source: Source[D], reaction: Dep.Reaction)(
        f: (A, D) => Maybe[FieldError] < Async
    )(using Frame): Rule[A, A] =
        leafReading(default, source, reaction)((a, d) => f(a, d).map(passOr(a)))

    /** What a check found, as the verdict of a rule yielding its input `a`. */
    private def passOr[A](a: A)(found: Maybe[FieldError]): Result.Partial[FieldError, A] =
        found match
            case Present(e) => Result.Failure(e)
            case Absent     => Result.Success(a)

    /** A pure rule, built without suspending, so it needs no `Frame`: the shape of every predicate built-in. */
    private[form] def pure[A, B](f: A => Result.Partial[FieldError, B]): Rule[A, B] = leaf(defaultOn)(a => f(a))

    /** The rule that passes every value. It has nothing to check, so it is never "not yet checked": a field with no rules
      * is valid whatever its events, which is what lets [[Validator.none]] be every field's default.
      */
    private[form] def identity[A]: Rule[A, A] =
        new Run[A, A]:
            def apply[S](a: A, ctx: Ctx[S]): Result.Partial[FieldError | S, A] < Async = Result.Success(a)
            def reads: Chunk[Signal[?]]                                                = Chunk.empty

    /** [[Validator.when]]'s body, here because only the companion sees the rule as what it is. */
    private[form] def guarded[A](cond: A => Boolean)(v: Rule[A, A]): Rule[A, A] =
        new Run[A, A]:
            def apply[S](a: A, ctx: Ctx[S]): Result.Partial[FieldError | S, A] < Async =
                if cond(a) then v(a, ctx) else Result.Success(a)
            def reads: Chunk[Signal[?]] = v.reads

    /** [[Validator.ifPresent]]'s body: `r` on a present value, refining inside the `Maybe`; an empty one passes. */
    private[form] def present[A, B](r: Rule[A, B])(using Frame): Rule[Maybe[A], Maybe[B]] =
        new Run[Maybe[A], Maybe[B]]:
            def apply[S](m: Maybe[A], ctx: Ctx[S]): Result.Partial[FieldError | S, Maybe[B]] < Async =
                m match
                    case Present(a) =>
                        r(a, ctx).map(_.foldPartial[Result.Partial[FieldError | S, Maybe[B]]](
                            b => Result.Success(Present(b)),
                            e => Result.Failure(e)
                        ))
                    case Absent => Result.Success(Absent)
            def reads: Chunk[Signal[?]] = r.reads

    /** A rule with its own function: the one place a rule decides, through `ctx`, whether it runs for this event. Its key
      * is its identity in a field's [[Store]], so each rule built here remembers its own last verdict.
      */
    private def leaf[A, B](default: Set[Activation.Field])(f: A => Result.Partial[FieldError, B] < Async): Rule[A, B] =
        val key = new Key[B]
        new Run[A, B]:
            def apply[S](a: A, ctx: Ctx[S]): Result.Partial[FieldError | S, B] < Async =
                ctx.gate(key, default, Chunk.empty, a, Chunk.empty)(f(a))
            def reads: Chunk[Signal[?]] = Chunk.empty
        end new
    end leaf

    /** [[leaf]] for a rule that reads `source` too. It reads every dependency once, at the start of the run, and what it
      * read is both what `f` receives and part of the input its verdict is kept for, so a verdict is reused only while the
      * value AND what the rule read are the ones it judged. `ctx` decides as for any leaf, with the reactions in hand for
      * a dependency's change.
      */
    private def leafReading[A, D, B](default: Set[Activation.Field], source: Source[D], reaction: Dep.Reaction)(
        f: (A, D) => Result.Partial[FieldError, B] < Async
    )(using Frame): Rule[A, B] =
        val key     = new Key[B]
        val deps    = source.reads(reaction)
        val signals = distinct(Chunk.empty, deps.map(_.signal))
        new Run[A, B]:
            def apply[S](a: A, ctx: Ctx[S]): Result.Partial[FieldError | S, B] < Async =
                source.current.map(d => ctx.gate(key, default, deps, a, source.values(d))(f(a, d)))
            def reads: Chunk[Signal[?]] = signals
        end new
    end leafReading

    /** `into` followed by each signal of `more` it does not hold yet. By identity: two leaves reading the same signal
      * share one subscription, and a signal is not a value to compare.
      */
    private def distinct(into: Chunk[Signal[?]], more: Chunk[Signal[?]]): Chunk[Signal[?]] =
        more.foldLeft(into)((acc, s) => if acc.exists(_ eq s) then acc else acc.append(s))

    extension [A, B](self: Rule[A, B])
        // `private[uic]` and not `private[form]`: a form field was the first host of these rules and is no longer the
        // only one. A DataTable cell runs the same chain over the value its column parsed, and the opaque type is what
        // keeps the function itself out of reach either way.
        /** Every rule of the chain, whatever its activation: a submit. Nothing is remembered. */
        private[uic] def run(a: A)(using Frame): Result.Partial[FieldError, B] < Async = self(a, Ctx.checkAll)

        /** The chain under `ctx`: how a field runs it for one of its events, or for its submit. */
        private[form] def runIn[S](a: A, ctx: Ctx[S]): Result.Partial[FieldError | S, B] < Async = self(a, ctx)

        /** Every signal a rule of the chain reads ([[Rule.reading]], [[Validator.reading]]), once each, in chain order: what
          * a field subscribes to when it is wired.
          */
        private[form] def dependencies: Chunk[Signal[?]] = self.reads

        /** Ordered short-circuit with refinement: run `self`; on a failure stop and report it, otherwise run `next` on what
          * `self` yielded. A field shows only ONE error at a time, the first open rule of the chain (`required and
          * minLength(5)` shows "required" until the field is non-empty, then "min-length"), and the chain's result type is
          * the last rule's: `Rule[A, B] and Rule[B, C]` is a `Rule[A, C]`.
          *
          * A rule that is not yet checked stops the chain the same way: `next` needs what it would have yielded.
          */
        infix def and[C](next: Rule[B, C])(using Frame): Rule[A, C] =
            new Run[A, C]:
                def apply[S](a: A, ctx: Ctx[S]): Result.Partial[FieldError | S, C] < Async =
                    self(a, ctx).map { r =>
                        r.foldPartial[Result.Partial[FieldError | S, C] < Async](b => next(b, ctx), e => Result.Failure(e))
                    }
                def reads: Chunk[Signal[?]] = distinct(self.reads, next.reads)

        /** When this rule runs in a field: on each of `ts` and at every submit, whatever the field declares.
          *
          * Without it a rule follows its field's `on` ([[FieldSpec.on]]), and without that its own default: every change and
          * the blur, the blur alone for [[Validator.async]]. The nearer `on` wins: `(a and b).on(Activation.Blur)` puts
          * every rule in it on the blur except one that states its own, so `(a.on(Activation.Change) and b).on(Activation.Blur)`
          * runs `a` on every change and `b` on the blur.
          *
          * An event the rule is not due for reuses its last verdict while its input is the value it judged, so a blur
          * check's error stays until the reader edits. Any other input leaves it NOT YET CHECKED: the chain stops there,
          * the field shows no error from it (the message about the old value goes), the rules after it do not run, and
          * [[Form.isValid]]`()` reads `false` until it has run. So put deferred rules at the END of a chain, after the cheap
          * ones that can already say something while the reader types:
          * {{{
          * form.field.rules(Validator.required() and Validator.minLength(3) and usernameFree.on(Activation.Blur))
          * }}}
          * `Activation.Submit` alone means "at submit only". A submit runs every rule regardless, and so does a DataTable
          * cell's commit.
          */
        def on(ts: Activation.Field*): Rule[A, B] =
            val set = ts.toSet
            new Run[A, B]:
                def apply[S](a: A, ctx: Ctx[S]): Result.Partial[FieldError | S, B] < Async = self(a, ctx.within(set))
                def reads: Chunk[Signal[?]]                                                = self.reads
        end on

        /** The text the reader sees when this rule fails: its error carries `text` as the
          * [[FieldError.fallback]], so [[ErrorTranslator.default]] shows it with no table at all, and the code stays
          * what it was, so a test or a summary can still tell the failures apart.
          *
          * Put it on each rule, not on a chain: `(a and b).message(t)` replaces the text of whichever rule failed, so
          * the reader would read `t` for both. `Validator.required().message("Write something first.") and
          * Validator.minLength(3).message("A comment has at least 3 characters.")` says one thing per rule.
          *
          * A translator still has the first word: an entry for the rule's code in an [[ErrorTranslator.englishMap]]
          * (or an i18n lookup) is shown instead, since a translator is how an app replaces every visible string at
          * once. Give a rule a code of its own (`required("comment-empty")`) to keep a table entry for the default
          * code from reaching it.
          */
        def message(text: String)(using Frame): Rule[A, B] =
            new Run[A, B]:
                def apply[S](a: A, ctx: Ctx[S]): Result.Partial[FieldError | S, B] < Async =
                    self(a, ctx).map(r =>
                        r.foldPartial[Result.Partial[FieldError | S, B]](
                            _ => r,
                            {
                                case e: FieldError => Result.Failure(e.copy(fallback = Present(text)))
                                case _             => r
                            }
                        )
                    )
                def reads: Chunk[Signal[?]] = self.reads

        /** Runs `self`, then the check `next` on the SAME input, and yields what `self` yielded. How a field appends a rule
          * over its value type after declaration ([[FormField.addRule]]) without losing the type its chain refines to.
          */
        private[form] def andCheck(next: Rule[A, Any])(using Frame): Rule[A, B] =
            new Run[A, B]:
                def apply[S](a: A, ctx: Ctx[S]): Result.Partial[FieldError | S, B] < Async =
                    self(a, ctx).map { r =>
                        r.foldPartial[Result.Partial[FieldError | S, B] < Async](
                            _ => next(a, ctx).map(n => n.foldPartial[Result.Partial[FieldError | S, B]](_ => r, e => Result.Failure(e))),
                            e => Result.Failure(e)
                        )
                    }
                def reads: Chunk[Signal[?]] = distinct(self.reads, next.reads)

        /** This rule over another input, converted by `f` first: how a number field states its rules over `A` and runs
          * them on the `Double` its control binds. The activation and the remembered verdicts carry through.
          */
        private[form] def contramap[A0](f: A0 => A): Rule[A0, B] =
            new Run[A0, B]:
                def apply[S](a0: A0, ctx: Ctx[S]): Result.Partial[FieldError | S, B] < Async = self(f(a0), ctx)
                def reads: Chunk[Signal[?]]                                                  = self.reads
    end extension

    /** What a rule is: run under a [[Ctx]] that says which event asked and what to do with a rule not due for it. `S` is
      * what such a rule answers instead of a verdict: [[NotChecked]] for a field's event, `Nothing` where every rule runs.
      */
    sealed abstract private[form] class Run[-A, +B]:
        def apply[S](a: A, ctx: Ctx[S]): Result.Partial[FieldError | S, B] < Async

        /** The signals the leaves below read, once each. Abstract, so a new combinator has to say: one that drops it
          * leaves a field deaf to what its rules read.
          */
        def reads: Chunk[Signal[?]]
    end Run

    /** The answer of a rule that was not due for the event and whose last verdict was about another value. */
    final private[form] class NotChecked private[Rule] ()

    private[form] val notChecked: NotChecked = new NotChecked

    /** A rule's identity in a field's [[Store]]: allocated once per rule built, typed by what the rule yields. */
    final private[form] class Key[B]

    /** What asks a field to run its rules: one of its own events, a change of a signal a rule reads, or a [[Sweep]]. */
    private[form] type Trigger = Activation.Field | Signal[?] | Sweep

    /** The triggers that are about no single event. */
    private[form] enum Sweep derives CanEqual:

        /** Every rule runs: a change of a signal the field `dependsOn`, which its rules read in no way the field can see. */
        case Every

        /** A run ended after an event it was not due for, which ran nothing and could not see the verdicts that run had
          * not stored yet. Every signal is taken as changed: a rule rechecking on one that moved since the rule last
          * judged it runs, and every other rule answers what it stored.
          */
        case Settle
    end Sweep

    /** Where a chain runs, and so whether each rule in it runs. */
    sealed abstract private[form] class Ctx[+S]:

        /** Run `check`, the function of the rule `key`, if the rule is due here; otherwise answer for it without running
          * it. `default` is the rule's own activation, used when no `on` around it and no field `on` says otherwise;
          * `reads` are the signals it reads, which decide for a change of one of them. Its verdict is kept for `value`
          * together with `read`, what it read, one value per signal of `reads`.
          */
        def gate[B](key: Key[B], default: Set[Activation.Field], reads: Chunk[Read], value: Any, read: Chunk[Any])(
            check: => Result.Partial[FieldError, B] < Async
        ): Result.Partial[FieldError | S, B] < Async

        /** This context inside an explicit `on(ts)`, which is the nearest one for every rule below it. */
        def within(on: Set[Activation.Field]): Ctx[S]
    end Ctx

    private[form] object Ctx:

        /** Every rule runs and nothing is remembered: how a rule is checked outside a field ([[run]]). */
        val checkAll: Ctx[Nothing] =
            new Ctx[Nothing]:
                def gate[B](key: Key[B], default: Set[Activation.Field], reads: Chunk[Read], value: Any, read: Chunk[Any])(
                    check: => Result.Partial[FieldError, B] < Async
                ): Result.Partial[FieldError, B] < Async            = check
                def within(on: Set[Activation.Field]): Ctx[Nothing] = this

        /** A field's submit: every rule runs, and each remembers its verdict for the events after it. */
        def all(store: Store, epoch: Int)(using Frame): Ctx[Nothing] =
            new Ctx[Nothing]:
                def gate[B](key: Key[B], default: Set[Activation.Field], reads: Chunk[Read], value: Any, read: Chunk[Any])(
                    check: => Result.Partial[FieldError, B] < Async
                ): Result.Partial[FieldError, B] < Async =
                    check.map(r => store.record(key, value, read, r, epoch).andThen(r))
                def within(on: Set[Activation.Field]): Ctx[Nothing] = this

        /** A field's rules run for `trigger`. `epoch` is the run's place among the field's runs, so a slower, older run
          * cannot overwrite what a newer one remembered, and `due` runs before each rule that does run: how the field
          * learns that the run checks something (and so shows `validating` and waits out its debounce) without asking
          * the chain twice.
          *
          *   - One of the field's own events: a rule runs if it is due for it, by its nearest `on`, else the field's `on`,
          *     else its own default. One that is not due answers its remembered verdict if it judged this input last, else
          *     [[NotChecked]].
          *   - A change of a signal: a rule that holds a verdict for exactly its current input answers it. Otherwise one
          *     that reads the signal with [[Dep.recheck]] runs, whatever its `on`, when the value it reads now differs
          *     from the one it last judged (or, having judged nothing, from what the signal held when the field began
          *     observing it), and every other rule is [[NotChecked]]. So a [[Dep.discard]] rule, whose input now
          *     differs, is not yet checked, a rule that does not read the signal keeps its verdict, and a rule whose own
          *     value moved while the signal did not waits for its own event, as it would have without the change.
          *   - [[Sweep.Every]]: every rule runs.
          *   - [[Sweep.Settle]]: a change of every signal at once.
          */
        def of(trigger: Trigger, fieldOn: Maybe[Set[Activation.Field]], store: Store, epoch: Int, due: Unit < Async)(using
            Frame
        ): Ctx[NotChecked] =
            OnEvent(trigger, fieldOn, Absent, store, epoch, due)

        /** What runs the rules of a field for a [[Trigger]]. */
        final private class OnEvent(
            trigger: Trigger,
            fieldOn: Maybe[Set[Activation.Field]],
            nearest: Maybe[Set[Activation.Field]],
            store: Store,
            epoch: Int,
            due: Unit < Async
        )(using Frame) extends Ctx[NotChecked]:
            def gate[B](key: Key[B], default: Set[Activation.Field], reads: Chunk[Read], value: Any, read: Chunk[Any])(
                check: => Result.Partial[FieldError, B] < Async
            ): Result.Partial[FieldError | NotChecked, B] < Async =
                def run: Result.Partial[FieldError | NotChecked, B] < Async =
                    due.andThen(check).map(r => store.record(key, value, read, r, epoch).andThen(r))
                // A change of `changed`, or of every signal when it is absent: compared with what the rule last judged, or
                // with what the field first knew of the signals while it has judged nothing.
                def onChange(changed: Maybe[Signal[?]]): Result.Partial[FieldError | NotChecked, B] < Async =
                    store.lookup(key).map {
                        case Present(e) if e.holds(value, read) => e.answer[B]
                        case Present(e)                         => if moved(changed, reads, e.read, read) then run else unchecked
                        case Absent                             =>
                            store.baseline(reads, read).map(before => if moved(changed, reads, before, read) then run else unchecked)
                    }
                trigger match
                    case event: Activation.Field =>
                        if nearest.orElse(fieldOn).getOrElse(default).contains(event) then run
                        else
                            store.lookup(key).map {
                                case Present(e) if e.holds(value, read) => e.answer[B]
                                case _                                  => unchecked
                            }
                    case signal: Signal[?] => onChange(Present(signal))
                    case sweep: Sweep      =>
                        sweep match
                            case Sweep.Every  => run
                            case Sweep.Settle => onChange(Absent)
                end match
            end gate

            def within(on: Set[Activation.Field]): Ctx[NotChecked] = OnEvent(trigger, fieldOn, Present(on), store, epoch, due)
        end OnEvent

        private val unchecked: Result.Partial[NotChecked, Nothing] = Result.Failure(notChecked)

        /** Whether a signal a rule rechecks on, `changed` or any when it is absent, holds another value now than it held
          * in `before`.
          */
        private def moved(changed: Maybe[Signal[?]], reads: Chunk[Read], before: Chunk[Any], now: Chunk[Any]): Boolean =
            @scala.annotation.tailrec
            def loop(i: Int): Boolean =
                if i >= reads.size || i >= before.size || i >= now.size then false
                else
                    val r = reads(i)
                    if r.recheck && changed.forall(_ eq r.signal) && !before(i).equals(now(i)) then true
                    else loop(i + 1)
            loop(0)
        end moved
    end Ctx

    /** One field's memory of its rules: per rule, the value it last judged, what it read then and what it found; and per
      * signal it observes, what that held when the observing began. It lives in the field, so a rule value shared by two
      * fields remembers separately for each.
      */
    final private[form] class Store private (
        entries: AtomicRef[Map[Key[?], Store.Entry]],
        subscribed: AtomicRef[Chunk[(Signal[?], Any)]]
    ):

        def lookup(key: Key[?])(using Frame): Maybe[Store.Entry] < Sync =
            entries.get.map(m => Maybe.fromOption(m.get(key)))

        /** Keep `result` as the verdict of `key` for `value` and `read`, unless a newer run stored one already. */
        def record[B](key: Key[B], value: Any, read: Chunk[Any], result: Result.Partial[FieldError, B], epoch: Int)(using
            Frame
        ): Unit < Sync =
            entries.updateAndGet { m =>
                if Maybe.fromOption(m.get(key)).exists(_.epoch > epoch) then m
                else m.updated(key, Store.Entry(value, read, result, epoch))
            }.unit

        /** The field starts observing `signals`: note what each holds now, synchronously, before any value of it is
          * delivered. A change of it is compared with this for a rule that has judged nothing yet.
          */
        def subscribe(signals: Chunk[Signal[?]])(using Frame): Unit < Sync =
            Kyo.foreach(signals)(s => s.current.map(v => (s, v: Any)))
                .map(now => subscribed.updateAndGet(held => held ++ now.filterNot(n => held.exists(_._1 eq n._1))).unit)

        /** What a rule that has judged nothing knew of its signals when it read `read` from `reads`: what each held when the
          * field began observing it, where it does, since a change after that is one the rule has not seen; otherwise what it
          * reads now.
          */
        def baseline(reads: Chunk[Read], read: Chunk[Any])(using Frame): Chunk[Any] < Sync =
            subscribed.get.map(held =>
                if held.isEmpty then read
                else
                    reads.zip(read).map { (r, v) =>
                        val i = held.indexWhere(_._1 eq r.signal)
                        if i >= 0 then held(i)._2 else v
                    }
            )

        /** Forget every verdict: a reset puts the field back to how it loaded. What the observed signals held when the
          * observing began stays, since the observing does.
          */
        def clear(using Frame): Unit < Sync = entries.set(Map.empty)
    end Store

    private[form] object Store:

        /** What a field remembers of one rule: the value it last judged, what it read then, its verdict, and the epoch of
          * the run that stored it.
          */
        final case class Entry(value: Any, read: Chunk[Any], verdict: Result.Partial[FieldError, Any], epoch: Int):

            /** Whether this verdict is about exactly `v` with `r` read. */
            def holds(v: Any, r: Chunk[Any]): Boolean = value.equals(v) && read.equals(r)

            /** The verdict, as the rule that stored it yields it. */
            // The entry under a `Key[B]` was recorded by the rule that key belongs to, whose verdicts are over `B`.
            def answer[B]: Result.Partial[FieldError, B] = verdict.asInstanceOf[Result.Partial[FieldError, B]]
        end Entry

        def init(using Frame): Store < Sync =
            for
                entries    <- AtomicRef.init(Map.empty[Key[?], Entry])
                subscribed <- AtomicRef.init(Chunk.empty[(Signal[?], Any)])
            yield new Store(entries, subscribed)
    end Store
end Rule
