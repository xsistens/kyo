package kyo.uic.form

import kyo.*
import scala.util.matching.Regex

/** A rule that checks a value without changing its type: what a field's chain, a cell type and every built-in except
  * [[Validator.chosen]] and [[Validator.ifPresent]] state. The name every existing rule and call site already uses.
  */
type Validator[A] = Rule[A, A]

object Validator:

    /** A check from a function: `Absent` = valid, `Present(e)` = invalid. The value passes through unchanged; for a rule
      * that yields another type, see [[Rule.apply]] and [[refine]]. Runs on every change and on the blur, like every
      * built-in, unless [[Rule.on]] or its field says otherwise.
      */
    def apply[A](f: A => Maybe[FieldError] < Async)(using Frame): Validator[A] = Rule.check(Rule.defaultOn)(f)

    /** A check whose body suspends for real (a mutation, a server round-trip): the author folds any `Abort` into a
      * `Maybe[FieldError]` here, keeping the core transport-agnostic.
      *
      * Unlike [[apply]] it runs on the BLUR by default, not on every change, so typing does not send a request per
      * keystroke: the reader hears about it when they leave the field, and the verdict stays while the value does.
      * `.on(...)` on the rule, or `on` on its field, says otherwise; a submit runs it either way.
      */
    def async[A](f: A => Maybe[FieldError] < Async)(using Frame): Validator[A] = Rule.check(Set(Activation.Blur))(f)

    /** No rules: every value passes. The default of a field that only holds a value:
      * `Form.field` starts every spec here, so a caller declaring no rules writes nothing.
      *
      * It is also the identity of [[Rule.and]], which is what `all()` over an empty sequence
      * already returned; that spelling was the only way to say "no rules" and read as an
      * accident. Needs no `Frame`, so it can be the default of a PURE field spec.
      *
      * Having nothing to check, it runs on every event and is never "not yet checked" ([[Rule.on]]).
      */
    def none[A]: Validator[A] = Rule.identity

    /** Chains checks left-to-right with the ordered short-circuit of [[Rule.and]]. */
    def all[A](vs: Validator[A]*)(using Frame): Validator[A] =
        if vs.isEmpty then none[A] else vs.reduce(_ and _)

    /** A rule that applies only when `cond` holds: the escape hatch for
      * "rule depends on a condition / on other form state". A check, since a value the rule skipped has nothing to refine.
      */
    def when[A](cond: A => Boolean)(v: Validator[A]): Validator[A] =
        Rule.guarded(cond)(v)

    /** A pure predicate rule: fail with `code`(+`args`) when `p` is false. */
    def satisfy[A](code: String, args: Map[String, String] = Map.empty)(p: A => Boolean): Validator[A] =
        Rule.pure(a => if p(a) then Result.Success(a) else Result.Failure(FieldError(code, args)))

    /** A pure refining rule: `f` either yields the refined value or `Absent`, which fails with `code`(+`args`). The
      * refining counterpart of [[satisfy]]: `refine("not-a-number")((s: String) => Maybe.fromOption(s.toIntOption))` is a
      * `Rule[String, Int]`.
      */
    def refine[A, B](code: String, args: Map[String, String] = Map.empty)(f: A => Maybe[B]): Rule[A, B] =
        Rule.pure { a =>
            f(a) match
                case Present(b) => Result.Success(b)
                case Absent     => Result.Failure(FieldError(code, args))
        }

    // ---- built-in String rules ------------------------------------------------

    def required(code: String = "required"): Validator[String] =
        satisfy(code)(_.trim.nonEmpty)

    def minLength(n: Int, code: String = "min-length"): Validator[String] =
        satisfy(code, Map("min" -> n.toString))(_.trim.length >= n)

    def maxLength(n: Int, code: String = "max-length"): Validator[String] =
        satisfy(code, Map("max" -> n.toString))(_.trim.length <= n)

    private val EmailRx = """^[^@\s]+@[^@\s]+\.[^@\s]+$""".r

    /** Empty passes (so `required` owns emptiness; chain them). */
    def email(code: String = "email"): Validator[String] =
        satisfy(code)(v => v.isEmpty || EmailRx.matches(v))

    /** The value must fully match `regex`. Empty passes (chain with [[required]]). Pass a
      * compiled `Regex` (`"[a-z0-9-]+".r`) so it is compiled once, not per validation.
      */
    def pattern(regex: Regex, code: String = "pattern"): Validator[String] =
        satisfy(code)(v => v.isEmpty || regex.matches(v))

    private val UrlRx = """^https?://[^\s/$.?#][^\s]*$""".r

    /** A well-formed `http`/`https` URL. Empty passes (chain with [[required]]). */
    def url(code: String = "url"): Validator[String] =
        satisfy(code)(v => v.isEmpty || UrlRx.matches(v))

    // ---- built-in choice rules ------------------------------------------------

    /** A field that may stay empty must hold a value: the `required` of a `FormField[Maybe[A]]`, whether it holds a choice
      * (a `Select[A]`) or a picked date (a `DatePicker[A]`), under the same code, so one translation covers a text field, a
      * choice and a date alike.
      *
      * It refines: what passes is the `A` the field was required to hold, so a rule chained after it is stated over `A`
      * (`chosen() and Validator.min(cutoff)`), and the field's validated type is `A`, which is what [[Form.submit]] hands
      * the handler.
      */
    def chosen[A](code: String = "required"): Rule[Maybe[A], A] =
        refine(code)(identity)

    /** Runs `r` on the value a `FormField[Maybe[A]]` holds and passes while it holds none, so a rule over the value itself
      * (a date after a cutoff) does not also make the field required. A refining `r` refines inside the `Maybe`. Where the
      * field IS required, chain the rule after [[chosen]] instead, which hands it the value itself.
      */
    def ifPresent[A, B](r: Rule[A, B])(using Frame): Rule[Maybe[A], Maybe[B]] =
        Rule.present(r)

    /** A choice of several must hold at least one option, again under the `required` code.
      *
      * A check, not a refinement: "a set with at least one element" has no type of its own here, and inventing one would
      * hand the submit handler a type no control binds and no other API takes, for a fact the handler can see anyway.
      */
    def chosenAny[A](code: String = "required"): Validator[Set[A]] =
        satisfy(code)(_.nonEmpty)

    // ---- built-in numeric / ordered rules -------------------------------------

    /** Value must be `>= bound`. Works for any ordered type (`Int` / `Long` / `Double`,
      * and dates via a given `Ordering`); the bound is exposed to the translator as `min`.
      */
    def min[A](bound: A, code: String = "min")(using ord: Ordering[A]): Validator[A] =
        satisfy(code, Map("min" -> bound.toString))(a => ord.gteq(a, bound))

    /** Value must be `<= bound` (exposed to the translator as `max`). */
    def max[A](bound: A, code: String = "max")(using ord: Ordering[A]): Validator[A] =
        satisfy(code, Map("max" -> bound.toString))(a => ord.lteq(a, bound))

    // ---- rules that read another field ----------------------------------------

    /** A check that reads `dep` too: `Validator.reading(password)((confirm: String, pw: String) => ...)`. The field it is
      * declared on subscribes to `dep` when the form mounts (or the row is added) and lets it go with the mount, so a
      * change of `dep` reaches the rule with nothing else to wire. A bare signal takes the reaction of the rule built:
      * a change of `dep` reruns [[Reading.apply]] at once and drops the verdict of [[Reading.async]] (see [[Dep]]).
      * The rule receives its own field's value as its first argument, so `dep` is another field's signal, never its own.
      */
    def reading[D](dep: Signal[D])(using Frame): Reading[D] = new Reading(Rule.Source.one(Dep.default(dep)))

    /** [[reading]] with the dependency's reaction stated: `Validator.reading(Dep.discard(country)).async(...)`. */
    def reading[D](dep: Dep[D])(using Frame): Reading[D] = new Reading(Rule.Source.one(dep))

    /** A check that reads two signals, each with its own reaction; the function receives both values as a pair. */
    def reading[D1, D2](d1: Dep[D1], d2: Dep[D2])(using Frame): Reading[(D1, D2)] = new Reading(Rule.Source.two(d1, d2))

    /** A check that reads three signals, each with its own reaction; the function receives the values as a triple. */
    def reading[D1, D2, D3](d1: Dep[D1], d2: Dep[D2], d3: Dep[D3])(using Frame): Reading[(D1, D2, D3)] =
        new Reading(Rule.Source.three(d1, d2, d3))

    /** What [[Validator.reading]] returns: the signals a check reads, waiting for its function. The preferred shape for a
      * rule over another field, because the rule carries what it reads: no `dependsOn` to pair it with, and nothing to
      * forget when the rule moves to another field.
      *
      * The function receives the field's value and the dependencies' values, read once at the start of each run. Those
      * values belong to the input its verdict is kept for, so an event that does not run the rule reuses the verdict only
      * while the field's value and every dependency are the ones it judged; otherwise the rule is not yet checked.
      *
      * A change of a dependency is an event for the field: a rule that reads it with [[Dep.recheck]] runs when the value it
      * reads differs from the one it last judged, unless it has already judged exactly the input it now has, and every
      * other rule keeps or drops its verdict as for any event it is not
      * due for, and the field is not touched, so whether an error shows is still up to its own events and the submit. A rule's [[Rule.on]] governs only the field's own
      * events; the dependency's reaction governs only its changes. A rule not yet checked still stops the chain, so a
      * rule that discards belongs at the end, as every deferred rule does.
      */
    final class Reading[D] private[form] (source: Rule.Source[D]):

        /** A sync check, `(value, read) => Absent` to pass or `Present(e)` to fail. It runs on every change and on the
          * blur like [[Validator.apply]], and a dependency given as a bare signal rechecks it.
          */
        def apply[A](f: (A, D) => Maybe[FieldError])(using Frame): Validator[A] =
            Rule.checkReading(Rule.defaultOn, source, Dep.Reaction.Recheck)((a: A, d: D) => f(a, d))

        /** A check whose body suspends for real. Like [[Validator.async]] it runs on the blur by default, and a
          * dependency given as a bare signal discards its verdict rather than sending a request nobody asked for.
          */
        def async[A](f: (A, D) => Maybe[FieldError] < Async)(using Frame): Validator[A] =
            Rule.checkReading(Set(Activation.Blur), source, Dep.Reaction.Discard)(f)
    end Reading

    // ---- built-in cross-field rule --------------------------------------------

    /** The value must equal the current value of `other`: the password-confirmation pattern, generalized. Built on
      * [[reading]], so it carries its dependency: a change of `other` rechecks it at once, whatever its `on`, and the
      * field needs no `dependsOn`. [[FormField.matches]] is the one-call form.
      */
    def matchesField[A](other: Signal[A], code: String = "must-match")(using CanEqual[A, A], Frame): Validator[A] =
        reading(other)((a: A, o: A) => if a == o then Absent else Present(FieldError(code)))
end Validator
