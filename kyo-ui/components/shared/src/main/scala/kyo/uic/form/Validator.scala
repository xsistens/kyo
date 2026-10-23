package kyo.uic.form

import kyo.*
import scala.util.matching.Regex

/** A rule that checks a value without changing its type: what a field's chain, a cell type and every built-in except
  * [[Validator.chosen]] and [[Validator.ifPresent]] state. The name every existing rule and call site already uses.
  */
type Validator[A] = Rule[A, A]

object Validator:

    /** A check from a function: `Absent` = valid, `Present(e)` = invalid. The value passes through unchanged; for a rule
      * that yields another type, see [[Rule.apply]] and [[refine]].
      */
    def apply[A](f: A => Maybe[FieldError] < Async)(using Frame): Validator[A] = Rule.check(f)

    /** Async escape hatch (identical to [[apply]]; reads clearly at call sites where
      * the body actually suspends, e.g. a mutation). The author folds any `Abort`
      * into a `Maybe[FieldError]` here, keeping the core transport-agnostic.
      */
    def async[A](f: A => Maybe[FieldError] < Async)(using Frame): Validator[A] = Rule.check(f)

    /** No rules: every value passes. The default of a field that only holds a value:
      * `Form.field` starts every spec here, so a caller declaring no rules writes nothing.
      *
      * It is also the identity of [[Rule.and]], which is what `all()` over an empty sequence
      * already returned; that spelling was the only way to say "no rules" and read as an
      * accident. Needs no `Frame`, so it can be the default of a PURE field spec.
      */
    def none[A]: Validator[A] = Rule.pure(a => Result.Success(a))

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
        Rule[Maybe[A], Maybe[B]] {
            case Present(a) =>
                r.run(a).map(_.foldPartial[Result.Partial[FieldError, Maybe[B]]](b => Result.Success(Present(b)), e => Result.Failure(e)))
            case Absent => Result.Success(Absent)
        }

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

    // ---- built-in cross-field rule --------------------------------------------

    /** The value must equal the current value of `other`: the password-confirmation
      * pattern, generalized. Reads `other` live at validation time; pair the field with
      * `field.dependsOn(other)` for as-you-type re-validation when the other field changes.
      */
    def matchesField[A](other: Signal[A], code: String = "must-match")(using CanEqual[A, A], Frame): Validator[A] =
        Validator(a => other.currentWith(o => (if a == o then Absent else Present(FieldError(code))): Maybe[FieldError]))
end Validator
