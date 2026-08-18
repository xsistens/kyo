package kyo.uic.form

import kyo.*
import scala.util.matching.Regex

/** A validator maps a value to an optional error in the one effect a mounted event
  * handler actually runs in: `Async`. A pure/sync rule widens for free; an async
  * rule (a kyo-apollo mutation, a REST call, anything) discharges its OWN failure
  * channel into a [[FieldError]] BEFORE it becomes a `Validator` — so the core
  * never depends on any transport library. Failure-as-error, not failure-as-Abort:
  * `Absent` = valid, `Present(e)` = invalid.
  */
opaque type Validator[-A] = A => Maybe[FieldError] < Async

object Validator:

    def apply[A](f: A => Maybe[FieldError] < Async): Validator[A] = f

    /** Async escape hatch (identical to [[apply]]; reads clearly at call sites where
      * the body actually suspends, e.g. a mutation). The author folds any `Abort`
      * into a `Maybe[FieldError]` here, keeping the core transport-agnostic.
      */
    def async[A](f: A => Maybe[FieldError] < Async): Validator[A] = f

    extension [A](self: Validator[A])
        private[form] def run(a: A)(using Frame): Maybe[FieldError] < Async = self(a)

        /** Ordered short-circuit: run `self`; on the first failure stop and report it,
          * otherwise run `next`. A field shows only ONE error at a time — the first
          * open rule of the chain (e.g. `required and minLength(5)` shows "required"
          * until the field is non-empty, then "min-length").
          */
        infix def and(next: Validator[A])(using Frame): Validator[A] =
            a =>
                self(a).map {
                    case p @ Present(_) => (p: Maybe[FieldError])
                    case Absent         => next(a)
                }
    end extension

    /** Chains validators left-to-right with the ordered short-circuit of [[and]]. */
    def all[A](vs: Validator[A]*)(using Frame): Validator[A] =
        if vs.isEmpty then (_ => (Absent: Maybe[FieldError])) else vs.reduce(_ and _)

    /** A rule that applies only when `cond` holds — the escape hatch for
      * "rule depends on a condition / on other form state".
      */
    def when[A](cond: A => Boolean)(v: Validator[A]): Validator[A] =
        a => if cond(a) then v(a) else (Absent: Maybe[FieldError])

    /** A pure predicate rule: fail with `code`(+`args`) when `p` is false. */
    def satisfy[A](code: String, args: Map[String, String] = Map.empty)(p: A => Boolean): Validator[A] =
        a => (if p(a) then Absent else Present(FieldError(code, args))): Maybe[FieldError]

    // ---- built-in String rules ------------------------------------------------

    def required(code: String = "required"): Validator[String] =
        satisfy(code)(_.trim.nonEmpty)

    def minLength(n: Int, code: String = "min-length"): Validator[String] =
        satisfy(code, Map("min" -> n.toString))(_.trim.length >= n)

    def maxLength(n: Int, code: String = "max-length"): Validator[String] =
        satisfy(code, Map("max" -> n.toString))(_.trim.length <= n)

    private val EmailRx = """^[^@\s]+@[^@\s]+\.[^@\s]+$""".r

    /** Empty passes (so `required` owns emptiness — chain them). */
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

    /** The value must equal the current value of `other` — the password-confirmation
      * pattern, generalized. Reads `other` live at validation time; pair the field with
      * `field.dependsOn(other)` for as-you-type re-validation when the other field changes.
      */
    def matchesField[A](other: Signal[A], code: String = "must-match")(using CanEqual[A, A], Frame): Validator[A] =
        a => other.currentWith(o => (if a == o then Absent else Present(FieldError(code))): Maybe[FieldError])
end Validator
