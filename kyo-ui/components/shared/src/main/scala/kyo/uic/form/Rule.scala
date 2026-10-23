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
  * Contravariant in what it checks, covariant in what it yields: a rule over `Any` checks a `String`, and a rule yielding a
  * `LocalDate` yields an `AnyRef`.
  */
opaque type Rule[-A, +B] = A => Result.Partial[FieldError, B] < Async

object Rule:

    /** A rule from its function: `Result.Success(b)` passes and yields `b`, `Result.Failure(e)` fails with `e`. The
      * function may suspend (a server round-trip); fold its own failures into a [[FieldError]] here, which keeps this
      * layer free of any transport. [[Validator.refine]] is the pure shorthand for the common "parse or fail with a code".
      */
    def apply[A, B](f: A => Result.Partial[FieldError, B] < Async): Rule[A, B] = f

    /** A check as a rule yielding its input: `Absent` passes, `Present(e)` fails with `e`. */
    private[form] def check[A](f: A => Maybe[FieldError] < Async)(using Frame): Rule[A, A] =
        a =>
            f(a).map {
                case Present(e) => (Result.Failure(e): Result.Partial[FieldError, A])
                case Absent     => Result.Success(a)
            }

    /** A pure rule, built without suspending, so it needs no `Frame`: the shape of every predicate built-in, which is
      * what lets [[Validator.none]] be the default of a pure field spec.
      */
    private[form] def pure[A, B](f: A => Result.Partial[FieldError, B]): Rule[A, B] = a => f(a)

    /** [[Validator.when]]'s body, here because only the companion sees the rule as the function it is. */
    private[form] def guarded[A](cond: A => Boolean)(v: Rule[A, A]): Rule[A, A] =
        a => if cond(a) then v(a) else Result.Success(a)

    extension [A, B](self: Rule[A, B])
        // `private[uic]` and not `private[form]`: a form field was the first host of these rules and is no longer the
        // only one. A DataTable cell runs the same chain over the value its column parsed, and the opaque type is what
        // keeps the function itself out of reach either way.
        private[uic] def run(a: A)(using Frame): Result.Partial[FieldError, B] < Async = self(a)

        /** Ordered short-circuit with refinement: run `self`; on a failure stop and report it, otherwise run `next` on what
          * `self` yielded. A field shows only ONE error at a time, the first open rule of the chain (`required and
          * minLength(5)` shows "required" until the field is non-empty, then "min-length"), and the chain's result type is
          * the last rule's: `Rule[A, B] and Rule[B, C]` is a `Rule[A, C]`.
          */
        infix def and[C](next: Rule[B, C])(using Frame): Rule[A, C] =
            a =>
                self(a).map { r =>
                    r.foldPartial[Result.Partial[FieldError, C] < Async](b => next(b), e => Result.Failure(e))
                }

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
            a =>
                self(a).map(r =>
                    r.foldPartial[Result.Partial[FieldError, B]](_ => r, e => Result.Failure(e.copy(fallback = Present(text))))
                )

        /** Runs `self`, then the check `next` on the SAME input, and yields what `self` yielded. How a field appends a rule
          * over its value type after declaration ([[FormField.addRule]]) without losing the type its chain refines to.
          */
        private[form] def andCheck(next: Rule[A, Any])(using Frame): Rule[A, B] =
            a =>
                self(a).map { r =>
                    r.foldPartial[Result.Partial[FieldError, B] < Async](
                        b => next(a).map(n => n.foldPartial[Result.Partial[FieldError, B]](_ => r, e => Result.Failure(e))),
                        e => Result.Failure(e)
                    )
                }
    end extension
end Rule
