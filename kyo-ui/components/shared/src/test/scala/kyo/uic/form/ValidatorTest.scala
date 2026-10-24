package kyo.uic.form

import kyo.*
import kyo.uic.UicTest

/** The rule algebra on its own, with no form around it: what a chain yields, which failure it reports, and what text
  * that failure carries. The form-level consequences (what submit hands over, what a translator shows) are in
  * `FormValidationTest`.
  */
class ValidatorTest extends UicTest:

    // `inline val` keeps every argument to the inline `typeCheck` a compile-time constant.
    private inline val preamble = "import kyo.*\nimport kyo.uic.form.*\n"

    private val digits: Rule[String, Int] = Validator.refine[String, Int]("not-a-number")(s => Maybe.fromOption(s.toIntOption))

    "and refines left to right and reports the first failure" in {
        val age: Rule[String, Int] = Validator.required() and digits and Validator.min(18)
        for
            empty <- age.run("")
            word  <- age.run("old")
            young <- age.run("12")
            adult <- age.run("42")
        yield
            assert(empty.failure.map(_.code) == Present("required"), s"the first rule owns emptiness: $empty")
            assert(word.failure.map(_.code) == Present("not-a-number"), s"the parse fails next: $word")
            assert(young.failure == Present(FieldError("min", Map("min" -> "18"))), s"the rule over the parsed Int runs last: $young")
            assert(adult.value == Present(42), s"what passes is the Int the chain refined to: $adult")
        end for
    }

    "and stops at the first failure: a later rule does not run" in {
        for
            ran <- AtomicInt.init(0)
            counted = Validator.async[String](_ => ran.incrementAndGet.andThen(Absent))
            chain   = Validator.required() and counted
            _      <- chain.run("")
            before <- ran.get
            _      <- chain.run("x")
            after  <- ran.get
        yield
            assert(before == 0, "a failed required keeps the next rule from running")
            assert(after == 1, "a passing required hands over to it")
    }

    "chosen yields the value an optional field was required to hold" in {
        val picked: Rule[Maybe[Int], Int]  = Validator.chosen[Int]()
        val bounded: Rule[Maybe[Int], Int] = Validator.chosen[Int]() and Validator.min(5)
        for
            none <- picked.run(Absent)
            some <- picked.run(Present(3))
            low  <- bounded.run(Present(3))
            high <- bounded.run(Present(7))
        yield
            assert(none.failure.map(_.code) == Present("required"), s"the code stays required: $none")
            assert(some.value == Present(3), s"the Maybe is unwrapped: $some")
            assert(low.failure.map(_.code) == Present("min"), s"a rule chained after chosen judges the value itself: $low")
            assert(high.value == Present(7))
        end for
    }

    "ifPresent passes an empty field and refines inside the Maybe" in {
        val optionalNumber: Rule[Maybe[String], Maybe[Int]] = Validator.ifPresent(digits)
        for
            none <- optionalNumber.run(Absent)
            good <- optionalNumber.run(Present("7"))
            bad  <- optionalNumber.run(Present("seven"))
        yield
            assert(none.value == Present(Absent), s"nothing to judge, nothing required: $none")
            assert(good.value == Present(Present(7)), s"the rule's result rides inside the Maybe: $good")
            assert(bad.failure.map(_.code) == Present("not-a-number"), s"and its failure is the field's: $bad")
        end for
    }

    "message gives the rule that failed its text, and keeps its code and args" in {
        val comment = Validator.required().message("Write something first.") and
            Validator.minLength(3).message("A comment has at least 3 characters.")
        for
            empty <- comment.run("")
            short <- comment.run("ab")
            ok    <- comment.run("abc")
        yield
            assert(empty.failure == Present(FieldError("required", fallback = Present("Write something first."))), s"$empty")
            assert(
                short.failure == Present(FieldError("min-length", Map("min" -> "3"), Present("A comment has at least 3 characters."))),
                s"$short"
            )
            assert(ok.value == Present("abc"))
        end for
    }

    "message on a chain is the text of whichever rule in it fails" in {
        val chain = (Validator.required() and Validator.minLength(3)).message("Too short.")
        for
            empty <- chain.run("")
            short <- chain.run("ab")
        yield
            assert(empty.failure.map(e => (e.code, e.fallback)) == Present(("required", Present("Too short."))))
            assert(short.failure.map(e => (e.code, e.fallback)) == Present(("min-length", Present("Too short."))))
        end for
    }

    "Rule.apply builds an async refining rule from a Result" in {
        val positive: Rule[Int, Long] =
            Rule(n => if n > 0 then Result.Success(n.toLong) else Result.Failure(FieldError("not-positive")))
        for
            yes <- positive.run(4)
            no  <- positive.run(-1)
        yield
            assert(yes.value == Present(4L))
            assert(no.failure.map(_.code) == Present("not-positive"))
        end for
    }

    "the check-only combinators still yield their input" in {
        for
            none  <- Validator.none[String].run("as is")
            all   <- Validator.all[String]().run("as is")
            skip  <- Validator.when[String](_.startsWith("#"))(Validator.minLength(5)).run("ab")
            apply <- Validator.when[String](_.startsWith("#"))(Validator.minLength(5)).run("#ab")
        yield
            assert(none.value == Present("as is"))
            assert(all.value == Present("as is"))
            assert(skip.value == Present("ab"), "a rule whose condition does not hold passes the value through")
            assert(apply.failure.map(_.code) == Present("min-length"))
        end for
    }

    // `run` is how a rule is checked outside a field (a DataTable cell's commit): a submit, which runs every rule.
    "run checks every rule, whatever activation it waits for" in {
        for
            runs <- AtomicInt.init(0)
            chain = Validator.async[String](_ => runs.incrementAndGet.andThen(Absent)).on(Activation.Blur) and
                Validator.satisfy[String]("never")(_ => false).on(Activation.Submit)
            res <- chain.run("a")
            n   <- runs.get
        yield
            assert(n == 1, s"the rule on(Blur) ran: $n")
            assert(res.failure.map(_.code) == Present("never"), s"and so did the one on(Submit): $res")
    }

    "the refined type is what the chain says" in {
        typeCheck(preamble + "def r(using Frame): Rule[Maybe[Int], Int] = Validator.chosen[Int]() and Validator.min(1)")
        // A chain that refines is not a check over its input any more.
        typeCheckFailure(preamble + "def r(using Frame): Validator[Maybe[Int]] = Validator.chosen[Int]() and Validator.min(1)")
        // And a rule over the value does not chain onto the Maybe it was unwrapped from.
        typeCheckFailure(preamble + "def r(using Frame) = Validator.chosen[Int]() and Validator.ifPresent(Validator.min[Int](1))")
    }

    // ---- rules that read another field ---------------------------------------------------------

    "a reading rule receives the dependencies' values at each run" in {
        for
            (low, high, both) <-
                for
                    min  <- Signal.initRef(3)
                    max  <- Signal.initRef(5)
                    unit <- Signal.initRef("cm")
                    length = Validator.reading(min)((s: String, n: Int) =>
                        if s.length >= n then Absent else Present(FieldError("too-short", Map("min" -> n.toString)))
                    )
                    range = Validator.reading(Dep.recheck(min), Dep.discard(max), Dep.recheck(unit))((s: String, d: (Int, Int, String)) =>
                        Present(FieldError("range", Map("range" -> s"${d._1}..${d._2} ${d._3}")))
                    )
                    l <- length.run("ab")
                    _ <- min.set(2)
                    h <- length.run("ab")
                    _ <- max.set(9)
                    b <- range.run("ab")
                yield (l.failure, h.failure, b.failure.map(_.args))
        yield
            assert(low == Present(FieldError("too-short", Map("min" -> "3"))), s"the rule read the first value: $low")
            assert(high == Absent, s"and the next run reads the new one: $high")
            assert(both == Present(Map("range" -> "2..9 cm")), s"three dependencies arrive as one tuple, in order: $both")
        end for
    }

    "Rule.reading refines, and hands what it yielded down the chain" in {
        for
            (eight, tooBig, nine) <-
                for
                    offset <- Signal.initRef(3)
                    shifted = Rule.reading(offset)((s: String, o: Int) =>
                        Maybe.fromOption(s.toIntOption) match
                            case Present(n) => (Result.Success(n + o): Result.Partial[FieldError, Int])
                            case Absent     => Result.Failure(FieldError("not-a-number"))
                    ) and Validator.max(10)
                    e <- shifted.run("5")
                    t <- shifted.run("9")
                    _ <- offset.set(0)
                    n <- shifted.run("9")
                yield (e.value, t.failure.map(_.code), n.value)
        yield
            assert(eight == Present(8), s"the rule yields the parsed value plus what it read: $eight")
            assert(tooBig == Present("max"), s"the rule after it judges what it yielded, 12: $tooBig")
            assert(nine == Present(9), s"and a new dependency value reaches the next run: $nine")
        end for
    }

    "reading takes a signal or a Dep for one dependency, and Deps for several" in {
        typeCheck(preamble + "def r(s: Signal[Int])(using Frame): Validator[String] = Validator.reading(s)((a: String, n: Int) => Absent)")
        typeCheck(preamble +
            "def r(s: Signal[Int])(using Frame): Validator[String] = Validator.reading(Dep.discard(s))((a: String, n: Int) => Absent)")
        typeCheck(
            preamble + "def r(s: Signal[Int])(using Frame): Validator[String] = Validator.reading(s).async((a: String, n: Int) => Absent)"
        )
        typeCheck(
            preamble + "def r(s: Signal[Int], t: Signal[Long])(using Frame): Rule[String, Long] = " +
                "Rule.reading(Dep.recheck(s), Dep.discard(t))((a: String, d: (Int, Long)) => Result.Success(d._2))"
        )
        // Several dependencies each state their reaction: a bare signal there has none to fall back on.
        typeCheckFailure(
            preamble + "def r(s: Signal[Int], t: Signal[Int])(using Frame) = Validator.reading(s, t)((a: String, d: (Int, Int)) => Absent)"
        )
    }
end ValidatorTest
