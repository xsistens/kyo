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

    "the refined type is what the chain says" in {
        typeCheck(preamble + "def r(using Frame): Rule[Maybe[Int], Int] = Validator.chosen[Int]() and Validator.min(1)")
        // A chain that refines is not a check over its input any more.
        typeCheckFailure(preamble + "def r(using Frame): Validator[Maybe[Int]] = Validator.chosen[Int]() and Validator.min(1)")
        // And a rule over the value does not chain onto the Maybe it was unwrapped from.
        typeCheckFailure(preamble + "def r(using Frame) = Validator.chosen[Int]() and Validator.ifPresent(Validator.min[Int](1))")
    }
end ValidatorTest
