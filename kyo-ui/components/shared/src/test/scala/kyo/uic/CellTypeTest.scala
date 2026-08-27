package kyo.uic

import kyo.*
import kyo.uic.form.FieldError
import kyo.uic.form.Validator

/** The value half of cell editing: what a cell type prints, what it reads back, and what
  * it refuses. What the editors it carries look like is pinned in GoldenRenderTest.
  */
class CellTypeTest extends UicTest:

    private def parsed[V](ct: CellType[V], raw: String): Result[FieldError, V] = ct.parse(raw)

    private def failure[V](ct: CellType[V], raw: String): FieldError =
        parsed(ct, raw) match
            case Result.Failure(e) => e
            case other             => throw new AssertionError(s"expected a refusal, got $other")

    "a value prints and reads back as itself" in {
        assert(parsed(CellType.string, "Ada") == Result.succeed("Ada"))
        assert(parsed(CellType.int, "42") == Result.succeed(42))
        assert(parsed(CellType.long, "42") == Result.succeed(42L))
        assert(parsed(CellType.double, "1.5") == Result.succeed(1.5))
        assert(parsed(CellType.bigDecimal, "1.5") == Result.succeed(BigDecimal("1.5")))
        assert(parsed(CellType.boolean, "true") == Result.succeed(true))
        assert(CellType.int.format(42) == "42")
        assert(CellType.boolean.format(false) == "false")
    }

    // The editor writes what the parser reads: a stepper on a whole-valued field produces
    // `5`, never `5.0`, which is the one place the two halves could have disagreed.
    "a whole number prints without a decimal point, so an Int column round-trips its editor" in {
        assert(CellEditor.numberText(5.0) == "5")
        assert(CellEditor.numberText(-5.0) == "-5")
        assert(CellEditor.numberText(5.5) == "5.5")
        assert(parsed(CellType.int, CellEditor.numberText(5.0)) == Result.succeed(5))
        assert(CellType.double.format(5.0) == "5")
    }

    "surrounding space is not a refusal" in {
        assert(parsed(CellType.int, "  42  ") == Result.succeed(42))
        assert(parsed(CellType.boolean, " TRUE ") == Result.succeed(true))
    }

    "text that is not a value is refused, with the code and the text that caused it" in {
        val e = failure(CellType.int, "nope")
        assert(e.code == "integer")
        assert(e.args == Map("value" -> "nope"))
        assert(e.fallback == Present("nope is not a whole number"))
        assert(failure(CellType.double, "nope").code == "number")
        assert(failure(CellType.boolean, "maybe").code == "boolean")
        // An Int column refuses a decimal rather than silently truncating it.
        assert(failure(CellType.int, "1.5").code == "integer")
    }

    "a BigDecimal is built from the text, not from a Double, so nothing is rounded on the way in" in {
        assert(parsed(CellType.bigDecimal, "0.1") == Result.succeed(BigDecimal("0.1")))
        assert(parsed(CellType.bigDecimal, "12345678901234567890.5") == Result.succeed(BigDecimal("12345678901234567890.5")))
    }

    "CellType.of maps a value to its label and back, and refuses a label it has no value for" in {
        enum Size derives CanEqual:
            case Small, Large
        val ct = CellType.of(Size.values.toSeq)(_.toString)
        assert(ct.format(Size.Large) == "Large")
        assert(parsed(ct, "Large") == Result.succeed(Size.Large))
        val e = failure(ct, "Huge")
        assert(e.code == "unknown-option")
        assert(e.fallback == Present("Huge is not one of the options"))
    }

    "a rule runs over the parsed value, and passes everything by default" in {
        for
            open <- CellType.int.check.run(3)
            gated = CellType.int.validate(Validator.min(10))
            low  <- gated.check.run(3)
            high <- gated.check.run(30)
        yield
            assert(open == Absent, "a type with no rule refuses nothing")
            assert(low.exists(_.code == "min"), "and one with a rule reports it by code")
            assert(high == Absent)
    }

    "withEditor keeps the value domain and swaps only what edits it" in {
        val custom = CellType.int.withEditor(CellEditor.text)
        assert(parsed(custom, "7") == Result.succeed(7))
        assert(custom.format(7) == "7")
    }

end CellTypeTest
