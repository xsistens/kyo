package kyo

import kyo.UI.*
import kyo.UI.Ast.*
import kyo.internal.HtmlRenderer
import scala.compiletime.testing.typeCheckErrors

class AriaKeyTest extends kyo.test.Test[Any]:

    private def renderHtml(ui: UI)(using Frame): String < Sync =
        HtmlRenderer.render(ui, Seq.empty)

    private inline def compileErrors(inline code: String): List[String] =
        typeCheckErrors(code).map(_.message)

    "aria interpolator writes the attribute its literal names" in {
        renderHtml(UI.span.aria(aria"aria-x" -> true)).map { s =>
            assert(s.contains("""aria-x="true""""), s)
        }
    }

    "Aria.disabled writes aria-disabled, the same attribute as the literal aria-disabled" in {
        for
            byConstant <- renderHtml(UI.span.aria(Aria.disabled -> true))
            byLiteral  <- renderHtml(UI.span.aria(aria"aria-disabled" -> true))
        yield assert(
            byConstant == byLiteral && byConstant.contains("""aria-disabled="true""""),
            s"constant: $byConstant, literal: $byLiteral"
        )
    }

    "a dynamic part builds the key when it runs" in {
        val n = 3
        renderHtml(UI.span.aria(aria"aria-item-$n" -> true)).map { s =>
            assert(s.contains("""aria-item-3="true""""), s)
        }
    }

    "a dynamic part that makes an invalid name is refused when the key is built" in {
        val n       = "Bad"
        val refused = scala.util.Try(aria"aria-item-$n").failed.toOption
        assert(refused.exists(_.isInstanceOf[IllegalArgumentException]), s"refused: $refused")
    }

    "a plain string is not a key, so an aria entry named by a string does not compile" in {
        val errors = compileErrors("""
            import kyo.*
            UI.span.aria("label" -> true)
            """)
        assert(errors.nonEmpty, "a string name compiled")
    }

    "a literal without the aria- prefix fails to compile, and the message names the prefix" in {
        val errors = compileErrors("""
            import kyo.*
            val key: Aria = aria"label"
            """)
        assert(errors.exists(_.contains("must start with 'aria-'")), s"errors: $errors")
    }

    "an uppercase letter in a static part fails to compile" in {
        val errors = compileErrors("""
            import kyo.*
            val key: Aria = aria"aria-Label"
            """)
        assert(errors.exists(_.contains("lowercase letters, digits and hyphens only")), s"errors: $errors")
    }

    "a name that starts with a digit fails to compile" in {
        val errors = compileErrors("""
            import kyo.*
            val key: Aria = aria"aria-1x"
            """)
        assert(errors.exists(_.contains("starts with a lowercase letter")), s"errors: $errors")
    }

    "an aria literal with no name after the prefix fails to compile" in {
        val errors = compileErrors("""
            import kyo.*
            val key: Aria = aria"aria-"
            """)
        assert(errors.exists(_.contains("needs a name after its prefix")), s"errors: $errors")
    }

    "a valid literal compiles without an error" in {
        val errors = compileErrors("""
            import kyo.*
            val key: Aria = aria"aria-valuenow"
            """)
        assert(errors.isEmpty, s"errors: $errors")
    }

    "a data key given to aria(...) is refused, not written under aria-" in {
        val refused = scala.util.Try(UI.span.aria(data"data-x" -> true)).failed.toOption
        assert(refused.exists(e => e.isInstanceOf[IllegalArgumentException] && e.getMessage.contains("data key")), s"refused: $refused")
    }

    "an Aria given to data(...) is refused with the kind it has" in {
        val refused = scala.util.Try(UI.span.data(Aria.label -> true)).failed.toOption
        assert(refused.exists(e => e.isInstanceOf[IllegalArgumentException] && e.getMessage.contains("aria key")), s"refused: $refused")
    }

    "a key and a literal in one call write both attributes in order" in {
        renderHtml(UI.span.aria(Aria.label -> "Save", Aria.level -> 2)).map { s =>
            assert(s.contains("""aria-label="Save"""") && s.contains("""aria-level="2""""), s)
        }
    }
end AriaKeyTest
