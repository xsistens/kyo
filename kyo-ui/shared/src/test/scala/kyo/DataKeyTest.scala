package kyo

import kyo.UI.*
import kyo.internal.HtmlRenderer
import scala.compiletime.testing.typeCheckErrors

class DataKeyTest extends kyo.test.Test[Any]:

    private def renderHtml(ui: UI)(using Frame): String < Sync =
        HtmlRenderer.render(ui, Seq.empty)

    private inline def compileErrors(inline code: String): List[String] =
        typeCheckErrors(code).map(_.message)

    "data interpolator writes the attribute its literal names" in {
        renderHtml(UI.span.data(data"data-vs-first" -> 4)).map { s =>
            assert(s.contains("""data-vs-first="4""""), s)
        }
    }

    "a dynamic part builds the key when it runs" in {
        val n = 7
        renderHtml(UI.span.data(data"data-item-$n" -> "x")).map { s =>
            assert(s.contains("""data-item-7="x""""), s)
        }
    }

    "a dynamic part that makes a kyo- name is refused when the key is built" in {
        val n       = "kyo-x"
        val refused = scala.util.Try(data"data-$n").failed.toOption
        assert(refused.exists(_.isInstanceOf[IllegalArgumentException]), s"refused: $refused")
    }

    "the reserved-prefix check of the data kind refuses a kyo- name" in {
        val refused = scala.util.Try(UI.Ast.AttrKind.Data.check("kyo-x")).failed.toOption
        assert(refused.exists(_.isInstanceOf[IllegalArgumentException]), s"refused: $refused")
    }

    "a literal that starts with kyo- fails to compile, and the message names the reserved prefix" in {
        val errors = compileErrors("""
            import kyo.*
            val key: Data = data"data-kyo-x"
            """)
        assert(errors.exists(_.contains("must not start with 'kyo-'")), s"errors: $errors")
    }

    "an uppercase letter in a data literal fails to compile" in {
        val errors = compileErrors("""
            import kyo.*
            val key: Data = data"data-Item"
            """)
        assert(errors.exists(_.contains("lowercase letters, digits and hyphens only")), s"errors: $errors")
    }

    "a data literal without the data- prefix fails to compile, and the message names the prefix" in {
        val errors = compileErrors("""
            import kyo.*
            val key: Data = data"vs-first"
            """)
        assert(errors.exists(_.contains("must start with 'data-'")), s"errors: $errors")
    }

    "a valid data literal compiles without an error" in {
        val errors = compileErrors("""
            import kyo.*
            val key: Data = data"data-vs-first"
            """)
        assert(errors.isEmpty, s"errors: $errors")
    }

    "an aria key given to data(...) is refused, not written under data-" in {
        val refused = scala.util.Try(UI.span.data(Aria.label -> true)).failed.toOption
        assert(refused.exists(e => e.isInstanceOf[IllegalArgumentException] && e.getMessage.contains("aria key")), s"refused: $refused")
    }
end DataKeyTest
