package kyo.uic.form

import kyo.uic.UicTest

/** A form is a mount, and it says so in its type: everything a caller can set on a `UI.mounted` it can set on a form,
  * with no second mount around it to hold the setting.
  */
class FormMountTest extends UicTest:

    // `inline val` + constant-folded `+` keeps every argument to the inline `typeCheck` a compile-time constant.
    private inline val preamble =
        "import kyo.*\nimport kyo.UI.*\nimport kyo.uic.form.*\n"

    "a form made with its translator takes a mount's settings" in {
        typeCheck(
            preamble + """def x(using Frame): UI.Ast.Mounted =
  Form.mountedWith(ErrorTranslator.default)(_ => div()).keyed("UI-1").placeholder(span()).onError(_ => span())"""
        )
    }

    "the key set on a form is the key of the mount that holds its state" in {
        import kyo.*
        import kyo.UI.*
        val form = Form.mountedWith(ErrorTranslator.default)(_ => div()).keyed("UI-1")
        assert(form.key.map(_.toString) == Present("UI-1"))
    }

    "a form whose translator comes from Env takes a mount's settings once the translator is there" in {
        typeCheck(
            preamble + """def x(using Frame): UI.Ast.Mounted < Env[ErrorTranslator] =
  Form.mounted()(_ => div()).map(_.keyed("UI-1"))"""
        )
    }
end FormMountTest
