package kyo.uic

import kyo.*

/** A form field binds the control of its value type, and nothing else, and a field of another type is reported against the
  * field the control binds. What only some controls of a value type offer, the binding reaches through `HasSetter`, whether
  * `Setters` generated the setter (ToggleButton) or the class wrote it, through the instance in its companion (Select's
  * `selection`, InputNumber's `integer`).
  */
class FormControlTest extends UicTest:

    private inline val preamble = "import kyo.*\nimport kyo.uic\nimport kyo.uic.form.*\n"

    "a generated setter is a HasSetter, which writes through it" in {
        val set = summon[HasSetter[ToggleButton, "checked", UI.Prop[Boolean]]]
        assert(set(uic.ToggleButton(), true).p.checked.bound.exists(_.equals(true)))
    }

    "a generated callback setter is a HasSetter too" in {
        val set = summon[HasSetter[ToggleButton, "onBlur", Boolean => UI.Action]]
        assert(set(uic.ToggleButton(), _ => Kyo.unit).p.onBlur.isDefined)
    }

    "a member the class writes is a HasSetter from its companion, where a binding asks for it" in {
        val chosen  = summon[HasSetter[Select[String], "selection", UI.Prop[String]]]
        val integer = summon[HasSetter[InputNumber, "integer", Boolean]]
        assert(chosen(uic.Select[String](), "a").p.choice.isDefined)
        assert(integer(uic.InputNumber(), true).p.integerFlag)
    }

    "a choice binds a field that always holds an option, and a field that may stay empty" in {
        typeCheck(preamble + """def x(f: FormField[String])(using Frame): uic.Select[String] = uic.Select[String]().bind(f)""")
        typeCheck(preamble + """def x(f: FormField[Maybe[String]])(using Frame): uic.Select[String] = uic.Select[String]().bind(f)""")
        typeCheckFailure(preamble + """def x(f: FormField[Boolean])(using Frame) = uic.Select[String]().bind(f)""")(
            "kyo.uic.Select[String] binds a FormField[Maybe[String]], or a FormField[String] where it has a `selection` over String; this field is a FormField[Boolean]."
        )
        // A date picker holds a Maybe too, and has no `selection`: a field that always holds a date is not its field.
        typeCheck(
            preamble +
                """import kyo.uic.DateKind.given
def x(f: FormField[Maybe[java.time.LocalDate]])(using Frame): uic.DatePicker[java.time.LocalDate] = uic.DatePicker[java.time.LocalDate]().bind(f)"""
        )
        typeCheckFailure(
            preamble + """import kyo.uic.DateKind.given
def x(f: FormField[java.time.LocalDate])(using Frame) = uic.DatePicker[java.time.LocalDate]().bind(f)"""
        )(
            "kyo.uic.DatePicker[java.time.LocalDate] binds a FormField[Maybe[java.time.LocalDate]], or a FormField[java.time.LocalDate] where it has a `selection` over java.time.LocalDate; this field is a FormField[java.time.LocalDate]."
        )
    }

    "a field offered to a control of another value type names the field the control binds" in {
        typeCheckFailure(preamble + """def x(f: FormField[String])(using Frame) = uic.CheckBox().bind(f)""")(
            "Found:    (f : kyo.uic.form.FormField[String])\nRequired: kyo.uic.form.FormField[Boolean]"
        )
        typeCheckFailure(preamble + """def x(f: FormField[String])(using Frame) = uic.ToggleButton().bind(f)""")(
            "Found:    (f : kyo.uic.form.FormField[String])\nRequired: kyo.uic.form.FormField[Boolean]"
        )
        typeCheckFailure(preamble + """def x(f: FormField[Boolean])(using Frame) = uic.Input().bind(f)""")(
            "Found:    (f : kyo.uic.form.FormField[Boolean])\nRequired: kyo.uic.form.FormField[String]"
        )
        typeCheckFailure(preamble + """def x(f: FormField[Boolean])(using Frame) = uic.TextArea().bind(f)""")(
            "Found:    (f : kyo.uic.form.FormField[Boolean])\nRequired: kyo.uic.form.FormField[String]"
        )
    }

    "no setter of that name, or of another type, is no HasSetter" in {
        typeCheckFailure("""summon[kyo.uic.HasSetter[kyo.uic.ToggleButton, "nope", kyo.UI.Prop[Boolean]]]""")
        typeCheckFailure("""summon[kyo.uic.HasSetter[kyo.uic.ToggleButton, "checked", kyo.UI.Prop[String]]]""")
    }

end FormControlTest
