package kyo.uic

import kyo.*

/** The form layer finds a component's `checked` and `onBlur` through `HasSetter`, whether `Setters` generated them (ToggleButton) or the
  * class wrote them as members `BooleanFormControl` asks for (CheckBox, through the trait's companion).
  */
class FormControlTest extends UicTest:

    "a generated setter is a HasSetter, which writes through it" in {
        val set = summon[HasSetter[ToggleButton, "checked", UI.Prop[Boolean]]]
        assert(set(uic.ToggleButton(), true).p.checked.bound.exists(_.equals(true)))
    }

    "a generated callback setter is a HasSetter too" in {
        val set = summon[HasSetter[ToggleButton, "onBlur", Boolean => UI.Action]]
        assert(set(uic.ToggleButton(), _ => Kyo.unit).p.onBlur.isDefined)
    }

    "a member a trait declares is a HasSetter, from the trait" in {
        val set = summon[HasSetter[CheckBox, "checked", UI.Prop[Boolean]]]
        assert(set(uic.CheckBox(), true).p.checkedSlot.bound.exists(_.equals(true)))
    }

    "no setter of that name, or of another type, is no HasSetter" in {
        typeCheckFailure("""summon[kyo.uic.HasSetter[kyo.uic.ToggleButton, "nope", kyo.UI.Prop[Boolean]]]""")
        typeCheckFailure("""summon[kyo.uic.HasSetter[kyo.uic.ToggleButton, "checked", kyo.UI.Prop[String]]]""")
    }

end FormControlTest
