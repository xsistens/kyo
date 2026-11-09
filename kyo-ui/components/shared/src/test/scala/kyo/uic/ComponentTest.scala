package kyo.uic

import kyo.*

/** A `HasSetter` is found for a setter `Setters` generated, from the component's `Fields`, and for a member a trait declares, from
  * that trait's companion; generic code (a form's `bind`) writes through it without knowing which.
  */
class ComponentTest extends UicTest:

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

end ComponentTest
