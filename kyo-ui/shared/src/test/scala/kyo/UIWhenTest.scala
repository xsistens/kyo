package kyo

import kyo.UI.*
import kyo.UI.Ast.*

/** `Element.when`: a setter under a condition, or over an optional value, kept in the chain. */
class UIWhenTest extends kyo.test.Test[Any]:

    "when(true) applies the setter and keeps the element's type" in {
        val b: Button = UI.button("x").when(true)(_.id("on"))
        assert(b.attrs.identifier == Present("on"), s"${b.attrs}")
    }

    "when(false) leaves the element as it is" in {
        val el = UI.button("x")
        assert(el.when(false)(_.id("on")) eq el)
    }

    "when(Present) hands the setter the value" in {
        val name: Maybe[String] = Present("Save")
        val b                   = UI.button("x").when(name)(_.aria("label", _))
        assert(b.attrs.ariaAttrs.get("label").contains("Save"), s"${b.attrs}")
    }

    "when(Absent) leaves the element as it is" in {
        val el                  = UI.button("x")
        val name: Maybe[String] = Absent
        assert(el.when(name)(_.aria("label", _)) eq el)
    }

    "when chains: each condition decides its own setter" in {
        val b = UI.div.when(true)(_.cssClass("a")).when(false)(_.cssClass("b")).when(Present("c"))(_.cssClass(_))
        assert(b.attrs.cssClasses == Chunk("a", "c"), s"${b.attrs.cssClasses}")
    }
end UIWhenTest
