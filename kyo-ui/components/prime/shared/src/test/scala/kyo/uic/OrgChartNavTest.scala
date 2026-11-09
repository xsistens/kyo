package kyo.uic

import kyo.*
import kyo.UI.Keyboard

/** [[OrgChartNav]]: where each key moves an [[OrganizationChart]]'s node cursor, over the chart as it is drawn. */
class OrgChartNavTest extends UicTest:

    private val chart = OrgChartNode(
        "CEO",
        "ceo",
        children = List(
            OrgChartNode("CFO", "cfo", children = List(OrgChartNode("Accounts", "acc"))),
            OrgChartNode("CTO", "cto", children = List(OrgChartNode("Dev", "dev"), OrgChartNode("Ops", "ops")))
        )
    )

    private val open: String => Boolean = _ => true

    private def from(at: String, key: Keyboard, isOpen: String => Boolean = open): Maybe[String] =
        OrgChartNav.onKey(chart, isOpen, at, key)

    "ArrowDown steps to the first child of an open node" in {
        assert(from("ceo", Keyboard.ArrowDown) == Present("cfo"))
        assert(from("cto", Keyboard.ArrowDown) == Present("dev"))
    }

    "ArrowDown from a closed node or a leaf has nowhere to go" in {
        assert(from("cto", Keyboard.ArrowDown, _ != "cto") == Absent, "the children of a closed node are not on the screen")
        assert(from("dev", Keyboard.ArrowDown) == Absent)
    }

    "ArrowUp steps to the parent, and the root has none" in {
        assert(from("ops", Keyboard.ArrowUp) == Present("cto"))
        assert(from("ceo", Keyboard.ArrowUp) == Absent)
    }

    "ArrowLeft and ArrowRight step between siblings and stop at the ends" in {
        assert(from("dev", Keyboard.ArrowRight) == Present("ops"))
        assert(from("ops", Keyboard.ArrowLeft) == Present("dev"))
        assert(from("ops", Keyboard.ArrowRight) == Absent, "no wrap to the first sibling")
        assert(from("cfo", Keyboard.ArrowLeft) == Absent)
        assert(from("acc", Keyboard.ArrowRight) == Absent, "a cousin is not a sibling")
    }

    "Home is the root, End the last node on the screen" in {
        assert(from("dev", Keyboard.Home) == Present("ceo"))
        assert(from("ceo", Keyboard.End) == Present("ops"))
        assert(from("ceo", Keyboard.End, _ != "cto") == Present("cto"), "a closed node hides its children from End")
        assert(from("ceo", Keyboard.Home) == Absent, "already there")
    }

    "a key that is not an arrow, Home or End is not the map's, nor is a node the chart does not have" in {
        assert(from("ceo", Keyboard.Enter) == Absent)
        assert(from("nobody", Keyboard.ArrowDown) == Absent)
    }

end OrgChartNavTest
