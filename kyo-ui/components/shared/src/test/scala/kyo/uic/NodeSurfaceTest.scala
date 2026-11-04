package kyo.uicsurface

import kyo.*
import kyo.uic
import kyo.uic.UicTest
import scala.language.implicitConversions

/** Outside `kyo.uic`, on purpose: from here a component shows its setters and what every object has, the way an application
  * meets it, and `private[uic]` holds. Each line of the sweep first checks that the expression itself compiles, or a typo would
  * pass as hidden.
  */
class NodeSurfaceTest extends UicTest:

    private inline val preamble =
        "import kyo.*\nimport kyo.UI.*\nimport kyo.uic\nimport scala.language.implicitConversions\ndef x(using Frame) = "

    private inline def hidden(inline e: String)(using kyo.test.AssertScope): Unit =
        typeCheck(preamble + e)
        typeCheckFailure(preamble + e + ".s")
        typeCheckFailure(preamble + e + "._1")
        typeCheckFailure(preamble + e + ".copy()")
        typeCheckFailure(preamble + e + ".productArity")
        typeCheckFailure(preamble + e + ".productIterator")
        typeCheckFailure(preamble + e + ".productPrefix")
        typeCheckFailure(preamble + e + ".canEqual(1)")
    end hidden

    "the carriers a reader is handed show what a reader needs, and no more" in {
        hidden("""uic.BreadcrumbItem("a")""")
        typeCheck(preamble + """uic.BreadcrumbItem("a").href""")
        typeCheckFailure(preamble + """uic.BreadcrumbItem("a").text""")
        typeCheckFailure(preamble + """uic.BreadcrumbItem("a").target""")
        typeCheckFailure(preamble + """uic.BreadcrumbItem("a").icon""")

        hidden("""uic.MeterGroup.Meter("a", 1.0)""")
        typeCheck(preamble + """uic.MeterGroup.Meter("a", 1.0).labelText""")
        typeCheckFailure(preamble + """uic.MeterGroup.Meter("a", 1.0).label""")
        typeCheckFailure(preamble + """uic.MeterGroup.Meter("a", 1.0).value""")
        typeCheckFailure(preamble + """uic.MeterGroup.Meter("a", 1.0).color""")
        typeCheckFailure(preamble + """uic.MeterGroup.Meter("a", 1.0).icon""")

        hidden("""uic.Theme.brand("a", Nil)""")
        typeCheck(preamble + """uic.Theme.brand("a", Nil).sheet""")
        typeCheck(preamble + """uic.Theme.brand("a", Nil).attr""")
        typeCheckFailure(preamble + """uic.Theme.brand("a", Nil).name""")
        succeed
    }

    "the display components show setters only" in {
        hidden("uic.Avatar()")
        hidden("uic.AvatarGroup()")
        hidden("uic.Badge()")
        hidden("uic.OverlayBadge(UI.empty)")
        hidden("uic.Card()")
        hidden("""uic.Chip("a")""")
        hidden("uic.Divider()")
        hidden("uic.FlexBox()")
        hidden("uic.Icon(uic.Icons.checkCircle)")
        hidden("uic.IconField(uic.Input())")
        hidden("uic.InputGroup()")
        hidden("""uic.Label("a")""")
        hidden("uic.Message()")
        hidden("uic.MeterGroup()")
        hidden("uic.ProgressBar()")
        hidden("uic.ProgressSpinner()")
        hidden("uic.ScrollPanel()")
        hidden("uic.Skeleton()")
        hidden("""uic.Tag("a")""")
        hidden("uic.Text()")
        hidden("uic.Timeline[Int]()")
        hidden("uic.Title()")
        hidden("uic.Toolbar()")
        hidden("""uic.Tooltip("a")""")
        succeed
    }

    "the buttons and the menus show setters only" in {
        hidden("""uic.Button("a")""")
        hidden("uic.Link()")
        hidden("""uic.SplitButton("a")""")
        hidden("uic.SpeedDial()")
        hidden("""uic.MenuItem("a")""")
        hidden("""uic.MenuGroup("a")""")
        hidden("""uic.MegaMenuItem("a")""")
        hidden("uic.Menu()")
        hidden("uic.Menubar()")
        hidden("uic.MegaMenu()")
        hidden("uic.ContextMenu()")
        hidden("uic.TieredMenu()")
        hidden("uic.Breadcrumb()")
        succeed
    }

    "the overlays and the containers show setters only" in {
        hidden("uic.Overlay(true)")
        hidden("uic.Popover()")
        hidden("uic.Dialog()")
        hidden("uic.Drawer()")
        hidden("uic.ConfirmDialog()")
        hidden("uic.Toast()")
        hidden("uic.Inplace()")
        hidden("uic.Fieldset()")
        hidden("uic.Panel()")
        succeed
    }

    "the fields show setters only" in {
        hidden("uic.Input()")
        hidden("uic.TextArea()")
        hidden("uic.Password()")
        hidden("uic.InputMask()")
        hidden("uic.InputOtp()")
        hidden("uic.ColorPicker()")
        hidden("uic.CheckBox()")
        hidden("uic.RadioButton()")
        hidden("uic.ToggleSwitch()")
        hidden("uic.ToggleButton()")
        hidden("uic.Knob()")
        hidden("uic.InputNumber()")
        hidden("uic.Rating()")
        hidden("uic.Slider()")
        hidden("uic.FileUpload()")
        hidden("""uic.FloatLabel(uic.Input(), "a")""")
        hidden("""uic.IftaLabel(uic.Input(), "a")""")
        succeed
    }

    "the pickers and their multiple forms show setters only" in {
        hidden("uic.Select[Int]()")
        hidden("uic.MultiSelect[Int]()")
        hidden("uic.CascadeSelect[Int]()")
        hidden("uic.AutoComplete[Int]()")
        hidden("uic.Listbox[Int]()")
        hidden("uic.TreeSelect[Int]()")
        hidden("uic.SelectButton[Int]()")
        hidden("uic.SelectButton[Int]().multiple")
        hidden("uic.DatePicker[java.time.LocalDate]()")
        hidden("uic.DatePicker[java.time.LocalDate]().values(Set.empty[java.time.LocalDate])")
        hidden("uic.DatePicker[java.time.LocalDate]().range(Maybe.empty[java.time.LocalDate], Maybe.empty[java.time.LocalDate])")
        succeed
    }

    "the structural components and their entries show setters only" in {
        hidden("""uic.Tab("a", UI.empty, 1)""")
        hidden("uic.Tabs[Int]()")
        hidden("""uic.AccordionPanel("a", UI.empty, 1)""")
        hidden("uic.Accordion[Int]()")
        hidden("uic.Accordion[Int]().value(Set.empty[Int])")
        hidden("""uic.Step[Int]("a")()""")
        hidden("uic.Stepper[Int]()")
        hidden("uic.Tree[Int]()")
        hidden("uic.OrganizationChart[Int]()")
        hidden("uic.Galleria()")
        hidden("uic.Carousel[Int]()")
        hidden("uic.VirtualScroller(Seq(1))")
        hidden("uic.DataView[Int]()")
        hidden("uic.Terminal()")
        hidden("uic.PickList[Int]()")
        hidden("uic.OrderList[Int]()")
        hidden("uic.Paginator()")
        succeed
    }

    "the tables and their columns show setters only" in {
        hidden("uic.DataTable[Int]()")
        hidden("uic.TreeTable[Int]()")
        hidden("""uic.Column[Int]("a")""")
        hidden("""uic.HeaderGroup[Int]("a")()""")
        hidden("uic.RowGroup[Int, Int](a => a)")
        succeed
    }

    "two builders of one class holding equal state are equal, as case classes were" in {
        given CanEqual[Any, Any] = CanEqual.derived
        val a: Any               = uic.Tag("a").severity(uic.Severity.Info)
        val same: Any            = uic.Tag("a").severity(uic.Severity.Info)
        val other: Any           = uic.Tag("b").severity(uic.Severity.Info)
        assert(a == same)
        assert(a.hashCode == same.hashCode)
        assert(a != other)
        assert((uic.MenuItem("a"): Any) == (uic.MenuItem("a"): Any))
        assert((uic.Input().size(uic.Size.Small): Any) == (uic.Input().size(uic.Size.Small): Any))
        assert((uic.Input().size(uic.Size.Small): Any) != (uic.Input(): Any))
    }

    "the shared groups take part in the comparison" in {
        given CanEqual[Any, Any] = CanEqual.derived
        assert((uic.Input().id("a"): Any) == (uic.Input().id("a"): Any))
        assert((uic.Input().id("a"): Any) != (uic.Input().id("b"): Any))
        assert((uic.Input().accessibleName("a"): Any) != (uic.Input(): Any))
        assert((uic.Input().invalidMessage("a"): Any) == (uic.Input().invalidMessage("a"): Any))
        assert((uic.Input().invalidMessage("a"): Any) != (uic.Input().invalidMessage("b"): Any))
    }

    "the carriers compare by what they hold too" in {
        given CanEqual[Any, Any] = CanEqual.derived
        assert((uic.BreadcrumbItem("a", Present("/a")): Any) == (uic.BreadcrumbItem("a", Present("/a")): Any))
        assert((uic.BreadcrumbItem("a", Present("/a")): Any) != (uic.BreadcrumbItem("a", Present("/b")): Any))
        assert((uic.MeterGroup.Meter("a", 1.0): Any) == (uic.MeterGroup.Meter("a", 1.0): Any))
        assert((uic.MeterGroup.Meter("a", 1.0): Any) != (uic.MeterGroup.Meter("a", 2.0): Any))
        assert(uic.Theme.brand("a", Nil) == uic.Theme.brand("a", Nil))
        assert(uic.Theme.brand("a", Nil) != uic.Theme.brand("b", Nil))
    }

    "the getters answer what was built" in {
        assert(uic.BreadcrumbItem("a", Present("/a")).href == Present("/a"))
        assert(uic.BreadcrumbItem("a").href == Absent)
        assert(uic.MeterGroup.Meter("a", 1.0).labelText == "a")
        assert(uic.MeterGroup.Meter(Signal.initConst("a"), 1.0).labelText == "")
        assert(uic.Theme.brand("a", Nil).attr == "a")
        assert(uic.Theme.brand("a", Nil).sheet.isEmpty)
    }

    "two classes holding the same component stay different" in {
        given CanEqual[Any, Any] = CanEqual.derived
        val d                    = uic.DatePicker[java.time.LocalDate]()
        val none                 = Maybe.empty[java.time.LocalDate]
        val multi: Any           = d.values(Set.empty[java.time.LocalDate])
        val range: Any           = d.range(none, none)
        assert(multi == (d.values(Set.empty[java.time.LocalDate]): Any))
        assert(multi != range)
        // One group, since a function in a state compares by identity, and two `apply`s are two functions on JS.
        val group = uic.SelectButton[Int]()
        assert((group.multiple: Any) == (group.multiple: Any))
        assert((group.multiple: Any) != (group: Any))
    }

    "a builder prints as what it is" in {
        assert(uic.Divider().toString.startsWith("Divider("))
        assert(uic.Input().toString.startsWith("Input("))
    }
end NodeSurfaceTest
