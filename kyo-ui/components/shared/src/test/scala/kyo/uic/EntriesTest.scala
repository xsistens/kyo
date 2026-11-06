package kyo.uic.test

import kyo.*
import kyo.UI.*
import kyo.uic
import kyo.uic.UicTest
import scala.language.implicitConversions

/** [[kyo.uic.Entry]]: every list setter takes its entries built directly or from `_`, the two give the same model, they mix in one call
  * and a prepared sequence splats in, and `_` offers only the builders of the entry type that setter takes.
  */
class EntriesTest extends UicTest:

    private inline val preamble =
        "import kyo.*\nimport kyo.UI.*\nimport kyo.uic\nimport scala.language.implicitConversions\n"

    private val content: UI = span("content")

    // The entry models are plain case classes compared structurally; nothing here compares across types.
    private given [A]: CanEqual[A, A] = CanEqual.derived

    /** The rows a host keeps as entries, resolved the way its render resolves them. */
    private def rows(es: List[uic.MenuItemLiveEntry])(using Frame): List[uic.MenuItem] =
        uic.MenuItemScope.reading(es) match
            case uic.ReactiveValue.Const(items) => items
            case _                              => throw new AssertionError("entries without a signal were read as live")

    /** The entries a structural component reads, which hold no signal and so read as fixed. */
    private def fixed[A](r: uic.ReactiveValue[List[A]]): List[A] =
        r match
            case uic.ReactiveValue.Const(as) => as
            case _                           => throw new AssertionError("entries without a signal were read as live")

    /** The rows a listbox keeps as sources, resolved the way its region resolves them. */
    private def options(lb: uic.Listbox[String])(using Frame): List[uic.OptionItem[uic.ListItem[String]]] =
        uic.OptionItem.reading(lb.p.optionSources) match
            case uic.ReactiveValue.Const(rows) => rows
            case _                             => throw new AssertionError("sources without a signal were read as live")

    "menu items: every host of MenuItem, and the nested items of an item and a group" in {
        val direct = List(uic.MenuItem("Update").icon(uic.Icons.refresh), uic.MenuItem.separator, uic.MenuItem("Homepage").url("/"))
        val scoped = Seq[uic.MenuItemEntry](
            _.item("Update").icon(uic.Icons.refresh),
            _.item.separator,
            _.item("Homepage").url("/")
        )
        assert(rows(uic.Menu().items(scoped*).p.entriesV) == direct)
        assert(rows(uic.Menubar().items(scoped*).p.entriesV) == direct)
        assert(rows(uic.TieredMenu().items(scoped*).p.entriesV) == direct)
        assert(rows(uic.ContextMenu().items(scoped*).p.entriesV) == direct)
        assert(rows(uic.SplitButton("Save").items(scoped*).p.entriesV) == direct)
        assert(rows(uic.SpeedDial().items(scoped*).p.entriesV) == direct)
        assert(uic.MenuItem("File").items(scoped*).p.itemsV == direct)
        assert(uic.MenuGroup("File").items(scoped*).p.itemsV == direct)
        assert(rows(uic.ContextMenu(scoped).p.entriesV) == direct)
        assert(rows(uic.ContextMenu(Seq(
            _.item("Update").icon(uic.Icons.refresh),
            _.item.separator,
            _.item("Homepage").url("/")
        )).p.entriesV) ==
            direct)
    }

    "the two spellings mix in one call, and a prepared sequence splats in" in {
        val mixed = uic.SplitButton("Save").items(
            uic.MenuItem("Update"),
            _.item.separator,
            _.item("Delete")
        )
        val prepared: Seq[uic.MenuItem] = Seq(uic.MenuItem("A"), uic.MenuItem("B"))
        val splat                       = uic.SplitButton("Save").items(prepared*)
        assert(rows(mixed.p.entriesV) == List(uic.MenuItem("Update"), uic.MenuItem.separator, uic.MenuItem("Delete")))
        assert(rows(splat.p.entriesV) == prepared.toList)
    }

    "mega menu: root items and the groups of a column" in {
        val group = uic.MenuGroup("Laptops").items(_.item("Pro"))
        val item  = uic.MegaMenuItem("Products").column(_.group("Laptops").items(_.item("Pro")))
        assert(item == uic.MegaMenuItem("Products").column(group))
        assert(uic.MegaMenuItemScope.reading(uic.MegaMenu().items(_.item("Products")).p.entriesV) ==
            uic.ReactiveValue.Const(List(uic.MegaMenuItem("Products"))))
    }

    "breadcrumb crumbs" in {
        val scoped = uic.Breadcrumb().items(_.item("Docs", href = Present("/docs")), _.item("Menu"))
        assert(fixed(scoped.entries) == List(uic.BreadcrumbItem("Docs", href = Present("/docs")), uic.BreadcrumbItem("Menu")))
    }

    "listbox options, and options with groups whose entries take the same spellings" in {
        val items = uic.Listbox[String]().items(_.item("Paris", "par"), _.item("Rome", "rom", additionalText = Present("IT")))
        assert(options(items) == options(uic.Listbox[String]().item("Paris", "par").items(
            uic.ListItem.of("Rome", "rom", additionalText = Present("IT"))
        )))
        val groups = uic.Listbox[String]().optionGroups(
            _.item("Anywhere", "any"),
            _.group("Europe")(_.item("Paris", "par"), uic.ListItem.of("Rome", "rom"))
        )
        assert(options(groups) == List(
            uic.OptionItem.item(uic.ListItem.of("Anywhere", "any")),
            uic.OptionItem.group("Europe")(uic.ListItem.of("Paris", "par"), uic.ListItem.of("Rome", "rom"))
        ))
    }

    "tree nodes, nested through their children" in {
        val direct = List(uic.TreeNode("Documents", "docs", children = List(uic.TreeNode("CV", "cv"))))
        assert(fixed(uic.Tree[String]().nodes(_.node("Documents", "docs", children = Seq(_.node("CV", "cv")))).entries) == direct)
        assert(uic.TreeSelect[String]().nodes(_.node("Documents", "docs", children = Seq(_.node("CV", "cv")))).roots ==
            uic.ReactiveValue.Const(direct))
    }

    "tree table nodes" in {
        val scoped = uic.TreeTable[String]().nodes(_.node("root", children = Seq(_.node("leaf"))))
        assert(fixed(scoped.entries) == List(uic.TreeTableNode("root", List(uic.TreeTableNode("leaf")))))
    }

    "galleria images" in {
        val scoped = uic.Galleria().items(_.item("/a.jpg", alt = "A"), _.item("/b.jpg"))
        assert(fixed(scoped.entries) == List(uic.GalleriaItem("/a.jpg", alt = "A"), uic.GalleriaItem("/b.jpg")))
    }

    "accordion panels and tabs, with a constant label" in {
        val panels = uic.Accordion[String]().panels(_.panel("Shipping", content, "ship", disabled = true))
        assert(fixed(panels.entries) == List(uic.AccordionPanel("Shipping", content, "ship", disabled = true)))
        val tabs = uic.Tabs[String]().tabs(_.tab("Profile", content, "profile", disabled = true))
        assert(fixed(tabs.entries) == List(uic.Tab("Profile", content, "profile", disabled = true)))
    }

    "accordion panels and tabs take a reactive label too, as their single-entry builders do" in {
        val label = Signal.initConst("Live")
        assert(fixed(uic.Accordion[String]().panels(_.panel(label, content, "a")).entries) ==
            fixed(uic.Accordion[String]().panel(label, "a")(content).entries))
        // Tabs.tab wraps its content in a fragment; the label is what is compared here.
        assert(fixed(uic.Tabs[String]().tabs(_.tab(label, content, "t")).entries).map(t => (t.p.text, t.p.id)) ==
            fixed(uic.Tabs[String]().tab(label, "t")(content).entries).map(t => (t.p.text, t.p.id)))
    }

    "`_` offers only what that setter takes" in {
        typeCheckFailure(preamble + """def x = uic.SplitButton("Save").items(_.group("A"))""")
        typeCheckFailure(preamble + """def x = uic.Breadcrumb().items(_.item.separator)""")
        typeCheckFailure(preamble + """def x = uic.Tree[String]().nodes(_.item("a", "a"))""")
        typeCheckFailure(preamble + """def x = uic.Tabs[String]().tabs(_.panel("a", span("x"), "a"))""")
        typeCheckFailure(preamble + """def x = uic.MegaMenuItem("P").column(_.item("A"))""")
        typeCheck(preamble + """def x = uic.SplitButton("Save").items(_.item("A"), _.item.separator)""")
    }

end EntriesTest
