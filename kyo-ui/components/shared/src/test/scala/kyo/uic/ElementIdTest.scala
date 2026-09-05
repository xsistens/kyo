package kyo.uic

import kyo.*
import kyo.UI.*

/** `id(...)` means "stamp this element's id", and the menu family plus `Tabs` used it as a BASE
  * only: the id a caller gave never reached the document, and the sole id that existed was the
  * derived `s"$id-active"` on whichever row the keyboard had highlighted (GAPS.md F-34).
  *
  * `HasElementId`'s own contract says what these owe — *"a container or display component puts it
  * on its own root"* — and `Panel` next door already does both halves. These pin the two halves
  * together, because they pull against each other: the components MINT a base when no id was
  * given, so stamping "the base" on the root would put a machine-made name in the document for
  * every menu that was never named. The caller's id and the part base are two values now, and
  * both cases below have to hold at once.
  */
class ElementIdTest extends UicTest:

    private val rows = List(
        uic.MenuItem("New").onSelect(()),
        uic.MenuItem("Search").onSelect(())
    )

    private def menu(using Frame) = uic.Menu().items(rows*)

    /** The id actually stamped on the element carrying `cls`. */
    private def idOf(ui: UI, cls: String)(using Frame): Maybe[String] < Sync =
        elementWithClass(ui, cls).map(_.attrs.identifier)

    "an inline Menu carries the id it was given, on its own root" in {
        for
            hi <- Signal.initRef(1)
            ui = menu.id("mine").wired(hi, "minted")
            root <- idOf(ui, "p-menu")
            list <- elementWithClass(ui, "p-menu-list")
        yield
            assert(root == Present("mine"))
            // …and the derived part still comes off the same value, which is the pull: one
            // setter has to answer both without either answer displacing the other.
            assert(list.attrs.ariaAttrs.get("activedescendant").contains("mine-active"))
    }

    "a Menu given NO id puts no minted name in the document, and still announces" in {
        for
            hi <- Signal.initRef(1)
            ui = menu.wired(hi, "minted")
            root <- idOf(ui, "p-menu")
            list <- elementWithClass(ui, "p-menu-list")
            row  <- elementWithClass(ui, "p-focus")
        yield
            assert(root.isEmpty, s"the mount's minted base must not become an element id, got $root")
            assert(list.attrs.ariaAttrs.get("activedescendant").contains("minted-active"))
            assert(row.attrs.identifier.contains("minted-active"))
    }

    "a popup Menu carries it on the list, the element that IS the menu" in {
        // The component's root in a popup is the overlay's panel, which the menu does not own.
        for
            open <- Signal.initRef(true)
            hi   <- Signal.initRef(1)
            ui = menu.id("mine").popup(open).wired(hi, "minted")
            list <- idOf(ui, "p-menu-list")
        yield assert(list == Present("mine"))
    }

    "a Menubar carries it on its root" in {
        val bar = uic.Menubar().items(uic.MenuItem("File").items(rows*), uic.MenuItem("Edit"))
        for
            refs  <- Kyo.foreach(bar.submenuPaths)(p => Signal.initRef(false).map(p -> _))
            focus <- Signal.initRef(List.empty[Int])
            ui   = bar.id("mine").wired(refs.toList, focus, "minted")
            bare = bar.wired(refs.toList, focus, "minted")
            named   <- idOf(ui, "p-menubar")
            unnamed <- idOf(bare, "p-menubar")
        yield
            assert(named == Present("mine"))
            assert(unnamed.isEmpty)
        end for
    }

    "a TieredMenu carries it on its root" in {
        val tm = uic.TieredMenu().items(uic.MenuItem("File").items(rows*), uic.MenuItem("Edit"))
        for
            refs  <- Kyo.foreach(tm.submenuPaths)(p => Signal.initRef(false).map(p -> _))
            focus <- Signal.initRef(List.empty[Int])
            ui   = tm.id("mine").wired(refs.toList, focus, "minted")
            bare = tm.wired(refs.toList, focus, "minted")
            named   <- idOf(ui, "p-tieredmenu")
            unnamed <- idOf(bare, "p-tieredmenu")
        yield
            assert(named == Present("mine"))
            assert(unnamed.isEmpty)
        end for
    }

    "a MegaMenu carries it on its root" in {
        val mm = uic.MegaMenu().items(
            uic.MegaMenuItem("Furniture").column(uic.MenuGroup("Living").items(uic.MenuItem("Chairs"))),
            uic.MegaMenuItem("Contact")
        )
        for
            refs  <- Kyo.foreach(mm.panelPaths)(p => Signal.initRef(false).map(p -> _))
            focus <- Signal.initRef(List.empty[Int])
            ui   = mm.id("mine").wired(refs.toList, focus, "minted")
            bare = mm.wired(refs.toList, focus, "minted")
            named   <- idOf(ui, "p-megamenu")
            unnamed <- idOf(bare, "p-megamenu")
        yield
            assert(named == Present("mine"))
            assert(unnamed.isEmpty)
        end for
    }

    "Tabs carries it on its root, alongside the headers derived from it" in {
        val strip = uic.Tabs().tabs(
            uic.Tab("One", p("first"), "one"),
            uic.Tab("Two", p("second"), "two")
        )
        for
            ui = strip.id("mine").wired(List("mine-one", "mine-two"), _ => ())
            root    <- idOf(ui, "p-tabs")
            headers <- elementsWithClass(ui, "p-tab")
        yield
            assert(root == Present("mine"))
            // The derived name is a different string, which is why the two uses never collided —
            // only one of them was ever rendered.
            assert(headers.map(_.attrs.identifier).toSeq == Seq(Present("mine-one"), Present("mine-two")))
        end for
    }
end ElementIdTest
