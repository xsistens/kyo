package kyo.uic

import kyo.*
import kyo.UI.*

/** What a [[MegaMenu]] keeps while its list changes under it: the panel the reader opened belongs to its root ITEM,
  * so a department the signal adds before it must not open a neighbour's panel instead.
  */
class MegaMenuTest extends UicTest:

    private def department(name: String, leaf: String): uic.MegaMenuItem =
        uic.MegaMenuItem(name).column(uic.MenuGroup(s"$name group").items(uic.MenuItem(leaf).onSelect(())))

    "a department the signal adds before an open panel leaves that panel open" in {
        for
            live  <- Signal.initRef(Seq.empty[uic.MegaMenuItem])
            subs  <- Submenus.init()
            focus <- Anchored.init(List.empty[Int])
            ui = uic.MegaMenu().items(department("Furniture", "Beds"), live, department("Garden", "Spades")).wired(subs, focus, "mm")
            links  <- elementsWithClass(ui, "p-megamenu-item-link")
            _      <- click(links(1))
            before <- renderHtml(ui)
            _      <- live.set(Seq(department("Season", "Loungers")))
            after  <- renderHtml(ui)
        yield
            assert(before.contains("Spades") && !before.contains("Beds"), "Garden's panel is the open one")
            assert(after.contains("Spades"), "and stays open once Season arrives before it")
            assert(!after.contains("Loungers"), "Season's panel does not open in its place")
        end for
    }

end MegaMenuTest
