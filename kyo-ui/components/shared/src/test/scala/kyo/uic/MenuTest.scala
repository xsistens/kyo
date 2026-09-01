package kyo.uic

import kyo.*
import kyo.UI.*

/** What [[Menu]] owes the reader beyond the moves [[ListNav]] already decides: an announcement of
  * the row it has highlighted, and a close when the reader tabs out of a floating one.
  *
  * The announcement is the part a screen reader depends on entirely. The highlight is a class on a
  * row, which says nothing to anyone who cannot see it; `aria-activedescendant` on the focused
  * list is the whole of what is said, so a menu that renders one without the other is silent.
  */
class MenuTest extends UicTest:

    private val rows = List(
        uic.MenuItem("New").onSelect(()),
        uic.MenuItem("Search").onSelect(()),
        uic.MenuItem("Print").onSelect(())
    )

    private def menu(using Frame) = uic.Menu().items(rows*)

    "a menu announces the row it has highlighted, without being given an id" in {
        for
            hi <- Signal.initRef(1)
            ui = menu.wired(hi, "m")
            list <- elementWithClass(ui, "p-menu-list")
            row  <- elementWithClass(ui, "p-focus")
        yield
            assert(list.attrs.ariaAttrs.get("activedescendant").contains("m-active"))
            assert(row.attrs.identifier.contains("m-active"), "and the highlighted row is what it names")
    }

    "the caller's own id still wins, since a page may want to name the row itself" in {
        for
            hi <- Signal.initRef(1)
            ui = menu.id("mine").wired(hi, "m")
            list <- elementWithClass(ui, "p-menu-list")
        yield assert(list.attrs.ariaAttrs.get("activedescendant").contains("mine-active"))
    }

    "nothing highlighted, nothing announced" in {
        for
            hi <- Signal.initRef(-1)
            ui = menu.wired(hi, "m")
            list <- elementWithClass(ui, "p-menu-list")
        yield assert(list.attrs.ariaAttrs.get("activedescendant").isEmpty)
    }

    "Tab closes a floating menu, because the reader has left it" in {
        for
            open <- Signal.initRef(true)
            hi   <- Signal.initRef(1)
            ui = menu.popup(open).wired(hi, "m")
            list  <- elementWithClass(ui, "p-menu-list")
            _     <- press(list, UI.Keyboard.Tab)
            still <- open.get
            at    <- hi.get
        yield assert(!still && at == -1, "and it takes the highlight with it, having nothing left to highlight")
    }

    "an inline menu has no popup to close, and keeps answering the arrows" in {
        for
            hi <- Signal.initRef(1)
            ui = menu.wired(hi, "m")
            list <- elementWithClass(ui, "p-menu-list")
            _    <- press(list, UI.Keyboard.ArrowDown)
            at   <- hi.get
        yield assert(at == 2)
    }

end MenuTest
