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

    // ── a reactive `disabled` ────────────────────────────────────────────────────

    /** Indices of the rows currently carrying `.p-disabled`. */
    private def dimmed(ui: UI)(using Frame): Seq[Int] < Sync =
        elementsWithClass(ui, "p-menu-item").map(_.toSeq.zipWithIndex.collect {
            case (r, i) if r.attrs.cssClasses.contains("p-disabled") => i
        })

    "an item's disabled follows a signal, and the row dims on emission" in {
        for
            off <- Signal.initRef(false)
            hi  <- Signal.initRef(0)
            ui = uic.Menu().items(rows(0), rows(1).disabled(off), rows(2)).wired(hi, "m")
            before <- dimmed(ui)
            _      <- off.set(true)
            after  <- dimmed(ui)
        yield assert(before.isEmpty && after == Seq(1))
    }

    "a disabled row loses its class, its handler and its place in the arrows together" in {
        // The things `disabled` means are all decided from ONE resolved boolean, so a
        // reactive one cannot leave them disagreeing.
        for
            off <- Signal.initRef(true)
            hi  <- Signal.initRef(0)
            ui = uic.Menu().items(rows(0), rows(1).disabled(off), rows(2)).wired(hi, "m")
            items <- elementsWithClass(ui, "p-menu-item")
            list  <- elementWithClass(ui, "p-menu-list")
            _     <- press(list, UI.Keyboard.ArrowDown)
            at    <- hi.get
        yield
            assert(items(1).attrs.cssClasses.contains("p-disabled"))
            assert(items(1).attrs.ariaAttrs.get("disabled").contains("true"))
            assert(at == 2, "ArrowDown from row 0 steps OVER the disabled row")
    }

    "a derived signal drives it too, not only a ref written directly" in {
        // The shape a context menu actually has: the flag is computed from the target the
        // host wrote plus a second live source, so what reaches the slot is a mapped
        // signal several hops from any ref.
        for
            target <- Signal.initRef(Absent: Maybe[String])
            list   <- Signal.initRef(Seq("a", "b"))
            derived = target.combineLatest(list).map { (t, l) =>
                t.map(k => l.contains(k)).getOrElse(false)
            }
            hi <- Signal.initRef(0)
            ui = uic.Menu().items(rows(0), rows(1).disabled(derived), rows(2)).wired(hi, "m")
            before <- dimmed(ui)
            _      <- target.set(Present("a"))
            after  <- dimmed(ui)
        yield assert(before.isEmpty && after == Seq(1), s"before=$before after=$after")
    }

    "resolving costs a region only when there is something to resolve" in {
        // Otherwise every menu in the library would grow a subscription it never needed.
        // `regionsAbove` counts the reactive wrappers over a row: the highlight render is
        // the one a constant menu already had.
        for
            off      <- Signal.initRef(false)
            hi       <- Signal.initRef(0)
            constant <- regionsAbove(menu.wired(hi, "m"), "p-menu-item")
            reactive <- regionsAbove(
                uic.Menu().items(rows(0), rows(1).disabled(off), rows(2)).wired(hi, "m"),
                "p-menu-item"
            )
        yield assert(constant == 1 && reactive == 2, s"constant=$constant reactive=$reactive")
    }

end MenuTest
