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
class MenuTest extends ReactiveValueContract:

    private val rows = List(
        uic.MenuItem("New").onSelect(()),
        uic.MenuItem("Search").onSelect(()),
        uic.MenuItem("Print").onSelect(())
    )

    private def menu(using Frame) = uic.Menu().items(rows*)

    "a menu announces the row it has highlighted, without being given an id" in {
        for
            hi <- Anchored.init(1)
            ui = menu.wired(hi, "m")
            list <- elementWithClass(ui, "p-menu-list")
            row  <- elementWithClass(ui, "p-focus")
        yield
            assert(list.attrs.ariaAttrs.get("activedescendant").contains("m-active"))
            assert(row.attrs.identifier.contains("m-active"), "and the highlighted row is what it names")
    }

    "the caller's own id still wins, since a page may want to name the row itself" in {
        for
            hi <- Anchored.init(1)
            ui = menu.id("mine").wired(hi, "m")
            list <- elementWithClass(ui, "p-menu-list")
        yield assert(list.attrs.ariaAttrs.get("activedescendant").contains("mine-active"))
    }

    "nothing highlighted, nothing announced" in {
        for
            hi <- Anchored.init(-1)
            ui = menu.wired(hi, "m")
            list <- elementWithClass(ui, "p-menu-list")
        yield assert(list.attrs.ariaAttrs.get("activedescendant").isEmpty)
    }

    "Tab closes a floating menu, because the reader has left it" in {
        for
            open <- Signal.initRef(true)
            hi   <- Anchored.init(1)
            ui = menu.popup(open).wired(hi, "m")
            list  <- elementWithClass(ui, "p-menu-list")
            _     <- press(list, UI.Keyboard.Tab)
            still <- open.get
            at    <- hi.get
        yield assert(!still && at == -1, "and it takes the highlight with it, having nothing left to highlight")
    }

    "an inline menu has no popup to close, and keeps answering the arrows" in {
        for
            hi <- Anchored.init(1)
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
            hi  <- Anchored.init(0)
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
            hi  <- Anchored.init(0)
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
            hi <- Anchored.init(0)
            ui = uic.Menu().items(rows(0), rows(1).disabled(derived), rows(2)).wired(hi, "m")
            before <- dimmed(ui)
            _      <- target.set(Present("a"))
            after  <- dimmed(ui)
        yield assert(before.isEmpty && after == Seq(1), s"before=$before after=$after")
    }

    "resolving a reactive disabled costs no region of its own: it is read in the menu's one region" in {
        // A region per resolution nested around the highlight's would repaint rows it closed over, so the
        // resolution is read beside the highlight. `regionsAbove` counts the reactive wrappers over a row: a
        // constant menu and a reactive one both have the one region the highlight needs.
        for
            off      <- Signal.initRef(false)
            hi       <- Anchored.init(0)
            constant <- regionsAbove(menu.wired(hi, "m"), "p-menu-item")
            reactive <- regionsAbove(
                uic.Menu().items(rows(0), rows(1).disabled(off), rows(2)).wired(hi, "m"),
                "p-menu-item"
            )
        yield assert(constant == 1 && reactive == 1, s"constant=$constant reactive=$reactive")
    }

    "a nav row says which page the reader is on, on the row AND on its link" in {
        // The class is for the sheet; `aria-current` is the whole of what is said to a reader
        // who cannot see it, which is the same split as the highlight two tests above.
        for
            hi <- Anchored.init(-1)
            ui = uic.Menu().items(
                uic.MenuItem("Home").url("/"),
                uic.MenuItem("Queue").url("/queue").current(true)
            ).wired(hi, "m")
            marked <- elementsWithClass(ui, "p-uic-menu-item-current")
            links  <- elementsWithClass(ui, "p-menu-item-link")
        yield
            assert(marked.size == 1, s"one row of two is current; got ${marked.size}")
            assert(links.count(_.attrs.ariaAttrs.get("current").contains("page")) == 1)
            assert(
                links.count(_.attrs.ariaAttrs.contains("current")) == 1,
                "and a row that declares nothing says nothing, so an ordinary menu renders as it did"
            )
    }

    "a reactive current is a CHANNEL: it costs no region, which is what a nav is built on" in {
        // The contrast with `disabled` directly above is the point. Resolving a reactive
        // `disabled` must cost a region, because the navigable index set, `MenuNav.skip` and
        // whether an activate effect is attached all read it as a plain Boolean. Being current
        // changes none of those, so it goes through `when(sig)(_.cssClass(name))` and `aria(name -> sig)`,
        // patched in place. That is what lets a nav be built ONCE: were this a region, every
        // navigation would rebuild the strip and throw away the reader's keyboard position.
        for
            here <- Signal.initRef(false)
            hi   <- Anchored.init(0)
            ui = uic.Menu().items(rows(0), rows(1).url("/queue").current(here), rows(2)).wired(hi, "m")
            regions <- regionsAbove(ui, "p-menu-item")
            rowEls  <- elementsWithClass(ui, "p-menu-item")
            links   <- elementsWithClass(ui, "p-menu-item-link")
        yield
            assert(regions == 1, s"the highlight render is the only one; got $regions")
            assert(
                rowEls.count(_.attrs.reactiveClasses.contains("p-uic-menu-item-current")) == 1,
                "the class is a channel on the row"
            )
            assert(
                links.count(_.attrs.reactiveAttrs.contains("aria-current")) == 1,
                "and aria-current is a channel on the link"
            )
    }

    // ── the navigation role: a list of links, not a menu widget ──────────────────

    private def site(using Frame) =
        uic.Menu()
            .accessibleRole(uic.MenuAccessibleRole.Navigation)
            .accessibleName("Components")
            .items(
                uic.MenuItem("Actions").items(
                    uic.MenuItem("Button").url("/c/button").current(true),
                    uic.MenuItem("Link").url("/c/link")
                ),
                uic.MenuItem("Report a problem").onSelect(())
            )

    "a navigation menu is a named nav landmark around a plain list" in {
        // A site's navigation is links, and the ARIA menu role promises an application menu
        // instead: one tab stop, arrows inside, and a screen reader that stops reading the
        // rows as links. So none of that vocabulary may appear here.
        for
            ui   <- Kyo.lift(site.render)
            root <- elementWithClass(ui, "p-menu")
            list <- elementWithClass(ui, "p-menu-list")
            rows <- elementsWithClass(ui, "p-menu-item")
            head <- elementWithClass(ui, "p-menu-submenu-label")
        yield
            assert(root.isInstanceOf[UI.Ast.Nav], "the root is a <nav>, which is the landmark")
            assert(root.attrs.ariaAttrs.get("label").contains("Components"), "named, since a page has several navs")
            assert(list.attrs.role.isEmpty, "the list keeps its native list semantics")
            assert(list.attrs.tabIndex.isEmpty && list.attrs.onKeyDown.isEmpty, "and is no tab stop with arrow keys")
            assert(rows.forall(_.attrs.role.isEmpty), "a row is a list item, not a menuitem")
            assert(head.attrs.role.isEmpty, "a group heading stays readable as a list item")
    }

    "every link in a navigation menu is its own tab stop and navigates natively" in {
        for
            links <- elementsWithClass(site.render, "p-menu-item-link")
        yield
            assert(links.forall(_.attrs.tabIndex.contains(0)), "the reader tabs from link to link")
            assert(links(0).attrs.onClick.isEmpty, "a url row declares no click handler, so the browser follows it")
            assert(links(0).attrs.ariaAttrs.get("current").contains("page"), "and the current page is announced")
            assert(links(2).attrs.onClick.isDefined, "a row with an action still runs it")
    }

    "a navigation brings the row of the page being shown into view" in {
        // A long nav opened on a deep link shows its top rows, and the page the reader is on can
        // be screens further down. The flag scrolls it in with `nearest`, so a row already in
        // view does not move.
        for
            rows <- elementsWithClass(site.render, "p-menu-item")
        yield
            assert(rows(0).attrs.scrollAuto.contains(true), "the current row carries the flag")
            assert(rows.drop(1).forall(_.attrs.scrollAuto.isEmpty), "and no other row does")
    }

    "the navigation role holds no highlight, so a bound one changes nothing" in {
        for
            hi <- Anchored.init(1)
            ui = site.wired(hi, "m")
            focused <- elementsWithClass(ui, "p-focus")
            list    <- elementWithClass(ui, "p-menu-list")
        yield
            assert(focused.isEmpty, "no roving highlight")
            assert(list.attrs.ariaAttrs.get("activedescendant").isEmpty, "and nothing announced through one")
    }

    "an ordinary menu keeps the menu vocabulary, and takes a name on its list" in {
        for
            hi <- Anchored.init(-1)
            ui = menu.accessibleName("File").wired(hi, "m")
            list <- elementWithClass(ui, "p-menu-list")
            row  <- elementWithClass(ui, "p-menu-item")
        yield
            assert(list.attrs.role.contains("menu"))
            assert(row.attrs.role.contains("menuitem"))
            assert(list.attrs.ariaAttrs.get("label").contains("File"), "the list is the menu, so it carries the name")
    }

    "a row the signal adds before the highlighted one leaves the highlight on its row" in {
        for
            live <- Signal.initRef(Seq.empty[uic.MenuItem])
            hi   <- Anchored.init(-1)
            ui = uic.Menu().items(_.item("New").onSelect(()), live, _.item("Print").onSelect(())).wired(hi, "m")
            _      <- elementWithClass(ui, "p-menu-list").map(press(_, UI.Keyboard.ArrowDown))
            _      <- elementWithClass(ui, "p-menu-list").map(press(_, UI.Keyboard.ArrowDown))
            before <- elementWithClass(ui, "p-focus")
            _      <- live.set(Seq(uic.MenuItem("Recent").onSelect(())))
            after  <- elementWithClass(ui, "p-focus")
        yield
            assert(labelOf(before) == "Print", s"Print is highlighted: ${labelOf(before)}")
            assert(labelOf(after) == "Print", s"and stays highlighted once Recent arrives before it: ${labelOf(after)}")
        end for
    }

    "a highlighted row the signal removes takes the highlight with it" in {
        for
            live <- Signal.initRef(Seq(uic.MenuItem("Recent").onSelect(())))
            hi   <- Anchored.init(-1)
            ui = uic.Menu().items(_.item("New").onSelect(()), live, _.item("Print").onSelect(())).wired(hi, "m")
            _       <- elementWithClass(ui, "p-menu-list").map(press(_, UI.Keyboard.ArrowDown))
            _       <- elementWithClass(ui, "p-menu-list").map(press(_, UI.Keyboard.ArrowDown))
            before  <- elementWithClass(ui, "p-focus")
            _       <- live.set(Seq.empty)
            focused <- elementsWithClass(ui, "p-focus")
        yield
            assert(labelOf(before) == "Recent", s"Recent is highlighted: ${labelOf(before)}")
            assert(focused.isEmpty, s"no neighbour takes it: ${focused.map(labelOf)}")
        end for
    }

    /** The label text a row shows. */
    private def labelOf(row: UI.Ast.Element): String =
        def texts(n: UI): List[String] = n match
            case t: UI.Ast.Text    => List(t.value)
            case e: UI.Ast.Element => e.children.toList.flatMap(texts)
            case _                 => Nil
        texts(row).mkString
    end labelOf

    private def links(t: UI)(using Frame)  = elementsWithClass(t, "p-menu-item-link").map(_.filter(declaresClick))
    private def panels(t: UI)(using Frame) = elementsWithClass(t, "p-menu-overlay")

    // A popup closes on a pick, an outside click or Escape: the rows are the control a gesture uses, and a fixed popup
    // is one nothing outside dismisses, since a pick still runs the item's own action.
    holds(new SlotCase[Menu, Boolean](
        name = "popup",
        bare = Present(menu.popup()),
        bind = menu.popup(_),
        hear = (m, f) => m.onPopupChange(f),
        event = "onPopupChange",
        render = _.render,
        start = true,
        next = false,
        control = links(_).map(_.head),
        shows = panels(_).map(_.nonEmpty),
        isFixed = Present(elementsWithClass(_, "p-uic-overlay-backdrop").map(_.isEmpty)),
        ready = Present(panels(_).map(_.nonEmpty))
    ))

end MenuTest
