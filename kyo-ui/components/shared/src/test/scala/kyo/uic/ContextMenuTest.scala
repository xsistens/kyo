package kyo.uic

import kyo.*
import kyo.UI.*

/** What [[ContextMenu]] owes the reader beyond the rows [[MenuNav]] already moves: a panel that
  * appears WHERE the right-click landed.
  *
  * The position is the whole point of the component, and it is also the part that fails silently.
  * `onContextMenu` has a payload-free overload beside the typed one, and a handler bound to the
  * wrong one still registers the event, still renders the same markup and still opens the menu:
  * only the point is missing, and the panel then sits wherever the last one did. So these tests
  * assert the typed handler is the one that got wired, and that what it receives reaches the ref
  * the panel is placed from.
  */
class ContextMenuTest extends UicTest:

    private def menu(using Frame) = uic.ContextMenu(
        Seq(uic.MenuItem("Copy").onSelect(()), uic.MenuItem("Delete").onSelect(()))
    )(UI.div.cssClass("region")(UI.span("right-click me")))

    /** The wrapped region, with the state the mount would have allocated. */
    private def wired(open: SignalRef[Boolean], at: SignalRef[Maybe[UI.Point]])(using Frame) =
        for
            focus <- Signal.initRef(List.empty[Int])
            ui = menu.wired(open, focus, at, Nil, "cm")
            target <- elementWithClass(ui, "p-uic-contextmenu-target")
        yield target

    /** Right-clicks the region at a viewport point, as the dispatcher would. */
    private def rightClick(el: UI.Ast.Element, x: Double, y: Double)(using Frame): Any < Async =
        el.attrs.onContextMenuEvt match
            case Present(f) => f(UI.MouseEvent(el.attrs.identifier, UI.Modifiers.none, Present(UI.Point(x, y))))
            case Absent     => throw new AssertionError("the region declares no typed context-menu handler")

    "a right-click opens the menu at the point it landed on" in {
        for
            open   <- Signal.initRef(false)
            at     <- Signal.initRef(Absent: Maybe[UI.Point])
            target <- wired(open, at)
            _      <- rightClick(target, 240, 130)
            shown  <- open.get
            point  <- at.get
        yield
            assert(shown, "the menu opens")
            assert(point == Present(UI.Point(240, 130)), "and it is placed where the pointer was")
    }

    "the point is the payload-free overload's blind spot, so the typed handler is the one wired" in {
        for
            open   <- Signal.initRef(false)
            at     <- Signal.initRef(Absent: Maybe[UI.Point])
            target <- wired(open, at)
        yield
            assert(target.attrs.onContextMenuEvt.isDefined, "the typed overload carries the position")
            assert(target.attrs.onContextMenu.isEmpty, "and nothing is bound to the one that drops it")
    }

    "a second right-click moves the menu, rather than leaving it where the first one put it" in {
        for
            open   <- Signal.initRef(false)
            at     <- Signal.initRef(Absent: Maybe[UI.Point])
            target <- wired(open, at)
            _      <- rightClick(target, 240, 130)
            _      <- rightClick(target, 40, 600)
            point  <- at.get
        yield assert(point == Present(UI.Point(40, 600)))
    }

    "the open panel's backdrop answers a right-click, so no native menu opens over it" in {
        for
            open  <- Signal.initRef(true)
            at    <- Signal.initRef(Present(UI.Point(240, 130)): Maybe[UI.Point])
            focus <- Signal.initRef(List.empty[Int])
            ui = menu.wired(open, focus, at, Nil, "cm")
            drop <- elementWithClass(ui, "p-uic-overlay-backdrop")
            _ <- drop.attrs.onContextMenuEvt match
                case Present(f) => f(UI.MouseEvent(Absent, UI.Modifiers.none, Present(UI.Point(10, 10))))
                case Absent     => throw new AssertionError("the backdrop declares no typed context-handler")
            still <- open.get
        yield assert(!still, "and it closes the menu, so the next right-click reaches what is under it")
    }

    "the panel is addressable, which is what places it at the point" in {
        for
            open  <- Signal.initRef(true)
            at    <- Signal.initRef(Present(UI.Point(240, 130)): Maybe[UI.Point])
            focus <- Signal.initRef(List.empty[Int])
            ui = menu.wired(open, focus, at, Nil, "cm")
            panel <- elementWithClass(ui, "p-uic-overlay-panel")
        yield
            assert(panel.attrs.identifier.contains("cm-panel"), "derived from the component's own id")
            assert(panel.attrs.cssClasses.contains("p-uic-overlay-portal"), "and it portals out of any clipping ancestor")
    }

    "the menu is closed and points nowhere until a right-click says otherwise" in {
        for
            open   <- Signal.initRef(false)
            at     <- Signal.initRef(Absent: Maybe[UI.Point])
            target <- wired(open, at)
            panels <- elementsWithClass(menu.render, "p-uic-overlay-panel")
        yield
            assert(target.attrs.cssClasses.contains("p-uic-overlay-anchor"))
            assert(panels.isEmpty, "the static projection renders the region inert, with no panel")
    }

    // A menu whose mount is keyless is rebuilt by every emission of the region around it, and
    // `openRef` IS that mount's state — so the menu resets, and a subscription of the departing
    // content that emits into a range the rebuild has already taken away dies on `Unknown
    // reactive range` and never paints again. The id the caller gave is the identity that stops
    // it; without one the mount stays keyless, exactly as it was.
    "a menu the caller named keeps its instance across a re-render of the region around it" in {
        val named = uic.ContextMenu(Seq(uic.MenuItem("Copy").onSelect(())))(UI.div("region")).id("m1")
        val anon  = uic.ContextMenu(Seq(uic.MenuItem("Copy").onSelect(())))(UI.div("region"))
        // `key` is `Maybe[Any]` and Any is not comparable under strict equality, so the keys are
        // held against each other by their rendered form rather than by ==.
        (named.render, anon.render) match
            case (n: Ast.Mounted, a: Ast.Mounted) =>
                assert(n.key.map(_.toString).contains((uic.ContextMenu -> "m1").toString), "the id is the key")
                assert(a.key.isEmpty, "and no id means no key, as before")
            case other => fail(s"a ContextMenu renders as a mount; got: $other")
        end match
    }

    // ---- targetless: the panel without the wrapper `div` ----
    //
    // The wrapping form's target is a `div`, and a `div` may not sit between a `<table>` and its
    // `<tr>` — the parser hoists it out. So the placement a context menu is most often wanted for
    // is the one the wrapper cannot have. The targetless form takes the listener off the component
    // and leaves the panel, which is the only part that had to be a component at all.

    private def rows(using Frame) = uic.ContextMenu(
        Seq(uic.MenuItem("Play").onSelect(()), uic.MenuItem("Queue").items(uic.MenuItem("Next").onSelect(())))
    )

    /** The state a caller's own mount would have allocated. */
    private def allocState(m: uic.ContextMenu)(using Frame): uic.ContextMenu.State < Async =
        UI.Commands.init(_ => ()).map(c => Env.run(c)(m.state))

    "the targetless form renders no target wrapper, which is the whole of what it is for" in {
        for
            st <- allocState(rows)
            ui = rows.targetless(st).render
            wrappers <- elementsWithClass(ui, "p-uic-contextmenu-target")
        yield
            assert(wrappers.isEmpty, "no div to be hoisted out of a table")
            assert(!ui.isInstanceOf[Ast.Mounted], "and no mount of its own, so nothing above it can reset one")
    }

    "openAt opens the menu at the point the caller's own element was right-clicked" in {
        for
            st <- allocState(rows)
            _  <- st.openAt(UI.MouseEvent(Present("row-7"), UI.Modifiers.none, Present(UI.Point(310, 92))))
            ui = rows.targetless(st).render
            panel <- elementWithClass(ui, "p-uic-overlay-panel")
        yield
            assert(panel.attrs.cssClasses.contains("p-uic-overlay-portal"), "the panel portals, as it always did")
            assert(
                panel.attrs.identifier.exists(_.endsWith("-panel")),
                "and it is addressable off a base the state minted, so a caller owes it no id"
            )
    }

    // The reset a caller could not have known to do. Escape and an outside click close the menu
    // through the Overlay's own ref write, which leaves the highlight and any open submenu exactly
    // where they were — the wrapping form flushes them on the NEXT right-click, and `openAt` is
    // where that lives once the right-click is the caller's.
    "openAt flushes the highlight and the submenu tree the last visit left open" in {
        for
            st <- allocState(rows)
            _  <- Kyo.foreachDiscard(st.submenus)((_, r) => r.set(true))
            _  <- st.focus.set(List(1, 0))
            _  <- st.openAt(UI.MouseEvent(Absent, UI.Modifiers.none, Present(UI.Point(10, 10))))
            f  <- st.focus.get
            os <- Kyo.foreach(st.submenus)((_, r) => r.get)
            on <- st.open.get
        yield
            assert(st.submenus.nonEmpty, "the menu has a submenu to leave open in the first place")
            assert(f.isEmpty, "the highlight starts at the top")
            assert(os.forall(!_), "and no submenu is carried over from where the reader last was")
            assert(on, "and the menu is open")
    }

    "close is for the closes the panel cannot see — the row it was opened on going away" in {
        for
            st <- allocState(rows)
            _  <- st.openAt(UI.MouseEvent(Absent, UI.Modifiers.none, Present(UI.Point(10, 10))))
            _  <- Kyo.foreachDiscard(st.submenus)((_, r) => r.set(true))
            _  <- st.close
            on <- st.open.get
            os <- Kyo.foreach(st.submenus)((_, r) => r.get)
        yield
            assert(!on)
            assert(os.forall(!_), "and it leaves nothing for the next open to inherit")
    }

    "the state carries one open/closed slot per submenu of the menu it was built from" in {
        for st <- allocState(rows)
        yield assert(st.submenus.map(_._1) == rows.submenuPaths)
    }

    "two named menus on one page take two distinct keys" in {
        val a = uic.ContextMenu(Seq(uic.MenuItem("Copy").onSelect(())))(UI.div("a")).id("m1").render
        val b = uic.ContextMenu(Seq(uic.MenuItem("Copy").onSelect(())))(UI.div("b")).id("m2").render
        (a, b) match
            case (x: Ast.Mounted, y: Ast.Mounted) =>
                assert(x.key.map(_.toString) != y.key.map(_.toString))
            case other => fail(s"a ContextMenu renders as a mount; got: $other")
        end match
    }

end ContextMenuTest
