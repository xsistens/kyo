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

end ContextMenuTest
