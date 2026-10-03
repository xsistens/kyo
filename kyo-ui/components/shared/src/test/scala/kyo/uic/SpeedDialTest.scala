package kyo.uic

import kyo.*
import kyo.UI.*

/** The keyboard of `uic.SpeedDial`, which carries `role="menu"` and until now answered no key.
  *
  * Two shapes are under test. The CLOSED fan renders no actions at all, because the sheet's way
  * of hiding them (`scale(0)`, `opacity: 0`) left every one of them a real tab stop on a dial the
  * reader had not opened. The OPEN fan is one tab stop with [[ListNav]] inside it, along the axis
  * the direction lays the actions out on, coming round because a menu comes round.
  */
class SpeedDialTest extends UicTest:

    private val actions = List(
        uic.MenuItem("Add").icon(Icons.pencil).onSelect(()),
        uic.MenuItem("Edit").icon(Icons.pencil).disabled(true).onSelect(()),
        uic.MenuItem("Delete").icon(Icons.trash).onSelect(())
    )

    private def dial(direction: SpeedDialDirection = SpeedDialDirection.Up)(using Frame) =
        uic.SpeedDial().items(actions*).direction(direction)

    /** The open fan, plus the ids its arrows asked focus to move to. */
    private def fan(direction: SpeedDialDirection = SpeedDialDirection.Up, open: Boolean = true)(using
        Frame
    ): (SignalRef[Boolean], SignalRef[List[String]], UI) < Async =
        for
            openRef <- Signal.initRef(open)
            moved   <- Signal.initRef(List.empty[String])
            ui = dial(direction).open(openRef).wired(openRef, "sd", id => moved.updateAndGet(_ :+ id))
        yield (openRef, moved, ui)

    /** Presses `key` on the action at `at` and reports the id focus was asked to move to. */
    private def after(at: Int, key: UI.Keyboard, direction: SpeedDialDirection = SpeedDialDirection.Up)(using
        Frame
    ): Maybe[String] < Async =
        for
            (_, moved, ui) <- fan(direction)
            items          <- elementsWithClass(ui, "p-speeddial-item")
            btn            <- elements(items(at)).map(_.find(_.attrs.role.contains("menuitem")).get)
            _              <- press(btn, key)
            got            <- moved.get
        yield Maybe.fromOption(got.headOption)

    "a closed fan renders no actions, so a reader does not tab through a dial they never opened" in {
        for
            (_, _, ui) <- fan(open = false)
            items      <- elementsWithClass(ui, "p-speeddial-item")
            list       <- elementWithClass(ui, "p-speeddial-list")
        yield
            assert(items.isEmpty)
            assert(list.attrs.role.contains("menu"), "the empty list is still the menu the toggle names")
    }

    "the dial is ONE tab stop, the toggle: an action is reached by opening, not by tabbing" in {
        for
            (_, _, ui) <- fan()
            toggle     <- elementWithClass(ui, "p-speeddial-button")
            btns       <- elements(ui).map(_.filter(_.attrs.role.contains("menuitem")))
        yield
            assert(btns.size == 3, "every action is a menuitem, disabled ones included")
            assert(!toggle.attrs.tabIndex.contains(-1), "the toggle is the tab stop, as a button is")
            assert(btns(0).attrs.tabIndex.contains(-1), "no action holds a tab stop, not even the first")
            assert(btns(2).attrs.tabIndex.contains(-1))
            assert(btns(1).attrs.tabIndex.isEmpty, "a disabled action is out of the tab order natively")
    }

    "Tab out of an open fan closes it, because the reader has left the menu" in {
        for
            (openRef, _, ui) <- fan()
            items            <- elementsWithClass(ui, "p-speeddial-item")
            btn              <- elements(items(0)).map(_.find(_.attrs.role.contains("menuitem")).get)
            _                <- press(btn, UI.Keyboard.Tab)
            still            <- openRef.get
        yield assert(!still)
    }

    "and Shift+Tab does too, since that leaves it just the same" in {
        for
            (openRef, _, ui) <- fan()
            items            <- elementsWithClass(ui, "p-speeddial-item")
            btn              <- elements(items(2)).map(_.find(_.attrs.role.contains("menuitem")).get)
            _                <- press(btn, UI.Keyboard.Tab, UI.Modifiers(shift = true))
            still            <- openRef.get
        yield assert(!still)
    }

    "Tab on the toggle of an open fan closes it too: the actions are not what it tabs into" in {
        for
            (openRef, _, ui) <- fan()
            toggle           <- elementWithClass(ui, "p-speeddial-button")
            _                <- press(toggle, UI.Keyboard.Tab)
            still            <- openRef.get
        yield assert(!still)
    }

    "Tab on a closed dial is the page's, and leaves the fan closed" in {
        for
            (openRef, moved, ui) <- fan(open = false)
            toggle               <- elementWithClass(ui, "p-speeddial-button")
            _                    <- press(toggle, UI.Keyboard.Tab)
            still                <- openRef.get
            went                 <- moved.get
        yield assert(!still && went.isEmpty)
    }

    "opening seeds focus onto the first action and closing hands it back to the toggle" in {
        for
            (_, _, ui) <- fan()
            btns       <- elements(ui).map(_.filter(_.attrs.role.contains("menuitem")))
        yield
            assert(btns(0).attrs.focusAuto.contains(true))
            assert(btns(0).attrs.focusRestore.contains(true))
            assert(btns(2).attrs.focusAuto.isEmpty, "only one action is seeded, or the fan fights itself")
    }

    "the arrows walk the fan along the axis its direction lays out" in {
        for
            down <- after(0, UI.Keyboard.ArrowDown)
            up   <- after(2, UI.Keyboard.ArrowUp)
        yield assert(down == Present("sd-i2") && up == Present("sd-i0"), "the disabled action is stepped over")
    }

    "and leave the other axis to the page" in {
        for
            right <- after(0, UI.Keyboard.ArrowRight)
            left  <- after(2, UI.Keyboard.ArrowLeft)
        yield assert(right == Absent && left == Absent)
    }

    "a horizontal dial answers the horizontal pair instead" in {
        for
            right <- after(0, UI.Keyboard.ArrowRight, SpeedDialDirection.Right)
            down  <- after(0, UI.Keyboard.ArrowDown, SpeedDialDirection.Right)
        yield assert(right == Present("sd-i2") && down == Absent)
    }

    "the fan comes round, because a menu comes round" in {
        for
            past   <- after(2, UI.Keyboard.ArrowDown)
            before <- after(0, UI.Keyboard.ArrowUp)
        yield assert(past == Present("sd-i0") && before == Present("sd-i2"))
    }

    "Home and End reach the ends" in {
        for
            home <- after(2, UI.Keyboard.Home)
            end  <- after(0, UI.Keyboard.End)
        yield assert(home == Present("sd-i0") && end == Present("sd-i2"))
    }

    "Escape on an action closes the fan" in {
        for
            (openRef, _, ui) <- fan()
            items            <- elementsWithClass(ui, "p-speeddial-item")
            btn              <- elements(items(2)).map(_.find(_.attrs.role.contains("menuitem")).get)
            _                <- press(btn, UI.Keyboard.Escape)
            still            <- openRef.get
        yield assert(!still)
    }

    "Enter is left to the browser, since an action IS a button" in
        after(0, UI.Keyboard.Enter).map(to => assert(to == Absent, "no third activation from the key handler"))

    "the arrow that points into the fan opens it from the toggle" in {
        for
            (openRef, _, ui) <- fan(open = false)
            toggle           <- elementWithClass(ui, "p-speeddial-button")
            _                <- press(toggle, UI.Keyboard.ArrowUp)
            opened           <- openRef.get
        yield assert(opened)
    }

    "the arrow that points away from it belongs to the page" in {
        for
            (openRef, _, ui) <- fan(open = false)
            toggle           <- elementWithClass(ui, "p-speeddial-button")
            _                <- press(toggle, UI.Keyboard.ArrowDown)
            opened           <- openRef.get
        yield assert(!opened)
    }

    "on an already-open fan that arrow moves focus in, instead of doing nothing" in {
        for
            (_, moved, ui) <- fan()
            toggle         <- elementWithClass(ui, "p-speeddial-button")
            _              <- press(toggle, UI.Keyboard.ArrowUp)
            got            <- moved.get
        yield assert(got == List("sd-i0"))
    }

    "Escape on the toggle closes the fan too" in {
        for
            (openRef, _, ui) <- fan()
            toggle           <- elementWithClass(ui, "p-speeddial-button")
            _                <- press(toggle, UI.Keyboard.Escape)
            still            <- openRef.get
        yield assert(!still)
    }

    "a separator is not an action, so the fan does not grow an empty button" in {
        for
            openRef <- Signal.initRef(true)
            ui = uic.SpeedDial()
                .items(uic.MenuItem("Add").icon(Icons.pencil).onSelect(()), uic.MenuItem.separator)
                .open(openRef)
                .wired(openRef, "sd", _ => ())
            items <- elementsWithClass(ui, "p-speeddial-item")
        yield assert(items.size == 1)
    }

    "a dial with nothing to reach claims no keys from the page" in {
        for
            openRef <- Signal.initRef(false)
            ui = uic.SpeedDial().open(openRef).wired(openRef, "sd", _ => ())
            root <- elementWithClass(ui, "p-speeddial")
        yield assert(!root.attrs.dataAttrs.contains("kyo-scroll-keys"))
    }

end SpeedDialTest
