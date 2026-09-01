package kyo.uic

import kyo.*
import kyo.UI.*

/** Where `uic.TreeSelect` puts the keyboard highlight when its panel opens, and what carries it
  * away again.
  *
  * The trigger is the combobox and keeps focus for as long as the panel is open, so the hosted
  * [[Tree]]'s keyboard runs from there. Two keys are the trigger's own either way: the one that
  * opens the panel, which has to land ON a row, and the Tab that leaves the widget, which has to
  * take the panel with it.
  */
class TreeSelectTest extends UicTest:

    private val nodes = List(
        TreeNode("src", "src", children = List(TreeNode("main", "main"), TreeNode("test", "test"))),
        TreeNode("build.sbt", "build")
    )

    private def treeSelect(using Frame) = uic.TreeSelect().nodes(nodes*).id("ts")

    /** Presses `key` on the CLOSED trigger and reports whether the panel opened and which visible
      * row the highlight landed on.
      */
    private def opening(key: UI.Keyboard, selected: Set[String], expanded: Set[String] = Set.empty)(using
        Frame
    ): (Boolean, Int) < Async =
        for
            open  <- Signal.initRef(false)
            exp   <- Signal.initRef(expanded)
            hi    <- Signal.initRef(-1)
            value <- Signal.initRef(selected)
            ui = treeSelect.value(value).expanded(exp).wired(open, exp, hi, "ts")
            trigger <- elementWithClass(ui, "p-treeselect")
            _       <- press(trigger, key)
            isOpen  <- open.get
            at      <- hi.get
        yield (isOpen, at)

    /** Whether the open panel is still open after `key`. */
    private def stillOpen(key: UI.Keyboard, mods: UI.Modifiers = UI.Modifiers.none)(using Frame): Boolean < Async =
        for
            open  <- Signal.initRef(true)
            exp   <- Signal.initRef(Set.empty[String])
            hi    <- Signal.initRef(1)
            value <- Signal.initRef(Set.empty[String])
            ui = treeSelect.value(value).expanded(exp).wired(open, exp, hi, "ts")
            trigger <- elementWithClass(ui, "p-treeselect")
            _       <- press(trigger, key)
            still   <- open.get
        yield still

    "an opening key lands on the first row, so the next arrow moves rather than starts" in {
        for
            down  <- opening(UI.Keyboard.ArrowDown, Set.empty)
            enter <- opening(UI.Keyboard.Enter, Set.empty)
        yield assert(down == (true, 0) && enter == (true, 0))
    }

    "and on the selected row where there is one" in
        opening(UI.Keyboard.ArrowDown, Set("build")).map((open, at) =>
            assert(open && at == 1, "the second visible row, since src is collapsed")
        )

    "a selected row inside a collapsed branch is not a row on the screen, so the first one holds" in
        opening(UI.Keyboard.ArrowDown, Set("test")).map((open, at) => assert(open && at == 0))

    "an expanded branch counts its children among the rows the highlight lands on" in
        opening(UI.Keyboard.ArrowDown, Set("build"), expanded = Set("src")).map((open, at) =>
            assert(open && at == 3, "src, main, test, build.sbt")
        )

    "Tab closes the panel, in both directions" in {
        for
            fwd  <- stillOpen(UI.Keyboard.Tab)
            back <- stillOpen(UI.Keyboard.Tab, UI.Modifiers(shift = true))
        yield assert(!fwd && !back, "the tree's keyboard lives on the trigger the Tab is leaving")
    }

    "Escape closes it too, and the arrows still reach the tree" in {
        for
            esc   <- stillOpen(UI.Keyboard.Escape)
            open  <- Signal.initRef(true)
            exp   <- Signal.initRef(Set.empty[String])
            hi    <- Signal.initRef(0)
            value <- Signal.initRef(Set.empty[String])
            ui = treeSelect.value(value).expanded(exp).wired(open, exp, hi, "ts")
            trigger <- elementWithClass(ui, "p-treeselect")
            _       <- press(trigger, UI.Keyboard.ArrowDown)
            at      <- hi.get
        yield assert(!esc && at == 1)
    }

    "the trigger is the combobox, and announces the row the highlight is on" in {
        for
            open  <- Signal.initRef(true)
            exp   <- Signal.initRef(Set.empty[String])
            hi    <- Signal.initRef(1)
            value <- Signal.initRef(Set.empty[String])
            ui = treeSelect.value(value).expanded(exp).wired(open, exp, hi, "ts")
            trigger <- elementWithClass(ui, "p-treeselect")
        yield
            assert(trigger.attrs.role.contains("combobox"))
            assert(trigger.attrs.ariaAttrs.get("controls").contains("ts-list"))
            assert(trigger.attrs.ariaAttrs.get("activedescendant").contains("ts-node-1"))
    }

end TreeSelectTest
