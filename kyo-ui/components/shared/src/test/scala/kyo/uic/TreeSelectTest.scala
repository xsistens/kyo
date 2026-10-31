package kyo.uic

import kyo.*
import kyo.UI.*

/** Where `uic.TreeSelect` puts the keyboard highlight when its panel opens, and what carries it
  * away again.
  *
  * The trigger is the combobox and keeps focus for as long as the panel is open, so the hosted
  * [[Tree]]'s keyboard runs from there. Two keys are the trigger's own either way: the one that
  * opens the panel, which has to land ON a row, and the Tab that leaves the widget, which has to
  * take the panel with it. Its selection, the branches open in its panel and its own open state are
  * held to rule A ([[ReactiveValueContract]]).
  */
class TreeSelectTest extends ReactiveValueContract:

    private val nodes = List(
        TreeNode("src", "src", children = List(TreeNode("main", "main"), TreeNode("test", "test"))),
        TreeNode("build.sbt", "build")
    )

    private def treeSelect(using Frame) = uic.TreeSelect[String]().nodes(nodes*).id("ts")

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
            ui = treeSelect.selection(value).expanded(exp).wired(open, exp, hi, "ts")
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
            ui = treeSelect.selection(value).expanded(exp).wired(open, exp, hi, "ts")
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
            ui = treeSelect.selection(value).expanded(exp).wired(open, exp, hi, "ts")
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
            ui = treeSelect.selection(value).expanded(exp).wired(open, exp, hi, "ts")
            trigger <- elementWithClass(ui, "p-treeselect")
        yield
            assert(trigger.attrs.role.contains("combobox"))
            assert(trigger.attrs.ariaAttrs.get("controls").contains("ts-list"))
            assert(trigger.attrs.ariaAttrs.get("activedescendant").contains("ts-node-1"))
    }

    private def classed(cls: String)(e: UI.Ast.Element): Boolean = e.attrs.cssClasses.contains(cls)

    private def saying(cls: String, text: String)(e: UI.Ast.Element): Boolean =
        classed(cls)(e) && e.children.exists {
            case t: UI.Ast.Text => t.value == text
            case _              => false
        }

    /** The tree select wired with its panel `open`, nothing selected and nothing expanded. */
    private def field(base: uic.TreeSelect[String], open: Boolean)(using
        Frame
    ): (SignalRef[Boolean], SignalRef[Set[String]], SignalRef[Set[String]], SignalRef[Int], UI) < Async =
        for
            o     <- Signal.initRef(open)
            exp   <- Signal.initRef(Set.empty[String])
            hi    <- Signal.initRef(-1)
            value <- Signal.initRef(Set.empty[String])
        yield (o, exp, value, hi, base.selection(value).expanded(exp).wired(o, exp, hi, "ts"))

    "the wired field is ONE reactive region over everything it reads, open or closed" in {
        for
            loading             <- Signal.initRef(false)
            ph                  <- Signal.initRef("Pick a node")
            (open, _, _, _, ui) <- field(treeSelect.loading(loading).placeholder(ph), open = true)
            whenOpen            <- regionsAbove(ui, "p-treeselect")
            panel               <- regionsAbove(ui, "p-treeselect-tree-container")
            _                   <- open.set(false)
            whenClosed          <- regionsAbove(ui, "p-treeselect")
        yield assert(
            whenOpen == 1 && panel == 1 && whenClosed == 1,
            s"a pick writes the value and closes the panel; a nested region can repaint the older state: $whenOpen / $panel / $whenClosed"
        )
    }

    "a click anywhere on the field opens it onto a row, the element carrying its id included" in {
        for
            (byId, _, _, hi, ui1)  <- field(treeSelect, open = false)
            _                      <- clickBubbling(ui1)(_.attrs.identifier.contains("ts"))
            viaId                  <- byId.get
            at                     <- hi.get
            (byIcon, _, _, _, ui2) <- field(treeSelect, open = false)
            _                      <- clickBubbling(ui2)(classed("p-treeselect-dropdown"))
            viaChevron             <- byIcon.get
            (shut, _, _, _, ui3)   <- field(treeSelect, open = true)
            _                      <- clickBubbling(ui3)(_.attrs.identifier.contains("ts"))
            closed                 <- shut.get
        yield
            assert(viaId, "the field itself is the control, so a click on its padding or by its id opens it")
            assert(at == 0, "a pointer opens through the same path as a key, so the next arrow moves rather than starts")
            assert(viaChevron && !closed)
        end for
    }

    "a click on a node picks it once and closes the panel" in {
        for
            seen                    <- Signal.initRef(0)
            (open, _, value, _, ui) <- field(treeSelect.onChange(_ => seen.updateAndGet(_ + 1).unit), open = true)
            _                       <- clickBubbling(ui)(saying("p-tree-node-label", "build.sbt"))
            got                     <- value.get
            changes                 <- seen.get
            still                   <- open.get
        yield assert(got == Set("build") && changes == 1 && !still)
    }

    "in Multiple mode a click on a node toggles it and the panel stays open" in {
        for
            (open, _, value, _, ui) <- field(treeSelect.selectionMode(SelectionMode.Multiple), open = true)
            _                       <- clickBubbling(ui)(saying("p-tree-node-label", "build.sbt"))
            got                     <- value.get
            still                   <- open.get
        yield assert(got == Set("build") && still)
    }

    "a click inside the open panel that selects no node leaves it open and picks nothing" in {
        val targets: List[(String, UI.Ast.Element => Boolean)] = List(
            ("expand toggle", classed("p-tree-node-toggle-button")),
            ("tree space", classed("p-treeselect-tree-container")),
            ("panel frame", classed("p-treeselect-overlay"))
        )
        Kyo.foreach(targets) { (name, target) =>
            for
                (open, _, value, _, ui) <- field(treeSelect, open = true)
                _                       <- clickBubbling(ui)(target)
                still                   <- open.get
                got                     <- value.get
            yield (name, still && got.isEmpty)
        }.map(rs => assert(rs.forall(_._2), rs.filterNot(_._2).map(_._1).mkString("toggled or picked: ", ", ", "")))
    }

    "the expand toggle opens its branch without closing the panel" in {
        for
            (open, exp, _, _, ui) <- field(treeSelect, open = true)
            _                     <- clickBubbling(ui)(classed("p-tree-node-toggle-button"))
            expanded              <- exp.get
            still                 <- open.get
        yield assert(expanded == Set("src") && still)
    }

    "the field is the one element listening for a click: not its label, not its chevron" in {
        def listening(ui: UI)(using Frame): Chunk[String] < Sync =
            elements(ui).map(_.filter(declaresClick).map(_.attrs.cssClasses.headOption.getOrElse("?")))
        for
            (_, _, _, _, closed) <- field(treeSelect, open = false)
            live                 <- listening(closed)
            (_, _, _, _, off)    <- field(treeSelect.disabled(true), open = false)
            disabled             <- listening(off)
        yield
            assert(live == Chunk("p-treeselect"), live.toString)
            assert(disabled.isEmpty, "a disabled field opens nothing")
        end for
    }

    // The contract below picks a root node; this one picks inside an open branch.
    "a pick inside an open branch is reported to a selection bound one way, without being written" in {
        for
            held <- Signal.initRef(Set.empty[String])
            seen <- Signal.initRef(Set.empty[String])
            open <- Signal.initRef(true)
            exp  <- Signal.initRef(Set("src"))
            hi   <- Signal.initRef(-1)
            ui = treeSelect.selection(held.readOnly).onChange(seen.set).expanded(exp).wired(open, exp, hi, "ts")
            main <- elementsWithClass(ui, "p-tree-node-label").map(_.find(textOf(_) == "main").get)
            _    <- click(main)
            h    <- held.get
            s    <- seen.get
        yield assert(h.isEmpty && s == Set("main"), s"held $h, reported $s")
        end for
    }

    // ---- rule A ----

    /** The trigger, which opens the panel and carries the tree's keyboard: what a reader reaches, and what says it is fixed. */
    private def trigger(t: UI)(using Frame): UI.Ast.Element < Sync = elementWithClass(t, "p-treeselect")

    private def opens(c: UI.Ast.Element): Boolean = declaresClick(c) || c.attrs.onKeyDown.nonEmpty

    private def readOnly(c: UI.Ast.Element): Boolean = c.attrs.ariaAttrs.get("readonly").contains("true")

    private def triggered(t: UI)(using Frame): Boolean < Sync = elementsWithClass(t, "p-treeselect").map(_.exists(declaresClick))

    private val idsByLabel = Map("src" -> "src", "main" -> "main", "test" -> "test", "build.sbt" -> "build")

    /** The ids of the nodes the label names. */
    private def named(t: UI)(using Frame): Set[String] < Sync =
        elementWithClass(t, "p-treeselect-label").map(l => textOf(l).split(", ").toSet.flatMap(idsByLabel.get))

    holds(new Slot[uic.TreeSelect[String], Set[String]](
        name = "selection",
        bare = Present(treeSelect),
        bind = treeSelect.selection(_),
        hear = (s, f) => s.onChange(f),
        event = "onChange",
        render = _.render,
        start = Set.empty,
        next = Set("build"),
        control = trigger(_),
        shows = named(_),
        answers = opens,
        saysFixed = readOnly,
        ready = Present(triggered(_)),
        after = Seq(t => clickBubbling(t)(saying("p-tree-node-label", "build.sbt")))
    ))

    holds(new Slot[uic.TreeSelect[String], Boolean](
        name = "open",
        bare = Present(treeSelect),
        bind = treeSelect.open(_),
        hear = (s, f) => s.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = false,
        next = true,
        control = trigger(_),
        shows = opened(_),
        answers = opens,
        saysFixed = readOnly,
        ready = Present(triggered(_)),
        // A click on the chevron reaches the trigger: the field is the one element listening.
        act = Present(clickBubbling(_)(_.attrs.cssClasses.contains("p-treeselect-dropdown")))
    ))

    /** The expand toggle of the `src` branch, the one branch at the root. */
    private def srcToggle(t: UI)(using Frame): UI.Ast.Element < Sync =
        elementsWithClass(t, "p-tree-node-toggle-button").map(_.headOption.getOrElse(throw new AssertionError("no toggle")))

    /** The branches the panel's tree draws open: `src`, the one branch, where its children show. */
    private def branchesOpen(t: UI)(using Frame): Set[String] < Sync =
        elementsWithClass(t, "p-tree-node-label").map(ls => if ls.exists(textOf(_) == "main") then Set("src") else Set.empty)

    // The branches open in the panel, which only the mount draws open: the panel is held open here, so the expansion is the
    // slot under test. The toggle is what a pointer opens a branch with. A tree has no readonly state for its expansion and
    // its toggles are hidden from assistive tech, so a fixed expansion says nothing beyond each branch's aria-expanded: it
    // is fixed because no toggle answers (the tree's keys ignore a fixed expansion too, Tree.toggleNodeLive's guard).
    holds(new Slot[uic.TreeSelect[String], Set[String]](
        name = "expanded",
        bare = Present(treeSelect.open(true)),
        bind = treeSelect.open(true).expanded(_),
        hear = (s, f) => s.onExpandedChange(f),
        event = "onExpandedChange",
        render = _.render,
        start = Set.empty,
        next = Set("src"),
        control = srcToggle(_),
        shows = branchesOpen(_),
        isFixed = Present(elementsWithClass(_, "p-tree-node-toggle-button").map(ts => ts.nonEmpty && ts.forall(t => !declaresClick(t)))),
        ready = Present(elementsWithClass(_, "p-tree-node-toggle-button").map(_.nonEmpty)),
        mountDraws = true
    ))

    private def opened(t: UI)(using Frame) = elementsWithClass(t, "p-treeselect-overlay").map(_.nonEmpty)

    /** The field as its mount publishes it, the trigger wired. */
    private def mounted(ui: UI)(using Frame, kyo.test.AssertScope): UI < Async =
        mountedWhen(ui)(t => elementsWithClass(t, "p-treeselect-dropdown").map(_.nonEmpty))

    "a panel fixed open stays: nothing outside dismisses it" in {
        for
            t     <- mounted(treeSelect.open(true).render)
            shown <- opened(t)
            out   <- elementsWithClass(t, "p-uic-overlay-backdrop")
        yield
            assert(shown, "the panel shows")
            assert(out.isEmpty, "no backdrop takes an outside click")
    }

end TreeSelectTest
