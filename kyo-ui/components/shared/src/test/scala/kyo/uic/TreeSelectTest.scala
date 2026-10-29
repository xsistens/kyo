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

    // The chosen nodes live somewhere else: the field shows them and hands a pick in its panel over.
    "a selection bound one way is shown, and a pick is reported without being written" in {
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
            deaf <- elementsWithClass(treeSelect.selection(held.readOnly).expanded(exp).wired(open, exp, hi, "ts"), "p-uic-key-error")
        yield
            assert(h.isEmpty && s == Set("main"), s"held $h, reported $s")
            assert(deaf.map(textOf).mkString.contains("onChange"), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

    "a constant nobody hears is fixed: no panel to open, and no card" in {
        val fixed = treeSelect.selection(Set("src"))
        for
            card  <- elementsWithClass(fixed.render, "p-uic-key-error")
            field <- elementWithClass(fixed.render, "p-treeselect")
        yield
            assert(card.isEmpty, s"a fixed constant: ${card.map(textOf)}")
            assert(!declaresClick(field) && field.attrs.onKeyDown.isEmpty, "a fixed field opens nothing")
            assert(field.attrs.ariaAttrs.get("readonly").contains("true"), "and says it is read-only")
        end for
    }

    "an unbound tree select keeps the node the reader picks" in {
        for
            after <- publishedAfterReady(
                treeSelect.render,
                t => elements(t).map(_.exists(e => e.attrs.identifier.contains("ts") && declaresClick(e))),
                until = t => elementsWithClass(t, "p-treeselect-label").map(_.exists(e => textOf(e) == "build.sbt"))
            )(
                t => clickBubbling(t)(_.attrs.identifier.contains("ts")),
                t => clickBubbling(t)(saying("p-tree-node-label", "build.sbt"))
            )
        yield assert(after.nonEmpty, "the field never showed the node the reader picked")
    }

    private def opened(t: UI)(using Frame) = elementsWithClass(t, "p-treeselect-overlay").map(_.nonEmpty)

    /** The field as its mount publishes it, the trigger wired. */
    private def mounted(ui: UI)(using Frame, kyo.test.AssertScope): UI < Async =
        mountedWhen(ui)(t => elementsWithClass(t, "p-treeselect-dropdown").map(_.nonEmpty))

    private def onDropdown(e: UI.Ast.Element): Boolean = e.attrs.cssClasses.contains("p-treeselect-dropdown")

    "a panel fixed shut draws the field read-only, and says nothing" in {
        for
            field <- elementWithClass(treeSelect.open(false).render, "p-treeselect")
            card  <- elementsWithClass(treeSelect.open(false).render, "p-uic-key-error")
        yield
            assert(field.attrs.ariaAttrs.get("readonly").contains("true"), "nothing may open it")
            assert(card.isEmpty, s"a fixed state is not a mistake: ${card.map(textOf)}")
    }

    "a panel fixed open stays: nothing outside dismisses it" in {
        for
            t     <- mounted(treeSelect.open(true).render)
            shown <- opened(t)
            out   <- elementsWithClass(t, "p-uic-overlay-backdrop")
        yield
            assert(shown, "the panel shows")
            assert(out.isEmpty, "no backdrop takes an outside click")
    }

    "a panel bound one way is followed, and a press is reported without being written" in {
        for
            held <- Signal.initRef(false)
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            t    <- mounted(treeSelect.open(held.readOnly).onOpenChange(v => seen.set(Present(v))).render)
            _    <- clickBubbling(t)(onDropdown)
            h    <- held.get
            s    <- seen.get
            deaf <- elementsWithClass(treeSelect.open(held.readOnly).render, "p-uic-key-error")
        yield
            assert(!h && s == Present(true), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onOpenChange"), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

    "a panel ref is written before the field reports it" in {
        for
            held <- Signal.initRef(false)
            seen <- Signal.initRef((false, false))
            t    <- mounted(treeSelect.open(held).onOpenChange(v => held.get.map(h => seen.set((v, h)))).render)
            _    <- clickBubbling(t)(onDropdown)
            s    <- seen.get
        yield assert(s == (true, true), s"reported, held: $s")
    }

end TreeSelectTest
