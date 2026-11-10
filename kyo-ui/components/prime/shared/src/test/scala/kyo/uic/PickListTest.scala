package kyo.uic

import kyo.*
import kyo.UI.*

/** `uic.PickList`'s transfer buttons, and the focus they used to drop.
  *
  * Every one of them was NATIVELY disabled while its side had no selection, and a transfer
  * clears that selection. So the button a reader had just pressed went disabled under their
  * hands, and a natively disabled button loses focus to the document: one keystroke moved the
  * items and sent the reader back to the top of the page.
  *
  * The library already had the answer, written in [[Button.ariaDisabled]]'s own scaladoc: a
  * control that can disable itself as a RESULT of being pressed stays in the tab order and says
  * `aria-disabled` instead.
  *
  * Both columns and both selections are held to rule A ([[ReactiveValueContract]]).
  */
class PickListTest extends ReactiveValueContract:

    private def picks(source: Seq[String], target: Seq[String], selected: Set[String])(using
        Frame
    ): (SignalRef[Seq[String]], SignalRef[Seq[String]], SignalRef[Set[String]], UI) < Async =
        for
            src    <- Signal.initRef(source)
            tgt    <- Signal.initRef(target)
            srcSel <- Signal.initRef(selected)
            tgtSel <- Signal.initRef(Set.empty[String])
            ui = uic.PickList[String]()
                .sourceItems(src)(identity)
                .targetItems(tgt)
                .sourceSelection(srcSel)
                .targetSelection(tgtSel)
                .render
        yield (src, tgt, srcSel, ui)

    /** A PickList wired the way its mount wires it, with a cursor per column. */
    private def wired(source: Seq[String], target: Seq[String], selected: Set[String], highlight: Int = -1)(using
        Frame
    ): (SignalRef[Seq[String]], SignalRef[Seq[String]], SignalRef[Set[String]], UI) < Async =
        for
            src    <- Signal.initRef(source)
            tgt    <- Signal.initRef(target)
            srcSel <- Signal.initRef(selected)
            tgtSel <- Signal.initRef(Set.empty[String])
            srcHi  <- Signal.initRef(highlight)
            tgtHi  <- Signal.initRef(-1)
            ui = uic.PickList[String]()
                .sourceItems(src)(identity)
                .targetItems(tgt)
                .sourceSelection(srcSel)
                .targetSelection(tgtSel)
                .wired(Present(uic.ListReorder.Cursor(srcHi, "s")), Present(uic.ListReorder.Cursor(tgtHi, "t")))
        yield (src, tgt, srcSel, ui)

    /** The source column's list, which is the element that holds the focus and answers the keys. */
    private def sourceList(ui: UI)(using Frame): UI.Ast.Element < Sync =
        elementsWithClass(ui, "p-listbox-list").map(_.head)

    private def button(ui: UI, name: String)(using Frame): UI.Ast.Button < Sync =
        elements(ui).map(_.collectFirst {
            case b: UI.Ast.Button if b.attrs.ariaAttrs.get("label").contains(name) => b
        }.getOrElse(throw new AssertionError(s"no button labelled $name")))

    "the whole control is ONE reactive region, so nothing can repaint an older snapshot" in {
        for
            (_, _, _, ui) <- picks(Seq("a", "b"), Nil, Set("a"))
            regions       <- regionsAbove(ui, "p-picklist")
        yield assert(regions == 1, "four nested renders let a transfer be undone by the next click on a row")
    }

    "a transfer button stays in the tab order, because pressing it is what turns it off" in {
        for
            (_, _, _, ui) <- picks(Seq("a", "b"), Nil, Set("a"))
            move          <- button(ui, "Move to Target")
        yield
            assert(!move.disabled.contains(true), "not natively disabled, or the press loses its own focus")
            assert(move.attrs.ariaAttrs.get("disabled").contains("false"), "with a selection it is simply enabled: aria-disabled=\"false\"")
    }

    "with nothing selected it says so instead of leaving" in {
        for
            (_, _, _, ui) <- picks(Seq("a", "b"), Nil, Set.empty)
            move          <- button(ui, "Move to Target")
        yield
            assert(!move.disabled.contains(true))
            assert(move.attrs.ariaAttrs.get("disabled").contains("true"))
            assert(move.attrs.cssClasses.contains("p-uic-aria-disabled"), "and it still looks unavailable")
    }

    "pressing it transfers the selection and clears it" in {
        for
            (src, tgt, sel, ui) <- picks(Seq("a", "b"), Nil, Set("a"))
            move                <- button(ui, "Move to Target")
            _                   <- click(move)
            left                <- src.get
            moved               <- tgt.get
            still               <- sel.get
        yield assert(left == Seq("b") && moved == Seq("a") && still.isEmpty)
    }

    "and pressing it again does nothing, since it is off rather than gone" in {
        for
            (src, tgt, _, ui) <- picks(Seq("a", "b"), Nil, Set.empty)
            move              <- button(ui, "Move to Target")
            _                 <- click(move)
            left              <- src.get
            moved             <- tgt.get
        yield assert(left == Seq("a", "b") && moved.isEmpty, "an off button that keeps its click keeps it inert")
    }

    "the whole-column transfer follows the same rule" in {
        for
            (src, tgt, _, ui) <- picks(Nil, Seq("a"), Set.empty)
            all               <- button(ui, "Move All to Target")
            _                 <- click(all)
            left              <- src.get
            moved             <- tgt.get
        yield
            assert(all.attrs.ariaAttrs.get("disabled").contains("true"), "an empty column has nothing to move")
            assert(!all.disabled.contains(true))
            assert(left.isEmpty && moved == Seq("a"))
    }

    "the reorder rails answer the same way" in {
        for
            (_, _, _, ui) <- picks(Seq("a", "b"), Nil, Set.empty)
            up            <- button(ui, "Move Up")
        yield
            assert(!up.disabled.contains(true))
            assert(up.attrs.ariaAttrs.get("disabled").contains("true"))
    }

    "a column is ONE tab stop with a highlight roving inside it" in {
        for
            (_, _, _, ui) <- wired(Seq("a", "b"), Nil, Set.empty)
            list          <- sourceList(ui)
            rows          <- elementsWithClass(ui, "p-listbox-option")
        yield
            assert(list.attrs.tabIndex.contains(0), "the list takes the Tab")
            assert(rows.forall(_.attrs.tabIndex.isEmpty), "and a row is not a tab stop of its own")
            assert(list.attrs.onKeyDown.isDefined)
    }

    "the arrow pointing at the other column transfers the selection into it" in {
        for
            (src, tgt, sel, ui) <- wired(Seq("a", "b"), Nil, Set("a"))
            list                <- sourceList(ui)
            _                   <- press(list, UI.Keyboard.ArrowRight)
            left                <- src.get
            moved               <- tgt.get
            still               <- sel.get
        yield assert(left == Seq("b") && moved == Seq("a") && still.isEmpty, "the same move the button makes")
    }

    "and carried, it sends the whole column" in {
        for
            (src, tgt, _, ui) <- wired(Seq("a", "b"), Nil, Set.empty)
            list              <- sourceList(ui)
            _                 <- press(list, UI.Keyboard.ArrowRight, UI.Modifiers.none.copy(meta = true))
            left              <- src.get
            moved             <- tgt.get
        yield assert(left.isEmpty && moved == Seq("a", "b"))
    }

    "a carried vertical arrow reorders the selection inside its own column" in {
        for
            (src, _, _, ui) <- wired(Seq("a", "b", "c"), Nil, Set("a"))
            list            <- sourceList(ui)
            _               <- press(list, UI.Keyboard.ArrowDown, UI.Modifiers.none.copy(ctrl = true))
            order           <- src.get
        yield assert(order == Seq("b", "a", "c"))
    }

    "and with Shift it goes to the end" in {
        for
            (src, _, _, ui) <- wired(Seq("a", "b", "c"), Nil, Set("a"))
            list            <- sourceList(ui)
            _               <- press(list, UI.Keyboard.ArrowDown, UI.Modifiers.none.copy(ctrl = true, shift = true))
            order           <- src.get
        yield assert(order == Seq("b", "c", "a"))
    }

    "a plain vertical arrow is still the list's own, and moves nothing but the highlight" in {
        for
            (src, tgt, _, ui) <- wired(Seq("a", "b"), Nil, Set("a"))
            list              <- sourceList(ui)
            _                 <- press(list, UI.Keyboard.ArrowDown)
            left              <- src.get
            moved             <- tgt.get
        yield assert(left == Seq("a", "b") && moved.isEmpty)
    }

    "the highlight follows the row a move carries, rather than staying on the index" in {
        for
            src    <- Signal.initRef(Seq("a", "b", "c"))
            tgt    <- Signal.initRef(Seq.empty[String])
            srcSel <- Signal.initRef(Set("a"))
            tgtSel <- Signal.initRef(Set.empty[String])
            srcHi  <- Signal.initRef(0)
            tgtHi  <- Signal.initRef(-1)
            ui = uic.PickList[String]()
                .sourceItems(src)(identity).targetItems(tgt).sourceSelection(srcSel).targetSelection(tgtSel)
                .wired(Present(uic.ListReorder.Cursor(srcHi, "s")), Present(uic.ListReorder.Cursor(tgtHi, "t")))
            list  <- sourceList(ui)
            _     <- press(list, UI.Keyboard.ArrowDown, UI.Modifiers.none.copy(meta = true))
            order <- src.get
            where <- srcHi.get
        yield assert(order == Seq("b", "a", "c") && where == 1, "the reader keeps their place")
    }

    "and where a transfer takes the row away, the highlight stays in the column" in {
        for
            src    <- Signal.initRef(Seq("a", "b"))
            tgt    <- Signal.initRef(Seq.empty[String])
            srcSel <- Signal.initRef(Set("b"))
            tgtSel <- Signal.initRef(Set.empty[String])
            srcHi  <- Signal.initRef(1)
            tgtHi  <- Signal.initRef(-1)
            ui = uic.PickList[String]()
                .sourceItems(src)(identity).targetItems(tgt).sourceSelection(srcSel).targetSelection(tgtSel)
                .wired(Present(uic.ListReorder.Cursor(srcHi, "s")), Present(uic.ListReorder.Cursor(tgtHi, "t")))
            list  <- sourceList(ui)
            _     <- press(list, UI.Keyboard.ArrowRight)
            left  <- src.get
            where <- srcHi.get
        yield assert(left == Seq("a") && where == 0, "clamped to what is left, not left pointing past the end")
    }

    "the arrow pointing away from the other column is nobody's" in {
        for
            (src, tgt, _, ui) <- wired(Seq("a", "b"), Nil, Set("a"))
            list              <- sourceList(ui)
            _                 <- press(list, UI.Keyboard.ArrowLeft)
            left              <- src.get
            moved             <- tgt.get
        yield assert(left == Seq("a", "b") && moved.isEmpty)
    }

    // The two columns and their selections live somewhere else: the control shows them and hands a transfer over.
    "columns and selections bound one way are reported on a transfer and not written" in {
        for
            src     <- Signal.initRef(Seq("a", "b"))
            tgt     <- Signal.initRef(Seq("c"))
            srcSel  <- Signal.initRef(Set("a"))
            seenSrc <- Signal.initRef(Seq("-"))
            seenTgt <- Signal.initRef(Seq("-"))
            seenSel <- Signal.initRef(Set("-"))
            ui = uic.PickList[String]()
                .sourceItems(src.readOnly)(identity).onSourceItemsChange(seenSrc.set)
                .targetItems(tgt.readOnly).onTargetItemsChange(seenTgt.set)
                .sourceSelection(srcSel.readOnly).onSourceSelectionChange(seenSel.set)
                .targetSelection(Set.empty[String]).onTargetSelectionChange(_ => ())
                .render
            move <- button(ui, "Move to Target")
            _    <- click(move)
            s    <- src.get
            t    <- tgt.get
            sel  <- srcSel.get
            rs   <- seenSrc.get
            rt   <- seenTgt.get
            rsel <- seenSel.get
        yield
            assert(s == Seq("a", "b") && t == Seq("c") && sel == Set("a"), s"held $s, $t, $sel")
            assert(rs == Seq("b") && rt == Seq("c", "a") && rsel.isEmpty, s"reported $rs, $rt, $rsel")
    }

    "disabled columns say nothing about who hears them, since the reader cannot change them" in {
        for
            src <- Signal.initRef(Seq("a"))
            off <- elementsWithClass(
                uic.PickList[String]().sourceItems(src.readOnly)(identity).targetItems(Seq.empty[String]).disabled(true).render,
                "p-uic-diagnostic"
            )
        yield assert(off.isEmpty, s"nobody can change them: ${off.map(textOf)}")
    }

    // ---- rule A ----

    /** The buttons inside the element with class `cls` (a rail, or the transfer controls). */
    private def buttonsIn(t: UI, cls: String)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elementWithClass(t, cls).map(c => elements(c).map(_.filter(_.attrs.cssClasses.contains("p-button"))))

    /** The button labelled `name` inside the element with class `cls`: both rails have a "Move Down". */
    private def buttonIn(t: UI, cls: String, name: String)(using Frame): UI.Ast.Element < Sync =
        buttonsIn(t, cls).map(_.find(_.attrs.ariaAttrs.get("label").contains(name)).getOrElse(
            throw new AssertionError(s"no button labelled $name in $cls")
        ))

    /** Whether every button in the elements with these classes moves nothing and says so. */
    private def buttonsFixed(t: UI, classes: String*)(using Frame): Boolean < Sync =
        Kyo.foreach(classes)(buttonsIn(t, _)).map(_.flatten).map(bs =>
            bs.nonEmpty && bs.forall(b => !declaresClick(b) && b.attrs.ariaAttrs.get("disabled").contains("true"))
        )

    /** The column container with class `cls`, and the rows and the list in it. */
    private def columnRows(t: UI, cls: String)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elementWithClass(t, cls).map(c => elements(c).map(_.filter(_.attrs.cssClasses.contains("p-listbox-option"))))

    private def rowTexts(t: UI, cls: String)(using Frame): Seq[String] < Sync = columnRows(t, cls).map(_.map(textOf).toList)

    private def chosenIn(t: UI, cls: String)(using Frame): Set[String] < Sync =
        columnRows(t, cls).map(_.filter(_.attrs.cssClasses.contains("p-listbox-option-selected")).map(textOf).toSet)

    private def rowIn(t: UI, cls: String, text: String)(using Frame): UI.Ast.Element < Sync =
        columnRows(t, cls).map(_.find(textOf(_) == text).getOrElse(throw new AssertionError(s"no row $text in $cls")))

    /** Whether no row of the column picks anything, and its list says the selection is read-only. */
    private def picksFixed(t: UI, cls: String)(using Frame): Boolean < Sync =
        for
            rows <- columnRows(t, cls)
            list <- elementWithClass(t, cls).map(c => elements(c).map(_.find(_.attrs.cssClasses.contains("p-listbox-list"))))
        yield rows.nonEmpty && rows.forall(r => !declaresClick(r)) && list.exists(_.attrs.ariaAttrs.get("readonly").contains("true"))

    private val sourceCls = "p-picklist-source-list-container"
    private val targetCls = "p-picklist-target-list-container"

    // An unbound column starts empty, so no gesture reaches one; the target's unbound case is where a column of the
    // list's own shows. A reorder in the source rail moves the selected row, held at `a`.
    holds(new SlotCase[uic.PickList[String], Seq[String]](
        name = "sourceItems",
        bare = Absent,
        bind = uic.PickList[String]().sourceItems(_)(identity).sourceSelection(Set("a")),
        hear = (p, f) => p.onSourceItemsChange(f),
        event = "onSourceItemsChange",
        render = _.render,
        start = Seq("a", "b"),
        next = Seq("b", "a"),
        control = buttonIn(_, "p-picklist-source-controls", "Move Down"),
        shows = rowTexts(_, sourceCls),
        isFixed = Present(buttonsFixed(_, "p-picklist-source-controls"))
    ))

    // The source and its selection are heard constants, so a transfer changes only the target the list keeps or is bound to.
    private def toTransfer(using Frame) =
        uic.PickList[String]().sourceItems(Seq("a", "b"))(identity).onSourceItemsChange(_ => ())
            .sourceSelection(Set("a")).onSourceSelectionChange(_ => ())

    holds(new SlotCase[uic.PickList[String], Seq[String]](
        name = "targetItems",
        bare = Present(toTransfer),
        bind = toTransfer.targetItems(_),
        hear = (p, f) => p.onTargetItemsChange(f),
        event = "onTargetItemsChange",
        render = _.render,
        start = Seq.empty,
        next = Seq("a"),
        control = buttonIn(_, "p-picklist-transfer-controls", "Move to Target"),
        shows = rowTexts(_, targetCls),
        // A transfer changes both columns, so a fixed target holds every transfer, and its own rail.
        isFixed = Present(buttonsFixed(_, "p-picklist-transfer-controls", "p-picklist-target-controls")),
        // Unbound, the target is the mount's.
        ready = Present(t => elements(t).map(_.exists(e => e.attrs.ariaAttrs.get("label").contains("Move to Target") && declaresClick(e))))
    ))

    private def selectionSlot(name: String, base: uic.PickList[String], cls: String, row: String)(
        bind: (uic.PickList[String], Set[String] | Signal[Set[String]]) => Frame ?=> uic.PickList[String],
        hear: (uic.PickList[String], Set[String] => UI.Action) => Frame ?=> uic.PickList[String],
        event: String
    )(using Frame) = new SlotCase[uic.PickList[String], Set[String]](
        name = name,
        bare = Present(base),
        bind = bind(base, _),
        hear = hear,
        event = event,
        render = _.render,
        start = Set.empty,
        next = Set(row),
        control = rowIn(_, cls, row),
        shows = chosenIn(_, cls),
        isFixed = Present(picksFixed(_, cls)),
        // Unbound, the selection is the mount's.
        ready = Present(t => elementsWithClass(t, "p-listbox-option").map(_.exists(declaresClick)))
    )

    holds(selectionSlot("sourceSelection", uic.PickList[String]().sourceItems(Seq("a", "b"))(identity), sourceCls, "a")(
        _.sourceSelection(_),
        (p, f) => p.onSourceSelectionChange(f),
        "onSourceSelectionChange"
    ))

    holds(selectionSlot("targetSelection", uic.PickList[String]().targetItems(Seq("c", "d")), targetCls, "c")(
        _.targetSelection(_),
        (p, f) => p.onTargetSelectionChange(f),
        "onTargetSelectionChange"
    ))

end PickListTest
