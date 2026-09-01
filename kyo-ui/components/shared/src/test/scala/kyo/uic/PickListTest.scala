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
  */
class PickListTest extends UicTest:

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
                .sourceSelected(srcSel)
                .targetSelected(tgtSel)
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
                .sourceSelected(srcSel)
                .targetSelected(tgtSel)
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
            assert(move.attrs.ariaAttrs.get("disabled").isEmpty, "with a selection it is simply enabled")
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
                .sourceItems(src)(identity).targetItems(tgt).sourceSelected(srcSel).targetSelected(tgtSel)
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
                .sourceItems(src)(identity).targetItems(tgt).sourceSelected(srcSel).targetSelected(tgtSel)
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

end PickListTest
