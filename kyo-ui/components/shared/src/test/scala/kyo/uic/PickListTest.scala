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

    private def button(ui: UI, name: String)(using Frame): UI.Ast.Button < Sync =
        elements(ui).map(_.collectFirst {
            case b: UI.Ast.Button if b.attrs.ariaAttrs.get("label").contains(name) => b
        }.getOrElse(throw new AssertionError(s"no button labelled $name")))

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

end PickListTest
