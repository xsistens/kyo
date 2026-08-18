package kyo.uic

import kyo.*
import kyo.UI.*

/** Package-internal reorder semantics shared by [[OrderList]] and [[PickList]] —
  * PrimeVue's exact move algorithms over an ordered list with a selected subset.
  */
private[uic] object ListReorder:

    /** Each selected item swaps with its predecessor, scanning top-down; a selected
      * item already at the top stops the pass (Prime's break).
      */
    def moveUp[A](xs: List[A], isSel: A => Boolean): List[A] =
        val buf  = xs.toBuffer
        var i    = 0
        var stop = false
        while i < buf.length && !stop do
            if isSel(buf(i)) then
                if i == 0 then stop = true
                else
                    val tmp = buf(i - 1)
                    buf(i - 1) = buf(i)
                    buf(i) = tmp
            end if
            i += 1
        end while
        buf.toList
    end moveUp

    /** Each selected item swaps with its successor, scanning bottom-up; a selected
      * item already at the bottom stops the pass (Prime's break).
      */
    def moveDown[A](xs: List[A], isSel: A => Boolean): List[A] =
        val buf  = xs.toBuffer
        var i    = buf.length - 1
        var stop = false
        while i >= 0 && !stop do
            if isSel(buf(i)) then
                if i == buf.length - 1 then stop = true
                else
                    val tmp = buf(i + 1)
                    buf(i + 1) = buf(i)
                    buf(i) = tmp
            end if
            i -= 1
        end while
        buf.toList
    end moveDown

    /** Selected items move to the front, keeping their relative order. */
    def moveTop[A](xs: List[A], isSel: A => Boolean): List[A] =
        val (sel, rest) = xs.partition(isSel)
        sel ++ rest

    /** Selected items move to the back, keeping their relative order. */
    def moveBottom[A](xs: List[A], isSel: A => Boolean): List[A] =
        val (sel, rest) = xs.partition(isSel)
        rest ++ sel
end ListReorder

/** OrderList — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * OrderList anatomy: `div.p-orderlist.p-component` > `div.p-orderlist-controls`
  * with the four secondary move Buttons (up / top / down / bottom — Prime's
  * angle glyphs, in Prime's order) beside an embedded multiple-selection
  * [[Listbox]]), so the extracted `@primeuix` orderlist + listbox CSS applies
  * verbatim.
  *
  * The ITEM ORDER is the model: `items` binds a `SignalRef[Seq[A]]` two-way —
  * the move buttons write the reordered Seq back (Prime's exact move-up/-top/
  * -down/-bottom semantics), and external ref writes re-render the list. The
  * `selected` ref (keyed by `itemKey`, falling back to the label) drives which
  * rows move; all four buttons are disabled while the selection is empty or the
  * list is `disabled` — Prime's `moveDisabled`. Rendering goes through the real
  * Listbox component (Prime embeds Listbox too); `itemTemplate` maps Prime's
  * option slot onto the typed items.
  */
final case class OrderList[A] private (
    itemsRef: Maybe[SignalRef[Seq[A]]] = Absent,
    labelF: Maybe[A => String] = Absent,
    keyF: Maybe[A => String] = Absent,
    templateF: Maybe[A => UI] = Absent,
    selectedRef: Maybe[SignalRef[Set[String]]] = Absent,
    disabledFlag: Boolean = false,
    accessibleNameV: Maybe[TextValue] = Absent
) extends Node:
    type Self = OrderList[A]

    /** Binds the ordered items two-way: the move buttons write the reordered Seq
      * back; `label` is the row text (and the default selection key). One column,
      * so one binding — [[PickList]] is the two-column member of the pair and
      * spells the same thing `sourceItems` / `targetItems`.
      */
    def items(ref: SignalRef[Seq[A]])(label: A => String): OrderList[A] =
        copy(itemsRef = Present(ref), labelF = Present(label))

    /** Stable item identity — the selection key (defaults to the label). */
    def itemKey(f: A => String): OrderList[A] = copy(keyF = Present(f))

    /** Custom row content (Prime's option slot), replacing the plain label text. */
    def itemTemplate(f: A => UI): OrderList[A] = copy(templateF = Present(f))

    /** Binds the selection two-way (a set of [[itemKey]] ids); the selected rows
      * are the ones the move buttons act on.
      */
    def selected(ref: SignalRef[Set[String]]): OrderList[A] = copy(selectedRef = Present(ref))

    /** Disables the whole control: the listbox dims and the buttons lock. */
    def disabled(v: Boolean): OrderList[A] = copy(disabledFlag = v)

    /** `aria-label` for the embedded listbox. */
    def accessibleName(v: String): OrderList[A] = copy(accessibleNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): OrderList[A] = copy(accessibleNameV = Present(TextValue.Dyn(sig)))

    private def keyOf(a: A): String =
        keyF.orElse(labelF).map(_(a)).getOrElse(a.toString)

    private[uic] def render(using Frame): UI =
        (itemsRef, selectedRef) match
            case (Present(ir), Present(sr)) => ir.render(xs => sr.render(sel => body(xs, sel)))
            case (Present(ir), Absent)      => ir.render(xs => body(xs, Set.empty))
            case _                          => body(Seq.empty, Set.empty)

    private def body(xs: Seq[A], sel: Set[String])(using Frame): UI =
        val moveDisabled = disabledFlag || sel.isEmpty || itemsRef.isEmpty

        def moveButton(glyph: IconGlyph, name: String, move: (List[A], A => Boolean) => List[A]): UI =
            Button()
                .icon(glyph)
                .severity(Severity.Secondary)
                .accessibleName(name)
                .disabled(moveDisabled)
                .onClick(reorder(move))
                .render

        val controls: UI = div.cssClass("p-orderlist-controls")(
            toChild(moveButton(Icons.angleUp, "Move Up", ListReorder.moveUp)),
            toChild(moveButton(Icons.angleDoubleUp, "Move Top", ListReorder.moveTop)),
            toChild(moveButton(Icons.angleDown, "Move Down", ListReorder.moveDown)),
            toChild(moveButton(Icons.angleDoubleDown, "Move Bottom", ListReorder.moveBottom))
        )

        // The embedded Listbox renders through `resolved` — this render already
        // subscribes to the selection ref, so a second nested subscription would
        // race (the Overlay renderOpen lesson); clicks still write the bound ref.
        var lb = Listbox()
            .items(xs.map(a => ListItem(TextValue.Const(labelF.map(_(a)).getOrElse(a.toString)), keyOf(a)))*)
            .selectionMode(SelectionMode.Multiple)
            .disabled(disabledFlag)
        selectedRef.foreach(r => lb = lb.value(r))
        accessibleNameV match
            case Present(TextValue.Const(v)) => lb = lb.accessibleName(v)
            case Present(TextValue.Dyn(s))   => lb = lb.accessibleName(s)
            case Absent                      => ()
        end match
        templateF.foreach { f =>
            val byKey = xs.map(a => keyOf(a) -> a).toMap
            lb = lb.itemTemplate(li =>
                byKey.get(li.id).map(f).getOrElse(li.text match
                    case TextValue.Const(t) => stringToUI(t)
                    case TextValue.Dyn(s)   => s.render(t => stringToUI(t)))
            )
        }
        val listUI: UI = lb.resolved(sel, "")

        div.cssClass("p-orderlist").cssClass("p-component")(toChild(controls), toChild(listUI))
    end body

    /** A move button writes the reordered Seq through the bound items ref, keyed on
      * the current selection.
      */
    private def reorder(move: (List[A], A => Boolean) => List[A])(using Frame): Any < Async =
        (itemsRef, selectedRef) match
            case (Present(ir), Present(sr)) =>
                for
                    sel <- sr.get
                    _   <- ir.getAndUpdate(xs => move(xs.toList, a => sel.contains(keyOf(a))))
                yield ()
            case _ => ()
end OrderList

object OrderList:
    def apply[A](): OrderList[A] = new OrderList[A]()
