package kyo.uic

import kyo.*
import kyo.UI.*

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
  * rows move; all four buttons go `aria-disabled` while the selection is empty
  * (Prime's `moveDisabled`) rather than natively disabled, so a button never
  * leaves the tab order under a reader who is standing on it, and natively
  * disabled only when the whole control is. Rendering goes through the real
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
) extends Node, HasAccessibleName:
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

    private[uic] def withAccessibleName(v: Maybe[TextValue]): OrderList[A] = copy(accessibleNameV = v)

    private def keyOf(a: A): String =
        keyF.orElse(labelF).map(_(a)).getOrElse(a.toString)

    private type Snapshot = ((Seq[A], Set[String]), Int)

    // What a region compares to decide whether to repaint is the SNAPSHOT, never an item: the
    // sequence and the set are compared structurally, and `A` needs no equality of its own for
    // that (it has none to require, since any type can be a row here).
    private given CanEqual[Seq[A], Seq[A]]                               = CanEqual.derived
    private given CanEqual[(Seq[A], Set[String]), (Seq[A], Set[String])] = CanEqual.derived
    private given CanEqual[Snapshot, Snapshot]                           = CanEqual.derived

    /** The order and the selection as ONE signal, so the tree is ONE reactive region.
      *
      * Nesting one render inside the other leaves the inner one subscribed against the value the
      * outer one held when it was created, which goes stale the moment an effect writes both refs.
      * [[PickList]] carries the same shape and is where that stopped being theoretical.
      */
    private def snapshot(cursor: Maybe[ListReorder.Cursor])(using Frame): Signal[Snapshot] =
        val items = itemsRef match
            case Present(r) => r: Signal[Seq[A]]
            case Absent     => Signal.initConst(Seq.empty[A])
        val sel = selectedRef match
            case Present(r) => r: Signal[Set[String]]
            case Absent     => Signal.initConst(Set.empty[String])
        val hi = cursor match
            case Present(c) => c.highlight: Signal[Int]
            case Absent     => Signal.initConst(-1)
        items.combineLatest(sel).combineLatest(hi)
    end snapshot

    /** One mount for the state the list roves: the highlight, and the id it announces it through.
      * Minted OUTSIDE the region, since a mount inside a subscribed region re-runs on every
      * emission and would hand out a new highlight per keystroke. The static projection renders
      * through the placeholder, which is the shape this control had before its list was one tab
      * stop: every row its own.
      */
    private[uic] def render(using Frame): UI =
        UI.mounted {
            for
                cmds <- UI.commands
                id   <- cmds.freshId
                hi   <- Signal.initRef(-1)
            yield wired(Present(ListReorder.Cursor(hi, id)))
        }.placeholder(wired(Absent))

    /** The tree the mount publishes — the seam the tests drive, since a golden render shows a mount
      * only as its placeholder.
      */
    private[uic] def wired(cursor: Maybe[ListReorder.Cursor])(using Frame): UI =
        snapshot(cursor).render { case ((xs, sel), hi) => body(xs, sel, cursor, hi) }

    private def body(xs: Seq[A], sel: Set[String], cursor: Maybe[ListReorder.Cursor] = Absent, focused: Int = -1)(
        using Frame
    ): UI =
        val moveDisabled = disabledFlag || sel.isEmpty || itemsRef.isEmpty

        // The same moves the rail makes, on the list the reader is standing in: the buttons are the
        // pointer's way to them and these are the keyboard's, so both act on the SELECTION. There is
        // no second column here, so no chord transfers anything.
        val hostKeys: UI.KeyboardEvent => Maybe[Listbox.HostKey] = e =>
            ListReorder.onKey(e.key, e.modifiers, Absent).map { move =>
                val eff: Any < Async = move match
                    case ListReorder.Move.Step(down) =>
                        reorder(if down then ListReorder.moveDown else ListReorder.moveUp)
                    case ListReorder.Move.Edge(down) =>
                        reorder(if down then ListReorder.moveBottom else ListReorder.moveTop)
                    case ListReorder.Move.Out(_) => ()
                Listbox.HostKey(keepingCursor(cursor, xs, focused, eff))
            }

        // `ariaDisabled` rather than the native attribute, for the reason [[Button.ariaDisabled]]
        // is there and [[PickList]] spells out: these buttons turn on and off with a selection the
        // reader makes next to them, and a control that leaves the tab order mid-interaction takes
        // the focus with it. The whole control being `disabled` stays native, since that does not
        // change under anybody.
        def moveButton(glyph: IconGlyph, name: String, move: (List[A], A => Boolean) => List[A]): UI =
            var b = Button()
                .icon(glyph)
                .severity(Severity.Secondary)
                .accessibleName(name)
            if disabledFlag then b = b.disabled(true)
            else b = b.ariaDisabled(moveDisabled).onClick(if moveDisabled then () else reorder(move))
            b.render
        end moveButton

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
            .onHostKey(hostKeys)
        cursor.foreach(c => lb = lb.id(c.id))
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
                    case TextValue.Dyn(s)   => signalStringToUI(s))
            )
        }
        val listUI: UI = lb.resolved(sel, "", cursor.map(_.highlight), focused)

        div.cssClass("p-orderlist").cssClass("p-component")(toChild(controls), toChild(listUI))
    end body

    /** Runs `eff` and puts the highlight back on the row it was on.
      *
      * A move rewrites the list under a reader whose focus is on it, and an index that no longer
      * names the same row is a highlight that jumps. [[PickList]] carries the same helper, where
      * the row can also leave the column entirely.
      */
    private def keepingCursor(cursor: Maybe[ListReorder.Cursor], xs: Seq[A], hi: Int, eff: Any < Async)(using
        Frame
    ): Any < Async =
        (cursor, itemsRef) match
            case (Present(c), Present(r)) if xs.isDefinedAt(hi) =>
                val id = keyOf(xs(hi))
                for
                    _     <- eff
                    after <- r.get
                    moved = after.indexWhere(a => keyOf(a) == id)
                    _ <- c.highlight.set(if moved >= 0 then moved else math.min(hi, after.size - 1))
                yield ()
                end for
            case _ => eff

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
