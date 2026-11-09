package kyo.uic

import kyo.*
import kyo.UI.*

/** OrderList: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * OrderList anatomy: `div.p-orderlist.p-component` > `div.p-orderlist-controls`
  * with the four secondary move Buttons (up / top / down / bottom, with Prime's
  * angle glyphs, in Prime's order) beside an embedded multiple-selection
  * [[Listbox]]), so the extracted `@primeuix` orderlist + listbox CSS applies
  * verbatim.
  *
  * The ITEM ORDER is the model: `items` takes a constant, a signal or a ref. A
  * ref binds two way: the move buttons write the reordered Seq back (Prime's exact
  * move-up/-top/-down/-bottom semantics) and `onItemsChange` hears it; a signal or a
  * constant binds one way, and a move only reports the order it asks for. The
  * `selection` (a `Set[A]` of the items themselves, matched by `itemKey`, falling
  * back to the label, bound the same three ways and reported to
  * `onSelectionChange`) drives which rows move; all four buttons go `aria-disabled` while the selection is empty
  * (Prime's `moveDisabled`) rather than natively disabled, so a button never
  * leaves the tab order under a reader who is standing on it, and natively
  * disabled only when the whole control is. Rendering goes through the real
  * Listbox component (Prime embeds Listbox too); `itemTemplate` maps Prime's
  * option slot onto the typed items.
  */
final class OrderList[A] private[uic] (private[uic] val p: OrderList.Props[A]) extends Node, HasAccessibleName, HasProps:
    import p.*

    type Self = OrderList[A]

    /** The ordered items; `label` is the row text (and the default selection key). A `SignalRef` binds two way: the
      * move buttons write the reordered Seq back, then [[onItemsChange]] hears it. A plain `Signal` or a constant binds
      * one way: the list shows it and a move only reports the order it asks for. One column, so one binding. [[PickList]]
      * is the two-column member of the pair and spells the same thing `sourceItems` / `targetItems`.
      */
    def items(v: Seq[A] | Signal[Seq[A]])(label: A => String): OrderList[A] =
        new OrderList(p.copy(itemsSlot = itemsSlot.bind(v), labelF = Present(label)))

    /** Fired with the order a move asks for, after a two-way [[items]] is written. */
    def onItemsChange(f: Seq[A] => UI.Action)(using Frame): OrderList[A] = new OrderList(p.copy(itemsSlot = itemsSlot.hear(f)))

    /** Stable item identity: the selection key (defaults to the label). */
    def itemKey(f: A => String): OrderList[A] = new OrderList(p.copy(keyF = Present(f)))

    /** Custom row content (Prime's option slot), replacing the plain label text. */
    def itemTemplate(f: A => UI): OrderList[A] = new OrderList(p.copy(templateF = Present(f)))

    /** The set of chosen items, matched by [[itemKey]]; the selected rows are the ones the move buttons act on. A
      * `SignalRef` binds two way; a plain `Signal` or a constant binds one way, and a click only reports the set it asks
      * for to [[onSelectionChange]].
      */
    def selection(v: Set[A] | Signal[Set[A]]): OrderList[A] = new OrderList(p.copy(selectedSlot = selectedSlot.bind(v)))

    /** Fired with the items chosen after a click, after a two-way [[selection]] is written. */
    def onSelectionChange(f: Set[A] => UI.Action)(using Frame): OrderList[A] = new OrderList(p.copy(selectedSlot = selectedSlot.hear(f)))

    /** Disables the whole control: the listbox dims and the buttons lock. */
    def disabled(v: Boolean): OrderList[A] = new OrderList(p.copy(disabledFlag = v))

    /** Whether picking takes a modifier key, forwarded to the embedded [[Listbox]] (Prime's
      * `metaKeySelection`, which this component has there too). Off by default, as in Prime:
      * every click toggles, which is what a list whose whole purpose is picking several wants.
      */
    def metaKeySelection(v: Boolean): OrderList[A] = new OrderList(p.copy(metaKeyFlag = v))

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new OrderList(p.copy(access = v))
    private def keyOf(a: A): String                            =
        keyF.orElse(labelF).map(_(a)).getOrElse(a.toString)

    private type Snapshot = ((Seq[A], Set[String]), Int)

    // What a region compares to decide whether to repaint is the SNAPSHOT, never an item: the
    // sequence and the set are compared structurally, and `A` needs no equality of its own for
    // that (it has none to require, since any type can be a row here).
    private given CanEqual[A, A]                                         = CanEqual.derived
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
        val items = itemsSlot.reading(Seq.empty[A]).signal
        val sel   = selectedSlot.binding match
            case Present(ReactiveValue.Dyn(sig)) => sig.map(_.map(keyOf))
            case Present(ReactiveValue.Const(c)) => Signal.initConst(c.map(keyOf))
            case _                               => Signal.initConst(Set.empty[String])
        val hi = cursor match
            case Present(c) => c.highlight: Signal[Int]
            case Absent     => Signal.initConst(-1)
        items.combineLatest(sel).combineLatest(hi)
    end snapshot

    /** One mount for the state the list roves: the highlight, and the id it announces it through.
      * Minted OUTSIDE the region, since a mount inside a subscribed region re-runs on every
      * emission and would hand out a new highlight per keystroke.
      */
    private[uic] def render(using Frame): UI =
        UI.mounted {
            for
                id <- UI.id
                hi <- Signal.initRef(-1)
                // Unbound, the list keeps the rows the reader picks itself.
                sel <- selectedSlot.owned(Set.empty[A])
            yield new OrderList(p.copy(selectedSlot = sel)).wired(Present(ListReorder.Cursor(hi, id)))
        }

    /** The tree the mount publishes: the seam the tests drive, since a golden render shows a mount
      * only as its placeholder.
      */
    private[uic] def wired(cursor: Maybe[ListReorder.Cursor])(using Frame): UI =
        snapshot(cursor).render { case ((xs, sel), hi) => body(xs, sel, cursor, hi) }

    private def body(xs: Seq[A], sel: Set[String], cursor: Maybe[ListReorder.Cursor] = Absent, focused: Int = -1)(
        using Frame
    ): UI =
        val moveDisabled          = disabledFlag || sel.isEmpty || itemsSlot.binding.isEmpty
        def chosen(a: A): Boolean = sel.contains(keyOf(a))

        // The same moves the rail makes, on the list the reader is standing in: the buttons are the
        // pointer's way to them and these are the keyboard's, so both act on the SELECTION. There is
        // no second column here, so no chord transfers anything.
        val hostKeys: UI.KeyboardEvent => Maybe[Listbox.HostKey] = e =>
            ListReorder.onKey(e.key, e.modifiers, Absent).map { move =>
                val step: Maybe[(List[A], A => Boolean) => List[A]] = move match
                    case ListReorder.Move.Step(down) => Present(if down then ListReorder.moveDown else ListReorder.moveUp)
                    case ListReorder.Move.Edge(down) => Present(if down then ListReorder.moveBottom else ListReorder.moveTop)
                    case ListReorder.Move.Out(_)     => Absent
                step match
                    case Present(m) if !itemsFixed => Listbox.HostKey(keepingCursor(cursor, xs, focused, m(xs.toList, chosen), reorder(m)))
                    case _                         => Listbox.HostKey(())
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
            // A fixed order shows its rail and moves nothing with it.
            else if itemsFixed then b = b.ariaDisabled(true)
            else b = b.ariaDisabled(moveDisabled).onClick(if moveDisabled then () else reorder(move))
            end if
            b.render
        end moveButton

        val controls: UI = div.cssClass("p-orderlist-controls")(
            moveButton(Icons.angleUp, "Move Up", ListReorder.moveUp),
            moveButton(Icons.angleDoubleUp, "Move Top", ListReorder.moveTop),
            moveButton(Icons.angleDown, "Move Down", ListReorder.moveDown),
            moveButton(Icons.angleDoubleDown, "Move Bottom", ListReorder.moveBottom)
        )

        // The embedded Listbox renders through `resolved`: this render already
        // subscribes to the selection ref, so a second nested subscription would
        // race (the Overlay renderOpen lesson); clicks still write the bound ref.
        var lb = Listbox[String]()
            .items(xs.map(a => ListItem(TextValue.Const(labelF.map(_(a)).getOrElse(a.toString)), keyOf(a)))*)
            .selectionMode(SelectionMode.Multiple)
            .metaKeySelection(metaKeyFlag)
            .disabled(disabledFlag)
            .fixed(selectedSlot.fixed)
            .onHostKey(hostKeys)
        cursor.foreach(c => lb = lb.id(c.id))
        selectedSlot.binding.foreach(v => lb = lb.selectionBinding(SetBinding.keyed(MultiChoice(v), xs, keyOf, selectedSlot.hearer)))
        access.name.foreach(v => lb = lb.accessibleName(v))
        templateF.foreach { f =>
            val byKey = xs.map(a => keyOf(a) -> a).toMap
            lb = lb.itemTemplate(li =>
                byKey.get(li.id).map(f).getOrElse(li.text match
                    case TextValue.Const(t) => stringToUI(t)
                    case TextValue.Dyn(s)   => signalStringToUI(s))
            )
        }
        val listUI: UI = lb.resolved(sel, "", cursor.map(_.highlight), focused)

        div.cssClass("p-orderlist").cssClass("p-component")(UI.children(unheard :+ controls :+ listUI)*)
    end body

    /** Bindings made one way that nothing hears: every move or click is painted away again. A disabled list takes no move
      * or click, so it has nothing to say.
      */
    private def unheard(using Frame): List[UI] =
        Diagnostics.cards("OrderList")(
            Diagnostic.unheard(itemsSlot, OrderList.itemsName, on = !disabledFlag),
            Diagnostic.unheard(selectedSlot, OrderList.selectionName, on = !disabledFlag)
        ).toList
    end unheard

    /** A constant order nobody hears is fixed: the list shows it, and no button or key moves a row. */
    private def itemsFixed: Boolean = itemsSlot.fixed

    /** Runs `eff` and puts the highlight back on the row it was on in `after`, the list the move asks for.
      *
      * A move rewrites the list under a reader whose focus is on it, and an index that no longer
      * names the same row is a highlight that jumps. The list is the one the move asks for, not the
      * one the binding holds afterwards: a one-way owner may write it back later, or not at all.
      * [[PickList]] carries the same helper, where the row can also leave the column entirely.
      */
    private def keepingCursor(cursor: Maybe[ListReorder.Cursor], xs: Seq[A], hi: Int, after: Seq[A], eff: UI.Action)(using
        Frame
    ): UI.Action =
        cursor match
            case Present(c) if itemsSlot.binding.isDefined && xs.isDefinedAt(hi) =>
                val id    = keyOf(xs(hi))
                val moved = after.indexWhere(a => keyOf(a) == id)
                eff.andThen(c.highlight.set(if moved >= 0 then moved else math.min(hi, after.size - 1)))
            case _ => eff

    /** A move steps the bound items, keyed on the current selection: written if they bind two way, then reported. */
    private def reorder(move: (List[A], A => Boolean) => List[A])(using Frame): UI.Action =
        selectedSlot.binding match
            case Present(s) if itemsSlot.binding.isDefined =>
                s.current.map { held =>
                    val sel = held.map(keyOf)
                    itemsSlot.update(xs => move(xs.toList, a => sel.contains(keyOf(a))))
                }
            case _ => ()
end OrderList

object OrderList:
    final private[uic] case class Props[A](
        itemsSlot: Controlled[Seq[A]] = Controlled[Seq[A]](),
        labelF: Maybe[A => String] = Absent,
        keyF: Maybe[A => String] = Absent,
        templateF: Maybe[A => UI] = Absent,
        selectedSlot: Controlled[Set[A]] = Controlled[Set[A]](),
        disabledFlag: Boolean = false,
        metaKeyFlag: Boolean = false,
        access: Accessibility = Accessibility.none
    )

    private val itemsName        = Controlled.Name("OrderList", "items", "a move", "onItemsChange")
    private val selectionName    = Controlled.Name("OrderList", "selection", "a click", "onSelectionChange")
    def apply[A](): OrderList[A] = new OrderList[A](Props())
end OrderList
