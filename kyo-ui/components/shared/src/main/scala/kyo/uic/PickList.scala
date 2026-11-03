package kyo.uic

import kyo.*
import kyo.UI.*

/** PickList: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * PickList anatomy: `div.p-picklist.p-component` > source reorder controls
  * (`div.p-picklist-controls.p-picklist-source-controls`, the four angle move
  * Buttons) > `div.p-picklist-list-container.p-picklist-source-list-container`
  * with the source [[Listbox]] > the transfer controls
  * (`div.p-picklist-controls.p-picklist-transfer-controls`: move-to-target /
  * move-all-to-target / move-to-source / move-all-to-source, Prime's angle
  * glyphs) > the target list container > target reorder controls), so the
  * extracted `@primeuix` picklist + listbox CSS applies verbatim.
  *
  * The two COLUMN CONTENTS are the model: `sourceItems` and `targetItems` each
  * take a constant, a signal or a ref. A ref binds two way: transfers append the
  * moved items to the other column (Prime's semantics) and reorders write the
  * shuffled Seq back, and `onSourceItemsChange` / `onTargetItemsChange` hear the new
  * column; a signal or a constant binds one way, and the change is only reported.
  * The `*Items` names are deliberate: [[OrderList]] is the one-column member of
  * this pair and binds a single [[OrderList.items]], so a PickList reads as two
  * of the same thing rather than as a different model. The two
  * selections (keyed by `itemKey`, falling back to the label, bound the same three
  * ways and reported to `on*SelectionChange`) drive which
  * rows transfer/move; a transfer clears the emptied column's selection, exactly
  * like Prime. Transfer buttons disable while their source selection is empty
  * (move-all while the column is empty); reorder buttons follow Prime's
  * `moveDisabled`. `showSourceControls`/`showTargetControls` (default true)
  * drop the per-column reorder rails.
  */
final case class PickList[A] private (
    sourceSlot: Slot[Seq[A]] = Slot[Seq[A]](),
    targetSlot: Slot[Seq[A]] = Slot[Seq[A]](),
    labelF: Maybe[A => String] = Absent,
    keyF: Maybe[A => String] = Absent,
    templateF: Maybe[A => UI] = Absent,
    sourceSelectedSlot: Slot[Set[A]] = Slot[Set[A]](),
    targetSelectedSlot: Slot[Set[A]] = Slot[Set[A]](),
    showSourceControlsFlag: Boolean = true,
    showTargetControlsFlag: Boolean = true,
    disabledFlag: Boolean = false,
    metaKeyFlag: Boolean = false
) extends Node:
    type Self = PickList[A]

    /** The source column, in the same curried `(items)(label)` shape as [[OrderList.items]]; `label` is the row text of
      * BOTH columns (and the default item key), which is why only this half carries it. A `SignalRef` binds two way; a
      * plain `Signal` or a constant binds one way, and a transfer or a reorder only reports the column it asks for to
      * [[onSourceItemsChange]].
      */
    def sourceItems(v: Seq[A] | Signal[Seq[A]])(label: A => String): PickList[A] =
        copy(sourceSlot = sourceSlot.bind(v), labelF = Present(label))

    /** The target column, bound like [[sourceItems]]; the label projection comes from [[sourceItems]] and covers both
      * columns.
      */
    def targetItems(v: Seq[A] | Signal[Seq[A]]): PickList[A] = copy(targetSlot = targetSlot.bind(v))

    /** Fired with the source column a transfer or a reorder asks for, after a two-way [[sourceItems]] is written. */
    def onSourceItemsChange(f: Seq[A] => UI.Action)(using Frame): PickList[A] = copy(sourceSlot = sourceSlot.hear(f))

    /** Fired with the target column a transfer or a reorder asks for, after a two-way [[targetItems]] is written. */
    def onTargetItemsChange(f: Seq[A] => UI.Action)(using Frame): PickList[A] = copy(targetSlot = targetSlot.hear(f))

    /** Stable item identity: the selection key (defaults to the label). */
    def itemKey(f: A => String): PickList[A] = copy(keyF = Present(f))

    /** Custom row content (Prime's option slot) for both columns. */
    def itemTemplate(f: A => UI): PickList[A] = copy(templateF = Present(f))

    /** The source column's selection, as the set of chosen items matched by [[itemKey]], bound like [[sourceItems]] and
      * reported to [[onSourceSelectionChange]]. A transfer out of the column clears it.
      */
    def sourceSelection(v: Set[A] | Signal[Set[A]]): PickList[A] =
        copy(sourceSelectedSlot = sourceSelectedSlot.bind(v))

    /** The target column's selection, bound and reported like [[sourceSelection]]. */
    def targetSelection(v: Set[A] | Signal[Set[A]]): PickList[A] =
        copy(targetSelectedSlot = targetSelectedSlot.bind(v))

    /** Fired with the source selection a click or a transfer asks for, after a two-way [[sourceSelection]] is written. */
    def onSourceSelectionChange(f: Set[A] => UI.Action)(using Frame): PickList[A] =
        copy(sourceSelectedSlot = sourceSelectedSlot.hear(f))

    /** Fired with the target selection a click or a transfer asks for, after a two-way [[targetSelection]] is written. */
    def onTargetSelectionChange(f: Set[A] => UI.Action)(using Frame): PickList[A] =
        copy(targetSelectedSlot = targetSelectedSlot.hear(f))

    /** One column's bindings and the events that hear them, so a move or a transfer is the same code for either. */
    final private case class Col(items: Slot[Seq[A]], sel: Slot[Set[A]]):
        /** A constant column nobody hears is fixed: it shows its rows, and nothing moves or transfers them. */
        def itemsFixed: Boolean = items.fixed

        /** A constant selection nobody hears is fixed: the column shows it, and no click picks a row. */
        def selFixed: Boolean = sel.fixed
    end Col

    private def sourceCol: Col = Col(sourceSlot, sourceSelectedSlot)
    private def targetCol: Col = Col(targetSlot, targetSelectedSlot)

    /** Renders the source column's reorder rail (default true, like Prime). */
    def showSourceControls(v: Boolean): PickList[A] = copy(showSourceControlsFlag = v)

    /** Renders the target column's reorder rail (default true, like Prime). */
    def showTargetControls(v: Boolean): PickList[A] = copy(showTargetControlsFlag = v)

    /** Disables the whole control: both listboxes dim and every button locks. */
    def disabled(v: Boolean): PickList[A] = copy(disabledFlag = v)

    /** Whether picking takes a modifier key, forwarded to BOTH embedded [[Listbox]]es (Prime's
      * `metaKeySelection`, which this component has there too). Off by default, as in Prime:
      * every click toggles, which is what a list whose whole purpose is picking several wants.
      */
    def metaKeySelection(v: Boolean): PickList[A] = copy(metaKeyFlag = v)

    private def keyOf(a: A): String =
        keyF.orElse(labelF).map(_(a)).getOrElse(a.toString)

    private type Columns    = (Seq[A], Seq[A])
    private type Selections = (Set[String], Set[String])
    private type Cursors    = (Int, Int)
    private type Snapshot   = ((Columns, Selections), Cursors)

    // What a region compares to decide whether to repaint is the SNAPSHOT, never an item: the
    // sequences and sets that hold them are compared structurally, and `A` needs no equality of
    // its own for that (it has none to require, since any type can be a row here).
    private given CanEqual[A, A]                                         = CanEqual.derived
    private given CanEqual[Seq[A], Seq[A]]                               = CanEqual.derived
    private given CanEqual[Columns, Columns]                             = CanEqual.derived
    private given CanEqual[Selections, Selections]                       = CanEqual.derived
    private given CanEqual[Cursors, Cursors]                             = CanEqual.derived
    private given CanEqual[(Columns, Selections), (Columns, Selections)] = CanEqual.derived
    private given CanEqual[Snapshot, Snapshot]                           = CanEqual.derived

    /** A column's selection as the keys its rows are matched by. */
    private def selectionKeys(slot: Slot[Set[A]])(using Frame): Signal[Set[String]] =
        slot.binding match
            case Present(ReactiveValue.Dyn(s))   => s.map(_.map(keyOf))
            case Present(ReactiveValue.Const(c)) => Signal.initConst(c.map(keyOf))
            case _                               => Signal.initConst(Set.empty[String])

    /** Everything one render of this control reads, as ONE signal.
      *
      * It was four nested renders, one per ref, and a transfer writes THREE of them. The outer
      * region re-rendered with the moved source while the inner regions were still subscribed
      * against the values they had closed over when they were created, and the next click on a row
      * repainted that older snapshot over the finished transfer: the item came back and the target
      * column emptied, while the refs themselves held the correct result the whole time.
      *
      * `combineLatest` reads every value on every emission, so there is one region and nothing left
      * to be stale. It is also why the highlight rides along: the columns and their highlights are
      * one state, and splitting them again is how this went wrong the first time.
      */
    private def snapshot(source: Maybe[ListReorder.Cursor], target: Maybe[ListReorder.Cursor])(using Frame): Signal[Snapshot] =
        val columns    = sourceSlot.reading(Seq.empty[A]).signal.combineLatest(targetSlot.reading(Seq.empty[A]).signal)
        val selections = selectionKeys(sourceSelectedSlot).combineLatest(selectionKeys(targetSelectedSlot))
        val cursors    = source.fold(Signal.initConst(-1))(_.highlight).combineLatest(target.fold(Signal.initConst(-1))(_.highlight))
        columns.combineLatest(selections).combineLatest(cursors)
    end snapshot

    /** One mount for the state the two columns rove: a highlight each, and the id each announces it
      * through. Both are minted OUTSIDE the region, since a mount inside a subscribed region re-runs
      * on every emission and would hand out a new highlight per keystroke.
      *
      * The static projection renders through the placeholder: each list is already its one tab
      * stop there, with no highlight on it yet.
      */
    private[uic] def render(using Frame): UI =
        UI.mounted {
            for
                cmds     <- UI.commands
                sourceId <- cmds.freshId
                targetId <- cmds.freshId
                sourceHi <- Signal.initRef(-1)
                targetHi <- Signal.initRef(-1)
                // Unbound, the list keeps its columns and what the reader picks in them itself.
                src    <- sourceSlot.owned(Seq.empty[A])
                tgt    <- targetSlot.owned(Seq.empty[A])
                srcSel <- sourceSelectedSlot.owned(Set.empty[A])
                tgtSel <- targetSelectedSlot.owned(Set.empty[A])
                self = copy(sourceSlot = src, targetSlot = tgt, sourceSelectedSlot = srcSel, targetSelectedSlot = tgtSel)
            yield self.wired(
                Present(ListReorder.Cursor(sourceHi, sourceId)),
                Present(ListReorder.Cursor(targetHi, targetId))
            )
        }.placeholder(wired(Absent, Absent))

    /** The tree the mount publishes, the seam the tests drive, since a golden render shows a mount
      * only as its placeholder.
      */
    private[uic] def wired(source: Maybe[ListReorder.Cursor], target: Maybe[ListReorder.Cursor])(using Frame): UI =
        snapshot(source, target).render {
            case (((src, tgt), (srcSel, tgtSel)), (srcHi, tgtHi)) =>
                body(src, tgt, srcSel, tgtSel, source, target, srcHi, tgtHi)
        }

    /** A move or transfer button.
      *
      * `ariaDisabled` rather than the native attribute, and for the reason [[Button.ariaDisabled]]
      * is there: a transfer CLEARS the selection it acted on, so the button the reader just
      * pressed turns off under their hands, and a natively disabled button loses focus to the
      * document. One keystroke moved the items and sent the reader back to the top of the page.
      * Staying in the tab order means the click stays wired too, so the action is guarded here
      * instead of by the attribute. The whole control being `disabled` is different: nothing about
      * that changes under the reader, so it stays native.
      */
    private def moveButton(glyph: IconGlyph, name: String, off: Boolean, fixed: Boolean, act: => UI.Action)(using Frame): UI =
        var b = Button()
            .icon(glyph)
            .severity(Severity.Secondary)
            .accessibleName(name)
        if disabledFlag then b = b.disabled(true)
        // A move over a fixed column is shown and does nothing.
        else if fixed then b = b.ariaDisabled(true)
        else b = b.ariaDisabled(off).onClick(if off then () else act)
        end if
        b.render
    end moveButton

    private def body(
        src: Seq[A],
        tgt: Seq[A],
        srcSel: Set[String],
        tgtSel: Set[String],
        source: Maybe[ListReorder.Cursor] = Absent,
        target: Maybe[ListReorder.Cursor] = Absent,
        srcHi: Int = -1,
        tgtHi: Int = -1
    )(using Frame): UI =
        val srcMoveOff = srcSel.isEmpty || sourceSlot.binding.isEmpty
        val tgtMoveOff = tgtSel.isEmpty || targetSlot.binding.isEmpty

        // The same moves the rails and the transfer buttons make, on the list the reader is
        // standing in: the buttons are the pointer's way to them and these are the keyboard's, so
        // both act on the column's SELECTION, and neither can do what the other cannot.
        def keys(
            col: Col,
            other: Col,
            toward: Keyboard,
            cursor: Maybe[ListReorder.Cursor],
            xs: Seq[A],
            sel: Set[String],
            hi: Int
        ): UI.KeyboardEvent => Maybe[Listbox.HostKey] = e =>
            def chosen(a: A): Boolean = sel.contains(keyOf(a))
            ListReorder.onKey(e.key, e.modifiers, Present(toward)).map { move =>
                val blocked = move match
                    case ListReorder.Move.Out(_) => col.itemsFixed || other.itemsFixed
                    case _                       => col.itemsFixed
                // The column the move asks for, which is where the highlight follows its row: a one-way owner may write
                // the column back later, or not at all.
                val planned: (UI.Action, Seq[A]) = move match
                    case ListReorder.Move.Step(down) =>
                        val m: (List[A], A => Boolean) => List[A] = if down then ListReorder.moveDown else ListReorder.moveUp
                        (reorder(col, m), m(xs.toList, chosen))
                    case ListReorder.Move.Edge(down) =>
                        val m: (List[A], A => Boolean) => List[A] = if down then ListReorder.moveBottom else ListReorder.moveTop
                        (reorder(col, m), m(xs.toList, chosen))
                    case ListReorder.Move.Out(all) =>
                        if all then (transferAll(col, other), Seq.empty)
                        else (transferSelected(col, other), xs.filterNot(chosen))
                if blocked then Listbox.HostKey(()) else Listbox.HostKey(keepingCursor(cursor, col, xs, hi, planned._2, planned._1))
            }
        end keys

        def reorderRail(cls: String, col: Col, off: Boolean): UI =
            div.cssClass("p-picklist-controls").cssClass(cls)(
                moveButton(Icons.angleUp, "Move Up", off, col.itemsFixed, reorder(col, ListReorder.moveUp)),
                moveButton(Icons.angleDoubleUp, "Move Top", off, col.itemsFixed, reorder(col, ListReorder.moveTop)),
                moveButton(Icons.angleDown, "Move Down", off, col.itemsFixed, reorder(col, ListReorder.moveDown)),
                moveButton(Icons.angleDoubleDown, "Move Bottom", off, col.itemsFixed, reorder(col, ListReorder.moveBottom))
            )

        def column(
            cls: String,
            xs: Seq[A],
            sel: Set[String],
            col: Col,
            cursor: Maybe[ListReorder.Cursor],
            focused: Int,
            hostKeys: UI.KeyboardEvent => Maybe[Listbox.HostKey]
        ): UI =
            var lb = Listbox[String]()
                .items(xs.map(a => ListItem(TextValue.Const(labelF.map(_(a)).getOrElse(a.toString)), keyOf(a)))*)
                .selectionMode(SelectionMode.Multiple)
                .metaKeySelection(metaKeyFlag)
                .disabled(disabledFlag)
                .fixed(col.selFixed)
                .onHostKey(hostKeys)
            cursor.foreach(c => lb = lb.id(c.id))
            col.sel.binding.foreach(v => lb = lb.selectionBinding(SetBinding.keyed(MultiChoice(v), xs, keyOf, col.sel.hearer)))
            templateF.foreach { f =>
                val byKey = xs.map(a => keyOf(a) -> a).toMap
                lb = lb.itemTemplate(li =>
                    byKey.get(li.id).map(f).getOrElse(li.text match
                        case TextValue.Const(t) => stringToUI(t)
                        case TextValue.Dyn(s)   => signalStringToUI(s))
                )
            }
            div.cssClass("p-picklist-list-container").cssClass(cls)(
                lb.resolved(sel, "", cursor.map(_.highlight), focused)
            )
        end column

        // A transfer changes both columns, so either being fixed holds it.
        val transferFixed = sourceCol.itemsFixed || targetCol.itemsFixed
        val transfer: UI  = div.cssClass("p-picklist-controls").cssClass("p-picklist-transfer-controls")(
            moveButton(Icons.angleRight, "Move to Target", srcMoveOff, transferFixed, transferSelected(sourceCol, targetCol)),
            moveButton(
                Icons.angleDoubleRight,
                "Move All to Target",
                src.isEmpty,
                transferFixed,
                transferAll(sourceCol, targetCol)
            ),
            moveButton(Icons.angleLeft, "Move to Source", tgtMoveOff, transferFixed, transferSelected(targetCol, sourceCol)),
            moveButton(Icons.angleDoubleLeft, "Move All to Source", tgt.isEmpty, transferFixed, transferAll(targetCol, sourceCol))
        )

        val sourceRail: List[UI] =
            if showSourceControlsFlag then
                List(reorderRail("p-picklist-source-controls", sourceCol, srcMoveOff))
            else Nil
        val targetRail: List[UI] =
            if showTargetControlsFlag then
                List(reorderRail("p-picklist-target-controls", targetCol, tgtMoveOff))
            else Nil

        val parts: List[UI] =
            sourceRail ++
                List(
                    column(
                        "p-picklist-source-list-container",
                        src,
                        srcSel,
                        sourceCol,
                        source,
                        srcHi,
                        keys(sourceCol, targetCol, Keyboard.ArrowRight, source, src, srcSel, srcHi)
                    ),
                    transfer,
                    column(
                        "p-picklist-target-list-container",
                        tgt,
                        tgtSel,
                        targetCol,
                        target,
                        tgtHi,
                        keys(targetCol, sourceCol, Keyboard.ArrowLeft, target, tgt, tgtSel, tgtHi)
                    )
                ) ++ targetRail

        div.cssClass("p-picklist").cssClass("p-component")(UI.children(unheard ++ parts)*)
    end body

    /** Bindings made one way that nothing hears: every transfer, move or click is painted away again. Disabled columns
      * take none of those, so they have nothing to say.
      */
    private def unheard(using Frame): List[UI] =
        Diagnostics.cards("PickList")(
            Diagnostic.unheard(sourceSlot, PickList.name("sourceItems", "onSourceItemsChange"), on = !disabledFlag),
            Diagnostic.unheard(targetSlot, PickList.name("targetItems", "onTargetItemsChange"), on = !disabledFlag),
            Diagnostic.unheard(sourceSelectedSlot, PickList.name("sourceSelection", "onSourceSelectionChange"), on = !disabledFlag),
            Diagnostic.unheard(targetSelectedSlot, PickList.name("targetSelection", "onTargetSelectionChange"), on = !disabledFlag)
        ).toList
    end unheard

    /** Runs `eff` and puts the highlight back on the row it was on in `after`, the column the move asks for.
      *
      * A move rewrites the column under a reader whose focus is on the list, and an index that no
      * longer names the same row is a highlight that jumps or vanishes. The row is followed by its
      * key where the column still holds it, and where the move carried it out of the column the
      * highlight keeps the position instead, clamped to what is left. The column is the one the move
      * asks for, not the one the binding holds afterwards: a one-way owner may write it back later.
      */
    private def keepingCursor(cursor: Maybe[ListReorder.Cursor], col: Col, xs: Seq[A], hi: Int, after: Seq[A], eff: UI.Action)(
        using Frame
    ): UI.Action =
        cursor match
            case Present(c) if col.items.binding.isDefined && xs.isDefinedAt(hi) =>
                val id    = keyOf(xs(hi))
                val moved = after.indexWhere(a => keyOf(a) == id)
                eff.andThen(c.highlight.set(if moved >= 0 then moved else math.min(hi, after.size - 1)))
            case _ => eff

    /** The keys a column's selection holds now. */
    private def chosenKeys(col: Col)(using Frame): Set[String] < Sync =
        col.sel.current(Set.empty[A]).map(_.map(keyOf))

    /** A reorder steps its column, keyed on that column's selection: written if it binds two way, then reported. */
    private def reorder(col: Col, move: (List[A], A => Boolean) => List[A])(using Frame): UI.Action =
        if col.items.binding.isEmpty || col.sel.binding.isEmpty then ()
        else chosenKeys(col).map(sel => col.items.update(xs => move(xs.toList, a => sel.contains(keyOf(a)))))

    /** Moves the items of `from` that `pick` names to the END of `to` (Prime appends), then clears `from`'s selection:
      * each column and the selection written where they bind two way, and reported in that order.
      */
    private def transfer(from: Col, to: Col)(pick: Set[String] => A => Boolean)(using Frame): UI.Action =
        (from.items.binding, to.items.binding, from.sel.binding) match
            case (Present(f), Present(_), Present(_)) =>
                for
                    sel <- chosenKeys(from)
                    xs  <- f.current
                    (moved, kept) = xs.partition(pick(sel))
                    _ <- from.items.change(kept)
                    _ <- to.items.update(_ ++ moved)
                    _ <- from.sel.change(Set.empty[A])
                yield ()
            case _ => ()

    /** Moves the selected items of `from` to the end of `to`. */
    private def transferSelected(from: Col, to: Col)(using Frame): UI.Action =
        transfer(from, to)(sel => a => sel.contains(keyOf(a)))

    /** Moves ALL items of `from` to the end of `to`. */
    private def transferAll(from: Col, to: Col)(using Frame): UI.Action =
        transfer(from, to)(_ => _ => true)
end PickList

object PickList:
    private def name(subject: String, event: String) = Slot.Name("PickList", subject, "a change", event)
    def apply[A](): PickList[A]                      = new PickList[A]()
