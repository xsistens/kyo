package kyo.uic

import kyo.*
import kyo.UI.*

/** PickList — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
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
  * bind a `SignalRef[Seq[A]]` two-way — transfers append the moved items to the
  * other column (Prime's semantics) and reorders write the shuffled Seq back.
  * The `*Items` names are deliberate: [[OrderList]] is the one-column member of
  * this pair and binds a single [[OrderList.items]], so a PickList reads as two
  * of the same thing rather than as a different model. The two
  * selection refs (keyed by `itemKey`, falling back to the label) drive which
  * rows transfer/move; a transfer clears the emptied column's selection, exactly
  * like Prime. Transfer buttons disable while their source selection is empty
  * (move-all while the column is empty); reorder buttons follow Prime's
  * `moveDisabled`. `showSourceControls`/`showTargetControls` (default true)
  * drop the per-column reorder rails.
  */
final case class PickList[A] private (
    sourceRef: Maybe[SignalRef[Seq[A]]] = Absent,
    targetRef: Maybe[SignalRef[Seq[A]]] = Absent,
    labelF: Maybe[A => String] = Absent,
    keyF: Maybe[A => String] = Absent,
    templateF: Maybe[A => UI] = Absent,
    sourceSelectedRef: Maybe[SignalRef[Set[String]]] = Absent,
    targetSelectedRef: Maybe[SignalRef[Set[String]]] = Absent,
    showSourceControlsFlag: Boolean = true,
    showTargetControlsFlag: Boolean = true,
    disabledFlag: Boolean = false,
    metaKeyFlag: Boolean = false
) extends Node:
    type Self = PickList[A]

    /** Binds the source column two-way, in the same curried `(ref)(label)` shape as
      * [[OrderList.items]]; `label` is the row text of BOTH columns (and the
      * default item key), which is why only this half carries it.
      */
    def sourceItems(ref: SignalRef[Seq[A]])(label: A => String): PickList[A] =
        copy(sourceRef = Present(ref), labelF = Present(label))

    /** Binds the target column two-way; the label projection comes from
      * [[sourceItems]] and covers both columns.
      */
    def targetItems(ref: SignalRef[Seq[A]]): PickList[A] = copy(targetRef = Present(ref))

    /** Stable item identity — the selection key (defaults to the label). */
    def itemKey(f: A => String): PickList[A] = copy(keyF = Present(f))

    /** Custom row content (Prime's option slot) for both columns. */
    def itemTemplate(f: A => UI): PickList[A] = copy(templateF = Present(f))

    /** Binds the source column's selection two-way (a set of [[itemKey]] ids). */
    def sourceSelected(ref: SignalRef[Set[String]]): PickList[A] =
        copy(sourceSelectedRef = Present(ref))

    /** Binds the target column's selection two-way (a set of [[itemKey]] ids). */
    def targetSelected(ref: SignalRef[Set[String]]): PickList[A] =
        copy(targetSelectedRef = Present(ref))

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
    private given CanEqual[Seq[A], Seq[A]]                               = CanEqual.derived
    private given CanEqual[Columns, Columns]                             = CanEqual.derived
    private given CanEqual[Selections, Selections]                       = CanEqual.derived
    private given CanEqual[Cursors, Cursors]                             = CanEqual.derived
    private given CanEqual[(Columns, Selections), (Columns, Selections)] = CanEqual.derived
    private given CanEqual[Snapshot, Snapshot]                           = CanEqual.derived

    private def sig[T](ref: Maybe[SignalRef[T]], fallback: T)(using CanEqual[T, T], Frame): Signal[T] =
        ref match
            case Present(r) => r
            case Absent     => Signal.initConst(fallback)

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
        val columns    = sig(sourceRef, Seq.empty[A]).combineLatest(sig(targetRef, Seq.empty[A]))
        val selections = sig(sourceSelectedRef, Set.empty[String]).combineLatest(sig(targetSelectedRef, Set.empty[String]))
        val cursors    = sig(source.map(_.highlight), -1).combineLatest(sig(target.map(_.highlight), -1))
        columns.combineLatest(selections).combineLatest(cursors)
    end snapshot

    /** One mount for the state the two columns rove: a highlight each, and the id each announces it
      * through. Both are minted OUTSIDE the region, since a mount inside a subscribed region re-runs
      * on every emission and would hand out a new highlight per keystroke.
      *
      * The static projection renders through the placeholder, which is the shape this control had
      * before its lists were one tab stop each: every row its own.
      */
    private[uic] def render(using Frame): UI =
        UI.mounted {
            for
                cmds     <- UI.commands
                sourceId <- cmds.freshId
                targetId <- cmds.freshId
                sourceHi <- Signal.initRef(-1)
                targetHi <- Signal.initRef(-1)
            yield wired(
                Present(ListReorder.Cursor(sourceHi, sourceId)),
                Present(ListReorder.Cursor(targetHi, targetId))
            )
        }.placeholder(wired(Absent, Absent))

    /** The tree the mount publishes — the seam the tests drive, since a golden render shows a mount
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
    private def moveButton(glyph: IconGlyph, name: String, off: Boolean, act: => Any < Async)(using Frame): UI =
        var b = Button()
            .icon(glyph)
            .severity(Severity.Secondary)
            .accessibleName(name)
        if disabledFlag then b = b.disabled(true)
        else b = b.ariaDisabled(off).onClick(if off then () else act)
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
        val srcMoveOff = srcSel.isEmpty || sourceRef.isEmpty
        val tgtMoveOff = tgtSel.isEmpty || targetRef.isEmpty

        // The same moves the rails and the transfer buttons make, on the list the reader is
        // standing in: the buttons are the pointer's way to them and these are the keyboard's, so
        // both act on the column's SELECTION, and neither can do what the other cannot.
        def keys(
            ref: Maybe[SignalRef[Seq[A]]],
            selRef: Maybe[SignalRef[Set[String]]],
            other: Maybe[SignalRef[Seq[A]]],
            toward: Keyboard,
            cursor: Maybe[ListReorder.Cursor],
            xs: Seq[A],
            hi: Int
        ): UI.KeyboardEvent => Maybe[Listbox.HostKey] = e =>
            ListReorder.onKey(e.key, e.modifiers, Present(toward)).map { move =>
                val eff: Any < Async = move match
                    case ListReorder.Move.Step(down) =>
                        reorder(ref, selRef, if down then ListReorder.moveDown else ListReorder.moveUp)
                    case ListReorder.Move.Edge(down) =>
                        reorder(ref, selRef, if down then ListReorder.moveBottom else ListReorder.moveTop)
                    case ListReorder.Move.Out(all) =>
                        if all then transferAll(ref, other, selRef) else transferSelected(ref, other, selRef)
                Listbox.HostKey(keepingCursor(cursor, ref, xs, hi, eff))
            }

        def reorderRail(cls: String, ref: Maybe[SignalRef[Seq[A]]], selRef: Maybe[SignalRef[Set[String]]], off: Boolean): UI =
            div.cssClass("p-picklist-controls").cssClass(cls)(
                toChild(moveButton(Icons.angleUp, "Move Up", off, reorder(ref, selRef, ListReorder.moveUp))),
                toChild(moveButton(Icons.angleDoubleUp, "Move Top", off, reorder(ref, selRef, ListReorder.moveTop))),
                toChild(moveButton(Icons.angleDown, "Move Down", off, reorder(ref, selRef, ListReorder.moveDown))),
                toChild(moveButton(Icons.angleDoubleDown, "Move Bottom", off, reorder(ref, selRef, ListReorder.moveBottom)))
            )

        def column(
            cls: String,
            xs: Seq[A],
            sel: Set[String],
            selRef: Maybe[SignalRef[Set[String]]],
            cursor: Maybe[ListReorder.Cursor],
            focused: Int,
            hostKeys: UI.KeyboardEvent => Maybe[Listbox.HostKey]
        ): UI =
            var lb = Listbox()
                .items(xs.map(a => ListItem(TextValue.Const(labelF.map(_(a)).getOrElse(a.toString)), keyOf(a)))*)
                .selectionMode(SelectionMode.Multiple)
                .metaKeySelection(metaKeyFlag)
                .disabled(disabledFlag)
                .onHostKey(hostKeys)
            cursor.foreach(c => lb = lb.id(c.id))
            selRef.foreach(r => lb = lb.value(r))
            templateF.foreach { f =>
                val byKey = xs.map(a => keyOf(a) -> a).toMap
                lb = lb.itemTemplate(li =>
                    byKey.get(li.id).map(f).getOrElse(li.text match
                        case TextValue.Const(t) => stringToUI(t)
                        case TextValue.Dyn(s)   => s.render(t => stringToUI(t)))
                )
            }
            div.cssClass("p-picklist-list-container").cssClass(cls)(
                toChild(lb.resolved(sel, "", cursor.map(_.highlight), focused))
            )
        end column

        val transfer: UI = div.cssClass("p-picklist-controls").cssClass("p-picklist-transfer-controls")(
            toChild(moveButton(Icons.angleRight, "Move to Target", srcMoveOff, transferSelected(sourceRef, targetRef, sourceSelectedRef))),
            toChild(moveButton(
                Icons.angleDoubleRight,
                "Move All to Target",
                src.isEmpty,
                transferAll(sourceRef, targetRef, sourceSelectedRef)
            )),
            toChild(moveButton(Icons.angleLeft, "Move to Source", tgtMoveOff, transferSelected(targetRef, sourceRef, targetSelectedRef))),
            toChild(moveButton(
                Icons.angleDoubleLeft,
                "Move All to Source",
                tgt.isEmpty,
                transferAll(targetRef, sourceRef, targetSelectedRef)
            ))
        )

        val sourceRail: List[UI] =
            if showSourceControlsFlag then
                List(reorderRail("p-picklist-source-controls", sourceRef, sourceSelectedRef, srcMoveOff))
            else Nil
        val targetRail: List[UI] =
            if showTargetControlsFlag then
                List(reorderRail("p-picklist-target-controls", targetRef, targetSelectedRef, tgtMoveOff))
            else Nil

        val parts: List[UI] =
            sourceRail ++
                List(
                    column(
                        "p-picklist-source-list-container",
                        src,
                        srcSel,
                        sourceSelectedRef,
                        source,
                        srcHi,
                        keys(sourceRef, sourceSelectedRef, targetRef, Keyboard.ArrowRight, source, src, srcHi)
                    ),
                    transfer,
                    column(
                        "p-picklist-target-list-container",
                        tgt,
                        tgtSel,
                        targetSelectedRef,
                        target,
                        tgtHi,
                        keys(targetRef, targetSelectedRef, sourceRef, Keyboard.ArrowLeft, target, tgt, tgtHi)
                    )
                ) ++ targetRail

        div.cssClass("p-picklist").cssClass("p-component")(parts.map(toChild)*)
    end body

    /** Runs `eff` and puts the highlight back on the row it was on.
      *
      * A move rewrites the column under a reader whose focus is on the list, and an index that no
      * longer names the same row is a highlight that jumps or vanishes. The row is followed by its
      * key where the column still holds it, and where the move carried it out of the column the
      * highlight keeps the position instead, clamped to what is left.
      */
    private def keepingCursor(
        cursor: Maybe[ListReorder.Cursor],
        ref: Maybe[SignalRef[Seq[A]]],
        xs: Seq[A],
        hi: Int,
        eff: Any < Async
    )(using Frame): Any < Async =
        (cursor, ref) match
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

    /** A reorder button writes the shuffled Seq through its column's ref, keyed on
      * that column's selection.
      */
    private def reorder(
        ref: Maybe[SignalRef[Seq[A]]],
        selRef: Maybe[SignalRef[Set[String]]],
        move: (List[A], A => Boolean) => List[A]
    )(using Frame): Any < Async =
        (ref, selRef) match
            case (Present(r), Present(sr)) =>
                for
                    sel <- sr.get
                    _   <- r.getAndUpdate(xs => move(xs.toList, a => sel.contains(keyOf(a))))
                yield ()
            case _ => ()

    /** Moves the selected items of `from` to the END of `to` (Prime appends),
      * then clears `from`'s selection.
      */
    private def transferSelected(
        from: Maybe[SignalRef[Seq[A]]],
        to: Maybe[SignalRef[Seq[A]]],
        selRef: Maybe[SignalRef[Set[String]]]
    )(using Frame): Any < Async =
        (from, to, selRef) match
            case (Present(f), Present(t), Present(sr)) =>
                for
                    sel <- sr.get
                    xs  <- f.get
                    split = xs.partition(a => sel.contains(keyOf(a)))
                    _ <- f.set(split._2)
                    _ <- t.getAndUpdate(_ ++ split._1)
                    _ <- sr.set(Set.empty)
                yield ()
            case _ => ()

    /** Moves ALL items of `from` to the END of `to`, then clears `from`'s selection. */
    private def transferAll(
        from: Maybe[SignalRef[Seq[A]]],
        to: Maybe[SignalRef[Seq[A]]],
        selRef: Maybe[SignalRef[Set[String]]]
    )(using Frame): Any < Async =
        (from, to, selRef) match
            case (Present(f), Present(t), Present(sr)) =>
                for
                    xs <- f.get
                    _  <- f.set(Seq.empty)
                    _  <- t.getAndUpdate(_ ++ xs)
                    _  <- sr.set(Set.empty)
                yield ()
            case _ => ()
end PickList

object PickList:
    def apply[A](): PickList[A] = new PickList[A]()
