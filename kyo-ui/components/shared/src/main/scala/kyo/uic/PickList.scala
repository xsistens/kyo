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
    disabledFlag: Boolean = false
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

    private def keyOf(a: A): String =
        keyF.orElse(labelF).map(_(a)).getOrElse(a.toString)

    private def withRef[T](ref: Maybe[SignalRef[T]], fallback: T)(k: T => UI)(using Frame): UI =
        ref match
            case Present(r) => r.render(k)
            case Absent     => k(fallback)

    private[uic] def render(using Frame): UI =
        withRef(sourceRef, Seq.empty[A]) { src =>
            withRef(targetRef, Seq.empty[A]) { tgt =>
                withRef(sourceSelectedRef, Set.empty[String]) { srcSel =>
                    withRef(targetSelectedRef, Set.empty[String]) { tgtSel =>
                        body(src, tgt, srcSel, tgtSel)
                    }
                }
            }
        }

    private def moveButton(glyph: IconGlyph, name: String, off: Boolean, act: Any < Async)(using Frame): UI =
        Button()
            .icon(glyph)
            .severity(Severity.Secondary)
            .accessibleName(name)
            .disabled(disabledFlag || off)
            .onClick(act)
            .render

    private def body(src: Seq[A], tgt: Seq[A], srcSel: Set[String], tgtSel: Set[String])(using Frame): UI =
        val srcMoveOff = srcSel.isEmpty || sourceRef.isEmpty
        val tgtMoveOff = tgtSel.isEmpty || targetRef.isEmpty

        def reorderRail(cls: String, ref: Maybe[SignalRef[Seq[A]]], selRef: Maybe[SignalRef[Set[String]]], off: Boolean): UI =
            div.cssClass("p-picklist-controls").cssClass(cls)(
                toChild(moveButton(Icons.angleUp, "Move Up", off, reorder(ref, selRef, ListReorder.moveUp))),
                toChild(moveButton(Icons.angleDoubleUp, "Move Top", off, reorder(ref, selRef, ListReorder.moveTop))),
                toChild(moveButton(Icons.angleDown, "Move Down", off, reorder(ref, selRef, ListReorder.moveDown))),
                toChild(moveButton(Icons.angleDoubleDown, "Move Bottom", off, reorder(ref, selRef, ListReorder.moveBottom)))
            )

        def column(cls: String, xs: Seq[A], sel: Set[String], selRef: Maybe[SignalRef[Set[String]]]): UI =
            var lb = Listbox()
                .items(xs.map(a => ListItem(TextValue.Const(labelF.map(_(a)).getOrElse(a.toString)), keyOf(a)))*)
                .selectionMode(SelectionMode.Multiple)
                .disabled(disabledFlag)
            selRef.foreach(r => lb = lb.value(r))
            templateF.foreach { f =>
                val byKey = xs.map(a => keyOf(a) -> a).toMap
                lb = lb.itemTemplate(li =>
                    byKey.get(li.id).map(f).getOrElse(li.text match
                        case TextValue.Const(t) => stringToUI(t)
                        case TextValue.Dyn(s)   => s.render(t => stringToUI(t)))
                )
            }
            div.cssClass("p-picklist-list-container").cssClass(cls)(toChild(lb.resolved(sel, "")))
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
                    column("p-picklist-source-list-container", src, srcSel, sourceSelectedRef),
                    transfer,
                    column("p-picklist-target-list-container", tgt, tgtSel, targetSelectedRef)
                ) ++ targetRail

        div.cssClass("p-picklist").cssClass("p-component")(parts.map(toChild)*)
    end body

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
