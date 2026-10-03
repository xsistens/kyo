package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.uic.form.FieldError

/** Handlers that must answer about the list AS IT IS, not as it was when their row was painted.
  *
  * Today every emission rebuilds every handler, so a stale one cannot exist and none of this is
  * observable. The moment a row is retained across an emission it keeps the handler it was built
  * with, and each of the five below turns from an invisible property into a defect: a range that
  * spans the wrong rows, a reorder that WRITES a list missing what arrived since, a commit that
  * silently drops what the reader typed, a cell that opens without committing the one that was
  * open, a PageDown that steps by a stale page height.
  *
  * Each test takes a handler off one render, changes the table's rows underneath it without
  * rendering again, and then runs it. That is precisely the situation reuse creates, and it is
  * reachable today because a handler is a value.
  */
class DataTableLiveHandlerTest extends UicTest:

    final case class Item(id: String, name: String) derives CanEqual

    private val trio = List(Item("1", "A"), Item("2", "B"), Item("3", "C"))

    private def rowsOf(node: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elements(node).map(_.filter(_.attrs.cssClasses.contains("p-datatable-selectable-row")))

    private def shiftClick(row: UI.Ast.Element)(using Frame): Any < Async =
        row.attrs.onClickEvt match
            case Present(f) => f(MouseEvent(row.attrs.identifier, Modifiers(shift = true)))
            case Absent     => throw new AssertionError("the row declares no typed click handler")

    private def plainClick(row: UI.Ast.Element)(using Frame): Any < Async =
        row.attrs.onClickEvt match
            case Present(f) => f(MouseEvent(row.attrs.identifier, Modifiers.none))
            case Absent     => throw new AssertionError("the row declares no typed click handler")

    "a range spans the list as it is now, not as it was painted" in {
        for
            rows   <- Signal.initRef[Seq[Item]](trio)
            sel    <- Signal.initRef(Set.empty[String])
            anchor <- Signal.initRef(Absent: Maybe[String])
            err    <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selected(sel)
                .metaKeySelection(true)
                .selectionMode(uic.SelectionMode.Multiple)
                .selectionAnchor(anchor)
                // `wired` is the mount's seam, so the anchor arrives the way the mount hands it over.
                .wired("t", Map.empty, err, _ => (), anchor = Present(anchor))
            painted <- rowsOf(ui)
            // The anchor is set on the first row, from the render the reader is looking at.
            _ <- plainClick(painted.head)
            // A row arrives between the anchor and the row about to be shift-clicked. Nothing is
            // rendered again: the handlers below are the ones from the render above.
            _     <- rows.set(List(Item("1", "A"), Item("9", "X"), Item("2", "B"), Item("3", "C")))
            at    <- anchor.get
            _     <- shiftClick(painted(1))
            after <- sel.get
        yield
            assert(at == Present("1"), s"the first click left the anchor at $at")
            assert(
                after == Set("1", "9", "2"),
                s"the range spanned $after; over the painted list it would have been Set(1, 2)"
            )
    }

    "a reorder writes the list as it is now" in {
        for
            rows  <- Signal.initRef[Seq[Item]](trio)
            err   <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            drag  <- Signal.initRef(Absent: Maybe[uic.RowDrag])
            moved <- Signal.initRef(List.empty[List[String]])
            table = uic.DataTable[Item]().rows(rows).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .reorderableRows(true)
                .onRowReorder(m => moved.getAndUpdate(_ :+ m.rows.map(_.name).toList))
            measureAll = (ids: Seq[String]) =>
                Chunk.from(ids.zipWithIndex.map((_, i) => UI.Rect(0, i * 40, 200, 40, 1000, 800))): Chunk[
                    UI.Rect
                ] < Async
            ui = table.wired(
                "t",
                Map.empty,
                err,
                _ => (),
                Map.empty,
                measureAll,
                Absent,
                Absent,
                Absent,
                Absent,
                Present(drag)
            )
            grips <- elements(ui).map(_.filter(_.attrs.cssClasses.contains("p-datatable-reorderable-row-handle")))
            grip = grips.head
            down = grip.attrs.onPointerDown.getOrElse(throw new AssertionError("no grab"))
            move = grip.attrs.onPointerMove.getOrElse(throw new AssertionError("no drag"))
            up   = grip.attrs.onPointerUp.getOrElse(throw new AssertionError("no release"))
            _ <- down(UI.PointerEvent(0, 10, 0, 0, 200, 40, 1, Absent))
            _ <- move(UI.PointerEvent(0, 95, 0, 0, 200, 40, 1, Absent))
            // A row is appended while the drag is in flight — the shape a paginated or streaming
            // table produces on its own.
            _     <- rows.set(trio :+ Item("4", "D"))
            _     <- up(UI.PointerEvent(0, 95, 0, 0, 200, 40, 1, Absent))
            after <- rows.get
            fired <- moved.get
        yield
            assert(
                after.map(_.name) == Seq("B", "A", "C", "D"),
                s"the stored list is ${after.map(_.name)}; a stale handler would have written B, A, C"
            )
            assert(fired == List(List("B", "A", "C", "D")))
    }

    "an open cell commits against the rows that are there now" in {
        for
            rows    <- Signal.initRef[Seq[Item]](trio)
            editing <- Signal.initRef(Absent: Maybe[CellPath])
            name    <- Signal.initRef("")
            err     <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            edited  <- Signal.initRef(List.empty[String])
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id)
                .columns(uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)))
                .editingCell(editing)
                .onCellValueChanged(c => edited.getAndUpdate(_ :+ c.after.name))
                .wired("t", Map(List("Name") -> name), err, _ => ())
            cells <- elements(ui).map(_.filter(_.attrs.cssClasses.contains("p-editable-column")))
            // Open the first row's cell and type into it.
            _ <- cells.head.attrs.onClick.getOrElse(throw new AssertionError("no cell click"))
            _ <- name.set("A-typed")
            // The rows are replaced by an equal-keyed list — the shape a re-fetch produces. The
            // cell handlers below still belong to the render above.
            _ <- rows.set(List(Item("1", "A"), Item("2", "B"), Item("3", "C"), Item("4", "D")))
            // Clicking another cell is a way of leaving the open one, so it has to commit it.
            _     <- cells(1).attrs.onClick.getOrElse(throw new AssertionError("no cell click"))
            fired <- edited.get
            after <- rows.get
        yield
            assert(fired == List("A-typed"), s"the edit committed as $fired")
            assert(after.map(_.name) == Seq("A-typed", "B", "C", "D"))
    }

end DataTableLiveHandlerTest
