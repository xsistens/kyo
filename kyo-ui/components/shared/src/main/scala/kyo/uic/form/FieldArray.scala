package kyo.uic.form

import kyo.*
import kyo.UI.*

/** A dynamic array of repeated form rows — the analogue of RHF's `useFieldArray`.
  * Each row is its own sub-form [[Form]] scope (so a row can hold any number of fields),
  * tracked in a reactive [[kyo.Signal]] list rather than the parent's static children.
  * That reactivity is the whole point: the parent's aggregations ([[Form.allErrors]],
  * [[Form.isDirty]], the submit gate) fold in each array's row signals through
  * [[kyo.Signal.switchMap]], so adding/removing a row updates them live.
  *
  * The type parameter `R` is the per-row MODEL the builder returns alongside the UI —
  * typically a case class of the row's field handles. It gives the array typed, live,
  * removal-correct access to every row ([[rows]] / [[currentRows]]), which is what
  * cross-row validation ([[satisfy]]) reads. Rows built without a model use `R = Unit`
  * (see [[Form.fieldArray]]); a typed model comes from [[Form.fieldArrayOf]].
  *
  * The set of arrays a form declares is fixed at build time; only the ROWS change at
  * runtime — which is why the aggregation stays correct even for nested arrays: every
  * array exposes a signal reactive over its own rows, and a row's pre-computed signals
  * (captured in [[RowEntry]] when the row is added) already fold in that row's own arrays.
  *
  * Constructed by [[Form.fieldArray]] / [[Form.fieldArrayOf]] — never directly.
  */
final class FieldArray[R] private[form] (
    private val rowsRef: SignalRef[Chunk[FieldArray.RowEntry[R]]],
    private val parent: Form,
    private val build: (Form, FieldArray.Row) => (UI, R) < Async
)(using CanEqual[R, R]):
    import FieldArray.*

    /** This array's cross-row checks, declared via [[satisfy]]. A plain `var` (not an
      * `AtomicRef`): registration runs single-threaded during `build`, exactly like
      * `Form.checks` / `Form.invariants` — which is what lets [[satisfy]] stay pure + chainable.
      */
    private var crossChecks: Chunk[(String, Chunk[R] => Boolean < Async, Reveal)] = Chunk.empty

    /** The live, keyed list of row UIs — drop this into the layout where the rows render.
      * Keyed on each row's stable id, so add/remove/move preserve the DOM (and focus/caret)
      * of the rows that stayed.
      */
    def render(using Frame): UI = rowsRef.foreachKeyed(_.key)(_.ui)

    /** The live list of row models — reactive and removal-correct (derived from the row
      * list, so a removed row drops out). Build a cross-row indicator or an ungated
      * validity signal off it.
      */
    def rows(using Frame): Signal[Chunk[R]] = rowsRef.map(_.map(_.model))

    /** A snapshot of the current row models — the imperative counterpart of [[rows]], for
      * reading inside a [[satisfy]] check or submit handler.
      */
    def currentRows(using Frame): Chunk[R] < Sync = rowsRef.get.map(_.map(_.model))

    /** Attach a cross-row validation, evaluated over ALL current rows at submit — the array
      * analogue of [[Form.satisfy]] (same `satisfy(code)(predicate)` shape), with typed access to
      * every row. Rows are only ever submit-validated (they live outside a `Scope`), so the
      * predicate is the submit-pull form `Chunk[R] => Boolean < Async`; `true` = valid. If it
      * returns `false`, `code` is raised on the parent scope (shows in its `errorSummary`) and
      * submit is blocked. Removal-correct: it reads the live rows, so deleted rows never count.
      * For per-row attribution, `pred` may also call `field.setError` on the offending rows'
      * handles (the value-scoped error clears itself on the next edit). `reveal` chooses between
      * [[Reveal.OnSubmit]] (default: raise `code` into the parent's summary on failure) and
      * [[Reveal.Manual]] (block submit silently; surface it yourself, e.g. via `setError`).
      * Pure + chainable, like [[Form.satisfy]].
      */
    def satisfy(code: String, reveal: Reveal)(pred: Chunk[R] => Boolean < Async): FieldArray[R] =
        crossChecks = crossChecks :+ (code, pred, reveal)
        this

    /** Cross-row validation with the default [[Reveal.OnSubmit]] — see the `(code, reveal)` overload. */
    def satisfy(code: String)(pred: Chunk[R] => Boolean < Async): FieldArray[R] =
        satisfy(code, Reveal.OnSubmit)(pred)

    /** Append a new row: allocate its sub-scope, run `build` (which declares the row's
      * fields and returns its UI + model), capture the row's aggregation signals, publish it.
      */
    def add(using Frame): Unit < Async =
        for
            key   <- parent.mintId()
            child <- parent.newChildScope
            row = new Row(key, this)
            built    <- build(child, row) // (UI, R)
            dirtySig <- child.isDirty
            entries  <- child.errorEntries
            fErrsSig <- child.fieldErrorsAgg(visible = true)
            rawFErrs <- child.fieldErrorsAgg(visible = false)
            _        <- rowsRef.updateAndGet(_ :+ RowEntry(key, child, built._1, built._2, dirtySig, entries, fErrsSig, rawFErrs))
        yield ()

    /** Remove the row at index `i` (no-op if out of range). */
    def removeAt(i: Int)(using Frame): Unit < Sync =
        rowsRef.updateAndGet(rows => if i >= 0 && i < rows.length then rows.patch(i, Nil, 1) else rows).unit

    /** Move the row at `from` to index `to` (clamped; no-op if either is out of range). */
    def move(from: Int, to: Int)(using Frame): Unit < Sync =
        rowsRef.updateAndGet(rows => reorder(rows, from, to)).unit

    private[form] def removeKey(key: String)(using Frame): Unit < Sync =
        rowsRef.updateAndGet(_.filterNot(_.key == key)).unit

    /** Swap the keyed row with its neighbour `delta` positions away (±1 = up/down). */
    private[form] def moveKey(key: String, delta: Int)(using Frame): Unit < Sync =
        rowsRef.updateAndGet { rows =>
            val i = rows.indexWhere(_.key == key)
            val j = i + delta
            if i < 0 || j < 0 || j >= rows.length then rows
            else rows.updated(i, rows(j)).updated(j, rows(i))
        }.unit

    private[form] def indexOf(key: String)(using Frame): Signal[Int] =
        rowsRef.map(_.indexWhere(_.key == key))

    // --- reactive aggregation over the CURRENT rows ----------------------------

    private[form] def dirtySignal(using Frame): Signal[Boolean] =
        rowsRef.switchMap { rows =>
            if rows.isEmpty then Signal.initConst(false)
            else Signal.combineLatestAll(rows.map(_.dirtySig)).map(_.exists(identity))
        }

    /** Union of the rows' error entries (field-id-tagged) over the current rows — folded into
      * `Form.errorEntries`.
      */
    private[form] def entriesSignal(using Frame): Signal[Chunk[Form.ErrorEntry]] =
        rowsRef.switchMap { rows =>
            if rows.isEmpty then Signal.initConst(Chunk.empty[Form.ErrorEntry])
            else Signal.combineLatestAll(rows.map(_.entriesSig)).map(_.foldLeft(Chunk.empty[Form.ErrorEntry])(_ ++ _))
        }

    private[form] def fieldErrorsSignal(visible: Boolean)(using Frame): Signal[Chunk[FieldError]] =
        unionSignal(e => if visible then e.fErrsSig else e.rawFErrsSig)

    private def unionSignal(pick: RowEntry[R] => Signal[Chunk[FieldError]])(using Frame): Signal[Chunk[FieldError]] =
        rowsRef.switchMap { rows =>
            if rows.isEmpty then Signal.initConst(Chunk.empty[FieldError])
            else Signal.combineLatestAll(rows.map(pick)).map(_.foldLeft(Chunk.empty[FieldError])(_ ++ _))
        }

    // --- effectful walks over the CURRENT rows ---------------------------------

    private[form] def validateRows(using Frame): Chunk[(FormField[?], Maybe[FieldError])] < Async =
        for
            rows <- rowsRef.get
            res  <- Kyo.foreach(rows)(_.form.validateSubtree)
        yield res.foldLeft(Chunk.empty[(FormField[?], Maybe[FieldError])])(_ ++ _)

    private[form] def runCheckRows(using Frame): Chunk[FieldError] < Async =
        for
            rows <- rowsRef.get
            res  <- Kyo.foreach(rows)(_.form.runChecks)
        yield res.foldLeft(Chunk.empty[FieldError])(_ ++ _)

    /** Evaluate this array's cross-row [[satisfy]] checks over all current rows; raise each failed
      * code on the parent scope and return the failures (which block submit).
      */
    private[form] def runCrossChecks(using Frame): Chunk[FieldError] < Async =
        for
            models <- currentRows
            fails <- Kyo.foreach(crossChecks) { case (code, pred, reveal) =>
                pred(models).map(ok => (reveal, if ok then (Absent: Maybe[FieldError]) else Present(FieldError(code))))
            }
            // A failing cross-row check always blocks submit; only RAISED into the summary unless Manual.
            _ <- Kyo.foreach(fails) {
                case (reveal, Present(e)) if reveal != Reveal.Manual => parent.raise(e)
                case _                                               => Kyo.unit
            }
        yield fails.collect { case (_, Present(e)) => e }

    private[form] def clearErrorRows(using Frame): Unit < Sync =
        for
            rows <- rowsRef.get
            _    <- Kyo.foreach(rows)(_.form.clearErrorsSubtree)
        yield ()

    private[form] def resetRows(using Frame): Unit < Sync =
        for
            rows <- rowsRef.get
            _    <- Kyo.foreach(rows)(_.form.reset)
        yield ()

    private[form] def markPristineRows(using Frame): Unit < Sync =
        for
            rows <- rowsRef.get
            _    <- Kyo.foreach(rows)(_.form.markPristine)
        yield ()

end FieldArray

object FieldArray:

    /** A materialized row: its stable key, its sub-form, its built UI, its typed model, and
      * the aggregation signals captured when it was added (so the parent folds them in).
      */
    final private[form] case class RowEntry[R](
        key: String,
        form: Form,
        ui: UI,
        model: R,
        dirtySig: Signal[Boolean],
        entriesSig: Signal[Chunk[Form.ErrorEntry]], // field-id-tagged errors — errorEntries/summary view
        fErrsSig: Signal[Chunk[FieldError]],        // gated (touched/submit) — submitDisabled view
        rawFErrsSig: Signal[Chunk[FieldError]]      // ungated — isValid(onlyVisible = false) view
    ) derives CanEqual

    /** The per-row control handed to a build function: it addresses THIS row by its stable
      * key, so a row can remove or reorder itself and read its own index.
      */
    final class Row private[form] (val key: String, array: FieldArray[?]):
        /** Remove this row from the array. */
        def remove(using Frame): Unit < Sync = array.removeKey(key)

        /** Swap this row with the one above it (no-op for the first row). */
        def moveUp(using Frame): Unit < Sync = array.moveKey(key, -1)

        /** Swap this row with the one below it (no-op for the last row). */
        def moveDown(using Frame): Unit < Sync = array.moveKey(key, +1)

        /** This row's live position in the array (`-1` once removed). */
        def index(using Frame): Signal[Int] = array.indexOf(key)
    end Row

    /** Move the element at `from` to index `to`, both clamped to the list (no-op if either
      * is out of range or they are equal).
      */
    private def reorder[R](rows: Chunk[RowEntry[R]], from: Int, to: Int): Chunk[RowEntry[R]] =
        if from < 0 || from >= rows.length || to < 0 || to >= rows.length || from == to then rows
        else
            val moved   = rows(from)
            val without = rows.patch(from, Nil, 1)
            without.patch(to, Seq(moved), 0)

end FieldArray
