package kyo.uic

import kyo.*
import kyo.UI.*

/** What a [[DataTable]] hands an editor when it opens a cell.
  *
  * `draft` is the cell's text, two-way: the table seeds it from the row before the editor
  * appears, the editor writes into it, and the commit reads it back. A ref and not a
  * rendered value, because an editor bound to a ref survives a re-render with what the
  * reader typed, and because the table never subscribes to it, so typing costs the
  * editor's own cell and nothing more.
  *
  * `commit` and `cancel` end the edit the way Enter and Escape do, for an editor whose own
  * gesture already decides (a picked option, a toggled box).
  *
  * The editor is told nothing about the row, the column or the value type. It moves text,
  * and the column's [[CellType]] turns that text back into a value: an editor that knew
  * the type would have to agree with a parser it cannot see.
  */
final case class EditorParams(
    draft: SignalRef[String],
    commit: Any < Async,
    cancel: Any < Async
)

/** The UI one open cell shows. A plain function, so any UI over [[EditorParams]] is an
  * editor and a caller implements no interface to supply one.
  *
  * The `Frame` arrives as a context function rather than a `using` clause on a method,
  * because an editor is a VALUE: `kyo.uic` cannot derive a Frame of its own, so a provided
  * editor defined here would have nowhere to get one. Written this way the frame is the
  * table's, taken at the point the editor is rendered. Same shape as `ColumnOf`.
  */
type CellEditor = EditorParams => (Frame ?=> UI)

/** The editors [[CellType]] uses out of the box.
  *
  * All four bind `params.draft` two-way. The text editor binds it directly; the three that
  * edit another shape read it through a subscription of their own and write it back
  * formatted, so the draft stays one string per column no matter what edits it.
  *
  * Each seeds focus (`focusAuto`), since a cell that opens from the keyboard has nothing
  * else to hand focus over: the element is new, so only the seed reaches it.
  */
object CellEditor:

    /** Prime's text field. Every keystroke is written; Enter and Escape leave the field
      * and reach the cell, which is what ends the edit.
      */
    val text: CellEditor = p => Input().value(p.draft).focusAuto(true)

    /** Prime's number field, with the native stepper and range. `integer` turns on the
      * whole-number step, which is what keeps an `Int` column from ever seeing a decimal.
      */
    def number(
        min: Maybe[Double] = Absent,
        max: Maybe[Double] = Absent,
        step: Maybe[Double] = Absent,
        integer: Boolean = false
    ): CellEditor =
        p =>
            // The draft is text and the field is numeric, so this one reads the draft
            // through a subscription and writes it back formatted. The subscription is
            // local to the cell: it is the editor's own value binding, not the table's.
            p.draft.render { cur =>
                var f = InputNumber()
                    .value(Maybe.fromOption(cur.toDoubleOption).getOrElse(0.0))
                    .focusAuto(true)
                    .onChange(d => p.draft.set(numberText(d)))
                min.foreach(v => f = f.min(v))
                max.foreach(v => f = f.max(v))
                step.foreach(v => f = f.step(v))
                if integer then f = f.integer(true)
                f.toUI
            }

    /** Prime's Select over a fixed option list. Picking writes and commits in one gesture,
      * because a pick is a decision, not a keystroke.
      *
      * The panel does not open by itself when the cell opens from the keyboard. AG Grid's
      * provided select editor has the same shape for the same reason: opening a picker is
      * a gesture the browser gives no programmatic hold on, and faking one with a bound
      * open-ref would leave the panel open over a cell the reader has already left.
      */
    def select(options: Seq[String]): CellEditor =
        p =>
            p.draft.render { cur =>
                Select[String]()
                    .options(options)
                    .current(cur)
                    .onChange(v => p.draft.set(v).andThen(p.commit))
                    .toUI
            }

    /** Prime's checkbox. A toggle writes and commits at once, so a boolean cell needs no
      * second gesture to confirm what it already shows.
      */
    val checkbox: CellEditor =
        p =>
            p.draft.render { cur =>
                CheckBox().checked(cur == "true").onChange(b => p.draft.set(b.toString).andThen(p.commit)).toUI
            }

    /** How a number reaches the draft as text: a whole value prints without a decimal
      * point, so an `Int` column's parser reads back what its editor wrote. Without it a
      * stepper click on an integer field would produce `5.0` and fail to parse.
      */
    private[uic] def numberText(d: Double): String =
        if d.isWhole && d.abs < 1e15 then d.toLong.toString else d.toString

end CellEditor
