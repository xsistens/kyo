package kyo.uic

import kyo.*
import kyo.uic.form.FieldError
import kyo.uic.form.Validator

/** One cell VALUE type: how it prints, how it reads back, and what edits it.
  *
  * The three travel together, and that is the point. A free-standing editor setter beside
  * a free-standing parser lets a Select over labels the parser has never heard of compile
  * and then reject every commit: the two would agree only by the author remembering to
  * make them. Inside one `CellType` they cannot drift, and [[CellType.of]] derives both
  * halves from a single label function.
  *
  * A `given CellType[V]` is what [[Column.editable]] looks up, so a column of a type kyo
  * knows about needs nothing beyond `read` and `write`. [[Column.editableAs]] takes one
  * explicitly, for a domain type or a second editor over a type that already has one.
  */
final class CellType[V] private (
    private[uic] val format: V => String,
    private[uic] val parse: String => Result[FieldError, V],
    private[uic] val editor: CellEditor,
    private[uic] val check: Validator[V]
):
    /** The same value domain behind a different editor: the parser is what stays. */
    def withEditor(e: CellEditor): CellType[V] = new CellType(format, parse, e, check)

    /** Rules the parsed value has to pass before it is written, in the vocabulary
      * `kyo.uic.form` already speaks: `Validator.min`, `Validator.pattern`, an async rule
      * that asks a server. A failing rule leaves the cell open on what the reader typed
      * and shows the error, which is the one thing AG Grid's `valueSetter` cannot do,
      * since a `false` there means both "invalid" and "unchanged" with no room for a
      * message.
      *
      * One rule per type; compose several with `Validator.all` or `and` at the call site,
      * where a `Frame` is in scope.
      */
    def validate(v: Validator[V]): CellType[V] = new CellType(format, parse, editor, v)
end CellType

object CellType:

    def apply[V](format: V => String)(parse: String => Result[FieldError, V])(editor: CellEditor): CellType[V] =
        new CellType(format, parse, editor, always)

    /** The empty rule chain: every parsed value passes. Not `Validator.all()`, which needs
      * a `Frame` this object cannot derive.
      */
    private def always[V]: Validator[V] = Validator[V](_ => (Absent: Maybe[FieldError]))

    /** A fixed set of values, labelled and edited through a Select.
      *
      * The label is both what the editor writes and what the parser reads back, so an
      * option can only round-trip. Two values sharing a label collide, and the Select
      * reports that itself in its `.p-uic-key-error` card, since it is the same failure
      * `optionKey` already guards.
      */
    def of[V](values: Seq[V])(label: V => String): CellType[V] =
        CellType[V](label) { raw =>
            Maybe.fromOption(values.find(v => label(v) == raw)) match
                case Present(v) => Result.succeed(v)
                case Absent =>
                    Result.fail(FieldError("unknown-option", Map("value" -> raw), Present(s"$raw is not one of the options")))
        }(CellEditor.select(values.map(label)))

    given string: CellType[String] = CellType[String](identity)(Result.succeed(_))(CellEditor.text)

    given int: CellType[Int] =
        CellType[Int](_.toString)(whole("integer", "a whole number", s => Maybe.fromOption(s.toIntOption)))(
            CellEditor.number(integer = true)
        )

    given long: CellType[Long] =
        CellType[Long](_.toString)(whole("integer", "a whole number", s => Maybe.fromOption(s.toLongOption)))(
            CellEditor.number(integer = true)
        )

    given double: CellType[Double] =
        CellType[Double](CellEditor.numberText)(whole("number", "a number", s => Maybe.fromOption(s.toDoubleOption)))(
            CellEditor.number()
        )

    given bigDecimal: CellType[BigDecimal] =
        CellType[BigDecimal](_.toString)(
            // `toDoubleOption` answers only whether the text IS a number; the value is
            // built from the text itself, so nothing is rounded through a Double on the
            // way in.
            whole("number", "a number", s => Maybe.fromOption(s.toDoubleOption).map(_ => BigDecimal(s)))
        )(CellEditor.number())

    given boolean: CellType[Boolean] =
        CellType[Boolean](_.toString)(s =>
            s.trim.toLowerCase match
                case "true"  => Result.succeed(true)
                case "false" => Result.succeed(false)
                case other =>
                    Result.fail(FieldError("boolean", Map("value" -> other), Present(s"$other is not true or false")))
        )(CellEditor.checkbox)

    /** A parser over a total projection: `Absent` becomes the error, so every provided
      * type reports its failure the same way and none of them throws.
      */
    private def whole[V](code: String, what: String, f: String => Maybe[V]): String => Result[FieldError, V] =
        raw =>
            f(raw.trim) match
                case Present(v) => Result.succeed(v)
                case Absent =>
                    Result.fail(FieldError(code, Map("value" -> raw), Present(s"$raw is not $what")))

end CellType
