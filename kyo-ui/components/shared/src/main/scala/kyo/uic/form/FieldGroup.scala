package kyo.uic.form

import kyo.*
import kyo.UI.*
import scala.NamedTuple.NamedTuple
import scala.annotation.implicitNotFound

/** A group of fields declared together by [[Form.fields]], read by name, and submitted as one validated named tuple.
  *
  * `N` is the tuple of names, `H` the tuple of live handles (what each spec declared: a [[FormField]] or [[NumberField]]
  * refined to its `Valid`, a [[GroupArray]], or a nested [[FieldGroup]]), and `V` the tuple of what each handle validates
  * to (a field's `Valid`, an array's `Chunk` of row tuples, a child's nested tuple). `H` and `V` are computed from the
  * specs by [[GroupMembers]], so the caller never writes them:
  * {{{
  * f <- form.fields((customer = form.field.rules(Validator.required()), qty = form.numberField(1)))
  * _ = uic.Input().bind(f.customer)                    // FormField[String] { type Valid = String }
  * _ = f.submit(v => order(v.customer, v.qty))         // v: (customer: String, qty: Int)
  * }}}
  *
  * Names are read through `Selectable` with a named-tuple `Fields` type, so a member of this class (`submit`, `scope`,
  * and on a row `remove`, `moveUp`, `moveDown`, `index`) shadows a field of the same name.
  *
  * [[submit]] is [[Form.submit]] with every leaf of the group named: one rule run per field, the whole scope validated,
  * focus on the first invalid field, `submitting` held through the handler, failures to the `onFailure` rules.
  */
sealed class FieldGroup[N <: Tuple, H <: Tuple, V <: Tuple] private[form] (
    /** The scope the group's fields were declared in: the form for [[Form.fields]], the child scope for [[Form.child]],
      * the row's scope for an array row. Hand it to [[Form.errorSummary]] for a summary of this group alone.
      */
    val scope: Form,
    private[form] val members: GroupMembers[N, ?] { type Handles = H; type Outs = V },
    private[form] val handles: H
) extends Selectable:

    /** What `group.name` selects: the declared handle under that name. */
    type Fields = NamedTuple[N, H]

    /** `Selectable`'s hook: the compiler turns `group.name` into this call, typed by [[Fields]]. */
    def selectDynamic(name: String): Any = handles.productElement(members.names.indexOf(name))

    /** Validate [[scope]] and, only if everything holds, run `onValid` with the validated values of every field of the
      * group, by name: each field's `Valid`, each array as the `Chunk` of its current rows' tuples, each child as its own
      * tuple. The values come from the one rule run that decided validity, as with [[Form.submit]].
      */
    def submit(onValid: NamedTuple[N, V] => UI.Action)(using Frame): UI.Action =
        members.snapshot(handles).map { snap =>
            scope.submitWith(snap.fields) {
                // `V` is a `NamedTuple[N, V]` by the named tuple's lower bound, so no conversion is needed here.
                snap.verdict().map((errs, value) => (errs, value.map(v => () => onValid(v))))
            }
        }
end FieldGroup

/** One row of a [[GroupArray]]: its fields by name, plus the controls that address this row by its stable key. */
final class ArrayRow[N <: Tuple, H <: Tuple, V <: Tuple] private[form] (
    rowScope: Form,
    rowMembers: GroupMembers[N, ?] { type Handles = H; type Outs = V },
    rowHandles: H,
    ctl: FieldArray.Row
) extends FieldGroup[N, H, V](rowScope, rowMembers, rowHandles):

    /** Remove this row from its array. */
    def remove(using Frame): Unit < Async = ctl.remove

    /** Swap this row with the one above it (no-op for the first row). */
    def moveUp(using Frame): Unit < Sync = ctl.moveUp

    /** Swap this row with the one below it (no-op for the last row). */
    def moveDown(using Frame): Unit < Sync = ctl.moveDown

    /** This row's live position in the array (`-1` once removed). */
    def index(using Frame): Signal[Int] = ctl.index
end ArrayRow

object ArrayRow:
    given [N <: Tuple, H <: Tuple, V <: Tuple]: CanEqual[ArrayRow[N, H, V], ArrayRow[N, H, V]] = CanEqual.derived

/** A typed array of field groups, declared by [[Form.array]]. Each row is its own scope holding the fields the row
  * builder declared; [[render]] lays them out, and a group submit hands the array over as the `Chunk` of the current
  * rows' validated tuples. Built over a [[FieldArray]], so rows take part in errors, `isDirty`, `reset`,
  * `markPristine` and focus-first-invalid exactly as a `fieldArrayOf` row does.
  */
final class GroupArray[N <: Tuple, H <: Tuple, V <: Tuple] private[form] (
    private[form] val underlying: FieldArray[ArrayRow[N, H, V]]
):
    /** The live, keyed list of rows, each laid out by `row`. Keyed on the row's stable id, so add, remove and move keep the
      * DOM of the rows that stayed.
      */
    def render(row: ArrayRow[N, H, V] => UI)(using Frame): UI = underlying.renderWith(row)

    /** Append a row, declaring its fields in a fresh row scope. */
    def add(using Frame): Unit < Async = underlying.add

    /** Remove the row at index `i` (no-op if out of range). */
    def removeAt(i: Int)(using Frame): Unit < Async = underlying.removeAt(i)

    /** Move the row at `from` to index `to` (no-op if either is out of range). */
    def move(from: Int, to: Int)(using Frame): Unit < Sync = underlying.move(from, to)

    /** The live list of rows. */
    def rows(using Frame): Signal[Chunk[ArrayRow[N, H, V]]] = underlying.rows

    /** A snapshot of the current rows. */
    def currentRows(using Frame): Chunk[ArrayRow[N, H, V]] < Sync = underlying.currentRows

    /** A cross-row check over the current rows, as [[FieldArray.satisfy]]. */
    def satisfy(code: String, reveal: Reveal = Reveal.OnSubmit)(pred: Chunk[ArrayRow[N, H, V]] => Boolean < Async): this.type =
        discard(underlying.satisfy(code, reveal)(pred))
        this
end GroupArray

/** The declaration of a [[GroupArray]], made by [[Form.array]]: a pure value, declared when [[Form.fields]] declares the
  * group it sits in.
  */
sealed abstract class ArraySpec[N <: Tuple, H <: Tuple, V <: Tuple] private[form] (
    private[form] val form: Form,
    private[form] val initial: Int
):
    /** Declare one row's fields in `scope` and hand back the row. */
    private[form] def declareRow(scope: Form, ctl: FieldArray.Row)(using Frame): ArrayRow[N, H, V] < Async

    private[form] def declare(using Frame): GroupArray[N, H, V] < Async =
        form.fieldArrayOf[ArrayRow[N, H, V]](initial)((scope, ctl) => declareRow(scope, ctl).map(r => (fragment(), r)))
            .map(new GroupArray(_))
end ArraySpec

private[form] object ArraySpec:
    def of[N <: Tuple, S <: Tuple, H <: Tuple, V <: Tuple](parent: Form, initial: Int, row: Form => NamedTuple[N, S])(using
        members: GroupMembers.Aux[N, S, H, V]
    ): ArraySpec[N, H, V] =
        new ArraySpec[N, H, V](parent, initial):
            private[form] def declareRow(scope: Form, ctl: FieldArray.Row)(using Frame) =
                members.declare(row(scope).toTuple).map(hs => new ArrayRow[N, H, V](scope, members, hs, ctl))
end ArraySpec

/** The declaration of a child group, made by [[Form.child]] with a builder: a pure value, declared (child scope first,
  * then its fields in it) when [[Form.fields]] declares the group it sits in.
  */
sealed abstract class ChildSpec[N <: Tuple, H <: Tuple, V <: Tuple] private[form] (private[form] val form: Form):
    private[form] def declare(using Frame): FieldGroup[N, H, V] < Async

private[form] object ChildSpec:
    def of[N <: Tuple, S <: Tuple, H <: Tuple, V <: Tuple](parent: Form, build: Form => NamedTuple[N, S])(using
        members: GroupMembers.Aux[N, S, H, V]
    ): ChildSpec[N, H, V] =
        new ChildSpec[N, H, V](parent):
            private[form] def declare(using Frame) =
                parent.child().map(c =>
                    members.declare(build(c).toTuple).map(hs => new FieldGroup[N, H, V](c, members, hs))
                )
end ChildSpec

/** What a group submit validates, snapshotted at the click: the fields it names (in group order, rows as they are now)
  * and the run that validates exactly those fields once and yields the group's value when all of them passed.
  */
final private[form] class Snapshot[+O](
    val fields: Chunk[FormField[?]],
    val verdict: () => (Chunk[Maybe[FieldError]], Maybe[O]) < Async
)

private[form] object Snapshot:
    def leaf[B](f: Validated { type Valid = B })(using Frame): Snapshot[B] =
        Snapshot(Chunk(f.submitField), () => f.verdict.map(r => (Chunk(r.failure), r.value)))

    /** The snapshots of several groups (an array's rows), validated in parallel, yielding all values or none. */
    def all[O](parts: Chunk[Snapshot[O]])(using Frame): Snapshot[Chunk[O]] =
        Snapshot(
            parts.flatMap(_.fields),
            () =>
                Async.foreach(parts)(_.verdict()).map { rs =>
                    val values = rs.flatMap(_._2.toChunk)
                    (rs.flatMap(_._1), if values.length == rs.length then Present(values) else Absent)
                }
        )
end Snapshot

/** Evidence that `S` is a spec [[Form.fields]] can declare: what it declares to (`Handle`) and what that validates to
  * (`Out`). One instance per spec kind, in this companion; a tuple of them is [[GroupMembers]].
  */
@implicitNotFound(
    "${S} is not a spec a field group can declare. A group member is an UNDECLARED spec: form.field(...), form.numberField(...), form.array(...)(row => (...)) or form.child(scope => (...)), without .declare"
)
trait GroupMember[S]:
    type Handle
    type Out
    private[form] def declare(spec: S)(using Frame): Handle < Async
    private[form] def snapshot(h: Handle)(using Frame): Snapshot[Out] < Sync
end GroupMember

object GroupMember:
    type Aux[S, H, O] = GroupMember[S] { type Handle = H; type Out = O }

    given field[A, B]: Aux[FieldSpec[A] { type Valid = B }, FormField[A] { type Valid = B }, B] =
        new GroupMember[FieldSpec[A] { type Valid = B }]:
            type Handle = FormField[A] { type Valid = B }
            type Out    = B
            private[form] def declare(spec: FieldSpec[A] { type Valid = B })(using Frame) = spec.declare
            private[form] def snapshot(h: Handle)(using Frame)                            = Snapshot.leaf(h)

    given number[A, B]: Aux[NumberFieldSpec[A] { type Valid = B }, NumberField[A] { type Valid = B }, B] =
        new GroupMember[NumberFieldSpec[A] { type Valid = B }]:
            type Handle = NumberField[A] { type Valid = B }
            type Out    = B
            private[form] def declare(spec: NumberFieldSpec[A] { type Valid = B })(using Frame) = spec.declare
            private[form] def snapshot(h: Handle)(using Frame)                                  = Snapshot.leaf(h)

    given array[N <: Tuple, H <: Tuple, V <: Tuple]: Aux[ArraySpec[N, H, V], GroupArray[N, H, V], Chunk[NamedTuple[N, V]]] =
        new GroupMember[ArraySpec[N, H, V]]:
            type Handle = GroupArray[N, H, V]
            type Out    = Chunk[NamedTuple[N, V]]
            private[form] def declare(spec: ArraySpec[N, H, V])(using Frame) = spec.declare
            private[form] def snapshot(h: Handle)(using Frame)               =
                h.currentRows.map(rows => Kyo.foreach(rows)(r => r.members.snapshot(r.handles)).map(Snapshot.all(_)))

    given child[N <: Tuple, H <: Tuple, V <: Tuple]: Aux[ChildSpec[N, H, V], FieldGroup[N, H, V], NamedTuple[N, V]] =
        new GroupMember[ChildSpec[N, H, V]]:
            type Handle = FieldGroup[N, H, V]
            type Out    = NamedTuple[N, V]
            private[form] def declare(spec: ChildSpec[N, H, V])(using Frame) = spec.declare
            private[form] def snapshot(h: Handle)(using Frame)               = h.members.snapshot(h.handles)
end GroupMember

/** Evidence that the named tuple of specs with names `N` and specs `S` can be declared as a group: the names at run
  * time, the tuple of handles it declares to (`Handles`) and the tuple of what those validate to (`Outs`). Derived
  * element by element from [[GroupMember]]; nothing to write by hand.
  */
@implicitNotFound(
    "Cannot declare a field group from these specs. Every member must be an UNDECLARED spec: form.field(...), form.numberField(...), form.array(...)(row => (...)) or form.child(scope => (...)), without .declare"
)
trait GroupMembers[N <: Tuple, S <: Tuple]:
    type Handles <: Tuple
    type Outs <: Tuple
    private[form] def names: Chunk[String]
    private[form] def declare(specs: S)(using Frame): Handles < Async
    private[form] def snapshot(hs: Handles)(using Frame): Snapshot[Outs] < Sync
end GroupMembers

object GroupMembers:
    type Aux[N <: Tuple, S <: Tuple, H <: Tuple, O <: Tuple] = GroupMembers[N, S] { type Handles = H; type Outs = O }

    given empty: Aux[EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple] =
        new GroupMembers[EmptyTuple, EmptyTuple]:
            type Handles = EmptyTuple
            type Outs    = EmptyTuple
            private[form] val names: Chunk[String]                    = Chunk.empty
            private[form] def declare(specs: EmptyTuple)(using Frame) = EmptyTuple
            private[form] def snapshot(hs: EmptyTuple)(using Frame)   = Snapshot(Chunk.empty, () => (Chunk.empty, Present(EmptyTuple)))

    // The element and the rest come in as `Aux` with type parameters rather than as paths (`m.Handle`), so the
    // instance's type, and every type computed from it, is concrete: a message then says `String`, not `?1.Out`.
    given cons[L <: String, NT <: Tuple, S, ST <: Tuple, SH, SO, RH <: Tuple, RO <: Tuple](using
        name: ValueOf[L],
        m: GroupMember.Aux[S, SH, SO],
        rest: GroupMembers.Aux[NT, ST, RH, RO]
    ): Aux[L *: NT, S *: ST, SH *: RH, SO *: RO] =
        new GroupMembers[L *: NT, S *: ST]:
            type Handles = SH *: RH
            type Outs    = SO *: RO
            private[form] val names                                = name.value +: rest.names
            private[form] def declare(specs: S *: ST)(using Frame) =
                m.declare(specs.head).map(h => rest.declare(specs.tail).map(t => h *: t))
            private[form] def snapshot(hs: Handles)(using Frame) =
                for
                    a <- m.snapshot(hs.head)
                    b <- rest.snapshot(hs.tail)
                yield Snapshot(
                    a.fields ++ b.fields,
                    () =>
                        Async.zip(a.verdict(), b.verdict()).map { (x, y) =>
                            (x._1 ++ y._1, x._2.flatMap(o => y._2.map(os => o *: os)))
                        }
                )
end GroupMembers
