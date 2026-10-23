package kyo.uic.form

import kyo.*
import kyo.UI.*
import scala.NamedTuple.NamedTuple
import scala.annotation.implicitNotFound
import scala.util.NotGiven

/** The type classes behind a field group, the named tuple of declared fields that [[Form.fields]] returns.
  *
  * A group is a plain named tuple: `f.customer` is a named-tuple selection, so a field can be called anything a named
  * tuple lets it be called, and nothing the group offers can stand in its way, because the group offers nothing. What
  * acts on a group is a function that takes it: [[Form.submit]], [[Form.scopeOf]], and the row control
  * [[GroupArray.render]] hands over beside each row. A named-tuple selection also wins over an extension method of the
  * same name, which a `Selectable` member selection does not, so no import can shadow a field either. The names it
  * cannot win against are [[Reserved]], and a group refuses them.
  *
  * Two derivations run over the group's types, and nothing here is written by hand:
  *
  *   - [[Specs]] (with [[Spec]] per member) from the named tuple of SPECS to the named tuple of handles they declare
  *     to: a [[FieldSpec]] to its [[FormField]], a [[NumberFieldSpec]] to its [[NumberField]], an [[ArraySpec]] to its
  *     [[GroupArray]], a [[ChildSpec]] to the child's own named tuple. It also refuses a name the named tuple itself
  *     answers ([[FreeName]]).
  *   - [[Shape]] (with [[Member]] per handle) from the named tuple of HANDLES to what each validates to: a field's
  *     `Valid`, an array's `Chunk` of row tuples, a child's tuple. This is what [[Form.submit]] reads, so a submit
  *     needs the group and nothing else.
  *
  * Both are type classes rather than match types: a match type cannot bind the `Valid` refinement of a field
  * (`case FormField[a] { type Valid = v }` leaves `v` unbound), and an instance unifies it without trouble.
  */
object FieldGroup:

    /** The names a field group member cannot have, because a named tuple answers them itself: `Any`'s members, and
      * `apply`, which the compiler refuses to select from a named tuple that has such a field.
      */
    type Reserved = "toString" | "hashCode" | "equals" | "getClass" | "isInstanceOf" | "asInstanceOf" | "apply" | "==" | "!=" | "##"

    /** Evidence that `L` names a field a named tuple selection reaches, i.e. that it is not [[Reserved]]. */
    @implicitNotFound(
        "A field group member cannot be named ${L}: the named tuple answers that name itself, so the field could never be read by it. Rename the member."
    )
    sealed trait FreeName[L <: String]

    /** The one instance: any name that is not [[Reserved]]. */
    object FreeName:
        /** `L` is free when it is not one of the [[Reserved]] names. */
        given free[L <: String](using NotGiven[L <:< Reserved]): FreeName[L] = new FreeName[L] {}

    /** Evidence that `S` is a spec a group can declare, and what it declares to (`Handle`). One instance per spec kind;
      * a tuple of them is [[Specs]].
      */
    @implicitNotFound(
        "${S} is not a spec a field group can declare. A group member is an UNDECLARED spec: form.field(...), form.numberField(...), form.array(...)(row => (...)) or form.child(scope => (...)), without .declare"
    )
    sealed trait Spec[S]:
        /** What the spec declares to. */
        type Handle
        private[form] def declare(spec: S, scope: Form)(using Frame): Handle < Async
    end Spec

    /** The instances, one per spec kind. */
    object Spec:
        /** A [[Spec]] whose handle is `H`, with the handle as a type parameter so it stays concrete. */
        type Aux[S, H] = Spec[S] { type Handle = H }

        /** A field spec declares to the field, refined to what its rules yield. */
        given field[A, B]: Aux[FieldSpec[A] { type Valid = B }, FormField[A] { type Valid = B }] =
            new Spec[FieldSpec[A] { type Valid = B }]:
                type Handle = FormField[A] { type Valid = B }
                private[form] def declare(spec: FieldSpec[A] { type Valid = B }, scope: Form)(using Frame) = spec.declareIn(scope)

        /** A number field spec declares to the number field, refined to what its rules yield. */
        given number[A, B]: Aux[NumberFieldSpec[A] { type Valid = B }, NumberField[A] { type Valid = B }] =
            new Spec[NumberFieldSpec[A] { type Valid = B }]:
                type Handle = NumberField[A] { type Valid = B }
                private[form] def declare(spec: NumberFieldSpec[A] { type Valid = B }, scope: Form)(using Frame) = spec.declareIn(scope)

        /** An array spec declares to its [[GroupArray]]. */
        given array[N <: Tuple, H <: Tuple]: Aux[ArraySpec[N, H], GroupArray[N, H]] =
            new Spec[ArraySpec[N, H]]:
                type Handle = GroupArray[N, H]
                private[form] def declare(spec: ArraySpec[N, H], scope: Form)(using Frame) = spec.declareIn(scope)

        /** A child spec declares to the child's named tuple of fields. */
        given child[N <: Tuple, H <: Tuple]: Aux[ChildSpec[N, H], NamedTuple[N, H]] =
            new Spec[ChildSpec[N, H]]:
                type Handle = NamedTuple[N, H]
                private[form] def declare(spec: ChildSpec[N, H], scope: Form)(using Frame) = spec.declareIn(scope)
    end Spec

    /** Evidence that a named tuple with names `N` and specs `S` can be declared as a group, and the tuple of handles it
      * declares to (`Handles`). Derived member by member from [[Spec]] and [[FreeName]].
      */
    @implicitNotFound(
        "Cannot declare a field group from these specs. Every member must be an UNDECLARED spec (form.field(...), form.numberField(...), form.array(...)(row => (...)) or form.child(scope => (...)), without .declare), and no member may be named toString, hashCode, equals, getClass, isInstanceOf, asInstanceOf, apply, ==, != or ##"
    )
    sealed trait Specs[N <: Tuple, S <: Tuple]:
        /** What the spec declares to. */
        type Handles <: Tuple
        private[form] def declare(specs: S, scope: Form)(using Frame): Handles < Async
    end Specs

    /** The derivation, member by member. */
    object Specs:
        /** [[Specs]] declaring to the handles `H`, as a type parameter so it stays concrete. */
        type Aux[N <: Tuple, S <: Tuple, H <: Tuple] = Specs[N, S] { type Handles = H }

        /** No specs declare to no handles. */
        given empty: Aux[EmptyTuple, EmptyTuple, EmptyTuple] =
            new Specs[EmptyTuple, EmptyTuple]:
                type Handles = EmptyTuple
                private[form] def declare(specs: EmptyTuple, scope: Form)(using Frame) = EmptyTuple

        // The member and the rest come in as `Aux` with type parameters rather than as paths (`m.Handle`), so the
        // instance's type, and every type computed from it, is concrete: a message then says `String`, not `?1.Handle`.
        /** A member with a free name and a declarable spec, then the rest. */
        given cons[L <: String, NT <: Tuple, S, ST <: Tuple, SH, RH <: Tuple](using
            free: FreeName[L],
            m: Spec.Aux[S, SH],
            rest: Aux[NT, ST, RH]
        ): Aux[L *: NT, S *: ST, SH *: RH] =
            new Specs[L *: NT, S *: ST]:
                type Handles = SH *: RH
                private[form] def declare(specs: S *: ST, scope: Form)(using Frame) =
                    m.declare(specs.head, scope).map(h => rest.declare(specs.tail, scope).map(t => h *: t))
    end Specs

    /** Evidence that `X` is a declared group member, what it validates to (`Out`), and where it lives. One instance per
      * handle kind; a tuple of them is [[Shape]].
      */
    @implicitNotFound(
        "${X} is not a member of a field group: a group holds declared fields, number fields, GroupArrays and child groups, as Form.fields returns them"
    )
    sealed trait Member[X]:
        /** What the member validates to. */
        type Out
        private[form] def snapshot(x: X)(using Frame): Snapshot[Out] < Sync

        /** The scope the member was declared in. */
        private[form] def owner(x: X): Form
    end Member

    /** The instances, one per handle kind. */
    object Member:
        /** A [[Member]] validating to `O`, as a type parameter so it stays concrete. */
        type Aux[X, O] = Member[X] { type Out = O }

        /** A field validates to what its rules yield. */
        given field[A, B]: Aux[FormField[A] { type Valid = B }, B] =
            new Member[FormField[A] { type Valid = B }]:
                type Out = B
                private[form] def snapshot(x: FormField[A] { type Valid = B })(using Frame) = Snapshot.leaf(x)
                private[form] def owner(x: FormField[A] { type Valid = B })                 = x.owner

        /** A number field validates to what its rules yield. */
        given number[A, B]: Aux[NumberField[A] { type Valid = B }, B] =
            new Member[NumberField[A] { type Valid = B }]:
                type Out = B
                private[form] def snapshot(x: NumberField[A] { type Valid = B })(using Frame) = Snapshot.leaf(x)
                private[form] def owner(x: NumberField[A] { type Valid = B })                 = x.owner

        /** An array validates to the `Chunk` of its rows' named tuples of values. */
        given array[N <: Tuple, H <: Tuple, V <: Tuple](using rows: Shape.Aux[H, V]): Aux[GroupArray[N, H], Chunk[NamedTuple[N, V]]] =
            new Member[GroupArray[N, H]]:
                type Out = Chunk[NamedTuple[N, V]]
                // The rows are read once, here, when the submit starts: the rows it validates are the rows it hands over.
                private[form] def snapshot(x: GroupArray[N, H])(using Frame) =
                    x.underlying.currentRows.map(rs => Kyo.foreach(rs)(r => rows.snapshot(r.fields.toTuple)).map(Snapshot.all(_)))
                private[form] def owner(x: GroupArray[N, H]) = x.underlying.parent

        /** A child validates to its own named tuple of values. Its scope is the one its first member was declared in,
          * and the child itself lives in the scope that scope was made in; a group always has a first member, since
          * `fields`, `array` and `child` refuse an empty one.
          */
        given child[N <: Tuple, X, T <: Tuple, V <: Tuple](using
            head: Member[X],
            shape: Shape.Aux[X *: T, V]
        ): Aux[NamedTuple[N, X *: T], NamedTuple[N, V]] =
            new Member[NamedTuple[N, X *: T]]:
                type Out = NamedTuple[N, V]
                private[form] def snapshot(x: NamedTuple[N, X *: T])(using Frame) = shape.snapshot(x.toTuple)
                private[form] def owner(x: NamedTuple[N, X *: T])                 = head.owner(x.toTuple.head).outer
    end Member

    /** Evidence that `H` is the tuple of handles of a field group, and what it validates to (`Outs`). Derived member by
      * member from [[Member]].
      */
    @implicitNotFound(
        "Cannot submit this as a field group: every member must be a declared field, number field, GroupArray or child group, as Form.fields returns them"
    )
    sealed trait Shape[H <: Tuple]:
        /** What the member validates to. */
        type Outs <: Tuple
        private[form] def snapshot(hs: H)(using Frame): Snapshot[Outs] < Sync
    end Shape

    /** The derivation, member by member. */
    object Shape:
        /** A [[Shape]] validating to `V`, as a type parameter so it stays concrete. */
        type Aux[H <: Tuple, V <: Tuple] = Shape[H] { type Outs = V }

        /** No members validate to the empty tuple. */
        given empty: Aux[EmptyTuple, EmptyTuple] =
            new Shape[EmptyTuple]:
                type Outs = EmptyTuple
                private[form] def snapshot(hs: EmptyTuple)(using Frame) = Snapshot(Chunk.empty, () => (Chunk.empty, Present(EmptyTuple)))

        /** A member, then the rest. */
        given cons[X, T <: Tuple, XO, TO <: Tuple](using m: Member.Aux[X, XO], rest: Aux[T, TO]): Aux[X *: T, XO *: TO] =
            new Shape[X *: T]:
                type Outs = XO *: TO
                private[form] def snapshot(hs: X *: T)(using Frame) =
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
    end Shape

    /** What a group submit validates, read when the submit starts: the fields it names (in group order, rows as they are
      * then) and the run that validates exactly those fields once and yields the group's value when all of them passed.
      */
    final private[form] class Snapshot[+O](
        val fields: Chunk[FormField[?]],
        val verdict: () => (Chunk[Maybe[FieldError]], Maybe[O]) < Async
    )

    private[form] object Snapshot:
        def leaf[B](f: Validated { type Valid = B })(using Frame): Snapshot[B] =
            Snapshot(Chunk(f.submitField), () => f.verdict.map(r => (Chunk(r.failure), r.value)))

        /** The snapshots of an array's rows, validated in parallel, yielding every row's value or none. */
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

end FieldGroup

/** The array member of a field group, declared by [[Form.array]]: rows that are named tuples of fields, each in a scope of
  * its own.
  *
  * The handle carries the array's operations and no field names, and a row carries field names and no operations, so
  * neither can shadow the other. The controls that address one row (`remove`, `moveUp`, `moveDown`, `index`) are the
  * [[FieldArray.Row]] that [[render]] hands over beside it:
  * {{{
  * f.lines.render((line, row) => div(uic.Input().bind(line.sku), uic.Button("Remove").onClick(row.remove)))
  * }}}
  * A group submit hands the array over as the `Chunk` of its rows' validated named tuples, in their current order. Built
  * over a [[FieldArray]], so rows take part in errors, `isDirty`, `reset`, `markPristine` and focus-first-invalid
  * exactly as a [[Form.fieldArrayOf]] row does, and [[Form.scopeOf]] of a row is the row's own scope.
  */
final class GroupArray[N <: Tuple, H <: Tuple] private[form] (
    private[form] val underlying: FieldArray[GroupArray.Entry[N, H]]
):
    import GroupArray.given

    /** The live, keyed list of rows, each laid out by `row` from its fields and its row control. Keyed on the row's stable
      * id, so add, remove and move keep the DOM of the rows that stayed.
      */
    def render(row: (NamedTuple[N, H], FieldArray.Row) => UI)(using Frame): UI = underlying.renderWith(e => row(e.fields, e.control))

    /** Append a row, declaring its fields in a fresh row scope. */
    def add(using Frame): Unit < Async = underlying.add

    /** Remove the row at index `i` (no-op if out of range). */
    def removeAt(i: Int)(using Frame): Unit < Async = underlying.removeAt(i)

    /** Move the row at `from` to index `to` (no-op if either is out of range). */
    def move(from: Int, to: Int)(using Frame): Unit < Sync = underlying.move(from, to)

    /** The live list of rows, removal-correct, for an indicator built off the rows. */
    def rows(using Frame): Signal[Chunk[NamedTuple[N, H]]] = underlying.rows.map(_.map(_.fields))

    /** The rows as they are now. */
    def currentRows(using Frame): Chunk[NamedTuple[N, H]] < Sync = underlying.currentRows.map(_.map(_.fields))

    /** A cross-row check over the current rows at submit, as [[FieldArray.satisfy]]: `false` blocks the submit and,
      * unless `reveal` is [[Reveal.Manual]], raises `code` on the scope the array was declared in.
      */
    def satisfy(code: String, reveal: Reveal)(pred: Chunk[NamedTuple[N, H]] => Boolean < Async): GroupArray[N, H] =
        discard(underlying.satisfy(code, reveal)(rs => pred(rs.map(_.fields))))
        this

    /** [[satisfy]] with the default [[Reveal.OnSubmit]]. */
    def satisfy(code: String)(pred: Chunk[NamedTuple[N, H]] => Boolean < Async): GroupArray[N, H] =
        satisfy(code, Reveal.OnSubmit)(pred)
end GroupArray

/** The row type of a [[GroupArray]] and the equality its signals need. */
object GroupArray:
    /** One row as the array holds it: the row's fields and the control that addresses it by its stable key. */
    final private[form] class Entry[N <: Tuple, H <: Tuple](val fields: NamedTuple[N, H], val control: FieldArray.Row)

    // Rows ride signals, which need equality on what they carry; a row is the same row by reference.
    private[form] given entryEq[N <: Tuple, H <: Tuple]: CanEqual[Entry[N, H], Entry[N, H]]         = CanEqual.derived
    private[form] given rowEq[N <: Tuple, H <: Tuple]: CanEqual[NamedTuple[N, H], NamedTuple[N, H]] = CanEqual.derived
end GroupArray

/** The declaration of a [[GroupArray]], made by [[Form.array]]: a pure value, declared when [[Form.fields]] declares the
  * group it sits in, into that group's scope.
  */
sealed abstract class ArraySpec[N <: Tuple, H <: Tuple] private[form] (private[form] val initial: Int):
    /** Declare one row's fields in its row scope and hand back the row. */
    private[form] def declareRow(scope: Form, ctl: FieldArray.Row)(using Frame): GroupArray.Entry[N, H] < Async

    private[form] def declareIn(scope: Form)(using Frame): GroupArray[N, H] < Async =
        import GroupArray.given
        scope.fieldArrayOf[GroupArray.Entry[N, H]](initial)((row, ctl) => declareRow(row, ctl).map(e => (fragment(), e)))
            .map(new GroupArray(_))
    end declareIn
end ArraySpec

private[form] object ArraySpec:
    def of[N <: Tuple, S <: Tuple, H <: Tuple](initial: Int, row: Form => NamedTuple[N, S])(using
        members: FieldGroup.Specs.Aux[N, S, H]
    ): ArraySpec[N, H] =
        new ArraySpec[N, H](initial):
            private[form] def declareRow(scope: Form, ctl: FieldArray.Row)(using Frame) =
                members.declare(row(scope).toTuple, scope).map(hs => new GroupArray.Entry[N, H](hs, ctl))
end ArraySpec

/** The declaration of a child group, made by [[Form.child]] with a builder: a pure value, declared (the child scope
  * first, made in the scope of the group it sits in, then its fields in the child scope) when [[Form.fields]] declares
  * that group.
  */
sealed abstract class ChildSpec[N <: Tuple, H <: Tuple] private[form] ():
    private[form] def declareIn(scope: Form)(using Frame): NamedTuple[N, H] < Async
end ChildSpec

private[form] object ChildSpec:
    def of[N <: Tuple, S <: Tuple, H <: Tuple](build: Form => NamedTuple[N, S])(using
        members: FieldGroup.Specs.Aux[N, S, H]
    ): ChildSpec[N, H] =
        new ChildSpec[N, H]:
            private[form] def declareIn(scope: Form)(using Frame) =
                scope.child().map(c => members.declare(build(c).toTuple, c).map(hs => hs: NamedTuple[N, H]))
end ChildSpec
