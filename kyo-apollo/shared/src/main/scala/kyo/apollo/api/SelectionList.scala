package kyo.apollo.api

import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.Concat

/** An entry point that takes a LIST of field selections — one lambda per field —
  * and combines them into a single selection with `~`, left to right:
  *
  * {{{
  * Fragment.embedded(Image)(
  *   _.url,
  *   _.vibrantColor,
  * )
  * Queries.album(id)(
  *   _.id,
  *   _.images(_.url, _.width),
  * ).toQuery("Album")
  * }}}
  *
  * Every lambda receives `S`, the generated selector object of `Origin`
  * (`Image.type`), so `_.` completes to exactly that type's fields, and a
  * nested object field opens its own list. A field selection is a
  * [[SelectionBuilder]] and carries no field accessors, so `_.url.width` does
  * not compile — there is one way to select several fields, and it reads as a
  * list. Factored selections pass through as values: `_ => CoverImage.fields.spread`,
  * `_ => Image(_.url, _.width)`, or `_ => Image.url ~ Image.width`.
  *
  * Overloads are resolved by argument count, so each arity has exactly one
  * candidate and the lambda parameter type is always known — which is what
  * keeps IDE completion working inside the list. Arities 2 to 22 are
  * mechanical.
  *
  * The result type is a free parameter `R`, tied to the entries by evidence
  * (`Concat[Concat[D1, D2], D3] =:= R`) instead of being spelled as that `Concat`
  * directly. The two are the same type once the entries are known, but they
  * behave differently while they are not: a call with an expected type
  * (`val x: Fields[T, Row] = T(…)`) makes overloading resolution check every
  * arity's result type against it before anything else. Spelled as nested
  * `Concat`s over unknown `Di`, that check either unfolds the arity-22 candidate
  * past the compiler's recursion limit or, in a flattened spelling, proves no
  * arity applicable at all. `Out[R]` conforms to any expected type, which fixes
  * `R`; the evidence is then resolved with every `Di` known, reduces, and holds.
  * Without an expected type the same evidence infers `R`.
  *
  * @tparam S      the selector object's type (`Image.type`)
  * @tparam Origin the phantom marker of the selected type
  * @tparam Out    what the entry point builds from the combined selection
  */
trait SelectionList[S, Origin, Out[_ <: AnyNamedTuple]]:

    /** The selector object every lambda is applied to. */
    protected def selector: S

    /** Build the entry point's result from the combined selection. */
    protected def fromFields[D <: AnyNamedTuple](sel: SelectionBuilder.Fields[Origin, D]): Out[D]

    // The evidence proves the combined selection's type IS `R`; the cast only
    // re-labels it (`Fields` is invariant, so `=:=` cannot lift through it).
    private def as[D <: AnyNamedTuple, R <: AnyNamedTuple](sel: SelectionBuilder.Fields[Origin, D])(using
        D =:= R
    ): SelectionBuilder.Fields[Origin, R] =
        sel.asInstanceOf[SelectionBuilder.Fields[Origin, R]]

    final def apply[D1 <: AnyNamedTuple](
        f1: S => SelectionBuilder.Fields[Origin, D1]
    ): Out[D1] =
        fromFields(f1(selector))

    final def apply[D1 <: AnyNamedTuple, D2 <: AnyNamedTuple, R <: AnyNamedTuple](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2]
    )(using Concat[D1, D2] =:= R): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s)))
    end apply

    final def apply[D1 <: AnyNamedTuple, D2 <: AnyNamedTuple, D3 <: AnyNamedTuple, R <: AnyNamedTuple](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3]
    )(using Concat[Concat[D1, D2], D3] =:= R): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s)))
    end apply

    final def apply[D1 <: AnyNamedTuple, D2 <: AnyNamedTuple, D3 <: AnyNamedTuple, D4 <: AnyNamedTuple, R <: AnyNamedTuple](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4]
    )(using Concat[Concat[Concat[D1, D2], D3], D4] =:= R): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5]
    )(using Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5] =:= R): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6]
    )(using Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6] =:= R): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7]
    )(using Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7] =:= R): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8]
    )(using Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8] =:= R): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9]
    )(using Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9] =:= R): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10]
    )(using Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9], D10] =:= R): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        D11 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10],
        f11: S => SelectionBuilder.Fields[Origin, D11]
    )(using
        Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9], D10], D11] =:= R
    ): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s) ~ f11(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        D11 <: AnyNamedTuple,
        D12 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10],
        f11: S => SelectionBuilder.Fields[Origin, D11],
        f12: S => SelectionBuilder.Fields[Origin, D12]
    )(using
        Concat[
            Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9], D10], D11],
            D12
        ] =:= R
    ): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s) ~ f11(s) ~ f12(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        D11 <: AnyNamedTuple,
        D12 <: AnyNamedTuple,
        D13 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10],
        f11: S => SelectionBuilder.Fields[Origin, D11],
        f12: S => SelectionBuilder.Fields[Origin, D12],
        f13: S => SelectionBuilder.Fields[Origin, D13]
    )(using
        Concat[Concat[
            Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9], D10], D11],
            D12
        ], D13] =:= R
    ): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s) ~ f11(s) ~ f12(s) ~ f13(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        D11 <: AnyNamedTuple,
        D12 <: AnyNamedTuple,
        D13 <: AnyNamedTuple,
        D14 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10],
        f11: S => SelectionBuilder.Fields[Origin, D11],
        f12: S => SelectionBuilder.Fields[Origin, D12],
        f13: S => SelectionBuilder.Fields[Origin, D13],
        f14: S => SelectionBuilder.Fields[Origin, D14]
    )(using
        Concat[Concat[Concat[
            Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9], D10], D11],
            D12
        ], D13], D14] =:= R
    ): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s) ~ f11(s) ~ f12(s) ~ f13(s) ~ f14(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        D11 <: AnyNamedTuple,
        D12 <: AnyNamedTuple,
        D13 <: AnyNamedTuple,
        D14 <: AnyNamedTuple,
        D15 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10],
        f11: S => SelectionBuilder.Fields[Origin, D11],
        f12: S => SelectionBuilder.Fields[Origin, D12],
        f13: S => SelectionBuilder.Fields[Origin, D13],
        f14: S => SelectionBuilder.Fields[Origin, D14],
        f15: S => SelectionBuilder.Fields[Origin, D15]
    )(using
        Concat[
            Concat[Concat[Concat[Concat[
                Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9], D10],
                D11
            ], D12], D13], D14],
            D15
        ] =:= R
    ): Out[R] =
        val s = selector
        fromFields(
            as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s) ~ f11(s) ~ f12(s) ~ f13(s) ~ f14(s) ~ f15(s))
        )
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        D11 <: AnyNamedTuple,
        D12 <: AnyNamedTuple,
        D13 <: AnyNamedTuple,
        D14 <: AnyNamedTuple,
        D15 <: AnyNamedTuple,
        D16 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10],
        f11: S => SelectionBuilder.Fields[Origin, D11],
        f12: S => SelectionBuilder.Fields[Origin, D12],
        f13: S => SelectionBuilder.Fields[Origin, D13],
        f14: S => SelectionBuilder.Fields[Origin, D14],
        f15: S => SelectionBuilder.Fields[Origin, D15],
        f16: S => SelectionBuilder.Fields[Origin, D16]
    )(using
        Concat[
            Concat[
                Concat[Concat[Concat[Concat[
                    Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9], D10],
                    D11
                ], D12], D13], D14],
                D15
            ],
            D16
        ] =:= R
    ): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s) ~ f11(s) ~ f12(s) ~ f13(s) ~
            f14(
                s
            ) ~ f15(s) ~ f16(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        D11 <: AnyNamedTuple,
        D12 <: AnyNamedTuple,
        D13 <: AnyNamedTuple,
        D14 <: AnyNamedTuple,
        D15 <: AnyNamedTuple,
        D16 <: AnyNamedTuple,
        D17 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10],
        f11: S => SelectionBuilder.Fields[Origin, D11],
        f12: S => SelectionBuilder.Fields[Origin, D12],
        f13: S => SelectionBuilder.Fields[Origin, D13],
        f14: S => SelectionBuilder.Fields[Origin, D14],
        f15: S => SelectionBuilder.Fields[Origin, D15],
        f16: S => SelectionBuilder.Fields[Origin, D16],
        f17: S => SelectionBuilder.Fields[Origin, D17]
    )(using
        Concat[
            Concat[
                Concat[
                    Concat[Concat[Concat[Concat[
                        Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9], D10],
                        D11
                    ], D12], D13], D14],
                    D15
                ],
                D16
            ],
            D17
        ] =:= R
    ): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s) ~ f11(s) ~ f12(s) ~ f13(s) ~
            f14(
                s
            ) ~ f15(s) ~ f16(s) ~ f17(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        D11 <: AnyNamedTuple,
        D12 <: AnyNamedTuple,
        D13 <: AnyNamedTuple,
        D14 <: AnyNamedTuple,
        D15 <: AnyNamedTuple,
        D16 <: AnyNamedTuple,
        D17 <: AnyNamedTuple,
        D18 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10],
        f11: S => SelectionBuilder.Fields[Origin, D11],
        f12: S => SelectionBuilder.Fields[Origin, D12],
        f13: S => SelectionBuilder.Fields[Origin, D13],
        f14: S => SelectionBuilder.Fields[Origin, D14],
        f15: S => SelectionBuilder.Fields[Origin, D15],
        f16: S => SelectionBuilder.Fields[Origin, D16],
        f17: S => SelectionBuilder.Fields[Origin, D17],
        f18: S => SelectionBuilder.Fields[Origin, D18]
    )(using
        Concat[
            Concat[
                Concat[
                    Concat[
                        Concat[Concat[Concat[Concat[
                            Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9], D10],
                            D11
                        ], D12], D13], D14],
                        D15
                    ],
                    D16
                ],
                D17
            ],
            D18
        ] =:= R
    ): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s) ~ f11(s) ~ f12(s) ~ f13(s) ~
            f14(
                s
            ) ~ f15(s) ~ f16(s) ~ f17(s) ~ f18(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        D11 <: AnyNamedTuple,
        D12 <: AnyNamedTuple,
        D13 <: AnyNamedTuple,
        D14 <: AnyNamedTuple,
        D15 <: AnyNamedTuple,
        D16 <: AnyNamedTuple,
        D17 <: AnyNamedTuple,
        D18 <: AnyNamedTuple,
        D19 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10],
        f11: S => SelectionBuilder.Fields[Origin, D11],
        f12: S => SelectionBuilder.Fields[Origin, D12],
        f13: S => SelectionBuilder.Fields[Origin, D13],
        f14: S => SelectionBuilder.Fields[Origin, D14],
        f15: S => SelectionBuilder.Fields[Origin, D15],
        f16: S => SelectionBuilder.Fields[Origin, D16],
        f17: S => SelectionBuilder.Fields[Origin, D17],
        f18: S => SelectionBuilder.Fields[Origin, D18],
        f19: S => SelectionBuilder.Fields[Origin, D19]
    )(using
        Concat[
            Concat[
                Concat[
                    Concat[
                        Concat[
                            Concat[Concat[Concat[Concat[Concat[
                                Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9],
                                D10
                            ], D11], D12], D13], D14],
                            D15
                        ],
                        D16
                    ],
                    D17
                ],
                D18
            ],
            D19
        ] =:= R
    ): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s) ~ f11(s) ~ f12(s) ~ f13(s) ~
            f14(
                s
            ) ~ f15(s) ~ f16(s) ~ f17(s) ~ f18(s) ~ f19(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        D11 <: AnyNamedTuple,
        D12 <: AnyNamedTuple,
        D13 <: AnyNamedTuple,
        D14 <: AnyNamedTuple,
        D15 <: AnyNamedTuple,
        D16 <: AnyNamedTuple,
        D17 <: AnyNamedTuple,
        D18 <: AnyNamedTuple,
        D19 <: AnyNamedTuple,
        D20 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10],
        f11: S => SelectionBuilder.Fields[Origin, D11],
        f12: S => SelectionBuilder.Fields[Origin, D12],
        f13: S => SelectionBuilder.Fields[Origin, D13],
        f14: S => SelectionBuilder.Fields[Origin, D14],
        f15: S => SelectionBuilder.Fields[Origin, D15],
        f16: S => SelectionBuilder.Fields[Origin, D16],
        f17: S => SelectionBuilder.Fields[Origin, D17],
        f18: S => SelectionBuilder.Fields[Origin, D18],
        f19: S => SelectionBuilder.Fields[Origin, D19],
        f20: S => SelectionBuilder.Fields[Origin, D20]
    )(using
        Concat[
            Concat[
                Concat[
                    Concat[
                        Concat[
                            Concat[
                                Concat[Concat[Concat[Concat[Concat[
                                    Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9],
                                    D10
                                ], D11], D12], D13], D14],
                                D15
                            ],
                            D16
                        ],
                        D17
                    ],
                    D18
                ],
                D19
            ],
            D20
        ] =:= R
    ): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s) ~ f11(s) ~ f12(s) ~ f13(s) ~
            f14(
                s
            ) ~ f15(s) ~ f16(s) ~ f17(s) ~ f18(s) ~ f19(s) ~ f20(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        D11 <: AnyNamedTuple,
        D12 <: AnyNamedTuple,
        D13 <: AnyNamedTuple,
        D14 <: AnyNamedTuple,
        D15 <: AnyNamedTuple,
        D16 <: AnyNamedTuple,
        D17 <: AnyNamedTuple,
        D18 <: AnyNamedTuple,
        D19 <: AnyNamedTuple,
        D20 <: AnyNamedTuple,
        D21 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10],
        f11: S => SelectionBuilder.Fields[Origin, D11],
        f12: S => SelectionBuilder.Fields[Origin, D12],
        f13: S => SelectionBuilder.Fields[Origin, D13],
        f14: S => SelectionBuilder.Fields[Origin, D14],
        f15: S => SelectionBuilder.Fields[Origin, D15],
        f16: S => SelectionBuilder.Fields[Origin, D16],
        f17: S => SelectionBuilder.Fields[Origin, D17],
        f18: S => SelectionBuilder.Fields[Origin, D18],
        f19: S => SelectionBuilder.Fields[Origin, D19],
        f20: S => SelectionBuilder.Fields[Origin, D20],
        f21: S => SelectionBuilder.Fields[Origin, D21]
    )(using
        Concat[
            Concat[
                Concat[
                    Concat[
                        Concat[
                            Concat[
                                Concat[
                                    Concat[Concat[Concat[Concat[Concat[
                                        Concat[Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8], D9],
                                        D10
                                    ], D11], D12], D13], D14],
                                    D15
                                ],
                                D16
                            ],
                            D17
                        ],
                        D18
                    ],
                    D19
                ],
                D20
            ],
            D21
        ] =:= R
    ): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s) ~ f11(s) ~ f12(s) ~ f13(s) ~
            f14(
                s
            ) ~ f15(s) ~ f16(s) ~ f17(s) ~ f18(s) ~ f19(s) ~ f20(s) ~ f21(s)))
    end apply

    final def apply[
        D1 <: AnyNamedTuple,
        D2 <: AnyNamedTuple,
        D3 <: AnyNamedTuple,
        D4 <: AnyNamedTuple,
        D5 <: AnyNamedTuple,
        D6 <: AnyNamedTuple,
        D7 <: AnyNamedTuple,
        D8 <: AnyNamedTuple,
        D9 <: AnyNamedTuple,
        D10 <: AnyNamedTuple,
        D11 <: AnyNamedTuple,
        D12 <: AnyNamedTuple,
        D13 <: AnyNamedTuple,
        D14 <: AnyNamedTuple,
        D15 <: AnyNamedTuple,
        D16 <: AnyNamedTuple,
        D17 <: AnyNamedTuple,
        D18 <: AnyNamedTuple,
        D19 <: AnyNamedTuple,
        D20 <: AnyNamedTuple,
        D21 <: AnyNamedTuple,
        D22 <: AnyNamedTuple,
        R <: AnyNamedTuple
    ](
        f1: S => SelectionBuilder.Fields[Origin, D1],
        f2: S => SelectionBuilder.Fields[Origin, D2],
        f3: S => SelectionBuilder.Fields[Origin, D3],
        f4: S => SelectionBuilder.Fields[Origin, D4],
        f5: S => SelectionBuilder.Fields[Origin, D5],
        f6: S => SelectionBuilder.Fields[Origin, D6],
        f7: S => SelectionBuilder.Fields[Origin, D7],
        f8: S => SelectionBuilder.Fields[Origin, D8],
        f9: S => SelectionBuilder.Fields[Origin, D9],
        f10: S => SelectionBuilder.Fields[Origin, D10],
        f11: S => SelectionBuilder.Fields[Origin, D11],
        f12: S => SelectionBuilder.Fields[Origin, D12],
        f13: S => SelectionBuilder.Fields[Origin, D13],
        f14: S => SelectionBuilder.Fields[Origin, D14],
        f15: S => SelectionBuilder.Fields[Origin, D15],
        f16: S => SelectionBuilder.Fields[Origin, D16],
        f17: S => SelectionBuilder.Fields[Origin, D17],
        f18: S => SelectionBuilder.Fields[Origin, D18],
        f19: S => SelectionBuilder.Fields[Origin, D19],
        f20: S => SelectionBuilder.Fields[Origin, D20],
        f21: S => SelectionBuilder.Fields[Origin, D21],
        f22: S => SelectionBuilder.Fields[Origin, D22]
    )(using
        Concat[
            Concat[
                Concat[
                    Concat[
                        Concat[
                            Concat[
                                Concat[
                                    Concat[
                                        Concat[Concat[Concat[Concat[Concat[Concat[
                                            Concat[Concat[Concat[Concat[Concat[Concat[Concat[D1, D2], D3], D4], D5], D6], D7], D8],
                                            D9
                                        ], D10], D11], D12], D13], D14],
                                        D15
                                    ],
                                    D16
                                ],
                                D17
                            ],
                            D18
                        ],
                        D19
                    ],
                    D20
                ],
                D21
            ],
            D22
        ] =:= R
    ): Out[R] =
        val s = selector
        fromFields(as(f1(s) ~ f2(s) ~ f3(s) ~ f4(s) ~ f5(s) ~ f6(s) ~ f7(s) ~ f8(s) ~ f9(s) ~ f10(s) ~ f11(s) ~ f12(s) ~ f13(s) ~
            f14(
                s
            ) ~ f15(s) ~ f16(s) ~ f17(s) ~ f18(s) ~ f19(s) ~ f20(s) ~ f21(s) ~ f22(s)))
    end apply
end SelectionList
