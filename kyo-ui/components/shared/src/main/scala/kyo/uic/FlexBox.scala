package kyo.uic

import kyo.*
import kyo.UI.*

/** Flex flow direction of a [[FlexBox]] (the four CSS flex directions). Maps
  * onto kyo's typed `Style` flex-direction (`row` / `rowReverse` / `column` /
  * `columnReverse`).
  */
enum FlexDirection derives CanEqual:
    case Row, RowReverse, Column, ColumnReverse

/** Flex layout container — native kyo-ui (`div.p-uic-flex`; Prime ships no flex
  * primitive, the class lives in the kyo remainder CSS).
  * Direction/alignment/wrap map straight onto the kyo `Style` flex ADT (kyo-ui
  * elements lay out as flex by default, so no display toggle is needed).
  * Alignment options reuse kyo's own `Alignment`/`Justification` enums — they ARE
  * the ecosystem vocabulary, so autocomplete via `_.spaceBetween` works as usual.
  *
  * `wrap` is three-way: `wrap(true)` maps onto the typed `flexWrap(_.wrap)`;
  * `wrapReverse(true)` uses the `p-uic-flex--wrap-reverse` class because kyo's
  * typed `FlexWrap` ADT has no reverse variant. `fitContainer` stretches the box
  * to `100%` of both parent dimensions via `p-uic-flex--fit`.
  */
final case class FlexBox private (
    directionV: Maybe[FlexDirection] = Absent,
    gapV: Maybe[Length.Px | Length.Em] = Absent,
    gapRaw: Maybe[String] = Absent,
    justifyV: Maybe[Style.Justification] = Absent,
    alignV: Maybe[Style.Alignment] = Absent,
    wrapFlag: Boolean = false,
    wrapReverseFlag: Boolean = false,
    fitContainerV: Boolean = false,
    kids: List[UI] = Nil
) extends Node:
    type Self = FlexBox

    /** Typed flow direction (default row). */
    def direction(v: FlexDirection): FlexBox = copy(directionV = Present(v))

    /** Column direction (default is row) — alias for `direction(Column/Row)`. */
    def vertical(v: Boolean): FlexBox =
        copy(directionV = Present(if v then FlexDirection.Column else FlexDirection.Row))

    def gap(px: Int): FlexBox = copy(gapV = Present(Length.Px(px)), gapRaw = Absent)

    /** Gap from a CSS length string (`"8px"`, `"0.5em"`, `"1rem"`, bare number as
      * px). An unparseable value is kept as a `data-uic-gap` hook only.
      */
    def gap(v: String): FlexBox =
        FlexBox.parseGap(v) match
            case Present(len) => copy(gapV = Present(len), gapRaw = Absent)
            case Absent       => copy(gapV = Absent, gapRaw = Present(v))

    def justify(f: Style.Justification.type => Style.Justification): FlexBox =
        copy(justifyV = Present(f(Style.Justification)))

    def align(f: Style.Alignment.type => Style.Alignment): FlexBox =
        copy(alignV = Present(f(Style.Alignment)))

    def wrap(v: Boolean): FlexBox = copy(wrapFlag = v)

    /** Wraps lines in reverse stacking order (`flex-wrap: wrap-reverse`); wins over `wrap`. */
    def wrapReverse(v: Boolean): FlexBox = copy(wrapReverseFlag = v)

    /** Stretches the box to 100% width and height of its container. */
    def fitContainer(v: Boolean): FlexBox = copy(fitContainerV = v)

    /** Adds children. */
    def apply(cs: UI*): FlexBox = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        var s = directionV.getOrElse(FlexDirection.Row) match
            case FlexDirection.Row           => Style.row
            case FlexDirection.RowReverse    => Style.rowReverse
            case FlexDirection.Column        => Style.column
            case FlexDirection.ColumnReverse => Style.columnReverse
        gapV match
            case Present(len) => s = s.gap(len)
            case Absent       => ()
        justifyV match
            case Present(j) => s = s.justify(j)
            case Absent     => ()
        alignV match
            case Present(a) => s = s.align(a)
            case Absent     => ()
        // wrap-reverse has no typed FlexWrap variant, so it goes through the CSS
        // class; an inline flexWrap would override the class, so it is skipped then.
        if wrapFlag && !wrapReverseFlag then s = s.flexWrap(_.wrap)
        var box = div.cssClass("p-uic-flex")
        if wrapReverseFlag then box = box.cssClass("p-uic-flex--wrap-reverse")
        if fitContainerV then box = box.cssClass("p-uic-flex--fit")
        gapRaw match
            case Present(v) => box = box.data("uic-gap", v)
            case Absent     => ()
        box.style(s)(kids.map(toChild)*)
    end render
end FlexBox

object FlexBox:
    def apply(): FlexBox = new FlexBox()

    /** Parses a CSS gap string into the `Px | Em` union kyo's `Style.gap` accepts
      * (`rem` converts at the standard 16px root).
      */
    private def parseGap(v: String): Maybe[Length.Px | Length.Em] =
        val t = v.trim
        def num(suffix: String): Option[Double] =
            if t.endsWith(suffix) then t.dropRight(suffix.length).trim.toDoubleOption else None
        num("px").map(d => Length.Px(d): Length.Px | Length.Em)
            .orElse(num("rem").map(d => Length.Px(d * 16)))
            .orElse(num("em").map(d => Length.Em(d)))
            .orElse(t.toDoubleOption.map(d => Length.Px(d)))
            .match
                case Some(l) => Present(l)
                case None    => Absent
        end match
    end parseGap
end FlexBox
