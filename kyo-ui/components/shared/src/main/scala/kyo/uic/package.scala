package kyo

import kyo.UI.Ast.HtmlChildVal

/** kyo-ui-components — native PrimeOne component library for kyo-ui.
  *
  * Downstream usage (no name prefix — the namespace carries the origin):
  * {{{
  *   import kyo.*
  *   import kyo.UI.*
  *   import kyo.uic
  *   import scala.language.implicitConversions
  *
  *   uic.Button("Save").design(uic.ButtonDesign.Emphasized).icon("save").onClick(save)
  *   uic.Input().placeholder("Email").value(emailRef)   // two-way signal bind
  * }}}
  */
package object uic:

    /** Wrap a fully-rendered `UI` as a kyo `HtmlChildVal`, so a `List[UI]` produced
      * inside a component's `render` can be splatted into a kyo container's
      * `apply(cs: HtmlChildVal*)`. Uses the `private[kyo]` constructor — available
      * because this lives in package `kyo`. Splatting is needed because an implicit
      * `UI => HtmlChildVal` conversion is NOT applied element-wise to a `xs*` splat.
      */
    private[uic] def toChild(u: UI): HtmlChildVal = new HtmlChildVal(u)

    /** Package-internal parsers from CSS value strings into kyo's typed `Style`
      * vocabulary — the Prime APIs mirrored by Skeleton/ScrollPanel/MeterGroup
      * take plain CSS strings (`width="10rem"`, `color="var(--p-cyan-500)"`), and
      * kyo has no raw style-attribute escape hatch.
      */
    private[uic] object CssValue:

        /** Parses a CSS length: `px`/`%`/`em` and bare numbers (as px) map onto the
          * typed `Length` cases; `auto` maps to `Length.Auto`; anything else (`rem`,
          * `vh`, expressions) passes through as `Length.Calc`, which renders as
          * `calc(<raw>)` — CSS-equivalent to the raw value.
          */
        def length(v: String): Length =
            val t = v.trim
            def num(suffix: String): Option[Double] =
                if t.endsWith(suffix) then t.dropRight(suffix.length).trim.toDoubleOption else None
            if t.equalsIgnoreCase("auto") then Length.Auto
            else
                num("px").map(d => Length.Px(d): Length)
                    .orElse(num("%").map(d => Length.Pct(d)))
                    .orElse(if t.endsWith("rem") then None else num("em").map(d => Length.Em(d)))
                    .orElse(t.toDoubleOption.map(d => Length.Px(d)))
                    .getOrElse(Length.Calc(t))
            end if
        end length

        /** Parses a CSS color into kyo's `Style.Color`: `var(--token)` references
          * and 3/4/6/8-digit hex values are supported; anything else is `Absent`
          * (callers fall back to their own default).
          */
        def color(v: String): Maybe[Style.Color] =
            val t = v.trim
            if t.startsWith("var(") && t.endsWith(")") then
                val inner = t.drop(4).dropRight(1).trim
                val name  = if inner.startsWith("--") then inner.drop(2) else inner
                if name.nonEmpty then Present(Style.Color.variable(name)) else Absent
            else Style.Color.hex(t)
            end if
        end color
    end CssValue
end uic
