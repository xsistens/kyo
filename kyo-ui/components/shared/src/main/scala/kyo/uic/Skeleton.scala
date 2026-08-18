package kyo.uic

import kyo.*
import kyo.UI.*

/** Skeleton shapes (`.p-skeleton-circle` vs the default rectangle). */
enum SkeletonShape derives CanEqual:
    case Rectangle, Circle

/** Skeleton — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Skeleton anatomy: `div.p-skeleton.p-component[.p-skeleton-circle]
  * [.p-skeleton-animation-none]`, an empty box whose `::after` runs the wave
  * shimmer), so the extracted `@primeuix` skeleton CSS applies verbatim.
  *
  * Dimensions are inline styles, exactly like Prime: `width` (default `100%`)
  * and `height` (default `1rem`), or the `size` square convenience that wins
  * over both (Prime semantics). `borderRadius` overrides the token radius;
  * `animation(false)` maps Prime's `animation="none"`. The `::after` shimmer
  * needs `position: relative` on the root — PrimeVue sets it as an inline
  * style, here it lives in [[Theme.primeExtraCss]]. The box is `aria-hidden`
  * (Prime semantics — a skeleton is a purely visual placeholder).
  */
final case class Skeleton private (
    shapeV: SkeletonShape = SkeletonShape.Rectangle,
    widthV: String = "100%",
    heightV: String = "1rem",
    sizeV: Maybe[String] = Absent,
    borderRadiusV: Maybe[String] = Absent,
    animationFlag: Boolean = true
) extends Node:
    type Self = Skeleton

    /** `Circle` renders `.p-skeleton-circle` (50% radius); default `Rectangle`. */
    def shape(v: SkeletonShape): Skeleton = copy(shapeV = v)

    /** CSS width (inline style; default `100%`). */
    def width(v: String): Skeleton = copy(widthV = v)

    /** CSS height (inline style; default `1rem`). */
    def height(v: String): Skeleton = copy(heightV = v)

    /** Square convenience: one CSS length for both dimensions — wins over
      * `width`/`height` (Prime semantics).
      */
    def size(v: String): Skeleton = copy(sizeV = Present(v))

    /** CSS border radius (inline style), overriding the token radius. */
    def borderRadius(v: String): Skeleton = copy(borderRadiusV = Present(v))

    /** The wave shimmer (default true); `false` renders `.p-skeleton-animation-none`. */
    def animation(v: Boolean): Skeleton = copy(animationFlag = v)

    private[uic] def render(using Frame): UI =
        var el = div.cssClass("p-skeleton").cssClass("p-component").aria("hidden", "true")
        if shapeV == SkeletonShape.Circle then el = el.cssClass("p-skeleton-circle")
        if !animationFlag then el = el.cssClass("p-skeleton-animation-none")
        val (w, h) = sizeV match
            case Present(s) => (s, s)
            case Absent     => (widthV, heightV)
        var st = Style.width(CssValue.length(w)).height(CssValue.length(h))
        borderRadiusV.foreach { r =>
            CssValue.length(r) match
                case l: (Length.Px | Length.Pct) => st = st.rounded(l)
                case _                           => () // non-px/% radii keep the token radius (typed Style limit)
        }
        el.style(st)()
    end render
end Skeleton

object Skeleton:
    def apply(): Skeleton = new Skeleton()
