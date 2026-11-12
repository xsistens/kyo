package kyo.uic

import kyo.*
import kyo.UI.*

/** Skeleton shapes (`.p-skeleton-circle` vs the default rectangle). */
enum SkeletonShape derives CanEqual:
    case Rectangle, Circle

/** Skeleton: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Skeleton anatomy: `div.p-skeleton.p-component[.p-skeleton-circle]
  * [.p-skeleton-animation-none]`, an empty box whose `::after` runs the wave
  * shimmer), so the extracted `@primeuix` skeleton CSS applies verbatim.
  *
  * Dimensions are inline styles, exactly like Prime: `width` (default `100%`)
  * and `height` (default `1rem`), or the `size` square convenience that wins
  * over both (Prime semantics). `borderRadius` overrides the token radius;
  * `animation(false)` maps Prime's `animation="none"`. Each takes a constant or
  * a signal, and a signal patches its class or the inline style in place. The
  * `::after` shimmer needs `position: relative` on the root; PrimeVue sets it as
  * an inline style, here it lives in [[Theme.primeExtraCss]]. The box is
  * `aria-hidden` (Prime semantics: a skeleton is a purely visual placeholder).
  * It keeps no state, so it has no instance: a host region that runs again has
  * nothing of it to lose.
  */
final class Skeleton private[uic] (private[uic] val p: Skeleton.Props) extends Component(Setters[Skeleton.Props, Skeleton]), Node:

    type Self = Skeleton

    /** Draws the box. Nothing of it decides the structure: the shape and the animation reach the root through its class channels,
      * and the dimensions and the radius through its inline-style channel, as one style the block computes from them, so a
      * constant is a class or a declaration, a signal patches it in place, and the box draws no region.
      */
    private[uic] def render(using Frame): UI =
        // `size` wins over `width` and `height` (Prime), so it is the prop both dimensions read where it is set.
        val width  = p.size.getOrElse(p.width)
        val height = p.size.getOrElse(p.height)
        // Only a `px` or `%` radius applies: kyo-ui's `Style.rounded` takes those two alone, so another unit keeps the token radius.
        val box: Prop[Style] = UI.reactive {
            val dimensions = Style.width(Length.parse(width.read)).height(Length.parse(height.read))
            p.borderRadius.read.map(Length.parse) match
                case Present(l: (Length.Px | Length.Pct)) => dimensions.rounded(l)
                case _                                    => dimensions
        }
        div.cssClass("p-skeleton", "p-component")
            .cssClass.matching(p.shape) {
                case SkeletonShape.Rectangle => Absent
                case SkeletonShape.Circle    => "p-skeleton-circle"
            }
            .cssClass.matching(p.animation) {
                case true  => Absent
                case false => "p-skeleton-animation-none"
            }
            .aria(Aria.hidden -> true)
            .inlineStyle(box)()
    end render
end Skeleton

object Skeleton:
    /** What a skeleton holds. Each field is a setter of its name (`uic.Skeleton().width("40px").height("1rem")`), which takes a
      * constant or a signal where the field is a `Prop` and the value itself (a constant or a signal of the string) where it is
      * an `Opt`.
      */
    final private[uic] case class Props(
        /** `Circle` renders `.p-skeleton-circle` (50% radius); default `Rectangle`. A signal swaps the class in place. */
        shape: Prop[SkeletonShape] = SkeletonShape.Rectangle,
        /** CSS width (inline style; default `100%`). A signal patches the declaration in place. */
        width: Prop[String] = "100%",
        /** CSS height (inline style; default `1rem`). A signal patches the declaration in place. */
        height: Prop[String] = "1rem",
        /** Square convenience: one CSS length for both dimensions; wins over `width` and `height` (Prime semantics). */
        size: Opt[Prop[String]] = Opt.unset,
        /** CSS border radius (inline style), overriding the token radius. Only a `px` or `%` length applies; another unit (`rem`,
          * `em`, an expression) keeps the token radius, since kyo-ui's `Style.rounded` takes those two alone.
          */
        borderRadius: Opt[Prop[String]] = Opt.unset,
        /** The wave shimmer (default true); `false` renders `.p-skeleton-animation-none`. A signal swaps the class in place. */
        animation: Prop[Boolean] = true
    ) derives CanEqual

    def apply(): Skeleton = new Skeleton(Props())
end Skeleton
