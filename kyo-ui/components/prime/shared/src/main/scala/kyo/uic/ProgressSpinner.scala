package kyo.uic

import kyo.*
import kyo.UI.*

/** ProgressSpinner: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ProgressSpinner anatomy: `div.p-progressspinner[role=progressbar]` >
  * `svg.p-progressspinner-spin[viewBox 25 25 50 50]` >
  * `circle.p-progressspinner-circle[cx=50 cy=50 r=20 fill=none]`), so the
  * extracted `@primeuix` progressspinner CSS (rotation, dash and four-color
  * stroke animation) applies verbatim.
  *
  * Prime sizes the spinner via an inline style on the root; here `size` maps to
  * the `p-uic-progressspinner-sm`/`-lg` remainder classes (Normal keeps Prime's
  * 100px default), and a signal swaps them in place. Always animating: Prime's
  * spinner has no active/delay/text surface. It keeps no state, so it has no
  * instance: a host region that runs again has nothing of it to lose.
  */
final class ProgressSpinner private[uic] (private[uic] val p: ProgressSpinner.Props)
    extends Component(Setters[ProgressSpinner.Props, ProgressSpinner]), Node, HasAccessibleName:

    type Self = ProgressSpinner

    private[uic] def accessibility: Accessibility              = p.access
    private[uic] def withAccessibility(v: Accessibility): Self = new ProgressSpinner(p.copy(access = v))

    /** Draws the spinner. Nothing of it decides the structure: the size reaches the root through its class channel and the accessible
      * name through its `aria-label` channel, so a constant is a class or an attribute, a signal patches it in place, and the
      * spinner draws no region.
      */
    private[uic] def render(using Frame): UI =
        div.cssClass("p-progressspinner").role("progressbar")
            .cssClass.matching(p.size) {
                case Size.Small  => "p-uic-progressspinner-sm"
                case Size.Normal => Absent
                case Size.Large  => "p-uic-progressspinner-lg"
            }
            .aria(Aria.label -> p.access.name)(
                Svg.svg
                    .cssClass("p-progressspinner-spin")
                    .viewBox(Svg.ViewBox(25, 25, 50, 50))(
                        Svg.circle
                            .cssClass("p-progressspinner-circle")
                            .cx(50)
                            .cy(50)
                            .r(20)
                            .fill(Svg.Paint.None)
                            .strokeWidth(2)
                            .strokeMiterlimit(10)
                    )
            )
    end render
end ProgressSpinner

object ProgressSpinner:
    /** What a spinner holds. Each field is a setter of its name (`uic.ProgressSpinner().size(Size.Small)`), which takes a constant
      * or a signal; `access` is written by the shared trait (`accessibleName`).
      */
    final private[uic] case class Props(
        /** Size preset: `Small` (2rem) / `Normal` (Prime's 100px default) / `Large` (8rem). A signal swaps the class in place. */
        size: Prop[Size] = Size.Normal,
        @noSetter access: Accessibility = Accessibility.none
    ) derives CanEqual

    def apply(): ProgressSpinner = new ProgressSpinner(Props())
end ProgressSpinner
