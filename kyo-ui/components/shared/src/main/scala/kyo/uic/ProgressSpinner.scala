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
  * Prime sizes the spinner via an inline style on the root; here `size(Size)`
  * maps to the `p-uic-progressspinner-sm`/`-lg` remainder classes (Normal keeps
  * Prime's 100px default). Always animating: Prime's spinner has no
  * active/delay/text surface.
  */
final class ProgressSpinner private[uic] (private[uic] val s: ProgressSpinner.State) extends Node, HasAccessibleName, Stated:
    import s.*

    type Self = ProgressSpinner

    /** Size preset: `Small` (2rem) / `Normal` (Prime's 100px default) / `Large` (8rem). */
    def size(v: Size): ProgressSpinner = new ProgressSpinner(s.copy(sizeV = v))

    private[uic] def accessibility: Accessibility              = accessV
    private[uic] def withAccessibility(v: Accessibility): Self = new ProgressSpinner(s.copy(accessV = v))
    private[uic] def render(using Frame): UI                   =
        var el = div.cssClass("p-progressspinner").role("progressbar")
        sizeV match
            case Size.Small  => el = el.cssClass("p-uic-progressspinner-sm")
            case Size.Large  => el = el.cssClass("p-uic-progressspinner-lg")
            case Size.Normal => ()
        end match
        accessV.nameV match
            case Present(TextValue.Const(n)) => el = el.aria("label", n)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        el(
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
    final private[uic] case class State(
        sizeV: Size = Size.Normal,
        accessV: Accessibility = Accessibility.none
    )

    def apply(): ProgressSpinner = new ProgressSpinner(State())
end ProgressSpinner
