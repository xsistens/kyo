package kyo.uic

import kyo.*
import kyo.UI.*

/** ProgressSpinner — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ProgressSpinner anatomy: `div.p-progressspinner[role=progressbar]` >
  * `svg.p-progressspinner-spin[viewBox 25 25 50 50]` >
  * `circle.p-progressspinner-circle[cx=50 cy=50 r=20 fill=none]`), so the
  * extracted `@primeuix` progressspinner CSS — rotation, dash and four-color
  * stroke animation — applies verbatim.
  *
  * Prime sizes the spinner via an inline style on the root; here `size(Size)`
  * maps to the `p-uic-progressspinner-sm`/`-lg` remainder classes (Normal keeps
  * Prime's 100px default). Always animating — Prime's spinner has no
  * active/delay/text surface.
  */
final case class ProgressSpinner private (
    sizeV: Size = Size.Normal,
    accessibleNameV: Maybe[TextValue] = Absent
) extends Node:
    type Self = ProgressSpinner

    /** Size preset: `Small` (2rem) / `Normal` (Prime's 100px default) / `Large` (8rem). */
    def size(v: Size): ProgressSpinner = copy(sizeV = v)

    /** Accessible name, emitted as `aria-label`. */
    def accessibleName(v: String): ProgressSpinner = copy(accessibleNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): ProgressSpinner = copy(accessibleNameV = Present(TextValue.Dyn(sig)))

    private[uic] def render(using Frame): UI =
        var el = div.cssClass("p-progressspinner").role("progressbar")
        sizeV match
            case Size.Small               => el = el.cssClass("p-uic-progressspinner-sm")
            case Size.Large | Size.XLarge => el = el.cssClass("p-uic-progressspinner-lg")
            case Size.Normal              => ()
        end match
        accessibleNameV match
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
    def apply(): ProgressSpinner = new ProgressSpinner()
