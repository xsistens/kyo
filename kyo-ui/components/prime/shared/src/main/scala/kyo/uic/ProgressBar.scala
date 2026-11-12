package kyo.uic

import kyo.*
import kyo.UI.*

/** ProgressBar mode: `Determinate` (default, value-driven fill) or
  * `Indeterminate` (the endless slide animation).
  */
enum ProgressBarMode derives CanEqual:
    case Determinate, Indeterminate

/** ProgressBar: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ProgressBar anatomy: `div.p-progressbar[role=progressbar]` with the
  * `.p-progressbar-determinate`/`-indeterminate` mode class > `.p-progressbar-value`
  * (width in %) > optional `.p-progressbar-label`), so the extracted `@primeuix`
  * progressbar CSS applies verbatim.
  *
  * The label shows `NN%` by default (Prime's `showValue`); `valueTemplate` formats
  * it, `showValue(false)` hides it. Like Prime, a zero value renders no label. Every
  * value takes a constant or a signal, and a signal patches its place. It keeps no
  * state, so it has no instance: a host region that runs again has nothing of it to
  * lose.
  */
final class ProgressBar private[uic] (private[uic] val p: ProgressBar.Props)
    extends Component(Setters[ProgressBar.Props, ProgressBar]), Node, HasAccessibleName, HasElementId:

    type Self = ProgressBar

    private[uic] def accessibility: Accessibility              = p.access
    private[uic] def withAccessibility(v: Accessibility): Self = new ProgressBar(p.copy(access = v))

    /** Draws the bar. Nothing of it decides the structure but whether the label shows: the mode reaches the root through its class
      * channel, the percent shown (the value clamped to 0 to 100, none while indeterminate) through `aria-valuenow` and the fill's
      * inline-style channel, the accessible name through `aria-label`, and the label's text through its text channel; an `id` lands
      * on the root. The label itself is drawn in its own region below the fill, over whether it shows (the value is not zero,
      * `showValue`, determinate), so a value moving within 1 to 100 patches in place, and constants draw no region.
      */
    private[uic] def render(using Frame): UI =
        val shown: Prop[Maybe[Int]] = UI.reactive {
            p.mode.read match
                case ProgressBarMode.Determinate   => Maybe(math.max(0, math.min(100, p.value.read)))
                case ProgressBarMode.Indeterminate => Maybe.empty[Int]
        }
        val fill: Prop[Style]       = UI.reactive(shown.read.fold(Style.empty)(v => Style.width(v.pct)))
        val text: Prop[String]      = UI.reactive(shown.read.fold("")(p.valueTemplate))
        val labelled: Prop[Boolean] = UI.reactive(p.showValue.read && shown.read.exists(_ != 0))
        div.cssClass("p-progressbar", "p-component")
            .when(p.access.id)(_.id(_))
            .cssClass.matching(p.mode) {
                case ProgressBarMode.Determinate   => "p-progressbar-determinate"
                case ProgressBarMode.Indeterminate => "p-progressbar-indeterminate"
            }
            .role("progressbar")
            .aria(Aria.valuemin -> 0, Aria.valuemax -> 100, Aria.valuenow -> shown, Aria.label -> p.access.name)(
                div.cssClass("p-progressbar-value").inlineStyle(fill)(
                    UI.reactive(if labelled.read then div.cssClass("p-progressbar-label")(text) else UI.empty)
                )
            )
    end render
end ProgressBar

object ProgressBar:
    /** What a progress bar holds. Each field is a setter of its name (`uic.ProgressBar().value(40).showValue(false)`), which takes a
      * constant or a signal where the field is a `Prop` and the function itself for `valueTemplate`; `access` is written by the
      * shared traits (`accessibleName`, `id`).
      */
    final private[uic] case class Props(
        /** The percentage shown, clamped to 0 to 100 when drawn. A signal patches the fill, `aria-valuenow` and the label's text in
          * place. A progress bar is never edited by the reader, so a writable `SignalRef` reads exactly like a one-way signal here,
          * and a derived signal (`loaded.zip(total).map(pct)`) is the natural binding.
          */
        value: Prop[Int] = 0,
        /** `Determinate` (default) fills to the value; `Indeterminate` runs the endless slide and shows no value. A signal swaps the
          * class, the fill and `aria-valuenow` in place.
          */
        mode: Prop[ProgressBarMode] = ProgressBarMode.Determinate,
        /** Shows the value label inside the bar (default true, Prime semantics). A signal draws or drops the label in its region. */
        showValue: Prop[Boolean] = true,
        /** Formats the label from the percent shown (default `NN%`, e.g. `v => s"$v/100"`). */
        valueTemplate: Int => String = percent,
        @noSetter access: Accessibility = Accessibility.none
    ) derives CanEqual

    def apply(): ProgressBar = new ProgressBar(Props())

    private val percent: Int => String = v => s"$v%"
end ProgressBar
