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
  * The `value` binds either to a constant `Int` or, like [[Input]]/[[CheckBox]],
  * reactively to a `SignalRef[Int]` via a const-or-ref carrier; a ref change
  * re-renders the bar width and `aria-valuenow`. The label shows `NN%` by
  * default (Prime's `showValue`); `valueTemplate` formats it, `showValue(false)`
  * hides it. Like Prime, a zero value renders no label.
  */
final class ProgressBar private[uic] (private[uic] val p: ProgressBar.Props) extends Node, HasAccessibleName, HasProps:
    import p.*

    type Self = ProgressBar

    /** The percentage shown (clamped to 0 to 100 at render). A constant renders the bar statically; any
      * `Signal[Int]` updates the bar width and `aria-valuenow` on emission.
      *
      * A progress bar is never edited by the reader, so the two-way case a writable `SignalRef` would
      * carry has nothing to write: it reads exactly like a one-way signal here. A derived signal
      * (`loaded.zip(total).map(pct)`) is the natural binding.
      */
    def value(v: Int | Signal[Int]): ProgressBar = new ProgressBar(p.copy(valueBinding = Present(ReactiveValue(v))))

    /** `Determinate` (default) or the endless `Indeterminate` slide. */
    def mode(v: ProgressBarMode): ProgressBar = new ProgressBar(p.copy(modeV = v))

    /** Shows the value label inside the bar (default true, Prime semantics). */
    def showValue(v: Boolean): ProgressBar = new ProgressBar(p.copy(showValueFlag = v))

    /** Formats the label from the current value (e.g. `v => s"$v/100"`). */
    def valueTemplate(f: Int => String): ProgressBar = new ProgressBar(p.copy(valueTemplateV = Present(f)))

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new ProgressBar(p.copy(access = v))
    private[uic] def render(using Frame): UI                   =
        modeV match
            case ProgressBarMode.Indeterminate => indeterminateBody
            case ProgressBarMode.Determinate   => valueBinding.getOrElse(ReactiveValue.Const(0)).render(determinateBody)

    private def root(using Frame) =
        var el = div
            .cssClass("p-progressbar")
            .cssClass("p-component")
            .role("progressbar")
            .aria(
                Aria.valuemin -> "0",
                Aria.valuemax -> "100"
            )
        access.name.foreach(n => el = el.aria(Aria.label -> n))
        el
    end root

    private def determinateBody(raw: Int)(using Frame): UI =
        val pct             = math.max(0, math.min(100, raw))
        val label: List[UI] =
            if !showValueFlag || pct == 0 then Nil
            else List(div.cssClass("p-progressbar-label")(valueTemplateV.map(_(pct)).getOrElse(s"$pct%")))
        root
            .cssClass("p-progressbar-determinate")
            .aria(Aria.valuenow -> pct)(
                div.cssClass("p-progressbar-value").style(_.width(pct.pct))(UI.children(label)*)
            )
    end determinateBody

    private def indeterminateBody(using Frame): UI =
        root.cssClass("p-progressbar-indeterminate")(
            div.cssClass("p-progressbar-value")
        )
end ProgressBar

object ProgressBar:
    final private[uic] case class Props(
        valueBinding: Maybe[ReactiveValue[Int]] = Absent,
        modeV: ProgressBarMode = ProgressBarMode.Determinate,
        showValueFlag: Boolean = true,
        valueTemplateV: Maybe[Int => String] = Absent,
        access: Accessibility = Accessibility.none
    )

    def apply(): ProgressBar = new ProgressBar(Props())
end ProgressBar
