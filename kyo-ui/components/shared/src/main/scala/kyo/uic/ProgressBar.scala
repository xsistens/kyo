package kyo.uic

import kyo.*
import kyo.UI.*

/** ProgressBar mode: `Determinate` (default, value-driven fill) or
  * `Indeterminate` (the endless slide animation).
  */
enum ProgressBarMode derives CanEqual:
    case Determinate, Indeterminate

/** ProgressBar — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ProgressBar anatomy: `div.p-progressbar[role=progressbar]` with the
  * `.p-progressbar-determinate`/`-indeterminate` mode class > `.p-progressbar-value`
  * (width in %) > optional `.p-progressbar-label`), so the extracted `@primeuix`
  * progressbar CSS applies verbatim.
  *
  * The `value` binds either to a constant `Int` or, like [[Input]]/[[CheckBox]],
  * reactively to a `SignalRef[Int]` via a const-or-ref carrier — a ref change
  * re-renders the bar width and `aria-valuenow`. The label shows `NN%` by
  * default (Prime's `showValue`); `valueTemplate` formats it, `showValue(false)`
  * hides it. Like Prime, a zero value renders no label.
  */
final case class ProgressBar private (
    valueBinding: Maybe[ReactiveValue[Int]] = Absent,
    modeV: ProgressBarMode = ProgressBarMode.Determinate,
    showValueFlag: Boolean = true,
    valueTemplateV: Maybe[Int => String] = Absent,
    accessibleNameV: Maybe[TextValue] = Absent
) extends Node:
    type Self = ProgressBar

    /** Sets a constant percentage (clamped to 0–100 at render). */
    def value(v: Int): ProgressBar = copy(valueBinding = Present(ReactiveValue.Const(v)))

    /** Binds reactively to `ref`: ref changes update the bar width and `aria-valuenow`. */
    def value(ref: SignalRef[Int]): ProgressBar = copy(valueBinding = Present(ReactiveVariable(ref)))

    /** Binds to a one-way DERIVED signal (e.g. `combineLatest(loaded, total).map(pct)`): the bar tracks it
      * read-only. Prefer this over an artificial `SignalRef` when the progress is computed, not user-edited.
      */
    def value(sig: Signal[Int]): ProgressBar = copy(valueBinding = Present(ReactiveValue.Dyn(sig)))

    /** `Determinate` (default) or the endless `Indeterminate` slide. */
    def mode(v: ProgressBarMode): ProgressBar = copy(modeV = v)

    /** Shows the value label inside the bar (default true — Prime semantics). */
    def showValue(v: Boolean): ProgressBar = copy(showValueFlag = v)

    /** Formats the label from the current value (e.g. `v => s"$v/100"`). */
    def valueTemplate(f: Int => String): ProgressBar = copy(valueTemplateV = Present(f))

    /** Accessible name, emitted as `aria-label`. */
    def accessibleName(v: String): ProgressBar = copy(accessibleNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): ProgressBar = copy(accessibleNameV = Present(TextValue.Dyn(sig)))

    private[uic] def render(using Frame): UI =
        modeV match
            case ProgressBarMode.Indeterminate => indeterminateBody
            case ProgressBarMode.Determinate =>
                valueBinding match
                    case Present(ReactiveVariable(ref))  => ref.render(determinateBody)
                    case Present(ReactiveValue.Dyn(sig)) => sig.render(determinateBody)
                    case Present(ReactiveValue.Const(v)) => determinateBody(v)
                    case Absent                          => determinateBody(0)

    private def root(using Frame) =
        var el = div
            .cssClass("p-progressbar")
            .cssClass("p-component")
            .role("progressbar")
            .aria("valuemin", "0")
            .aria("valuemax", "100")
        accessibleNameV match
            case Present(TextValue.Const(n)) => el = el.aria("label", n)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        el
    end root

    private def determinateBody(raw: Int)(using Frame): UI =
        val pct = math.max(0, math.min(100, raw))
        val label: List[UI] =
            if !showValueFlag || pct == 0 then Nil
            else List(div.cssClass("p-progressbar-label")(valueTemplateV.map(_(pct)).getOrElse(s"$pct%")))
        root
            .cssClass("p-progressbar-determinate")
            .aria("valuenow", pct.toString)(
                div.cssClass("p-progressbar-value").style(_.width(pct.pct))(label.map(toChild)*)
            )
    end determinateBody

    private def indeterminateBody(using Frame): UI =
        root.cssClass("p-progressbar-indeterminate")(
            div.cssClass("p-progressbar-value")
        )
end ProgressBar

object ProgressBar:
    def apply(): ProgressBar = new ProgressBar()
