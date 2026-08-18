package kyo.uic

import kyo.*
import kyo.UI.*

/** Stepper — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Stepper anatomy). Horizontal: `div.p-stepper.p-component[.p-stepper-readonly]`
  * > `div.p-steplist` of `div.p-step[.p-step-active][.p-disabled]` items — each
  * a `button.p-step-header[role=tab]` > `span.p-step-number` +
  * `span.p-step-title`, followed by the `span.p-stepper-separator` (except the
  * last step) — plus `div.p-steppanels` > `div.p-steppanel` holding only the
  * ACTIVE step's content. Vertical (`vertical(true)`, Prime's StepItem
  * composition): the root stacks `div.p-stepitem[.p-stepitem-active]` wrappers,
  * each holding its `div.p-step` header and — for the active item — the
  * `div.p-steppanel.p-steppanel-active` > `div.p-steppanel-content-wrapper` >
  * vertical `span.p-stepper-separator` (except the last item) +
  * `div.p-steppanel-content` inline under the header. Both layouts use the
  * extracted `@primeuix` stepper CSS verbatim (including the
  * completed-separator accent via `.p-step:has(~ .p-step-active)`).
  *
  * Steps are value-keyed like Prime's: each step carries a String value
  * (defaulting to its 1-based position, Prime's convention) and `value(ref)`
  * binds the ACTIVE VALUE two-way; the index-based `active(ref)` binding stays
  * as the compatibility path (`value` wins when both are bound). Header clicks
  * write the bound ref back before firing `onActivate`. `linear(true)` renders
  * the stepper readonly-forward: headers AHEAD of the active step are disabled
  * (backward navigation stays allowed); per-step `disabled = true` blocks a
  * header unconditionally. The app advances by writing the ref (a "Next"
  * button).
  */
final case class Stepper private (
    steps: List[Stepper.StepDef] = Nil,
    activeRef: Maybe[SignalRef[Int]] = Absent,
    valueRef: Maybe[SignalRef[String]] = Absent,
    linearFlag: Boolean = false,
    verticalFlag: Boolean = false,
    onActivateF: Maybe[Int => Any < Async] = Absent
) extends Node:
    type Self = Stepper

    /** Appends one step: its header `title`, an optional stable `value` (defaults
      * to the 1-based position, Prime's convention), an optional unconditional
      * `disabled`, plus the panel content.
      */
    def step(title: String, value: Maybe[String] = Absent, disabled: Boolean = false)(content: UI*): Stepper =
        copy(steps = steps :+ new Stepper.StepDef(TextValue.Const(title), value, disabled, content.toList))

    /** Reactive-title variant: the step's header title tracks `title` — re-renders in
      * place on emission (e.g. a locale-driven `I18n.t` leaf). The `value` key and
      * `disabled` flag stay in their fixed positions; they carry no defaults here so
      * this overload's synthesized default-arg getters do not clash with the plain
      * `String` overload's.
      */
    def step(title: Signal[String], value: Maybe[String], disabled: Boolean)(content: UI*): Stepper =
        copy(steps = steps :+ new Stepper.StepDef(TextValue.Dyn(title), value, disabled, content.toList))

    /** Binds the 0-based active step index two-way to `ref` (clamped at render). */
    def active(ref: SignalRef[Int]): Stepper = copy(activeRef = Present(ref))

    /** Binds the ACTIVE STEP VALUE two-way to `ref` (Prime's `v-model:value`):
      * header clicks write the step's value back; an unknown ref value falls back
      * to the first step. Wins over [[active]] when both are bound.
      */
    def value(ref: SignalRef[String]): Stepper = copy(valueRef = Present(ref))

    /** Linear mode: header clicks cannot jump AHEAD of the active step
      * (backward stays allowed); advance by writing the bound ref.
      */
    def linear(v: Boolean): Stepper = copy(linearFlag = v)

    /** Vertical layout (Prime's StepItem composition): each step's panel renders
      * inline UNDER its header with the vertical connecting separator.
      */
    def vertical(v: Boolean): Stepper = copy(verticalFlag = v)

    /** Fired with the target step index after a header click's ref write-back. */
    def onActivate(f: Int => Any < Async): Stepper = copy(onActivateF = Present(f))

    /** The step's stable value: explicit, else the 1-based position (Prime). */
    private def stepValue(i: Int): String = steps(i).value.getOrElse((i + 1).toString)

    private[uic] def render(using Frame): UI =
        valueRef match
            case Present(ref) =>
                ref.render(v => body(steps.indices.find(stepValue(_) == v).getOrElse(0)))
            case Absent =>
                activeRef match
                    case Present(ref) => ref.render(body)
                    case Absent       => body(0)

    private def body(active: Int)(using Frame): UI =
        val cur = math.min(math.max(active, 0), math.max(steps.length - 1, 0))
        if verticalFlag then verticalBody(cur) else horizontalBody(cur)

    /** The shared `div.p-step` header row (button + number + title). */
    private def stepHeader(st: Stepper.StepDef, i: Int, cur: Int)(using Frame): UI =
        val isActive = i == cur
        // Linear: only backward (and re-clicking the current step) is reachable
        // via the headers; forward jumps go through the bound ref (a Next button).
        // A per-step disabled blocks the header unconditionally.
        val blocked = st.disabled || (linearFlag && i > cur)

        var header = button
            .cssClass("p-step-header")
            .jsProp("type", "button")
            .role("tab")
        if blocked then header = header.disabled(true).tabIndex(-1)
        else if !isActive then header = header.onClick(activate(i))
        val titleSlot: UI = st.title match
            case TextValue.Const(t) => span.cssClass("p-step-title")(t): UI
            case TextValue.Dyn(s)   => s.render(t => span.cssClass("p-step-title")(t))
        val headerEl: UI = header(
            toChild(span.cssClass("p-step-number")((i + 1).toString)),
            toChild(titleSlot)
        )

        var stepEl = div.cssClass("p-step").role("presentation")
        if isActive then stepEl = stepEl.cssClass("p-step-active").aria("current", "step")
        if blocked then stepEl = stepEl.cssClass("p-disabled")
        // Horizontal steps carry the separator inside the step row (Prime); the
        // vertical layout renders it inside the active panel instead.
        val separatorSlot: List[UI] =
            if !verticalFlag && i < steps.length - 1 then List(span.cssClass("p-stepper-separator")) else Nil
        stepEl((headerEl :: separatorSlot).map(toChild)*)
    end stepHeader

    private def horizontalBody(cur: Int)(using Frame): UI =
        val stepEls: List[UI] = steps.zipWithIndex.map((st, i) => stepHeader(st, i, cur))

        val panel: List[UI] = steps.lift(cur).toList.map { st =>
            animatedPanel(cur, horizontalPanel(st, animated = false), horizontalPanel(st, animated = true))
        }

        var root = div.cssClass("p-stepper").cssClass("p-component")
        if linearFlag then root = root.cssClass("p-stepper-readonly")
        root(
            toChild(div.cssClass("p-steplist")(stepEls.map(toChild)*)),
            toChild(div.cssClass("p-steppanels")(panel.map(toChild)*))
        )
    end horizontalBody

    /** The `div.p-steppanel` for the active horizontal step; `animated` adds the
      * crossfade transition hooks (only the live mount, never the placeholder).
      */
    private def horizontalPanel(st: Stepper.StepDef, animated: Boolean)(using Frame): UI =
        var p = div.cssClass("p-steppanel").role("tabpanel")
        if animated then
            p = p.cssClass("p-uic-steppanel-anim").enterTransition("p-uic-enter-fade").leaveTransition("p-uic-leave-fade")
        p(st.content.map(toChild)*)
    end horizontalPanel

    /** Wraps the active panel in a step-keyed mount so a step change animates it:
      * the outgoing panel's leave ghost fades out (`p-uic-leave-fade`) while the
      * incoming panel fades in (`p-uic-enter-fade`). A bare `enterTransition` would
      * never replay — `ref.render` keeps the (horizontal) panel's structural path
      * stable — so the mount's key (the active index) is what forces the teardown +
      * remount. The placeholder is the un-animated panel, so SSR/golden output stays
      * byte-identical (the animation is a pure runtime enhancement).
      */
    private def animatedPanel(cur: Int, plain: UI, animated: UI)(using Frame): UI =
        UI.mounted(animated).keyed(cur).placeholder(plain)

    /** The `div.p-steppanel.p-steppanel-active` for the active vertical step;
      * `animated` adds the crossfade transition hooks (only the live mount).
      */
    private def verticalPanel(wrapperChildren: List[UI], animated: Boolean)(using Frame): UI =
        var p = div.cssClass("p-steppanel").cssClass("p-steppanel-active").role("tabpanel")
        if animated then
            p = p.cssClass("p-uic-steppanel-anim").enterTransition("p-uic-enter-fade").leaveTransition("p-uic-leave-fade")
        p(toChild(div.cssClass("p-steppanel-content-wrapper")(wrapperChildren.map(toChild)*)))
    end verticalPanel

    private def verticalBody(cur: Int)(using Frame): UI =
        val items: List[UI] = steps.zipWithIndex.map { (st, i) =>
            val isActive = i == cur

            // Only the ACTIVE item renders its panel (Prime v-shows the others away);
            // the separator is the vertical connector next to the content.
            val panel: List[UI] =
                if !isActive then Nil
                else
                    val sep: List[UI] =
                        if i < steps.length - 1 then List(span.cssClass("p-stepper-separator")) else Nil
                    val wrapperChildren = sep :+ div.cssClass("p-steppanel-content")(st.content.map(toChild)*)
                    List(
                        animatedPanel(
                            cur,
                            verticalPanel(wrapperChildren, animated = false),
                            verticalPanel(wrapperChildren, animated = true)
                        )
                    )

            var item = div.cssClass("p-stepitem")
            if isActive then item = item.cssClass("p-stepitem-active")
            item((stepHeader(st, i, cur) :: panel).map(toChild)*)
        }

        var root = div.cssClass("p-stepper").cssClass("p-component")
        if linearFlag then root = root.cssClass("p-stepper-readonly")
        root(items.map(toChild)*)
    end verticalBody

    /** Header click: write the bound ref (value wins, matching render), then fire
      * `onActivate` with the index.
      */
    private def activate(i: Int)(using Frame): Any < Async =
        val write: Any < Async = valueRef match
            case Present(ref) => ref.set(stepValue(i))
            case Absent =>
                activeRef match
                    case Present(ref) => ref.set(i)
                    case Absent       => ()
        val fire: Any < Async = onActivateF match
            case Present(f) => f(i)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end activate
end Stepper

object Stepper:
    def apply(): Stepper = new Stepper()

    /** One step definition: header `title`, optional stable `value` (defaults to
      * the 1-based position), unconditional `disabled`, and the panel content.
      */
    final case class StepDef private[uic] (
        title: TextValue,
        value: Maybe[String] = Absent,
        disabled: Boolean = false,
        content: List[UI] = Nil
    )
end Stepper
