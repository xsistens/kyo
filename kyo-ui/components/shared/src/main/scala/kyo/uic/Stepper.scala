package kyo.uic

import kyo.*
import kyo.UI.*

/** One step of a [[Stepper]], a hand-authored carrier: the header `title`, an optional stable `value` (what a
  * [[Stepper.value]] binding holds while this step is active), an unconditional `disabled`, and the panel `content`.
  * Built as `uic.Step("Account")(form)`, or as `_.step(...)` (see [[StepScope]]) with the same slots.
  */
final case class Step[+K] private[uic] (
    title: TextValue,
    value: Maybe[K],
    disabled: Boolean,
    // No defaults: the synthetic companion `apply` would then also carry defaults and clash with the hand-written one.
    content: List[UI]
) derives CanEqual

object Step:
    /** A step with a constant or reactive `title`. A `Signal[String]` title re-renders the header in place on emission
      * (e.g. a locale-driven `I18n.t` leaf).
      */
    def apply[K](title: String | Signal[String], value: Maybe[K] = Absent, disabled: Boolean = false)(content: UI*): Step[K] =
        new Step(ReactiveValue(title), value, disabled, content.toList)
end Step

/** A step, built directly or as `_.step(...)` (see [[StepScope]]). */
type StepEntry[K] = Entry[Step[K], StepScope[K]]

/** A [[StepEntry]] or a signal of steps, for a stepper whose steps change while it is shown (see [[LiveEntry]]). */
type StepLiveEntry[K] = LiveEntry[Step[K], StepScope[K]]

/** What `_` stands for in [[Stepper.steps]]: `_.step("Account", value = Present(Account))(form)`. */
final class StepScope[K] private ():
    /** A step, as [[Step.apply]]. */
    def step(title: String | Signal[String], value: Maybe[K] = Absent, disabled: Boolean = false)(content: UI*): Step[K] =
        Step(title, value, disabled)(content*)
end StepScope

object StepScope:
    // The scope holds nothing of its own type, so one instance serves every `K`.
    private val scope = new StepScope[Any]

    private[uic] def reading[K](entries: Seq[StepLiveEntry[K]])(using CanEqual[K, K], Frame): Reading[List[Step[K]]] =
        Entry.reading(scope.asInstanceOf[StepScope[K]], entries)
end StepScope

/** Stepper: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Stepper anatomy). Horizontal: `div.p-stepper.p-component[.p-stepper-readonly]`
  * > `div.p-steplist` of `div.p-step[.p-step-active][.p-disabled]` items (each
  * a `button.p-step-header[role=tab]` > `span.p-step-number` +
  * `span.p-step-title`, followed by the `span.p-stepper-separator` except on the
  * last step), plus `div.p-steppanels` > `div.p-steppanel` holding only the
  * ACTIVE step's content. Vertical (`vertical(true)`, Prime's StepItem
  * composition): the root stacks `div.p-stepitem[.p-stepitem-active]` wrappers,
  * each holding its `div.p-step` header and (for the active item) the
  * `div.p-steppanel.p-steppanel-active` > `div.p-steppanel-content-wrapper` >
  * vertical `span.p-stepper-separator` (except the last item) +
  * `div.p-steppanel-content` inline under the header. Both layouts use the
  * extracted `@primeuix` stepper CSS verbatim (including the
  * completed-separator accent via `.p-step:has(~ .p-step-active)`).
  *
  * Steps are value-keyed like Prime's: a step may carry a value of the app's own
  * type `K` and `value(ref)` binds the ACTIVE VALUE two-way; the index-based
  * `active(ref)` binding needs no values at all (`value` wins when both are
  * bound). Header clicks
  * write the bound ref back before firing `onActivate`. `linear(true)` renders
  * the stepper readonly-forward: headers AHEAD of the active step are disabled
  * (backward navigation stays allowed); per-step `disabled = true` blocks a
  * header unconditionally. The app advances by writing the ref (a "Next"
  * button).
  *
  * The steps may change while the stepper is shown: [[steps]] takes a signal of them beside fixed ones (see
  * [[LiveEntry]]). The binding is read against the steps there are at each render, so a value no step carries shows
  * the first step and an index past the last shows the last, until the signal brings the step they name. A header's
  * id is the mount's base and its position, `s"$base-$index"`, so a step the signal adds needs nothing minted.
  */
final case class Stepper[K] private (
    // The steps the headers draw, resolved from `stepSources` by the render.
    stepList: List[Step[K]] = Nil,
    stepSources: List[StepLiveEntry[K]] = Nil,
    activeRef: Maybe[SignalRef[Int]] = Absent,
    valueRef: Maybe[SignalRef[K]] = Absent,
    linearFlag: Boolean = false,
    verticalFlag: Boolean = false,
    onActivateF: Maybe[Int => UI.Action] = Absent
) extends Node:
    type Self = Stepper[K]

    // Step values compare by `equals`, the identity an app's own type already has.
    private given CanEqual[K, K] = CanEqual.derived

    /** Appends one step: its header `title`, an optional stable `value` (what a
      * [[value]] binding holds while this step is active), an optional unconditional
      * `disabled`, plus the panel content. A `Signal[String]` title re-renders the step header
      * in place on emission (e.g. a locale-driven `I18n.t` leaf).
      *
      * `value` and `disabled` keep their defaults for BOTH title shapes. As two overloads they
      * could not: the synthesized default-arg getters of same-named methods clash, so the
      * reactive variant had to spell both out at every call site.
      */
    def step(title: String | Signal[String], value: Maybe[K] = Absent, disabled: Boolean = false)(
        content: UI*
    ): Stepper[K] =
        copy(stepSources = stepSources :+ Step(title, value, disabled)(content*))

    /** Appends the given steps, each built directly or from `_` (see [[StepScope]]), or a signal of steps that change
      * while the stepper is shown (see [[LiveEntry]]). A `_` builder needs `K` settled before it: bind [[value]] first,
      * or hand `uic.Step(...)` values, which settle it as a step's `value` does.
      */
    def steps(ss: StepLiveEntry[K]*): Stepper[K] = copy(stepSources = stepSources ++ ss)

    /** The steps as ONE reading: fixed unless one of them was handed over as a signal. */
    private[uic] def entries(using Frame): Reading[List[Step[K]]] = StepScope.reading(stepSources)

    /** Binds the 0-based active step index two-way to `ref` (clamped at render). */
    def active(ref: SignalRef[Int]): Stepper[K] = copy(activeRef = Present(ref))

    /** Binds the ACTIVE STEP VALUE two-way to `ref` (Prime's `v-model:value`):
      * header clicks write the step's value back (a step with no value writes
      * nothing); a ref value no step carries falls back to the first step. Wins
      * over [[active]] when both are bound.
      */
    def value(ref: SignalRef[K]): Stepper[K] = copy(valueRef = Present(ref))

    /** Linear mode: header clicks cannot jump AHEAD of the active step
      * (backward stays allowed); advance by writing the bound ref.
      */
    def linear(v: Boolean): Stepper[K] = copy(linearFlag = v)

    /** Vertical layout (Prime's StepItem composition): each step's panel renders
      * inline UNDER its header with the vertical connecting separator.
      */
    def vertical(v: Boolean): Stepper[K] = copy(verticalFlag = v)

    /** Fired with the target step index after a header click's ref write-back. */
    def onActivate(f: Int => UI.Action)(using Frame): Stepper[K] = copy(onActivateF = Present(Callback.of(f)))

    private[uic] def render(using Frame): UI =
        UI.mounted {
            UI.commands.map(cmds => cmds.freshId.map(base => wired(base, id => cmds.focusId(id))))
        }.placeholder(shown(Absent, _ => ()))

    /** The rendered stepper: the steps and the binding read as ONE region. No `base` is the placeholder a golden render
      * and an SSG page show: no mount has run, so there is nothing to move focus with yet, but a horizontal stepper is
      * already the one tab stop its tablist will be, on the active header.
      */
    private def shown(base: Maybe[String], focus: String => UI.Action)(using Frame): UI =
        def drawn(ss: List[Step[K]], active: Int): UI =
            copy(stepList = ss).body(active, base.fold(Nil)(b => ss.indices.toList.map(i => s"$b-$i")), focus)
        valueRef match
            case Present(ref) =>
                entries.and(Reading.Live(ref)).render((ss, v) => drawn(ss, math.max(ss.indexWhere(_.value.contains(v)), 0)))
            case Absent =>
                activeRef match
                    case Present(ref) => entries.and(Reading.Live(ref)).render((ss, a) => drawn(ss, a))
                    case Absent       => entries.render(ss => drawn(ss, 0))
        end match
    end shown

    /** The seam the golden tests render, since a mount shows only its placeholder there. */
    private[uic] def wired(base: String, focus: String => UI.Action)(using Frame): UI = shown(Present(base), focus)

    /** The positions a header arrow may land on: a blocked step is not focusable, so it is stepped
      * over. Linear mode blocks everything past the current step.
      */
    private def navigableSteps(cur: Int): List[Int] =
        stepList.indices.toList.filterNot(i => stepList(i).disabled || (linearFlag && i > cur))

    /** Moves focus between headers, and does nothing else: a header IS a `<button>`, so Enter and
      * Space already reach it once, and a third activation from here would be one too many.
      */
    private def headerMove(ids: List[String], navigable: List[Int], self: Int, axis: ListNav.Orientation, focus: String => UI.Action)(
        using Frame
    ): KeyboardEvent => UI.Action = e =>
        ListNav.onKey(navigable, self, e.key, wrap = true, axis) match
            case Present(step) if step.focus != self && ids.isDefinedAt(step.focus) => focus(ids(step.focus))
            case _                                                                  => ()

    private def body(active: Int, ids: List[String], focus: String => UI.Action)(using Frame): UI =
        val cur = math.min(math.max(active, 0), math.max(stepList.length - 1, 0))
        if verticalFlag then verticalBody(cur, ids, focus) else horizontalBody(cur, ids, focus)

    /** The shared `div.p-step` header row (button + number + title).
      *
      * `tablist` says whether the headers sit together in one container. They do in the horizontal
      * layout, which makes that layout a real ARIA tablist: `role="tab"` on the headers, ONE tab
      * stop, and the arrows moving between them. They do not in the vertical layout, where each
      * header sits inside its own `p-stepitem` beside its own panel; that is an accordion's shape,
      * not a tablist's, so the headers carry no `role="tab"` there (a tab outside a tablist is a
      * structure a screen reader cannot read) and each stays its own tab stop, with the arrows
      * added on top exactly as [[Accordion]] has them.
      */
    private def stepHeader(
        st: Step[K],
        i: Int,
        cur: Int,
        tablist: Boolean,
        ids: List[String],
        focus: String => UI.Action
    )(using Frame): UI =
        val isActive = i == cur
        // Linear: only backward (and re-clicking the current step) is reachable
        // via the headers; forward jumps go through the bound ref (a Next button).
        // A per-step disabled blocks the header unconditionally.
        val blocked = st.disabled || (linearFlag && i > cur)

        var header = button
            .cssClass("p-step-header")
            .jsProp("type", "button")
        if tablist then header = header.role("tab").aria("selected", isActive.toString)
        // A tablist is one tab stop, on the active header, from the first render: the arrows that
        // move between the headers come with the mount's ids, the single stop does not wait for them.
        if tablist && !blocked then header = header.tabIndex(if isActive then 0 else -1)
        if blocked then header = header.disabled(true).tabIndex(-1)
        else
            if !isActive then header = header.onClick(activate(i))
            if ids.isDefinedAt(i) then
                val axis = if tablist then ListNav.Orientation.Horizontal else ListNav.Orientation.Vertical
                header = header.id(ids(i)).onKeyDown(headerMove(ids, navigableSteps(cur), i, axis, focus))
            end if
        end if
        val titleSlot: UI = st.title match
            case TextValue.Const(t) => span.cssClass("p-step-title")(t): UI
            case TextValue.Dyn(s)   => span.cssClass("p-step-title")(s)
        val headerEl: UI = header(
            toChild(span.cssClass("p-step-number")((i + 1).toString)),
            toChild(titleSlot)
        )

        var stepEl = div.cssClass("p-step").role("presentation")
        if isActive then stepEl = stepEl.cssClass("p-step-active").aria("current", "step")
        if blocked then stepEl = stepEl.cssClass("p-disabled")
        // Horizontal stepList carry the separator inside the step row (Prime); the
        // vertical layout renders it inside the active panel instead.
        val separatorSlot: List[UI] =
            if !verticalFlag && i < stepList.length - 1 then List(span.cssClass("p-stepper-separator")) else Nil
        stepEl((headerEl :: separatorSlot).map(toChild)*)
    end stepHeader

    private def horizontalBody(cur: Int, ids: List[String], focus: String => UI.Action)(using Frame): UI =
        val stepEls: List[UI] = stepList.zipWithIndex.map((st, i) => stepHeader(st, i, cur, tablist = true, ids, focus))

        val panel: List[UI] = stepList.lift(cur).toList.map { st =>
            animatedPanel(cur, horizontalPanel(st, animated = false), horizontalPanel(st, animated = true))
        }

        var root = div.cssClass("p-stepper").cssClass("p-component")
        if linearFlag then root = root.cssClass("p-stepper-readonly")
        root(
            toChild(div.cssClass("p-steplist").role("tablist")(stepEls.map(toChild)*)),
            toChild(div.cssClass("p-steppanels")(panel.map(toChild)*))
        )
    end horizontalBody

    /** The `div.p-steppanel` for the active horizontal step; `animated` adds the
      * crossfade transition hooks (only the live mount, never the placeholder).
      */
    private def horizontalPanel(st: Step[K], animated: Boolean)(using Frame): UI =
        var p = div.cssClass("p-steppanel").role("tabpanel")
        if animated then
            p = p.cssClass("p-uic-steppanel-anim").enterTransition("p-uic-enter-fade").leaveTransition("p-uic-leave-fade")
        p(st.content.map(toChild)*)
    end horizontalPanel

    /** Wraps the active panel in a step-keyed mount so a step change animates it:
      * the outgoing panel's leave ghost fades out (`p-uic-leave-fade`) while the
      * incoming panel fades in (`p-uic-enter-fade`). A bare `enterTransition` would
      * never replay (`ref.render` keeps the horizontal panel's structural path
      * stable), so the mount's key (the active index) is what forces the teardown +
      * remount. The placeholder is the un-animated panel, so SSR/golden output stays
      * byte-identical (the animation is a pure runtime enhancement).
      */
    private def animatedPanel(cur: Int, plain: UI, animated: UI)(using Frame): UI =
        UI.mounted(animated).keyed(cur).placeholder(plain)

    /** The `div.p-steppanel.p-steppanel-active` for the active vertical step;
      * `animated` adds the crossfade transition hooks (only the live mount).
      */
    private def verticalPanel(wrapperChildren: List[UI], animated: Boolean)(using Frame): UI =
        // No `role="tabpanel"` here: the vertical layout has no tablist, so its headers are not
        // tabs and this is not their panel. A tabpanel with no tab is a dangling reference a
        // screen reader reads as a broken relationship.
        var p = div.cssClass("p-steppanel").cssClass("p-steppanel-active")
        if animated then
            p = p.cssClass("p-uic-steppanel-anim").enterTransition("p-uic-enter-fade").leaveTransition("p-uic-leave-fade")
        p(toChild(div.cssClass("p-steppanel-content-wrapper")(wrapperChildren.map(toChild)*)))
    end verticalPanel

    private def verticalBody(cur: Int, ids: List[String], focus: String => UI.Action)(using Frame): UI =
        val items: List[UI] = stepList.zipWithIndex.map { (st, i) =>
            val isActive = i == cur

            // Only the ACTIVE item renders its panel (Prime v-shows the others away);
            // the separator is the vertical connector next to the content.
            val panel: List[UI] =
                if !isActive then Nil
                else
                    val sep: List[UI] =
                        if i < stepList.length - 1 then List(span.cssClass("p-stepper-separator")) else Nil
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
            item((stepHeader(st, i, cur, tablist = false, ids, focus) :: panel).map(toChild)*)
        }

        var root = div.cssClass("p-stepper").cssClass("p-component")
        if linearFlag then root = root.cssClass("p-stepper-readonly")
        root(items.map(toChild)*)
    end verticalBody

    /** Header click: write the bound ref (value wins, matching render), then fire
      * `onActivate` with the index.
      */
    private def activate(i: Int)(using Frame): UI.Action =
        val write: UI.Action = valueRef match
            case Present(ref) =>
                stepList(i).value match
                    case Present(v) => ref.set(v)
                    case Absent     => ()
            case Absent =>
                activeRef match
                    case Present(ref) => ref.set(i)
                    case Absent       => ()
        val fire: UI.Action = onActivateF match
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

    /** An empty stepper whose steps may carry values of type `K`.
      *
      * `K` is the type of the step VALUES, so it never needs spelling out: in a chain, `uic.Stepper()` takes it from the
      * first setter that mentions it (`value(ref)`, or a step's `value`), and a stepper driven by position alone
      * ([[Stepper.active]]) ends up a `Stepper[Nothing]`, the exact statement that no step carries a value. Nothing has
      * no values, so a step value or a `value` binding added to such a stepper later does not compile, rather than being
      * accepted at some placeholder type the stepper would never read. Spelling a placeholder (`Stepper[String]()`
      * beside `active`) is what to avoid: it admits values nothing binds.
      */
    def apply[K](): Stepper[K] = new Stepper[K]()
end Stepper
