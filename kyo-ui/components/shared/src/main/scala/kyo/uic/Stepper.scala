package kyo.uic

import kyo.*
import kyo.UI.*

/** One step of a [[Stepper]], a hand-authored carrier: the header `title`, an optional stable `value` (what a
  * [[Stepper.value]] binding holds while this step is active), an unconditional `disabled`, and the panel `content`.
  * Built as `uic.Step("Account")(form)`, or as `_.step(...)` (see [[StepScope]]) with the same slots.
  */
final class Step[+K] private[uic] (private[uic] val p: Step.Props[K]) extends HasProps derives CanEqual

object Step:
    final private[uic] case class Props[+K](
        title: TextValue,
        value: Maybe[K],
        disabled: Boolean,
        content: List[UI]
    )

    /** A step with a constant or reactive `title`. A `Signal[String]` title re-renders the header in place on emission
      * (e.g. a locale-driven `I18n.t` leaf).
      */
    def apply[K](title: String | Signal[String], value: Maybe[K] = Absent, disabled: Boolean = false)(content: UI*): Step[K] =
        new Step(Props(ReactiveValue(title), value, disabled, content.toList))
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

    private[uic] def reading[K](entries: Seq[StepLiveEntry[K]])(using CanEqual[K, K], Frame): ReactiveValue[List[Step[K]]] =
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
  * type `K` and `value` binds the ACTIVE VALUE; the index-based `active` binding
  * needs no values at all (`value` wins when both are bound). Each takes a
  * constant, a signal or a ref, and unbound the stepper keeps the active step
  * itself. A header click writes a ref, reports to `onValueChange` in value mode,
  * then to `onActiveChange`. `linear(true)` renders
  * the stepper readonly-forward: headers AHEAD of the active step are disabled
  * (backward navigation stays allowed); per-step `disabled = true` blocks a
  * header unconditionally. The app advances by writing its binding (a "Next"
  * button).
  *
  * The steps may change while the stepper is shown: [[steps]] takes a signal of them beside fixed ones (see
  * [[LiveEntry]]). The binding is read against the steps there are at each render, so a value no step carries shows
  * the first step and an index past the last shows the last, until the signal brings the step they name. A header's
  * id is the mount's base and its position, `s"$base-$index"`, so a step the signal adds needs nothing minted.
  */
final class Stepper[K] private[uic] (private[uic] val p: Stepper.Props[K]) extends Node, HasProps:
    import p.*

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
        new Stepper(p.copy(stepSources = stepSources :+ Step(title, value, disabled)(content*)))

    /** Appends the given steps, each built directly or from `_` (see [[StepScope]]), or a signal of steps that change
      * while the stepper is shown (see [[LiveEntry]]). A `_` builder needs `K` settled before it: bind [[value]] first,
      * or hand `uic.Step(...)` values, which settle it as a step's `value` does.
      */
    def steps(ss: StepLiveEntry[K]*): Stepper[K] = new Stepper(p.copy(stepSources = stepSources ++ ss))

    /** The steps as ONE reading: fixed unless one of them was handed over as a signal. */
    private[uic] def entries(using Frame): ReactiveValue[List[Step[K]]] = StepScope.reading(stepSources)

    /** Binds the 0-based active step index (clamped at render). A `SignalRef` binds two way: a header click writes it,
      * then [[onActiveChange]] hears the index. A plain `Signal` binds one way: the stepper shows it and only reports. A
      * constant nobody hears is fixed: the stepper stays on that step, its headers reachable and inert. Unbound, the
      * stepper keeps the active step itself, starting on the first. A signal nothing hears is reported in a card.
      */
    def active(v: Int | Signal[Int]): Stepper[K] = new Stepper(p.copy(activeSlot = activeSlot.bind(v)))

    /** Binds the ACTIVE STEP VALUE (Prime's `v-model:value`), bound like [[active]]: a header click asks for the step's
      * value (a step with no value asks for nothing) and [[onValueChange]] hears it; a value no step carries falls back to
      * the first step. Wins over [[active]] when both are bound.
      */
    // `Signal[K]` first: with `K` still open (`uic.Stepper().value(ref)`), the solver then reads `K` off the ref's own type
    // instead of taking the whole ref as `K`.
    def value(v: Signal[K] | K): Stepper[K] = new Stepper(p.copy(valueSlot = valueSlot.bind(v)))

    /** Fired with the step value the reader asks for, after a two-way [[value]] is written and before [[onActiveChange]]. */
    def onValueChange(f: K => UI.Action)(using Frame): Stepper[K] = new Stepper(p.copy(valueSlot = valueSlot.hear(f)))

    /** A constant nobody hears is fixed: the stepper stays on that step, and every header is inert. */
    private def fixed: Boolean = if valueSlot.binding.isDefined then valueSlot.fixed else activeSlot.fixed

    /** Linear mode: header clicks cannot jump AHEAD of the active step
      * (backward stays allowed); advance by writing the bound ref.
      */
    def linear(v: Boolean): Stepper[K] = new Stepper(p.copy(linearFlag = v))

    /** Vertical layout (Prime's StepItem composition): each step's panel renders
      * inline UNDER its header with the vertical connecting separator.
      */
    def vertical(v: Boolean): Stepper[K] = new Stepper(p.copy(verticalFlag = v))

    /** Fired with the index of the step the reader asks for after a header click, after a two-way [[active]] or
      * [[value]] is written. One event carries the step, whichever binding moved.
      */
    def onActiveChange(f: Int => UI.Action)(using Frame): Stepper[K] =
        new Stepper(p.copy(activeSlot = activeSlot.hear(f), valueSlot = valueSlot.heardToo))

    private[uic] def render(using Frame): UI =
        val stepper = UI.mounted {
            for
                base <- UI.id
                // Unbound, the stepper keeps the active step itself, starting on the first.
                self <-
                    if valueSlot.binding.isEmpty then activeSlot.owned(0).map(s => new Stepper(this.p.copy(activeSlot = s)))
                    else Kyo.lift(this)
            yield self.wired(base, focusById)
        }
        // A binding one way that nothing hears: every click is painted away again.
        // The card names the event of the slot that is bound: in value mode onValueChange hears the value, and onActiveChange
        // hears the click as well.
        if valueSlot.binding.isDefined then
            Diagnostics.before(stepper)("Stepper")(Diagnostic.unheard(valueSlot, Stepper.activeName("onValueChange (or onActiveChange)")))
        else Diagnostics.before(stepper)("Stepper")(Diagnostic.unheard(activeSlot, Stepper.activeName("onActiveChange")))
        end if
    end render

    /** The rendered stepper: the steps and the binding read as ONE region. No `base` is the placeholder a golden render
      * and an SSG page show: no mount has run, so there is nothing to move focus with yet, but a horizontal stepper is
      * already the one tab stop its tablist will be, on the active header.
      */
    private def shown(base: Maybe[String], focus: String => UI.Action)(using Frame): UI =
        def drawn(ss: List[Step[K]], active: Int): UI =
            new Stepper(p.copy(stepList = ss)).body(active, base.fold(Nil)(b => ss.indices.toList.map(i => s"$b-$i")), focus)
        // Steps carry values the app compares by its own equality.
        given CanEqual[K, K] = CanEqual.derived
        valueSlot.binding match
            case Present(v) =>
                entries.and(v).render((ss, held) => drawn(ss, math.max(ss.indexWhere(_.p.value.contains(held)), 0)))
            case _ => entries.and(activeSlot.reading(0)).render((ss, at) => drawn(ss, at))
        end match
    end shown

    /** The seam the golden tests render, since a mount shows only its placeholder there. */
    private[uic] def wired(base: String, focus: String => UI.Action)(using Frame): UI = shown(Present(base), focus)

    /** The positions a header arrow may land on: a blocked step is not focusable, so it is stepped
      * over. Linear mode blocks everything past the current step.
      */
    private def navigableSteps(cur: Int): List[Int] =
        stepList.indices.toList.filterNot(i => stepList(i).p.disabled || (linearFlag && i > cur))

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
        val blocked = st.p.disabled || (linearFlag && i > cur)

        var header = button
            .cssClass("p-step-header")
            .jsProp("type", "button")
        if tablist then header = header.role("tab").aria("selected", isActive.toString)
        // A tablist is one tab stop, on the active header, from the first render: the arrows that
        // move between the headers come with the mount's ids, the single stop does not wait for them.
        if tablist && !blocked then header = header.tabIndex(if isActive then 0 else -1)
        if blocked then header = header.disabled(true).tabIndex(-1)
        else
            // A fixed stepper keeps every other header a stop that says it moves nothing.
            if !isActive then header = if fixed then header.aria("disabled", "true") else header.onClick(activate(i))
            if ids.isDefinedAt(i) then
                val axis = if tablist then ListNav.Orientation.Horizontal else ListNav.Orientation.Vertical
                header = header.id(ids(i)).onKeyDown(headerMove(ids, navigableSteps(cur), i, axis, focus))
            end if
        end if
        val titleSlot: UI = st.p.title match
            case TextValue.Const(t) => span.cssClass("p-step-title")(t): UI
            case TextValue.Dyn(s)   => span.cssClass("p-step-title")(s)
        val headerEl: UI = header(
            span.cssClass("p-step-number")((i + 1).toString),
            titleSlot
        )

        var stepEl = div.cssClass("p-step").role("presentation")
        if isActive then stepEl = stepEl.cssClass("p-step-active").aria("current", "step")
        if blocked then stepEl = stepEl.cssClass("p-disabled")
        // Horizontal stepList carry the separator inside the step row (Prime); the
        // vertical layout renders it inside the active panel instead.
        val separatorSlot: List[UI] =
            if !verticalFlag && i < stepList.length - 1 then List(span.cssClass("p-stepper-separator")) else Nil
        stepEl(UI.children(headerEl :: separatorSlot)*)
    end stepHeader

    private def horizontalBody(cur: Int, ids: List[String], focus: String => UI.Action)(using Frame): UI =
        val stepEls: List[UI] = stepList.zipWithIndex.map((st, i) => stepHeader(st, i, cur, tablist = true, ids, focus))

        val panel: List[UI] = stepList.lift(cur).toList.map { st =>
            animatedPanel(cur, horizontalPanel(st, animated = true))
        }

        var root = div.cssClass("p-stepper").cssClass("p-component")
        if linearFlag then root = root.cssClass("p-stepper-readonly")
        root(
            div.cssClass("p-steplist").role("tablist")(UI.children(stepEls)*),
            div.cssClass("p-steppanels")(UI.children(panel)*)
        )
    end horizontalBody

    /** The `div.p-steppanel` for the active horizontal step; `animated` adds the
      * crossfade transition hooks (only the live mount, never the placeholder).
      */
    private def horizontalPanel(st: Step[K], animated: Boolean)(using Frame): UI =
        var p = div.cssClass("p-steppanel").role("tabpanel")
        if animated then
            p = p.cssClass("p-uic-steppanel-anim").enterTransition("p-uic-enter-fade").leaveTransition("p-uic-leave-fade")
        p(UI.children(st.p.content)*)
    end horizontalPanel

    /** Wraps the active panel in a step-keyed mount so a step change animates it:
      * the outgoing panel's leave ghost fades out (`p-uic-leave-fade`) while the
      * incoming panel fades in (`p-uic-enter-fade`). A bare `enterTransition` would
      * never replay (`ref.render` keeps the horizontal panel's structural path
      * stable), so the mount's key (the active index) is what forces the teardown +
      * remount.
      */
    private def animatedPanel(cur: Int, animated: UI)(using Frame): UI =
        UI.mounted(animated).keyed(cur)

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
        p(div.cssClass("p-steppanel-content-wrapper")(UI.children(wrapperChildren)*))
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
                    val wrapperChildren = sep :+ div.cssClass("p-steppanel-content")(UI.children(st.p.content)*)
                    List(
                        animatedPanel(cur, verticalPanel(wrapperChildren, animated = true))
                    )

            var item = div.cssClass("p-stepitem")
            if isActive then item = item.cssClass("p-stepitem-active")
            item(UI.children(stepHeader(st, i, cur, tablist = false, ids, focus) :: panel)*)
        }

        var root = div.cssClass("p-stepper").cssClass("p-component")
        if linearFlag then root = root.cssClass("p-stepper-readonly")
        root(UI.children(items)*)
    end verticalBody

    /** Header click: ask for the step (value wins, matching render), written into a two-way binding and reported, then
      * `onActiveChange` with the index.
      */
    private def activate(i: Int)(using Frame): UI.Action =
        if valueSlot.binding.isDefined then
            val asked: UI.Action = stepList(i).p.value match
                case Present(v) => valueSlot.change(v)
                case Absent     => ()
            asked.andThen(activeSlot.hearer.fold[UI.Action](())(f => f(i)))
        else activeSlot.change(i)
    end activate
end Stepper

object Stepper:
    final private[uic] case class Props[K](
        // The steps the headers draw, resolved from `stepSources` by the render.
        stepList: List[Step[K]] = Nil,
        stepSources: List[StepLiveEntry[K]] = Nil,
        activeSlot: Slot[Int] = Slot(),
        valueSlot: Slot[K] = Slot[K](),
        linearFlag: Boolean = false,
        verticalFlag: Boolean = false
    )

    private def activeName(event: String) = Slot.Name("Stepper", "the active step", "a click", event)

    /** An empty stepper whose steps may carry values of type `K`.
      *
      * `K` is the type of the step VALUES, so it never needs spelling out: in a chain, `uic.Stepper()` takes it from the
      * first setter that mentions it (`value(ref)`, or a step's `value`), and a stepper driven by position alone
      * ([[Stepper.active]]) ends up a `Stepper[Nothing]`, the exact statement that no step carries a value. Nothing has
      * no values, so a step value or a `value` binding added to such a stepper later does not compile, rather than being
      * accepted at some placeholder type the stepper would never read. Spelling a placeholder (`Stepper[String]()`
      * beside `active`) is what to avoid: it admits values nothing binds.
      */
    def apply[K](): Stepper[K] = new Stepper[K](Props())
end Stepper
