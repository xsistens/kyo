package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** One panel of an [[Accordion]], a hand-authored carrier: a `header` label, the
  * panel `content`, a stable `id` of the app's own type `K` (what the bound ref holds
  * and `onToggle` reports), a `disabled` flag, and an optional `headerUI` slot that
  * wins over the plain header text.
  */
final class AccordionPanel[K] private[uic] (private[uic] val s: AccordionPanel.State[K]) extends Stated derives CanEqual

object AccordionPanel:
    final private[uic] case class State[K](
        header: TextValue,
        content: UI,
        id: K,
        disabled: Boolean,
        headerUI: Maybe[UI]
    )

    /** Construct a panel with a constant `header` title. The `Accordion.panel` builder takes
      * the same slot as a `String | Signal[String]` union, for a reactive title.
      */
    def apply[K](
        header: String,
        content: UI,
        id: K,
        disabled: Boolean = false,
        headerUI: Maybe[UI] = Absent
    ): AccordionPanel[K] = new AccordionPanel(State(TextValue.Const(header), content, id, disabled, headerUI))
end AccordionPanel

/** An Accordion panel, built directly or as `_.panel(...)` (see [[AccordionPanelScope]]). */
type AccordionPanelEntry[K] = Entry[AccordionPanel[K], AccordionPanelScope[K]]

/** An [[AccordionPanelEntry]] or a signal of panels, for an accordion whose panels change while it is shown (see
  * [[LiveEntry]]).
  */
type AccordionPanelLiveEntry[K] = LiveEntry[AccordionPanel[K], AccordionPanelScope[K]]

/** What `_` stands for in [[Accordion.panels]]: `_.panel("Shipping", shippingForm, "shipping")`. The header takes a `Signal[String]`
  * too, as [[Accordion.panel]] does.
  */
final class AccordionPanelScope[K] private ():
    /** A panel keyed by `id`, as [[AccordionPanel.apply]], with a constant or reactive header. */
    def panel(
        header: String | Signal[String],
        content: UI,
        id: K,
        disabled: Boolean = false,
        headerUI: Maybe[UI] = Absent
    ): AccordionPanel[K] = new AccordionPanel(AccordionPanel.State(ReactiveValue(header), content, id, disabled, headerUI))
end AccordionPanelScope

object AccordionPanelScope:
    // The scope holds nothing of its own type, so one instance serves every `K`.
    private val scope = new AccordionPanelScope[Any]

    private[uic] def resolve[K](entries: Seq[AccordionPanelEntry[K]]): List[AccordionPanel[K]] =
        Entry.resolve(scope.asInstanceOf[AccordionPanelScope[K]], entries)

    private[uic] def reading[K](entries: Seq[AccordionPanelLiveEntry[K]])(using
        CanEqual[K, K],
        Frame
    ): ReactiveValue[List[AccordionPanel[K]]] =
        Entry.reading(scope.asInstanceOf[AccordionPanelScope[K]], entries)
end AccordionPanelScope

/** Accordion: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Accordion anatomy: `div.p-accordion.p-component` >
  * `div.p-accordionpanel[.p-accordionpanel-active][.p-disabled]` entries, each a
  * `button.p-accordionheader` (header slot + the chevron
  * `.p-accordionheader-toggle-icon`: up while active, down while collapsed,
  * Prime's glyphs) over, while active, `div.p-accordioncontent` >
  * `div.p-accordioncontent-wrapper` > `div.p-accordioncontent-content`), so the
  * extracted `@primeuix` accordion CSS applies verbatim.
  *
  * The open state binds through `value`, as panel ids of the app's own type `K`:
  * a `Maybe[K]` keeps at most ONE panel open (Prime's default; `Absent` = all
  * closed), a `Set[K]` is Prime's `multiple` mode and gives a [[MultipleAccordion]].
  * The value TYPE selects the mode, no separate flag. Each takes a constant, a
  * signal or a ref: a click writes a ref, then reports to `onValueChange`, then
  * fires `onToggle`; a constant nobody hears renders the panels fixed and the
  * headers inert. Unbound, the accordion keeps the open panel itself, in single
  * mode, starting with all closed.
  *
  * Keyboard (WAI-ARIA accordion): the headers are buttons, so each is its own tab stop and
  * Enter or Space toggles the focused one natively. ArrowDown/ArrowUp move focus to the next
  * and previous enabled header, cycling at the ends, and Home/End reach the first and last;
  * focus moves for real, by id, since a header that looked focused without being focused would
  * take Enter nowhere. The arrows work in the mounted render, the same place the clicks do.
  *
  * Collapse ANIMATION: in the mounted render the whole body is built ONCE
  * inside a `UI.mounted`, so each panel's content container stays STABLE across
  * toggles; a whole-body `ref.render` would replace it and reset the grid-rows
  * transition. Per panel: the `.p-accordioncontent.p-uic-collapse` container is
  * ALWAYS present (the grid 0fr row hides a collapsed panel), and the shell's
  * `.p-accordionpanel-active` class plus the content's `.p-uic-collapsed` class
  * are reactive classes, toggled IN PLACE on the element. The header button must
  * stay a DIRECT child of the shell for Prime's `.p-accordionpanel[...] >
  * .p-accordionheader` rules (active background, chevron color, first/last
  * radii), so it is NEVER wrapped in a reactive region: its `aria-expanded` is a
  * reactive attribute, patched in place, and only the chevron glyph swaps in a
  * tiny reactive region INSIDE the button (the toggle-icon rule is descendant,
  * so that inner wrapper is harmless). The mount's `.placeholder` is the
  * un-animated whole-body render (content only-when-active), the SSR/golden
  * fallback.
  *
  * Panels from a signal (see [[LiveEntry]]) are a list keyed by panel id inside
  * the one body: a panel the signal adds is a new row, and one that stays keeps
  * its content and its transition. Every row shares the list's one region, so
  * the first-child and last-child radii still see one run of panels.
  */
final class Accordion[K] private[uic] (private[uic] val s: Accordion.State[K]) extends Node, Stated:
    import s.*

    type Self = Accordion[K]

    // Panel ids compare the way the sets that hold them already compare them, by `equals`.
    private given CanEqual[K, K] = CanEqual.derived

    /** Appends the given panels, each built directly or from `_` (see [[AccordionPanelScope]]), or a signal of panels that
      * change while the accordion is shown (see [[LiveEntry]]). A panel whose id stays in the signal keeps its content.
      * The panel's DOM ids and its row in a live list are named by the id's text, so two panels need ids that print
      * differently, as two tabs of [[Tabs]] do.
      */
    def panels(ps: AccordionPanelLiveEntry[K]*): Accordion[K] = new Accordion(s.copy(panelSources = panelSources ++ ps))

    /** Appends a single panel (ergonomic builder: no need to construct
      * [[AccordionPanel]]). A `Signal[String]` header re-renders the panel title in place on
      * emission (e.g. a locale-driven `I18n.t` leaf).
      */
    def panel(header: String | Signal[String], id: K)(content: UI): Accordion[K] =
        new Accordion(this.s.copy(panelSources =
            panelSources :+ new AccordionPanel(AccordionPanel.State(ReactiveValue(header), content, id, false, Absent))
        ))

    /** Binds the open panel, SINGLE mode (Prime's default): at most one panel open, the value holding its id (`Absent` =
      * all closed); clicking the open header closes it. A `SignalRef` binds two way and [[onValueChange]] hears every
      * click; a plain `Signal` binds one way and the accordion only reports; a constant nobody hears is fixed, the headers
      * reachable and inert. Unbound, the accordion keeps the open panel itself, starting with all closed. A signal nothing
      * hears is reported in a card; [[onToggle]] hears it too.
      */
    def value(v: Maybe[K] | Signal[Maybe[K]]): Accordion[K] =
        new Accordion(s.copy(singleSlot = singleSlot.bind(v), multiSlot = multiSlot.copy(binding = Absent)))

    /** The same accordion in MULTIPLE mode (Prime's `multiple`): a [[MultipleAccordion]] over the set of open ids, which
      * header clicks toggle membership of. Bound like the single value. The single value and its event do not carry over.
      */
    @targetName("valueMultiple")
    def value(v: Set[K] | Signal[Set[K]]): MultipleAccordion[K] =
        MultipleAccordion(new Accordion(s.copy(singleSlot = Slot[Maybe[K]](), multiSlot = multiSlot.bind(v))))

    /** Fired with the panel now open (`Absent` once all are closed) after a header click, after a two-way [[value]] is
      * written and before [[onToggle]].
      */
    def onValueChange(f: Maybe[K] => UI.Action)(using Frame): Accordion[K] = new Accordion(s.copy(singleSlot = singleSlot.hear(f)))

    /** Package-internal: the event [[MultipleAccordion]] sets. */
    private[uic] def withValuesChange(f: Set[K] => UI.Action)(using Frame): Accordion[K] =
        new Accordion(s.copy(multiSlot = multiSlot.hear(f)))

    /** A constant nobody hears is fixed: the open panels stay, and every header is inert. */
    private def fixed: Boolean = singleSlot.fixed || multiSlot.fixed

    /** Fired with the panel id after a header click (after the value write). */
    def onToggle(f: K => UI.Action)(using Frame): Accordion[K] =
        new Accordion(s.copy(onToggleF = Present(Callback.of(f)), singleSlot = singleSlot.heardToo, multiSlot = multiSlot.heardToo))

    /** The panels as ONE reading: fixed unless one of them was handed over as a signal. */
    private[uic] def entries(using Frame): ReactiveValue[List[AccordionPanel[K]]] = AccordionPanelScope.reading(panelSources)

    /** The open panel ids, read from whichever value is bound (single mode lifts the one id into a set); none before the
      * mount allocates the accordion's own.
      */
    private def openIds(using Frame): ReactiveValue[Set[K]] =
        given CanEqual[Set[K], Set[K]] = CanEqual.derived
        (singleSlot.binding, multiSlot.binding) match
            case (Present(v), _) => v.map(_.toList.toSet)
            case (_, Present(v)) => v
            case _               => ReactiveValue.Const(Set.empty)
        end match
    end openIds

    private[uic] def render(using Frame): UI =
        val still = entries.and(openIds).render((ps, open) => new Accordion(s.copy(panelList = ps)).body(open))
        val drawn =
            if !interactive then still
            else
                UI.mounted {
                    for
                        cmds <- UI.commands
                        base <- cmds.freshId
                        // Unbound, the accordion keeps the open panel itself, in single mode, starting with all closed.
                        self <-
                            if multiSlot.binding.isEmpty then
                                singleSlot.owned(Maybe.empty[K]).map(s => new Accordion(this.s.copy(singleSlot = s)))
                            else Kyo.lift(this)
                    yield self.wired(base, id => cmds.focusId(id))
                }.placeholder(still)
        // A value bound one way that nothing hears: every click is painted away again.
        Diagnostics.before(drawn)("Accordion")(Diagnostic(singleSlot.deaf || multiSlot.deaf, Accordion.valueName.advice))
    end render

    /** The animated body the mount publishes, and the seam the golden tests render: built ONCE, each panel toggling its
      * own classes and `aria-expanded` in place, so its content container stays stable and the grid-rows transition
      * plays. Every id derives from `base` and the panel's own id, so a panel a signal adds later has its ids without a
      * mint of its own.
      */
    private[uic] def wired(base: String, focus: String => UI.Action)(using Frame): UI =
        val open                          = openIds.signal
        def row(p: AccordionPanel[K]): UI = renderAnimatedPanel(p, base, open, focus)
        val rows: List[UI]                = entries match
            case ReactiveValue.Const(ps) => ps.map(row)
            case ReactiveValue.Dyn(sig)  =>
                given CanEqual[Chunk[AccordionPanel[K]], Chunk[AccordionPanel[K]]] = CanEqual.derived
                List(sig.map(Chunk.from(_)).foreachKeyed(_.s.id.toString)(row))
        div.cssClass("p-accordion").cssClass("p-component")(UI.children(rows)*)
    end wired

    /** A panel id as it appears in a DOM id: its text behind its length, so the ids below stay distinct for any two
      * panels whose ids print differently ("a" plus "-content" is not the panel "a-content").
      */
    private def idPart(id: K): String =
        val s = id.toString
        s"${s.length}-$s"
    end idPart

    private def shellId(base: String, id: K): String   = s"$base-${idPart(id)}"
    private def contentId(base: String, id: K): String = s"$base-${idPart(id)}-content"
    private def headerId(base: String, id: K): String  = s"$base-${idPart(id)}-header"

    /** The panels as they are now: what a key press moves among, read when the key arrives so a live list counts the
      * panels it holds at that moment.
      */
    private def current(using Frame): List[AccordionPanel[K]] < Sync = entries.current

    /** ArrowDown/ArrowUp/Home/End move focus between the headers, which is the ARIA accordion
      * pattern and what Prime does: the headers are buttons and each is its own tab stop, so
      * the arrows are the fast way past a long accordion rather than the only way in.
      *
      * Focus moves for real, by id, rather than through a highlight the way a listbox roves
      * one: the thing being focused is a button, and a button that looks focused without being
      * focused would take Enter nowhere. Enter and Space are left alone for exactly that
      * reason, since the browser already presses the focused header with them.
      */
    private def headerMove(base: String, self: K, focus: String => UI.Action)(using Frame): KeyboardEvent => UI.Action = e =>
        current.map { ps =>
            // A disabled header is not focusable, so ArrowDown steps over it.
            val navigable = ps.zipWithIndex.collect { case (p, i) if !p.s.disabled => i }
            val at        = ps.indexWhere(_.s.id == self)
            // Prime's accordion cycles, so ArrowDown on the last header comes back to the first.
            val moved: UI.Action = ListNav.onKey(navigable, at, e.key, wrap = true) match
                case Present(step) if step.focus != at && ps.isDefinedAt(step.focus) => focus(headerId(base, ps(step.focus).s.id))
                case _                                                               => ()
            moved
        }

    private def renderAnimatedPanel(p: AccordionPanel[K], base: String, open: Signal[Set[K]], focus: String => UI.Action)(using
        Frame
    ): UI =
        val active = open.map(_.contains(p.s.id))
        var shell  = div.cssClass("p-accordionpanel").id(shellId(base, p.s.id)).cssClass("p-accordionpanel-active", active)
        if p.s.disabled then shell = shell.cssClass("p-disabled")

        val headerSlot: UI = p.s.headerUI match
            case Present(u) => u
            case Absent     =>
                p.s.header match
                    case TextValue.Const(t) => stringToUI(t)
                    case TextValue.Dyn(s)   => signalStringToUI(s)

        // The button stays a DIRECT child of the shell so Prime's `.p-accordionpanel[...]
        // > .p-accordionheader` rules (active background, chevron color, first/last radii)
        // match; a Reactive wrapper span between panel and header would break the `>`
        // combinator. So `aria-expanded` is a reactive attribute patched in place, and the
        // chevron glyph swaps in a tiny nested reactive region INSIDE the button (the
        // toggle-icon rule is descendant, so the inner wrapper is harmless).
        var header = button
            .cssClass("p-accordionheader")
            .id(headerId(base, p.s.id))
            .jsProp("type", "button")
            .aria("expanded", active.map(_.toString))
        if p.s.disabled then header = header.jsProp("disabled", "true")
        else
            if interactive then header = header.onClick(toggle(p.s.id)) else header = header.aria("disabled", "true")
            header = header.preventScrollKeys.onKeyDown(headerMove(base, p.s.id, focus))
        end if
        val headerEl: UI = header(
            headerSlot,
            active.render(a => GlyphSvg(if a then Icons.chevronUp else Icons.chevronDown, "p-accordionheader-toggle-icon"))
        )

        // Content is ALWAYS present: the grid 0fr row hides a collapsed panel.
        val contentEl: UI =
            div.cssClass("p-accordioncontent").role("region").cssClass("p-uic-collapse").id(contentId(base, p.s.id))
                .cssClass("p-uic-collapsed", active.map(!_))(
                    div.cssClass("p-accordioncontent-wrapper")(
                        div.cssClass("p-accordioncontent-content")(p.s.content)
                    )
                )

        shell(headerEl, contentEl)
    end renderAnimatedPanel

    private def body(open: Set[K])(using Frame): UI =
        div.cssClass("p-accordion").cssClass("p-component")(
            UI.children(panelList.map(p => renderPanel(p, open.contains(p.s.id))))*
        )

    private def renderPanel(p: AccordionPanel[K], active: Boolean)(using Frame): UI =
        var shell = div.cssClass("p-accordionpanel")
        if active then shell = shell.cssClass("p-accordionpanel-active")
        if p.s.disabled then shell = shell.cssClass("p-disabled")

        val headerSlot: UI = p.s.headerUI match
            case Present(u) => u
            case Absent     =>
                p.s.header match
                    case TextValue.Const(t) => stringToUI(t)
                    case TextValue.Dyn(s)   => signalStringToUI(s)

        var header = button
            .cssClass("p-accordionheader")
            .jsProp("type", "button")
            .aria("expanded", active.toString)
        if p.s.disabled then header = header.jsProp("disabled", "true")
        else if interactive then header = header.onClick(toggle(p.s.id))
        else header = header.aria("disabled", "true")
        val headerEl: UI = header(
            headerSlot,
            GlyphSvg(if active then Icons.chevronUp else Icons.chevronDown, "p-accordionheader-toggle-icon")
        )

        val contentEl: List[UI] =
            if active then
                List(
                    div.cssClass("p-accordioncontent").role("region")(
                        div.cssClass("p-accordioncontent-wrapper")(
                            div.cssClass("p-accordioncontent-content")(p.s.content)
                        )
                    )
                )
            else Nil

        shell(UI.children(headerEl :: contentEl)*)
    end renderPanel

    private def interactive: Boolean = !fixed

    /** A header click asks for the panel opened or closed (per the mode the bound value selected): written into a two-way
      * value, reported to the mode's value event, then `onToggle`.
      */
    private def toggle(id: K)(using Frame): UI.Action =
        val write: UI.Action =
            if singleSlot.binding.isDefined then singleSlot.update(cur => if cur.contains(id) then Absent else Present(id))
            else multiSlot.update(cur => if cur.contains(id) then cur - id else cur + id)
        val fire: UI.Action = onToggleF match
            case Present(f) => f(id)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end toggle
end Accordion

object Accordion:
    final private[uic] case class State[K](
        // The panels `body` draws, resolved from `panelSources` by the render.
        panelList: List[AccordionPanel[K]] = Nil,
        panelSources: List[AccordionPanelLiveEntry[K]] = Nil,
        singleSlot: Slot[Maybe[K]] = Slot[Maybe[K]](),
        multiSlot: Slot[Set[K]] = Slot[Set[K]](),
        onToggleF: Maybe[K => UI.Action] = Absent
    )

    private val valueName        = Slot.Name("Accordion", "the open panels", "a click", "onValueChange")
    def apply[K](): Accordion[K] = new Accordion[K](State())
end Accordion

/** An [[Accordion]] in MULTIPLE mode, from [[Accordion.value]] over a set: several panels may be open, and header clicks
  * toggle membership. It has exactly one value event, over the set, so no single-value callback can be set that never
  * fires; every other setter is the accordion's own.
  */
final case class MultipleAccordion[K] private[uic] (private[uic] val accordion: Accordion[K]) extends Node:
    type Self = MultipleAccordion[K]

    private def on(a: Accordion[K]): MultipleAccordion[K] = copy(accordion = a)

    /** Fired with the panels now open after a header click, after a two-way value is written and before [[onToggle]]. */
    def onValueChange(f: Set[K] => UI.Action)(using Frame): MultipleAccordion[K] = on(accordion.withValuesChange(f))

    /** Fired with the panel id after a header click, as [[Accordion.onToggle]]. */
    def onToggle(f: K => UI.Action)(using Frame): MultipleAccordion[K] = on(accordion.onToggle(f))

    /** Appends panels, as [[Accordion.panels]]. */
    def panels(ps: AccordionPanelLiveEntry[K]*): MultipleAccordion[K] = on(accordion.panels(ps*))

    /** Appends a single panel, as [[Accordion.panel]]. */
    def panel(header: String | Signal[String], id: K)(content: UI): MultipleAccordion[K] = on(accordion.panel(header, id)(content))

    private[uic] def render(using Frame): UI = accordion.render
end MultipleAccordion
