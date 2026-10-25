package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** One panel of an [[Accordion]], a hand-authored carrier: a `header` label, the
  * panel `content`, a stable `id` of the app's own type `K` (what the bound ref holds
  * and `onToggle` reports), a `disabled` flag, and an optional `headerUI` slot that
  * wins over the plain header text.
  */
final case class AccordionPanel[K] private[uic] (
    header: TextValue,
    content: UI,
    id: K,
    disabled: Boolean,
    headerUI: Maybe[UI]
) derives CanEqual

object AccordionPanel:
    /** Construct a panel with a constant `header` title. The `Accordion.panel` builder takes
      * the same slot as a `String | Signal[String]` union, for a reactive title.
      */
    def apply[K](
        header: String,
        content: UI,
        id: K,
        disabled: Boolean = false,
        headerUI: Maybe[UI] = Absent
    ): AccordionPanel[K] = new AccordionPanel(TextValue.Const(header), content, id, disabled, headerUI)
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
    ): AccordionPanel[K] = new AccordionPanel(ReactiveValue(header), content, id, disabled, headerUI)
end AccordionPanelScope

object AccordionPanelScope:
    // The scope holds nothing of its own type, so one instance serves every `K`.
    private val scope = new AccordionPanelScope[Any]

    private[uic] def resolve[K](entries: Seq[AccordionPanelEntry[K]]): List[AccordionPanel[K]] =
        Entry.resolve(scope.asInstanceOf[AccordionPanelScope[K]], entries)

    private[uic] def reading[K](entries: Seq[AccordionPanelLiveEntry[K]])(using CanEqual[K, K], Frame): Reading[List[AccordionPanel[K]]] =
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
  * The open state binds two-way through `value`, as panel ids of the app's own
  * type `K`: a `SignalRef[Maybe[K]]` keeps at most ONE panel open (Prime's default;
  * `Absent` = all closed), a `SignalRef[Set[K]]` is Prime's `multiple` mode. The
  * ref TYPE selects the mode, no separate flag. Header clicks write the ref before firing `onToggle`.
  * Without a bound ref the accordion renders fully collapsed and inert
  * (server-honest: no internal state).
  *
  * Keyboard (WAI-ARIA accordion): the headers are buttons, so each is its own tab stop and
  * Enter or Space toggles the focused one natively. ArrowDown/ArrowUp move focus to the next
  * and previous enabled header, cycling at the ends, and Home/End reach the first and last;
  * focus moves for real, by id, since a header that looked focused without being focused would
  * take Enter nowhere. The arrows work in the bound-ref render only, the same place the clicks
  * do: an accordion with no ref is inert by design.
  *
  * Collapse ANIMATION: with a bound ref the whole body is built ONCE
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
final case class Accordion[K] private (
    // The panels `body` draws, resolved from `panelSources` by the render.
    panelList: List[AccordionPanel[K]] = Nil,
    panelSources: List[AccordionPanelLiveEntry[K]] = Nil,
    singleRef: Maybe[SignalRef[Maybe[K]]] = Absent,
    multiRef: Maybe[SignalRef[Set[K]]] = Absent,
    onToggleF: Maybe[K => UI.Action] = Absent
) extends Node:
    type Self = Accordion[K]

    // Panel ids compare the way the sets that hold them already compare them, by `equals`.
    private given CanEqual[K, K] = CanEqual.derived

    /** Appends the given panels, each built directly or from `_` (see [[AccordionPanelScope]]), or a signal of panels that
      * change while the accordion is shown (see [[LiveEntry]]). A panel whose id stays in the signal keeps its content.
      * The panel's DOM ids and its row in a live list are named by the id's text, so two panels need ids that print
      * differently, as two tabs of [[Tabs]] do.
      */
    def panels(ps: AccordionPanelLiveEntry[K]*): Accordion[K] = copy(panelSources = panelSources ++ ps)

    /** Appends a single panel (ergonomic builder: no need to construct
      * [[AccordionPanel]]). A `Signal[String]` header re-renders the panel title in place on
      * emission (e.g. a locale-driven `I18n.t` leaf).
      */
    def panel(header: String | Signal[String], id: K)(content: UI): Accordion[K] =
        copy(panelSources = panelSources :+ new AccordionPanel(ReactiveValue(header), content, id, false, Absent))

    /** Binds the open panel two-way, SINGLE mode (Prime's default): at most one
      * panel open, the ref holds its id (`Absent` = all closed); clicking the open
      * header closes it.
      */
    def value(ref: SignalRef[Maybe[K]]): Accordion[K] = copy(singleRef = Present(ref))

    /** Binds the open panels two-way, MULTIPLE mode (Prime's `multiple`): the ref
      * holds the set of open ids; header clicks toggle membership.
      */
    @targetName("valueMultiple")
    def value(ref: SignalRef[Set[K]]): Accordion[K] = copy(multiRef = Present(ref))

    /** Fired with the panel id after a header click (after the value write). */
    def onToggle(f: K => UI.Action)(using Frame): Accordion[K] = copy(onToggleF = Present(Callback.of(f)))

    /** The panels as ONE reading: fixed unless one of them was handed over as a signal. */
    private[uic] def entries(using Frame): Reading[List[AccordionPanel[K]]] = AccordionPanelScope.reading(panelSources)

    /** The open panel ids, read from whichever ref is bound (single mode lifts the one id into a set); none, and fixed,
      * without a ref.
      */
    private def openIds(using Frame): Reading[Set[K]] =
        (singleRef, multiRef) match
            case (Present(r), _)      => Reading.Live(r.map(_.toList.toSet))
            case (Absent, Present(r)) => Reading.Live(r)
            case _                    => Reading.Fixed(Set.empty)

    private[uic] def render(using Frame): UI =
        val still = entries.and(openIds).render((ps, open) => copy(panelList = ps).body(open))
        if interactive then
            UI.mounted {
                UI.commands.map(cmds => cmds.freshId.map(base => wired(base, id => cmds.focusId(id))))
            }.placeholder(still)
        else still
        end if
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
            case Reading.Fixed(ps) => ps.map(row)
            case Reading.Live(sig) =>
                given CanEqual[Chunk[AccordionPanel[K]], Chunk[AccordionPanel[K]]] = CanEqual.derived
                List(sig.map(Chunk.from(_)).foreachKeyed(_.id.toString)(row))
        div.cssClass("p-accordion").cssClass("p-component")(rows.map(toChild)*)
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
    private def current(using Frame): List[AccordionPanel[K]] < Sync =
        entries match
            case Reading.Fixed(ps) => ps
            case Reading.Live(sig) => sig.current

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
            val navigable = ps.zipWithIndex.collect { case (p, i) if !p.disabled => i }
            val at        = ps.indexWhere(_.id == self)
            // Prime's accordion cycles, so ArrowDown on the last header comes back to the first.
            val moved: UI.Action = ListNav.onKey(navigable, at, e.key, wrap = true) match
                case Present(step) if step.focus != at && ps.isDefinedAt(step.focus) => focus(headerId(base, ps(step.focus).id))
                case _                                                               => ()
            moved
        }

    private def renderAnimatedPanel(p: AccordionPanel[K], base: String, open: Signal[Set[K]], focus: String => UI.Action)(using
        Frame
    ): UI =
        val active = open.map(_.contains(p.id))
        var shell  = div.cssClass("p-accordionpanel").id(shellId(base, p.id)).cssClass("p-accordionpanel-active", active)
        if p.disabled then shell = shell.cssClass("p-disabled")

        val headerSlot: UI = p.headerUI match
            case Present(u) => u
            case Absent     =>
                p.header match
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
            .id(headerId(base, p.id))
            .jsProp("type", "button")
            .aria("expanded", active.map(_.toString))
        if p.disabled then header = header.jsProp("disabled", "true")
        else
            if interactive then header = header.onClick(toggle(p.id))
            header = header.preventScrollKeys.onKeyDown(headerMove(base, p.id, focus))
        end if
        val headerEl: UI = header(
            toChild(headerSlot),
            toChild(active.render(a => GlyphSvg(if a then Icons.chevronUp else Icons.chevronDown, "p-accordionheader-toggle-icon")))
        )

        // Content is ALWAYS present: the grid 0fr row hides a collapsed panel.
        val contentEl: UI =
            div.cssClass("p-accordioncontent").role("region").cssClass("p-uic-collapse").id(contentId(base, p.id))
                .cssClass("p-uic-collapsed", active.map(!_))(
                    div.cssClass("p-accordioncontent-wrapper")(
                        div.cssClass("p-accordioncontent-content")(toChild(p.content))
                    )
                )

        shell(toChild(headerEl), toChild(contentEl))
    end renderAnimatedPanel

    private def body(open: Set[K])(using Frame): UI =
        div.cssClass("p-accordion").cssClass("p-component")(
            panelList.map(p => toChild(renderPanel(p, open.contains(p.id))))*
        )

    private def renderPanel(p: AccordionPanel[K], active: Boolean)(using Frame): UI =
        var shell = div.cssClass("p-accordionpanel")
        if active then shell = shell.cssClass("p-accordionpanel-active")
        if p.disabled then shell = shell.cssClass("p-disabled")

        val headerSlot: UI = p.headerUI match
            case Present(u) => u
            case Absent     =>
                p.header match
                    case TextValue.Const(t) => stringToUI(t)
                    case TextValue.Dyn(s)   => signalStringToUI(s)

        var header = button
            .cssClass("p-accordionheader")
            .jsProp("type", "button")
            .aria("expanded", active.toString)
        if p.disabled then header = header.jsProp("disabled", "true")
        else if interactive then header = header.onClick(toggle(p.id))
        val headerEl: UI = header(
            toChild(headerSlot),
            toChild(GlyphSvg(if active then Icons.chevronUp else Icons.chevronDown, "p-accordionheader-toggle-icon"))
        )

        val contentEl: List[UI] =
            if active then
                List(
                    div.cssClass("p-accordioncontent").role("region")(
                        div.cssClass("p-accordioncontent-wrapper")(
                            div.cssClass("p-accordioncontent-content")(toChild(p.content))
                        )
                    )
                )
            else Nil

        shell((headerEl :: contentEl).map(toChild)*)
    end renderPanel

    private def interactive: Boolean = singleRef.isDefined || multiRef.isDefined

    /** A header click writes the bound value ref (per the mode the ref type
      * selected), then fires `onToggle`.
      */
    private def toggle(id: K)(using Frame): UI.Action =
        val write: UI.Action = (singleRef, multiRef) match
            case (Present(ref), _)      => ref.getAndUpdate(cur => if cur.contains(id) then Absent else Present(id))
            case (Absent, Present(ref)) =>
                ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case _ => ()
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
    def apply[K](): Accordion[K] = new Accordion[K]()
