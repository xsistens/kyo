package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** One panel of an [[Accordion]] — a hand-authored carrier: a `header` label, the
  * panel `content`, a stable `id` (the value key carried by the bound ref and the
  * `onToggle` payload), a `disabled` flag, and an optional `headerUI` slot that
  * wins over the plain header text.
  */
final case class AccordionPanel private[uic] (
    header: TextValue,
    content: UI,
    id: String,
    disabled: Boolean,
    headerUI: Maybe[UI]
)

object AccordionPanel:
    /** Construct a panel with a constant `header` title. The header slot also accepts
      * a reactive `Signal[String]` through the `Accordion.panel` builder overload.
      */
    def apply(
        header: String,
        content: UI,
        id: String,
        disabled: Boolean = false,
        headerUI: Maybe[UI] = Absent
    ): AccordionPanel = new AccordionPanel(TextValue.Const(header), content, id, disabled, headerUI)
end AccordionPanel

/** Accordion — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Accordion anatomy: `div.p-accordion.p-component` >
  * `div.p-accordionpanel[.p-accordionpanel-active][.p-disabled]` entries, each a
  * `button.p-accordionheader` (header slot + the chevron
  * `.p-accordionheader-toggle-icon` — up while active, down while collapsed,
  * Prime's glyphs) over, while active, `div.p-accordioncontent` >
  * `div.p-accordioncontent-wrapper` > `div.p-accordioncontent-content`), so the
  * extracted `@primeuix` accordion CSS applies verbatim.
  *
  * The open state binds two-way through `value`: a `SignalRef[String]` keeps at
  * most ONE panel open (Prime's default; `""` = all closed), a
  * `SignalRef[Set[String]]` is Prime's `multiple` mode — the ref TYPE selects the
  * mode, no separate flag. Header clicks write the ref before firing `onToggle`.
  * Without a bound ref the accordion renders fully collapsed and inert
  * (server-honest: no internal state).
  *
  * Collapse ANIMATION (mounted8): with a bound ref the whole body is built ONCE
  * inside a `UI.mounted`, so each panel's content container stays STABLE across
  * toggles — a whole-body `ref.render` would replace it and reset the grid-rows
  * transition. Per panel: the `.p-accordioncontent.p-uic-collapse` container is
  * id-stamped and ALWAYS present (the grid 0fr row hides a collapsed panel), and
  * the shell's `.p-accordionpanel-active` class plus the content's
  * `.p-uic-collapsed` class are toggled IN PLACE via `bindClassById` (a nested
  * mount driving the binds after publish, the [[Overlay]] `measureTrigger`
  * precedent). The header button must stay a DIRECT child of the shell for Prime's
  * `.p-accordionpanel[...] > .p-accordionheader` rules (active background, chevron
  * color, first/last radii), so it is NEVER wrapped in a reactive region: its
  * `aria-expanded` tracks the open state via the id-addressed `bindAttrById` channel
  * (in place, no wrapper), and only the chevron glyph swaps in a tiny reactive region
  * INSIDE the button (the toggle-icon rule is descendant, so that inner wrapper is
  * harmless). The mount's
  * `.placeholder` is today's un-animated whole-body `ref.render` (content
  * only-when-active) — the byte-identical SSR/golden fallback.
  */
final case class Accordion private (
    panelList: List[AccordionPanel] = Nil,
    singleRef: Maybe[SignalRef[String]] = Absent,
    multiRef: Maybe[SignalRef[Set[String]]] = Absent,
    onToggleF: Maybe[String => Any < Async] = Absent
) extends Node:
    type Self = Accordion

    /** Appends the given panels. */
    def panels(ps: AccordionPanel*): Accordion = copy(panelList = panelList ++ ps.toList)

    /** Appends a single panel (ergonomic builder — no need to construct
      * [[AccordionPanel]]).
      */
    def panel(header: String, id: String)(content: UI): Accordion =
        copy(panelList = panelList :+ new AccordionPanel(TextValue.Const(header), content, id, false, Absent))

    /** Reactive-title variant: the panel header tracks `header` — re-renders in
      * place on emission (e.g. a locale-driven `I18n.t` leaf).
      */
    def panel(header: Signal[String], id: String)(content: UI): Accordion =
        copy(panelList = panelList :+ new AccordionPanel(TextValue.Dyn(header), content, id, false, Absent))

    /** Binds the open panel two-way — SINGLE mode (Prime's default): at most one
      * panel open, the ref holds its id (`""` = all closed); clicking the open
      * header closes it.
      */
    def value(ref: SignalRef[String]): Accordion = copy(singleRef = Present(ref))

    /** Binds the open panels two-way — MULTIPLE mode (Prime's `multiple`): the ref
      * holds the set of open ids; header clicks toggle membership.
      */
    @targetName("valueMultiple")
    def value(ref: SignalRef[Set[String]]): Accordion = copy(multiRef = Present(ref))

    /** Fired with the panel id after a header click (after the value write). */
    def onToggle(f: String => Any < Async): Accordion = copy(onToggleF = Present(f))

    private[uic] def render(using Frame): UI =
        (singleRef, multiRef) match
            case (Present(r), _) =>
                animated(id => r.map(_ == id), r.render(v => body(if v.isEmpty then Set.empty else Set(v))))
            case (Absent, Present(r)) =>
                animated(id => r.map(_.contains(id)), r.render(body))
            case _ => body(Set.empty)

    /** The animated body: build the accordion ONCE in a mount so the per-panel
      * content containers stay stable, mint a shell id + content id per panel, and
      * drive their in-place class toggles from a nested mount (after publish).
      * `activeOf` maps a panel id to its open `Signal`; `placeholder` is today's
      * un-animated whole-body render.
      */
    private def animated(activeOf: String => Signal[Boolean], placeholder: => UI)(using Frame): UI =
        UI.mounted {
            for
                cmds <- UI.commands
                ids <- Kyo.foreach(panelList) { _ =>
                    for
                        shellId   <- cmds.freshId
                        contentId <- cmds.freshId
                        headerId  <- cmds.freshId
                    yield (shellId, contentId, headerId)
                }
                open <- currentOpen
            yield
                val pairs = panelList.zip(ids)
                fragment(
                    activeBinder(cmds, pairs, activeOf),
                    div.cssClass("p-accordion").cssClass("p-component")(
                        pairs.map { case (p, (shellId, contentId, headerId)) =>
                            toChild(renderAnimatedPanel(p, shellId, contentId, headerId, open.contains(p.id), activeOf(p.id)))
                        }*
                    )
                )
        }.placeholder(placeholder)

    /** The currently-open panel ids, read from whichever ref is bound (single mode
      * lifts the one id into a set; `""` = none).
      */
    private def currentOpen(using Frame): Set[String] < Sync =
        (singleRef, multiRef) match
            case (Present(r), _)      => r.get.map(v => if v.isEmpty then Set.empty else Set(v))
            case (Absent, Present(r)) => r.get
            case _                    => Set.empty[String]

    /** Nested mount (runs after the body is published) that installs, per panel, the
      * in-place binds — all WITHOUT replacing the shell or header, so Prime's
      * `.p-accordionpanel[...] > .p-accordionheader` child-combinator rules keep
      * matching: `.p-accordionpanel-active` on the shell tracks the open signal,
      * `.p-uic-collapsed` on the content tracks its negation, and `aria-expanded` on
      * the header button tracks the open state via the id-addressed attribute channel.
      */
    private def activeBinder(
        cmds: UI.Commands,
        pairs: List[(AccordionPanel, (String, String, String))],
        activeOf: String => Signal[Boolean]
    )(using Frame): UI =
        UI.mounted {
            Kyo.foreach(pairs) { case (p, (shellId, contentId, headerId)) =>
                val active = activeOf(p.id)
                cmds
                    .bindClassById(shellId, "p-accordionpanel-active", active)
                    .andThen(cmds.bindClassById(contentId, "p-uic-collapsed", active.map(!_)))
                    .andThen(cmds.bindAttrById(headerId, "aria-expanded", active.map(_.toString)))
            }.andThen(UI.empty)
        }.placeholder(UI.empty)

    private def renderAnimatedPanel(
        p: AccordionPanel,
        shellId: String,
        contentId: String,
        headerId: String,
        active0: Boolean,
        active: Signal[Boolean]
    )(using Frame): UI =
        var shell = div.cssClass("p-accordionpanel").id(shellId)
        if active0 then shell = shell.cssClass("p-accordionpanel-active")
        if p.disabled then shell = shell.cssClass("p-disabled")

        val headerSlot: UI = p.headerUI match
            case Present(u) => u
            case Absent =>
                p.header match
                    case TextValue.Const(t) => stringToUI(t)
                    case TextValue.Dyn(s)   => s.render(t => stringToUI(t))

        // The button stays a DIRECT child of the shell so Prime's `.p-accordionpanel[...]
        // > .p-accordionheader` rules (active background, chevron color, first/last radii)
        // match — a Reactive wrapper span between panel and header would break the `>`
        // combinator. So the two reactive header bits patch without wrapping the button:
        // `aria-expanded` via the id-addressed `bindAttrById` channel (see activeBinder),
        // baked here to the initial state; the chevron glyph via a tiny nested reactive
        // region INSIDE the button (the toggle-icon rule is descendant, so the inner
        // wrapper is harmless).
        var header = button
            .cssClass("p-accordionheader")
            .id(headerId)
            .jsProp("type", "button")
            .aria("expanded", active0.toString)
        if p.disabled then header = header.jsProp("disabled", "true")
        else if interactive then header = header.onClick(toggle(p.id))
        val headerEl: UI = header(
            toChild(headerSlot),
            toChild(active.render(a => GlyphSvg(if a then Icons.chevronUp else Icons.chevronDown, "p-accordionheader-toggle-icon")))
        )

        // Content is ALWAYS present — the grid 0fr row hides a collapsed panel.
        val contentEl: UI =
            var c = div.cssClass("p-accordioncontent").role("region").cssClass("p-uic-collapse").id(contentId)
            if !active0 then c = c.cssClass("p-uic-collapsed")
            c(
                div.cssClass("p-accordioncontent-wrapper")(
                    div.cssClass("p-accordioncontent-content")(toChild(p.content))
                )
            )
        end contentEl

        shell(toChild(headerEl), toChild(contentEl))
    end renderAnimatedPanel

    private def body(open: Set[String])(using Frame): UI =
        div.cssClass("p-accordion").cssClass("p-component")(
            panelList.map(p => toChild(renderPanel(p, open.contains(p.id))))*
        )

    private def renderPanel(p: AccordionPanel, active: Boolean)(using Frame): UI =
        var shell = div.cssClass("p-accordionpanel")
        if active then shell = shell.cssClass("p-accordionpanel-active")
        if p.disabled then shell = shell.cssClass("p-disabled")

        val headerSlot: UI = p.headerUI match
            case Present(u) => u
            case Absent =>
                p.header match
                    case TextValue.Const(t) => stringToUI(t)
                    case TextValue.Dyn(s)   => s.render(t => stringToUI(t))

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
    private def toggle(id: String)(using Frame): Any < Async =
        val write: Any < Async = (singleRef, multiRef) match
            case (Present(ref), _) => ref.getAndUpdate(cur => if cur == id then "" else id)
            case (Absent, Present(ref)) =>
                ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case _ => ()
        val fire: Any < Async = onToggleF match
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
    def apply(): Accordion = new Accordion()
