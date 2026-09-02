package kyo.uic

import kyo.*
import kyo.UI.*

/** One tab of a [[Tabs]] — a hand-authored carrier: a `text` label, the
  * already-rendered `content` shown when the tab is active, a stable `id` (the
  * value the `selected` ref holds and the `onTabSelect` payload), an optional
  * leading `icon` glyph, an optional `additionalText` count badge (kyo
  * extension — `span.p-uic-tab-count`), and a `disabled` flag (non-clickable).
  */
final case class Tab private[uic] (
    text: TextValue,
    content: UI,
    id: String,
    icon: Maybe[IconGlyph],
    additionalText: Maybe[String],
    disabled: Boolean
)

object Tab:
    /** Construct a tab with a constant `text` label. The `Tabs.tab` builder takes the same
      * slot as a `String | Signal[String]` union, for a reactive label.
      */
    def apply(
        text: String,
        content: UI,
        id: String,
        icon: Maybe[IconGlyph] = Absent,
        additionalText: Maybe[String] = Absent,
        disabled: Boolean = false
    ): Tab = new Tab(TextValue.Const(text), content, id, icon, additionalText, disabled)
end Tab

/** Tabs — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's compound
  * Tabs anatomy: `div.p-tabs.p-component` > `div.p-tablist` >
  * `div.p-tablist-content.p-tablist-viewport` > `div.p-tablist-tab-list[role=tablist]`
  * of `button.p-tab[role=tab]` headers (the active one carrying `.p-tab-active`
  * plus the `span.p-tablist-active-bar` ink bar) above `div.p-tabpanels` >
  * `div.p-tabpanel[role=tabpanel]` — only the active panel renders), so the
  * extracted `@primeuix` tabs CSS applies verbatim. Prime positions the ink bar
  * with JS measuring; here it sits inside the active tab and spans it via a
  * remainder rule, which is visually identical without a measuring runtime.
  *
  * The active tab is one `selected` slot taking `String | Signal[String]`, so the
  * selection may live anywhere: a constant, a signal DERIVED from something else
  * (a route, a normalized cache record, a parent), or a writable `SignalRef` for
  * the two-way case. Only the last writes back — see [[Tabs.selected]]. With
  * nothing bound, the first tab is shown. Tabs are real `<button>`s, so keyboard
  * activation (Enter/Space) is native; the tablist is ONE tab stop and the arrows
  * move between headers inside it, which is the ARIA tablist pattern and the only
  * thing that makes an inactive header reachable at all once the roving tabindex
  * has taken it out of the Tab order.
  */
final case class Tabs private (
    tabList: List[Tab] = Nil,
    selectedBinding: Maybe[ReactiveValue[String]] = Absent,
    onTabSelectF: Maybe[String => Any < Async] = Absent
) extends Node:
    type Self = Tabs

    /** Appends the given tabs. */
    def tabs(ts: Tab*): Tabs = copy(tabList = tabList ++ ts.toList)

    /** Appends a single tab from bare content children (ergonomic builder). */
    def tab(text: String | Signal[String], id: String)(content: UI*)(using Frame): Tabs =
        copy(tabList = tabList :+ new Tab(ReactiveValue(text), fragment(content*), id, Absent, Absent, false))

    /** The active tab's id. A `SignalRef` binds two way — clicking a tab writes its
      * id back before firing `onTabSelect`, which is the self-contained panel
      * switcher. Any other `Signal` binds ONE way: the strip renders what the signal
      * says and fires `onTabSelect`, and never writes, which is what a selection
      * owned elsewhere needs — a route is a `map` over the location and has no ref
      * to write into. A plain `String` is the same contract without a stream.
      *
      * Reading needs no ref at all (`Signal.render` serves every case); only the
      * write-back does. Demanding a `SignalRef` from every caller to serve the
      * callers that want write-back is the capability this slot used to require and
      * no longer does.
      *
      * The two-way choice is made on the RUNTIME class, so ascribing a ref as
      * `Signal[String]` does not opt out; pass `ref.readOnly` for that.
      */
    def selected(v: String | Signal[String]): Tabs = copy(selectedBinding = Present(ReactiveValue(v)))

    def onTabSelect(f: String => Any < Async): Tabs = copy(onTabSelectF = Present(f))

    private[uic] def render(using Frame): UI =
        UI.mounted {
            UI.commands.map { cmds =>
                Kyo.foreach(tabList)(_ => cmds.freshId).map(ids => renderTabs(Present((cmds, ids.toList))))
            }
        }.placeholder(renderTabs(Absent))

    /** The rendered tabs, with the roving-focus wiring when there is a mount to mint ids in.
      *
      * `Absent` is the placeholder: a golden render and an SSG page both show it, and it is what
      * the component did before the arrows, which is a usable tab strip whose inactive headers are
      * reachable by clicking. The wiring arrives with the mount.
      */
    private def renderTabs(nav: Maybe[(UI.Commands, List[String])])(using Frame): UI =
        withActive(active => body(active, nav))

    /** The seam the golden tests render, since a mount shows only its placeholder there. */
    private[uic] def wired(ids: List[String], focus: String => Any < Async)(using Frame): UI =
        withActive(active => bodyWith(active, ids, focus))

    /** Builds `f` against the active tab id, however the selection is bound.
      *
      * `Dyn` before `Const` and no `ReactiveVariable` case: a two-way binding IS-A
      * `Dyn`, and reading it is identical to reading a one-way signal. The ref is
      * only needed to WRITE, which happens in [[select]].
      */
    private def withActive(f: String => UI)(using Frame): UI =
        selectedBinding match
            case Present(ReactiveValue.Dyn(sig)) => sig.render(f)
            case Present(ReactiveValue.Const(v)) => f(v)
            case Absent                          => f(tabList.headOption.map(_.id).getOrElse(""))

    /** The ref to write back to — `Absent` unless the binding is two-way. */
    private def selectedRef: Maybe[SignalRef[String]] = selectedBinding match
        case Present(ReactiveVariable(ref)) => Present(ref)
        case _                              => Absent

    private def body(active: String, nav: Maybe[(UI.Commands, List[String])])(using Frame): UI =
        nav match
            case Present((cmds, ids)) => bodyWith(active, ids, id => cmds.focusId(id))
            case Absent               => bodyWith(active, Nil, _ => ())

    /** Moves focus to the header `step` lands on, and to nothing else.
      *
      * Activation is deliberately not read from [[ListNav]] here: a tab IS a `<button>`, so the
      * browser and the dispatcher already agree on one activation per Enter or Space, and a third
      * from this handler would be one too many. Accordion declines it for the same reason.
      */
    private def headerMove(ids: List[String], focus: String => Any < Async, navigable: List[Int], self: Int)(using
        Frame
    ): KeyboardEvent => Any < Async = e =>
        ListNav.onKey(navigable, self, e.key, wrap = true, ListNav.Orientation.Horizontal) match
            case Present(step) if step.focus != self && ids.isDefinedAt(step.focus) => focus(ids(step.focus))
            case _                                                                  => ()

    private def bodyWith(active: String, ids: List[String], focus: String => Any < Async)(using Frame): UI =
        // The positions the keyboard may land on: a disabled tab is out of the tab order, so an
        // arrow steps over it.
        val navigable = tabList.zipWithIndex.collect { case (t, i) if !t.disabled => i }
        val headers: List[UI] = tabList.zipWithIndex.map { (t, i) =>
            val isActive            = t.id == active
            val iconSlot: List[UI]  = t.icon.toList.map(g => GlyphSvg(g, "p-uic-tab-icon"))
            val countSlot: List[UI] = t.additionalText.toList.map(c => span.cssClass("p-uic-tab-count")(c))
            val barSlot: List[UI] =
                if isActive then List(span.cssClass("p-tablist-active-bar").aria("hidden", "true"))
                else Nil
            val textSlot: UI = t.text match
                case TextValue.Const(x) => span(x): UI
                case TextValue.Dyn(s)   => s.render(x => span(x))
            val content: List[UI] =
                (iconSlot :+ textSlot) ++ countSlot ++ barSlot
            var tabEl = button
                .cssClass("p-tab")
                .role("tab")
                .jsProp("type", "button")
                .aria("selected", isActive.toString)
            if isActive then tabEl = tabEl.cssClass("p-tab-active")
            if t.disabled then tabEl = tabEl.cssClass("p-disabled").disabled(true)
            else
                tabEl = tabEl
                    .tabIndex(if isActive then 0 else -1)
                    .onClick(select(t.id))
                // The roving tabindex above takes every inactive tab OUT of the Tab order, which
                // is what the ARIA tablist pattern asks for and what leaves the arrows as the only
                // way back to them. Without this handler they were simply unreachable.
                if ids.isDefinedAt(i) then
                    tabEl = tabEl.id(ids(i)).onKeyDown(headerMove(ids, focus, navigable, i))
            end if
            tabEl(content.map(toChild)*)
        }
        val tablist: UI =
            div.cssClass("p-tablist")(
                toChild(
                    div.cssClass("p-tablist-content").cssClass("p-tablist-viewport")(
                        toChild(
                            div.cssClass("p-tablist-tab-list").role("tablist")(headers.map(toChild)*)
                        )
                    )
                )
            )

        val panelContent: UI =
            tabList.find(_.id == active).orElse(tabList.headOption).map(_.content).getOrElse(UI.empty)
        val panels: UI =
            div.cssClass("p-tabpanels")(
                toChild(div.cssClass("p-tabpanel").role("tabpanel")(toChild(panelContent)))
            )

        div.cssClass("p-tabs").cssClass("p-component")(toChild(tablist), toChild(panels))
    end bodyWith

    /** Selecting a tab writes its id into the bound ref (only a two-way binding has
      * one), then fires `onTabSelect`.
      */
    private def select(id: String)(using Frame): Any < Async =
        val setActive: Any < Async = selectedRef match
            case Present(ref) => ref.set(id)
            case Absent       => ()
        val fireSelect: Any < Async = onTabSelectF match
            case Present(f) => f(id)
            case Absent     => ()
        for
            _ <- setActive
            r <- fireSelect
        yield r
        end for
    end select
end Tabs

object Tabs:
    def apply(): Tabs = new Tabs()
