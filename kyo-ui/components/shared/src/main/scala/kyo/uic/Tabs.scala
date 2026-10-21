package kyo.uic

import kyo.*
import kyo.UI.*

/** One tab of a [[Tabs]] — a hand-authored carrier: a `text` label, the
  * already-rendered `content` shown when the tab is active, a stable `id` (the
  * value the `selected` binding holds and the `onTabSelect` payload), an optional
  * leading `icon` glyph, an optional `additionalText` count badge (kyo
  * extension — `span.p-uic-tab-count`), an optional `url` (renders the header as
  * a real anchor), and a `disabled` flag (non-clickable).
  */
final case class Tab private[uic] (
    text: TextValue,
    content: UI,
    id: String,
    icon: Maybe[IconGlyph],
    additionalText: Maybe[String],
    disabled: Boolean,
    // No default: the synthetic companion `apply` would then also carry defaults and
    // clash with the hand-written one below.
    urlV: Maybe[String]
):
    /** Navigation target: the header renders as a real `<a href=…>` instead of a
      * `<button>`, so a plain click navigates, a middle click opens a new tab, the
      * link can be copied, and a crawler can follow it — none of which a button
      * offers. Same slot and same reasoning as [[MenuItem.url]].
      *
      * The component learns no navigation concept from this. It emits an anchor;
      * turning that into client-side routing is `UILocation`'s document-level
      * interceptor, which claims same-origin unmodified clicks and deliberately
      * lets a modified one (Ctrl/Cmd/Shift/Alt, middle button) fall through to the
      * browser.
      *
      * A `url` header does NOT write back into a two-way `selected` binding: the
      * navigation IS the selection, and writing would race it. The `.p-tab` rule
      * carries no button reset, so the extracted CSS applies to the anchor
      * unchanged.
      *
      * `onTabSelect` still fires — but ADDING it to a url strip costs the modified
      * click. kyo-ui prevent-defaults a click on any anchor carrying a kyo click
      * handler, without exempting Ctrl/Cmd/Shift/Alt or the middle button, so the
      * handler that fires `onTabSelect` is also what stops the browser opening a new
      * tab. A url strip with no `onTabSelect` declares no handler and keeps the full
      * native behaviour, which is why a plain navigational strip should not reach for
      * one. (Measured in a browser: a url tab without `onTabSelect` leaves a
      * Ctrl-click unprevented; with one, it is prevented.)
      */
    def url(v: String): Tab = copy(urlV = Present(v))
end Tab

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
    ): Tab = new Tab(TextValue.Const(text), content, id, icon, additionalText, disabled, Absent)
end Tab

/** Tabs — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's compound
  * Tabs anatomy: `div.p-tabs.p-component` > `div.p-tablist` >
  * `div.p-tablist-content.p-tablist-viewport` > `div.p-tablist-tab-list[role=tablist]`
  * of `button.p-tab[role=tab]` headers (the active one carrying `.p-tab-active`
  * plus the `span.p-tablist-active-bar` ink bar) above `div.p-tabpanels` >
  * one `div.p-tabpanel[role=tabpanel]` per tab), so the extracted `@primeuix`
  * tabs CSS applies verbatim. As in PrimeVue with `lazy` off, every panel is
  * rendered once and the inactive ones carry `hidden`; [[Tabs.lazyPanels]]
  * renders the active panel alone instead. Prime positions the ink bar
  * with JS measuring; here it sits inside the active tab and spans it via a
  * remainder rule, which is visually identical without a measuring runtime.
  *
  * The active tab is one `selected` slot taking `String | Signal[String]`, so the
  * selection may live anywhere: a constant, a signal DERIVED from something else
  * (a route, a normalized cache record, a parent), or a writable `SignalRef` for
  * the two-way case. Only the last writes back — see [[Tabs.selected]]. With
  * nothing bound, the first tab is shown.
  *
  * A header is a real `<button>`, or a real `<a href>` when the tab carries a
  * [[Tab.url]] — the navigational strip, which is what Prime v4 answers with now
  * that `TabMenu` is gone (there is no `p-tabmenu` sheet to mirror, and `.p-tab`
  * carries no button reset, so the extracted CSS fits either element). Either way
  * keyboard activation (Enter/Space) is native; the tablist is ONE tab stop and
  * the arrows move between headers inside it, which is the ARIA tablist pattern
  * and the only thing that makes an inactive header reachable at all once the
  * roving tabindex has taken it out of the Tab order.
  *
  * `id(...)` is the base each header derives from: `s"$id-${tab.id}"`, using the
  * tab's own logical id, so a strip's headers are addressable without `Tab`
  * growing a second id slot beside the one it already has. Without a base the
  * mount mints them, which keeps the arrow wiring working unasked but leaves the
  * ids arbitrary — and absent entirely from a golden render or an SSG page,
  * since minting needs a mount. With a base they are the same in all three.
  */
final case class Tabs private (
    tabList: List[Tab] = Nil,
    selectedBinding: Maybe[ReactiveValue[String]] = Absent,
    onTabSelectF: Maybe[String => UI.Action] = Absent,
    idV: Maybe[String] = Absent,
    lazyPanelsFlag: Boolean = false
) extends Node, HasElementId:
    type Self = Tabs

    /** Appends the given tabs. */
    def tabs(ts: Tab*): Tabs = copy(tabList = tabList ++ ts.toList)

    /** Appends a single tab from bare content children (ergonomic builder). */
    def tab(text: String | Signal[String], id: String)(content: UI*)(using Frame): Tabs =
        copy(tabList = tabList :+ new Tab(ReactiveValue(text), fragment(content*), id, Absent, Absent, false, Absent))

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

    def onTabSelect(f: String => UI.Action)(using Frame): Tabs = copy(onTabSelectF = Present(Callback.of(f)))

    /** Renders the active panel alone, and builds a panel again each time its tab is chosen
      * (PrimeVue's `lazy`). Off by default: every panel is rendered once and a switch only
      * toggles `hidden`, so what a reader left in a panel (a typed value, a scroll position,
      * an editor, an open row) is still there when they come back to it. Turn it on where the
      * panels are expensive to build and nothing in them needs to survive a switch.
      */
    def lazyPanels(v: Boolean): Tabs = copy(lazyPanelsFlag = v)

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): Tabs = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Header ids derived from a caller-set base — `s"$base-${tab.id}"`, chosen and stable.
      * `Absent` when no base was given, which is what sends [[render]] to the mount to mint
      * them instead.
      */
    private def derivedIds: Maybe[List[String]] =
        idV.map(base => tabList.map(t => s"$base-${t.id}"))

    private[uic] def render(using Frame): UI =
        UI.mounted {
            UI.commands.map { cmds =>
                idsFor(cmds).map(ids => renderTabs(ids, id => cmds.focusId(id)))
            }
        }
            // With a base the ids are known without a mount, so the placeholder carries them
            // too: a golden render, an SSG page and the live tree then agree on the header
            // ids instead of only the live one having any. Arrow-key focus still needs the
            // mount, which is what the no-op focus says here.
            .placeholder(renderTabs(derivedIds.getOrElse(Nil), _ => ()))

    /** The header ids: a caller's base wins, and only without one does the mount mint them.
      * Same rule as [[Menu]]'s base — the ARIA and focus wiring works unasked, and a caller
      * who wants to address a header can.
      */
    private def idsFor(cmds: UI.Commands)(using Frame): List[String] < Sync =
        derivedIds match
            case Present(ids) => ids
            case Absent       => Kyo.foreach(tabList)(_ => cmds.freshId).map(_.toList)

    /** The rendered tabs. `ids` empty means no header carries one, which is what a
      * placeholder shows for an un-based strip: a usable tab strip whose inactive headers
      * are reachable by clicking, with the arrow wiring arriving with the mount.
      */
    private def renderTabs(ids: List[String], focus: String => UI.Action)(using Frame): UI =
        body(ids, focus)

    /** The seam the golden tests render, since a mount shows only its placeholder there. */
    private[uic] def wired(ids: List[String], focus: String => UI.Action)(using Frame): UI =
        body(ids, focus)

    /** The tab a selection shows: the one it names, or the first when it names none. */
    private def resolve(selected: String): String =
        if tabList.exists(_.id == selected) then selected else tabList.headOption.map(_.id).getOrElse("")

    /** Builds `f` against the active tab id, however the selection is bound.
      *
      * `Dyn` before `Const` and no `ReactiveVariable` case: a two-way binding IS-A
      * `Dyn`, and reading it is identical to reading a one-way signal. The ref is
      * only needed to WRITE, which happens in [[select]].
      */
    private def withActive(f: String => UI)(using Frame): UI =
        selectedBinding match
            case Present(ReactiveValue.Dyn(sig)) => sig.render(a => f(resolve(a)))
            case Present(ReactiveValue.Const(v)) => f(resolve(v))
            case Absent                          => f(resolve(""))

    /** Hides the panel of tab `id` while another tab is active. A bound signal toggles the
      * attribute in place, which is what keeps the panel's subtree alive across a switch.
      */
    private def hideUnlessActive[E <: UI.Ast.Element { type Self = E }](el: E, id: String)(using Frame): E =
        selectedBinding match
            case Present(ReactiveValue.Dyn(sig)) => el.hidden(sig.map(a => resolve(a) != id))
            case Present(ReactiveValue.Const(v)) => if resolve(v) == id then el else el.hidden(true)
            case Absent                          => if resolve("") == id then el else el.hidden(true)

    /** The ref to write back to — `Absent` unless the binding is two-way. */
    private def selectedRef: Maybe[SignalRef[String]] = selectedBinding match
        case Present(ReactiveVariable(ref)) => Present(ref)
        case _                              => Absent

    /** Moves focus to the header `step` lands on, and to nothing else.
      *
      * Activation is deliberately not read from [[ListNav]] here: a tab IS a `<button>`, so the
      * browser and the dispatcher already agree on one activation per Enter or Space, and a third
      * from this handler would be one too many. Accordion declines it for the same reason.
      */
    private def headerMove(ids: List[String], focus: String => UI.Action, navigable: List[Int], self: Int)(using
        Frame
    ): KeyboardEvent => UI.Action = e =>
        ListNav.onKey(navigable, self, e.key, wrap = true, ListNav.Orientation.Horizontal) match
            case Present(step) if step.focus != self && ids.isDefinedAt(step.focus) => focus(ids(step.focus))
            case _                                                                  => ()

    /** Everything a header carries regardless of which element it is: the tablist
      * roles, the active marker, the roving tabindex, the click, and the arrow-key
      * handler. Generic over the builder rather than written twice, because every
      * setter here lives on `UI.Ast.Interactive` — which `Button` and `Anchor` both
      * are — and each returns the builder's own `Self`.
      */
    private def interactive[E <: UI.Ast.Interactive { type Self = E }](
        el: E,
        t: Tab,
        isActive: Boolean,
        i: Int,
        ids: List[String],
        focus: String => UI.Action,
        navigable: List[Int]
    )(using Frame): E =
        var out = el.role("tab").aria("selected", isActive.toString)
        if isActive then out = out.cssClass("p-tab-active")
        // The id goes on before the disabled branch, not inside it. While ids were minted
        // only to move the arrow focus, giving one to a header the arrows skip was pointless;
        // a CHOSEN id is different — a caller who names a header may well be naming it to
        // assert that it is disabled.
        if ids.isDefinedAt(i) then
            out = out.id(ids(i))
            // A lazy strip has only the active panel in the page, so only its header may point at one.
            if !lazyPanelsFlag || isActive then out = out.aria("controls", panelId(ids(i)))
        end if
        if t.disabled then out = out.cssClass("p-disabled")
        else
            out = out.tabIndex(if isActive then 0 else -1)
            // A url header gets a click handler only if there is something for it to
            // do. It never writes back, so with no `onTabSelect` the handler would be
            // empty — and an EMPTY handler is not free: kyo-ui prevent-defaults a click
            // on any anchor that declares one (DomBackend, "the handler, not the href,
            // drives the action"), which also takes the middle-click and the
            // Ctrl/Cmd-click with it. Declaring nothing keeps the link a link.
            if t.urlV.isEmpty || onTabSelectF.isDefined then
                out = out.onClick(select(t.id, write = t.urlV.isEmpty))
            // The roving tabindex above takes every inactive tab OUT of the Tab order, which
            // is what the ARIA tablist pattern asks for and what leaves the arrows as the only
            // way back to them. Without this handler they were simply unreachable.
            if ids.isDefinedAt(i) then out = out.onKeyDown(headerMove(ids, focus, navigable, i))
        end if
        out
    end interactive

    /** The id of the panel a header controls, derived from the header's own. */
    private def panelId(headerId: String): String = s"$headerId-panel"

    /** The strip and the panels. Only the strip lives in the active-tab region: the panels sit
      * outside it, each hiding itself while another tab is active, so choosing a tab repaints
      * the headers and toggles two attributes, and builds no panel. A lazy strip puts the
      * panels in a region of their own instead, since it builds the active one on each switch.
      */
    private def body(ids: List[String], focus: String => UI.Action)(using Frame): UI =
        def panel(t: Tab, i: Int): UI.Ast.Div =
            var el = div.cssClass("p-tabpanel").role("tabpanel")
            if ids.isDefinedAt(i) then el = el.id(panelId(ids(i))).aria("labelledby", ids(i))
            el
        end panel
        val indexed    = tabList.zipWithIndex
        val panels: UI =
            if lazyPanelsFlag then
                withActive { active =>
                    div.cssClass("p-tabpanels")(
                        indexed.filter(_._1.id == active).map((t, i) => toChild(panel(t, i)(toChild(t.content))))*
                    )
                }
            else
                div.cssClass("p-tabpanels")(
                    indexed.map((t, i) => toChild(hideUnlessActive(panel(t, i), t.id)(toChild(t.content))))*
                )
        // The caller's id on the component's own root, as HasElementId requires of a container.
        // The headers derive `s"$id-${tab.id}"` from the same value; the two do not collide,
        // because a derived name is a different string (GAPS.md F-34).
        var root = div.cssClass("p-tabs").cssClass("p-component")
        idV.foreach(v => root = root.id(v))
        root(toChild(withActive(active => tablistWith(active, ids, focus))), toChild(panels))
    end body

    private def tablistWith(active: String, ids: List[String], focus: String => UI.Action)(using Frame): UI =
        // The positions the keyboard may land on: a disabled tab is out of the tab order, so an
        // arrow steps over it.
        val navigable         = tabList.zipWithIndex.collect { case (t, i) if !t.disabled => i }
        val headers: List[UI] = tabList.zipWithIndex.map { (t, i) =>
            val isActive            = t.id == active
            val iconSlot: List[UI]  = t.icon.toList.map(g => GlyphSvg(g, "p-uic-tab-icon"))
            val countSlot: List[UI] = t.additionalText.toList.map(c => span.cssClass("p-uic-tab-count")(c))
            val barSlot: List[UI]   =
                if isActive then List(span.cssClass("p-tablist-active-bar").aria("hidden", "true"))
                else Nil
            val textSlot: UI = t.text match
                case TextValue.Const(x) => span(x): UI
                case TextValue.Dyn(s)   => span(s): UI
            val content: List[UI] = (iconSlot :+ textSlot) ++ countSlot ++ barSlot
            val kids              = content.map(toChild)
            t.urlV match
                case Present(u) =>
                    // An anchor has no native `disabled`, so a disabled nav tab drops its
                    // href and says so through ARIA instead — Link's handling, for the same
                    // reason.
                    var el = interactive(a.cssClass("p-tab"), t, isActive, i, ids, focus, navigable)
                    if t.disabled then el = el.aria("disabled", "true")
                    else el = el.href(Href.Path(u))
                    el(kids*)
                case Absent =>
                    var el = interactive(
                        button.jsProp("type", "button").cssClass("p-tab"),
                        t,
                        isActive,
                        i,
                        ids,
                        focus,
                        navigable
                    )
                    if t.disabled then el = el.disabled(true)
                    el(kids*)
            end match
        }
        div.cssClass("p-tablist")(
            toChild(
                div.cssClass("p-tablist-content").cssClass("p-tablist-viewport")(
                    toChild(
                        div.cssClass("p-tablist-tab-list").role("tablist")(headers.map(toChild)*)
                    )
                )
            )
        )
    end tablistWith

    /** Selecting a tab writes its id into the bound ref (only a two-way binding has
      * one), then fires `onTabSelect`.
      *
      * `write` is false for a `url` header: the navigation is the selection, and a
      * write would race it — the ref would hold the new id while the location, and
      * so anything derived from it, still held the old one.
      */
    private def select(id: String, write: Boolean)(using Frame): UI.Action =
        val setActive: UI.Action = selectedRef match
            case Present(ref) if write => ref.set(id)
            case _                     => ()
        val fireSelect: UI.Action = onTabSelectF match
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
