package kyo.uic

import kyo.*
import kyo.UI.*

/** One tab of a [[Tabs]], a hand-authored carrier: a `text` label, the
  * already-rendered `content` shown when the tab is active, a stable `id` (the
  * value the `selected` binding holds and the `onSelectedChange` payload), an optional
  * leading `icon` glyph, an optional `additionalText` count badge (kyo
  * extension: `span.p-uic-tab-count`), an optional `url` (renders the header as
  * a real anchor), and a `disabled` flag (non-clickable).
  */
final class Tab[K] private[uic] (private[uic] val p: Tab.Props[K]) extends HasProps derives CanEqual:
    import p.*

    /** Navigation target: the header renders as a real `<a href=…>` instead of a
      * `<button>`, so a plain click navigates, a middle click opens a new tab, the
      * link can be copied, and a crawler can follow it, none of which a button
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
      * `onSelectedChange` still fires, but ADDING it to a url strip costs the modified
      * click. kyo-ui prevent-defaults a click on any anchor carrying a kyo click
      * handler, without exempting Ctrl/Cmd/Shift/Alt or the middle button, so the
      * handler that fires `onSelectedChange` is also what stops the browser opening a new
      * tab. A url strip with no `onSelectedChange` declares no handler and keeps the full
      * native behaviour, which is why a plain navigational strip should not reach for
      * one. (Measured in a browser: a url tab without `onSelectedChange` leaves a
      * Ctrl-click unprevented; with one, it is prevented.)
      */
    def url(v: String): Tab[K] = new Tab(p.copy(urlV = Present(v)))
end Tab

object Tab:
    final private[uic] case class Props[K](
        text: TextValue,
        content: UI,
        id: K,
        icon: Maybe[IconGlyph],
        additionalText: Maybe[String],
        disabled: Boolean,
        urlV: Maybe[String]
    )

    /** Construct a tab with a constant `text` label. The `Tabs.tab` builder takes the same
      * slot as a `String | Signal[String]` union, for a reactive label.
      */
    def apply[K](
        text: String,
        content: UI,
        id: K,
        icon: Maybe[IconGlyph] = Absent,
        additionalText: Maybe[String] = Absent,
        disabled: Boolean = false
    ): Tab[K] = new Tab(Props(TextValue.Const(text), content, id, icon, additionalText, disabled, Absent))
end Tab

/** A tab, built directly or as `_.tab(...)` (see [[TabScope]]). */
type TabEntry[K] = Entry[Tab[K], TabScope[K]]

/** A [[TabEntry]] or a signal of tabs, for a strip whose tabs change while it is shown (see [[LiveEntry]]). */
type TabLiveEntry[K] = LiveEntry[Tab[K], TabScope[K]]

/** What `_` stands for in [[Tabs.tabs]]: `_.tab("Profile", profileForm, "profile")`. The label takes a `Signal[String]` too, as
  * [[Tabs.tab]] does.
  */
final class TabScope[K] private ():
    /** A tab keyed by `id`, as [[Tab.apply]], with a constant or reactive label. */
    def tab(
        text: String | Signal[String],
        content: UI,
        id: K,
        icon: Maybe[IconGlyph] = Absent,
        additionalText: Maybe[String] = Absent,
        disabled: Boolean = false
    ): Tab[K] = new Tab(Tab.Props(ReactiveValue(text), content, id, icon, additionalText, disabled, Absent))
end TabScope

object TabScope:
    // The scope holds nothing of its own type, so one instance serves every `K`.
    private val scope = new TabScope[Any]

    private[uic] def resolve[K](entries: Seq[TabEntry[K]]): List[Tab[K]] =
        Entry.resolve(scope.asInstanceOf[TabScope[K]], entries)

    private[uic] def reading[K](entries: Seq[TabLiveEntry[K]])(using CanEqual[K, K], Frame): ReactiveValue[List[Tab[K]]] =
        Entry.reading(scope.asInstanceOf[TabScope[K]], entries)
end TabScope

/** Tabs: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's compound
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
  * The active tab is one `selected` slot taking `K | Signal[K]`, `K` being the
  * app's own tab id type, so the
  * selection may live anywhere: a constant, a signal DERIVED from something else
  * (a route, a normalized cache record, a parent), or a writable `SignalRef` for
  * the two-way case. Only the last writes back (see [[Tabs.selected]]). With
  * nothing bound, the first tab is shown.
  *
  * A header is a real `<button>`, or a real `<a href>` when the tab carries a
  * [[Tab.url]]: the navigational strip, which is what Prime v4 answers with now
  * that `TabMenu` is gone (there is no `p-tabmenu` sheet to mirror, and `.p-tab`
  * carries no button reset, so the extracted CSS fits either element). Either way
  * keyboard activation (Enter/Space) is native; the tablist is ONE tab stop and
  * the arrows move between headers inside it, which is the ARIA tablist pattern
  * and the only thing that makes an inactive header reachable at all once the
  * roving tabindex has taken it out of the Tab order.
  *
  * `id(...)` is the base each header derives from: `s"$id-${tab.id}"`, using the
  * tab's own logical id, so a strip's headers are addressable without `Tab`
  * growing a second id slot beside the one it already has. Its panel is
  * `s"${id}_panel-${tab.id}"`, which no header id can equal, whatever the tab ids are.
  * Without a base the
  * mount mints one, which keeps the arrow wiring working unasked but leaves the
  * ids arbitrary, and absent entirely from a golden render or an SSG page,
  * since minting needs a mount. With a base they are the same in all three.
  * Either way a tab a signal adds later (see [[LiveEntry]]) derives its id the
  * same way, with nothing more to mint.
  */
final class Tabs[K] private[uic] (private[uic] val p: Tabs.Props[K]) extends Node, HasElementId, HasProps:
    import p.*

    type Self = Tabs[K]

    // Tab ids compare by `equals`, the identity an app's id type already has.
    private given CanEqual[K, K] = CanEqual.derived

    /** Appends the given tabs, each built directly or from `_` (see [[TabScope]]), or a signal of tabs that change while the
      * strip is shown (see [[LiveEntry]]). A tab whose id stays in the signal keeps its panel, and what the reader left in it.
      */
    def tabs(ts: TabLiveEntry[K]*): Tabs[K] = new Tabs(p.copy(tabSources = tabSources ++ ts))

    /** Appends a single tab from bare content children (ergonomic builder). */
    def tab(text: String | Signal[String], id: K)(content: UI*)(using Frame): Tabs[K] =
        new Tabs(p.copy(tabSources =
            tabSources :+ new Tab(Tab.Props(ReactiveValue(text), fragment(content*), id, Absent, Absent, false, Absent))
        ))

    /** The tabs as ONE reading: fixed unless one of them was handed over as a signal. */
    private[uic] def entries(using Frame): ReactiveValue[List[Tab[K]]] = TabScope.reading(tabSources)

    /** The active tab's id. A `SignalRef` binds two way: clicking a tab writes its
      * id back before firing `onSelectedChange`, which is the self-contained panel
      * switcher. Any other `Signal` binds ONE way: the strip renders what the signal
      * says and fires `onSelectedChange`, and never writes, which is what a selection
      * owned elsewhere needs: a route is a `map` over the location and has no ref
      * to write into. A plain `String` is the same contract without a stream, and one nothing hears is fixed: the strip
      * shows that tab, switches to no other, and its inactive headers say so. Unbound, the strip keeps the tab the reader
      * picks itself, starting on the first.
      *
      * Reading needs no ref at all (`Signal.render` serves every case); only the
      * write-back does. Demanding a `SignalRef` from every caller to serve the
      * callers that want write-back is the capability this slot used to require and
      * no longer does.
      *
      * The two-way choice is made on the RUNTIME class, so ascribing a ref as
      * `Signal[K]` does not opt out; pass `ref.readOnly` for that.
      */
    def selected(v: K | Signal[K]): Tabs[K] =
        // The setter takes no Frame, and a public signature does not change for this. Nothing here runs: a Frame only names
        // where a signal was built.
        given Frame = Frame.internal
        new Tabs(p.copy(selectedSlot = selectedSlot.bind(v match
            case r: SignalRef[K] @unchecked => r.zoom(Maybe(_))((old, m) => m.getOrElse(old))
            case s: Signal[K] @unchecked    => s.map(Maybe(_))
            case k: K @unchecked            => Maybe(k))))
    end selected

    /** Fired with the id of the tab the reader picks, after a two-way [[selected]] is written. */
    def onSelectedChange(f: K => UI.Action)(using Frame): Tabs[K] =
        new Tabs(p.copy(selectedSlot = selectedSlot.hear(_.fold[UI.Action](())(f))))

    /** Renders the active panel alone, and builds a panel again each time its tab is chosen
      * (PrimeVue's `lazy`). Off by default: every panel is rendered once and a switch only
      * toggles `hidden`, so what a reader left in a panel (a typed value, a scroll position,
      * an editor, an open row) is still there when they come back to it. Turn it on where the
      * panels are expensive to build and nothing in them needs to survive a switch.
      */
    def lazyPanels(v: Boolean): Tabs[K] = new Tabs(p.copy(lazyPanelsFlag = v))

    private[uic] def accessibility: Accessibility              = accessV
    private[uic] def withAccessibility(v: Accessibility): Self = new Tabs(p.copy(accessV = v))

    private[uic] def render(using Frame): UI =
        val strip = renderLive
        // A signal nobody hears: every tab the reader picks is painted away again. A url header is heard by the navigation it
        // starts, so only a header without one makes the strip deaf. The card reads the list, so it is drawn over it.
        if !selectedSlot.deaf then strip
        else
            val card = entries.render(ts =>
                fragment(Diagnostics.cards("Tabs")(Diagnostic.unheard(
                    selectedSlot,
                    Tabs.selectedName,
                    on = ts.exists(_.p.urlV.isEmpty)
                ))*)
            )
            fragment(card, strip)
        end if
    end render

    private def renderLive(using Frame): UI =
        UI.mounted {
            // A caller's base wins, and only without one does the mount draw it. Same rule as [[Menu]]'s base: the ARIA
            // and focus wiring works unasked, and a caller who wants to address a header can.
            val base: String < Sync = accessV.idV match
                case Present(b) => b
                case Absent     => UI.id
            // Unbound, the strip keeps the tab the reader picks itself, starting on the first.
            val first: Maybe[K] < Sync = entries match
                case ReactiveValue.Const(ts) => Maybe.fromOption(ts.headOption.map(_.p.id))
                case ReactiveValue.Dyn(sig)  => sig.current.map(ts => Maybe.fromOption(ts.headOption.map(_.p.id)))
            val self: Tabs[K] < Sync = selectedSlot.owned(first).map(s => new Tabs(this.p.copy(selectedSlot = s)))
            base.map(b => self.map(_.wired(b, focusById)))
        }

    /** The seam a test renders to choose the base itself. */
    private[uic] def wired(base: String, focus: String => UI.Action)(using Frame): UI =
        body(Present(base), focus)

    /** The id of tab `t`'s header under `base`; none without a base, which is what a placeholder shows for an un-based
      * strip: a usable tab strip whose inactive headers are reachable by clicking, with the arrow wiring arriving with the
      * mount.
      */
    private def headerId(base: Maybe[String], t: Tab[K]): Maybe[String] = base.map(b => s"$b-${t.p.id}")

    /** The tab a selection shows among `ts`: the one it names, or the first when it names none. Resolved against the tabs
      * read with it, since a live list may no longer hold the tab a selection names.
      */
    private def resolveIn(ts: List[Tab[K]], selected: Maybe[K]): Maybe[K] =
        selected.filter(s => ts.exists(_.p.id == s)).orElse(Maybe.fromOption(ts.headOption.map(_.p.id)))

    /** Hides the panel of tab `id` while another tab is active. A live selection toggles the attribute in place, which is
      * what keeps the panel's subtree alive across a switch.
      */
    private def hideUnlessActive[E <: UI.Ast.Element { type Self = E }](el: E, id: K, act: ReactiveValue[Maybe[K]])(using Frame): E =
        act match
            case ReactiveValue.Dyn(sig) => el.hidden(sig.map(a => !a.contains(id)))
            case ReactiveValue.Const(a) => if a.contains(id) then el else el.hidden(true)

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
      * setter here lives on `UI.Ast.Interactive` (which `Button` and `Anchor` both
      * are) and each returns the builder's own `Self`.
      */
    private def interactive[E <: UI.Ast.Interactive { type Self = E }](
        el: E,
        t: Tab[K],
        isActive: Boolean,
        i: Int,
        base: Maybe[String],
        ids: List[String],
        focus: String => UI.Action,
        navigable: List[Int]
    )(using Frame): E =
        var out = el.role("tab").aria("selected", isActive.toString)
        if isActive then out = out.cssClass("p-tab-active")
        // The id goes on before the disabled branch, not inside it. While ids were minted
        // only to move the arrow focus, giving one to a header the arrows skip was pointless;
        // a CHOSEN id is different: a caller who names a header may well be naming it to
        // assert that it is disabled.
        for h <- headerId(base, t); pid <- panelId(base, t) do
            out = out.id(h)
            // A lazy strip has only the active panel in the page, so only its header may point at one.
            if !lazyPanelsFlag || isActive then out = out.aria("controls", pid)
        end for
        if t.p.disabled then out = out.cssClass("p-disabled")
        else
            out = out.tabIndex(if isActive then 0 else -1)
            // A url header gets a click handler only if there is something for it to
            // do. It never writes back, so with no `onSelectedChange` the handler would be
            // empty, and an EMPTY handler is not free: kyo-ui prevent-defaults a click
            // on any anchor that declares one (DomBackend, "the handler, not the href,
            // drives the action"), which also takes the middle-click and the
            // Ctrl/Cmd-click with it. Declaring nothing keeps the link a link.
            if (t.p.urlV.isEmpty || selectedSlot.hearer.isDefined) && !selectedSlot.fixed then
                out = out.onClick(select(t.p.id, write = t.p.urlV.isEmpty))
            // A fixed strip keeps every header a stop the arrows reach, and an inactive one says it switches nothing: a
            // tab has no readonly state, so it says it the way a radio does. A url header still navigates, so it says
            // nothing.
            else if selectedSlot.fixed && !isActive && t.p.urlV.isEmpty then out = out.aria("disabled", "true")
            end if
            // The roving tabindex above takes every inactive tab OUT of the Tab order, which
            // is what the ARIA tablist pattern asks for and what leaves the arrows as the only
            // way back to them. Without this handler they were simply unreachable.
            if ids.isDefinedAt(i) then out = out.onKeyDown(headerMove(ids, focus, navigable, i))
        end if
        out
    end interactive

    /** The id of tab `t`'s panel under `base`. It cannot be the header's id plus a suffix: a header is `s"$base-$id"` for
      * any tab id, so a tab named "two-panel" would print the panel id of "two". The underscore after the base is what no
      * header id has there.
      */
    private def panelId(base: Maybe[String], t: Tab[K]): Maybe[String] = base.map(b => s"${b}_panel-${t.p.id}")

    /** The strip and the panels. Only the strip lives in the active-tab region: the panels sit
      * outside it, each hiding itself while another tab is active, so choosing a tab repaints
      * the headers and toggles two attributes, and builds no panel. A lazy strip puts the
      * panels in a region of their own instead, since it builds the active one on each switch.
      *
      * The panels of a live strip are a list keyed by tab id: a tab the signal adds is a new
      * row, and a tab that stays keeps the panel it has, so the list is the one region above
      * a panel and a change to it builds only the panels that are new.
      */
    private def body(base: Maybe[String], focus: String => UI.Action)(using Frame): UI =
        def panel(t: Tab[K]): UI.Ast.Div =
            var el = div.cssClass("p-tabpanel").role("tabpanel")
            for h <- headerId(base, t); pid <- panelId(base, t) do el = el.id(pid).aria("labelledby", h)
            el
        end panel
        val listed     = entries.and(selectedSlot.reading(Absent))
        val act        = listed.map((ts, sel) => resolveIn(ts, sel))
        val panels: UI =
            if lazyPanelsFlag then
                listed.render { (ts, sel) =>
                    val a = resolveIn(ts, sel)
                    div.cssClass("p-tabpanels")(UI.children(ts.filter(t => a.contains(t.p.id)).map(t => panel(t)(t.p.content)))*)
                }
            else
                def shown(t: Tab[K]): UI = hideUnlessActive(panel(t), t.p.id, act)(t.p.content)
                entries match
                    case ReactiveValue.Const(ts) => div.cssClass("p-tabpanels")(UI.children(ts.map(t => shown(t)))*)
                    case ReactiveValue.Dyn(sig)  =>
                        given CanEqual[Chunk[Tab[K]], Chunk[Tab[K]]] = CanEqual.derived
                        div.cssClass("p-tabpanels")(sig.map(Chunk.from(_)).foreachKeyed(_.p.id.toString)(shown))
                end match
        // The caller's id on the component's own root, as HasElementId requires of a container.
        // The headers derive `s"$id-${tab.id}"` from the same value; the two do not collide,
        // because a derived name is a different string (GAPS.md F-34).
        var root = div.cssClass("p-tabs").cssClass("p-component")
        accessV.idV.foreach(v => root = root.id(v))
        val headers = listed.render { (ts, sel) =>
            new Tabs(p.copy(tabList = ts)).tablistWith(resolveIn(ts, sel), base, focus)
        }
        root(headers, panels)
    end body

    private def tablistWith(active: Maybe[K], base: Maybe[String], focus: String => UI.Action)(using Frame): UI =
        val ids = tabList.flatMap(t => headerId(base, t).toList)
        // The positions the keyboard may land on: a disabled tab is out of the tab order, so an
        // arrow steps over it.
        val navigable         = tabList.zipWithIndex.collect { case (t, i) if !t.p.disabled => i }
        val headers: List[UI] = tabList.zipWithIndex.map { (t, i) =>
            val isActive            = active.contains(t.p.id)
            val iconSlot: List[UI]  = t.p.icon.toList.map(g => GlyphSvg(g, "p-uic-tab-icon"))
            val countSlot: List[UI] = t.p.additionalText.toList.map(c => span.cssClass("p-uic-tab-count")(c))
            val barSlot: List[UI]   =
                if isActive then List(span.cssClass("p-tablist-active-bar").aria("hidden", "true"))
                else Nil
            val textSlot: UI = t.p.text match
                case TextValue.Const(x) => span(x): UI
                case TextValue.Dyn(s)   => span(s): UI
            val content: List[UI] = (iconSlot :+ textSlot) ++ countSlot ++ barSlot
            val kids              = UI.children(content)
            t.p.urlV match
                case Present(u) =>
                    // An anchor has no native `disabled`, so a disabled nav tab drops its
                    // href and says so through ARIA instead: Link's handling, for the same
                    // reason.
                    var el = interactive(a.cssClass("p-tab"), t, isActive, i, base, ids, focus, navigable)
                    if t.p.disabled then el = el.aria("disabled", "true")
                    else el = el.href(Href.Path(u))
                    el(kids*)
                case Absent =>
                    var el = interactive(
                        button.jsProp("type", "button").cssClass("p-tab"),
                        t,
                        isActive,
                        i,
                        base,
                        ids,
                        focus,
                        navigable
                    )
                    if t.p.disabled then el = el.disabled(true)
                    el(kids*)
            end match
        }
        div.cssClass("p-tablist")(
            div.cssClass("p-tablist-content").cssClass("p-tablist-viewport")(
                div.cssClass("p-tablist-tab-list").role("tablist")(UI.children(headers)*)
            )
        )
    end tablistWith

    /** Selecting a tab writes its id into the bound ref (only a two-way binding has
      * one), then fires `onSelectedChange`.
      *
      * `write` is false for a `url` header: the navigation is the selection, and a
      * write would race it: the ref would hold the new id while the location, and
      * so anything derived from it, still held the old one.
      */
    private def select(id: K, write: Boolean)(using Frame): UI.Action =
        if write then selectedSlot.change(Present(id))
        else selectedSlot.hearer.fold[UI.Action](())(f => f(Present(id)))
    end select
end Tabs

object Tabs:
    final private[uic] case class Props[K](
        // The tabs the header strip draws, resolved from `tabSources` by the render.
        tabList: List[Tab[K]] = Nil,
        tabSources: List[TabLiveEntry[K]] = Nil,
        // Over `Maybe[K]`, since the strip's own state may start with no tab to name: a live list can be empty at mount.
        selectedSlot: Slot[Maybe[K]] = Slot[Maybe[K]](),
        accessV: Accessibility = Accessibility.none,
        lazyPanelsFlag: Boolean = false
    )

    def apply[K](): Tabs[K] = new Tabs[K](Props())

    private val selectedName = Slot.Name("Tabs", "selected", "a change", "onSelectedChange")
end Tabs
