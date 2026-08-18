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
    /** Construct a tab with a constant `text` label. The label slot also accepts a
      * reactive `Signal[String]` through the `Tabs.tab` builder overload.
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
  * The active tab binds two-way to a `SignalRef[String]` holding the tab id: the
  * panel re-renders reactively so it always shows the selected tab, and clicking
  * a tab writes its id back before firing `onTabSelect`. With no ref bound, the
  * first tab is shown. Tabs are real `<button>`s, so keyboard activation
  * (Enter/Space) is native.
  */
final case class Tabs private (
    tabList: List[Tab] = Nil,
    selectedRef: Maybe[SignalRef[String]] = Absent,
    onTabSelectF: Maybe[String => Any < Async] = Absent
) extends Node:
    type Self = Tabs

    /** Appends the given tabs. */
    def tabs(ts: Tab*): Tabs = copy(tabList = tabList ++ ts.toList)

    /** Appends a single tab from bare content children (ergonomic builder). */
    def tab(text: String, id: String)(content: UI*)(using Frame): Tabs =
        copy(tabList = tabList :+ new Tab(TextValue.Const(text), fragment(content*), id, Absent, Absent, false))

    /** Reactive-label variant: the tab label tracks `text` — re-renders in place on
      * emission (e.g. a locale-driven `I18n.t` leaf).
      */
    def tab(text: Signal[String], id: String)(content: UI*)(using Frame): Tabs =
        copy(tabList = tabList :+ new Tab(TextValue.Dyn(text), fragment(content*), id, Absent, Absent, false))

    /** Binds the active tab two-way to `ref` (holds the active tab id). */
    def selected(ref: SignalRef[String]): Tabs = copy(selectedRef = Present(ref))

    def onTabSelect(f: String => Any < Async): Tabs = copy(onTabSelectF = Present(f))

    private[uic] def render(using Frame): UI =
        selectedRef match
            case Present(ref) => ref.render(active => body(active))
            case Absent       => body(tabList.headOption.map(_.id).getOrElse(""))

    private def body(active: String)(using Frame): UI =
        val headers: List[UI] = tabList.map { t =>
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
    end body

    /** Selecting a tab writes its id into the bound ref (if any), then fires `onTabSelect`. */
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
