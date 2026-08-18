package kyo.uic

import kyo.*
import kyo.UI.*

/** Menu — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Menu
  * anatomy: `div.p-menu.p-component` > `ul.p-menu-list[role=menu]` of
  * `li.p-menu-item` rows — each `div.p-menu-item-content` >
  * `a.p-menu-item-link` with icon + label — plus `li.p-menu-separator` dividers
  * and `li.p-menu-submenu-label` section headings for grouped items), so the
  * extracted `@primeuix` menu CSS applies verbatim.
  *
  * Two modes like Prime: inline (the default render) and `popup(openRef)` — the
  * popup wraps the same list in the [[Overlay]] primitive (Prime's
  * `.p-menu-overlay` skin): writes to the ref open and close it, an outside
  * click or Escape closes, and the panel seeds focus on open so the keyboard
  * works without a prior click. Place the popup inside a `position: relative`
  * anchor (stamp `p-uic-overlay-anchor` on your trigger's container — the
  * Select/Popover geometry story).
  *
  * Items are the shared typed [[MenuItem]] model: `onSelect(effect)` runs on
  * click or Enter and the popup closes; `url` rows render real anchors;
  * top-level items carrying `items(...)` render as flat labelled sections
  * (Prime's grouped Menu — for nested floating submenus use TieredMenu).
  * Keyboard (WAI-ARIA menu): the list is the single tab stop; ArrowDown/ArrowUp
  * move Prime's `.p-focus` row over the enabled items (disabled and separators
  * skipped), Home/End jump to the first/last, Enter or Space activates it. The
  * item links carry `tabindex="-1"` (out of the Tab order — the list roves the
  * highlight), and `id(...)` wires `aria-activedescendant` to the focused row.
  *
  * Honest deferrals: no per-item templates; typeahead is not implemented.
  */
final case class Menu private (
    itemsV: List[MenuItem] = Nil,
    popupRefV: Maybe[SignalRef[Boolean]] = Absent,
    idV: Maybe[String] = Absent
) extends Node:
    type Self = Menu

    /** Appends items ([[MenuItem]] rows, `MenuItem.separator` dividers, and
      * grouped sections via nested `items`).
      */
    def items(is: MenuItem*): Menu = copy(itemsV = itemsV ++ is.toList)

    /** Base id for the list — enables `aria-activedescendant` (the focused row is
      * exposed as `s"$id-active"`). Give distinct ids to multiple menus on a page.
      * Without it the keyboard highlight still works; only the ARIA hook is
      * omitted.
      */
    def id(v: String): Menu = copy(idV = Present(v))

    /** Popup mode: the menu renders as a floating [[Overlay]] panel bound two-way
      * to `ref` — writes open and close it, outside click/Escape write back.
      */
    def popup(ref: SignalRef[Boolean]): Menu = copy(popupRefV = Present(ref))

    /** The flat row list: section items are flattened to a heading + their
      * children (Prime's grouped Menu model).
      */
    private def flatRows: List[MenuRow] =
        itemsV.flatMap { it =>
            if it.separatorFlag then List(MenuRow.Sep)
            else if it.itemsV.nonEmpty then
                MenuRow.Heading(it.labelV) :: it.itemsV.map { c =>
                    if c.separatorFlag then MenuRow.Sep else MenuRow.Item(c)
                }
            else List(MenuRow.Item(it))
        }

    private[uic] def render(using Frame): UI =
        // The keyboard highlight lives in a signal allocated by this effectful
        // mount; static projections (SSG, the SSR page HTML) render the same
        // anatomy inert until the client transport attaches.
        val stat: UI = body(-1, Absent)
        UI.mounted {
            for hi <- Signal.initRef(-1)
            yield wired(hi)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes — the seam golden tests render
      * directly (a full top-down re-render shows mounted regions as
      * placeholders).
      */
    private[uic] def wired(hi: SignalRef[Int])(using Frame): UI =
        hi.render(h => body(h, Present(hi)))

    private def body(h: Int, hiRef: Maybe[SignalRef[Int]])(using Frame): UI =
        val rows = flatRows
        // Positions (into `rows`) of the keyboard-navigable rows.
        val navigable: List[Int] = rows.zipWithIndex.collect {
            case (MenuRow.Item(it), i) if !it.disabledFlag => i
        }
        val hiRow: Int = if h >= 0 && h < navigable.size then navigable(h) else -1

        def activate(it: MenuItem): Any < Async =
            hiRef match
                case Present(hi) =>
                    for
                        _ <- it.actionV match
                            case Present(eff) => eff
                            case Absent       => (): Any < Async
                        _ <- popupRefV match
                            case Present(r) => r.set(false)
                            case Absent     => (): Any < Async
                        _ <- hi.set(-1)
                    yield ()
                case Absent => ()

        def move(step: Int): Any < Async =
            hiRef match
                case Present(hi) if navigable.nonEmpty =>
                    val next = math.max(0, math.min(navigable.size - 1, h + step))
                    hi.set(next)
                case _ => ()

        def jump(to: Int): Any < Async =
            hiRef match
                case Present(hi) if navigable.nonEmpty => hi.set(to)
                case _                                 => ()

        val keyHandler: KeyboardEvent => Any < Async = e =>
            e.key match
                case Keyboard.ArrowDown => move(1)
                case Keyboard.ArrowUp   => move(-1)
                case Keyboard.Home      => jump(0)
                case Keyboard.End       => jump(navigable.size - 1)
                case Keyboard.Enter | Keyboard.Space if hiRow >= 0 =>
                    rows(hiRow) match
                        case MenuRow.Item(it) => activate(it)
                        case _                => ()
                case _ => ()

        val rowUIs: List[UI] = rows.zipWithIndex.map {
            case (MenuRow.Sep, _) =>
                li.cssClass("p-menu-separator").role("separator")
            case (MenuRow.Heading(text), _) =>
                text match
                    case TextValue.Const(t) => li.cssClass("p-menu-submenu-label").role("presentation")(t)
                    case TextValue.Dyn(s) =>
                        li.cssClass("p-menu-submenu-label").role("presentation")(toChild(s.render(t => stringToUI(t))))
            case (MenuRow.Item(it), i) =>
                var row = li.cssClass("p-menu-item").role("presentation")
                if it.disabledFlag then row = row.cssClass("p-disabled").aria("disabled", "true")
                if i == hiRow then
                    row = row.cssClass("p-focus")
                    idV.foreach(base => row = row.id(s"$base-active"))
                val act: Maybe[Any < Async] =
                    if hiRef.isDefined && !it.disabledFlag then Present(activate(it)) else Absent
                row(toChild(MenuRender.itemContent("menu", it, act, Absent, roving = true)))
        }

        var list = ul.cssClass("p-menu-list").role("menu")
        if hiRow >= 0 then idV.foreach(base => list = list.aria("activedescendant", s"$base-active"))
        popupRefV match
            case Present(openRef) =>
                Overlay(openRef)
                    .matchWidth(false)
                    .animate(false)
                    .panelClass("p-menu")
                    .panelClass("p-component")
                    .panelClass("p-menu-overlay")
                    .onPanelKeyDown(keyHandler)(list(rowUIs.map(toChild)*))
                    .render
            case Absent =>
                if hiRef.isDefined then list = list.tabIndex(0).preventScrollKeys.onKeyDown(keyHandler)
                div.cssClass("p-menu").cssClass("p-component")(
                    toChild(list(rowUIs.map(toChild)*))
                )
        end match
    end body
end Menu

/** Package-internal flat row model of the plain Menu (grouped items flatten to
  * a heading followed by their children — Prime's Menu is a flat list).
  */
private[uic] enum MenuRow derives CanEqual:
    case Item(it: MenuItem)
    case Heading(text: TextValue)
    case Sep
end MenuRow

object Menu:
    /** An empty menu — add rows via [[Menu.items]]. */
    def apply(): Menu = new Menu()
