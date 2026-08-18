package kyo.uic

import kyo.*
import kyo.UI.*

/** One root item of a [[MegaMenu]]: a label with optional icon whose panel
  * holds columns of labelled [[MenuGroup]]s — `column(groups*)` appends one
  * panel column (the grid divides Prime's 12-column raster evenly, so two
  * columns render `.p-megamenu-col-6`, three `.p-megamenu-col-4`, ...). An item
  * without columns is a plain row (activate via `onSelect`/`url`).
  */
final case class MegaMenuItem private (
    labelV: TextValue,
    iconV: Maybe[IconGlyph] = Absent,
    disabledFlag: Boolean = false,
    urlV: Maybe[String] = Absent,
    actionV: Maybe[Any < Async] = Absent,
    columnsV: List[List[MenuGroup]] = Nil
):
    /** Leading icon (`.p-megamenu-item-icon`). */
    def icon(glyph: IconGlyph): MegaMenuItem = copy(iconV = Present(glyph))

    /** Renders the root row non-interactive (`.p-disabled`). */
    def disabled(v: Boolean): MegaMenuItem = copy(disabledFlag = v)

    /** Navigation target for a column-less root row (real `<a href=...>`). */
    def url(v: String): MegaMenuItem = copy(urlV = Present(v))

    /** Runs `action` when a column-less root row is selected. */
    def onSelect(action: => Any < Async)(using Frame): MegaMenuItem =
        copy(actionV = Present(Sync.defer(action)))

    /** Appends one panel column holding the given groups. */
    def column(groups: MenuGroup*): MegaMenuItem = copy(columnsV = columnsV :+ groups.toList)

    /** The equivalent plain [[MenuItem]] of a column-less root row. */
    private[uic] def asMenuItem: MenuItem =
        var it = MenuItem.fromText(labelV)
        iconV.foreach(g => it = it.icon(g))
        urlV.foreach(u => it = it.url(u))
        it = it.disabled(disabledFlag)
        it.withAction(actionV)
    end asMenuItem
end MegaMenuItem

object MegaMenuItem:
    /** A root item labelled `label` — add panel columns via
      * [[MegaMenuItem.column]].
      */
    def apply(label: String): MegaMenuItem = new MegaMenuItem(TextValue.Const(label))

    /** A root item whose label tracks `label` — re-renders in place on emission
      * (e.g. a locale-driven `I18n.t` leaf).
      */
    def apply(label: Signal[String]): MegaMenuItem = new MegaMenuItem(TextValue.Dyn(label))
end MegaMenuItem

/** MegaMenu — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * MegaMenu anatomy: `div.p-megamenu.p-component.p-megamenu-horizontal|-vertical`
  * > `ul.p-megamenu-root-list[role=menubar]` of `li.p-megamenu-item` rows; the
  * active root item opens ONE `p-megamenu-overlay` panel holding a
  * `.p-megamenu-grid` of `.p-megamenu-col-N` columns, each column a stack of
  * `ul.p-megamenu-submenu` groups headed by `li.p-megamenu-submenu-label`), so
  * the extracted `@primeuix` megamenu CSS applies verbatim.
  *
  * The panel is a real [[Overlay]] anchored on the megamenu ROOT (the sheet
  * makes `.p-megamenu` the positioning context, so the panel spans the whole
  * bar — Prime's `left: 0; min-width: 100%`): a root-row click toggles it
  * (deliberate deviation from Prime's hover-open), an outside click or Escape
  * closes, the panel seeds focus. `orientation(Vertical)` renders the sheet's
  * vertical variant (root list stacked, panel to the side).
  *
  * Keyboard (grid navigation, see [[MegaNav]]): the root list is the single tab
  * stop. ArrowLeft/Right move Prime's `.p-focus` between root items (closing the
  * panel), ArrowDown opens the focused root's panel onto its first item; inside
  * a panel ArrowUp/Down move within a column (wrapping), ArrowLeft/Right move
  * between columns (row index clamped), Home/End jump, Enter/Space activate, and
  * Escape closes the panel back to the bar. The panel does NOT seed focus (the
  * root list keeps the keys); `id(...)` wires `aria-activedescendant`.
  *
  * Honest deferrals: no mobile/hamburger mode; typeahead is not implemented.
  */
final case class MegaMenu private (
    itemsV: List[MegaMenuItem] = Nil,
    orientationV: Orientation = Orientation.Horizontal,
    startV: Maybe[UI] = Absent,
    endV: Maybe[UI] = Absent,
    idV: Maybe[String] = Absent
) extends Node:
    type Self = MegaMenu

    /** Appends root items ([[MegaMenuItem]]). */
    def items(is: MegaMenuItem*): MegaMenu = copy(itemsV = itemsV ++ is.toList)

    /** Bar orientation: `Horizontal` (default) or the sheet's `Vertical` variant. */
    def orientation(v: Orientation): MegaMenu = copy(orientationV = v)

    /** Content before the root list (`div.p-megamenu-start`). */
    def start(ui: UI): MegaMenu = copy(startV = Present(ui))

    /** Content after the root list (`div.p-megamenu-end`). */
    def end(ui: UI): MegaMenu = copy(endV = Present(ui))

    /** Base id for the bar — enables `aria-activedescendant` (the focused row is
      * exposed as `s"$id-active"`). Keyboard navigation works without it.
      */
    def id(v: String): MegaMenu = copy(idV = Present(v))

    /** Root indices that open a panel (one open/closed ref each). */
    private[uic] def panelPaths: List[List[Int]] =
        itemsV.zipWithIndex.collect { case (it, i) if it.columnsV.nonEmpty => List(i) }

    private[uic] def render(using Frame): UI =
        val stat: UI = body(Map.empty.withDefaultValue(false), Absent, Nil, Absent)
        UI.mounted {
            for
                refs  <- Kyo.foreach(panelPaths)(p => Signal.initRef(false).map(p -> _))
                focus <- Signal.initRef(List.empty[Int])
            yield wired(refs.toList, focus)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam). */
    private[uic] def wired(
        refs: List[(List[Int], SignalRef[Boolean])],
        focus: SignalRef[List[Int]]
    )(using Frame): UI =
        focus.render { f =>
            MenuRender.renderAll(refs)(open => body(open.withDefaultValue(false), Present(refs), f, Present(focus)))
        }

    /** `.p-megamenu-col-N`: Prime divides the 12-column raster evenly. */
    private def colClass(count: Int): String =
        val n = count match
            case 1 => 12
            case 2 => 6
            case 3 => 4
            case 4 => 3
            case _ => 2
        s"p-megamenu-col-$n"
    end colClass

    private def body(
        open: Map[List[Int], Boolean],
        refs: Maybe[List[(List[Int], SignalRef[Boolean])]],
        focus: List[Int],
        focusRef: Maybe[SignalRef[List[Int]]]
    )(using Frame): UI =
        def closeAll: Any < Async =
            refs match
                case Present(rs) => MenuRender.openExactly(rs, Absent)
                case Absent      => ()

        def resetFocus: Any < Async =
            focusRef match
                case Present(fr) => fr.set(Nil)
                case Absent      => ()

        // Mouse activation (a leaf click) closes the panel tree and clears the highlight.
        def onActivated: Any < Async = for _ <- closeAll; _ <- resetFocus yield ()

        // Keyboard: MegaNav computes the next grid position; here we thread it onto
        // the open + focus signals, running the focused item's action on activation.
        def applyStep(step: MenuNav.Step): Any < Async =
            focusRef match
                case Present(fr) =>
                    val finish = step.activate || step.dismiss
                    def openTo(t: Maybe[List[Int]]): Any < Async =
                        refs match
                            case Present(rs) => MenuRender.openExactly(rs, t)
                            case Absent      => ()
                    val action: Maybe[Any < Async] =
                        if !step.activate then Absent
                        else
                            step.focus match
                                case r :: c :: i :: Nil =>
                                    val col = MegaNav.colItems(itemsV, r, c)
                                    if i >= 0 && i < col.size then col(i).actionV else Absent
                                case r :: Nil =>
                                    if r >= 0 && r < itemsV.size then itemsV(r).asMenuItem.actionV else Absent
                                case _ => Absent
                    for
                        _ <- step.open match
                            case MenuNav.OpenOp.Keep      => (): Any < Async
                            case MenuNav.OpenOp.Close     => openTo(Absent)
                            case MenuNav.OpenOp.OpenTo(p) => openTo(Present(p))
                        _ <- action match
                            case Present(eff) => eff
                            case Absent       => (): Any < Async
                        _ <- if finish then openTo(Absent) else (): Any < Async
                        _ <- fr.set(if finish then Nil else step.focus)
                    yield ()
                    end for
                case Absent => ()

        val keyHandler: KeyboardEvent => Any < Async = e =>
            focusRef match
                case Present(_) =>
                    MegaNav.onKey(itemsV, orientationV == Orientation.Horizontal, focus, e.key) match
                        case Present(step) => applyStep(step)
                        case Absent        => ()
                case Absent => ()

        // One column: its groups' navigable items share a single index space (into
        // MegaNav.colItems), so the `.p-focus`/id highlight tracks focus == (r,c,i).
        def columnGroups(r: Int, c: Int, groups: List[MenuGroup]): List[UI] =
            var navIdx = 0
            groups.map { g =>
                val heading: UI = g.labelV match
                    case TextValue.Const(t) => li.cssClass("p-megamenu-submenu-label").role("presentation")(t)
                    case TextValue.Dyn(s) =>
                        li.cssClass("p-megamenu-submenu-label").role("presentation")(toChild(s.render(t => stringToUI(t))))
                val rows: List[UI] = g.itemsV.map { it =>
                    if it.separatorFlag then li.cssClass("p-megamenu-separator").role("separator")
                    else
                        val myIdx = navIdx
                        if !it.disabledFlag then navIdx += 1
                        var row = li.cssClass("p-megamenu-item").role("presentation")
                        if it.disabledFlag then row = row.cssClass("p-disabled").aria("disabled", "true")
                        else if focus == List(r, c, myIdx) then
                            row = row.cssClass("p-focus")
                            idV.foreach(b => row = row.id(s"$b-active"))
                        end if
                        def activate: Any < Async =
                            for
                                _ <- it.actionV match
                                    case Present(eff) => eff
                                    case Absent       => (): Any < Async
                                _ <- onActivated
                            yield ()
                        val act: Maybe[Any < Async] =
                            if refs.isDefined && !it.disabledFlag then Present(activate) else Absent
                        row(toChild(MenuRender.itemContent("megamenu", it, act, Absent, roving = true)))
                }
                ul.cssClass("p-megamenu-submenu").role("menu")((heading :: rows).map(toChild)*)
            }
        end columnGroups

        val rowUIs: List[UI] = itemsV.zipWithIndex.map { (it, i) =>
            val p = List(i)
            if it.columnsV.nonEmpty then
                val isOpen = open(p)
                def toggle: Any < Async =
                    refs match
                        case Present(rs) =>
                            MenuRender.openExactly(rs, if isOpen then Absent else Present(p))
                        case Absent => ()
                var row = li
                    .cssClass("p-megamenu-item")
                    .role("presentation")
                    .aria("haspopup", "menu")
                    .aria("expanded", isOpen.toString)
                if isOpen then row = row.cssClass("p-megamenu-item-active")
                if focus == p then
                    row = row.cssClass("p-focus")
                    idV.foreach(b => row = row.id(s"$b-active"))
                if it.disabledFlag then row = row.cssClass("p-disabled").aria("disabled", "true")
                val act: Maybe[Any < Async] =
                    if refs.isDefined && !it.disabledFlag then Present(toggle) else Absent
                val subIcon = if orientationV == Orientation.Vertical then Icons.angleRight else Icons.angleDown
                val content =
                    MenuRender.itemContent("megamenu", it.asMenuItem, act, Present(subIcon), roving = true)
                // Host-gated overlay (renderOpen): the wired render already subscribes
                // to every panel ref, so the overlay must not subscribe again. Roving
                // keeps focus on the root list, so the panel must NOT seed focus.
                val panel: List[UI] = refs match
                    case Present(rs) if isOpen =>
                        rs.collectFirst { case (`p`, ref) => ref } match
                            case Some(ref) =>
                                val anchor =
                                    if orientationV == Orientation.Vertical then OverlayAnchor.RightStart
                                    else OverlayAnchor.BottomStart
                                val cols: List[UI] = it.columnsV.zipWithIndex.map { (groups, c) =>
                                    div.cssClass(colClass(it.columnsV.size))(columnGroups(i, c, groups).map(toChild)*)
                                }
                                List(
                                    Overlay(ref)
                                        .anchor(anchor)
                                        .matchWidth(false)
                                        .animate(false)
                                        .seedFocus(false)
                                        .panelClass("p-megamenu-overlay")
                                        .apply(div.cssClass("p-megamenu-grid")(cols.map(toChild)*))
                                        .renderOpen
                                )
                            case None => Nil
                    case _ => Nil
                row((content :: panel).map(toChild)*)
            else
                var row = li.cssClass("p-megamenu-item").role("presentation")
                if focus == p then
                    row = row.cssClass("p-focus")
                    idV.foreach(b => row = row.id(s"$b-active"))
                if it.disabledFlag then row = row.cssClass("p-disabled").aria("disabled", "true")
                val plain = it.asMenuItem
                def activate: Any < Async =
                    for
                        _ <- plain.actionV match
                            case Present(eff) => eff
                            case Absent       => (): Any < Async
                        _ <- onActivated
                    yield ()
                val act: Maybe[Any < Async] =
                    if refs.isDefined && !it.disabledFlag then Present(activate) else Absent
                row(toChild(MenuRender.itemContent("megamenu", plain, act, Absent, roving = true)))
            end if
        }

        val startUI: List[UI] = startV.toList.map(u => div.cssClass("p-megamenu-start")(toChild(u)))
        val endUI: List[UI]   = endV.toList.map(u => div.cssClass("p-megamenu-end")(toChild(u)))
        var list              = ul.cssClass("p-megamenu-root-list").role("menubar")
        if focusRef.isDefined then list = list.tabIndex(0).preventScrollKeys.onKeyDown(keyHandler)
        if focus.nonEmpty then idV.foreach(b => list = list.aria("activedescendant", s"$b-active"))
        val listUI: UI = list(rowUIs.map(toChild)*)
        div
            .cssClass("p-megamenu")
            .cssClass("p-component")
            .cssClass(s"p-megamenu-${orientationV.token}")(
                ((startUI :+ listUI) ++ endUI).map(toChild)*
            )
    end body
end MegaMenu

object MegaMenu:
    /** An empty mega menu — add root items via [[MegaMenu.items]]. */
    def apply(): MegaMenu = new MegaMenu()
