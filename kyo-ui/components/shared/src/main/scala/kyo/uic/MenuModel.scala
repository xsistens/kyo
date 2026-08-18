package kyo.uic

import kyo.*
import kyo.UI.*

/** One entry of the menu family (Menu, Menubar, TieredMenu, SplitButton,
  * SpeedDial; MegaMenu groups its items through [[MenuGroup]]): a label with an
  * optional leading icon, an optional `onSelect` effect, an optional `url`
  * (renders the row as a real anchor — Link precedent), a `disabled` state, and
  * nested `items` (a submenu — flattened to a labelled section in the plain
  * Menu, a nested floating panel in Menubar/TieredMenu).
  *
  * `MenuItem.separator` is the divider row (`li.p-<component>-separator`).
  */
final case class MenuItem private (
    labelV: TextValue,
    iconV: Maybe[IconGlyph] = Absent,
    disabledFlag: Boolean = false,
    urlV: Maybe[String] = Absent,
    actionV: Maybe[Any < Async] = Absent,
    itemsV: List[MenuItem] = Nil,
    separatorFlag: Boolean = false
):
    /** Leading icon (`.p-<component>-item-icon`). */
    def icon(glyph: IconGlyph): MenuItem = copy(iconV = Present(glyph))

    /** Renders the row non-interactive (`.p-disabled`; skipped by keyboard
      * navigation).
      */
    def disabled(v: Boolean): MenuItem = copy(disabledFlag = v)

    /** Navigation target: the row renders as a real `<a href=...>`, so plain
      * clicks (and Enter on the focused row) navigate natively.
      */
    def url(v: String): MenuItem = copy(urlV = Present(v))

    /** Runs `action` (any kyo `Async` effect) when the item is selected; the
      * hosting menu closes afterwards.
      */
    def onSelect(action: => Any < Async)(using Frame): MenuItem =
        copy(actionV = Present(Sync.defer(action)))

    /** Nested items: a labelled flat section in the plain Menu, a nested floating
      * submenu in Menubar/TieredMenu.
      */
    def items(is: MenuItem*): MenuItem = copy(itemsV = itemsV ++ is.toList)

    /** Package-internal: hosts (MegaMenu) carry an already-deferred effect over. */
    private[uic] def withAction(eff: Maybe[Any < Async]): MenuItem = copy(actionV = eff)
end MenuItem

object MenuItem:
    /** An item labelled `label`. */
    def apply(label: String): MenuItem = new MenuItem(TextValue.Const(label))

    /** An item whose label tracks `label` — re-renders in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def apply(label: Signal[String]): MenuItem = new MenuItem(TextValue.Dyn(label))

    /** Package-internal: build an item from an already-carried label carrier
      * (hosts such as [[MegaMenuItem.asMenuItem]] forward their own `TextValue`).
      */
    private[uic] def fromText(label: TextValue): MenuItem = new MenuItem(label)

    /** The divider row (`li.p-<component>-separator`, `role="separator"`). */
    val separator: MenuItem = new MenuItem(TextValue.Const(""), separatorFlag = true)
end MenuItem

/** One labelled group of items inside a [[MegaMenu]] panel column: the group
  * heading (`li.p-megamenu-submenu-label`) followed by its item rows.
  */
final case class MenuGroup private (
    labelV: TextValue,
    itemsV: List[MenuItem] = Nil
):
    /** Appends items to the group. */
    def items(is: MenuItem*): MenuGroup = copy(itemsV = itemsV ++ is.toList)
end MenuGroup

object MenuGroup:
    /** A group headed `label` — add rows via [[MenuGroup.items]]. */
    def apply(label: String): MenuGroup = new MenuGroup(TextValue.Const(label))

    /** A group whose heading tracks `label` — re-renders in place on emission (e.g.
      * a locale-driven `I18n.t` leaf).
      */
    def apply(label: Signal[String]): MenuGroup = new MenuGroup(TextValue.Dyn(label))
end MenuGroup

/** Package-internal machinery shared by the menu family. */
private[uic] object MenuRender:

    /** Paths (index chains from the root) of every item carrying a submenu, in
      * tree order — one open/closed `SignalRef[Boolean]` is allocated per path.
      */
    def submenuPaths(items: List[MenuItem], prefix: List[Int] = Nil): List[List[Int]] =
        items.zipWithIndex.flatMap { (it, i) =>
            val p = prefix :+ i
            if it.itemsV.nonEmpty then p :: submenuPaths(it.itemsV, p) else Nil
        }

    /** Subscribes to every submenu ref and rebuilds the tree on any change — ONE
      * reactive wrapper chain around the whole component, so Prime's direct-child
      * selectors (`.p-menubar-root-list > .p-menubar-item > ...`) stay intact (a
      * per-item subscription would interpose kyo's classless reactive `<span>`).
      */
    def renderAll(refs: List[(List[Int], SignalRef[Boolean])])(
        f: Map[List[Int], Boolean] => UI
    )(using Frame): UI =
        def loop(rest: List[(List[Int], SignalRef[Boolean])], acc: Map[List[Int], Boolean]): UI =
            rest match
                case Nil            => f(acc)
                case (p, r) :: tail => r.render(v => loop(tail, acc + (p -> v)))
        loop(refs, Map.empty)
    end renderAll

    /** Opens exactly the chain leading to `target` (every prefix of the path) and
      * closes everything else — this also flushes stale descendant state left
      * behind by per-level backdrop dismissal (the Overlay backdrop only writes
      * its own level's ref). `Absent` closes the whole tree.
      */
    def openExactly(
        refs: List[(List[Int], SignalRef[Boolean])],
        target: Maybe[List[Int]]
    )(using Frame): Unit < Async =
        Kyo.foreachDiscard(refs) { (p, r) =>
            r.set(target.exists(t => t.startsWith(p)))
        }

    /** Applies a [[MenuNav.Step]] to a keyboard host's signals: threads the
      * `OpenOp` through [[openExactly]], and — on `activate` (run the focused
      * leaf's action) or `dismiss` — collapses the submenu tree, closes the popup
      * if any, and clears the highlight. Plain movement just writes the new focus
      * path. Shared by every nested menu host (TieredMenu, ContextMenu, Menubar,
      * MegaMenu).
      */
    def applyStep(
        items: List[MenuItem],
        refs: Maybe[List[(List[Int], SignalRef[Boolean])]],
        focusRef: SignalRef[List[Int]],
        popupRef: Maybe[SignalRef[Boolean]],
        step: MenuNav.Step
    )(using Frame): Any < Async =
        val finish = step.activate || step.dismiss
        def openTo(t: Maybe[List[Int]]): Any < Async =
            refs match
                case Present(rs) => openExactly(rs, t)
                case Absent      => ()
        val applyOpen: Any < Async =
            step.open match
                case MenuNav.OpenOp.Keep      => ()
                case MenuNav.OpenOp.Close     => openTo(Absent)
                case MenuNav.OpenOp.OpenTo(p) => openTo(Present(p))
        val runActivate: Any < Async =
            if !step.activate then ()
            else
                MenuNav.itemAt(items, step.focus).flatMap(_.actionV) match
                    case Present(eff) => eff
                    case _            => ()
        val closePopup: Any < Async =
            if !finish then ()
            else
                popupRef match
                    case Present(r) => r.set(false)
                    case Absent     => ()
        for
            _ <- applyOpen
            _ <- runActivate
            _ <- if finish then openTo(Absent) else (): Any < Async
            _ <- closePopup
            _ <- focusRef.set(if finish then Nil else step.focus)
        yield ()
        end for
    end applyStep

    /** The shared row content: `div.p-<prefix>-item-content` >
      * `a.p-<prefix>-item-link` holding icon + label (+ trailing submenu glyph).
      * `activate` (when interactive) runs on click and on Enter with the row
      * link focused.
      */
    def itemContent(
        prefix: String,
        it: MenuItem,
        activate: Maybe[Any < Async],
        submenuIcon: Maybe[IconGlyph],
        roving: Boolean = false
    )(using Frame): UI =
        var link = a.cssClass(s"p-$prefix-item-link")
        it.urlV.foreach(u => link = link.href(Href.Path(u)))
        if !it.disabledFlag then
            // Roving hosts give the enclosing list the single tab stop and handle keys
            // centrally (the WAI-ARIA aria-activedescendant model the `.p-focus` CSS
            // expects); the legacy path makes every link its own tab stop with a
            // per-link Enter handler.
            link = link.tabIndex(if roving then -1 else 0)
            activate.foreach { eff =>
                link = link.onClick(eff)
                if !roving then
                    link = link.onKeyDown { e =>
                        e.key match
                            case Keyboard.Enter => eff
                            case _              => ()
                    }
                end if
            }
        end if
        val icon: List[UI] = it.iconV.toList.map(g => GlyphSvg(g, s"p-$prefix-item-icon", "p-icon"))
        val label: UI = it.labelV match
            case TextValue.Const(t) => span.cssClass(s"p-$prefix-item-label")(t)
            case TextValue.Dyn(s)   => s.render(t => span.cssClass(s"p-$prefix-item-label")(t))
        val subIcon: List[UI] =
            submenuIcon.toList.map(g => GlyphSvg(g, s"p-$prefix-submenu-icon", "p-icon"))
        div.cssClass(s"p-$prefix-item-content")(
            toChild(link(((icon :+ label) ++ subIcon).map(toChild)*))
        )
    end itemContent
end MenuRender
