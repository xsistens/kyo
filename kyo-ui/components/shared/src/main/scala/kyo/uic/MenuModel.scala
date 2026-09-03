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
    disabledFlag: Maybe[BoolValue] = Absent,
    urlV: Maybe[String] = Absent,
    actionV: Maybe[Any < Async] = Absent,
    itemsV: List[MenuItem] = Nil,
    separatorFlag: Boolean = false
):
    /** Leading icon (`.p-<component>-item-icon`). */
    def icon(glyph: IconGlyph): MenuItem = copy(iconV = Present(glyph))

    /** Renders the row non-interactive (`.p-disabled`; skipped by keyboard
      * navigation).
      *
      * A `Signal[Boolean]` is what a CONTEXT menu needs, and the reason this slot
      * carries the union while several plain-`Boolean` `disabled` slots elsewhere do
      * not: every other one describes the state of the page, while a menu item's
      * describes its TARGET, and the target changes with each right-click. Rebuilding
      * the item list per target is not the alternative — the host allocates its open
      * state in its own unkeyed `UI.mounted`, so re-rendering it would close the menu
      * on the click that opened it.
      *
      * Resolved by the host BEFORE it builds rows, through
      * [[MenuRender.resolveDisabled]], so everything downstream — the navigable index
      * set, `MenuNav.skip`, the `.p-disabled` class, whether an activate effect is
      * attached — stays a pure function of a plain `Boolean` and reads it through
      * `constTrue`. A host that forgets to resolve reads `false`, which is the same
      * trap `BoolValue` carries everywhere and is pinned per host by a test.
      */
    def disabled(v: Boolean | Signal[Boolean]): MenuItem =
        copy(disabledFlag = Present(ReactiveValue(v)))

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

    /** Package-internal: [[MenuRender.resolveDisabled]] pins the resolved slot back in.
      * A writer rather than `copy`, because the constructor is private and `MenuRender`
      * is not this class.
      */
    private[uic] def withDisabled(v: Maybe[BoolValue]): MenuItem = copy(disabledFlag = v)

    /** Package-internal: replaces the submenu wholesale (the resolution rebuilds a
      * branch), where the public `items` appends.
      */
    private[uic] def withItems(is: List[MenuItem]): MenuItem = copy(itemsV = is)
end MenuItem

object MenuItem:
    /** An item labelled `label`. A `Signal[String]` re-renders the label in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def apply(label: String | Signal[String]): MenuItem = new MenuItem(ReactiveValue(label))

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

    /** Package-internal: replaces the items wholesale (see [[MenuItem.withItems]]). */
    private[uic] def withItems(is: List[MenuItem]): MenuGroup = copy(itemsV = is)
end MenuGroup

object MenuGroup:
    /** A group headed by `label`. A `Signal[String]` re-renders the heading in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def apply(label: String | Signal[String]): MenuGroup = new MenuGroup(ReactiveValue(label))

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

    /** Paths (index chains from the root) of every item whose `disabled` is reactive,
      * in tree order.
      */
    private def dynamicDisabled(items: List[MenuItem], prefix: List[Int] = Nil): List[(List[Int], Signal[Boolean])] =
        items.zipWithIndex.flatMap { (it, i) =>
            val p    = prefix :+ i
            val here = it.disabledFlag.dynSig.toList.map(s => (p, s))
            here ++ dynamicDisabled(it.itemsV, p)
        }

    /** Rewrites the item at `path` with its `disabled` pinned to `v`. */
    private def pinDisabled(items: List[MenuItem], path: List[Int], v: Boolean): List[MenuItem] =
        path match
            case Nil => items
            case i :: rest =>
                items.zipWithIndex.map { (it, j) =>
                    if j != i then it
                    else if rest.isEmpty then it.withDisabled(Present(BoolValue.Const(v)))
                    else it.withItems(pinDisabled(it.itemsV, rest, v))
                }
        end match
    end pinDisabled

    /** Resolves every reactive `disabled` in the tree before the host builds rows, and
      * hands `f` a list whose slots are all `Const`.
      *
      * The value-level [[BoolValue.reactive]] over a LIST, and subscribed the same way
      * [[renderAll]] subscribes submenu state: ONE chain around the whole component
      * rather than a subscription per row. That is not a micro-optimisation — a per-row
      * `Signal.render` interposes kyo's classless reactive `<span>` between the list and
      * its rows, and Prime's direct-child selectors (`.p-menubar-root-list >
      * .p-menubar-item > ...`) stop matching.
      *
      * With no reactive slot anywhere this adds no subscription at all and calls `f`
      * directly, so a menu of constants renders exactly as it did.
      */
    def resolveDisabled(items: List[MenuItem])(f: List[MenuItem] => UI)(using Frame): UI =
        chain(dynamicDisabled(items), items)(pinDisabled)(f)

    /** The subscription chain itself, shared by the two traversals: nest one
      * `Signal.render` per reactive slot, pinning each value into the accumulator, and
      * call `f` once at the bottom. With `dyns` empty it is `f(seed)` and adds no region.
      */
    private def chain[T](dyns: List[(List[Int], Signal[Boolean])], seed: T)(
        pin: (T, List[Int], Boolean) => T
    )(f: T => UI)(using Frame): UI =
        def loop(rest: List[(List[Int], Signal[Boolean])], acc: T): UI =
            rest match
                case Nil            => f(acc)
                case (p, s) :: tail => s.render(v => loop(tail, pin(acc, p, v)))
        loop(dyns, seed)
    end chain

    /** [[resolveDisabled]] for MegaMenu, whose leaves sit two containers deeper —
      * `MegaMenuItem.columnsV : List[List[MenuGroup]]`, each group holding the
      * `MenuItem`s. The root rows carry their own plain-`Boolean` `disabled` and are
      * untouched; only the leaves have the reactive slot. Paths are
      * `[root, column, group, item]`.
      */
    def resolveMegaDisabled(items: List[MegaMenuItem])(f: List[MegaMenuItem] => UI)(using Frame): UI =
        val dyns =
            for
                (root, ri)  <- items.zipWithIndex
                (col, ci)   <- root.columnsV.zipWithIndex
                (group, gi) <- col.zipWithIndex
                (item, ii)  <- group.itemsV.zipWithIndex
                sig         <- item.disabledFlag.dynSig.toList
            yield (List(ri, ci, gi, ii), sig)

        def pin(acc: List[MegaMenuItem], path: List[Int], v: Boolean): List[MegaMenuItem] =
            path match
                case ri :: ci :: gi :: ii :: Nil =>
                    acc.updated(
                        ri, {
                            val root = acc(ri)
                            val col  = root.columnsV(ci)
                            val grp  = col(gi)
                            val its =
                                grp.itemsV.updated(ii, grp.itemsV(ii).withDisabled(Present(BoolValue.Const(v))))
                            root.withColumns(root.columnsV.updated(ci, col.updated(gi, grp.withItems(its))))
                        }
                    )
                case _ => acc

        chain(dyns, items)(pin)(f)
    end resolveMegaDisabled

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
        if !it.disabledFlag.constTrue then
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
