package kyo.uic

import kyo.*
import kyo.UI.Keyboard

/** Pure WAI-ARIA keyboard-navigation state machine shared by the menu family
  * (Menu, Menubar, TieredMenu, ContextMenu, MegaMenu) and by [[CascadeSelect]], whose nested
  * option panels are the same machine under a different skin. It operates over a tree of
  * nodes and the current focus PATH (an index chain from the root; `Nil` = nothing focused)
  * and returns the [[MenuNav.Step]] the host maps onto its focus + submenu-open signals. No
  * DOM and no effects — the semantics are unit-tested directly (`MenuNavTest`).
  *
  * The tree is reached through [[Nodes]] rather than being a [[MenuItem]] list, because a
  * cascade's options nest exactly the way a menu's items do and one navigation is better than
  * two that drift. A type needs to answer two questions to navigate: what a node's children
  * are, and whether the keyboard skips it.
  *
  * Root axis is parameterised: a [[Menubar]] navigates its roots horizontally
  * (Left/Right), every other menu navigates its top level vertically (Up/Down);
  * submenus are always vertical. The open/closed submenu tree is expressed as
  * [[OpenOp]], matching [[MenuRender.openExactly]] (`Close` ⇒ `openExactly(Absent)`,
  * `OpenTo(p)` ⇒ `openExactly(Present(p))`).
  */
private[uic] object MenuNav:

    /** What this needs of a node to navigate a tree of them: its children, and whether the
      * keyboard passes over it (a separator, a disabled row).
      */
    trait Nodes[N]:
        def children(n: N): List[N]
        def skip(n: N): Boolean

    given Nodes[MenuItem] with
        def children(n: MenuItem): List[MenuItem] = n.itemsV
        def skip(n: MenuItem): Boolean            = n.separatorFlag || n.disabledFlag.constTrue

    /** Root-level navigation axis. */
    enum Orientation derives CanEqual:
        case Vertical, Horizontal

    /** What a key does to the submenu-open tree (see [[MenuRender.openExactly]]). */
    enum OpenOp derives CanEqual:
        case Keep
        case Close
        case OpenTo(path: List[Int])
    end OpenOp

    /** The outcome of one key press. `focus` is the new highlight path; `activate`
      * asks the host to run the focused leaf's action (then close); `dismiss` asks
      * the host to close the whole popup (root Escape).
      */
    final case class Step(
        focus: List[Int],
        open: OpenOp = OpenOp.Keep,
        activate: Boolean = false,
        dismiss: Boolean = false
    ) derives CanEqual

    // ---- tree queries ----

    /** The item list at `parent` — the root list when empty, the addressed item's
      * children deeper (an out-of-range index yields an empty level).
      */
    private def levelItems[N](items: List[N], parent: List[Int])(using nodes: Nodes[N]): List[N] =
        parent.foldLeft(items) { (its, i) =>
            if i >= 0 && i < its.size then nodes.children(its(i)) else Nil
        }

    /** Enabled, non-separator sibling indices at `parent` (the keyboard-reachable ones). */
    private def navigable[N](items: List[N], parent: List[Int])(using nodes: Nodes[N]): List[Int] =
        levelItems(items, parent).zipWithIndex.collect {
            case (it, i) if !nodes.skip(it) => i
        }

    /** The item addressed by `path` (`Absent` for the root or an out-of-range
      * index) — hosts use it to resolve the focused leaf's action on activation.
      */
    private[uic] def itemAt[N: Nodes](items: List[N], path: List[Int]): Maybe[N] =
        if path.isEmpty then Absent
        else
            val lvl = levelItems(items, path.init)
            val i   = path.last
            if i >= 0 && i < lvl.size then Present(lvl(i)) else Absent

    /** The first row a highlight may sit on under `parent` (the root list where it is empty).
      *
      * What a host seeds when its panel opens: opening has to land ON a row, or the arrow that
      * opened it has moved nothing and the reader presses it twice to reach the first one.
      */
    private[uic] def firstFocus[N: Nodes](items: List[N], parent: List[Int] = Nil): Maybe[List[Int]] =
        firstChild(items, parent)

    private def hasChildren[N](items: List[N], path: List[Int])(using nodes: Nodes[N]): Boolean =
        itemAt(items, path).exists(n => nodes.children(n).nonEmpty)

    private def firstChild[N: Nodes](items: List[N], parent: List[Int]): Maybe[List[Int]] =
        Maybe.fromOption(navigable(items, parent).headOption).map(parent :+ _)

    private def lastChild[N: Nodes](items: List[N], parent: List[Int]): Maybe[List[Int]] =
        Maybe.fromOption(navigable(items, parent).lastOption).map(parent :+ _)

    private def parentOf(focus: List[Int]): List[Int] = if focus.isEmpty then Nil else focus.init

    /** Move within the current level, wrapping; falls back to the first navigable
      * sibling when the current index is not itself navigable.
      */
    private def move[N: Nodes](items: List[N], focus: List[Int], dir: Int): List[Int] =
        val parent = parentOf(focus)
        val nav    = navigable(items, parent)
        if nav.isEmpty then focus
        else
            val cur  = focus.lastOption.getOrElse(-1)
            val idx  = nav.indexOf(cur)
            val next = if idx < 0 then 0 else (idx + dir + nav.size) % nav.size
            parent :+ nav(next)
        end if
    end move

    // ---- entering / leaving submenus ----

    /** Open the focused item's submenu and land on its first child (`Absent` if the
      * item has no navigable children).
      */
    private def enterSubmenu[N: Nodes](items: List[N], focus: List[Int]): Maybe[Step] =
        firstChild(items, focus).map(fc => Step(focus = fc, open = OpenOp.OpenTo(fc)))

    /** Leave the current submenu: focus the parent row and close the parent's
      * submenu while keeping ancestors open. `Absent` at depth ≤ 1 (no submenu to
      * leave).
      */
    private def leaveSubmenu(focus: List[Int]): Maybe[Step] =
        if focus.size <= 1 then Absent
        else
            val parent     = focus.init
            val keepOpenTo = parent.init
            val op         = if keepOpenTo.isEmpty then OpenOp.Close else OpenOp.OpenTo(keepOpenTo)
            Present(Step(focus = parent, open = op))

    /** Cross to a sibling root (menubar Left/Right from within/at the root),
      * opening its submenu when it has one.
      */
    private def gotoRoot[N: Nodes](items: List[N], focus: List[Int], dir: Int, openIt: Boolean): Step =
        val root = move(items, focus.take(1), dir)
        if openIt then
            firstChild(items, root) match
                case Present(fc) => Step(focus = fc, open = OpenOp.OpenTo(fc))
                case Absent      => Step(focus = root, open = OpenOp.Close)
        else Step(focus = root, open = OpenOp.Close)
        end if
    end gotoRoot

    // ---- the key handler ----

    def onKey[N: Nodes](
        items: List[N],
        rootOrientation: Orientation,
        focus: List[Int],
        key: Keyboard
    ): Maybe[Step] =
        val depth          = focus.size
        val atRoot         = depth <= 1
        val rootHorizontal = rootOrientation == Orientation.Horizontal

        import Keyboard.*
        key match
            // vertical movement (any vertical level: submenus always, roots unless horizontal)
            case ArrowDown if !(atRoot && rootHorizontal) =>
                Present(Step(focus = if focus.isEmpty then firstChild(items, Nil).getOrElse(Nil) else move(items, focus, +1)))
            case ArrowUp if !(atRoot && rootHorizontal) =>
                Present(Step(focus = if focus.isEmpty then lastChild(items, Nil).getOrElse(Nil) else move(items, focus, -1)))

            // horizontal root (menubar): Left/Right move roots (closing subs); Down/Up open the submenu
            case ArrowRight if atRoot && rootHorizontal =>
                Present(gotoRoot(items, focus, +1, openIt = false))
            case ArrowLeft if atRoot && rootHorizontal =>
                Present(gotoRoot(items, focus, -1, openIt = false))
            case ArrowDown if atRoot && rootHorizontal =>
                Present(enterSubmenu(items, focus).getOrElse(Step(focus)))
            case ArrowUp if atRoot && rootHorizontal =>
                Present(lastChild(items, focus).map(lc => Step(focus = lc, open = OpenOp.OpenTo(lc))).getOrElse(Step(focus)))

            // Right/Left inside a vertical context
            case ArrowRight =>
                enterSubmenu(items, focus).orElse {
                    // a leaf inside a menubar submenu: cross to the next root and open it
                    if rootHorizontal && depth == 2 then Present(gotoRoot(items, focus, +1, openIt = true))
                    else Absent
                }
            case ArrowLeft =>
                // depth-2 of a menubar: cross to the previous root and open it; otherwise climb out
                if rootHorizontal && depth == 2 then Present(gotoRoot(items, focus, -1, openIt = true))
                else leaveSubmenu(focus)

            case Home => Present(Step(focus = firstChild(items, parentOf(focus)).getOrElse(focus)))
            case End  => Present(Step(focus = lastChild(items, parentOf(focus)).getOrElse(focus)))

            case Enter | Space =>
                if focus.isEmpty then Absent
                else if hasChildren(items, focus) then enterSubmenu(items, focus)
                else Present(Step(focus, activate = true))

            case Escape =>
                leaveSubmenu(focus).orElse(Present(Step(focus = Nil, open = OpenOp.Close, dismiss = true)))

            // Escape leaves ONE level; Tab leaves the widget, so it closes every level at once. The
            // browser has already moved focus by the time the close lands, which is what makes this
            // the right place for it: the menu's keys live on the element the Tab is carrying the
            // reader away from, so a panel left open behind them would answer nothing.
            case Tab =>
                Present(Step(focus = Nil, open = OpenOp.Close, dismiss = true))

            case _ => Absent
        end match
    end onKey
end MenuNav
