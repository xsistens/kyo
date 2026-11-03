package kyo.uic

import kyo.*
import kyo.UI.*

/** One entry of the menu family (Menu, Menubar, TieredMenu, SplitButton,
  * SpeedDial; MegaMenu groups its items through [[MenuGroup]]): a label with an
  * optional leading icon, an optional `onSelect` effect, an optional `url`
  * (renders the row as a real anchor, the Link precedent), a `disabled` state,
  * and nested `items` (a submenu: flattened to a labelled section in the plain
  * Menu, a nested floating panel in Menubar/TieredMenu).
  *
  * `MenuItem.separator` is the divider row (`li.p-<component>-separator`).
  */
final case class MenuItem private (
    labelV: TextValue,
    iconV: Maybe[IconGlyph] = Absent,
    disabledFlag: Maybe[BoolValue] = Absent,
    urlV: Maybe[String] = Absent,
    actionV: Maybe[UI.Action] = Absent,
    itemsV: List[MenuItem] = Nil,
    separatorFlag: Boolean = false,
    currentFlag: Maybe[BoolValue] = Absent
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
      * the item list per target is not the alternative: the host allocates its open
      * state in its own unkeyed `UI.mounted`, so re-rendering it would close the menu
      * on the click that opened it.
      *
      * Resolved by the host BEFORE it builds rows, through
      * [[MenuRender.readDisabled]], so everything downstream (the navigable index
      * set, `MenuNav.skip`, the `.p-disabled` class, whether an activate effect is
      * attached) stays a pure function of a plain `Boolean` and reads it through
      * `constTrue`. A host that forgets to resolve reads `false`, which is the same
      * trap `BoolValue` carries everywhere and is pinned per host by a test.
      */
    def disabled(v: Boolean | Signal[Boolean]): MenuItem =
        copy(disabledFlag = Present(ReactiveValue(v)))

    /** Navigation target: the row renders as a real `<a href=...>`, so plain
      * clicks (and Enter on the focused row) navigate natively.
      */
    def url(v: String): MenuItem = copy(urlV = Present(v))

    /** THIS row is the page the reader is on: `aria-current="page"` on the row's link plus
      * `.p-uic-menu-item-current` on the row, for a menu carrying [[url]] rows, which is what
      * a menu becomes the moment it is a nav, and the one thing such a menu is for.
      *
      * Distinct from `.p-focus`, which is the roving keyboard highlight the component moves as
      * the reader arrows through and drops when the list loses focus. The two co-occur: arrowing
      * past the current page must not hide which page that is.
      *
      * Takes a `Signal[Boolean]` because the current route is one, and both slots it writes are
      * kyo-ui CHANNELS: `cssClass(name, sig)` toggles the class through `classList.toggle` and
      * `aria(name, sig)` patches the attribute in place, each with NO re-render. So a nav is
      * built once and stays built: navigating does not rebuild the list, and the keyboard
      * position survives the navigation that a `Routes.current.render { … }` around the whole
      * strip would have thrown away.
      *
      * That is also why this needs none of [[MenuRender.readDisabled]]'s machinery, which
      * `disabled` does need: being current changes nothing structural (not the navigable index
      * set, not `MenuNav.skip`, not whether an activate effect is attached), so there is nothing
      * downstream that must read it as a plain `Boolean`.
      *
      * Rendered by every host that builds `MenuItem` rows: Menu, Menubar, TieredMenu, ContextMenu
      * and MegaMenu's leaves. MegaMenu's ROOT rows are [[MegaMenuItem]]s, a different carrier, and
      * carry no such slot; SpeedDial renders buttons rather than links and carries none either.
      */
    def current(v: Boolean | Signal[Boolean]): MenuItem =
        copy(currentFlag = Present(ReactiveValue(v)))

    /** Runs `action` (any kyo `Async` effect) when the item is selected; the
      * hosting menu closes afterwards.
      */
    def onSelect(action: => UI.Action)(using Frame): MenuItem =
        copy(actionV = Present(Callback(action)))

    /** Nested items: a labelled flat section in the plain Menu, a nested floating
      * submenu in Menubar/TieredMenu. Each is an item or `_.item(...)` (see [[MenuItemScope]]).
      */
    def items(is: MenuItemEntry*): MenuItem = copy(itemsV = itemsV ++ MenuItemScope.resolve(is))

    /** Package-internal: hosts (MegaMenu) carry an already-deferred effect over. */
    private[uic] def withAction(eff: Maybe[UI.Action]): MenuItem = copy(actionV = eff)

    /** Package-internal: [[MenuRender.readDisabled]] pins the resolved slot back in.
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
    // Lets a host's one region skip a resolution that pins the slots it already drew, and a caller hold items in a signal.
    given CanEqual[MenuItem, MenuItem] = CanEqual.derived

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
    /** Appends items to the group. Each is an item or `_.item(...)` (see [[MenuItemScope]]). */
    def items(is: MenuItemEntry*): MenuGroup = copy(itemsV = itemsV ++ MenuItemScope.resolve(is))

    /** Package-internal: replaces the items wholesale (see [[MenuItem.withItems]]). */
    private[uic] def withItems(is: List[MenuItem]): MenuGroup = copy(itemsV = is)
end MenuGroup

object MenuGroup:
    /** A group headed by `label`. A `Signal[String]` re-renders the heading in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def apply(label: String | Signal[String]): MenuGroup = new MenuGroup(ReactiveValue(label))

end MenuGroup

/** A menu item, built directly or as `_.item(...)` or `_.item.separator` (see [[MenuItemScope]]). */
type MenuItemEntry = Entry[MenuItem, MenuItemScope]

/** A [[MenuItemEntry]] or a signal of items, for a menu whose rows change while it is shown (see [[LiveEntry]]). */
type MenuItemLiveEntry = LiveEntry[MenuItem, MenuItemScope]

/** What `_` stands for in a setter that takes menu items: `_.item("Save")` and `_.item.separator`, the same as [[MenuItem]]'s own
  * constructors. Taken by every host of [[MenuItem]] (Menu, Menubar, TieredMenu, ContextMenu, SplitButton, SpeedDial) and by the
  * nested `items` of an item or a [[MenuGroup]].
  */
final class MenuItemScope private ():
    /** `item(label)` builds an item, `item.separator` the divider row. */
    object item:
        /** An item labelled `label`, as [[MenuItem.apply]]. */
        def apply(label: String | Signal[String]): MenuItem = MenuItem(label)

        /** The divider row, as [[MenuItem.separator]]. */
        def separator: MenuItem = MenuItem.separator
    end item
end MenuItemScope

object MenuItemScope:
    private val scope = new MenuItemScope

    private[uic] def resolve(entries: Seq[MenuItemEntry]): List[MenuItem] = Entry.resolve(scope, entries)

    private[uic] def reading(entries: Seq[MenuItemLiveEntry])(using Frame): ReactiveValue[List[MenuItem]] = Entry.reading(scope, entries)
end MenuItemScope

/** A titled group of a MegaMenu column, built directly or as `_.group(...)` (see [[MenuGroupScope]]). */
type MenuGroupEntry = Entry[MenuGroup, MenuGroupScope]

/** What `_` stands for in [[MegaMenuItem.column]]: `_.group("Heading")`, as [[MenuGroup.apply]]. */
final class MenuGroupScope private ():
    /** A group headed by `label`, as [[MenuGroup.apply]]. */
    def group(label: String | Signal[String]): MenuGroup = MenuGroup(label)
end MenuGroupScope

object MenuGroupScope:
    private val scope = new MenuGroupScope

    private[uic] def resolve(entries: Seq[MenuGroupEntry]): List[MenuGroup] = Entry.resolve(scope, entries)
end MenuGroupScope

/** A host's positional state (an open path, a highlighted row) together with the root items it was written against, so the
  * state belongs to an ITEM rather than to a place in the list.
  *
  * Positions are index chains, and the handlers that write them compute them against the items of the render they came
  * from. A list that takes a signal among its items can move under them: an item the signal adds before the open one
  * would otherwise hand the open submenu, or the highlight, to its neighbour. So each write records the RAW root items
  * of its render (`Snap.roots`), and every reader translates the value onto the roots it shows now ([[Snap.at]]),
  * matching items by reference identity. An item that is gone takes its state with it.
  *
  * ONE ref holds the value and its roots, so a reader never sees a value with the wrong roots. `Absent` roots mean
  * "relative to whatever the host shows", the meaning a value written before any render (an `init`) has. Writes go
  * through a [[View]], which a host builds in its one region from the snap it read and the roots it shows.
  */
final private[uic] class Anchored[S] private (ref: SignalRef[Anchored.Snap[S]]):

    /** The value and its roots, for the host's one region. */
    def reading: ReactiveValue[Anchored.Snap[S]] = ReactiveValue.Dyn(ref)

    /** The value as last written, untranslated: for tests and for state that means the same against any roots. */
    def get(using Frame): S < Sync = ref.get.map(_.value)

    /** Writes `v` against whatever roots the host shows next, the meaning an `init` has: for a value that is the same
      * against any roots (nothing open, no highlight), or for a writer with no render to anchor to (a test seeding state).
      */
    def reset(v: S)(using Frame): UI.Action = ref.set(Anchored.Snap(v, Absent))

    /** The state as the host shows it over `roots`, with writers that record those roots. */
    def view(snap: Anchored.Snap[S], roots: List[AnyRef])(using Anchored.Rebase[S]): Anchored.View[S] =
        new Anchored.View(ref, roots, snap.at(roots))
end Anchored

private[uic] object Anchored:

    def init[S](v: S)(using Frame): Anchored[S] < Sync = Signal.initRef(Snap(v, Absent)).map(new Anchored(_))

    /** A value and the raw root items it was written against. */
    final case class Snap[S](value: S, roots: Maybe[List[AnyRef]]):
        /** The value translated onto `now`. */
        def at(now: List[AnyRef])(using r: Rebase[S]): S =
            roots match
                case Present(from) if !(from eq now) => r.rebase(value, from, now)
                case _                               => value
    end Snap

    given [S]: CanEqual[Snap[S], Snap[S]] = CanEqual.derived

    /** How a value moves when its roots do: every root index it names is looked up again by identity. */
    trait Rebase[S]:
        def rebase(v: S, from: List[AnyRef], to: List[AnyRef]): S

    /** Where root `i` of `from` sits in `to`, by the strongest match there is:
      *
      *   1. the same item (`eq`), at the same place if it is still there, else at the first place it is;
      *   2. an equal item (`equals`), the same way, since a signal may emit its items again as new values;
      *   3. the same place, when the list kept its length and the item there now matches none of the old ones: an item
      *      rebuilt where it stood, as a label that carries a count is on every emission.
      *
      * Else nowhere, and the state the item had goes with it.
      */
    def index(i: Int, from: List[AnyRef], to: List[AnyRef]): Maybe[Int] =
        if i < 0 || i >= from.size then Absent
        else
            val item                                                = from(i)
            def find(same: (AnyRef, AnyRef) => Boolean): Maybe[Int] =
                if i < to.size && same(to(i), item) then Present(i)
                else
                    val j = to.indexWhere(same(_, item))
                    if j >= 0 then Present(j) else Absent
            def rebuiltInPlace: Maybe[Int] =
                if from.size == to.size && !from.exists(f => (f eq to(i)) || f.equals(to(i))) then Present(i) else Absent
            find(_ eq _).orElse(find(_.equals(_))).orElse(rebuiltInPlace)
        end if
    end index

    /** A path from the root, `Nil` for none; a path whose root item is gone becomes `Nil`. Only the head moves: below the
      * root the items are the root item's own, which a signal among the roots cannot reorder.
      */
    given Rebase[List[Int]] = (v, from, to) =>
        v match
            case Nil       => Nil
            case h :: rest => index(h, from, to).fold(Nil)(_ :: rest)

    /** A set of paths; a path whose root item is gone drops out. */
    given Rebase[Set[List[Int]]] = (v, from, to) =>
        v.flatMap {
            case Nil       => Set.empty[List[Int]]
            case h :: rest => index(h, from, to).fold(Set.empty[List[Int]])(j => Set(j :: rest))
        }

    /** The state over the roots a host shows: `value` translated onto them, and writers that store against them. */
    final class View[S] private[Anchored] (ref: SignalRef[Snap[S]], roots: List[AnyRef], val value: S)(using Rebase[S]):

        /** Stores `v`, a value computed against the roots this view shows. */
        def set(v: S)(using Frame): UI.Action = ref.set(Snap(v, Present(roots)))

        /** Stores `f` of the value as it stands now, translated onto these roots first, in one write. */
        def update(f: S => S)(using Frame): UI.Action =
            ref.updateAndGet(s => Snap(f(s.at(roots)), Present(roots))).unit
    end View
end Anchored

/** The rows a host draws beside the RAW root items they were resolved from, which its [[Anchored]] state is matched against
  * (see [[MenuRender.readRooted]]).
  */
final private[uic] case class Rooted[A](roots: List[AnyRef], items: List[A])

private[uic] object Rooted:
    given [A]: CanEqual[Rooted[A], Rooted[A]] = CanEqual.derived

/** Which submenus of a host are open: ONE anchored set of open paths (index chains from the root), however the items
  * change. A ref per path could only be allocated for the paths of the items the host was built with, so rows that arrive
  * through a signal would have none; and the paths follow their items ([[Anchored]]), so an item a signal adds before an
  * open submenu leaves that submenu open.
  *
  * A host reads [[reading]] in its one region beside its other state and builds a [[Submenus.View]] over the roots it
  * shows; a row asks `view.open(path)`. A submenu panel closes its own path, and with it everything below, through
  * [[Submenus.View.close]].
  */
final private[uic] class Submenus private (paths: Anchored[Set[List[Int]]]):

    /** The open paths and their roots, for the host's one region. */
    def reading: ReactiveValue[Anchored.Snap[Set[List[Int]]]] = paths.reading

    /** The open paths now, untranslated. */
    def openPaths(using Frame): Set[List[Int]] < Sync = paths.get

    /** Closes the whole tree: the same against any roots, so it needs no render to write from. */
    def closeAll(using Frame): UI.Action = paths.reset(Set.empty)

    /** Opens the chain to `target` against whatever roots the host shows next, the meaning an `init` has. For a writer
      * with no render to anchor to (a test seeding state); a host writes through a [[Submenus.View]].
      */
    def openUnanchored(target: List[Int])(using Frame): UI.Action =
        paths.reset((1 to target.size).map(target.take).toSet)

    /** The open paths over the roots a host shows. */
    def view(snap: Anchored.Snap[Set[List[Int]]], roots: List[AnyRef]): Submenus.View =
        new Submenus.View(paths.view(snap, roots))
end Submenus

private[uic] object Submenus:
    def init(open: Set[List[Int]] = Set.empty)(using Frame): Submenus < Sync = Anchored.init(open).map(new Submenus(_))

    /** The open paths as a host shows them, with the writers its rows use. */
    final class View private[Submenus] (paths: Anchored.View[Set[List[Int]]]):

        /** The open paths over the host's roots. */
        def open: Set[List[Int]] = paths.value

        /** Opens exactly the chain leading to `target` (every prefix of the path) and closes everything else; `Absent`
          * closes the whole tree.
          */
        def openExactly(target: Maybe[List[Int]])(using Frame): UI.Action =
            paths.set(target.fold(Set.empty[List[Int]])(t => (1 to t.size).map(t.take).toSet))

        /** Closes `path` and every submenu below it, as an Escape or an outside click on that submenu does. */
        def close(path: List[Int])(using Frame): UI.Action =
            paths.update(_.filterNot(_.startsWith(path)))
    end View
end Submenus

/** Package-internal machinery shared by the menu family. */
private[uic] object MenuRender:

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
            case Nil       => items
            case i :: rest =>
                items.zipWithIndex.map { (it, j) =>
                    if j != i then it
                    else if rest.isEmpty then it.withDisabled(Present(BoolValue.Const(v)))
                    else it.withItems(pinDisabled(it.itemsV, rest, v))
                }
        end match
    end pinDisabled

    /** Every reactive `disabled` in the tree as ONE [[ReactiveValue]] of the items, each slot pinned to `Const`, for a host that
      * reads it beside its own state in one region.
      *
      * The value-level [[BoolValue.reactive]] over a LIST, read the same way [[Submenus.reading]]
      * reads submenu state: ONE region around the whole component rather than a
      * subscription per row. That is not a micro-optimisation: a per-row `Signal.render`
      * interposes kyo's classless reactive `<span>` between the list and its rows, and
      * Prime's direct-child selectors (`.p-menubar-root-list > .p-menubar-item > ...`)
      * stop matching.
      *
      * With no reactive slot anywhere the reading is fixed and adds no subscription at
      * all, so a menu of constants renders exactly as it did.
      */
    def readDisabled(items: List[MenuItem])(using Frame): ReactiveValue[List[MenuItem]] =
        pinned(dynamicDisabled(items), items)(pinDisabled)

    /** A host's rows as ONE [[ReactiveValue]]: the entries a setter took, signals among them read in place, and every reactive
      * `disabled` resolved as [[readDisabled]] does. Fixed while nothing in it is reactive.
      */
    def readItems(entries: List[MenuItemLiveEntry])(using Frame): ReactiveValue[List[MenuItem]] =
        MenuItemScope.reading(entries) match
            case ReactiveValue.Const(items) => readDisabled(items)
            case ReactiveValue.Dyn(sig)     => ReactiveValue.Dyn(sig.switchMap(items => readDisabled(items).signal))

    /** [[readItems]] beside the RAW root items it was resolved from, which are what [[Anchored]] state is matched against.
      * Raw, because resolving a reactive `disabled` copies the items it pins, so the rows a host draws are new values on
      * every emission; the raw ones are the setter's fixed entries, resolved once, and the app's own instances from a
      * signal.
      */
    def readRooted(entries: List[MenuItemLiveEntry])(using Frame): ReactiveValue[Rooted[MenuItem]] =
        MenuItemScope.reading(entries) match
            case ReactiveValue.Const(items) => readDisabled(items).map(pinned => Rooted(items, pinned))
            case ReactiveValue.Dyn(sig)     =>
                ReactiveValue.Dyn(sig.switchMap(items => readDisabled(items).map(pinned => Rooted(items, pinned)).signal))
    end readRooted

    /** The reading itself, shared by the two traversals: every reactive slot as one `combineLatestAll`, each value
      * pinned into `seed`. With `dyns` empty it is `seed`, fixed.
      */
    private def pinned[T](dyns: List[(List[Int], Signal[Boolean])], seed: T)(
        pin: (T, List[Int], Boolean) => T
    )(using Frame): ReactiveValue[T] =
        if dyns.isEmpty then ReactiveValue.Const(seed)
        else
            given CanEqual[T, T] = CanEqual.derived
            val paths            = dyns.map(_._1)
            ReactiveValue.Dyn(Signal.combineLatestAll(dyns.map(_._2)).map { values =>
                paths.zip(values).foldLeft(seed) { case (acc, (p, v)) => pin(acc, p, v) }
            })
    end pinned

    /** [[readDisabled]] for MegaMenu, whose leaves sit two containers deeper:
      * `MegaMenuItem.columnsV : List[List[MenuGroup]]`, each group holding the
      * `MenuItem`s. The root rows carry their own plain-`Boolean` `disabled` and are
      * untouched; only the leaves have the reactive slot. Paths are
      * `[root, column, group, item]`.
      */
    def readMegaDisabled(items: List[MegaMenuItem])(using Frame): ReactiveValue[List[MegaMenuItem]] =
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
                            val its  =
                                grp.itemsV.updated(ii, grp.itemsV(ii).withDisabled(Present(BoolValue.Const(v))))
                            root.withColumns(root.columnsV.updated(ci, col.updated(gi, grp.withItems(its))))
                        }
                    )
                case _ => acc

        pinned(dyns, items)(pin)
    end readMegaDisabled

    /** Applies a [[MenuNav.Step]] to a keyboard host's signals: threads the
      * `OpenOp` through [[Submenus.View.openExactly]], and, on `activate` (run the focused
      * leaf's action) or `dismiss`, collapses the submenu tree, runs the host's `closePopup`
      * (what closing means is the host's: a binding written and reported, or nothing inline),
      * and clears the highlight. Plain movement just writes the new focus
      * path. Shared by every nested menu host (TieredMenu, ContextMenu, Menubar,
      * MegaMenu).
      */
    def applyStep(
        items: List[MenuItem],
        submenus: Maybe[Submenus.View],
        focusRef: Anchored.View[List[Int]],
        closePopup: UI.Action,
        step: MenuNav.Step
    )(using Frame): UI.Action =
        val finish                                 = step.activate || step.dismiss
        def openTo(t: Maybe[List[Int]]): UI.Action =
            submenus match
                case Present(subs) => subs.openExactly(t)
                case Absent        => ()
        val applyOpen: UI.Action =
            step.open match
                case MenuNav.OpenOp.Keep      => ()
                case MenuNav.OpenOp.Close     => openTo(Absent)
                case MenuNav.OpenOp.OpenTo(p) => openTo(Present(p))
        val runActivate: UI.Action =
            if !step.activate then ()
            else
                MenuNav.itemAt(items, step.focus).flatMap(_.actionV) match
                    case Present(eff) => eff
                    case _            => ()
        for
            _ <- applyOpen
            _ <- runActivate
            _ <- if finish then openTo(Absent) else (): UI.Action
            _ <- if finish then closePopup else (): UI.Action
            _ <- focusRef.set(if finish then Nil else step.focus)
        yield ()
        end for
    end applyStep

    /** Stamps [[MenuItem.current]] onto a row, through kyo-ui's class channel: a constant applies
      * statically, a `Signal[Boolean]` toggles the class in place with no re-render.
      *
      * One function rather than a line in each of the five hosts that build `MenuItem` rows, so
      * the class name and the channel cannot drift apart between them. A row whose item declares
      * nothing is returned untouched, so every existing render is byte-identical.
      */
    def markCurrent[E <: UI.Ast.Element { type Self <: E }](row: E, it: MenuItem): E =
        it.currentFlag.foldFlag(row)(on => row.cssClass("p-uic-menu-item-current", on))

    /** The shared row content: `div.p-<prefix>-item-content` >
      * `a.p-<prefix>-item-link` holding icon + label (+ trailing submenu glyph).
      * `activate` (when interactive) runs on click and on Enter with the row
      * link focused.
      */
    def itemContent(
        prefix: String,
        it: MenuItem,
        activate: Maybe[UI.Action],
        submenuIcon: Maybe[IconGlyph],
        roving: Boolean = false
    )(using Frame): UI =
        var link = a.cssClass(s"p-$prefix-item-link")
        it.urlV.foreach(u => link = link.href(Href.Path(u)))
        // `aria-current` belongs on the LINK, not on the row: it says "this link points at the
        // page you are on". The reactive form is the attribute channel (patched in place), and
        // the negative spelling is written out rather than omitted so the attribute the channel
        // writes into exists from the first render.
        it.currentFlag match
            case Present(BoolValue.Const(true)) => link = link.aria("current", "page")
            case Present(BoolValue.Dyn(sig))    => link = link.aria("current", sig.map(if _ then "page" else "false"))
            case _                              => ()
        end match
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
        val label: UI      = it.labelV match
            case TextValue.Const(t) => span.cssClass(s"p-$prefix-item-label")(t)
            case TextValue.Dyn(s)   => span.cssClass(s"p-$prefix-item-label")(s)
        val subIcon: List[UI] =
            submenuIcon.toList.map(g => GlyphSvg(g, s"p-$prefix-submenu-icon", "p-icon"))
        div.cssClass(s"p-$prefix-item-content")(
            link(UI.children((icon :+ label) ++ subIcon)*)
        )
    end itemContent
end MenuRender
