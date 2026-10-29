package kyo.uic

import kyo.*

/** Base class for the kyo-ui-components suites, the component-library counterpart of kyo-ui's `UITest`.
  *
  * These suites are pure: they assert on rendered HTML strings, on the menu state machines, and on what does or does
  * not type-check. Nothing drives a browser, so none of `UITest`'s Chrome accommodations apply and the kyo-test
  * defaults are exactly right.
  *
  * The helpers below take a handler off the rendered tree and call it with a synthesized event, which is how a suite
  * asserts BEHAVIOUR without a DOM: the golden renders pin what the markup says, the `*Nav` suites pin what a key
  * means, and these pin the wiring between the two. They are `private[uic]`, which reaches the `kyo.uic.test`
  * subpackage as well.
  */
abstract class UicTest extends kyo.test.Test[Any]:

    /** Runs a handler's effect. None of the handlers under test is meant to fail, so a failure fails the test. */
    private[uic] def handled(action: UI.Action)(using Frame): Any < Async =
        Abort.run[Any](action).map {
            case Result.Success(a) => a
            case other             => throw new AssertionError(s"the handler failed: $other")
        }

    /** Runs `ui` the way a transport does, its mount effects included, for as long as the enclosing `Scope` is open.
      *
      * The walks below show a mount as its placeholder, so they cannot see what a component starts in its own mount.
      * This subscribes the tree with a command channel installed and every change dropped, which is enough for the
      * mounts to run; what they publish is then read through the signals they feed.
      */
    private[uic] def live(ui: UI)(using Frame): Unit < (Async & Scope) =
        UI.Commands.init(_ => Kyo.unit).map { cmds =>
            kyo.internal.UICommands.commands.let(Present(cmds)) {
                kyo.internal.ReactiveUI.normalize(ui, Seq.empty).map(root =>
                    kyo.internal.ReactiveUI.subscribe(root, kyo.internal.UIExchange.noop).unit
                )
            }
        }

    /** Runs `ui` under the engine, hands `act` the tree its mounts and regions last published (the whole of `ui` when
      * none did), and answers whether a region published again after `act` ran. That is the one sign a field paints
      * what its owner holds once more after a report the owner did not take: nothing in the markup changes.
      *
      * A region publishes while it settles too, so `act` runs only once every ref in `reads` has an observer parked on it
      * again (`waiters`), which the engine does only after handling the value before: the barrier
      * [[DataTableReuseTest]] uses. `within` only turns a wait that never ends into an answer instead of a hang; it is not
      * what passes a test.
      */
    private[uic] def repaintsAfter(ui: UI, reads: Signal.SignalRef[?]*)(act: UI => Any < Async)(using
        Frame,
        kyo.test.AssertScope
    ): Boolean < Async =
        publishedAfter(ui, reads*)(act).map(_.nonEmpty)

    /** [[repaintsAfter]], answering with what the field published last once `act` made it publish again, so a test can
      * read what it now shows.
      */
    private[uic] def publishedAfter(ui: UI, reads: Signal.SignalRef[?]*)(act: UI => Any < Async)(using
        Frame,
        kyo.test.AssertScope
    ): Maybe[UI] < Async =
        publishedAfterEach(ui, reads*)(act)

    /** [[publishedAfter]] for several gestures in a row (open a panel, then pick in it): each acts on what the field
      * published last and waits for it to publish again, and the answer is what it published after the last one, or
      * `Absent` as soon as one gesture made it publish nothing.
      */
    private[uic] def publishedAfterEach(ui: UI, reads: Signal.SignalRef[?]*)(acts: (UI => Any < Async)*)(using
        Frame,
        kyo.test.AssertScope
    ): Maybe[UI] < Async =
        publishedAfterReady(ui, _ => true, reads = reads)(acts*)

    /** [[publishedAfterEach]] that first waits for what the field published last to satisfy `ready`: a field built of
      * nested mounts publishes the outer one before the inner one has wired what a gesture reaches. A gesture sees every
      * tree published so far, newest first, since a panel published by a mount of its own is not inside the tree its
      * field published, and it is tried again until it finds what it acts on, since that panel may not be published yet.
      * After the last gesture the answer waits for `until` to hold over everything published since that gesture (a field
      * and the panel it opens publish as separate regions, in either order), and is that, newest first, or `Absent` if it
      * never does.
      */
    private[uic] def publishedAfterReady(
        ui: UI,
        ready: UI => Boolean < Sync,
        until: UI => Boolean < Sync = _ => true,
        reads: Seq[Signal.SignalRef[?]] = Nil
    )(acts: (UI => Any < Async)*)(using Frame, kyo.test.AssertScope): Maybe[UI] < Async =
        drive(ui, ready, reads, acts) { d =>
            for
                holds <- settles(
                    Sync.defer(d.published.get.size > d.publishedBefore).map(grew =>
                        if grew then d.since.map(until) else Kyo.lift(false)
                    ),
                    d.within
                )
                answer <- d.since
            yield if holds then Present(answer) else Absent
        }

    /** `ui` as its mount publishes it once `ready` holds, the tree a gesture acts on: the walks above see a mount as its
      * placeholder, which carries none of the wiring the mount adds. A mount that never publishes such a tree fails the
      * test.
      */
    private[uic] def mountedWhen(ui: UI)(ready: UI => Boolean < Sync)(using Frame, kyo.test.AssertScope): UI < Async =
        publishedAfterReady(ui, ready)().map(_.getOrElse(throw new AssertionError("never mounted")))

    /** The first HTML a transport renders for `ui`, which is what a static page carries. */
    private[uic] def renderHtml(ui: UI)(using Frame): String < Async =
        UI.runRender(ui).take(1).run.map(_.mkString)

    /** [[publishedAfterReady]] for what reaches the page as an attribute patch rather than a region: whether, after the
      * gestures, the field patched an attribute `holds` accepts, as `(name, value)`. A header's sort state, say, rides
      * that channel and never publishes a region of its own.
      */
    private[uic] def attrPatchedAfter(ui: UI, ready: UI => Boolean < Sync)(acts: (UI => Any < Async)*)(
        holds: ((String, String)) => Boolean
    )(using Frame, kyo.test.AssertScope): Boolean < Async =
        drive(ui, ready, Nil, acts)(d =>
            settles(Sync.defer(d.patched.get.take(d.patched.get.size - d.patchedBefore).exists(holds)), d.within)
        )

    /** What [[drive]] hands its `finish`: everything the field published and patched, and how much of each came before
      * the last gesture.
      */
    final private class Driven(
        val published: java.util.concurrent.atomic.AtomicReference[List[UI]],
        val patched: java.util.concurrent.atomic.AtomicReference[List[(String, String)]],
        val publishedBefore: Int,
        val patchedBefore: Int,
        val within: Duration
    )(using Frame):
        /** Every tree published since the last gesture, newest first. */
        def since: UI < Sync =
            Sync.defer(published.get).map(now => UI.fragment(now.take(now.size - publishedBefore)*))
    end Driven

    /** Runs `ui` under the engine, waits for it to be `ready`, runs the gestures (each tried again until it finds what it
      * acts on, and each seeing every tree published so far, newest first), then hands `finish` what came of them.
      */
    private def drive[R](ui: UI, ready: UI => Boolean < Sync, reads: Seq[Signal.SignalRef[?]], acts: Seq[UI => Any < Async])(
        finish: Driven => R < Async
    )(using Frame, kyo.test.AssertScope): R < Async =
        val within = 5.seconds
        Scope.run {
            val published      = new java.util.concurrent.atomic.AtomicReference[List[UI]](Nil)
            val patched        = new java.util.concurrent.atomic.AtomicReference[List[(String, String)]](Nil)
            def all: UI < Sync = Sync.defer(published.get).map(seen => if seen.isEmpty then ui else UI.fragment(seen*))
            UI.Commands.init(_ => Kyo.unit).map { cmds =>
                kyo.internal.UICommands.commands.let(Present(cmds)) {
                    for
                        root <- kyo.internal.ReactiveUI.normalize(ui, Seq.empty)
                        _    <- kyo.internal.ReactiveUI.subscribe(
                            root,
                            new kyo.internal.UIExchange:
                                def onChange(
                                    region: kyo.internal.ReactiveRegion,
                                    path: Seq[String],
                                    contentContext: kyo.internal.ReactiveRegion.RegionIdentity,
                                    parentContext: kyo.internal.ReactiveRegion.ParentContext,
                                    previous: Maybe[UI],
                                    changed: UI
                                )(using Frame): Unit < Async = Sync.defer(discard(published.updateAndGet(changed :: _)))
                                override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
                                    Sync.defer(discard(patched.updateAndGet((name, value) :: _)))
                        )
                        // A mount publishes what it wired once its effect ran; act on that, not on the placeholder. A
                        // field with no mount publishes nothing here, and is acted on as it rendered.
                        _      <- settles(Sync.defer(published.get.nonEmpty), within)
                        parked <- settles(Kyo.foreach(reads)(_.waiters).map(_.forall(_ > 0)), within)
                        _ = if !parked then throw new AssertionError(s"the field never settled on what it reads: $reads")
                        set <- settles(all.map(ready), within)
                        _ = if !set then throw new AssertionError("the field never published a tree ready for the gesture")
                        last <- Kyo.foldLeft(acts)((0, 0)) { (_, act) =>
                            for
                                before <- Sync.defer((published.get.size, patched.get.size))
                                done   <- settles(
                                    all.map(t => Abort.run[AssertionError](Abort.catching[AssertionError](act(t))).map(_.isSuccess)),
                                    within
                                )
                                _ = if !done then throw new AssertionError("a gesture never found what it acts on")
                            yield before
                        }
                        r <- finish(Driven(published, patched, last._1, last._2, within))
                    yield r
                }
            }
        }
    end drive

    /** Whether `cond` holds within `within`, polled the way `assertEventually` polls, answering instead of failing. */
    private def settles(cond: => Boolean < Async, within: Duration)(using Frame): Boolean < Async =
        Abort.run[Unit](Retry[Unit](Schedule.fixed(10.millis).maxDuration(within))(
            cond.map(ok => if ok then Kyo.unit else Abort.fail(()))
        )).map(_.isSuccess)

    /** Every element of a rendered tree, with the reactive nodes resolved to their current content.
      *
      * Reactive is a subscription boundary, not a node the client sees, so a walk that stopped there would miss
      * everything a component renders inside its own refs. A mount is shown as its placeholder, which is what the
      * golden renderer does.
      */
    private[uic] def elements(node: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        node match
            case e: UI.Ast.Element =>
                Kyo.foreach(e.children)(elements).map(cs => Chunk(e) ++ cs.flatten)
            case r: UI.Ast.Reactive[?] =>
                r.signal.current(using r.frame).map(elements)
            case f: UI.Ast.Fragment[?] =>
                Kyo.foreach(f.children)(elements).map(_.flatten)
            case f: UI.Ast.Foreach[?, ?] => foreachElements(f)
            case k: UI.Ast.KeyedChild[?] => elements(k.child)
            case m: UI.Ast.Mounted       =>
                m.placeholderUI match
                    case Present(ui) => elements(ui)
                    case Absent      => Chunk.empty
            case _ => Chunk.empty

    /** Every reactive node in the tree, in walk order, WITHOUT resolving it away.
      *
      * `elements` deliberately looks through a `Reactive`, because a reader never sees the boundary. A suite that
      * pins a slot as a text CHANNEL has to see it: a `Source.Lifted` in `UI.Ast.Reactive.source` is what
      * `kyo.internal.ReactiveUI.bindTextRegion` keys on, and a region that carries it is bound straight to the
      * backend's text write instead of running a fiber, a Scope and a re-walk to change one string. The rendered
      * HTML is the same either way, so the golden render cannot tell the two apart and this is the only place the
      * difference is visible.
      */
    private[uic] def reactiveNodes(node: UI)(using Frame): Chunk[UI.Ast.Reactive[?]] < Sync =
        node match
            case e: UI.Ast.Element       => Kyo.foreach(e.children)(reactiveNodes).map(_.flattenChunk)
            case f: UI.Ast.Fragment[?]   => Kyo.foreach(f.children)(reactiveNodes).map(_.flattenChunk)
            case k: UI.Ast.KeyedChild[?] => reactiveNodes(k.child)
            case r: UI.Ast.Reactive[?]   =>
                r.signal.current(using r.frame).map(reactiveNodes).map(Chunk(r) ++ _)
            case f: UI.Ast.Foreach[?, ?] =>
                f.applyTyped([T] =>
                    (signal: Signal[Chunk[T]], _: Maybe[T => String], render: (Int, T) => UI) =>
                        signal.current(using f.frame).map { items =>
                            Kyo.foreach(items.zipWithIndex) { (item, i) => reactiveNodes(render(i, item)) }
                                .map(_.flattenChunk)
                        }
                )
            case m: UI.Ast.Mounted =>
                m.placeholderUI match
                    case Present(ui) => reactiveNodes(ui)
                    case Absent      => Chunk.empty
            case _ => Chunk.empty

    /** The list regions in the tree, kept rather than resolved, for the suites that pin a slot as a reconciled list
      * rather than a block of children rebuilt per emission. Same reasoning as [[reactiveNodes]]: the two render the
      * same HTML, so the boundary is the only place the difference shows. The walk descends THROUGH a `Reactive`,
      * since a component may put its list behind a region of its own (a paginator's page region).
      */
    private[uic] def foreachNodes(node: UI)(using Frame): Chunk[UI.Ast.Foreach[?, ?]] < Sync =
        node match
            case e: UI.Ast.Element       => Kyo.foreach(e.children)(foreachNodes).map(_.flattenChunk)
            case f: UI.Ast.Fragment[?]   => Kyo.foreach(f.children)(foreachNodes).map(_.flattenChunk)
            case k: UI.Ast.KeyedChild[?] => foreachNodes(k.child)
            case r: UI.Ast.Reactive[?]   => r.signal.current(using r.frame).map(foreachNodes)
            case f: UI.Ast.Foreach[?, ?] => Kyo.lift(Chunk(f))
            case m: UI.Ast.Mounted       =>
                m.placeholderUI match
                    case Present(ui) => foreachNodes(ui)
                    case Absent      => Chunk.empty
            case _ => Chunk.empty

    /** A keyed list region resolved to the elements it currently renders.
      *
      * Same reasoning as `Reactive` above: a list region is a subscription boundary, not a node the client sees, so a
      * walk that stopped here would miss every row a component renders through one. Split out because recovering the
      * element type costs a polymorphic continuation, which does not fit inside a match arm.
      */
    private def foreachElements(f: UI.Ast.Foreach[?, ?])(using Frame): Chunk[UI.Ast.Element] < Sync =
        f.applyTyped([T] =>
            (signal: Signal[Chunk[T]], _: Maybe[T => String], render: (Int, T) => UI) =>
                signal.current(using f.frame).map { items =>
                    Kyo.foreach(items.zipWithIndex) { (item, i) => elements(render(i, item)) }.map(_.flatten)
                }
        )

    /** Paths (index chains from the root) of every item carrying a submenu, for a test that opens some of them in a
      * [[Submenus]]. Only the fixed entries count: a test that opens a submenu knows the tree it built.
      */
    private[uic] def submenuPaths(entries: List[MenuItemLiveEntry])(using Frame): List[List[Int]] =
        def loop(items: List[MenuItem], prefix: List[Int]): List[List[Int]] =
            items.zipWithIndex.flatMap { (it, i) =>
                val p = prefix :+ i
                if it.itemsV.nonEmpty then p :: loop(it.itemsV, p) else Nil
            }
        MenuItemScope.reading(entries) match
            case Reading.Fixed(items) => loop(items, Nil)
            case Reading.Live(_)      => throw new AssertionError("submenu paths are read from fixed entries")
    end submenuPaths

    /** The root indices of a MegaMenu that open a panel. */
    private[uic] def panelPaths(entries: List[MegaMenuItemLiveEntry])(using Frame): List[List[Int]] =
        MegaMenuItemScope.reading(entries) match
            case Reading.Fixed(items) => items.zipWithIndex.collect { case (it, i) if it.columnsV.nonEmpty => List(i) }
            case Reading.Live(_)      => throw new AssertionError("panel paths are read from fixed entries")

    /** Paths of every group of a CascadeSelect's options. */
    private[uic] def groupPaths[A](c: CascadeSelect[A])(using Frame): List[List[Int]] =
        def loop(is: List[CascadeItem[A]], prefix: List[Int]): List[List[Int]] =
            is.zipWithIndex.flatMap {
                case (CascadeItem.Group(_, children), i) =>
                    val p = prefix :+ i
                    p :: loop(children, p)
                case _ => Nil
            }
        Entry.reading((), c.itemSources) match
            case Reading.Fixed(roots) => loop(roots, Nil)
            case Reading.Live(_)      => throw new AssertionError("group paths are read from fixed roots")
    end groupPaths

    /** How many regions sit above the mount of the control drawn with `cls`. Unlike [[regionsAbove]] it stops at the
      * mount, so the regions a control draws inside itself do not count: a region above the mount is one whose every
      * repaint starts the control's mount again.
      */
    private[uic] def regionsAboveMount(node: UI, cls: String)(using Frame): Int < Sync =
        def holds(n: UI): Boolean < Sync               = elementsWithClass(n, cls).map(_.nonEmpty)
        def walk(n: UI, depth: Int): Chunk[Int] < Sync =
            n match
                case m: UI.Ast.Mounted =>
                    m.placeholderUI match
                        case Present(ui) => holds(ui).map(found => if found then Chunk(depth) else Chunk.empty)
                        case Absent      => Chunk.empty
                case e: UI.Ast.Element       => Kyo.foreach(e.children)(walk(_, depth)).map(_.flattenChunk)
                case r: UI.Ast.Reactive[?]   => r.signal.current(using r.frame).map(walk(_, depth + 1))
                case f: UI.Ast.Fragment[?]   => Kyo.foreach(f.children)(walk(_, depth)).map(_.flattenChunk)
                case f: UI.Ast.Foreach[?, ?] =>
                    // A list region counts like `Reactive`: a row is drawn again when its value changes.
                    f.applyTyped([T] =>
                        (signal: Signal[Chunk[T]], _: Maybe[T => String], render: (Int, T) => UI) =>
                            signal.current(using f.frame).map { items =>
                                Kyo.foreach(items.zipWithIndex) { (item, i) => walk(render(i, item), depth + 1) }
                                    .map(_.flattenChunk)
                            }
                    )
                case k: UI.Ast.KeyedChild[?] => walk(k.child, depth)
                case _                       => Chunk.empty
        walk(node, 0).map(ds => if ds.isEmpty then throw new AssertionError(s"no mount draws $cls") else ds.min)
    end regionsAboveMount

    /** How many reactive regions sit between `node` and the nearest element carrying `cls`.
      *
      * One is what a control that reads several refs wants. Nesting a render inside another leaves
      * the inner region subscribed against the values the outer one held when it created it, so an
      * effect that writes both refs leaves the inner one able to repaint a snapshot that predates
      * the write. `PickList` is where that stopped being theoretical.
      */
    private[uic] def regionsAbove(node: UI, cls: String)(using Frame): Int < Sync =
        def walk(n: UI, depth: Int): Chunk[Int] < Sync =
            n match
                case e: UI.Ast.Element if e.attrs.cssClasses.contains(cls) => Chunk(depth)
                case e: UI.Ast.Element                                     => Kyo.foreach(e.children)(walk(_, depth)).map(_.flattenChunk)
                case r: UI.Ast.Reactive[?]                                 => r.signal.current(using r.frame).map(walk(_, depth + 1))
                case f: UI.Ast.Fragment[?]                                 => Kyo.foreach(f.children)(walk(_, depth)).map(_.flattenChunk)
                case f: UI.Ast.Foreach[?, ?]                               =>
                    // A list region is a subscription boundary like `Reactive`, so it counts the same: a row rendered
                    // through one is a region deeper than the list itself.
                    f.applyTyped([T] =>
                        (signal: Signal[Chunk[T]], _: Maybe[T => String], render: (Int, T) => UI) =>
                            signal.current(using f.frame).map { items =>
                                Kyo.foreach(items.zipWithIndex) { (item, i) => walk(render(i, item), depth + 1) }
                                    .map(_.flattenChunk)
                            }
                    )
                case k: UI.Ast.KeyedChild[?] => walk(k.child, depth)
                case m: UI.Ast.Mounted       =>
                    m.placeholderUI match
                        case Present(ui) => walk(ui, depth)
                        case Absent      => Chunk.empty
                case _ => Chunk.empty
        walk(node, 0).map(ds =>
            if ds.isEmpty then throw new AssertionError(s"no element with class $cls") else ds.min
        )
    end regionsAbove

    private[uic] def elementWithId(node: UI, id: String)(using Frame): UI.Ast.Element < Sync =
        elements(node).map(_.find(_.attrs.identifier.contains(id)).getOrElse(
            throw new AssertionError(s"no element with id $id")
        ))

    private[uic] def elementWithClass(node: UI, cls: String)(using Frame): UI.Ast.Element < Sync =
        elements(node).map(_.find(_.attrs.cssClasses.contains(cls)).getOrElse(
            throw new AssertionError(s"no element with class $cls")
        ))

    private[uic] def elementsWithClass(node: UI, cls: String)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elements(node).map(_.filter(_.attrs.cssClasses.contains(cls)))

    /** All the text an element holds, however deeply, which is how a button is found by its label rather than by a class
      * the sheet does not give it.
      */
    private[uic] def textOf(e: UI.Ast.Element)(using Frame): String =
        def walk(u: UI): String = u match
            case t: UI.Ast.Text     => t.value
            case el: UI.Ast.Element => el.children.map(walk).mkString
            case _                  => ""
        e.children.map(walk).mkString
    end textOf

    /** Sends one key to `el`'s own handler, as the dispatcher would. */
    private[uic] def press(el: UI.Ast.Element, key: UI.Keyboard, mods: UI.Modifiers = UI.Modifiers.none)(
        using Frame
    ): Any < Async =
        el.attrs.onKeyDown match
            case Present(f) => handled(f(UI.KeyboardEvent(key, mods, el.attrs.identifier)))
            case Absent     => throw new AssertionError("the element declares no key handler")

    /** Runs `el`'s click handler, as the dispatcher would.
      *
      * The handler is an EFFECT stored in a `Maybe`, so it is taken out by hand: a match over it infers `Any` and
      * lands inert, which is the trap the module documents.
      */
    private[uic] def click(el: UI.Ast.Element)(using Frame): Any < Async =
        el.attrs.onClick match
            case Present(eff) => handled(eff)
            case Absent       =>
                el.attrs.onClickEvt match
                    case Present(f) => handled(f(UI.MouseEvent(el.attrs.identifier, UI.Modifiers.none)))
                    case Absent     => throw new AssertionError("the element declares no click handler")

    /** Clicks the first element `target` matches the way the dispatcher delivers a click: the target's own handlers
      * first, then each enclosing element's, innermost first, until an element that declared a click handler also set
      * `stopPropagation(true)` (`ReactiveUI.keepBubbling`). `onClickSelf` counts on the target alone, and a disabled
      * target skips its own handlers but still lets the click bubble, as the dispatcher does.
      *
      * [[click]] runs one element's handler in isolation, which cannot show what a click on a row inside a panel does
      * to the field around it; this is the helper for that question. Mounts are walked as their placeholders, as
      * everywhere in this class.
      */
    private[uic] def clickBubbling(root: UI)(target: UI.Ast.Element => Boolean)(using Frame): Any < Async =
        def chains(node: UI, above: List[UI.Ast.Element]): Chunk[List[UI.Ast.Element]] < Sync =
            node match
                case e: UI.Ast.Element =>
                    val here = e :: above
                    Kyo.foreach(e.children)(chains(_, here)).map(cs => Chunk(here) ++ cs.flatten)
                case r: UI.Ast.Reactive[?]   => r.signal.current(using r.frame).map(chains(_, above))
                case f: UI.Ast.Fragment[?]   => Kyo.foreach(f.children)(chains(_, above)).map(_.flatten)
                case k: UI.Ast.KeyedChild[?] => chains(k.child, above)
                case f: UI.Ast.Foreach[?, ?] =>
                    f.applyTyped([T] =>
                        (signal: Signal[Chunk[T]], _: Maybe[T => String], render: (Int, T) => UI) =>
                            signal.current(using f.frame).map { items =>
                                Kyo.foreach(items.zipWithIndex) { (item, i) => chains(render(i, item), above) }
                                    .map(_.flatten)
                            }
                    )
                case m: UI.Ast.Mounted =>
                    m.placeholderUI match
                        case Present(ui) => chains(ui, above)
                        case Absent      => Chunk.empty
                case _ => Chunk.empty
        def disabled(e: UI.Ast.Element): Boolean = e match
            case d: UI.Ast.HasDisabled => d.disabled.getOrElse(false)
            case _                     => false
        def run(eff: Maybe[UI.Action]): Any < Async = eff match
            case Present(a) => handled(a)
            case Absent     => ()
        def runWith(f: Maybe[UI.MouseEvent => UI.Action], e: UI.Ast.Element): Any < Async = f match
            case Present(g) => handled(g(UI.MouseEvent(e.attrs.identifier, UI.Modifiers.none)))
            case Absent     => ()
        def walk(chain: List[UI.Ast.Element], isTarget: Boolean): Any < Async =
            chain match
                case Nil                                  => ()
                case e :: rest if isTarget && disabled(e) => walk(rest, isTarget = false)
                case e :: rest                            =>
                    val a        = e.attrs
                    val declared = a.onClick.nonEmpty || a.onClickEvt.nonEmpty ||
                        (isTarget && (a.onClickSelf.nonEmpty || a.onClickSelfEvt.nonEmpty))
                    val self: Any < Async =
                        if isTarget then run(a.onClickSelf).andThen(runWith(a.onClickSelfEvt, e)) else ()
                    self
                        .andThen(runWith(a.onClickEvt, e))
                        .andThen(run(a.onClick))
                        .andThen(if declared && a.stopPropagation.getOrElse(false) then () else walk(rest, isTarget = false))
        chains(root, Nil).map { all =>
            all.find(c => target(c.head)) match
                case Some(chain) => walk(chain, isTarget = true)
                case None        => throw new AssertionError("no element matches the click target")
        }
    end clickBubbling

    /** A click as the client sends it: addressed by the path the element had on the page, not by the element. */
    private[uic] def clickAt(path: Seq[String]): kyo.internal.UIEvent =
        kyo.internal.UIEvent.Click(path, kyo.internal.MouseEventData(UI.Modifiers.none, Absent))

    /** The `data-kyo-path` the element with id `id` carries in `html`, which is the address the client clicks it by. */
    private[uic] def pathOf(html: String, id: String): Seq[String] =
        val tag = ("""<[^>]*\bid="""" + java.util.regex.Pattern.quote(id) + """"[^>]*>""").r
            .findFirstIn(html).getOrElse(throw new AssertionError(s"no element with id $id in $html"))
        val raw = """data-kyo-path="([^"]*)"""".r.findFirstMatchIn(tag).map(_.group(1))
            .getOrElse(throw new AssertionError(s"the element with id $id carries no path: $tag"))
        if raw.isEmpty then Seq.empty else raw.split('.').toSeq
    end pathOf

    /** Whether `el` declares any click handler, the self-only one included. */
    private[uic] def declaresClick(el: UI.Ast.Element): Boolean =
        val a = el.attrs
        a.onClick.nonEmpty || a.onClickEvt.nonEmpty || a.onClickSelf.nonEmpty || a.onClickSelfEvt.nonEmpty

    /** The same, for a click that reached `el` THROUGH a control of the reader's own: an anchor in
      * a cell, a button in a row. Only the typed handler can be told, since the flag rides
      * [[kyo.UI.MouseEvent.onControl]]; an element that took the untyped form never asked.
      */
    private[uic] def clickViaControl(el: UI.Ast.Element)(using Frame): Any < Async =
        el.attrs.onClickEvt match
            case Present(f) =>
                handled(f(UI.MouseEvent(el.attrs.identifier, UI.Modifiers.none, Absent, onControl = true)))
            case Absent => throw new AssertionError("the element declares no typed click handler")

end UicTest
