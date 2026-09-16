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
            case m: UI.Ast.Mounted =>
                m.placeholderUI match
                    case Present(ui) => elements(ui)
                    case Absent      => Chunk.empty
            case _ => Chunk.empty

    /** Every reactive node in the tree, in walk order, WITHOUT resolving it away.
      *
      * `elements` deliberately looks through a `Reactive`, because a reader never sees the boundary. A suite that
      * pins a slot as a text CHANNEL has to see it: `UI.Ast.Reactive.text` is the single field
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
            case r: UI.Ast.Reactive[?] =>
                r.signal.current(using r.frame).map(reactiveNodes).map(Chunk(r) ++ _)
            case f: UI.Ast.Foreach[?, ?] =>
                f.applyTyped([T] =>
                    (signal: Signal[Chunk[T]], _: Maybe[T => String], render: (Int, T) => UI) =>
                        signal.current(using f.frame).map { items =>
                            Kyo.foreach(items.zipWithIndex) { (item, i) => reactiveNodes(render(i, item)) }
                                .map(_.flattenChunk)
                    })
            case m: UI.Ast.Mounted =>
                m.placeholderUI match
                    case Present(ui) => reactiveNodes(ui)
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
            })

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
                        })
                case k: UI.Ast.KeyedChild[?] => walk(k.child, depth)
                case m: UI.Ast.Mounted =>
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

    /** Sends one key to `el`'s own handler, as the dispatcher would. */
    private[uic] def press(el: UI.Ast.Element, key: UI.Keyboard, mods: UI.Modifiers = UI.Modifiers.none)(
        using Frame
    ): Any < Async =
        el.attrs.onKeyDown match
            case Present(f) => f(UI.KeyboardEvent(key, mods, el.attrs.identifier))
            case Absent     => throw new AssertionError("the element declares no key handler")

    /** Runs `el`'s click handler, as the dispatcher would.
      *
      * The handler is an EFFECT stored in a `Maybe`, so it is taken out by hand: a match over it infers `Any` and
      * lands inert, which is the trap the module documents.
      */
    private[uic] def click(el: UI.Ast.Element)(using Frame): Any < Async =
        el.attrs.onClick match
            case Present(eff) => eff
            case Absent =>
                el.attrs.onClickEvt match
                    case Present(f) => f(UI.MouseEvent(el.attrs.identifier, UI.Modifiers.none))
                    case Absent     => throw new AssertionError("the element declares no click handler")

    /** The same, for a click that reached `el` THROUGH a control of the reader's own — an anchor in
      * a cell, a button in a row. Only the typed handler can be told, since the flag rides
      * [[kyo.UI.MouseEvent.onControl]]; an element that took the untyped form never asked.
      */
    private[uic] def clickViaControl(el: UI.Ast.Element)(using Frame): Any < Async =
        el.attrs.onClickEvt match
            case Present(f) =>
                f(UI.MouseEvent(el.attrs.identifier, UI.Modifiers.none, Absent, onControl = true))
            case Absent => throw new AssertionError("the element declares no typed click handler")

end UicTest
