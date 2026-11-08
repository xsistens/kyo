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

    /** Whether a [[kyo.UI.Loss]] a region of this suite reports fails the leaf that ran it: set by the suite of a component its wave
      * has freed of every loss (T19), so a change that brings one back is red where it is made. A suite that is not armed counts.
      */
    private[uic] def armed: Boolean = false

    // Every leaf runs under the loss counter, so each region a suite runs again reports what it takes from the reader; in an armed
    // suite a leaf that saw one fails, naming each loss.
    override def aroundLeaf[A](body: A < (Async & Abort[Any] & Scope))(using Frame): A < (Async & Abort[Any] & Scope) =
        Sync.defer(new java.util.concurrent.atomic.AtomicReference(Chunk.empty[String])).map { lost =>
            val count = UicTest.countLoss(getClass.getName)
            UI.devtools { event =>
                count(event)
                event.kind match
                    case UI.RenderKind.Lost(loss) =>
                        discard(lost.getAndUpdate(_.append(s"${UicTest.described(loss)} | region ${event.frame.position.show}")))
                    case _ => ()
                end match
            }(super.aroundLeaf(body)).map { a =>
                val seen = lost.get()
                if armed && seen.nonEmpty then
                    throw new AssertionError(s"a region of this armed suite took the reader's state (T13): ${seen.mkString("; ")}")
                else a
            }
        }

    /** A session's command channel for a page with no layout: every measure is answered as for an element not on the
      * page (`Absent`), and every other command goes nowhere. A component that measures before it publishes (an overlay
      * placing itself) then publishes, rather than waiting for a reply no page will send. Under [[UicTest.layout]] a
      * measure of an element it places is answered with its box.
      */
    private[uic] def headlessCommands(using Frame): UI.Commands < Sync =
        UicTest.layout.use { placed =>
            AtomicRef.init(Maybe.empty[UI.Commands]).map { channel =>
                UI.Commands.init {
                    case kyo.internal.HtmlOp.RequestMeasureById(id) =>
                        channel.get.map(_.fold(Kyo.unit)(cmds =>
                            placed.get(id) match
                                case Some(rect) => cmds.deliverMeasureById(id, rect)
                                case None       => cmds.deliverMeasureMissing(id)
                        ))
                    case _ => Kyo.unit
                }.map(cmds => channel.set(Present(cmds)).andThen(cmds))
            }
        }

    /** Whether `e` carries `cls` now: as a class of its own, or through a class channel whose signal holds true, which is
      * how a mounted component shows state it patches in place.
      */
    private[uic] def carriesClass(e: UI.Ast.Element, cls: String)(using Frame): Boolean < Sync =
        if e.attrs.cssClasses.contains(cls) then Kyo.lift(true)
        else e.attrs.reactiveClasses.get(cls).fold(Kyo.lift(false))(_.current)

    /** The value of `aria-$name` on `e` now: a plain attribute, or one a channel patches in place, read at its current value. */
    private[uic] def ariaNow(e: UI.Ast.Element, name: String)(using Frame): Maybe[String] < Sync =
        e.attrs.ariaAttrs.get(name) match
            case Some(v) => Kyo.lift(Present(v))
            case None    => e.attrs.reactiveAttrs.get(s"aria-$name").fold(Kyo.lift(Absent: Maybe[String]))(_.current.map(Present(_)))

    /** Runs a handler's effect. None of the handlers under test is meant to fail, so a failure fails the test. */
    private[uic] def handled(action: UI.Action)(using Frame): Any < Async =
        // A handler runs in a session: a mount draws no static copy without its focus commands any more.
        headlessCommands.map(cmds => kyo.internal.UICommands.commands.let(Present(cmds))(Abort.run[Any](action))).map {
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
        headlessCommands.map { cmds =>
            kyo.internal.UICommands.commands.let(Present(cmds)) {
                kyo.internal.ReactiveUI.normalize(ui, Seq.empty).map(root =>
                    kyo.internal.ReactiveUI.subscribe(root, kyo.internal.UIExchange.noop).unit
                )
            }
        }

    /** The attribute and boolean-attribute patches a native element's edit brings, oldest first, as `(name, value)`. Runs `ui` under the engine
      * and sends `event`, as the client would, to the first element whose tag holds `at` in the first page; with `mounted`, once a mount has
      * published. That one edit reaching the kernel is what a field with native state relies on to put back what its owner did not take; a
      * handler called directly goes past it.
      */
    private[uic] def assertsAfter(
        ui: UI,
        mounted: Boolean = false,
        settled: Chunk[(String, String)] => Boolean = _ => true
    )(at: String)(event: Seq[String] => kyo.internal.UIEvent)(using
        Frame
    ): Chunk[(String, String)] < Async =
        import kyo.internal.RecordingExchange.Op
        val log                                      = new kyo.internal.RecordingExchange
        def asserted: Chunk[(String, String)] < Sync = log.ops.map(_.collect {
            case op: Op.Attr     => (op.name, op.value)
            case op: Op.BoolAttr => (op.name, op.value.toString)
        })
        for
            html <- renderHtml(ui)
            path = pathHolding(html, at)
            cmds    <- headlessCommands
            patches <- kyo.internal.ReactiveUI.subscribed(ui, log, commands = Present(cmds)) { sub =>
                // A patch a channel sends after the event's own write comes later than the handler: `settled` says what to wait for.
                (if mounted then log.awaitOp(_.isInstanceOf[Op.Paint]) else Kyo.unit)
                    .andThen(sub.handle(path, event(path)))
                    .andThen(waitFor(asserted.map(settled)))
                    .andThen(asserted)
            }
        yield patches
        end for
    end assertsAfter

    /** Whether a native element's binding is the one-way kind. */
    private[uic] def follows(b: Maybe[UI.Bound[?]]): Boolean = b match
        case Present(_: UI.Bound.Follow[?]) => true
        case _                              => false

    /** Runs `ui` under the engine, hands `act` the tree its mounts and regions last published (the whole of `ui` when
      * none did), and answers with what the field published last once `act` made it publish again, so a test can read what
      * it now shows.
      *
      * A region publishes while it settles too, so `act` runs only once every ref in `reads` has an observer parked on it
      * again (`waiters`), which the engine does only after handling the value before: the barrier
      * [[DataTableReuseTest]] uses. No wait here has a time budget: each waits for the state it needs, and one that never comes
      * is the test's timeout, never an answer.
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
      * and the panel it opens publish as separate regions, in either order), and is that, newest first. It waits for as long
      * as that takes: a tree that never comes is the test's timeout, not an answer.
      */
    private[uic] def publishedAfterReady(
        ui: UI,
        ready: UI => Boolean < Sync,
        until: UI => Boolean < Sync = _ => true,
        reads: Seq[Signal.SignalRef[?]] = Nil
    )(acts: (UI => Any < Async)*)(using Frame, kyo.test.AssertScope): Maybe[UI] < Async =
        drive(ui, ready, reads, acts) { d =>
            for
                _ <- waitFor(
                    Sync.defer(d.published.size > d.publishedBefore).map(grew =>
                        if grew then d.since.map(until) else Kyo.lift(false)
                    )
                )
                answer <- d.since
            yield Present(answer)
        }

    /** Everything `ui` published once `until` holds over all of it after the gestures, newest first, with the trees of it that came
      * after the last gesture, and the ones before it, each with the path that published it. A component that patches itself in
      * place draws nothing of itself again after a host run: its state is read in the tree it published first, whose regions and
      * channels follow, and a tree it sends again after the gesture is one it had sent before at that path. `sent` holds as well
      * once the trees after the gesture satisfy it (handed the trees before it too), for a run that changes nothing a reader sees.
      */
    private[uic] def paintedAround(
        ui: UI,
        ready: UI => Boolean < Sync,
        until: UI => Boolean < Sync,
        sent: (Chunk[(Seq[String], UI)], Chunk[(Seq[String], UI)]) => Boolean = (_, _) => true
    )(acts: (UI => Any < Async)*)(using
        Frame,
        kyo.test.AssertScope
    ): (UI, Chunk[(Seq[String], UI)], Chunk[(Seq[String], UI)]) < Async =
        drive(ui, ready, Nil, acts) { d =>
            def all: UI < Sync = Sync.defer(UI.fragment(d.published*))
            for
                _ <- waitFor(UicTest.walkingLive.let(true)(all.map(until)))
                _ <- waitFor(Sync.defer(sent(
                    Chunk.from(d.paints.take(d.paints.size - d.publishedBefore)),
                    Chunk.from(d.paints.drop(d.paints.size - d.publishedBefore))
                )))
                every <- all
                since <- Sync.defer(Chunk.from(d.paints.take(d.paints.size - d.publishedBefore)))
                prior <- Sync.defer(Chunk.from(d.paints.drop(d.paints.size - d.publishedBefore)))
            yield (every, since, prior)
            end for
        }

    /** The paths, among `since`, that sent a tree holding an element of class `cls` they had not sent before: what was drawn again
      * after a gesture, as against sent again with a host region's run.
      */
    private[uic] def drawnAgain(since: Chunk[(Seq[String], UI)], prior: Chunk[(Seq[String], UI)], cls: String)(using
        Frame
    ): Chunk[Seq[String]] < Sync =
        Kyo.foreach(since)((path, t) =>
            UicTest.walkingLive.let(true)(elementsWithClass(t, cls)).map(found =>
                if found.isEmpty || prior.exists((at, before) => at == path && (before eq t)) then Chunk.empty else Chunk(path)
            )
        ).map(_.flatten)

    /** `act` on `ui`, then `sw` off and on again, each step once the one before shows: `act`'s result, then the off state, then the on
      * state again. Each is read in the newest tree published since its step that holds `root`, the component, so neither a late
      * repaint from before the step nor a region that does not show the component can answer for it. Writing the switch twice without
      * that barrier lets the second write overtake the first: the region then sees no change, publishes nothing, and nothing after
      * the last write could show the state coming back. The answer is that newest tree.
      */
    private[uic] def publishedThroughSwitch(
        ui: UI,
        sw: Signal.SignalRef[Boolean],
        root: String,
        on: UI => Boolean < Sync,
        off: UI => Boolean < Sync
    )(act: UI => Any < Async)(using Frame, kyo.test.AssertScope): UI < Async =
        drive(ui, _ => true, Nil, Seq(act)) { d =>
            // The newest tree holding the component among those published after the first `from`, if one has come yet.
            def newest(from: Int): Maybe[UI] < Sync =
                Sync.defer(d.published).map(now =>
                    Kyo.foreach(now.take(now.size - from))(t =>
                        elementsWithClass(t, root).map(rs => if rs.nonEmpty then Present(t) else Absent)
                    )
                        .map(found => Maybe.fromOption(found.flatten.headOption))
                )
            def shows(from: Int, cond: UI => Boolean < Sync): Unit < Async =
                waitFor(newest(from).map(_.fold(Kyo.lift(false))(cond)))
            for
                _      <- shows(0, on)
                n0     <- Sync.defer(d.published.size)
                _      <- sw.set(false)
                _      <- shows(n0, off)
                n1     <- Sync.defer(d.published.size)
                _      <- sw.set(true)
                _      <- shows(n1, on)
                answer <- newest(n1)
            yield answer.getOrElse(ui)
            end for
        }

    /** Runs `ui` under the engine and, once what it published satisfies `ready`, the gestures in a row, each on every tree
      * published so far, newest first, and tried again until it finds what it acts on. It ends with the last gesture, for a
      * test that reads what the gestures reported rather than what was drawn after them.
      */
    private[uic] def actedOn(ui: UI, ready: UI => Boolean < Sync)(acts: (UI => Any < Async)*)(using
        Frame,
        kyo.test.AssertScope
    ): Unit < Async =
        drive(ui, ready, Nil, acts)(_ => Kyo.unit)

    /** `ui` as its mount publishes it once `ready` holds, the tree a gesture acts on: the walks above see a mount as its
      * placeholder, which carries none of the wiring the mount adds. A mount that never publishes such a tree fails the
      * test.
      */
    private[uic] def mountedWhen(ui: UI)(ready: UI => Boolean < Sync)(using Frame, kyo.test.AssertScope): UI < Async =
        publishedAfterReady(ui, ready)().map(_.getOrElse(throw new AssertionError("never mounted")))

    /** [[mountedWhen]] for a tree that may hold no mount at all (a field bound one way that draws its state through
      * regions alone): once `ready` holds, everything its mounts published, newest first, or `ui` itself where nothing in
      * it publishes. A tree that never becomes ready fails the test.
      */
    private[uic] def readyTree(ui: UI)(ready: UI => Boolean < Sync)(using Frame, kyo.test.AssertScope): UI < Async =
        drive(ui, ready, Nil, Nil)(d => Sync.defer(d.published).map(seen => if seen.isEmpty then ui else UI.fragment(seen*)))

    /** Sets `ref` and waits until everything that observes it has handled the value and parked again, which is the barrier a
      * test needs between two writes that must both be seen: a value the observers have not reached yet is overtaken by the
      * next, and only the last is drawn. It counts only observers parked on the ref itself (`waiters`): one that reads it through a
      * derived signal is not counted, and with none counted the barrier passes at once. A switch the component reads that way is
      * written through [[publishedThroughSwitch]], which waits for what the component publishes instead.
      */
    private[uic] def setSettled[A](ref: Signal.SignalRef[A], a: A)(using Frame): Unit < Async =
        ref.waiters.map(before => ref.set(a).andThen(waitFor(ref.waiters.map(_ >= before))).unit)

    /** The first HTML a transport renders for `ui`, which is what a static page carries. */
    private[uic] def renderHtml(ui: UI)(using Frame): String < Async =
        UI.runRender(ui).take(1).run.map(_.mkString)

    /** Whether an element in `html` carries `cls` among the classes of its `class` attribute (a marker attribute that
      * names the class does not count).
      */
    private[uic] def classInHtml(html: String, cls: String): Boolean =
        "class=\"([^\"]*)\"".r.findAllMatchIn(html).exists(_.group(1).split(' ').contains(cls))

    /** [[publishedAfterReady]] for what reaches the page as an attribute patch rather than a region: whether, after the
      * gestures, the field patched an attribute `holds` accepts, as `(name, value)`. A header's sort state, say, rides
      * that channel and never publishes a region of its own.
      */
    private[uic] def attrPatchedAfter(ui: UI, ready: UI => Boolean < Sync)(acts: (UI => Any < Async)*)(
        holds: ((String, String)) => Boolean
    )(using Frame, kyo.test.AssertScope): Boolean < Async =
        drive(ui, ready, Nil, acts)(d =>
            waitFor(Sync.defer(d.patched.take(d.patched.size - d.patchedBefore).exists(holds))).andThen(true)
        )

    /** What [[drive]] hands its `finish`: everything the field published and patched, and how much of each came before
      * the last gesture.
      */
    final private class Driven(
        log: kyo.internal.RecordingExchange,
        val publishedBefore: Int,
        val patchedBefore: Int
    )(using Frame):
        def published: List[UI]             = UicTest.publishedIn(log)
        def patched: List[(String, String)] = UicTest.patchedIn(log)

        /** Every tree published, with the path of the region or mount that published it, newest first. */
        def paints: List[(Seq[String], UI)] =
            log.recorded.collect { case op: kyo.internal.RecordingExchange.Op.Paint => (op.path, op.ui) }.toList.reverse

        /** Every tree published since the last gesture, newest first. */
        def since: UI < Sync =
            Sync.defer(published).map(now => UI.fragment(now.take(now.size - publishedBefore)*))
    end Driven

    /** Runs `ui` under the engine, waits for it to be `ready`, runs the gestures (each tried again until it finds what it
      * acts on, and each seeing every tree published so far, newest first), then hands `finish` what came of them.
      */
    private def drive[R](ui: UI, ready: UI => Boolean < Sync, reads: Seq[Signal.SignalRef[?]], acts: Seq[UI => Any < Async])(
        finish: Driven => R < Async
    )(using Frame, kyo.test.AssertScope): R < Async =
        val log            = new kyo.internal.RecordingExchange
        def seen: List[UI] = UicTest.publishedIn(log)
        def all: UI < Sync = Sync.defer(seen).map(trees => if trees.isEmpty then ui else UI.fragment(trees*))
        // Once anything is published the walk is of a live page.
        def walked[A, S](v: A < S)(using Frame): A < (S & Sync) =
            Sync.defer(seen.nonEmpty).map(live => UicTest.walkingLive.let(live)(v))
        headlessCommands.map { cmds =>
            kyo.internal.ReactiveUI.subscribed(ui, log, commands = Present(cmds)) { _ =>
                for
                    // A mount publishes what it wired; act on that. A field with no mount publishes nothing here, and is acted on as it
                    // rendered, where it draws what is inside: waiting for a publish from it would wait for something that never comes.
                    mounts <- holdsMount(ui)
                    _      <- if mounts then waitFor(Sync.defer(seen.nonEmpty)) else Kyo.unit
                    _      <- waitFor(Kyo.foreach(reads)(_.waiters).map(_.forall(_ > 0)))
                    _      <- waitFor(walked(all.map(ready)))
                    last   <- Kyo.foldLeft(acts)((0, 0)) { (_, act) =>
                        for
                            before <- Sync.defer((seen.size, UicTest.patchedIn(log).size))
                            _      <- waitFor(
                                walked(all).map(t =>
                                    walked(Abort.run[AssertionError](Abort.catching[AssertionError](act(t)))).map(_.isSuccess)
                                )
                            )
                        yield before
                    }
                    r <- finish(Driven(log, last._1, last._2))
                yield r
            }
        }
    end drive

    /** Waits until `cond` holds, looking again after every short pause. No budget: what passes a test is the state, and a state that
      * never comes is the test's timeout. The pause is pacing, never a pass condition.
      */
    private[uic] def waitFor(cond: => Boolean < Async)(using Frame): Unit < Async =
        Loop(()) { _ =>
            cond.map(ok => if ok then Loop.done(()) else Async.sleep(10.millis).andThen(Loop.continue(())))
        }

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
            case d: UI.Ast.Drawn         => drawnContent(d).map(elements)
            case m: UI.Ast.Mounted       =>
                m.placeholderUI match
                    case Present(ui) => elements(ui)
                    case Absent      => Chunk.empty
            case _ => Chunk.empty

    /** Whether the tree, as it renders now, holds a mount anywhere: what decides whether a live page publishes it. */
    private[uic] def holdsMount(node: UI)(using Frame): Boolean < Sync =
        node match
            case _: UI.Ast.MountNode     => Kyo.lift(true)
            case e: UI.Ast.Element       => Kyo.foreach(e.children)(holdsMount).map(_.exists(identity))
            case f: UI.Ast.Fragment[?]   => Kyo.foreach(f.children)(holdsMount).map(_.exists(identity))
            case k: UI.Ast.KeyedChild[?] => holdsMount(k.child)
            case r: UI.Ast.Reactive[?]   => r.signal.current(using r.frame).map(holdsMount)
            case f: UI.Ast.Foreach[?, ?] =>
                f.applyTyped([T] =>
                    (signal: Signal[Chunk[T]], _: Maybe[T => String], render: (Int, T) => UI) =>
                        signal.current(using f.frame).map(items =>
                            Kyo.foreach(items.zipWithIndex)((item, i) => holdsMount(render(i, item))).map(_.exists(identity))
                        )
                )
            case _ => Kyo.lift(false)

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
            case d: UI.Ast.Drawn   => drawnContent(d).map(reactiveNodes)
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
            case d: UI.Ast.Drawn         => drawnContent(d).map(foreachNodes)
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
                if it.p.itemsV.nonEmpty then p :: loop(it.p.itemsV, p) else Nil
            }
        MenuItemScope.reading(entries) match
            case ReactiveValue.Const(items) => loop(items, Nil)
            case ReactiveValue.Dyn(_)       => throw new AssertionError("submenu paths are read from fixed entries")
    end submenuPaths

    /** The root indices of a MegaMenu that open a panel. */
    private[uic] def panelPaths(entries: List[MegaMenuItemLiveEntry])(using Frame): List[List[Int]] =
        MegaMenuItemScope.reading(entries) match
            case ReactiveValue.Const(items) => items.zipWithIndex.collect { case (it, i) if it.p.columnsV.nonEmpty => List(i) }
            case ReactiveValue.Dyn(_)       => throw new AssertionError("panel paths are read from fixed entries")

    /** Paths of every group of a CascadeSelect's options. */
    private[uic] def groupPaths[A](c: CascadeSelect[A])(using Frame): List[List[Int]] =
        def loop(is: List[CascadeItem[A]], prefix: List[Int]): List[List[Int]] =
            is.zipWithIndex.flatMap {
                case (CascadeItem.Group(_, children), i) =>
                    val p = prefix :+ i
                    p :: loop(children, p)
                case _ => Nil
            }
        Entry.reading((), c.p.itemSources) match
            case ReactiveValue.Const(roots) => loop(roots, Nil)
            case ReactiveValue.Dyn(_)       => throw new AssertionError("group paths are read from fixed roots")
    end groupPaths

    /** How many regions sit above the mount of the control drawn with `cls`. Unlike [[regionsAbove]] it stops at the
      * mount, so the regions a control draws inside itself do not count: a region above the mount is one whose every
      * repaint starts the control's mount again.
      */
    private[uic] def regionsAboveMount(node: UI, cls: String)(using Frame): Int < Sync =
        def holds(n: UI): Boolean < Sync               = elementsWithClass(n, cls).map(_.nonEmpty)
        def walk(n: UI, depth: Int): Chunk[Int] < Sync =
            n match
                case d: UI.Ast.Drawn   => drawnContent(d).map(holds).map(found => if found then Chunk(depth) else Chunk.empty)
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
                case d: UI.Ast.Drawn         => drawnContent(d).map(walk(_, depth))
                case m: UI.Ast.Mounted       =>
                    m.placeholderUI match
                        case Present(ui) => walk(ui, depth)
                        case Absent      => Chunk.empty
                case _ => Chunk.empty
        walk(node, 0).map(ds =>
            if ds.isEmpty then throw new AssertionError(s"no element with class $cls") else ds.min
        )
    end regionsAbove

    /** How many mounts sit above the nearest element carrying `cls`, seen through their placeholders. One is what a component
      * that mints its state beside its ids wants: a slot that adds a mount of its own there is a mount inside a mount for every
      * instance, and while a mount restarts kyo-ui shows an inert snapshot no click or key reaches.
      */
    /** Runs `v` as a mount effect runs, for what only works in one (`UI.id`, `ContextMenu.state`). */
    private[uic] def inDraw[A, S](v: A < S)(using Frame): A < S =
        kyo.internal.DrawContext.current.let(Present(new kyo.internal.DrawContext("kyo-r")))(v)

    /** What a drawn mount shows, for a walk of the tree: its draw, at no place in particular, with nothing started. In a walk of what a live
      * page published it shows nothing: its instance publishes its content itself, and a draw of its own here would hand a gesture state no
      * instance holds.
      */
    private[uic] def drawnContent(d: UI.Ast.Drawn)(using Frame): UI < Sync =
        d.display match
            case Present(shown) => shown
            case Absent         =>
                UicTest.walkingLive.use(live =>
                    if live then UI.empty
                    else
                        kyo.internal.MountDraw.run(
                            d,
                            kyo.internal.ReactiveRegion.RegionIdentity(Seq.empty, 0),
                            kyo.internal.DrawContext.IdScope.root,
                            0
                        )
                            .map(_.content(kyo.internal.MountedSpec(d, Seq.empty, Nil)))
                )

    private[uic] def mountsAbove(node: UI, cls: String)(using Frame): Int < Sync =
        def walk(n: UI, depth: Int): Chunk[Int] < Sync =
            n match
                case e: UI.Ast.Element if e.attrs.cssClasses.contains(cls) => Chunk(depth)
                case e: UI.Ast.Element                                     => Kyo.foreach(e.children)(walk(_, depth)).map(_.flattenChunk)
                case r: UI.Ast.Reactive[?]                                 => r.signal.current(using r.frame).map(walk(_, depth))
                case f: UI.Ast.Fragment[?]                                 => Kyo.foreach(f.children)(walk(_, depth)).map(_.flattenChunk)
                case f: UI.Ast.Foreach[?, ?]                               =>
                    f.applyTyped([T] =>
                        (signal: Signal[Chunk[T]], _: Maybe[T => String], render: (Int, T) => UI) =>
                            signal.current(using f.frame).map { items =>
                                Kyo.foreach(items.zipWithIndex) { (item, i) => walk(render(i, item), depth) }
                                    .map(_.flattenChunk)
                            }
                    )
                case k: UI.Ast.KeyedChild[?] => walk(k.child, depth)
                case d: UI.Ast.Drawn         => drawnContent(d).map(walk(_, depth + 1))
                case m: UI.Ast.Mounted       =>
                    m.placeholderUI match
                        case Present(ui) => walk(ui, depth + 1)
                        case Absent      => Chunk.empty
                case _ => Chunk.empty
        walk(node, 0).map(ds =>
            if ds.isEmpty then throw new AssertionError(s"no element with class $cls") else ds.min
        )
    end mountsAbove

    /** The cards on the page as it lives: a walk of the tree that, at a mount, reads what the mount publishes rather than its
      * placeholder. A card the static render does not hold, or a second one the mount adds, shows here and nowhere else.
      */
    private[uic] def cardsOnPage(ui: UI)(using Frame): Chunk[UI.Ast.Element] < Async =
        def walk(n: UI): Chunk[UI.Ast.Element] < (Abort[Any] & Async & Scope) =
            n match
                case e: UI.Ast.Element =>
                    val own = if e.attrs.cssClasses.contains(UicTest.cardClass) then Chunk(e) else Chunk.empty
                    Kyo.foreach(e.children)(walk).map(cs => own ++ cs.flattenChunk)
                case r: UI.Ast.Reactive[?]   => r.signal.current(using r.frame).map(walk)
                case f: UI.Ast.Fragment[?]   => Kyo.foreach(f.children)(walk).map(_.flattenChunk)
                case f: UI.Ast.Foreach[?, ?] =>
                    f.applyTyped([T] =>
                        (signal: Signal[Chunk[T]], _: Maybe[T => String], render: (Int, T) => UI) =>
                            signal.current(using f.frame).map { items =>
                                Kyo.foreach(items.zipWithIndex) { (item, i) => walk(render(i, item)) }.map(_.flattenChunk)
                            }
                    )
                case k: UI.Ast.KeyedChild[?] => walk(k.child)
                case d: UI.Ast.Drawn         => drawnContent(d).map(walk)
                case m: UI.Ast.Mounted       => m.effect.map(walk)
                case _                       => Chunk.empty
        headlessCommands.map { cmds =>
            kyo.internal.UICommands.commands.let(Present(cmds)) {
                Abort.run[Any](Scope.run(walk(ui))).map {
                    case Result.Success(cards) => cards
                    case other                 => throw new AssertionError(s"a mount failed: $other")
                }
            }
        }
    end cardsOnPage

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
      * the sheet does not give it. A fragment and a keyed child draw their children in place, so their text is the element's;
      * a region's is not read here (see [[textNow]]).
      */
    private[uic] def textOf(e: UI.Ast.Element)(using Frame): String =
        def walk(u: UI): String = u match
            case t: UI.Ast.Text          => t.value
            case el: UI.Ast.Element      => el.children.map(walk).mkString
            case f: UI.Ast.Fragment[?]   => f.children.map(walk).mkString
            case k: UI.Ast.KeyedChild[?] => walk(k.child)
            case _                       => ""
        e.children.map(walk).mkString
    end textOf

    /** The text of `e` as it shows now, a text that follows a signal (a text channel) read at its current value. */
    private[uic] def textNow(e: UI.Ast.Element)(using Frame): String < Sync =
        def walk(u: UI): String < Sync = u match
            case t: UI.Ast.Text          => t.value
            case el: UI.Ast.Element      => Kyo.foreach(el.children)(walk).map(_.mkString)
            case r: UI.Ast.Reactive[?]   => r.signal.current(using r.frame).map(walk)
            case f: UI.Ast.Fragment[?]   => Kyo.foreach(f.children)(walk).map(_.mkString)
            case k: UI.Ast.KeyedChild[?] => walk(k.child)
            case _                       => ""
        Kyo.foreach(e.children)(walk).map(_.mkString)
    end textNow

    /** Sends one key pressed on `el` itself to its own handlers, as the dispatcher would: the self-only one first. */
    private[uic] def press(el: UI.Ast.Element, key: UI.Keyboard, mods: UI.Modifiers = UI.Modifiers.none)(
        using Frame
    ): Any < Async =
        val event = UI.KeyboardEvent(key, mods, el.attrs.identifier)
        if el.attrs.onKeyDown.isEmpty && el.attrs.onKeyDownSelf.isEmpty then
            throw new AssertionError("the element declares no key handler")
        val self: Any < Async = el.attrs.onKeyDownSelf.fold[Any < Async](())(f => handled(f(event)))
        self.andThen(el.attrs.onKeyDown.fold[Any < Async](())(f => handled(f(event))))
    end press

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

    /** The reader typing `text` into `field`, the way the transport dispatches an input event: a field bound to a ref has
      * it written first, then `onInput` is told. A field that has neither sends nothing at all, since the browser keeps
      * the text itself, so nothing happens here either.
      */
    private[uic] def typeInto(field: UI.Ast.TextInput, text: String)(using Frame): Any < Async =
        transported(field, text, field.onInput)

    /** The reader committing `text` in `field` (a blur or Enter after an edit), the way the transport dispatches a change
      * event: a field bound to a ref has it written first, then `onChange` is told.
      */
    private[uic] def commitInto(field: UI.Ast.TextInput, text: String)(using Frame): Any < Async =
        transported(field, text, field.onChange)

    /** Focus leaving `field` while it holds `text`, the way the transport dispatches a blur: the payload-free `onBlur`,
      * then the one with the event, then the one with the text the client read off the field. The field fails the
      * test if it declares none of them.
      */
    private[uic] def blurOut(field: UI.Ast.TextInput, text: String)(using Frame): Any < Async =
        val attrs = field.attrs
        if attrs.onBlur.isEmpty && attrs.onBlurEvt.isEmpty && field.onBlurText.isEmpty then
            throw new AssertionError("the field declares no blur handler")
        val plain: UI.Action = attrs.onBlur.fold[UI.Action](())(identity)
        val evt: UI.Action   = attrs.onBlurEvt.fold[UI.Action](())(f => f(UI.MouseEvent(attrs.identifier, UI.Modifiers.none)))
        val told: UI.Action  = field.onBlurText.fold[UI.Action](())(f => f(UI.MouseEvent(attrs.identifier, UI.Modifiers.none), text))
        handled(plain.andThen(evt).andThen(told))
    end blurOut

    private def transported(field: UI.Ast.TextInput, text: String, told: Maybe[String => UI.Action])(using Frame): Any < Async =
        val write: UI.Action = field.value match
            case Present(UI.Bound.Ref(ref)) => ref.set(text)
            case _                          => ()
        handled(write.andThen(told.fold[UI.Action](())(f => f(text))))
    end transported

    /** Clicks the first element `target` matches the way the dispatcher delivers a click: the target's own handlers
      * first, then each enclosing element's, innermost first, until an element that declared a click handler also set
      * `stopPropagation(true)` (`kyo.internal.EventDispatch.keepBubbling`). `onClickSelf` counts on the target alone, and a disabled
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
                case d: UI.Ast.Drawn   => drawnContent(d).map(chains(_, above))
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

    /** The `data-kyo-path` of the first element whose tag holds `text` in `html`, which is the address the client sends it events by. */
    private[uic] def pathHolding(html: String, text: String): Seq[String] =
        val tag = ("""<[^>]*""" + java.util.regex.Pattern.quote(text) + """[^>]*>""").r
            .findFirstIn(html).getOrElse(throw new AssertionError(s"no element holding $text in $html"))
        val raw = """data-kyo-path="([^"]*)"""".r.findFirstMatchIn(tag).map(_.group(1))
            .getOrElse(throw new AssertionError(s"the element holding $text carries no path: $tag"))
        if raw.isEmpty then Seq.empty else raw.split('.').toSeq
    end pathHolding

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

object UicTest:

    /** Whether a walk of a tree is of what a live page published (see [[UicTest.drawnContent]]). */
    private[uic] val walkingLive: Local[Boolean] = Local.init(false)

    /** Every tree `log` was asked to paint, newest first. */
    private[uic] def publishedIn(log: kyo.internal.RecordingExchange): List[UI] =
        log.recorded.collect { case op: kyo.internal.RecordingExchange.Op.Paint => op.ui }.toList.reverse

    /** Every attribute patch `log` took, as `(name, value)`, newest first. */
    private[uic] def patchedIn(log: kyo.internal.RecordingExchange): List[(String, String)] =
        log.recorded.collect { case op: kyo.internal.RecordingExchange.Op.Attr => (op.name, op.value) }.toList.reverse

    /** Where the elements of a headless page sit, by id: what [[UicTest.headlessCommands]] answers a measure with. Empty,
      * the page has no layout and every measure is answered as for an element not on it. A case that drives a gesture
      * the component works out from geometry (a column dragged across the header) lets it run with the boxes it would
      * meet in a browser.
      */
    private[uic] val layout: Local[Map[String, UI.Rect]] = Local.init(Map.empty[String, UI.Rect])

    /** The class a card carries. */
    private[uic] val cardClass = "p-uic-diagnostic"

    /** The class of the marker a contract's host region draws beside the component, saying which run it is. */
    private[uic] val hostRunClass = "p-uic-test-host-run"

    /** A loss as one line: what it takes, from which element, and where that element was built. */
    private[uic] def described(loss: UI.Loss): String = loss match
        case UI.Loss.StatedValue(element, attribute, frame) => s"stated $attribute | $element | ${frame.position.show}"
        case UI.Loss.UnkeyedMount(frame)                    => s"unkeyed mount | | ${frame.position.show}"

    // The lines `countLoss` has written, so each suite names each place once.
    private val lossesSeen = new java.util.concurrent.atomic.AtomicReference(Set.empty[String])

    /** A devtools sink that writes every [[kyo.UI.Loss]] a suite's regions report, once per suite and place, as a `UIC-LOSS` line:
      * the count of the places in uic that lose the reader's state when the region around them runs again (T13, before the check
      * fails a test).
      */
    private[uic] def countLoss(suite: String): UI.RenderEvent => Unit = event =>
        event.kind match
            case UI.RenderKind.Lost(loss) =>
                val line = s"UIC-LOSS $suite | ${described(loss)} | region ${event.frame.position.show}"
                if !lossesSeen.getAndUpdate(_ + line).contains(line) then java.lang.System.out.println(line)
            case _ => ()

end UicTest
