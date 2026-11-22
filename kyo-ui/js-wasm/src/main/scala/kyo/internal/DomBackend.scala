package kyo.internal

import kyo.*
import kyo.internal.DomReactiveRegions.eachSpanNode
import kyo.internal.DomReactiveRegions.spanClose
import org.scalajs.dom
import org.scalajs.dom.document
import scala.annotation.tailrec
import scala.scalajs.js

/** Scala.js UI backend. Mounts a UI into the browser DOM. */
private[kyo] object DomBackend:

    private[kyo] trait MountDiagnostics:
        def channelClosed(): Unit
        def drainInterrupting(): Unit
        def drainJoined(): Unit
        def dragRuntimeInstalled(runtime: DomDragRuntime.Handle): Unit = ()
        def dragEventQueued(event: UIEvent): Unit                      = ()
        def dragEventHandled(event: UIEvent): Unit                     = ()
        def drainInstalled(drain: Fiber[Unit, Any]): Unit              = ()
        def drainStarted(): Unit                                       = ()
        def regionApplied(): Unit                                      = ()
        def regionsOpened(regions: DomReactiveRegions): Unit           = ()
    end MountDiagnostics

    private object NoMountDiagnostics extends MountDiagnostics:
        def channelClosed(): Unit     = ()
        def drainInterrupting(): Unit = ()
        def drainJoined(): Unit       = ()
    end NoMountDiagnostics

    /** One seeded `data-kyo-focus-auto` element and where focus should go when it leaves the document.
      *
      * @param path
      *   `data-kyo-path` of the seeded element
      * @param returnTo
      *   `data-kyo-path` of the element focused just before seeding, `Absent` when nothing was focused
      * @param restore
      *   whether the seeded element declared `data-kyo-focus-restore`
      */
    final private case class FocusSeed(path: String, returnTo: Maybe[String], restore: Boolean)

    /** Seeded focus-auto elements, innermost last. Mirrors `__focusReturnStack` in HtmlRenderer.clientJs. Module-level
      * mutable state is safe: all mutation runs inside `Sync.defer` on the single-threaded JS runtime.
      */
    private var focusReturnStack: Chunk[FocusSeed] = Chunk.empty

    /** What a list patch read before anything moved, handed through [[MorphScript]] and back. */
    final private class ListState(
        val locator: Maybe[RangeFocus],
        val selectionStart: Maybe[Int],
        val selectionEnd: Maybe[Int],
        val oldEnter: Set[String],
        val oldFocusAuto: Set[String],
        val ghosts: Seq[(dom.Element, dom.Element, String)]
    )

    /** [[MorphScript]] over this client's hooks, built once for the page. Reconciling a re-rendered region against its live nodes instead of
      * replacing them is what keeps node identity across a paint: focus, caret, scroll position, an imperatively bound attribute, anything a
      * DOM-local expando holds. Replacing the range is always correct and always loses all of it, so it stays the fallback.
      *
      * A host whose `Content-Security-Policy` lacks `unsafe-eval` fails here, at mount, loudly (see `mountInto`).
      */
    private[kyo] lazy val morphScript: js.Dynamic =
        given Frame                                               = Frame.internal
        def elements(nodes: js.Array[dom.Node]): Seq[dom.Element] = nodes.toSeq.collect { case e: dom.Element => e }
        val hooks                                                 = js.Dynamic.literal(
            syncField = js.Any.fromFunction3((el: dom.Element, name: String, value: String) => syncFieldProperty(el, name, value)),
            portalTwin = js.Any.fromFunction1((path: String) => portalTwin(path)),
            rewrite = js.Any.fromFunction3((slot: dom.Node, twin: dom.Node, change: js.Function0[dom.Node]) =>
                discard(DomReactiveRegions.rewriting(slot, twin)(Maybe(change())))
            ),
            beginAnimations = js.Any.fromFunction1((node: dom.Node) =>
                node match
                    case element: dom.Element => beginAnimationsSync(element)
                    case _                    => ()
            ),
            // An onAttached hook may have put content of its own inside an element; the server-push client has no such hook.
            guardAttached = true,
            listBefore = js.Any.fromFunction3((target: js.Dynamic, disturbed: js.Array[dom.Node], repainted: js.Array[dom.Node]) =>
                listBefore(target.scalaTarget.asInstanceOf[DomReactiveRegions.MorphTarget], elements(disturbed), elements(repainted))
                    .asInstanceOf[js.Any]
            ),
            listAfter = js.Any.fromFunction3((state: js.Any, target: js.Dynamic, touched: js.Array[dom.Node]) =>
                listAfter(state.asInstanceOf[ListState], target.scalaTarget.asInstanceOf[DomReactiveRegions.MorphTarget], elements(touched))
            )
        )
        js.Dynamic.newInstance(js.Dynamic.global.Function)("h", MorphScript.js).asInstanceOf[js.Function1[js.Any, js.Dynamic]](hooks)
    end morphScript

    /** Mirror a patched `value` onto the field's DOM PROPERTY, which is what an input or textarea actually renders.
      * The property stops tracking the attribute the first time the user types, so `setAttribute("value", ...)` alone
      * is invisible on any field that has been touched: a clear button writes the bound ref, the attribute changes,
      * and the field still shows what was typed. Assigning only on a real difference leaves a focused field's caret
      * alone, since the echo of the user's own keystroke compares equal. Mirrors `__kyoSyncField` in
      * HtmlRenderer.clientJs.
      */
    private def syncFieldProperty(el: dom.Element, name: String, value: String): Unit =
        if name == "value" && (el.tagName == "INPUT" || el.tagName == "TEXTAREA") then
            val dyn = el.asInstanceOf[js.Dynamic]
            if dyn.value.asInstanceOf[String] != value then
                dyn.value = value
                applyFilled(el)
            end if
        else if name == "value" && el.tagName == "SELECT" then
            val dyn = el.asInstanceOf[js.Dynamic]
            if dyn.value.asInstanceOf[String] != value then dyn.value = value
        end if
    end syncFieldProperty

    /** A patch of `checked`: a control shows its PROPERTY once the reader has toggled it, so the attribute alone would not move it. The browser
      * took the group's selection from the other radios when the reader clicked this one, and the one that held it has no patch of its own
      * coming, so each bound radio of the group shows what its attribute says again. A free radio's tick stays. Twin of `__kyoSyncChecked`.
      */
    private def syncChecked(el: dom.Element, on: Boolean): Unit =
        val dyn = el.asInstanceOf[js.Dynamic]
        dyn.checked = on
        val name = dyn.name.asInstanceOf[String]
        if dyn.selectDynamic("type").asInstanceOf[String] == "radio" && name.nonEmpty then
            val radios = document.querySelectorAll("input[type=radio][data-kyo-bound]")
            var i      = 0
            while i < radios.length do
                val radio = radios(i).asInstanceOf[dom.Element]
                val other = radio.asInstanceOf[js.Dynamic]
                if other.name.asInstanceOf[String] == name && js.special.strictEquals(other.form, dyn.form) then
                    other.checked = radio.hasAttribute("checked")
                i += 1
            end while
        end if
    end syncChecked

    /** `filledClass`: toggles the classes `field` marks (on itself, and on the closest ancestor its selector names) from
      * whether it holds text. Twin of `__kyoFilled` in HtmlRenderer.clientJs; both read `FilledClass`.
      */
    private[kyo] def applyFilled(field: dom.Element): Unit =
        val self = field.getAttribute(FilledClass.classAttr)
        val up   = field.getAttribute(FilledClass.upAttr)
        if self != null || up != null then
            val text = field.asInstanceOf[js.Dynamic].value
            val on   = js.typeOf(text) == "string" && FilledClass.filled(text.asInstanceOf[String])
            if self != null && self.nonEmpty then discard(field.classList.toggle(self, on))
            if up != null && up.nonEmpty then
                val target = field.closest(field.getAttribute(FilledClass.targetAttr))
                if target != null then discard(target.classList.toggle(up, on))
            end if
        end if
    end applyFilled

    /** [[applyFilled]] for every field that carries a marker: after a patch, which may have written a target's `class`
      * attribute from what the server last knew, and at boot. Twin of `__kyoSweepFilled` in HtmlRenderer.clientJs.
      */
    private def sweepFilled(): Unit =
        if saw(Marker.Filled) then
            val fs = document.querySelectorAll(s"[${FilledClass.classAttr}],[${FilledClass.upAttr}]")
            (0 until fs.length).foreach(i => applyFilled(fs(i).asInstanceOf[dom.Element]))
    end sweepFilled

    /** The reconciler of a live run, for the table that holds the two clients to one answer (`DomBackendMorphParityTest`). */
    private[kyo] def morphRun(parent: dom.Element, fromStart: dom.Node, fromEnd: dom.Node, toStart: dom.Node, toEnd: dom.Node): Unit =
        discard(morphScript.run(parent, fromStart, fromEnd, toStart, toEnd))

    /** Reconcile a region's live nodes toward the incoming payload instead of replacing them: the `tryMorph` the browser mount hands
      * [[DomReactiveRegions.replaceWith]].
      *
      * Nested ranges are reconciled as logical children, so a region containing another region morphs like any
      * other; `replaceWith` re-reads the live markers afterwards and hands the registry what is actually there.
      */
    private[kyo] def tryMorphRange(target: DomReactiveRegions.MorphTarget)(using Frame): Boolean =
        morphRun(
            target.parent.asInstanceOf[dom.Element],
            target.start.nextSibling,
            target.end,
            target.fragment.firstChild,
            null
        )
        true
    end tryMorphRange

    /** Reconcile a list region against a row order rather than a rendered document. The payload holds only the rows that changed; every other
      * row is named by key and stays where it is. Answers `false`, having changed nothing, when the rows are not individually addressable;
      * the caller then repaints the whole list.
      */
    private[kyo] def listPatch(
        target: DomReactiveRegions.MorphTarget,
        scope: DomReactiveRegions.PatchScope,
        path: Seq[String],
        rows: Seq[ListRow]
    )(using Frame): Boolean =
        val pathAttr = path.mkString(".")
        val keys     = js.Array[String]()
        val changed  = js.Array[Boolean]()
        rows.foreach { row =>
            discard(keys.push(if pathAttr.isEmpty then row.key else s"$pathAttr.${row.key}"))
            discard(changed.push(row.changed))
        }
        val scriptTarget = js.Dynamic.literal(
            parent = target.parent,
            start = target.start,
            end = target.end,
            fragment = target.fragment,
            scalaTarget = target.asInstanceOf[js.Any]
        )
        val scriptScope = js.Dynamic.literal(
            retire = js.Any.fromFunction1((first: dom.Node) => scope.retire(first)),
            placed = js.Any.fromFunction2((first: dom.Node, until: dom.Node) => scope.placed(first, until))
        )
        morphScript.applyList(scriptTarget, scriptScope, keys, changed).asInstanceOf[Boolean]
    end listPatch

    /** What a list patch reads before anything moves. Focus spans the whole region, not just the disturbed rows: a retained row keeps its DOM,
      * but moving it is a remove and an insert, which blurs whatever it holds, so a pure reorder is the case that would drop the caret.
      */
    private def listBefore(target: DomReactiveRegions.MorphTarget, disturbed: Seq[dom.Element], repainted: Seq[dom.Element]): ListState =
        val active = Maybe(document.activeElement).filter(el => (el ne document.body) && containsAny(target.oldElements, el))
        val (selectionStart, selectionEnd) = active.map(readSelection).getOrElse((Absent, Absent))
        new ListState(
            active.flatMap(focusLocator(target.oldElements, _)),
            selectionStart,
            selectionEnd,
            enterPaths(repainted),
            focusAutoPaths(repainted),
            prepareLeaveGhosts(disturbed, leavePaths(target.newElements))
        )
    end listBefore

    /** What a list patch does once the rows stand: the properties, focus and caret, enter and focus seeds, portals, ghosts, sweeps. */
    private def listAfter(state: ListState, target: DomReactiveRegions.MorphTarget, touched: Seq[dom.Element]): Unit =
        val live = elementsBetweenLive(target)
        live.foreach(applyJsPropsSync)
        state.locator.flatMap(resolveFocus(live, _)).foreach { element =>
            focusNoScroll(element)
            (state.selectionStart, state.selectionEnd) match
                case (Present(start), Present(end)) => setSelection(element, start, end)
                case _                              => ()
        }
        seedEnter(touched, state.oldEnter)
        seedFocusAuto(touched, state.oldFocusAuto)
        touched.foreach(adoptPortals)
        retireOrphanTwins()
        spawnGhosts(state.ghosts)
        sweepFocusAuto()
        sweepScrollAuto()
        sweepFilled()
    end listAfter

    private def elementsBetweenLive(target: DomReactiveRegions.MorphTarget): Seq[dom.Element] =
        val out            = scala.collection.mutable.ArrayBuffer.empty[dom.Element]
        var node: dom.Node = target.start.nextSibling
        while node != null && !(node eq target.end) do
            node match
                case element: dom.Element => out += element
                case _                    => ()
            node = node.nextSibling
        end while
        out.toSeq
    end elementsBetweenLive

    /** What one browser mount owns, so that tearing one mount down leaves another one on the page delivering.
      *
      * `commands` and `events` are late-bound: `commands` breaks the emit<->Commands construction cycle (emit resolves
      * measure callbacks via Commands, Commands needs emit), and `events` is the mount's drain channel, through which
      * its viewport listeners (raw JS callbacks, outside any Kyo context) bridge `deliverMeasureById` back in via
      * [[fireFromJs]]. Both are set in `mountInto` before any op can be emitted. `viewportObservers` holds, per element
      * id, the single handler registered for BOTH `window` scroll (capture phase) and resize; Unobserve removes it from
      * both and drops the entry.
      */
    final private class LocalSession:
        var commands: UI.Commands                                                   = null
        var events: Maybe[Channel[Unit < Async]]                                    = Absent
        val viewportObservers: js.WrappedMap[String, js.Function1[dom.Event, Unit]] =
            new js.WrappedMap(js.Map.empty[String, js.Function1[dom.Event, Unit]])
    end LocalSession

    /** Mark attribute `name` on `el` as owned by the imperative id-addressed channel (SetClassById/SetStyleById),
      * applied out of the render pass so CSS transitions on the toggled class/style fire. The owned names live in a
      * `__kyoOwn` expando dict ON the element, so the flag is reclaimed with the node (no session-lived set that only
      * ever grows) and [[MorphScript]] shields each owned attribute BY NAME. Mirrors `__kyoMark` in HtmlRenderer.clientJs.
      */
    private def markOwned(el: dom.Element, name: String): Unit =
        val d   = el.asInstanceOf[js.Dynamic]
        val own =
            if js.isUndefined(d.__kyoOwn) then
                val fresh = js.Dictionary.empty[Boolean]
                d.__kyoOwn = fresh.asInstanceOf[js.Any]
                fresh
            else d.__kyoOwn.asInstanceOf[js.Dictionary[Boolean]]
        own.update(name, true)
    end markOwned

    /** Mount a UI into the page body. */
    def mount(ui: UI)(using Frame): Unit < (Async & Scope) =
        mountInto(ui, document.body, NoMountDiagnostics)

    private[kyo] def mount(ui: UI, diagnostics: MountDiagnostics)(using Frame): Unit < (Async & Scope) =
        mountInto(ui, document.body, diagnostics)

    /** Mount a UI into a specific DOM element selected by CSS selector. */
    def mount(ui: UI, selector: String)(using Frame): Unit < (Async & Scope) =
        mount(ui, selector, NoMountDiagnostics)

    private[kyo] def mount(ui: UI, selector: String, diagnostics: MountDiagnostics)(using Frame): Unit < (Async & Scope) =
        Sync.defer {
            val target = document.querySelector(selector)
            if target == null then Abort.panic(UIException(s"Element not found: $selector"))
            else mountInto(ui, target.asInstanceOf[dom.Element], diagnostics)
        }
    end mount

    /** Injects a rendered stylesheet CSS string into the live document.
      *
      * The base reset is injected first (idempotently) so it precedes the authored CSS in document
      * order, matching the SSG page head where `baseCss` is emitted before `head.css`. The reset is a
      * foundational layer authored stylesheets are meant to override (e.g. `body { font-family }`); if
      * it were appended AFTER the sheet (as happens when an app calls `runStylesheet` before `runMount`,
      * which injects the reset), its equal-specificity `body` rule would win on document order and clobber
      * the app's own `body` font, producing a fallback-font flash. Injecting the reset first here makes the
      * cascade order independent of which entry point runs first.
      */
    private[kyo] def injectStylesheet(sheet: Stylesheet)(using Frame): Unit < Sync =
        DomStyleSheet.injectBase().andThen(Sync.defer(DomStyleSheet.injectStylesheet(sheet.render)))

    /** The elements in the document whose `onAttached` hook runs, each with the scope it lives in and the element value it started for.
      *
      * Attached means in the document, so this is decided here and nowhere else: after every patch the page took, an element carrying
      * the marker that is not tracked yet starts its hook, and a tracked one that is no longer connected closes its scope. Content a
      * boundary holds back never reaches the page and is never seen. A node a morph kept that now shows a rebuilt element value closes
      * the old scope and starts the hook again, so whether the morph happened to keep the node never decides what runs.
      *
      * Both go through the session's event drain, the attach as an [[UIEvent.Attach]] dispatched to the element's path, so the hook
      * meets what a handler there meets; one drain also keeps an element's close after its start.
      */
    final private class Attachments(container: dom.Element):
        private val live = new js.WrappedMap(js.Map.empty[dom.Element, (Scope.Finalizer, String)])
        // Set once the session's dispatch and drain exist; a patch before that (the initial subscription's) is swept again then.
        var dispatch: Maybe[(Seq[String], UIEvent) => Boolean < Async] = Absent
        var events: Maybe[Channel[Unit < Async]]                       = Absent

        def sweep()(using Frame): Unit =
            if saw(Marker.Attach) && dispatch.nonEmpty && events.nonEmpty then
                val handle = dispatch.get
                val queue  = events.get
                val gone   = js.Array[dom.Element]()
                live.keysIterator.foreach(el => if !el.isConnected then discard(gone.push(el)))
                gone.foreach(close(queue, _))
                val found = document.querySelectorAll("[data-kyo-attach]")
                var i     = 0
                while i < found.length do
                    val el    = found(i).asInstanceOf[dom.Element]
                    val value = el.getAttribute("data-kyo-attach")
                    val known = live.get(el).map(_._2)
                    // A leave ghost is a copy of an element that has already gone; it is not that element arriving. An element
                    // another mount on the page shows is that mount's to start.
                    if !known.contains(value) && el.closest("[data-kyo-ghost]") == null && owns(el) then
                        if known.nonEmpty then close(queue, el)
                        // Unsafe: a sweep runs synchronously inside a patch, outside any kyo computation; the scope is a
                        // fresh allocation that only this entry and the hook it starts ever hold.
                        import AllowUnsafe.embrace.danger
                        val scope = Sync.Unsafe.evalOrThrow(Scope.Finalizer.init(1))
                        live.update(el, (scope, value))
                        val path = parsePath(el.getAttribute("data-kyo-path"))
                        // The element may have left, or shown a rebuilt value, by the time the drain gets here; it then starts
                        // nothing, and the close queued behind this releases the scope.
                        fireFromJs(
                            queue,
                            Sync.defer(el.isConnected && live.get(el).exists(_._1 eq scope)).map { still =>
                                if still then
                                    AttachSite.local.let(Present(AttachSite.Site(el, scope)))(handle(path, UIEvent.Attach(path))).unit
                                else Kyo.unit
                            }
                        )
                    end if
                    i += 1
                end while
            end if
        end sweep

        /** Whether this session shows `el`: inside its container, or inside a portal it moved to the body, whose slot is in the
          * container. Paths are per session, so another mount's element dispatched here would reach the wrong handler.
          */
        private def owns(el: dom.Element): Boolean =
            container.contains(el) || {
                val portal = el.closest("[data-kyo-portal]")
                portal != null && (portal.parentNode eq document.body) && {
                    val slot = document.querySelector(attrSelector("data-kyo-portal-slot", portal.getAttribute("data-kyo-path")))
                    slot != null && container.contains(slot)
                }
            }

        private def close(queue: Channel[Unit < Async], el: dom.Element)(using Frame): Unit =
            val scope = live(el)._1
            live -= el
            fireFromJs(queue, scope.close(Absent).andThen(scope.await))
        end close

        /** Closes every scope still open: the session is ending, and so is every element it showed. */
        def closeAll(using Frame): Unit < Async =
            val scopes = js.Array[Scope.Finalizer]()
            live.valuesIterator.foreach(entry => discard(scopes.push(entry._1)))
            live.clear()
            Kyo.foreachDiscard(scopes.toSeq)(scope => scope.close(Absent).andThen(scope.await))
        end closeAll
    end Attachments

    private def mountInto(ui: UI, container: dom.Element, diagnostics: MountDiagnostics)(using Frame): Unit < (Async & Scope) =
        val session     = new LocalSession
        val attachments = new Attachments(container)
        val listeners   = new BodyListeners
        for
            _        <- Sync.defer(discard(morphScript))
            _        <- DomStyleSheet.injectBase()
            commands <- UI.Commands.init(op => applyOpLocal(op, session))
            _ = session.commands = commands
            // Installed so component handlers and mounted effects reach `UI.commands` and `UI.scrollIntoView`, as the server session's
            // are: the first paint draws mounts, and the region fibers subscribe forks and the event drain all start inside it.
            _ <- UICommands.commands.let(Present(commands))(UICommands.scrollSink.let(Present(scrollLocal)) {
                for
                    root <- ReactiveUI.normalize(ui, Seq.empty)
                    // Drawn into the table the subscription below reads, so an instance takes the content this paint shows.
                    html <- MountDispatch.live.let(root.mountDispatch)(HtmlRenderer.render(ui, Seq.empty))
                    // The first paint is on the page before the session behind it runs, and the browser hands over the reader's events in
                    // between. So the queue, its drain and the listeners come before the paint, and what they hear waits in the queue until
                    // the subscription lets the drain go, as the server-push client's `__q` keeps what is raised before its session is live.
                    // Every listener on the page offers here, on the one JS thread; the drain is the only consumer.
                    events <- Channel.init[Unit < Async](256, Access.MultiProducerSingleConsumer)
                    _ = session.events = Present(events)
                    subscribed <- Promise.init[(Seq[String], UIEvent) => Boolean < Async, Any]
                    handle = (path: Seq[String], event: UIEvent) => subscribed.get.map(h => h(path, event))
                    // Single-consumer drain owned by the ambient page Scope. The single consumer preserves event ordering.
                    // runPartial captures only the Closed failure (the channel closed on page teardown -> stop draining); a
                    // Panic propagates rather than being silently swallowed as a clean drain end.
                    drain <- Fiber.initUnscoped(DragCommands.resolveSink.let(Present(resolveLocal)) {
                        subscribed.get.andThen(Sync.defer(diagnostics.drainStarted())).andThen(
                            Loop.foreach(Abort.runPartial[Closed](events.take).map {
                                case Result.Success(eff) => eff.andThen(Loop.continue)
                                case Result.Failure(_)   => Loop.done
                            })
                        )
                    })
                    // Removes callbacks before closing their queue and finally interrupts and joins the drain. Registered twice: here for
                    // a mount that fails on its way up, and again after the subscription, since finalizers are LIFO and the events have to
                    // stop before the exchange closes. Only the first one to run does anything.
                    ended <- AtomicBoolean.init(false)
                    endEvents = ended.getAndSet(true).map { done =>
                        if done then Kyo.unit
                        else
                            Sync.defer(listeners.removeAll())
                                .andThen(events.close.unit)
                                .andThen(Sync.defer(diagnostics.channelClosed()))
                                .andThen(Sync.defer(diagnostics.drainInterrupting()))
                                .andThen(drain.interrupt)
                                .andThen(drain.getResult)
                                .andThen(Sync.defer(diagnostics.drainJoined()))
                    }
                    _ <- Scope.ensure(endEvents)
                    _ <- Sync.defer(diagnostics.drainInstalled(drain))
                    _ <- Sync.defer {
                        setupEventDelegation(handle, events, listeners)
                        setupPointerDelegation(handle, events, listeners)
                        setupInputMasking(listeners)
                    }
                    _       <- Sync.defer { noteMarkers(html); container.innerHTML = html }
                    regions <- DomReactiveRegions.init(container)
                    _       <- Sync.defer(diagnostics.regionsOpened(regions))
                    _       <- applyJsProps(container)
                    _       <- Sync.defer(seedEnter(container, Set.empty))
                    _       <- Sync.defer(seedFocusAuto(container, Set.empty))
                    // Record the initial carriers without scrolling: the first paint is where the reader starts, not a
                    // movement to follow.
                    _ <- Sync.defer(sweepScrollAuto(scroll = false))
                    _ <- Sync.defer(sweepFilled())
                    // Portal adopt for the initial paint: a portal element present at load re-homes immediately.
                    _        <- Sync.defer { adoptPortals(container); retireOrphanTwins() }
                    _        <- Sync.defer(beginAnimationsSync(container))
                    exchange <- Kyo.lift(LocalExchange(regions, diagnostics, attachments))
                    dispatch <- ReactiveUI.subscribe(root, exchange)
                    // A subscription interrupt is asynchronous. Stop accepting publications before the Scope begins
                    // interrupting region fibers, so an already-woken observer cannot publish into the registry after its
                    // close finalizer has run.
                    _ <- Scope.ensure(exchange.close)
                    _ <- Scope.ensure(endEvents)
                    // Hooks start through the drain, so they can start only now; what the first paint and the initial
                    // subscription put on the page is found by this first sweep. Registered after the drain, the close runs
                    // before it is interrupted, so every hook ends inside a live session.
                    _ <- Scope.ensure(attachments.closeAll)
                    _ <- Sync.defer {
                        attachments.dispatch = Present(dispatch.handle)
                        attachments.events = Present(events)
                        attachments.sweep()
                    }
                    // Lets the drain go from a fiber of its own, so it runs behind the regions' first passes the subscription started. One
                    // of those may rebuild an element the sweep above found; the hook it queued then starts on the new value, and only there.
                    _           <- Fiber.initUnscoped(subscribed.completeDiscard(Result.succeed(dispatch.handle)))
                    dragRuntime <- DomDragRuntime.install(
                        container,
                        event =>
                            diagnostics.dragEventQueued(event)
                            fireFromJs(
                                events,
                                dispatch.handle(event.path, event).map { result =>
                                    diagnostics.dragEventHandled(event)
                                    result
                                }.unit
                            )
                    )
                    _ <- Sync.defer(diagnostics.dragRuntimeInstalled(dragRuntime))
                    _ <- Async.never
                yield ()
            })
        yield ()
        end for
    end mountInto

    // The local-document scroll sink, the browser-mount counterpart of the server session's WebSocket op; mirrors the embedded
    // client's ScrollIntoView handling exactly (missing id = no-op, smooth scroll to the block start),
    // so the same command behaves the same under either runner.
    private def scrollLocal(id: String)(using Frame): Unit < Async =
        Sync.defer {
            val el = document.getElementById(id)
            if el != null then
                discard(el.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal(behavior = "smooth", block = "start")))
        }

    /** Publishes a typed drag resolution for the local drag runtime without retaining listeners or
      * session state. DomDragRuntime consumes this reserved document event.
      */
    private def resolveLocal(sessionId: String, decision: Drag.Decision)(using Frame): Unit < Async =
        Sync.defer {
            val detail  = Json.encode[HtmlOp](HtmlOp.ResolveDrag(sessionId, decision))
            val publish = () =>
                val event = js.Dynamic.newInstance(dom.window.asInstanceOf[js.Dynamic].CustomEvent)(
                    "kyo:resolve-drag",
                    js.Dynamic.literal(detail = detail)
                )
                discard(document.asInstanceOf[js.Dynamic].dispatchEvent(event))
            discard(dom.window.setTimeout(publish, 0))
        }

    final private case class RangePatchState(
        active: Maybe[RangeFocus],
        selectionStart: Maybe[Int],
        selectionEnd: Maybe[Int],
        oldEnter: Set[String],
        ghosts: Seq[(dom.Element, dom.Element, String)],
        oldFocusAuto: Set[String]
    )

    final private case class RangeFocus(path: Maybe[String], rootIndex: Int, childIndexes: Seq[Int])

    // ---- local op application for the SPA transport (CommandById / RequestMeasureById) ----

    /** `[data-kyo-path="..."]` for a dot-joined render path, with the two characters a quoted CSS attribute
      * selector cannot carry raw escaped. Twin of `__kyoPathSel` in `HtmlRenderer.clientJs`.
      */
    private def pathSelector(joinedPath: String): String = attrSelector("data-kyo-path", joinedPath)

    /** `[name="value"]` with the value escaped, so a path whose row key holds a quote or a backslash still selects. */
    private def attrSelector(name: String, value: String): String =
        val escaped = value.replace("\\", "\\\\").replace("\"", "\\\"")
        s"[$name=\"$escaped\"]"

    private def queryByPath(path: Seq[String]): dom.Element =
        document.querySelector(pathSelector(path.mkString(".")))

    /** A conservative "focusable" CSS selector: what a focus command may land on. Mirrors
      * the reactive-focus-restore query used elsewhere in this backend / HtmlRenderer.
      */
    private val FocusableSelector = "input,textarea,select,button,a[href],[tabindex],[contenteditable]"

    /** Focus `el` if it is itself focusable, else its FIRST focusable descendant. Lets a
      * focus command target a non-focusable WRAPPER (e.g. an InputGroup around several
      * fields) and land on the first field inside it. A focusable element (an `<input>`,
      * …) matches the selector and focuses itself, so existing focus targets are unchanged.
      */
    private def focusInto(el: dom.Element): Unit =
        if el != null then
            val dyn         = el.asInstanceOf[scalajs.js.Dynamic]
            val selfMatches = scalajs.js.typeOf(dyn.matches) == "function" && dyn.matches(FocusableSelector).asInstanceOf[Boolean]
            val target      = if selfMatches then el else el.querySelector(FocusableSelector)
            if target != null then
                val tdyn = target.asInstanceOf[scalajs.js.Dynamic]
                if scalajs.js.typeOf(tdyn.focus) == "function" then discard(tdyn.focus())

    /** Apply a whitelisted `verb` to `el`. Unknown verbs are ignored. */
    private def applyVerbDom(el: dom.Element, verb: String): Unit =
        if el != null then
            val dyn = el.asInstanceOf[scalajs.js.Dynamic]
            verb match
                case "focus"          => focusInto(el)
                case "scrollIntoView" =>
                    if scalajs.js.typeOf(dyn.scrollIntoView) == "function" then
                        discard(dyn.scrollIntoView(scalajs.js.Dynamic.literal(block = "nearest")))
                case _ => ()
            end match
        end if
    end applyVerbDom

    /** Resolve the command target by DOM id (getElementById). */
    private def applyCommandDomById(id: String, verb: String): Unit =
        applyVerbDom(document.getElementById(id), verb)

    private def measureRect(el: dom.Element): Maybe[UI.Rect] =
        if el == null then Absent
        else
            val r = el.getBoundingClientRect()
            Present(UI.Rect(r.left, r.top, r.width, r.height, dom.window.innerWidth, dom.window.innerHeight))

    /** Measure the element with DOM id `id` (getElementById). */
    private def measureDomById(id: String): Maybe[UI.Rect] =
        measureRect(document.getElementById(id))

    private def applyOpLocal(op: HtmlOp, session: LocalSession)(using Frame): Unit < Async =
        op match
            case HtmlOp.CommandById(id, verb)  => Sync.defer(applyCommandDomById(id, verb))
            case HtmlOp.RequestMeasureById(id) =>
                Sync.defer(measureDomById(id)).map {
                    case Present(rect) => session.commands.deliverMeasureById(id, rect)
                    case Absent        => session.commands.deliverMeasureMissing(id)
                }
            case HtmlOp.SetClassById(id, className, on) =>
                Sync.defer {
                    val el = document.getElementById(id)
                    if el != null then
                        markOwned(el, "class")
                        discard(el.classList.toggle(className, on))
                }
            case HtmlOp.SetStyleById(id, css) =>
                Sync.defer {
                    val el = document.getElementById(id)
                    if el != null then
                        markOwned(el, "style")
                        mergeStyleDomById(id, css)
                }
            // set an attribute in place (element stays in the DOM, so a CSS `>` anchored on it keeps matching).
            case HtmlOp.SetAttrById(id, name, value) =>
                Sync.defer {
                    val el = document.getElementById(id)
                    if el != null then
                        markOwned(el, name)
                        el.setAttribute(name, value)
                        syncFieldProperty(el, name, value)
                    end if
                }
            // measure now + deliver, then attach the continuous scroll/resize observer for `id`.
            case HtmlOp.ObserveViewportById(id) =>
                Sync.defer(registerViewportObserver(id, session)).andThen(
                    Sync.defer(measureDomById(id)).map {
                        case Present(rect) => session.commands.deliverMeasureById(id, rect)
                        case Absent        => Kyo.unit
                    }
                )
            case HtmlOp.UnobserveViewportById(id) =>
                Sync.defer(unregisterViewportObserver(id, session))
            // Replace/Remove/InjectCss reach the DOM through LocalExchange, never this imperative channel.
            case _ => Kyo.unit
    end applyOpLocal

    /** Merges a serialized `Style` declaration string ("prop:val;prop:val") onto getElementById(id) with setProperty
      * per declaration, so it merges over other inline props rather than clobbering them (unlike a full `style=""`
      * replace). Blank declarations and those without a `:` are skipped.
      */
    private def mergeStyleDomById(id: String, css: String): Unit =
        val el = document.getElementById(id)
        if el != null then
            val style = el.asInstanceOf[dom.HTMLElement].style
            css.split(';').foreach { decl =>
                val trimmed = decl.trim
                if trimmed.nonEmpty then
                    val colon = trimmed.indexOf(':')
                    if colon > 0 then
                        style.setProperty(trimmed.substring(0, colon).trim, trimmed.substring(colon + 1).trim)
                end if
            }
        end if
    end mergeStyleDomById

    /** Attaches a single handler to `window` scroll (capture) + resize that re-measures getElementById(id) and
      * bridges the deliver back into the drain via [[fireFromJs]]. Guards against double-registration for the same id.
      */
    private def registerViewportObserver(id: String, session: LocalSession)(using Frame): Unit =
        if !session.viewportObservers.contains(id) then
            val handler: js.Function1[dom.Event, Unit] = (_: dom.Event) =>
                measureDomById(id) match
                    case Present(rect) => session.events.foreach(ev => fireFromJs(ev, session.commands.deliverMeasureById(id, rect)))
                    case Absent        => ()
            session.viewportObservers(id) = handler
            dom.window.addEventListener("scroll", handler, true)
            dom.window.addEventListener("resize", handler)
        end if
    end registerViewportObserver

    /** Removes the scroll/resize handler registered for `id` (from both listeners) and drops the map entry. */
    private def unregisterViewportObserver(id: String, session: LocalSession): Unit =
        if session.viewportObservers.contains(id) then
            val handler = session.viewportObservers(id)
            dom.window.removeEventListener("scroll", handler, true)
            dom.window.removeEventListener("resize", handler)
            discard(session.viewportObservers.remove(id))
        end if
    end unregisterViewportObserver

    /** Exchange that renders UI to HTML and applies directly to the DOM. */
    private class LocalExchange(regions: DomReactiveRegions, diagnostics: MountDiagnostics, attachments: Attachments)
        extends UIExchange:

        private var open = true

        // A range that waits when the mount ends gets no release from its boundary any more, so the marks go here.
        private[kyo] def close(using Frame): Unit < Sync = regions.releaseHolds.andThen(Sync.defer { open = false })

        override def onHold(region: ReactiveRegion, held: Boolean)(using Frame): Unit < Async =
            Sync.defer(open).flatMap { isOpen =>
                if !isOpen then Kyo.unit
                else
                    region match
                        case ReactiveRegion.HtmlRange(regionId) => regions.hold(regionId, held)
                        case _: ReactiveRegion.SvgElement       => Kyo.unit
            }

        // An SVG region is replaced whole on every paint, so it keeps no slot from one paint to the next.
        override def onFreshInstance(region: ReactiveRegion)(using Frame): Unit < Async =
            Sync.defer(open).flatMap { isOpen =>
                if !isOpen then Kyo.unit
                else
                    region match
                        case ReactiveRegion.HtmlRange(regionId) => regions.releaseSlots(regionId)
                        case _: ReactiveRegion.SvgElement       => Kyo.unit
            }

        /** Fingerprint of the HTML each region painted last, kept only while a devtools sink is observing.
          *
          * The server transport learns "this render changed nothing" for free, because it compares rendered
          * bytes against what it last sent in order to decide what to put on the socket. This one has no such
          * comparison to borrow: it renders the region and morphs, and a morph that finds everything equal is
          * indistinguishable from the outside. So it is asked here, on the string the paint already produced.
          *
          * A fingerprint, not the HTML: keeping the painted bytes of the whole tree alive would double the
          * page's memory to answer one boolean. Length in the high word, hash in the low, which makes a
          * collision require both to agree, and the cost of one is a render reported as wasted that was not.
          *
          * A region's first value (`previous` absent) is never compared: an earlier region may have been torn down
          * at the same place, and a fresh region's paint is not a repeat of it. A later value is compared with the
          * fingerprint of the region's last observed paint, and where there is none, because the last paint was the
          * enclosing render's or a list patch changed the range since, `previous` is rendered once to get one. An HTML
          * range keeps its fingerprint in the range registry, which drops it with the range; an SVG region keeps it on
          * its element, which a repaint replaces.
          *
          * Answers the fingerprint of `html` for the caller to record once the paint landed, `Absent` with no sink.
          */
        private def noteWasted(previous: Maybe[UI], html: String, known: => Maybe[Long])(render: UI => String < Sync)(using
            Frame
        ): Maybe[Long] < Sync =
            def fingerprint(of: String): Long = (of.length.toLong << 32) | (of.hashCode() & 0xffffffffL)
            Devtools.observing.map { observing =>
                if !observing then Maybe.empty[Long]
                else
                    val last: Maybe[Long] < Sync = previous match
                        case Absent      => Absent
                        case Present(ui) =>
                            Sync.defer(known).map {
                                case Absent => render(ui).map(prior => Present(fingerprint(prior)))
                                case found  => found
                            }
                    last.map { last =>
                        val current = fingerprint(html)
                        Devtools.notePaint(html.length, last.contains(current)).andThen(Present(current))
                    }
            }
        end noteWasted

        private def lastFingerprint(region: ReactiveRegion): Maybe[Long] =
            region match
                case ReactiveRegion.HtmlRange(regionId)   => regions.fingerprint(regionId)
                case svgRegion: ReactiveRegion.SvgElement =>
                    Maybe(document.querySelector(pathSelector(svgRegion.path.mkString(".")))).flatMap { element =>
                        val stored = element.asInstanceOf[scalajs.js.Dynamic].__kyoFingerprint
                        if scalajs.js.isUndefined(stored) then Absent else Present(stored.asInstanceOf[String].toLong)
                    }

        // In-process: the update is a DOM write, not bytes on a wire, so this replaces the region whole
        // (morphing where it can) rather than diffing it. `previous` is what the server-side exchange
        // uses to send only what moved; here an absent `previous` marks the region's first value, whose
        // markup the enclosing render already wrote, so the range is left as it is when that markup is unchanged.
        def onChange(
            region: ReactiveRegion,
            path: Seq[String],
            contentContext: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            ui: UI
        )(using Frame): Unit < Async =
            Sync.defer(open).flatMap { isOpen =>
                if !isOpen then Kyo.unit
                else
                    val boundaryMode =
                        if ReactiveRegion.owns(region, contentContext) then ReactiveRegion.BoundaryMode.Suppress
                        else ReactiveRegion.BoundaryMode.Emit
                    HtmlRenderer.renderRegion(ui, path, contentContext, region, parentContext, boundaryMode).flatMap { html =>
                        // Every painted byte passes here, so this is where the optional-feature flags learn about
                        // an attribute that only a later re-render brings in (see noteMarkers), and where a
                        // devtools sink learns whether this render produced anything (see noteWasted).
                        noteMarkers(html)
                        noteWasted(previous, html, lastFingerprint(region))(ui =>
                            // Rendered for its fingerprint only: nothing it draws goes on the page.
                            MountDispatch.live.let(Absent)(
                                HtmlRenderer.renderRegion(ui, path, contentContext, region, parentContext, boundaryMode)
                            )
                        ).map(noted => (noted, open)).flatMap { (noted, stillOpen) =>
                            if !stillOpen then Kyo.unit
                            else
                                val applied = region match
                                    case ReactiveRegion.HtmlRange(regionId) =>
                                        // A region that is not painted right now has nothing to patch. It is the
                                        // sibling of the `open` guard above, and it is reachable for the same kind
                                        // of reason: a patch somewhere above discarded this subtree, and whatever
                                        // owns it has not repainted yet. The emission that lands in that window is
                                        // either obsolete (the next paint renders the signal's CURRENT value, so
                                        // nothing is lost) or early (same). Neither is worth the registry's
                                        // strict diagnostic, which panics, and a panic here does not just skip a
                                        // frame, it ends the subscription fiber, so the region never paints again.
                                        // The registry keeps that diagnostic for callers who really are addressing
                                        // a range that ought to exist; the engine simply does not ask when it knows
                                        // there is none. Same reading `withRegionFragment` already applies to a
                                        // range that was never painted.
                                        regions.contains(regionId).flatMap { painted =>
                                            if !painted then Kyo.unit
                                            else
                                                regions.replaceWith(regionId, html, keepCurrent = previous.isEmpty)(tryMorphRange)(
                                                    prepareRangePatch
                                                )(finishRangePatch)
                                                    .andThen(Sync.defer(noted.foreach(regions.recordFingerprint(regionId, _))))
                                        }
                                    case svgRegion: ReactiveRegion.SvgElement =>
                                        replaceSvg(svgRegion, html).map(shown =>
                                            for fingerprint <- noted; element <- shown do
                                                element.asInstanceOf[scalajs.js.Dynamic].__kyoFingerprint = fingerprint.toString
                                        )
                                applied.andThen(Sync.defer {
                                    attachments.sweep()
                                    diagnostics.regionApplied()
                                })
                        }
                    }
            }
        end onChange

        /** Render only the rows that changed, and as one payload rather than one parse per row: a full replacement
          * must not lose the single bulk parse it has today just because a one-row removal wants to skip it.
          *
          * Rows that turn out not to be addressable one-to-one leave the DOM untouched and fall through to the
          * inherited whole-list repaint, which needs no per-row structure at all.
          */
        override def onListPatch(
            region: ReactiveRegion,
            path: Seq[String],
            contentContext: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            rows: Seq[ListRow]
        )(using Frame): Boolean < Async =
            def repaint: Boolean < Async = super.onListPatch(region, path, contentContext, parentContext, previous, rows)
            region match
                case ReactiveRegion.HtmlRange(regionId) =>
                    val host = ReactiveRegion.renderHost(
                        region,
                        parentContext,
                        ReactiveRegion.tableContent(rows.iterator.map(_.ui))
                    )
                    HtmlRenderer.renderRows(rows, path, contentContext, host).flatMap { changed =>
                        noteMarkers(changed)
                        Sync.defer(open).flatMap { isOpen =>
                            // A closed backend paints nothing, so there is nothing to subscribe again.
                            if !isOpen then true
                            else
                                regions.withRegionFragment(regionId, changed)(listPatch(_, _, path, rows)).flatMap {
                                    applied =>
                                        if applied then
                                            Sync.defer { regions.forgetFingerprint(regionId); attachments.sweep() }
                                                .andThen(Devtools.notePaint(changed.length, wasted = false))
                                                .andThen(true)
                                        else repaint
                                }
                        }
                    }
                // An SVG region replaces its own group element; there is no row range to address.
                case _: ReactiveRegion.SvgElement => repaint
            end match
        end onListPatch

        // Each channel patches in place. Unlike the id-addressed channels it does NOT mark the attribute owned: the
        // render writes a path channel's value into the HTML, so a morph that repaints the element carries the value
        // the channel stands for, and the observer the repaint starts is seeded with that value. Owning it instead
        // kept the morph from ever reconciling the attribute again, so an element repainted with a different value
        // (a list row whose item changed) kept the one the first patch wrote. The `*Now` forms are the patches
        // themselves, written without the Sync wrapper for a caller that already holds the thread (see UIExchange's
        // synchronous sinks); the effectful twins do nothing but defer to them, so the two can never drift apart.
        private def attrPatchNow(path: Seq[String], name: String, value: String): Unit =
            val el = queryByPath(path)
            if el != null then
                el.setAttribute(name, value)
                syncFieldProperty(el, name, value)
            end if
        end attrPatchNow

        private def boolAttrPatchNow(path: Seq[String], name: String, value: Boolean): Unit =
            val el = queryByPath(path)
            if el != null then
                if value then el.setAttribute(name, "") else el.removeAttribute(name)
                if name == "checked" && el.tagName == "INPUT" then syncChecked(el, value)
        end boolAttrPatchNow

        // Toggle in place (so CSS transitions fire) rather than re-render.
        private def classPatchNow(path: Seq[String], name: String, on: Boolean): Unit =
            val el = queryByPath(path)
            if el != null then
                discard(el.classList.toggle(name, on))
        end classPatchNow

        // The inline-style channel: drop what the previous value had and this one lacks, then set this one's declarations,
        // property by property, so the element's other inline properties stay as they are.
        private def stylePatchNow(path: Seq[String], set: Seq[(String, String)], removed: Seq[String]): Unit =
            val el = queryByPath(path)
            if el != null then
                val style = el.asInstanceOf[dom.HTMLElement].style
                removed.foreach(n => discard(style.removeProperty(n)))
                set.foreach((n, v) => style.setProperty(n, v))
            end if
        end stylePatchNow

        override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
            Sync.defer(attrPatchNow(path, name, value))

        override def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async =
            Sync.defer(boolAttrPatchNow(path, name, value))

        override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
            Sync.defer(classPatchNow(path, name, on))

        override def onStylePatch(path: Seq[String], set: Seq[(String, String)], removed: Seq[String])(using
            Frame
        ): Unit < Async =
            Sync.defer(stylePatchNow(path, set, removed))

        // A bound text region: the value goes into the text node as-is. Unlike patchLoneText, which receives
        // rendered html and must therefore refuse payloads carrying `<` or `&`, this receives the string itself,
        // and a text node holds literal characters, so every string is writable here with no escaping and no
        // parser. An unpainted path is a silent no-op, exactly as for the attribute patches above.
        private def textPatchNow(path: Seq[String], value: String): Unit =
            discard(regions.setTextAt(path, value))

        override val attrPatcherNow: Maybe[(Seq[String], String, String) => Unit]                      = Present(attrPatchNow)
        override val boolAttrPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit]                 = Present(boolAttrPatchNow)
        override val classPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit]                    = Present(classPatchNow)
        override val stylePatcherNow: Maybe[(Seq[String], Seq[(String, String)], Seq[String]) => Unit] = Present(stylePatchNow)
        override val textPatcherNow: Maybe[(Seq[String], String) => Unit]                              = Present(textPatchNow)

        private def prepareRangePatch(oldElements: Seq[dom.Element], newElements: Seq[dom.Element]): RangePatchState =
            val active = Maybe(document.activeElement).filter(el => (el ne document.body) && containsAny(oldElements, el))
            val (selectionStart, selectionEnd) = active.map(readSelection).getOrElse((Absent, Absent))
            RangePatchState(
                active.flatMap(focusLocator(oldElements, _)),
                selectionStart,
                selectionEnd,
                enterPaths(oldElements),
                prepareLeaveGhosts(oldElements, leavePaths(newElements)),
                focusAutoPaths(oldElements)
            )
        end prepareRangePatch

        private def finishRangePatch(state: RangePatchState, newElements: Seq[dom.Element], morphed: Boolean): Unit =
            newElements.foreach { element =>
                applyJsPropsSync(element)
                // A replacement inserted every one of these roots, so every animation under them is new. A morph
                // reused what it could and started the animations of the nodes it did insert as it inserted them.
                if !morphed then beginAnimationsSync(element)
            }
            state.active.flatMap(resolveFocus(newElements, _)).foreach { target =>
                focusNoScroll(target)
                (state.selectionStart, state.selectionEnd) match
                    case (Present(start), Present(end)) => setSelection(target, start, end)
                    case _                              => ()
            }
            seedEnter(newElements, state.oldEnter)
            // Seed after focus restoration so a newly appeared focus-auto element wins over the trigger.
            seedFocusAuto(newElements, state.oldFocusAuto)
            // Portal upkeep: re-home portal elements this patch inserted, and retire the placeholder of one it
            // removed. Runs over the patched roots, so an untouched part of the page is not swept.
            newElements.foreach(adoptPortals)
            retireOrphanTwins()
            spawnGhosts(state.ghosts)
            sweepFocusAuto()
            // A patch that left the flag on a different element scrolls that element into view; one that left it
            // where it was scrolls nothing.
            sweepScrollAuto()
            sweepFilled()
        end finishRangePatch

        /** Replaces the SVG region's element with `html`, answering the element that shows it afterwards. */
        private def replaceSvg(region: ReactiveRegion.SvgElement, html: String)(using Frame): Maybe[dom.Element] < Sync =
            Sync.defer {
                val pathAttr  = region.path.mkString(".")
                val finalHtml = HtmlRenderer.wrapReactiveRegion(region, html)
                val element   = document.querySelector(pathSelector(pathAttr))
                if element == null then Absent
                else if element.outerHTML == finalHtml then Present(element)
                else
                    val active = Maybe(document.activeElement).filter(el => (el ne document.body) && element.contains(el))
                    val (selectionStart, selectionEnd) = active.map(readSelection).getOrElse((Absent, Absent))
                    val activePath                     = active.flatMap(el => Maybe(el.getAttribute("data-kyo-path")))
                    val oldEnter                       = enterPaths(Seq(element))
                    val ghosts                         = prepareLeaveGhosts(Seq(element), leaveSurvSet(finalHtml))
                    val oldFocus                       = focusAutoPaths(Seq(element))
                    // The element may hold HTML ranges (under a foreignObject), which the replacement removes and
                    // brings anew, so the registry follows it.
                    val updated = regions.rewrite(element) {
                        element.outerHTML = finalHtml
                        Maybe(document.querySelector(pathSelector(pathAttr)))
                    }.toList
                    updated.foreach { newElement =>
                        applyJsPropsSync(newElement)
                        beginAnimationsSync(newElement)
                    }
                    activePath.foreach(path => restoreSvgFocus(path, selectionStart, selectionEnd))
                    seedEnter(updated, oldEnter)
                    seedFocusAuto(updated, oldFocus)
                    spawnGhosts(ghosts)
                    sweepFocusAuto()
                    Maybe.fromOption(updated.headOption)
                end if
            }
        end replaceSvg
    end LocalExchange

    private def readSelection(el: dom.Element): (Maybe[Int], Maybe[Int]) =
        val dyn                                      = el.asInstanceOf[scalajs.js.Dynamic]
        def asInt(v: scalajs.js.Dynamic): Maybe[Int] =
            if scalajs.js.typeOf(v) == "number" then Present(v.asInstanceOf[Int]) else Absent
        (asInt(dyn.selectionStart), asInt(dyn.selectionEnd))
    end readSelection

    /** Focus the engine performs as bookkeeping (restoring focus after a patch, seeding `focusAuto`, returning focus from a closed
      * `focusRestore` element) passes `preventScroll`: the browser's default `focus()` scrolls every scrollable ancestor to reveal the
      * target, which would move containers the user never scrolled. Focus the app asks for, and keyboard navigation, keep the default.
      */
    private def focusNoScroll(el: dom.Element): Unit =
        discard(el.asInstanceOf[scalajs.js.Dynamic].focus(scalajs.js.Dynamic.literal(preventScroll = true)))

    private def restoreSvgFocus(capturedPath: String, selStart: Maybe[Int], selEnd: Maybe[Int]): Unit =
        val located = document.querySelector(pathSelector(capturedPath))
        if located != null then
            focusNoScroll(located)
            (selStart, selEnd) match
                case (Present(s), Present(e)) => setSelection(located, s, e)
                case _                        => ()
        end if
    end restoreSvgFocus

    /** The set of `data-kyo-path` values of every `data-kyo-focus-auto` element inside `root`, `root` itself included.
      *
      * Callers capture this BEFORE replacing a region so that [[seedFocusAuto]] can tell a newly appeared element from
      * one that was already on screen: an echo re-render of an open overlay must not steal focus back from the user.
      */
    private def focusAutoPaths(root: dom.Element): Set[String] =
        if !saw(Marker.FocusAuto) then return Set.empty
        val els         = root.querySelectorAll("[data-kyo-focus-auto]")
        val descendants = (0 until els.length).flatMap { i =>
            Maybe(els(i).asInstanceOf[dom.Element].getAttribute("data-kyo-path")).toList
        }.toSet
        if root.hasAttribute("data-kyo-focus-auto") && root.hasAttribute("data-kyo-path") then
            descendants + root.getAttribute("data-kyo-path")
        else descendants
    end focusAutoPaths

    private def focusAutoPaths(roots: Seq[dom.Element]): Set[String] =
        roots.iterator.flatMap(focusAutoPaths).toSet

    /** Seed the FIRST `data-kyo-focus-auto` element under `newRoot` whose path is not in `oldSet` (i.e. it newly
      * appeared): record the previously focused element's path plus the focus-restore flag on the stack, then call
      * `.focus()` on it. On the initial mount `oldSet` is empty, so any focus-auto element is seeded, like native
      * `autofocus`. Mirrors `seedFocusAuto` in HtmlRenderer.clientJs.
      */
    private def seedFocusAuto(newRoot: dom.Element, oldSet: Set[String]): Unit =
        seedFocusAuto(Seq(newRoot), oldSet)

    private def seedFocusAuto(newRoots: Seq[dom.Element], oldSet: Set[String]): Unit =
        // An app that never renders the attribute pays no selector sweep for it (see noteMarkers).
        if !saw(Marker.FocusAuto) then return
        val candidates = newRoots.flatMap { newRoot =>
            val els = newRoot.querySelectorAll("[data-kyo-focus-auto]")
            (if newRoot.hasAttribute("data-kyo-focus-auto") then Seq(newRoot) else Seq.empty) ++
                (0 until els.length).map(els(_).asInstanceOf[dom.Element])
        }
        candidates.find { el =>
            val p = el.getAttribute("data-kyo-path")
            p != null && !oldSet.contains(p)
        }.foreach { el =>
            val ae  = document.activeElement
            val ret =
                if ae != null && (ae ne document.body) then Maybe(ae.getAttribute("data-kyo-path"))
                else Absent
            focusReturnStack =
                focusReturnStack.append(
                    FocusSeed(el.getAttribute("data-kyo-path"), ret, el.hasAttribute("data-kyo-focus-restore"))
                )
            focusNoScroll(el)
        }
    end seedFocusAuto

    private def containsAny(roots: Seq[dom.Element], element: dom.Element): Boolean =
        roots.exists(root => (root eq element) || root.contains(element))

    private def focusLocator(roots: Seq[dom.Element], active: dom.Element): Maybe[RangeFocus] =
        Maybe.fromOption(roots.indexWhere(root => (root eq active) || root.contains(active)) match
            case -1    => None
            case index => Some(index)).map { rootIndex =>
            val indexes = scala.collection.mutable.ArrayBuffer.empty[Int]
            var current = active
            val root    = roots(rootIndex)
            while current ne root do
                val parent = current.parentNode.asInstanceOf[dom.Element]
                var index  = 0
                while index < parent.children.length && (parent.children(index) ne current) do index += 1
                indexes.prepend(index)
                current = parent
            end while
            RangeFocus(Maybe(active.getAttribute("data-kyo-path")), rootIndex, indexes.toSeq)
        }
    end focusLocator

    private def resolveFocus(roots: Seq[dom.Element], locator: RangeFocus): Maybe[dom.Element] =
        val byPath = locator.path.flatMap { path =>
            Maybe.fromOption(roots.iterator.flatMap { root =>
                val descendants = root.querySelectorAll("[data-kyo-path]")
                val candidates  =
                    Iterator.single(root) ++ (0 until descendants.length).iterator.map { i =>
                        descendants(i).asInstanceOf[dom.Element]
                    }
                candidates.find(_.getAttribute("data-kyo-path") == path)
            }.nextOption())
        }
        byPath.orElse {
            Maybe.fromOption(roots.lift(locator.rootIndex)).flatMap { root =>
                var current: dom.Element = root
                var valid                = true
                val indexes              = locator.childIndexes.iterator
                while valid && indexes.hasNext do
                    val index = indexes.next()
                    if index >= current.children.length then valid = false
                    else current = current.children(index).asInstanceOf[dom.Element]
                end while
                if valid then Present(current) else Absent
            }
        }
    end resolveFocus

    /** Unwind stack entries whose seeded focus-auto element left the document, returning focus at most once.
      *
      * Stops at the first entry whose element is still in the document: that seed is still on screen, and
      * restoring an entry below it would move focus out of it. Below that, exactly one restore may land: a
      * deeper entry belongs to a seed that closed while a newer one stayed open, so its return target is stale
      * and must not override the one just restored. Its entry is still dropped, so it cannot fire on a later
      * sweep either. Mirrors `sweepFocusAuto` in HtmlRenderer.clientJs.
      *
      * A restore only lands where the removal actually took the focus with it (see [[focusWasLost]]).
      */
    @tailrec
    private def sweepFocusAuto(restored: Boolean = false): Unit =
        focusReturnStack.lastMaybe match
            case Present(seed)
                if document.querySelector(pathSelector(seed.path) + "[data-kyo-focus-auto]") == null =>
                focusReturnStack = focusReturnStack.dropLeftAndRight(0, 1)
                val landed =
                    !restored && seed.restore && focusWasLost && seed.returnTo.exists(retPath => focusIfPresent(retPath))
                sweepFocusAuto(restored || landed)
            case _ => ()
    end sweepFocusAuto

    /** Paths of the elements that carried `data-kyo-scroll-auto` after the last patch.
      *
      * The flag moves (a highlight walking down a list), which is the difference from focus-auto's
      * per-region "newly appeared" test: both the row losing it and the row taking it were already on
      * screen, so only the carrier set tells them apart. Mirrors `__scrollAutoPaths` in HtmlRenderer.clientJs.
      */
    private var scrollAutoPaths: Set[String] = Set.empty

    /** Scrolls the element that newly carries `data-kyo-scroll-auto` into view, at most one per patch.
      *
      * `block: "nearest"` is the whole point: it moves the nearest scrollable ancestor by the least it can and
      * does nothing when the element is already visible, so a highlight walking within view never moves the
      * panel under the reader. A patch that leaves the flag where it was scrolls nothing.
      */
    private def sweepScrollAuto(scroll: Boolean = true): Unit =
        if !saw(Marker.ScrollAuto) then return
        val els   = document.querySelectorAll("[data-kyo-scroll-auto]")
        val list  = (0 until els.length).map(els(_).asInstanceOf[dom.Element])
        val now   = list.flatMap(el => Maybe(el.getAttribute("data-kyo-path")).toList).toSet
        val fresh = list.find { el =>
            val p = el.getAttribute("data-kyo-path")
            p != null && !scrollAutoPaths.contains(p)
        }
        scrollAutoPaths = now
        if scroll then
            fresh.foreach(el =>
                discard(el.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal(block = "nearest", inline = "nearest")))
            )
        else
            // The first paint leaves the document where the browser puts it: its position is the one restored on
            // reload and on back, and scrolling it at load would fight that. A scroll container of the page's own
            // has nothing restored into it, so a flag there (a list opened on its selected row, a rail opened on a
            // deep link) is brought into view, and `container: "nearest"` keeps the scroll inside that container.
            fresh.filter(inOwnScroller).foreach(el =>
                discard(el.asInstanceOf[js.Dynamic].scrollIntoView(
                    js.Dynamic.literal(block = "nearest", inline = "nearest", container = "nearest")
                ))
            )
        end if
    end sweepScrollAuto

    /** Whether something between `el` and the document scrolls on its own. Twin of `__kyoInOwnScroller` in
      * HtmlRenderer.clientJs.
      */
    private def inOwnScroller(el: dom.Element): Boolean =
        val body: AnyRef = document.body
        val root: AnyRef = document.documentElement
        // `parentElement` is declared on Node subtypes this facade does not give an Element, so it is read off the
        // object the way the rest of this file reads DOM members the facade lacks.
        def parentOf(e: dom.Element): dom.Element = e.asInstanceOf[js.Dynamic].parentElement.asInstanceOf[dom.Element]
        @tailrec
        def loop(p: dom.Element): Boolean =
            if (p eq null) || (p eq body) || (p eq root) then false
            else
                dom.window.getComputedStyle(p).overflowY match
                    case "auto" | "scroll" | "overlay" => true
                    case _                             => loop(parentOf(p))
        loop(parentOf(el))
    end inOwnScroller

    /** Whether the patch that removed the seeded element took the focus with it.
      *
      * Removing the focused element leaves `document.activeElement` on `body` (or null), which is the case a
      * restore exists for: the reader was inside the panel and would otherwise be dropped at the top of the
      * document. Any other element there means they moved focus themselves before the panel went, a Tab out of
      * a combobox being the everyday case, and putting it back would undo the move they just made. Twin of the
      * same check in HtmlRenderer.clientJs.
      */
    private def focusWasLost: Boolean =
        val active = document.activeElement
        active == null || (active eq document.body)

    /** Focus the element carrying `path`; `false` when it is no longer in the document (nothing focused). */
    private def focusIfPresent(path: String): Boolean =
        val el = document.querySelector(pathSelector(path))
        if el == null then false
        else
            focusNoScroll(el)
            true
        end if
    end focusIfPresent

    /** Moves the caret on `el`, tolerating the two documented ways that is a no-op.
      *
      * Elements outside input and textarea (select, contenteditable) have no `setSelectionRange` at all, and on
      * input types without a text selection (email, number, both of which kyo-ui offers as text inputs) it throws
      * `InvalidStateError`. In either case the value is already set and only the caret stays put. Any other
      * JavaScript exception is a real failure and propagates rather than being swallowed. Mirrored by
      * `kyoSetCaret` in `HtmlRenderer.clientJs`.
      */
    private def setSelection(el: dom.Element, start: Int, end: Int): Unit =
        val dyn = el.asInstanceOf[scalajs.js.Dynamic]
        if scalajs.js.typeOf(dyn.setSelectionRange) == "function" then
            try discard(dyn.setSelectionRange(start, end))
            catch
                case ex: scalajs.js.JavaScriptException
                    if ex.exception.asInstanceOf[scalajs.js.Dynamic].name.asInstanceOf[String] == "InvalidStateError" =>
                    ()
        end if
    end setSelection

    // Bridge a Kyo Async computation from a JS callback boundary by offering it to the page-scoped drain
    // channel. The single AllowUnsafe site narrows to the offer crossing (the JS callback has no Kyo
    // context); a drop on a closed channel is fine (the page is being torn down anyway).
    private def fireFromJs(events: Channel[Unit < Async], eff: Unit < Async)(using Frame): Unit =
        // Unsafe: JS event callbacks run outside any Kyo context; this is the one controlled crossing point.
        import AllowUnsafe.embrace.danger
        // runPartial drops only a Closed (offer on a torn-down channel); a Panic propagates to evalOrThrow and
        // surfaces (thrown at the boundary) rather than being swallowed by the discard.
        discard(Sync.Unsafe.evalOrThrow(Abort.runPartial[Closed](events.offer(eff)).unit))
    end fireFromJs

    /** Scan `root` and all descendants for `data-kyo-prop-*` attributes, apply each as a direct
      * DOM property on the element, then remove the data attribute so it does not linger.
      */
    private def applyJsProps(root: dom.Element)(using Frame): Unit < Sync =
        Sync.defer(applyJsPropsSync(root))

    private def applyJsPropsSync(root: dom.Element): Unit =
        if !saw(Marker.JsProp) then return
        // CSS has no attribute-name-prefix selector, so `[data-kyo-prop-*]` is not a valid selector and
        // throws SyntaxError. Visit the root and every descendant, reading property names directly from
        // the live attribute map.
        def applyElement(el: dom.Element): Unit =
            var i = el.attributes.length - 1
            while i >= 0 do
                val attrName = el.attributes(i).name
                if attrName.startsWith(JsPropPrefix) then
                    val propName = attrName.stripPrefix(JsPropPrefix)
                    val value    = el.getAttribute(attrName)
                    val d        = el.asInstanceOf[scalajs.js.Dynamic]
                    // Each assignment is kept on the element, for reassertEnclosingSelect to assign again, with the
                    // value the element had before the first one, for restoreJsProps to give back.
                    if js.isUndefined(d.__kyoProps) then d.__kyoProps = js.Dictionary.empty[js.Dynamic].asInstanceOf[js.Any]
                    val props = d.__kyoProps.asInstanceOf[js.Dictionary[js.Dynamic]]
                    val base  = props.get(propName).fold(d.selectDynamic(propName).asInstanceOf[js.Any])(_.base.asInstanceOf[js.Any])
                    props(propName) = js.Dynamic.literal(base = base, value = value)
                    d.updateDynamic(propName)(value)
                    el.removeAttribute(attrName)
                end if
                i -= 1
            end while
        end applyElement

        applyElement(root)
        val elements = root.querySelectorAll("*")
        var i        = 0
        while i < elements.length do
            applyElement(elements(i).asInstanceOf[dom.Element])
            i += 1
        end while
        reassertEnclosingSelect(root)
    end applyJsPropsSync

    /** The attribute prefix a rendered JS property travels under (see [[applyJsPropsSync]]). */
    private val JsPropPrefix = "data-kyo-prop-"

    /** Assign a select's bound value again after the options inside it were painted.
      *
      * A select chooses among the options it holds when its value is assigned, so a value assigned before its
      * options arrive (a list that loads later) selects nothing, and the options' paint never reaches the select.
      * Mirrors `__kyoReassertSelect` in HtmlRenderer.reactiveRangesJs.
      */
    private def reassertEnclosingSelect(root: dom.Element): Unit =
        val parent = root.parentNode
        if parent != null && parent.nodeType == dom.Node.ELEMENT_NODE then
            val select = parent.asInstanceOf[dom.Element].closest("select")
            if select != null then
                val d = select.asInstanceOf[js.Dynamic]
                if !js.isUndefined(d.__kyoProps) then
                    val props = d.__kyoProps.asInstanceOf[js.Dictionary[js.Dynamic]]
                    props.get("value").foreach { record =>
                        val value = record.value.asInstanceOf[String]
                        if d.value.asInstanceOf[String] != value then d.value = value
                    }
                end if
            end if
        end if
    end reassertEnclosingSelect

    /** Start every freshly-inserted SMIL animation under `root`.
      *
      * Chart transition `<animate>` elements use `begin="indefinite"` so they do not auto-play against the
      * shared SVG document timeline (which would make a post-load update snap to the frozen `to` value).
      * Calling `beginElement()` after the node is inserted starts the tween relative to now. The call is
      * deferred one animation frame so the SMIL engine has registered the newly inserted elements; a node
      * that was already replaced again by then throws and is ignored.
      */
    private def beginAnimationsSync(root: dom.Element): Unit =
        if !saw(Marker.SvgAnim) then return
        val anims = root.querySelectorAll("animate,animateTransform,animateMotion")
        if anims.length > 0 then
            discard(dom.window.requestAnimationFrame { (_: Double) =>
                var i = 0
                while i < anims.length do
                    try anims(i).asInstanceOf[scalajs.js.Dynamic].beginElement()
                    catch case _: Throwable => ()
                    i += 1
                end while
            })
        end if
    end beginAnimationsSync

    /** The next element up the LOGICAL tree. That is the DOM parent everywhere except at a portal: a re-homed
      * element sits directly under `document.body`, so the climb hops back to its placeholder slot first and
      * continues from there. The client does the same hop inline in `he` (HtmlRenderer.clientJs).
      */
    private def logicalAncestor(n: dom.Element): dom.Element =
        val hopped =
            if n.hasAttribute("data-kyo-portal") && (n.parentNode eq document.body) then
                val p = n.getAttribute("data-kyo-path")
                if p == null then null else document.querySelector(attrSelector("data-kyo-portal-slot", p))
            else n
        if hopped == null then null
        else
            hopped.parentNode match
                case p: dom.Element => p
                case _              => null
        end if
    end logicalAncestor

    /** True when `start` or any of its ancestors below `document.body` declares event type `t` in `data-kyo-ev`.
      *
      * EventDispatch.dispatchToElement bubbles an event to every ancestor that declared a handler for its type, so the
      * SPA forwarding gate must forward when ANY ancestor declared it, not just the target (checking only the
      * target's own data-kyo-ev would drop e.g. a keydown meant for an ancestor panel before bubble dispatch runs).
      *
      * The walk climbs the LOGICAL tree, which the portal makes distinct from the DOM one: see [[logicalAncestor]].
      * Twin of `he` in HtmlRenderer.clientJs; keep the two in lockstep.
      */
    private[kyo] def declaredInChain(start: dom.Element, t: String): Boolean =
        var n: dom.Element = start
        var found          = false
        while !found && n != null && (n ne document.body) do
            val ev = n.getAttribute("data-kyo-ev")
            if ev != null && ev.split(",").contains(t) then found = true
            else n = logicalAncestor(n)
        end while
        found
    end declaredInChain

    /** The listeners one mount has on the body. They go on before the first paint and come off before their queue closes, which is after
      * everything the mount does in between, so they are not added through `Scope`, whose finalizers run in the reverse of the order things
      * were added.
      */
    final private class BodyListeners:
        private var installed = List.empty[() => Unit]

        def add(eventType: String, listener: scalajs.js.Function1[dom.Event, Unit], capture: Boolean): Unit =
            val target = document.body
            target.addEventListener(eventType, listener, capture)
            installed = (() => target.removeEventListener(eventType, listener, capture)) :: installed
        end add

        def add(eventType: String, listener: scalajs.js.Function1[dom.Event, Unit], options: dom.EventListenerOptions): Unit =
            val target = document.body
            target.addEventListener(eventType, listener, options)
            installed = (() => target.removeEventListener(eventType, listener, options)) :: installed
        end add

        def removeAll(): Unit =
            val all = installed
            installed = Nil
            all.foreach(_())
        end removeAll
    end BodyListeners

    /** Set up capture-phase event delegation on document.body. */
    private def setupEventDelegation(
        dispatch: (Seq[String], UIEvent) => Boolean < Async,
        events: Channel[Unit < Async],
        listeners: BodyListeners
    )(using Frame): Unit =
        final class ChainTypes(target: dom.Element):
            def contains(t: String): Boolean = declaredInChain(target, t)

        // A submit button's click makes the browser fire a native `submit` right after, but the Click dispatch
        // already emulates onSubmit; this flag (set on Click, cleared on a 0-timeout) suppresses that one
        // following native submit so the form handler runs once. Mirrors clientJs's `_kyoClickSubmit` guard.
        var clickSubmitGuard = false

        val handler: scalajs.js.Function1[dom.Event, Unit] = (e: dom.Event) =>
            // Runs before path resolution so it fires even when the focused descendant is not a path element;
            // the keydown is still forwarded below, so the region's own onKeyDown runs (see `scrollKeyPrevented`).
            if e.`type` == "keydown" then
                val ke  = e.asInstanceOf[dom.KeyboardEvent]
                val tgt = e.target.asInstanceOf[dom.Element]
                // preventActivation: an inert region declines the browser default for the keys that change a
                // native control's value, so a readonly checkbox stays focusable rather than being disabled.
                // The keydown is still forwarded. Twin of the `data-kyo-inert` block in clientJs.
                if tgt != null && KeyPolicy.suppressesActivation(ke.key) && tgt.closest("[data-kyo-inert]") != null then
                    e.preventDefault()
                if tgt != null && radioArrow(ke.key, tgt) then e.preventDefault()
                if tgt != null && scrollKeyPrevented(ke.key, tgt) && tgt.closest("[data-kyo-scroll-keys]") != null then
                    e.preventDefault()
                if tgt != null && doubleActivation(ke.key, tgt) then e.preventDefault()
            end if
            // filledClass: the class follows the field's text where the reader changes it, before any event is forwarded.
            if e.`type` == "input" || e.`type` == "change" then
                val tgt = e.target.asInstanceOf[dom.Element]
                if tgt != null && js.typeOf(tgt.asInstanceOf[js.Dynamic].getAttribute) == "function" then applyFilled(tgt)
            end if
            // The click's default goes the same way, so a readonly control does not toggle under the pointer.
            if e.`type` == "click" then
                val tgt = e.target.asInstanceOf[dom.Element]
                if tgt != null && tgt.closest("[data-kyo-inert]") != null then e.preventDefault()
            findPathElement(e.target.asInstanceOf[dom.Element]).foreach { target =>
                val path    = parsePath(target.getAttribute("data-kyo-path"))
                val evTypes = ChainTypes(target)
                val t       = e.`type`

                val event: Maybe[UIEvent] =
                    if t == "click" then
                        val targetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val me       = e.asInstanceOf[dom.MouseEvent]
                        val mouse    = MouseEventData(
                            modifiers = UI.Modifiers(me.ctrlKey, me.altKey, me.shiftKey, me.metaKey),
                            targetId = targetId,
                            position = Present(UI.Point(me.clientX, me.clientY)),
                            self = e.target.asInstanceOf[dom.Element] eq target
                        )
                        // Prevent the browser's default navigation only when the anchor carries a kyo
                        // click handler (so the handler, not the href, drives the action). A plain href
                        // keeps native behavior: an in-page `#anchor` scrolls, and a cross-document route
                        // is handled by UILocation's interceptor. Prevent-defaulting every anchor here
                        // would also kill those. A modified click (ctrl/cmd/shift/alt, or a non-primary button) asks
                        // the browser for a new tab or window, so its default stays too; the handler runs either way.
                        // Same test as UILocation's anchor interceptor. A download anchor keeps its default as well.
                        val modifiedClick = me.ctrlKey || me.metaKey || me.shiftKey || me.altKey || me.button != 0
                        if !modifiedClick && target.tagName.toLowerCase == "a" && !target.hasAttribute("download") &&
                            evTypes.contains("click")
                        then
                            e.preventDefault()
                        end if
                        clickSubmitGuard = true
                        discard(dom.window.setTimeout(() => clickSubmitGuard = false, 0))
                        Present(UIEvent.Click(path, mouse))
                    else if t == "contextmenu" && evTypes.contains("contextmenu") then
                        // Suppress the native menu only when a handler was declared (this branch fired).
                        e.preventDefault()
                        val targetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val me       = e.asInstanceOf[dom.MouseEvent]
                        Present(UIEvent.ContextMenu(
                            path,
                            MouseEventData(
                                modifiers = UI.Modifiers(me.ctrlKey, me.altKey, me.shiftKey, me.metaKey),
                                targetId = targetId,
                                position = Present(UI.Point(me.clientX, me.clientY))
                            )
                        ))
                    else if t == "input" && evTypes.contains("input") then
                        Present(UIEvent.Input(path, e.target.asInstanceOf[dom.html.Input].value))
                    else if t == "change" && evTypes.contains("change") then
                        val tgt = e.target.asInstanceOf[dom.html.Input]
                        val typ = tgt.`type`
                        if typ == "checkbox" || typ == "radio" then
                            Present(UIEvent.ChangeChecked(path, tgt.checked))
                        else if typ == "number" || typ == "range" then
                            Present(UIEvent.ChangeNumeric(path, tgt.value.toDouble))
                        else if typ == "file" then
                            val files = tgt.files
                            if evTypes.contains("fileselect") then
                                // A FileReader per file, indexed so order survives; post FileSelect once all complete.
                                val n = files.length
                                if n > 0 then
                                    val results = new Array[UI.FilePayload](n)
                                    var doneCt  = 0
                                    var i       = 0
                                    while i < n do
                                        val ix     = i
                                        val f      = files(ix)
                                        val reader = new dom.FileReader()
                                        reader.onload = (_: dom.Event) =>
                                            val content = reader.result.asInstanceOf[String]
                                            results(ix) = UI.FilePayload(f.name, f.size.toLong, f.`type`, content)
                                            doneCt += 1
                                            if doneCt == n then
                                                fireFromJs(events, dispatch(path, UIEvent.FileSelect(path, results.toSeq)).unit)
                                        reader.readAsText(f)
                                        i += 1
                                    end while
                                end if
                                Absent
                            else
                                // Without onFileSelect: the first file's text as a Change.
                                if files.length > 0 then
                                    val reader = new dom.FileReader()
                                    reader.onload = (_: dom.Event) =>
                                        val content = reader.result.asInstanceOf[String]
                                        fireFromJs(events, dispatch(path, UIEvent.Change(path, content)).unit)
                                    reader.readAsText(files(0))
                                end if
                                Absent
                            end if
                        else
                            Present(UIEvent.Change(path, tgt.value))
                        end if
                    else if t == "submit" && evTypes.contains("submit") then
                        e.preventDefault()
                        if clickSubmitGuard then Absent
                        else
                            val submitTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                            val submitMouse    = MouseEventData(
                                modifiers = UI.Modifiers.none,
                                targetId = submitTargetId
                            )
                            Present(UIEvent.Submit(path, submitMouse))
                        end if
                    else if t == "keydown" && evTypes.contains("keydown") then
                        val ke         = e.asInstanceOf[dom.KeyboardEvent]
                        val kdTargetId = Maybe(ke.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        Present(UIEvent.KeyDown(
                            path,
                            KeyboardEventData(
                                key = ke.key,
                                modifiers = UI.Modifiers(ke.ctrlKey, ke.altKey, ke.shiftKey, ke.metaKey),
                                targetId = kdTargetId,
                                self = ke.target.asInstanceOf[dom.Element] eq target
                            )
                        ))
                    else if t == "keyup" && evTypes.contains("keyup") then
                        val ke         = e.asInstanceOf[dom.KeyboardEvent]
                        val kuTargetId = Maybe(ke.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        Present(UIEvent.KeyUp(
                            path,
                            KeyboardEventData(
                                key = ke.key,
                                modifiers = UI.Modifiers(ke.ctrlKey, ke.altKey, ke.shiftKey, ke.metaKey),
                                targetId = kuTargetId
                            )
                        ))
                    else if t == "focus" && evTypes.contains("focus") then
                        val focusTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        // FocusEvent does not carry modifier keys (not a MouseEvent); use Modifiers.none
                        Present(UIEvent.Focus(
                            path,
                            MouseEventData(UI.Modifiers.none, focusTargetId)
                        ))
                    else if t == "blur" && evTypes.contains("blur") then
                        val blurred      = e.target.asInstanceOf[dom.Element]
                        val blurTargetId = Maybe(blurred.id).filter(_.nonEmpty)
                        // blurtext: a text field's blur carries the text it holds as focus leaves it, read off the field
                        // itself. Twin of the blur branch in clientJs.
                        val text: Maybe[String] =
                            if !(blurred eq target) || !evTypes.contains("blurtext") then Absent
                            else
                                blurred.tagName.toUpperCase match
                                    case "INPUT"    => Present(blurred.asInstanceOf[dom.html.Input].value)
                                    case "TEXTAREA" => Present(blurred.asInstanceOf[dom.html.TextArea].value)
                                    case _          => Absent
                        Present(UIEvent.Blur(path, MouseEventData(UI.Modifiers.none, blurTargetId), text))
                    else if t == "mouseover" && evTypes.contains("mouseover") then
                        val hoverTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val me            = e.asInstanceOf[dom.MouseEvent]
                        Present(UIEvent.Hover(
                            path,
                            MouseEventData(
                                modifiers = UI.Modifiers(me.ctrlKey, me.altKey, me.shiftKey, me.metaKey),
                                targetId = hoverTargetId,
                                position = Present(UI.Point(me.clientX, me.clientY))
                            )
                        ))
                    else if t == "mouseout" && evTypes.contains("mouseout") then
                        val unhoverTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val me              = e.asInstanceOf[dom.MouseEvent]
                        Present(UIEvent.Unhover(
                            path,
                            MouseEventData(
                                modifiers = UI.Modifiers(me.ctrlKey, me.altKey, me.shiftKey, me.metaKey),
                                targetId = unhoverTargetId,
                                position = Present(UI.Point(me.clientX, me.clientY))
                            )
                        ))
                    else if t == "wheel" && evTypes.contains("wheel") then
                        val wheelTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val we            = e.asInstanceOf[dom.WheelEvent]
                        Present(UIEvent.Scroll(
                            path,
                            deltaX = we.deltaX,
                            deltaY = we.deltaY,
                            modifiers = UI.Modifiers(we.ctrlKey, we.altKey, we.shiftKey, we.metaKey),
                            targetId = wheelTargetId
                        ))
                    else
                        Absent

                event.foreach { ev =>
                    fireFromJs(events, dispatch(path, ev).unit)
                }
            }
        end handler

        val wheelOptions = js.Dynamic.literal(capture = true, passive = false).asInstanceOf[dom.EventListenerOptions]
        // onScrollPosition: rAF-coalesces a scroll burst to one dispatch per frame. Scroll does not bubble, but the
        // capture-phase listener catches descendant viewport scrolls; a page-level scroll (no data-kyo-path) is ignored.
        var scrRaf                                               = 0
        var scrEl: dom.Element                                   = null
        var scrPath: Seq[String]                                 = Seq.empty
        val scrollHandler: scalajs.js.Function1[dom.Event, Unit] = (e: dom.Event) =>
            findPathElement(e.target.asInstanceOf[dom.Element]).foreach { target =>
                if declaredInChain(target, "scroll") then
                    scrEl = target
                    scrPath = parsePath(target.getAttribute("data-kyo-path"))
                    if scrRaf == 0 then
                        scrRaf = dom.window.requestAnimationFrame { (_: Double) =>
                            scrRaf = 0
                            val sid = Maybe(scrEl.id).filter(_.nonEmpty)
                            fireFromJs(
                                events,
                                dispatch(scrPath, UIEvent.ScrollPosition(scrPath, scrEl.scrollTop, scrEl.scrollLeft, sid)).unit
                            )
                        }
                    end if
            }
        val scrollOptions = js.Dynamic.literal(capture = true, passive = true).asInstanceOf[dom.EventListenerOptions]
        listeners.add("click", handler, true)
        listeners.add("contextmenu", handler, true)
        listeners.add("input", handler, true)
        listeners.add("change", handler, true)
        listeners.add("submit", handler, true)
        listeners.add("keydown", handler, true)
        listeners.add("keyup", handler, true)
        listeners.add("focus", handler, true)
        listeners.add("blur", handler, true)
        listeners.add("mouseover", handler, true)
        listeners.add("mouseout", handler, true)
        listeners.add("wheel", handler, wheelOptions)
        listeners.add("scroll", scrollHandler, scrollOptions)
    end setupEventDelegation

    private def findPathElement(el: dom.Element): Maybe[dom.Element] =
        if el == null || (el eq document.body) then Absent
        else if el.hasAttribute("data-kyo-path") then Present(el)
        else
            el.parentNode match
                case p: dom.Element => findPathElement(p)
                case _              => Absent

    private def parsePath(p: String): Seq[String] =
        if p == null || p.isEmpty then Seq.empty
        else p.split("\\.").toSeq

    // ---- feature-marker gates ----

    /** The optional features a patch scans for, as bits of [[markersIn]]. */
    private[kyo] object Marker:
        inline val Enter      = 1
        inline val Leave      = 2
        inline val FocusAuto  = 4
        inline val ScrollAuto = 8
        inline val JsProp     = 16
        inline val SvgAnim    = 32
        inline val Portal     = 64
        inline val Attach     = 128
        inline val Filled     = 256
        inline val All        = 511
    end Marker

    /** The markers `html` carries, among those not already in `known`. */
    private[kyo] def markersIn(html: String, known: Int = 0): Int =
        var found = 0
        if (known & Marker.Enter) == 0 && html.contains("data-kyo-enter") then found |= Marker.Enter
        if (known & Marker.Leave) == 0 && html.contains("data-kyo-leave") then found |= Marker.Leave
        if (known & Marker.FocusAuto) == 0 && html.contains("data-kyo-focus-auto") then found |= Marker.FocusAuto
        if (known & Marker.ScrollAuto) == 0 && html.contains("data-kyo-scroll-auto") then found |= Marker.ScrollAuto
        if (known & Marker.JsProp) == 0 && html.contains("data-kyo-prop-") then found |= Marker.JsProp
        // Covers <animate, <animateTransform and <animateMotion in one search.
        if (known & Marker.SvgAnim) == 0 && html.contains("<animate") then found |= Marker.SvgAnim
        if (known & Marker.Portal) == 0 && html.contains("data-kyo-portal") then found |= Marker.Portal
        if (known & Marker.Attach) == 0 && html.contains("data-kyo-attach") then found |= Marker.Attach
        // One search covers data-kyo-filled and data-kyo-filled-up.
        if (known & Marker.Filled) == 0 && html.contains(FilledClass.classAttr) then found |= Marker.Filled
        found
    end markersIn

    /** The markers any HTML that entered this document carried.
      *
      * Sticky, and a clear bit is a proof rather than a guess: markup becomes DOM only through the boot
      * `innerHTML`, a region repaint and a list patch, each of which records here first, and no code path adds
      * these attributes to a live element (`data-kyo-ghost` is the only imperative one, and nothing scans for it).
      * So while a bit is clear, every scan it guards would walk a subtree to return nothing.
      *
      * An app with no enter or leave transition, focus-auto, scroll-auto, JS property or SMIL animation skips all of
      * these scans, including the `querySelectorAll("*")` for JS properties, which visits every descendant of every
      * new row.
      */
    private var seenMarkers = 0

    private def saw(marker: Int): Boolean = (seenMarkers & marker) != 0

    private def noteMarkers(html: String): Unit =
        if seenMarkers != Marker.All then seenMarkers |= markersIn(html, seenMarkers)

    // ---- enter/leave transition mirror (SPA transport) ----

    /** The set of `data-kyo-path` values of every `data-kyo-enter` element inside `root`, `root` itself included. */
    private def enterPaths(root: dom.Element): Set[String] =
        if !saw(Marker.Enter) then return Set.empty
        val els = root.querySelectorAll("[data-kyo-enter]")
        val ds  = (0 until els.length).flatMap { i =>
            Maybe(els(i).asInstanceOf[dom.Element].getAttribute("data-kyo-path")).toList
        }.toSet
        if root.hasAttribute("data-kyo-enter") && root.hasAttribute("data-kyo-path") then
            ds + root.getAttribute("data-kyo-path")
        else ds
    end enterPaths

    private def enterPaths(roots: Seq[dom.Element]): Set[String] =
        roots.iterator.flatMap(enterPaths).toSet

    /** Animate every `data-kyo-enter` element under `newRoot` (root included) whose path is not in `oldSet`: add the enter
      * classes, force a reflow, then remove them next frame so the CSS transition runs from the enter-from state.
      */
    private def seedEnter(newRoot: dom.Element, oldSet: Set[String]): Unit =
        seedEnter(Seq(newRoot), oldSet)

    private def seedEnter(newRoots: Seq[dom.Element], oldSet: Set[String]): Unit =
        if !saw(Marker.Enter) then return
        newRoots.foreach { newRoot =>
            val els  = newRoot.querySelectorAll("[data-kyo-enter]")
            val cand =
                (if newRoot.hasAttribute("data-kyo-enter") then Seq(newRoot) else Seq.empty) ++
                    (0 until els.length).map(els(_).asInstanceOf[dom.Element])
            cand.foreach { el =>
                val p = el.getAttribute("data-kyo-path")
                if p != null && !oldSet.contains(p) then
                    val cls     = el.getAttribute("data-kyo-enter").split("\\s+").filter(_.nonEmpty)
                    val clsList = el.asInstanceOf[scalajs.js.Dynamic].classList
                    cls.foreach(c => clsList.add(c))
                    val _ = el.asInstanceOf[scalajs.js.Dynamic].offsetWidth // force reflow
                    discard(dom.window.requestAnimationFrame { (_: Double) =>
                        cls.foreach(c => clsList.remove(c))
                    })
                end if
            }
        }
    end seedEnter

    private def leavePaths(roots: Seq[dom.Element]): Set[String] =
        if !saw(Marker.Leave) then return Set.empty
        roots.iterator.flatMap { root =>
            val descendants = root.querySelectorAll("[data-kyo-leave]")
            val elements    =
                (if root.hasAttribute("data-kyo-leave") then Iterator.single(root) else Iterator.empty) ++
                    (0 until descendants.length).iterator.map(descendants(_).asInstanceOf[dom.Element])
            elements.flatMap(element => Maybe(element.getAttribute("data-kyo-path")).toList)
        }.toSet
    end leavePaths

    /** The set of paths of `data-kyo-leave` elements in an HTML fragment that survive an element replacement. */
    private def leaveSurvSet(html: String): Set[String] =
        if !saw(Marker.Leave) then return Set.empty
        val tpl = document.createElement("template").asInstanceOf[scalajs.js.Dynamic]
        tpl.innerHTML = html
        val content = tpl.content.asInstanceOf[dom.DocumentFragment]
        val els     = content.querySelectorAll("[data-kyo-leave]")
        (0 until els.length).flatMap { i =>
            Maybe(els(i).asInstanceOf[dom.Element].getAttribute("data-kyo-path")).toList
        }.toSet
    end leaveSurvSet

    /** Strip `data-kyo-*` and `id` from a subtree so a ghost clone is inert (no selector collisions). */
    private def stripKyo(el: dom.Element): Unit =
        def strip(e: dom.Element): Unit =
            val dyn = e.asInstanceOf[scalajs.js.Dynamic]
            if scalajs.js.typeOf(dyn.getAttributeNames) == "function" then
                val names = dyn.getAttributeNames().asInstanceOf[scalajs.js.Array[String]]
                names.foreach(n => if n.startsWith("data-kyo-") || n == "id" then e.removeAttribute(n))
        end strip
        strip(el)
        val ds = el.querySelectorAll("*")
        (0 until ds.length).foreach(i => strip(ds(i).asInstanceOf[dom.Element]))
    end stripKyo

    /** Prepare leave ghosts for the OUTERMOST `data-kyo-leave` elements under `root` being removed (path not in `surv`).
      * Captures rect + clone WHILE the node is still in the DOM; returns (sourceNode, ghostNode, leaveClasses)
      * descriptors. The survivor set is a PREDICTION: a preserved subtree survives the patch despite not matching, which
      * is exactly what the opaque mount boundary does when it keeps a live mount's content under differently-shaped
      * incoming html. [[spawnGhosts]] therefore re-checks the SOURCE at spawn time and drops nodes still in the document.
      */
    private def prepareLeaveGhosts(root: dom.Element, surv: Set[String]): Seq[(dom.Element, dom.Element, String)] =
        prepareLeaveGhosts(Seq(root), surv)

    private def prepareLeaveGhosts(roots: Seq[dom.Element], surv: Set[String]): Seq[(dom.Element, dom.Element, String)] =
        if !saw(Marker.Leave) then return Seq.empty
        val cand = roots.flatMap { root =>
            val els = root.querySelectorAll("[data-kyo-leave]")
            (if root.getAttribute("data-kyo-leave") != null then Seq(root) else Seq.empty) ++
                (0 until els.length).map(els(_).asInstanceOf[dom.Element])
        }
        val removed = cand.filter { e =>
            val p = e.getAttribute("data-kyo-path")
            p == null || !surv.contains(p)
        }
        val outer = removed.filterNot(e => removed.exists(o => (o ne e) && o.contains(e)))
        outer.map { node =>
            val rect  = node.asInstanceOf[scalajs.js.Dynamic].getBoundingClientRect()
            val leave = node.getAttribute("data-kyo-leave")
            val g     = node.cloneNode(true).asInstanceOf[dom.Element]
            stripKyo(g)
            val st = g.asInstanceOf[scalajs.js.Dynamic].style
            st.position = "fixed"
            st.left = rect.left.asInstanceOf[Double].toString + "px"
            st.top = rect.top.asInstanceOf[Double].toString + "px"
            st.width = rect.width.asInstanceOf[Double].toString + "px"
            st.height = rect.height.asInstanceOf[Double].toString + "px"
            // At <body> a class's percentage min-size resolves against the viewport and beats the inline size (a
            // dropdown's min-width:100% stretched the ghost to the window edge). Only the minimum is reset: max-* stays
            // with the classes, so a leave that collapses through max-height still can.
            st.boxSizing = "border-box"
            st.minWidth = "0"
            st.minHeight = "0"
            st.margin = "0"
            st.pointerEvents = "none"
            g.setAttribute("data-kyo-ghost", "1")
            (node, g, if leave == null then "" else leave)
        }
    end prepareLeaveGhosts

    /** Append prepared ghosts to `<body>`, add their leave classes next frame, remove on transitionend/animationend or a
      * 1s safety. A ghost whose SOURCE node is still in the document is dropped: the patch preserved it, so playing a
      * leave animation over the live element would be a false departure. Removal-based rather than predictive, so every
      * preservation mechanism the morph grows is covered without a matching change here.
      */
    private def spawnGhosts(ghosts: Seq[(dom.Element, dom.Element, String)]): Unit =
        ghosts.foreach { case (src, g, leave) =>
            if !document.contains(src) then spawnGhost(g, leave)
        }
    end spawnGhosts

    private def spawnGhost(g: dom.Element, leave: String): Unit =
        discard(document.body.appendChild(g))
        val cls     = leave.split("\\s+").filter(_.nonEmpty)
        val clsList = g.asInstanceOf[scalajs.js.Dynamic].classList
        discard(dom.window.requestAnimationFrame((_: Double) => cls.foreach(c => clsList.add(c))))
        var done            = false
        def cleanup(): Unit =
            if !done then
                done = true
                if g.parentNode != null then discard(g.parentNode.removeChild(g))
        val listener: scalajs.js.Function1[dom.Event, Unit] = (_: dom.Event) => cleanup()
        g.addEventListener("transitionend", listener)
        g.addEventListener("animationend", listener)
        val to: scalajs.js.Function0[Unit] = () => cleanup()
        discard(dom.window.setTimeout(to, 1000.0))
    end spawnGhost

    // ---- input filter/mask (SPA transport) ----
    // The character-level decisions live in the shared InputMasking so they are testable without a DOM;
    // what stays here is the DOM wiring.

    private def dispatchInput(t: dom.EventTarget): Unit =
        val ctor = scalajs.js.Dynamic.global.Event
        val ev   = scalajs.js.Dynamic.newInstance(ctor)("input", scalajs.js.Dynamic.literal(bubbles = true))
        discard(t.asInstanceOf[scalajs.js.Dynamic].dispatchEvent(ev))
    end dispatchInput

    private def setValue(t: dom.html.Input, v: String): Unit =
        t.value = v
        setSelection(t, v.length, v.length)
        dispatchInput(t)
    end setValue

    private def setFilteredAt(t: dom.html.Input, txt: String, s: Int, e: Int): Unit =
        val v = t.value
        t.value = v.substring(0, s) + txt + v.substring(e)
        val np = s + txt.length
        setSelection(t, np, np)
        dispatchInput(t)
    end setFilteredAt

    private def setupInputMasking(listeners: BodyListeners)(using Frame): Unit =
        val handler: scalajs.js.Function1[dom.Event, Unit] = (e: dom.Event) =>
            val tRaw = e.target
            if tRaw != null then
                val el = tRaw.asInstanceOf[dom.Element]
                // Interactive.data lets any element carry data-kyo-filter, and everything below assumes a value
                // property and a text selection. Throwing from a beforeinput capture listener would break typing
                // for the whole page, so anything but a text field is left alone.
                val isTextField = el.tagName == "INPUT" || el.tagName == "TEXTAREA"
                val filt        = if isTextField then el.getAttribute("data-kyo-filter") else null
                val mask        = if isTextField then el.getAttribute("data-kyo-mask") else null
                if filt != null || mask != null then
                    val t             = tRaw.asInstanceOf[dom.html.Input]
                    val dyn           = e.asInstanceOf[scalajs.js.Dynamic]
                    val it            = if scalajs.js.typeOf(dyn.inputType) == "string" then dyn.inputType.asInstanceOf[String] else ""
                    def selStart: Int =
                        val d = t.asInstanceOf[scalajs.js.Dynamic]
                        if scalajs.js.typeOf(d.selectionStart) == "number" then d.selectionStart.asInstanceOf[Int] else t.value.length
                    def selEnd: Int =
                        val d = t.asInstanceOf[scalajs.js.Dynamic]
                        if scalajs.js.typeOf(d.selectionEnd) == "number" then d.selectionEnd.asInstanceOf[Int] else selStart
                    def transferText: String =
                        val dt = dyn.dataTransfer
                        if dt != null && scalajs.js.typeOf(dt) == "object" then dt.getData("text").asInstanceOf[String]
                        else if scalajs.js.typeOf(dyn.data) == "string" then dyn.data.asInstanceOf[String]
                        else ""
                    end transferText
                    // insertCompositionText is deliberately absent below: preventDefault on it does not filter the
                    // input, it aborts the composition, which breaks CJK input, dead keys and mobile autocorrect.
                    // Composition is let through and the finished text is corrected by the compositionend listener.
                    if filt != null then
                        if it.startsWith("delete") then ()
                        else if it == "insertText" || it == "insertReplacementText" then
                            if scalajs.js.typeOf(dyn.data) == "string" then
                                val ds = dyn.data.asInstanceOf[String]
                                val f1 = InputMasking.filterStr(filt, ds, t.value)
                                if f1 != ds then
                                    e.preventDefault()
                                    if f1.nonEmpty then setFilteredAt(t, f1, selStart, selEnd)
                        else if it == "insertFromPaste" || it == "insertFromDrop" then
                            e.preventDefault()
                            val f2 = InputMasking.filterStr(filt, transferText, t.value)
                            if f2.nonEmpty then setFilteredAt(t, f2, selStart, selEnd)
                        end if
                    else if mask != null then
                        val tokens = InputMasking.parseMask(mask)
                        if it.startsWith("delete") then
                            e.preventDefault()
                            val raw = InputMasking.maskRaw(tokens, t.value)
                            val nr  = if raw.nonEmpty then raw.substring(0, raw.length - 1) else raw
                            setValue(t, InputMasking.maskFormat(tokens, nr))
                        else if it == "insertText" || it == "insertReplacementText" ||
                            it == "insertFromPaste" || it == "insertFromDrop"
                        then
                            e.preventDefault()
                            val ins = if it == "insertFromPaste" || it == "insertFromDrop" then transferText
                            else if scalajs.js.typeOf(dyn.data) == "string" then dyn.data.asInstanceOf[String]
                            else ""
                            var raw2 = InputMasking.maskRaw(tokens, t.value)
                            var ci   = 0
                            var full = false
                            while ci < ins.length && !full do
                                InputMasking.maskClassAt(tokens, raw2.length) match
                                    case Present(cls) =>
                                        val ch = ins.charAt(ci)
                                        if InputMasking.maskOk(cls, ch) then raw2 = raw2 + ch
                                    case Absent => full = true
                                end match
                                ci += 1
                            end while
                            setValue(t, InputMasking.maskFormat(tokens, raw2))
                        end if
                    end if
                end if
            end if
        listeners.add("beforeinput", handler, true)
        listeners.add("compositionend", compositionEndHandler, true)
    end setupInputMasking

    /** Corrects the whole value once a composition finishes.
      *
      * An IME, a dead key or mobile autocorrect produces its text only when the composition ends, so there is no
      * per-character event to constrain; the finished value is filtered or formatted here instead. Writing back only
      * on a change keeps a composition that already conforms free of a caret jump and of a spurious input event.
      * Mirrored by `kyoCompositionEnd` in `HtmlRenderer.clientJs`.
      */
    private val compositionEndHandler: scalajs.js.Function1[dom.Event, Unit] = (e: dom.Event) =>
        val tRaw = e.target
        if tRaw != null then
            val el = tRaw.asInstanceOf[dom.Element]
            if el.tagName == "INPUT" || el.tagName == "TEXTAREA" then
                val t    = tRaw.asInstanceOf[dom.html.Input]
                val filt = el.getAttribute("data-kyo-filter")
                val mask = el.getAttribute("data-kyo-mask")
                val v    = t.value
                val nv   =
                    if filt != null then InputMasking.filterStr(filt, v, "")
                    else if mask != null then InputMasking.maskNormalize(mask, v)
                    else v
                if nv != v then setValue(t, nv)
            end if
        end if

    /** Whether the browser's own activation of `target` would run a handler the dispatcher runs again.
      *
      * The rule itself is [[KeyPolicy.doubleActivates]], shared with the server-push client; this
      * reads the facts it needs off the DOM. The button's `type` is not among them: the dispatcher
      * answers a keydown by synthesizing the click, so the browser's activation is the second one
      * whatever the type says (see the rule's own scaladoc).
      */
    private def doubleActivation(key: String, target: dom.Element): Boolean =
        // Every branch of the rule needs one of these keys, so any other key leaves before the ancestor walk.
        KeyPolicy.mayActivate(key) && {
            val declared = target.getAttribute("data-kyo-ev")
            KeyPolicy.doubleActivates(
                key = key,
                tag = target.tagName,
                declaresClick = declared != null && declared.split(",").contains("click"),
                declaresKeyDown = declaredInChain(target, "keydown"),
                insideForm = target.closest("form") != null,
                role = if target.hasAttribute("role") then target.getAttribute("role") else ""
            )
        }
    end doubleActivation

    /** Whether a `preventScrollKeys` region should suppress the browser's page scroll for `key` on
      * `target`. The rule itself is [[KeyPolicy.preventsPageScroll]], shared with the server-push
      * client; this reads the two facts it needs off the DOM.
      */
    /** Walks a radio group that has an inert radio in it, the way [[KeyPolicy.radioStep]] says, and whether it did, in
      * which case the browser's own walk has to be declined. The browser's walk lands on the inert radio and sticks there;
      * this one lands where the browser's would if the inert radios were disabled, focusing that radio and clicking it, so
      * the selection and its change event are the browser's own. Twin of the radio block in clientJs.
      */
    private[kyo] def radioArrow(key: String, target: dom.Element): Boolean =
        // Tag and type are read as attributes, the way the rest of this client tells inputs apart: a type test against
        // HTMLInputElement needs that constructor as a global, which a DOM without it (jsdom as the tests install it) lacks.
        target.tagName == "INPUT" && KeyPolicy.radioStepKeys.contains(key) && {
            val r = target.asInstanceOf[dom.html.Input]
            r.`type` == "radio" && r.name.nonEmpty && {
                val radios = "input[type=\"radio\"]"
                val all    = if r.form != null then r.form.querySelectorAll(radios) else r.ownerDocument.querySelectorAll(radios)
                val group  = (0 until all.length).map(all(_).asInstanceOf[dom.html.Input])
                    .filter(x => x.name == r.name && (x.form eq r.form))
                val from = group.indexWhere(_ eq r)
                from >= 0 && {
                    val inert = group.map(_.closest("[data-kyo-inert]") != null)
                    val rtl   = dom.window.getComputedStyle(r).direction == "rtl"
                    KeyPolicy.radioStep(key, rtl, inert, group.map(_.disabled), from, r.checked) match
                        case KeyPolicy.RadioStep.Native    => false
                        case KeyPolicy.RadioStep.Hold      => true
                        case KeyPolicy.RadioStep.Select(i) =>
                            group(i).focus()
                            group(i).click()
                            true
                    end match
                }
            }
        }
    end radioArrow

    private def scrollKeyPrevented(key: String, target: dom.Element): Boolean =
        // `isContentEditable` is an HTMLElement member: an SVG target does not carry it, and jsdom implements
        // contentEditable on no element at all, so the property reads as undefined. Casting undefined straight to
        // Boolean throws a ClassCastException out of the keydown listener, which takes the whole delegation path
        // down with it. Read the property defensively and treat an absent one as "not editable".
        val value = target.asInstanceOf[scalajs.js.Dynamic].isContentEditable
        KeyPolicy.preventsPageScroll(
            key = key,
            tag = target.tagName,
            contentEditable = !scalajs.js.isUndefined(value) && value != null && value.asInstanceOf[Boolean],
            inputType = Maybe(target.getAttribute("type")).getOrElse("")
        )
    end scrollKeyPrevented

    private val SvgNs = "http://www.w3.org/2000/svg"

    /** The body twin of a portal slot: the (unique) re-homed element carrying `path`, a direct body child. */
    private def portalTwin(path: String): dom.Element =
        document.querySelector("body > " + pathSelector(path) + "[data-kyo-portal]")

    /** Re-homes every `data-kyo-portal` element still sitting inline under `root` (root included, so a freshly inserted one is
      * caught) to document.body, behind an inert placeholder stamped `data-kyo-portal-slot="<path>"` at its logical position. Twin
      * of `__kyoAdoptPortals` in HtmlRenderer.clientJs; keep them in lockstep.
      *
      * Must run after focus and enter seeding, both of which are subtree-scoped and need the element inline. Reparenting drops DOM
      * focus, so focus and caret held inside the moved subtree are captured and re-applied after the move; the enter transition is
      * unaffected, since its from-state classes release on the next frame. A document that never received portal markup skips the
      * scan (see [[seenMarkers]]).
      */
    private def adoptPortals(root: dom.Element): Unit =
        if root != null && saw(Marker.Portal) then
            val els  = root.querySelectorAll("[data-kyo-portal]")
            val cand =
                (if root.hasAttribute("data-kyo-portal") then Seq(root) else Seq.empty) ++
                    (0 until els.length).map(els(_).asInstanceOf[dom.Element])
            cand.foreach { el =>
                val path = el.getAttribute("data-kyo-path")
                // Only path-carrying elements can portal (the placeholder must key the slot); the parent check keeps
                // the sweep idempotent, since an adopted twin is a direct body child and never under a region again.
                if path != null && el.parentNode != null && (el.parentNode ne document.body) then
                    // A stale twin from a lost placeholder would collide on the path: drop it first (defensive).
                    val stale = portalTwin(path)
                    if stale != null && (stale ne el) then
                        DomReactiveRegions.forgetRangesUnder(stale)
                        discard(document.body.removeChild(stale))
                    val slot =
                        if el.namespaceURI == SvgNs then document.createElementNS(SvgNs, "g")
                        else document.createElement("span")
                    slot.setAttribute("data-kyo-portal-slot", path)
                    slot.setAttribute("hidden", "")
                    val ae                 = document.activeElement
                    val hadFocus           = ae != null && (ae ne document.body) && ((el eq ae) || el.contains(ae))
                    val (selStart, selEnd) = if hadFocus then readSelection(ae) else (Absent, Absent)
                    discard(el.parentNode.insertBefore(slot, el))
                    discard(document.body.appendChild(el))
                    if hadFocus then
                        focusNoScroll(ae)
                        (selStart, selEnd) match
                            case (Present(s), Present(e)) => setSelection(ae, s, e)
                            case _                        => ()
                    end if
                end if
            }
        end if
    end adoptPortals

    /** Retires every body twin whose placeholder is gone: its logical slot was removed or replaced, so it left with its region.
      * Its leave ghost is prepared, the twin removed, and the ghost spawned, in that order, since [[spawnGhosts]] drops ghosts whose
      * source is still connected. Twin of `__kyoRetireOrphanTwins` in HtmlRenderer.clientJs.
      *
      * Document-wide by necessity, since a twin sits outside every region subtree, so it runs once per patch, after the patch's roots
      * were adopted. A patch that only removes still runs it: it inserts nothing to adopt, but may have removed a placeholder.
      */
    private def retireOrphanTwins(): Unit =
        if saw(Marker.Portal) then
            val twins = document.querySelectorAll("body > [data-kyo-portal][data-kyo-path]")
            (0 until twins.length).foreach { i =>
                val twin = twins(i).asInstanceOf[dom.Element]
                val p    = twin.getAttribute("data-kyo-path")
                if document.querySelector(attrSelector("data-kyo-portal-slot", p)) == null then
                    val ghosts = prepareLeaveGhosts(twin, Set.empty)
                    DomReactiveRegions.forgetRangesUnder(twin)
                    discard(document.body.removeChild(twin))
                    spawnGhosts(ghosts)
                end if
            }
    end retireOrphanTwins

    // ---- pointer/drag delegation (SPA transport) ----

    // Drag-session state. Module-level mutable is safe on the single-threaded JS runtime (mutated only inside JS
    // event callbacks). A session is active between a pointerdown on a declaring element and its pointerup.
    private var ptrActive: Boolean             = false
    private var ptrEl: dom.Element             = null
    private var ptrPath: Seq[String]           = Seq.empty
    private var ptrRaf: Int                    = 0
    private var ptrPendingEv: dom.PointerEvent = null

    private def pointerPayload(el: dom.Element, ev: dom.PointerEvent): UI.PointerEvent =
        val r   = el.getBoundingClientRect()
        val tid = Maybe(ev.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
        UI.PointerEvent(
            x = ev.clientX - r.left,
            y = ev.clientY - r.top,
            rectX = r.left,
            rectY = r.top,
            rectW = r.width,
            rectH = r.height,
            buttons = ev.buttons,
            targetId = tid
        )
    end pointerPayload

    private def setupPointerDelegation(
        dispatch: (Seq[String], UIEvent) => Boolean < Async,
        events: Channel[Unit < Async],
        listeners: BodyListeners
    )(using Frame): Unit =
        val down: scalajs.js.Function1[dom.Event, Unit] = (e0: dom.Event) =>
            val e = e0.asInstanceOf[dom.PointerEvent]
            findPathElement(e.target.asInstanceOf[dom.Element]).foreach { el =>
                if declaredInChain(el, "pointerdown") then
                    try
                        val d = el.asInstanceOf[scalajs.js.Dynamic]
                        if scalajs.js.typeOf(d.setPointerCapture) == "function" then discard(d.setPointerCapture(e.pointerId))
                    catch case _: Throwable => ()
                    end try
                    ptrActive = true
                    ptrEl = el
                    ptrPath = parsePath(el.getAttribute("data-kyo-path"))
                    fireFromJs(events, dispatch(ptrPath, UIEvent.PointerDown(ptrPath, pointerPayload(el, e))).unit)
                end if
            }

        val move: scalajs.js.Function1[dom.Event, Unit] = (e0: dom.Event) =>
            // Only stream during an active session; coalesce to at most one dispatch per animation frame.
            if ptrActive && ptrEl != null then
                ptrPendingEv = e0.asInstanceOf[dom.PointerEvent]
                if ptrRaf == 0 then
                    ptrRaf = dom.window.requestAnimationFrame { (_: Double) =>
                        ptrRaf = 0
                        if ptrActive && ptrEl != null && ptrPendingEv != null then
                            val ev = ptrPendingEv
                            ptrPendingEv = null
                            fireFromJs(events, dispatch(ptrPath, UIEvent.PointerMove(ptrPath, pointerPayload(ptrEl, ev))).unit)
                        end if
                    }
                end if

        val up: scalajs.js.Function1[dom.Event, Unit] = (e0: dom.Event) =>
            if ptrActive && ptrEl != null then
                val e = e0.asInstanceOf[dom.PointerEvent]
                try
                    val d = ptrEl.asInstanceOf[scalajs.js.Dynamic]
                    if scalajs.js.typeOf(d.releasePointerCapture) == "function" then discard(d.releasePointerCapture(e.pointerId))
                catch case _: Throwable => ()
                end try
                if ptrRaf != 0 then
                    dom.window.cancelAnimationFrame(ptrRaf)
                    ptrRaf = 0
                ptrPendingEv = null
                val el   = ptrEl
                val path = ptrPath
                ptrActive = false
                ptrEl = null
                ptrPath = Seq.empty
                fireFromJs(events, dispatch(path, UIEvent.PointerUp(path, pointerPayload(el, e))).unit)

        listeners.add("pointerdown", down, true)
        listeners.add("pointermove", move, true)
        listeners.add("pointerup", up, true)
    end setupPointerDelegation

end DomBackend
