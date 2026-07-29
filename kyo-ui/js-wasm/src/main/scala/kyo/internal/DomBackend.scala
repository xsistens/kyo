package kyo.internal

import kyo.*
import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js

/** Scala.js UI backend. Mounts a UI into the browser DOM. */
private[kyo] object DomBackend:

    /** Focus seeding/restore stack for `data-kyo-focus-auto` / `data-kyo-focus-restore`. Each entry is (seeded element's
      * data-kyo-path, previously focused element's data-kyo-path if any, focus-restore flag). Mirrors the `__frs` stack in
      * HtmlRenderer.clientJs for the server-push transport. Module-level mutable state is safe here: all mutation happens
      * inside `Sync.defer` blocks on the single-threaded JS runtime.
      */
    private var focusReturnStack: List[(String, Maybe[String], Boolean)] = Nil

    /** Unit 8: the page-scoped drain channel, captured once per mount so the viewport scroll/resize listeners (raw JS
      * callbacks, outside any Kyo context) can bridge their `deliverMeasureById` effect back in via [[fireFromJs]]. Set
      * in `mountInto` before any op can be emitted. Module-level mutable state is safe on the single-threaded JS runtime.
      */
    private var sessionEvents: Maybe[Channel[Unit < Async]] = Absent

    /** Unit 8: live viewport observers for the SPA transport, keyed by element id. Each entry is the single handler
      * registered for BOTH `window` scroll (capture phase) and resize; Unobserve removes it from both and drops the
      * entry. Backed by a native `js.Map` (mirrors `UIMouseEventOps`), giving `contains`/`apply`/`update`/`remove`.
      */
    private val viewportObservers: js.WrappedMap[String, js.Function1[dom.Event, Unit]] =
        new js.WrappedMap(js.Map.empty[String, js.Function1[dom.Event, Unit]])

    /** Per-element ownership of specific attributes by the imperative id-addressed channel
      * (SetClassById/SetStyleById/SetAttrById), applied out of the render pass so CSS transitions on the toggled
      * class/attribute fire. The owned attribute names live in a `__kyoOwn` expando dict ON the element, so the flag
      * is reclaimed with the node (no session-lived set that only ever grows) and shields each owned attribute BY
      * NAME — `class`/`style` for the class/style ops, the given name for the attribute op. `morphAttrs` reads it so
      * an owned attribute is never reconciled back to server HTML, which never carries the client-set value. Mirrors
      * `__kyoMark`/`__kyoOwn` in HtmlRenderer.clientJs.
      */
    private def markOwned(el: dom.Element, name: String): Unit =
        val d = el.asInstanceOf[js.Dynamic]
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
        mountInto(ui, document.body, Absent)

    def mount(ui: UI, onHandlerError: Throwable => Unit < Async)(using Frame): Unit < (Async & Scope) =
        mountInto(ui, document.body, Present(onHandlerError))

    /** Mount a UI into a specific DOM element selected by CSS selector. */
    def mount(ui: UI, selector: String)(using Frame): Unit < (Async & Scope) =
        mount(ui, selector, Absent)

    def mount(ui: UI, selector: String, onHandlerError: Maybe[Throwable => Unit < Async])(using
        Frame
    ): Unit < (Async & Scope) =
        Sync.defer {
            val target = document.querySelector(selector)
            if target == null then Abort.panic(UIException(s"Element not found: $selector"))
            else mountInto(ui, target.asInstanceOf[dom.Element], onHandlerError)
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

    private def mountInto(
        ui: UI,
        container: dom.Element,
        onHandlerError: Maybe[Throwable => Unit < Async]
    )(using Frame): Unit < (Async & Scope) =
        // Unit 2/3: the session's imperative op channel. `emit` applies ops straight to the local DOM (there is no
        // wire): a Command focuses/scrolls the element; a RequestMeasure measures it and resolves the pending
        // requestMeasure callback directly (the SPA analog of the server's op -> client -> Measure round-trip). The
        // late-bound `sessionCommands` breaks the emit<->Commands construction cycle; it is set before any op is emitted.
        var sessionCommands: UI.Commands = null
        for
            _    <- DomStyleSheet.injectBase()
            root <- ReactiveUI.normalize(ui, Seq.empty, onHandlerError = onHandlerError)
            html <- HtmlRenderer.render(ui, Seq.empty)
            _    <- Sync.defer(container.innerHTML = html)
            _    <- applyJsProps(container)
            // Initial-mount seeding: everything just appeared, so the old path set is empty —
            // consistent with native autofocus (mirrors HtmlRenderer.clientJs's startup faSeed).
            _ <- Sync.defer(seedFocusAuto(container, Set.empty))
            // Unit 1: initial-mount enter seeding (any data-kyo-enter present at load animates in; old set empty).
            _ <- Sync.defer(seedEnter(container, Set.empty))
            // Portal adopt for the initial paint (a portal element present at load re-homes immediately).
            _ <- Sync.defer(portalSweep(container))
            _ <- Sync.defer(beginAnimationsSync(container))
            // Unit 3: document-level beforeinput capture listener for input filter/mask.
            _        <- setupInputMasking()
            commands <- UI.Commands.init(op => applyOpLocal(op, () => sessionCommands))
            _ = sessionCommands = commands
            // Env.run so component handlers and mounted effects resolve `UI.commands` at run time (the subscribe
            // region fibers and the event-drain fiber all fork inside this scope).
            _ <- Env.run(commands) {
                val exchange = LocalExchange(root)
                for
                    dispatch <- ReactiveUI.subscribe(root, exchange)
                    // Single-consumer drain owned by the ambient page Scope: every JS event effect is run by a
                    // Fiber.init consumer (interrupted on page teardown). The single consumer preserves event ordering
                    // and is scoped, so page teardown interrupt propagates to the drain via the ambient Scope.
                    events <- Channel.init[Unit < Async](256)
                    // Unit 8: capture the drain so viewport listener callbacks can bridge deliverMeasureById back in.
                    _ = sessionEvents = Present(events)
                    // runPartial captures only the Closed failure (the channel closed on page teardown -> stop draining); a
                    // Panic propagates rather than being silently swallowed as a clean drain end.
                    // The drain also carries the session's scroll sink: a handler calling UI.scrollIntoView scrolls the
                    // local document, the browser-mount counterpart of the server session's WebSocket op. Nested inside
                    // the ambient Env.run(commands) so both the upstream Local sink and the fork's Env channel are live.
                    _ <- Fiber.init(UICommands.scrollSink.let(Present(scrollLocal)) {
                        Loop.foreach(Abort.runPartial[Closed](events.take).map {
                            case Result.Success(eff) => eff.andThen(Loop.continue)
                            case Result.Failure(_)   => Loop.done
                        })
                    })
                    _ <- setupEventDelegation(dispatch.handle, events)
                    // Unit 1: pointer/drag delegation (setPointerCapture + rAF-coalesced move stream).
                    _ <- setupPointerDelegation(dispatch.handle, events)
                    _ <- Async.never
                yield ()
                end for
            }
        yield ()
        end for
    end mountInto

    // The local-document scroll sink installed on the mount's event-drain fiber; mirrors the embedded
    // client's ScrollIntoView handling exactly (missing id = no-op, smooth scroll to the block start),
    // so the same UI.scrollIntoView command behaves the same under either runner.
    private def scrollLocal(id: String)(using Frame): Unit < Async =
        Sync.defer {
            val el = document.getElementById(id)
            if el != null then
                discard(el.asInstanceOf[scalajs.js.Dynamic].scrollIntoView(scalajs.js.Dynamic.literal(
                    behavior = "smooth",
                    block = "start"
                )))
            end if
        }

    // ---- Unit 2/3: local op application for the SPA transport ----

    /** A conservative "focusable" CSS selector — what a focus command may land on. Mirrors
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

    /** Apply a whitelisted `verb` to `el` (shared by path- and id-addressed commands). Unknown verbs are ignored. */
    private def applyVerbDom(el: dom.Element, verb: String): Unit =
        if el != null then
            val dyn = el.asInstanceOf[scalajs.js.Dynamic]
            verb match
                case "focus" => focusInto(el)
                case "scrollIntoView" =>
                    if scalajs.js.typeOf(dyn.scrollIntoView) == "function" then
                        discard(dyn.scrollIntoView(scalajs.js.Dynamic.literal(block = "nearest")))
                case _ => () // unknown verb: ignore (forward-compat)
            end match
        end if
    end applyVerbDom

    /** Locate the element at `path`, preferring the non-wrapper match (reactive wrapper spans share the path). */
    private def applyCommandDom(path: Seq[String], verb: String): Unit =
        applyVerbDom(locateByPath(path.mkString(".")), verb)

    /** Self-addressing: resolve the command target by DOM id (getElementById) instead of the render path. */
    private def applyCommandDomById(id: String, verb: String): Unit =
        applyVerbDom(document.getElementById(id), verb)

    private def measureRect(el: dom.Element): Maybe[UI.Rect] =
        if el == null then Absent
        else
            val r = el.getBoundingClientRect()
            Present(UI.Rect(r.left, r.top, r.width, r.height, dom.window.innerWidth, dom.window.innerHeight))

    private def measureDom(path: Seq[String]): Maybe[UI.Rect] =
        measureRect(locateByPath(path.mkString(".")))

    /** Self-addressing: measure the element with DOM id `id` (getElementById). */
    private def measureDomById(id: String): Maybe[UI.Rect] =
        measureRect(document.getElementById(id))

    private def applyOpLocal(op: HtmlOp, commands: () => UI.Commands)(using Frame): Unit < Async =
        op match
            case HtmlOp.Command(path, verb) => Sync.defer(applyCommandDom(path, verb))
            case HtmlOp.RequestMeasure(path) =>
                Sync.defer(measureDom(path)).map {
                    case Present(rect) => commands().deliverMeasure(path, rect)
                    case Absent        => Kyo.unit
                }
            // Self-addressing: id-addressed twins resolve the element via getElementById.
            case HtmlOp.CommandById(id, verb) => Sync.defer(applyCommandDomById(id, verb))
            case HtmlOp.RequestMeasureById(id) =>
                Sync.defer(measureDomById(id)).map {
                    case Present(rect) => commands().deliverMeasureById(id, rect)
                    case Absent        => Kyo.unit
                }
            // Unit 8: toggle a class in place (element stays in the DOM, so CSS transitions on the class fire).
            case HtmlOp.SetClassById(id, className, on) =>
                Sync.defer {
                    val el = document.getElementById(id)
                    if el != null then
                        markOwned(el, "class")
                        discard(el.classList.toggle(className, on))
                }
            // Unit 8: merge an inline-style declaration string over the element's existing inline props (no clobber).
            case HtmlOp.SetStyleById(id, css) =>
                Sync.defer {
                    val el = document.getElementById(id)
                    if el != null then
                        markOwned(el, "style")
                        mergeStyleDomById(id, css)
                }
            // Unit 8: set an attribute in place (element stays in the DOM, so a CSS `>` anchored on it keeps matching).
            case HtmlOp.SetAttrById(id, name, value) =>
                Sync.defer {
                    val el = document.getElementById(id)
                    if el != null then
                        markOwned(el, name)
                        el.setAttribute(name, value)
                }
            // Unit 8: measure now + deliver, then attach the continuous scroll/resize observer for `id`.
            case HtmlOp.ObserveViewportById(id) =>
                Sync.defer(registerViewportObserver(id, commands)).andThen(
                    Sync.defer(measureDomById(id)).map {
                        case Present(rect) => commands().deliverMeasureById(id, rect)
                        case Absent        => Kyo.unit
                    }
                )
            // Unit 8: detach the scroll/resize observer previously registered for `id`.
            case HtmlOp.UnobserveViewportById(id) =>
                Sync.defer(unregisterViewportObserver(id))
            // Replace/Remove/InjectCss reach the DOM through LocalExchange, never this imperative channel.
            case _ => Kyo.unit
    end applyOpLocal

    /** Unit 8: merge a serialized `Style` declaration string ("prop:val;prop:val") onto getElementById(id) via
      * setProperty per declaration, so it MERGES over other inline props rather than clobbering them (unlike a full
      * `style=""` replace). Blank declarations and those without a `:` are skipped.
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

    /** Unit 8: attach a single handler to `window` scroll (capture) + resize that re-measures getElementById(id) and
      * bridges the deliver back into the drain via [[fireFromJs]]. Guards against double-registration for the same id.
      */
    private def registerViewportObserver(id: String, commands: () => UI.Commands)(using Frame): Unit =
        if !viewportObservers.contains(id) then
            val handler: js.Function1[dom.Event, Unit] = (_: dom.Event) =>
                measureDomById(id) match
                    case Present(rect) => sessionEvents.foreach(ev => fireFromJs(ev, commands().deliverMeasureById(id, rect)))
                    case Absent        => ()
            viewportObservers(id) = handler
            dom.window.addEventListener("scroll", handler, true)
            dom.window.addEventListener("resize", handler)
        end if
    end registerViewportObserver

    /** Unit 8: remove the scroll/resize handler registered for `id` (from both listeners) and drop the map entry. */
    private def unregisterViewportObserver(id: String): Unit =
        if viewportObservers.contains(id) then
            val handler = viewportObservers(id)
            dom.window.removeEventListener("scroll", handler, true)
            dom.window.removeEventListener("resize", handler)
            discard(viewportObservers.remove(id))
        end if
    end unregisterViewportObserver

    /** Exchange that renders UI to HTML and applies directly to the DOM. */
    private class LocalExchange(root: ReactiveUI) extends UIExchange:
        private def svgContextAt(path: Seq[String]): Boolean =
            ReactiveUI.findNode(root, path).map(_.svgContext).getOrElse(false)

        // Declarative reactive-attribute patch: set the attribute in place on the element at `path`, marking
        // ownership so a parent region's morph won't reconcile the live value back (the __kyoOwn shield).
        override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
            Sync.defer {
                val el = locateByPath(path.mkString("."))
                if el != null then
                    markOwned(el, name)
                    el.setAttribute(name, value)
            }

        // Presence-based boolean twin: toggle the attribute in place, ownership-marked so a morph won't undo it.
        override def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async =
            Sync.defer {
                val el = locateByPath(path.mkString("."))
                if el != null then
                    markOwned(el, name)
                    if value then el.setAttribute(name, "") else el.removeAttribute(name)
            }

        // Reactive class twin: toggle the class in place (CSS transitions fire), own "class" against the morph.
        override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
            Sync.defer {
                val el = locateByPath(path.mkString("."))
                if el != null then
                    markOwned(el, "class")
                    discard(el.classList.toggle(name, on))
            }

        def onChange(path: Seq[String], ui: UI, mount: Boolean)(using Frame): Unit < Async =
            // Render the content at its (possibly nested-reactive) sub-path so a reactive-valued region
            // wraps a DISTINCT inner wrapper — matching SSR/walkStatic. The wrapper stays at `path`, so
            // the morph still keys/addresses this region's node.
            HtmlRenderer.render(ui, HtmlRenderer.contentPath(path, ui), mountSlot = true).map { html =>
                // Always wrap the rendered html in the reactive boundary element so the node carrying
                // data-kyo-path=path survives subsequent replacements. A Fragment, Text, or RawHtml value
                // renders without a path-carrying root, so an unwrapped replace would drop the marker and
                // the next update could not locate the node. In SVG context the boundary is a <g> (a <span>
                // is invalid inside <svg>); otherwise a <span> (CSS sets `display: contents` so it is layout-
                // transparent).
                val tag      = if svgContextAt(path) then "g" else "span"
                val pathAttr = path.mkString(".")
                // A keyless mount's content paint flags its root with data-kyo-mount so a parent region's morph treats
                // it as opaque and never wipes the live subtree (the mount repaints it here). Client-only marker —
                // never emitted by the server render, so golden HTML stays byte-identical.
                val mountAttr = if mount then " data-kyo-mount" else ""
                val finalHtml = s"""<$tag data-kyo-path="$pathAttr" data-kyo-reactive$mountAttr>$html</$tag>"""
                Sync.defer {
                    val el = document.querySelector(s"""[data-kyo-path="$pathAttr"]""")
                    // An enclosing region (a `when`/gate that wraps the mount at the mount's own path) repaints this
                    // slot on every re-render. Leave the live mount root untouched ONLY when the new content is itself
                    // a mount slot (its rendered root carries data-kyo-mount-slot) — the SAME mount re-rendering, which
                    // owns and repaints its own subtree. Different content (no marker) or an EMPTY repaint (gate closed)
                    // falls through so the morph reconciles/empties the slot.
                    if el != null && !mount && htmlRootHasMountSlot(html) &&
                        el.asInstanceOf[dom.Element].hasAttribute("data-kyo-mount")
                    then ()
                    else if el != null && el.outerHTML != finalHtml then
                        // Capture focus and caret of the active element inside the replaced region,
                        // keyed on data-kyo-path identity (mirrors HtmlRenderer.clientJs:576-583 on
                        // the JS DOM API). Plain DOM inside the already-suspended Sync.defer; no new
                        // AllowUnsafe crossing.
                        val ae = document.activeElement
                        val insideRegion = ae != null && (ae ne document.body) &&
                            (ae.getAttribute("data-kyo-path") == pathAttr || el.contains(ae))
                        // Use the active element's own data-kyo-path when it carries one (nested
                        // reactive region), otherwise fall back to pathAttr so the region wrapper
                        // itself is queried (common case: value-bound input inside the region has
                        // no data-kyo-path of its own).
                        val activePath =
                            if insideRegion then
                                if ae.hasAttribute("data-kyo-path") then ae.getAttribute("data-kyo-path")
                                else pathAttr
                            else null
                        val (selStart, selEnd) = if insideRegion then readSelection(ae) else (Absent, Absent)
                        // Capture the focus-auto paths present BEFORE the replace so seeding only fires
                        // for elements that newly appeared (a re-render of an already-open overlay must
                        // not steal the user's focus).
                        val oldFocusAuto = focusAutoPaths(el)
                        // Unit 1: capture enter old-set + prepare leave ghosts (rect+clone) BEFORE the DOM mutates.
                        val oldEnter = enterPaths(el)
                        val ghosts   = prepareLeaveGhosts(el, leaveSurvSet(finalHtml))
                        // Morph the existing subtree toward finalHtml instead of replacing it, so focus/caret/scroll/
                        // in-flight transitions on reused nodes survive the update. The surrounding capture-restore
                        // ceremony is retained: it now largely no-ops (identity is preserved) but stays correct. morph
                        // returns the region root, so the caller does not re-query the document for the path.
                        val updated = morph(el, finalHtml)
                        if updated != null then
                            applyJsPropsSync(updated)
                            beginAnimationsSync(updated)
                        if activePath != null then
                            restoreFocus(activePath, selStart, selEnd)
                        // Seeding runs AFTER restoreFocus so a newly-appeared focus-auto element wins over
                        // restore-to-trigger (that is the point: opening a panel moves focus into it).
                        if updated != null then
                            seedFocusAuto(updated, oldFocusAuto)
                        // Unit 1: animate newly-appeared enter elements, then spawn the leave ghosts captured above.
                        if updated != null then
                            seedEnter(updated, oldEnter)
                        spawnGhosts(ghosts)
                        // Portal upkeep: re-home freshly-inserted portal elements to document.body (after the
                        // subtree-scoped focus/enter seeding above) and retire body twins whose placeholder left.
                        if updated != null then
                            portalSweep(updated)
                        sweepFocusAuto()
                    end if
                }
            }
    end LocalExchange

    private def readSelection(el: dom.Element): (Maybe[Int], Maybe[Int]) =
        val dyn = el.asInstanceOf[scalajs.js.Dynamic]
        def asInt(v: scalajs.js.Dynamic): Maybe[Int] =
            if scalajs.js.typeOf(v) == "number" then Present(v.asInstanceOf[Int]) else Absent
        (asInt(dyn.selectionStart), asInt(dyn.selectionEnd))
    end readSelection

    private def restoreFocus(capturedPath: String, selStart: Maybe[Int], selEnd: Maybe[Int]): Unit =
        // Reactive wrapper spans share their data-kyo-path with the first element they wrap,
        // so a plain path query can hit the (unfocusable) wrapper — prefer the real element.
        val located = locateByPath(capturedPath)
        if located != null then
            val focusTarget =
                if located.hasAttribute("data-kyo-reactive") then
                    val inner = located.querySelector("input,textarea,select,[contenteditable]")
                    if inner != null then inner else located
                else located
            val _ = focusTarget.asInstanceOf[scalajs.js.Dynamic].focus()
            (selStart, selEnd) match
                case (Present(s), Present(e)) =>
                    val dyn = focusTarget.asInstanceOf[scalajs.js.Dynamic]
                    if scalajs.js.typeOf(dyn.setSelectionRange) == "function" then
                        try
                            val _ = dyn.setSelectionRange(s, e)
                        catch
                            // setSelectionRange throws InvalidStateError on input types that do not
                            // support text selection (e.g. email, number). Mirrors HtmlRenderer.clientJs:583:
                            // `catch(e){if(e.name!=='InvalidStateError')throw e;}`. Re-throw any other
                            // JS exception so genuine failures are not silently dropped.
                            case ex: scalajs.js.JavaScriptException
                                if ex.exception.asInstanceOf[scalajs.js.Dynamic].name.asInstanceOf[String] == "InvalidStateError" =>
                                ()
                    end if
                case _ => ()
            end match
        end if
    end restoreFocus

    /** The element at `path`, preferring a non-wrapper match: reactive wrapper spans share their
      * `data-kyo-path` with the first element they wrap, so a plain first-match query can return
      * the transparent (unfocusable) wrapper instead of the real element.
      */
    private def locateByPath(path: String): dom.Element =
        val direct = document.querySelector(s"""[data-kyo-path="$path"]:not([data-kyo-reactive])""")
        if direct != null then direct else document.querySelector(s"""[data-kyo-path="$path"]""")

    /** The set of `data-kyo-path` values of every `data-kyo-focus-auto` element inside `root`, `root` itself included. */
    private def focusAutoPaths(root: dom.Element): Set[String] =
        val els = root.querySelectorAll("[data-kyo-focus-auto]")
        val descendants = (0 until els.length).flatMap { i =>
            Maybe(els(i).asInstanceOf[dom.Element].getAttribute("data-kyo-path")).toList
        }.toSet
        if root.hasAttribute("data-kyo-focus-auto") && root.hasAttribute("data-kyo-path") then
            descendants + root.getAttribute("data-kyo-path")
        else descendants
    end focusAutoPaths

    /** Seed the FIRST `data-kyo-focus-auto` element under `newRoot` whose path is not in `oldSet` (i.e. it newly
      * appeared): record the previously focused element's path plus the focus-restore flag on the stack, then call
      * `.focus()` on it. Mirrors `faSeed` in HtmlRenderer.clientJs.
      */
    private def seedFocusAuto(newRoot: dom.Element, oldSet: Set[String]): Unit =
        val els = newRoot.querySelectorAll("[data-kyo-focus-auto]")
        val candidates =
            (if newRoot.hasAttribute("data-kyo-focus-auto") then Seq(newRoot) else Seq.empty) ++
                (0 until els.length).map(els(_).asInstanceOf[dom.Element])
        candidates.find { el =>
            val p = el.getAttribute("data-kyo-path")
            p != null && !oldSet.contains(p)
        }.foreach { el =>
            val ae = document.activeElement
            val ret =
                if ae != null && (ae ne document.body) then Maybe(ae.getAttribute("data-kyo-path"))
                else Absent
            focusReturnStack =
                (el.getAttribute("data-kyo-path"), ret, el.hasAttribute("data-kyo-focus-restore")) :: focusReturnStack
            val _ = el.asInstanceOf[scalajs.js.Dynamic].focus()
        }
    end seedFocusAuto

    /** Pop stack entries whose seeded focus-auto element left the document; when the entry carried
      * `data-kyo-focus-restore`, focus the recorded previous element (skip silently if it is gone too).
      * Mirrors `frSweep` in HtmlRenderer.clientJs.
      */
    private def sweepFocusAuto(): Unit =
        var continue = true
        while continue do
            focusReturnStack match
                case (faPath, ret, restore) :: rest =>
                    if document.querySelector(s"""[data-kyo-path="$faPath"][data-kyo-focus-auto]""") != null then
                        continue = false
                    else
                        focusReturnStack = rest
                        if restore then
                            ret.foreach { retPath =>
                                val re = locateByPath(retPath)
                                if re != null then
                                    val _ = re.asInstanceOf[scalajs.js.Dynamic].focus()
                            }
                        end if
                case Nil => continue = false
        end while
    end sweepFocusAuto

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
        val propPrefix = "data-kyo-prop-"
        // CSS has no attribute-name-prefix selector, so `[data-kyo-prop-*]` is not a valid selector and
        // throws SyntaxError. Collect the root plus every descendant and keep those carrying any
        // data-kyo-prop-* attribute; the apply loop reads the prop name off each attribute.
        val elements = root.querySelectorAll("*")
        val self =
            if hasAnyKyoProp(root) then
                Seq(root)
            else
                Seq.empty
        (self ++ (0 until elements.length).map(elements(_).asInstanceOf[dom.Element])).foreach { el =>
            val attrNames = (0 until el.attributes.length).map(el.attributes(_).name)
            val toRemove  = attrNames.filter(_.startsWith(propPrefix))
            toRemove.foreach { attrName =>
                val propName = attrName.stripPrefix(propPrefix)
                val value    = el.getAttribute(attrName)
                el.asInstanceOf[scalajs.js.Dynamic].updateDynamic(propName)(value)
            }
            toRemove.foreach(el.removeAttribute)
        }
    end applyJsPropsSync

    private def hasAnyKyoProp(el: dom.Element): Boolean =
        (0 until el.attributes.length).exists(i => el.attributes(i).name.startsWith("data-kyo-prop-"))

    /** Start every freshly-inserted SMIL animation under `root`.
      *
      * Chart transition `<animate>` elements use `begin="indefinite"` so they do not auto-play against the
      * shared SVG document timeline (which would make a post-load update snap to the frozen `to` value).
      * Calling `beginElement()` after the node is inserted starts the tween relative to now. The call is
      * deferred one animation frame so the SMIL engine has registered the newly inserted elements; a node
      * that was already replaced again by then throws and is ignored.
      */
    private def beginAnimationsSync(root: dom.Element): Unit =
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

    // ---- DOM morphing: patch an existing subtree in place toward new HTML, preserving element identity ----
    //
    // Replacing a reactive region with `el.outerHTML = html` discards every DOM-local state the browser keeps on the
    // nodes: focus, caret/selection, scroll offset, in-flight CSS transitions and `setPointerCapture` sessions. Morphing
    // reuses the existing nodes and patches only what differs, so that state survives an update.
    //
    // Reconciliation is SIBLING-SCOPED and keyed on `data-kyo-path`: within one parent's child list a `data-kyo-path`
    // is unique (Foreach items get `path :+ key`, element children `path :+ i`), so it is a stable per-child key that
    // survives reordering. Across the wrapper/inner nesting the SAME path repeats at different levels, so the keyed
    // lookup is rebuilt per child list and is never global — a document-wide id set (Idiomorph's optimization) would
    // mis-match a wrapper against its own child. Twin of `__kyoMorph` in HtmlRenderer.clientJs; keep the two in lockstep.
    private val SvgNs = "http://www.w3.org/2000/svg"

    /** The reconciliation key of a node: its `data-kyo-path` when it is an element carrying one, else its
      * `data-kyo-portal-slot` when it is a portal placeholder (the slot stands in for the re-homed element at the
      * element's logical position, so it must key as that element), else null (text, comment, and unkeyed elements
      * reconcile positionally).
      */
    private def kyoKey(node: dom.Node): String =
        if node.nodeType == 1 then
            val el = node.asInstanceOf[dom.Element]
            if el.hasAttribute("data-kyo-path") then el.getAttribute("data-kyo-path")
            else if el.hasAttribute("data-kyo-portal-slot") then el.getAttribute("data-kyo-portal-slot")
            else null
        else null

    /** A live keyless-mount content root, flagged by the mount's own paint (data-kyo-mount). A parent region's morph
      * reuses and repositions such a node but never reconciles its subtree — the mount owns it and repaints it in
      * place — even though the parent's top-down HTML shows only the mount placeholder at that path.
      */
    private def isMountRoot(node: dom.Node): Boolean =
        node.nodeType == 1 && node.asInstanceOf[dom.Element].hasAttribute("data-kyo-mount")

    /** A top-down mount placeholder produced by a client region re-render (data-kyo-mount-slot). Positive
      * signal that the incoming content at this slot IS a mount — as opposed to unrelated reactive content
      * that merely collided on the positional data-kyo-path key. Twin of `__kyoIsMountSlot` in clientJs.
      */
    private def isMountSlot(node: dom.Node): Boolean =
        node.nodeType == 1 && node.asInstanceOf[dom.Element].hasAttribute("data-kyo-mount-slot")

    /** A live portal placeholder (data-kyo-portal-slot="<path>") left at the logical position of an element the
      * client re-homed to document.body. Carries NO data-kyo-path of its own, so the body twin stays the document's
      * only carrier of the path (every path-addressed lookup keeps hitting the real element); `kyoKey` keys the slot
      * by the slot attribute so the sibling reconciliation still matches the incoming portal element against it.
      * Twin of `__kyoIsPortalSlot` in clientJs.
      */
    private def isPortalSlot(node: dom.Node): Boolean =
        node.nodeType == 1 && node.asInstanceOf[dom.Element].hasAttribute("data-kyo-portal-slot")

    /** An element that declared `portal(true)` (data-kyo-portal) — rendered inline by the server, re-homed to
      * document.body by [[portalSweep]]. Twin of `__kyoIsPortal` in clientJs.
      */
    private def isPortalEl(node: dom.Node): Boolean =
        node.nodeType == 1 && node.asInstanceOf[dom.Element].hasAttribute("data-kyo-portal")

    /** The body twin of a portal slot: the (unique) re-homed element carrying `path`, a direct body child. */
    private def portalTwin(path: String): dom.Element =
        document.querySelector(s"""body > [data-kyo-path="$path"][data-kyo-portal]""")

    /** Parse a single-wrapper HTML string to a detached root element. In SVG context the fragment is parsed inside a
      * throwaway `<svg>` so the parser produces real SVG elements (a bare `<g>` in an HTML template becomes an unknown
      * HTML element) — mirroring how `el.outerHTML =` parses against the live element's namespace.
      */
    private def firstElementChildOf(parent: dom.Node): dom.Element =
        var c = parent.firstChild
        while c != null && c.nodeType != 1 do c = c.nextSibling
        if c == null then null else c.asInstanceOf[dom.Element]
    end firstElementChildOf

    private def parseNewRoot(fromEl: dom.Element, html: String): dom.Element =
        val tpl = document.createElement("template").asInstanceOf[dom.HTMLTemplateElement]
        val svg = fromEl.namespaceURI == SvgNs && fromEl.tagName.toLowerCase != "svg"
        val root =
            if svg then
                tpl.innerHTML = "<svg>" + html + "</svg>"
                val outer = firstElementChildOf(tpl.content)
                if outer == null then null else firstElementChildOf(outer)
            else
                tpl.innerHTML = html
                firstElementChildOf(tpl.content)
        if root != null then collapseReactiveWrappers(root)
        root
    end parseNewRoot

    /** True when the FIRST tag of `html` carries data-kyo-mount-slot — i.e. the region's new content root is a
      * mount placeholder (the SAME mount re-rendering). Bounded to the opening tag so a descendant marker deeper
      * in the string can't false-match, and it avoids allocating a trimmed copy of a possibly-large html (the old
      * `html.trim.nonEmpty` did).
      */
    private def htmlRootHasMountSlot(html: String): Boolean =
        val i = html.indexOf('<')
        if i < 0 then false
        else
            val j = html.indexOf('>', i)
            j > i && html.substring(i, j).contains("data-kyo-mount-slot")
        end if
    end htmlRootHasMountSlot

    private def onlyElementChild(el: dom.Element): dom.Element =
        var found: dom.Element = null
        var count              = 0
        var c                  = el.firstChild
        while c != null do
            if c.nodeType == 1 then
                count += 1
                found = c.asInstanceOf[dom.Element]
            c = c.nextSibling
        end while
        if count == 1 then found else null
    end onlyElementChild

    /** Collapse a chain of transparent same-path reactive wrappers to a single wrapper. Nested reactive regions at one
      * position (e.g. `open.render { hi.render { q.render { … } } }`) each emit a `<span data-kyo-path=P data-kyo-reactive>`
      * around the same content, and a given region's paint carries only its own depth — so consecutive paints at path P
      * arrive with different wrapper counts. Left as-is, morphing a shallow paint against a deep live subtree (or vice
      * versa) forces a tag mismatch that replaces the whole subtree. The wrappers are `display: contents`, so collapsing
      * them changes nothing visible and makes every paint at P present the same depth. A mount wrapper is never collapsed
      * (its marker is load-bearing).
      */
    private def collapseReactiveWrappers(el: dom.Element): Unit =
        if el.hasAttribute("data-kyo-reactive") && !el.hasAttribute("data-kyo-mount") then
            val p    = el.getAttribute("data-kyo-path")
            var cont = true
            while cont do
                val child = onlyElementChild(el)
                if child != null && child.hasAttribute("data-kyo-reactive") &&
                    child.getAttribute("data-kyo-path") == p && !child.hasAttribute("data-kyo-mount")
                then
                    while child.firstChild != null do discard(el.insertBefore(child.firstChild, child))
                    discard(el.removeChild(child))
                else cont = false
                end if
            end while
        end if
        var c = el.firstChild
        while c != null do
            val next = c.nextSibling
            if c.nodeType == 1 then collapseReactiveWrappers(c.asInstanceOf[dom.Element])
            c = next
        end while
    end collapseReactiveWrappers

    /** Patch `fromEl` in place toward `newHtml` (a single reactive-wrapper element string) and return the resulting
      * region root — `fromEl` itself when identity is preserved (the common case), or the replacement node when a root
      * tag change forced a replace. The caller reuses this instead of re-querying the document for the path.
      */
    private def morph(fromEl: dom.Element, newHtml: String): dom.Element =
        val toEl = parseNewRoot(fromEl, newHtml)
        if toEl == null then fromEl
        else
            // Normalize the live side the same way the parsed side was normalized, so a subtree that arrived from the
            // server render (or an earlier paint) with redundant nested same-path wrappers aligns instead of forcing a
            // whole-subtree replace.
            collapseReactiveWrappers(fromEl)
            if fromEl.tagName != toEl.tagName then
                val replacement = document.importNode(toEl, true).asInstanceOf[dom.Element]
                discard(fromEl.parentNode.replaceChild(replacement, fromEl))
                replacement
            else
                morphEl(fromEl, toEl)
                fromEl
            end if
        end if
    end morph

    private def morphNode(fromNode: dom.Node, toNode: dom.Node): Unit =
        if toNode.nodeType != 1 then
            if fromNode.nodeValue != toNode.nodeValue then fromNode.nodeValue = toNode.nodeValue
        else
            val fromEl = fromNode.asInstanceOf[dom.Element]
            val toEl   = toNode.asInstanceOf[dom.Element]
            if fromEl.tagName != toEl.tagName then
                discard(fromEl.parentNode.replaceChild(document.importNode(toEl, true), fromEl))
            else morphEl(fromEl, toEl)
            end if
    end morphNode

    private def morphEl(fromEl: dom.Element, toEl: dom.Element): Unit =
        morphAttrs(fromEl, toEl)
        // A focused contenteditable would lose its caret if its children were rewritten mid-edit; leave its subtree
        // alone (INPUT/TEXTAREA have no element children, so they need no such guard).
        val editing = (fromEl eq document.activeElement) && fromEl.hasAttribute("contenteditable")
        if !editing then morphChildren(fromEl, toEl)
    end morphEl

    private def morphAttrs(fromEl: dom.Element, toEl: dom.Element): Unit =
        // An attribute the imperative id-addressed channel owns (its name is in the element's `__kyoOwn` expando dict)
        // is never reconciled: server HTML never carries the client-set value, so reconciling would clobber it.
        val own                         = fromEl.asInstanceOf[js.Dynamic].__kyoOwn
        val ownDict                     = if js.isUndefined(own) then null else own.asInstanceOf[js.Dictionary[Boolean]]
        def owns(name: String): Boolean = ownDict != null && ownDict.contains(name)
        val tag                         = fromEl.tagName
        val activeInput =
            (fromEl eq document.activeElement) && (tag == "INPUT" || tag == "TEXTAREA")
        val toAttrs = toEl.attributes
        var i       = 0
        while i < toAttrs.length do
            val a    = toAttrs(i)
            val name = a.name
            if !owns(name) && fromEl.getAttribute(name) != a.value then
                fromEl.setAttribute(name, a.value)
            i += 1
        end while
        // Remove attributes gone from `to`. Walk the live NamedNodeMap backward so an in-place removal never shifts an
        // index still to be visited — no intermediate collection is allocated per element.
        val fromAttrs = fromEl.attributes
        var j         = fromAttrs.length - 1
        while j >= 0 do
            val name = fromAttrs(j).name
            if !owns(name) && !toEl.hasAttribute(name) then
                fromEl.removeAttribute(name)
            j -= 1
        end while
        // Active-input preservation: two-way binding echoes each keystroke back as a re-render. Never overwrite the
        // focused field's live `.value` (and thus its caret) with its own echo — that value already matches, so assign
        // only a genuine external change (submit-clear, programmatic update). Subsumes the HtmlRenderer.clientJs input hack.
        if activeInput then
            val nv =
                if tag == "TEXTAREA" then toEl.textContent
                else
                    val v = toEl.getAttribute("value")
                    if v == null then "" else v
            val dyn = fromEl.asInstanceOf[scalajs.js.Dynamic]
            if nv != dyn.value.asInstanceOf[String] then dyn.value = nv
        end if
    end morphAttrs

    private def morphChildren(fromParent: dom.Element, toParent: dom.Element): Unit =
        // Sibling-scoped keyed lookups over the DIRECT children only (keys are unique among siblings). `toKeyed`
        // records which stale from-children are reused elsewhere so an unkeyed slot does not destroy them. Both
        // dictionaries are allocated lazily on the first keyed child seen — leaf-ish subtrees (the majority) carry
        // no `data-kyo-path` child and pay no allocation; a null dictionary means "no keyed children on that side".
        var fromKeyed: js.Dictionary[dom.Node] = null
        var toKeyed: js.Dictionary[Boolean]    = null
        var scan                               = fromParent.firstChild
        while scan != null do
            val k = kyoKey(scan)
            if k != null then
                if fromKeyed == null then fromKeyed = js.Dictionary.empty[dom.Node]
                fromKeyed(k) = scan
            scan = scan.nextSibling
        end while
        scan = toParent.firstChild
        while scan != null do
            val k = kyoKey(scan)
            if k != null then
                if toKeyed == null then toKeyed = js.Dictionary.empty[Boolean]
                toKeyed(k) = true
            scan = scan.nextSibling
        end while
        var curFrom = fromParent.firstChild
        var curTo   = toParent.firstChild
        while curTo != null do
            val toNext = curTo.nextSibling
            val tKey   = kyoKey(curTo)
            if tKey != null then
                val m = if fromKeyed != null then fromKeyed.get(tKey).orNull else null
                if m != null then
                    if m ne curFrom then discard(fromParent.insertBefore(m, curFrom))
                    else curFrom = curFrom.nextSibling
                    // Portal slot: the incoming portal element's live node was re-homed to document.body behind this
                    // placeholder. Reconcile the BODY TWIN in place (content updates flow to it) and keep the
                    // placeholder — materializing the incoming node inline would duplicate its data-kyo-path. A lost
                    // twin (defensive) falls back to re-materializing inline; the post-patch portalSweep re-homes it.
                    if isPortalSlot(m) && isPortalEl(curTo) then
                        val twin = portalTwin(kyoKey(curTo))
                        if twin != null then morphNode(twin, curTo)
                        else discard(fromParent.replaceChild(document.importNode(curTo, true), m))
                    // Opaque mount boundary: leave a live mount root's subtree alone ONLY when the incoming top-down
                    // node is itself a mount slot (data-kyo-mount-slot) — the SAME mount re-rendering, which owns and
                    // repaints its own subtree, so reconciling against the placeholder would wipe the panel. Otherwise
                    // the slot now holds different content that merely collided on the positional key (or an empty
                    // gate-closed slot): reconcile normally so the stale mount is replaced/emptied and its
                    // data-kyo-mount stripped.
                    else if !(isMountRoot(m) && isMountSlot(curTo)) then morphNode(m, curTo)
                    end if
                else
                    discard(fromParent.insertBefore(document.importNode(curTo, true), curFrom))
                end if
            else
                var handled = false
                var loop    = true
                while loop && curFrom != null do
                    val fNext = curFrom.nextSibling
                    val fKey  = kyoKey(curFrom)
                    if fKey != null then
                        // A keyed from-child sitting at an unkeyed slot: keep it if `to` reuses it (its own slot will
                        // move it into place), otherwise it is stale and removed here. A null toKeyed means `to` has no
                        // keyed child at all, so this one is unquestionably stale.
                        if toKeyed == null || !toKeyed.contains(fKey) then discard(fromParent.removeChild(curFrom))
                        curFrom = fNext
                    else if compatible(curFrom, curTo) then
                        morphNode(curFrom, curTo)
                        curFrom = fNext
                        handled = true
                        loop = false
                    else
                        discard(fromParent.removeChild(curFrom))
                        curFrom = fNext
                    end if
                end while
                if !handled then discard(fromParent.insertBefore(document.importNode(curTo, true), curFrom))
            end if
            curTo = toNext
        end while
        while curFrom != null do
            val fNext = curFrom.nextSibling
            discard(fromParent.removeChild(curFrom))
            curFrom = fNext
        end while
    end morphChildren

    /** Two nodes may be patched into each other positionally: same node kind, and for elements the same tag. */
    private def compatible(a: dom.Node, b: dom.Node): Boolean =
        a.nodeType == b.nodeType &&
            (a.nodeType != 1 || a.asInstanceOf[dom.Element].tagName == b.asInstanceOf[dom.Element].tagName)

    /** Set up capture-phase event delegation on document.body. */
    private def setupEventDelegation(dispatch: (Seq[String], UIEvent) => Boolean < Async, events: Channel[Unit < Async])(using
        Frame
    ): Unit < Sync = Sync.defer {
        // Bubbling events are forwarded when ANY ancestor declared the handler (a keydown on a
        // panel's filter input must reach the panel's own onKeyDown). Mirrors `he(el,t)` in
        // HtmlRenderer.clientJs — checking only the target's own data-kyo-ev drops those events
        // at the JS boundary before the server-side bubble dispatch can ever run.
        def declaredInChain(start: dom.Element, t: String): Boolean =
            var n: dom.Element = start
            var found          = false
            while !found && n != null && (n ne document.body) do
                val ev = n.getAttribute("data-kyo-ev")
                if ev != null && ev.split(",").contains(t) then found = true
                // Portal hop: a re-homed element's DOM parent is <body>; continue the walk at its LOGICAL parent
                // (the placeholder slot's parent) so handlers declared on logical ancestors still forward. Server
                // dispatch bubbles over the logical tree anyway — this keeps the client gate consistent with it.
                else if n.hasAttribute("data-kyo-portal") && (n.parentNode eq document.body) then
                    val p    = n.getAttribute("data-kyo-path")
                    val slot = if p != null then document.querySelector(s"""[data-kyo-portal-slot="$p"]""") else null
                    n =
                        if slot == null then null
                        else
                            slot.parentNode match
                                case pe: dom.Element => pe
                                case _               => null
                else
                    n = n.parentNode match
                        case p: dom.Element => p
                        case _              => null
                end if
            end while
            found
        end declaredInChain

        final class ChainTypes(target: dom.Element):
            def contains(t: String): Boolean = declaredInChain(target, t)

        val handler: scalajs.js.Function1[dom.Event, Unit] = (e: dom.Event) =>
            // preventScrollKeys: suppress the browser's native page-scroll for navigation keys inside a region that
            // declared `preventScrollKeys` (data-kyo-scroll-keys). Runs before path resolution so it fires even when
            // the focused descendant is not itself a path element; the keydown is still forwarded below so the
            // region's onKeyDown (highlight move) runs. Mirrors PrimeReact's per-handler preventDefault.
            if e.`type` == "keydown" then
                val ke  = e.asInstanceOf[dom.KeyboardEvent]
                val tgt = e.target.asInstanceOf[dom.Element]
                if tgt != null && scrollKeyPrevented(ke.key, tgt) && tgt.closest("[data-kyo-scroll-keys]") != null then
                    e.preventDefault()
            end if
            findPathElement(e.target.asInstanceOf[dom.Element]).foreach { target =>
                val path    = parsePath(target.getAttribute("data-kyo-path"))
                val evTypes = ChainTypes(target)
                val t       = e.`type`

                val event: Maybe[UIEvent] =
                    if t == "click" then
                        val targetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val me       = e.asInstanceOf[dom.MouseEvent]
                        val mouse = MouseEventData(
                            modifiers = UI.Modifiers(me.ctrlKey, me.altKey, me.shiftKey, me.metaKey),
                            targetId = targetId
                        )
                        // Prevent the browser's default navigation only when the anchor carries a kyo
                        // click handler (so the handler, not the href, drives the action). A plain href
                        // keeps native behavior: an in-page `#anchor` scrolls, and a cross-document route
                        // is handled by UILocation's interceptor. Prevent-defaulting every anchor here
                        // would also kill those.
                        if target.tagName.toLowerCase == "a" && evTypes.contains("click") then e.preventDefault()
                        Present(UIEvent.Click(path, mouse))
                    else if t == "contextmenu" && evTypes.contains("contextmenu") then
                        // Suppress the native context menu only when an ancestor declared a contextmenu
                        // handler (this branch); a bare right-click elsewhere keeps the browser's own menu.
                        e.preventDefault()
                        val targetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val me       = e.asInstanceOf[dom.MouseEvent]
                        Present(UIEvent.ContextMenu(
                            path,
                            MouseEventData(
                                modifiers = UI.Modifiers(me.ctrlKey, me.altKey, me.shiftKey, me.metaKey),
                                targetId = targetId
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
                                // Unit 2: read ALL files (a FileReader each, indexed so order is preserved), post one
                                // FileSelect once every read completes.
                                val n = files.length
                                if n > 0 then
                                    val results = scala.collection.mutable.ArrayBuffer.fill[Maybe[UI.FilePayload]](n)(Absent)
                                    var doneCt  = 0
                                    var i       = 0
                                    while i < n do
                                        val ix     = i
                                        val f      = files(ix)
                                        val reader = new dom.FileReader()
                                        reader.onload = (_: dom.Event) =>
                                            val content = reader.result.asInstanceOf[String]
                                            results(ix) = Present(UI.FilePayload(f.name, f.size.toLong, f.`type`, content))
                                            doneCt += 1
                                            if doneCt == n then
                                                val payloads = results.toSeq.collect { case Present(p) => p }
                                                fireFromJs(events, dispatch(path, UIEvent.FileSelect(path, payloads)).unit)
                                        reader.readAsText(f)
                                        i += 1
                                    end while
                                end if
                                Absent
                            else
                                // Legacy: first file's text -> Change (byte-compatible content-only).
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
                        val submitTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val submitMouse = MouseEventData(
                            modifiers = UI.Modifiers.none,
                            targetId = submitTargetId
                        )
                        Present(UIEvent.Submit(path, submitMouse))
                    else if t == "keydown" && evTypes.contains("keydown") then
                        val ke         = e.asInstanceOf[dom.KeyboardEvent]
                        val kdTargetId = Maybe(ke.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        Present(UIEvent.KeyDown(
                            path,
                            KeyboardEventData(
                                key = ke.key,
                                modifiers = UI.Modifiers(ke.ctrlKey, ke.altKey, ke.shiftKey, ke.metaKey),
                                targetId = kdTargetId
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
                        val blurTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        Present(UIEvent.Blur(path, MouseEventData(UI.Modifiers.none, blurTargetId)))
                    else if t == "mouseover" && evTypes.contains("mouseover") then
                        val hoverTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val me            = e.asInstanceOf[dom.MouseEvent]
                        Present(UIEvent.Hover(
                            path,
                            MouseEventData(
                                modifiers = UI.Modifiers(me.ctrlKey, me.altKey, me.shiftKey, me.metaKey),
                                targetId = hoverTargetId
                            )
                        ))
                    else if t == "mouseout" && evTypes.contains("mouseout") then
                        val unhoverTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val me              = e.asInstanceOf[dom.MouseEvent]
                        Present(UIEvent.Unhover(
                            path,
                            MouseEventData(
                                modifiers = UI.Modifiers(me.ctrlKey, me.altKey, me.shiftKey, me.metaKey),
                                targetId = unhoverTargetId
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

        Seq("click", "contextmenu", "input", "change", "submit", "keydown", "keyup", "focus", "blur", "mouseover", "mouseout").foreach {
            t =>
                document.body.addEventListener(t, handler, true)
        }
        document.body.addEventListener(
            "wheel",
            handler,
            js.Dynamic.literal(capture = true, passive = false).asInstanceOf[dom.EventListenerOptions]
        )
        // Native scroll POSITION (onScrollPosition): a separate rAF-coalesced listener so a scroll burst collapses to
        // one dispatch per frame. Scroll does not bubble, but the capture-phase body listener catches descendant
        // viewport scrolls; a page-level scroll (target has no data-kyo-path) resolves to Absent and is ignored.
        var scrRaf               = 0
        var scrEl: dom.Element   = null
        var scrPath: Seq[String] = Seq.empty
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
        document.body.addEventListener(
            "scroll",
            scrollHandler,
            js.Dynamic.literal(capture = true, passive = true).asInstanceOf[dom.EventListenerOptions]
        )
    }
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

    /** True when `key` is a page-scrolling navigation key a `preventScrollKeys` region should suppress on `target`.
      * Vertical keys are always suppressed; horizontal/edge keys only when `target` is not text-editable, so a filter
      * input keeps caret movement.
      */
    private def scrollKeyPrevented(key: String, target: dom.Element): Boolean =
        def editable =
            val tag = target.tagName
            tag == "INPUT" || tag == "TEXTAREA" || tag == "SELECT" ||
            target.asInstanceOf[scalajs.js.Dynamic].isContentEditable.asInstanceOf[Boolean]
        end editable
        key match
            case "ArrowUp" | "ArrowDown" | "PageUp" | "PageDown" => true
            case "ArrowLeft" | "ArrowRight" | "Home" | "End"     => !editable
            case _                                               => false
        end match
    end scrollKeyPrevented

    // ---- Unit 1: pointer/drag delegation (SPA transport) ----

    // Drag-session state. Module-level mutable is safe on the single-threaded JS runtime (mutated only inside JS
    // event callbacks). A session is active between a pointerdown on a declaring element and its pointerup.
    private var ptrActive: Boolean             = false
    private var ptrEl: dom.Element             = null
    private var ptrPath: Seq[String]           = Seq.empty
    private var ptrRaf: Int                    = 0
    private var ptrPendingEv: dom.PointerEvent = null

    /** True if `start` or any ancestor up to (not including) body declares event token `t` in its data-kyo-ev.
      * A body-re-homed portal element hops to its LOGICAL parent (its placeholder slot's parent) — mirrors
      * `declaredInChain` in setupEventDelegation; keep the two in lockstep.
      */
    private def declaredInChainAt(start: dom.Element, t: String): Boolean =
        var n: dom.Element = start
        var found          = false
        while !found && n != null && (n ne document.body) do
            val ev = n.getAttribute("data-kyo-ev")
            if ev != null && ev.split(",").contains(t) then found = true
            else if n.hasAttribute("data-kyo-portal") && (n.parentNode eq document.body) then
                val p    = n.getAttribute("data-kyo-path")
                val slot = if p != null then document.querySelector(s"""[data-kyo-portal-slot="$p"]""") else null
                n =
                    if slot == null then null
                    else
                        slot.parentNode match
                            case pe: dom.Element => pe
                            case _               => null
            else
                n = n.parentNode match
                    case p: dom.Element => p
                    case _              => null
            end if
        end while
        found
    end declaredInChainAt

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
        events: Channel[Unit < Async]
    )(using Frame): Unit < Sync = Sync.defer {
        val down: scalajs.js.Function1[dom.Event, Unit] = (e0: dom.Event) =>
            val e = e0.asInstanceOf[dom.PointerEvent]
            findPathElement(e.target.asInstanceOf[dom.Element]).foreach { el =>
                if declaredInChainAt(el, "pointerdown") then
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

        document.body.addEventListener("pointerdown", down, true)
        document.body.addEventListener("pointermove", move, true)
        document.body.addEventListener("pointerup", up, true)
    }
    end setupPointerDelegation

    // ---- Unit 1: enter/leave transition mirror (SPA transport) ----

    /** The set of `data-kyo-path` values of every `data-kyo-enter` element inside `root`, `root` itself included. */
    private def enterPaths(root: dom.Element): Set[String] =
        val els = root.querySelectorAll("[data-kyo-enter]")
        val ds = (0 until els.length).flatMap { i =>
            Maybe(els(i).asInstanceOf[dom.Element].getAttribute("data-kyo-path")).toList
        }.toSet
        if root.hasAttribute("data-kyo-enter") && root.hasAttribute("data-kyo-path") then
            ds + root.getAttribute("data-kyo-path")
        else ds
    end enterPaths

    /** Animate every `data-kyo-enter` element under `newRoot` (root included) whose path is not in `oldSet`: add the enter
      * classes, force a reflow, then remove them next frame so the CSS transition runs from the enter-from state.
      */
    private def seedEnter(newRoot: dom.Element, oldSet: Set[String]): Unit =
        val els = newRoot.querySelectorAll("[data-kyo-enter]")
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
    end seedEnter

    /** The set of paths of `data-kyo-leave` elements in an HTML fragment (which leave-elements survive a region
      * replace). Keyed on leave-carrying elements, NOT all `data-kyo-path`: a reactive wrapper span shares its path
      * with the (leaving) element it wraps, so an all-path set would wrongly report the element as surviving.
      */
    private def leaveSurvSet(html: String): Set[String] =
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
      * Captures rect + clone WHILE the node is still in the DOM; returns (ghostNode, leaveClasses) descriptors.
      */
    private def prepareLeaveGhosts(root: dom.Element, surv: Set[String]): Seq[(dom.Element, String)] =
        val els = root.querySelectorAll("[data-kyo-leave]")
        val cand =
            (if root.getAttribute("data-kyo-leave") != null then Seq(root) else Seq.empty) ++
                (0 until els.length).map(els(_).asInstanceOf[dom.Element])
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
            // The clone still carries the source's classes; a percentage min/max-width
            // (e.g. an overlay panel's `min-width: 100%`) would resolve against <body>
            // (the ghost's new parent) = the viewport and OVERRIDE the pinned width,
            // ballooning the ghost full-width. Neutralize so the rect pin holds.
            st.minWidth = "0"
            st.maxWidth = "none"
            st.minHeight = "0"
            st.maxHeight = "none"
            st.margin = "0"
            st.pointerEvents = "none"
            g.setAttribute("data-kyo-ghost", "1")
            (g, if leave == null then "" else leave)
        }
    end prepareLeaveGhosts

    /** Append prepared ghosts to `<body>`, add their leave classes next frame, remove on transitionend/animationend or a 1s safety. */
    private def spawnGhosts(ghosts: Seq[(dom.Element, String)]): Unit =
        ghosts.foreach { case (g, leave) =>
            discard(document.body.appendChild(g))
            val cls     = leave.split("\\s+").filter(_.nonEmpty)
            val clsList = g.asInstanceOf[scalajs.js.Dynamic].classList
            discard(dom.window.requestAnimationFrame((_: Double) => cls.foreach(c => clsList.add(c))))
            var done = false
            def cleanup(): Unit =
                if !done then
                    done = true
                    if g.parentNode != null then discard(g.parentNode.removeChild(g))
            val listener: scalajs.js.Function1[dom.Event, Unit] = (_: dom.Event) => cleanup()
            g.addEventListener("transitionend", listener)
            g.addEventListener("animationend", listener)
            val to: scalajs.js.Function0[Unit] = () => cleanup()
            discard(dom.window.setTimeout(to, 1000.0))
        }
    end spawnGhosts

    /** Portal upkeep after a patch (twin of `__kyoPortalSweep` in HtmlRenderer.clientJs). Two phases:
      *
      * ADOPT — every `data-kyo-portal` element still sitting inline under `root` (root included; freshly inserted by
      * this patch) is re-homed to document.body behind an inert placeholder stamped
      * `data-kyo-portal-slot="<path>"` at its logical position. Must run AFTER focus/enter seeding (both are
      * subtree-scoped and need the element inline). Reparenting drops DOM focus, so focus (and caret) held inside the
      * moved subtree is captured and re-applied after the move — the enter transition is unaffected because its
      * from-state classes are released on the NEXT frame, after the move.
      *
      * ORPHANS — a body twin whose placeholder is gone (its logical slot was removed or replaced by this patch)
      * leaves with its region: its leave ghost (if any) is spawned via the regular ghost machinery — which already
      * lives at document.body — and the twin is removed. Document-wide by necessity: the twin is outside every
      * region subtree, so the regular leave sweep cannot see it.
      */
    private def portalSweep(root: dom.Element): Unit =
        // Phase 1: adopt freshly-inserted inline portal elements.
        val els = root.querySelectorAll("[data-kyo-portal]")
        val cand =
            (if root.hasAttribute("data-kyo-portal") then Seq(root) else Seq.empty) ++
                (0 until els.length).map(els(_).asInstanceOf[dom.Element])
        cand.foreach { el =>
            val path = el.getAttribute("data-kyo-path")
            // Only path-carrying elements can portal (the placeholder must key the slot); the parent check keeps the
            // sweep idempotent (an adopted twin is a direct body child, never under a region root again).
            if path != null && el.parentNode != null && (el.parentNode ne document.body) then
                // A stale twin from a lost placeholder would collide on the path: drop it first (defensive).
                val stale = portalTwin(path)
                if stale != null && (stale ne el) then discard(document.body.removeChild(stale))
                val slot =
                    if el.namespaceURI == SvgNs then document.createElementNS(SvgNs, "g")
                    else document.createElement("span")
                slot.setAttribute("data-kyo-portal-slot", path)
                slot.setAttribute("hidden", "")
                val ae       = document.activeElement
                val hadFocus = ae != null && (ae ne document.body) && ((el eq ae) || el.contains(ae))
                val (selStart, selEnd) =
                    if hadFocus then readSelection(ae) else (Absent, Absent)
                discard(el.parentNode.insertBefore(slot, el))
                discard(document.body.appendChild(el))
                if hadFocus then
                    val dyn = ae.asInstanceOf[scalajs.js.Dynamic]
                    val _   = dyn.focus()
                    (selStart, selEnd) match
                        case (Present(s), Present(e)) if scalajs.js.typeOf(dyn.setSelectionRange) == "function" =>
                            try
                                val _ = dyn.setSelectionRange(s, e)
                            catch
                                case ex: scalajs.js.JavaScriptException
                                    if ex.exception
                                        .asInstanceOf[scalajs.js.Dynamic]
                                        .name
                                        .asInstanceOf[String] == "InvalidStateError" =>
                                    ()
                        case _ => ()
                    end match
                end if
            end if
        }
        // Phase 2: remove orphaned body twins (placeholder gone), playing their leave ghosts.
        val twins = document.querySelectorAll("body > [data-kyo-portal][data-kyo-path]")
        (0 until twins.length).foreach { i =>
            val twin = twins(i).asInstanceOf[dom.Element]
            val p    = twin.getAttribute("data-kyo-path")
            if document.querySelector(s"""[data-kyo-portal-slot="$p"]""") == null then
                spawnGhosts(prepareLeaveGhosts(twin, Set.empty))
                discard(document.body.removeChild(twin))
        }
    end portalSweep

    // ---- Unit 3: input filter/mask mirror (SPA transport) ----

    private def filterStr(pat: String, str: String, curVal: String): String =
        val sb     = new StringBuilder
        var hasSep = pat == "decimal" && (curVal.contains(".") || curVal.contains(","))
        str.foreach { ch =>
            if pat == "int" then
                if ch >= '0' && ch <= '9' then sb.append(ch)
            else if pat == "decimal" then
                if ch >= '0' && ch <= '9' then sb.append(ch)
                else if (ch == '.' || ch == ',') && !hasSep then
                    sb.append(ch); hasSep = true
            else if pat.contains(ch) then sb.append(ch)
        }
        sb.toString
    end filterStr

    private def maskClassAt(mask: String, idx: Int): Maybe[Char] =
        var c   = 0
        var i   = 0
        var res = Maybe.empty[Char]
        while i < mask.length && res.isEmpty do
            val mc = mask.charAt(i)
            if mc == '9' || mc == 'a' || mc == '*' then
                if c == idx then res = Present(mc)
                c += 1
            i += 1
        end while
        res
    end maskClassAt

    private def maskOk(cls: Char, ch: Char): Boolean =
        if cls == '9' then ch >= '0' && ch <= '9'
        else if cls == 'a' then (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
        else (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')

    private def maskFormat(mask: String, raw: String): String =
        val sb   = new StringBuilder
        var ri   = 0
        var i    = 0
        var stop = false
        while i < mask.length && !stop do
            val mc = mask.charAt(i)
            if mc == '9' || mc == 'a' || mc == '*' then
                if ri < raw.length then
                    sb.append(raw.charAt(ri)); ri += 1
                else stop = true
            else if ri < raw.length then sb.append(mc)
            else stop = true
            end if
            i += 1
        end while
        sb.toString
    end maskFormat

    private def maskRaw(mask: String, value: String): String =
        val sb = new StringBuilder
        var vi = 0
        var i  = 0
        while i < mask.length && vi < value.length do
            val mc = mask.charAt(i)
            if mc == '9' || mc == 'a' || mc == '*' then
                sb.append(value.charAt(vi)); vi += 1
            else if value.charAt(vi) == mc then vi += 1
            else
                sb.append(value.charAt(vi)); vi += 1
            end if
            i += 1
        end while
        sb.toString
    end maskRaw

    private def dispatchInput(t: dom.EventTarget): Unit =
        val ctor = scalajs.js.Dynamic.global.Event
        val ev   = scalajs.js.Dynamic.newInstance(ctor)("input", scalajs.js.Dynamic.literal(bubbles = true))
        discard(t.asInstanceOf[scalajs.js.Dynamic].dispatchEvent(ev))
    end dispatchInput

    private def setMasked(t: dom.html.Input, v: String): Unit =
        t.value = v
        val dyn = t.asInstanceOf[scalajs.js.Dynamic]
        if scalajs.js.typeOf(dyn.setSelectionRange) == "function" then
            try discard(dyn.setSelectionRange(v.length, v.length))
            catch case _: Throwable => ()
        dispatchInput(t)
    end setMasked

    private def setFilteredAt(t: dom.html.Input, txt: String, s: Int, e: Int): Unit =
        val v = t.value
        t.value = v.substring(0, s) + txt + v.substring(e)
        val np  = s + txt.length
        val dyn = t.asInstanceOf[scalajs.js.Dynamic]
        if scalajs.js.typeOf(dyn.setSelectionRange) == "function" then
            try discard(dyn.setSelectionRange(np, np))
            catch case _: Throwable => ()
        dispatchInput(t)
    end setFilteredAt

    private def setupInputMasking()(using Frame): Unit < Sync = Sync.defer {
        val handler: scalajs.js.Function1[dom.Event, Unit] = (e: dom.Event) =>
            val tRaw = e.target
            if tRaw != null then
                val el   = tRaw.asInstanceOf[dom.Element]
                val filt = el.getAttribute("data-kyo-filter")
                val mask = el.getAttribute("data-kyo-mask")
                if filt != null || mask != null then
                    val t   = tRaw.asInstanceOf[dom.html.Input]
                    val dyn = e.asInstanceOf[scalajs.js.Dynamic]
                    val it  = if scalajs.js.typeOf(dyn.inputType) == "string" then dyn.inputType.asInstanceOf[String] else ""
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
                    if filt != null then
                        if it.startsWith("delete") then ()
                        else if it == "insertText" || it == "insertReplacementText" || it == "insertCompositionText" then
                            if scalajs.js.typeOf(dyn.data) == "string" then
                                val ds = dyn.data.asInstanceOf[String]
                                val f1 = filterStr(filt, ds, t.value)
                                if f1 != ds then
                                    e.preventDefault()
                                    if f1.nonEmpty then setFilteredAt(t, f1, selStart, selEnd)
                        else if it == "insertFromPaste" || it == "insertFromDrop" then
                            e.preventDefault()
                            val f2 = filterStr(filt, transferText, t.value)
                            if f2.nonEmpty then setFilteredAt(t, f2, selStart, selEnd)
                        end if
                    else if mask != null then
                        if it.startsWith("delete") then
                            e.preventDefault()
                            val raw = maskRaw(mask, t.value)
                            val nr  = if raw.nonEmpty then raw.substring(0, raw.length - 1) else raw
                            setMasked(t, maskFormat(mask, nr))
                        else if it == "insertText" || it == "insertReplacementText" || it == "insertCompositionText" ||
                            it == "insertFromPaste" || it == "insertFromDrop"
                        then
                            e.preventDefault()
                            val ins = if it == "insertFromPaste" || it == "insertFromDrop" then transferText
                            else if scalajs.js.typeOf(dyn.data) == "string" then dyn.data.asInstanceOf[String]
                            else ""
                            var raw2 = maskRaw(mask, t.value)
                            var ci   = 0
                            var full = false
                            while ci < ins.length && !full do
                                maskClassAt(mask, raw2.length) match
                                    case Present(cls) =>
                                        val ch = ins.charAt(ci)
                                        if maskOk(cls, ch) then raw2 = raw2 + ch
                                    case Absent => full = true
                                end match
                                ci += 1
                            end while
                            setMasked(t, maskFormat(mask, raw2))
                        end if
                    end if
                end if
            end if
        document.body.addEventListener("beforeinput", handler, true)
    }
    end setupInputMasking

end DomBackend
