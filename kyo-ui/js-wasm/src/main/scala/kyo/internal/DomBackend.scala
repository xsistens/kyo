package kyo.internal

import kyo.*
import org.scalajs.dom
import org.scalajs.dom.document
import scala.annotation.tailrec
import scala.scalajs.js

/** Scala.js UI backend. Mounts a UI into the browser DOM. */
private[kyo] object DomBackend:

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

    /** The page-scoped drain channel, captured once per mount so the viewport scroll/resize listeners (raw JS
      * callbacks, outside any Kyo context) can bridge their `deliverMeasureById` effect back in via [[fireFromJs]].
      * Set in `mountInto` before any op can be emitted. Module-level mutable state is safe on the single-threaded runtime.
      */
    private var sessionEvents: Maybe[Channel[Unit < Async]] = Absent

    /** Live viewport observers for the SPA transport, keyed by element id. Each entry is the single handler
      * registered for BOTH `window` scroll (capture phase) and resize; Unobserve removes it from both and drops the
      * entry. Backed by a native `js.Map` (mirrors `UIMouseEventOps`), giving `contains`/`apply`/`update`/`remove`.
      */
    private val viewportObservers: js.WrappedMap[String, js.Function1[dom.Event, Unit]] =
        new js.WrappedMap(js.Map.empty[String, js.Function1[dom.Event, Unit]])

    /** Mark attribute `name` on `el` as owned by the imperative id-addressed channel (SetClassById/SetStyleById),
      * applied out of the render pass so CSS transitions on the toggled class/style fire. The owned names live in a
      * `__kyoOwn` expando dict ON the element, so the flag is reclaimed with the node (no session-lived set that only
      * ever grows) and `morphAttrs` shields each owned attribute BY NAME. Mirrors `__kyoMark` in HtmlRenderer.clientJs.
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
        mountInto(ui, document.body)

    /** Mount a UI into a specific DOM element selected by CSS selector. */
    def mount(ui: UI, selector: String)(using Frame): Unit < (Async & Scope) =
        Sync.defer {
            val target = document.querySelector(selector)
            if target == null then Abort.panic(UIException(s"Element not found: $selector"))
            else mountInto(ui, target.asInstanceOf[dom.Element])
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

    private def mountInto(ui: UI, container: dom.Element)(using Frame): Unit < (Async & Scope) =
        // Late-bound to break the emit<->Commands construction cycle (emit resolves measure callbacks via Commands,
        // Commands needs emit). Set before any op is emitted.
        var sessionCommands: UI.Commands = null
        for
            _    <- DomStyleSheet.injectBase()
            root <- ReactiveUI.normalize(ui, Seq.empty)
            // `mountSlot = true` on the BOOT render: this HTML goes straight into the live DOM and is never
            // a golden/SSR artifact, so the mount slots may carry their `s`/`k` flags from the very first
            // byte. Without them a keyed mount's region starts keyless, and the first parent re-render has
            // to fall through the opacity guard (rebuilding the live subtree once) just to adopt the key.
            html <- HtmlRenderer.render(ui, Seq.empty, mountSlot = true)
            _    <- Sync.defer(container.innerHTML = html)
            // Comment markers produce no node handles from an innerHTML assignment; one full scan
            // builds the path->range registry the patch path resolves against.
            _        <- Sync.defer { scanRoot = container; rebuildRegions() }
            _        <- applyJsProps(container)
            _        <- Sync.defer(seedEnter(container, Set.empty))
            _        <- Sync.defer(seedFocusAuto(container, Set.empty))
            _        <- Sync.defer(beginAnimationsSync(container))
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
                    _ = sessionEvents = Present(events)
                    // runPartial captures only the Closed failure (the channel closed on page teardown -> stop draining);
                    // a Panic propagates rather than being silently swallowed as a clean drain end. The drain carries the
                    // session's scroll sink: a handler calling UI.scrollIntoView scrolls the local document, the
                    // browser-mount counterpart of the server session's WebSocket op.
                    _ <- Fiber.init(UICommands.scrollSink.let(Present(scrollLocal)) {
                        Loop.foreach(Abort.runPartial[Closed](events.take).map {
                            case Result.Success(eff) => eff.andThen(Loop.continue)
                            case Result.Failure(_)   => Loop.done
                        })
                    })
                    _ <- setupEventDelegation(dispatch.handle, events)
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
    // so the same command behaves the same under either runner.
    private def scrollLocal(id: String)(using Frame): Unit < Async =
        Sync.defer {
            val el = document.getElementById(id)
            if el != null then
                discard(el.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal(behavior = "smooth", block = "start")))
        }

    // Keyed-list keys are user data and become path segments: escape the CSS attribute-selector
    // metacharacters (backslash, double quote) so a key cannot break or redirect the query.
    private def pathSelector(joined: String): String =
        s"""[data-kyo-path="${joined.replace("\\", "\\\\").replace("\"", "\\\"")}"]"""

    // ---- local op application for the SPA transport (Command / RequestMeasure) ----

    private def queryByPath(path: Seq[String]): dom.Element =
        document.querySelector(pathSelector(path.mkString(".")))

    /** Resolve a path-addressed command/measure target: the element carrying the path, else (a region
      * path: regions have no element of their own) the region's first element child, else null.
      */
    private def resolveElementByPath(path: Seq[String]): dom.Element =
        val el = queryByPath(path)
        if el != null then el
        else
            regions.get(path.mkString(".")).orNull match
                case null => null
                case r =>
                    var found: dom.Element = null
                    foreachRangeElement(r)(e => if found == null then found = e)
                    found
        end if
    end resolveElementByPath

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

    /** Apply a whitelisted `verb` to `el` (shared by path- and id-addressed commands). Unknown verbs are ignored. */
    private def applyVerbDom(el: dom.Element, verb: String): Unit =
        if el != null then
            val dyn = el.asInstanceOf[scalajs.js.Dynamic]
            verb match
                case "focus" => focusInto(el)
                case "scrollIntoView" =>
                    if scalajs.js.typeOf(dyn.scrollIntoView) == "function" then
                        discard(dyn.scrollIntoView(scalajs.js.Dynamic.literal(block = "nearest")))
                case _ => ()
            end match
        end if
    end applyVerbDom

    private def applyCommandDom(path: Seq[String], verb: String): Unit =
        applyVerbDom(resolveElementByPath(path), verb)

    /** Self-addressing: resolve the command target by DOM id (getElementById) instead of the render path. */
    private def applyCommandDomById(id: String, verb: String): Unit =
        applyVerbDom(document.getElementById(id), verb)

    private def measureRect(el: dom.Element): Maybe[UI.Rect] =
        if el == null then Absent
        else
            val r = el.getBoundingClientRect()
            Present(UI.Rect(r.left, r.top, r.width, r.height, dom.window.innerWidth, dom.window.innerHeight))

    private def measureDom(path: Seq[String]): Maybe[UI.Rect] =
        measureRect(resolveElementByPath(path))

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
            case HtmlOp.CommandById(id, verb) => Sync.defer(applyCommandDomById(id, verb))
            case HtmlOp.RequestMeasureById(id) =>
                Sync.defer(measureDomById(id)).map {
                    case Present(rect) => commands().deliverMeasureById(id, rect)
                    case Absent        => Kyo.unit
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
                }
            // measure now + deliver, then attach the continuous scroll/resize observer for `id`.
            case HtmlOp.ObserveViewportById(id) =>
                Sync.defer(registerViewportObserver(id, commands)).andThen(
                    Sync.defer(measureDomById(id)).map {
                        case Present(rect) => commands().deliverMeasureById(id, rect)
                        case Absent        => Kyo.unit
                    }
                )
            case HtmlOp.UnobserveViewportById(id) =>
                Sync.defer(unregisterViewportObserver(id))
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

    /** Removes the scroll/resize handler registered for `id` (from both listeners) and drops the map entry. */
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

        // Each channel patches in place and marks the attribute owned (__kyoOwn), so a parent region's morph
        // will not reconcile the live value back. The `*Now` forms are the patches themselves, written without
        // the Sync wrapper for a caller that already holds the thread (see UIExchange's synchronous sinks); the
        // effectful twins do nothing but defer to them, so the two can never drift apart.
        private def attrPatchNow(path: Seq[String], name: String, value: String): Unit =
            val el = queryByPath(path)
            if el != null then
                markOwned(el, name)
                el.setAttribute(name, value)
        end attrPatchNow

        private def boolAttrPatchNow(path: Seq[String], name: String, value: Boolean): Unit =
            val el = queryByPath(path)
            if el != null then
                markOwned(el, name)
                if value then el.setAttribute(name, "") else el.removeAttribute(name)
        end boolAttrPatchNow

        // Toggle in place (so CSS transitions fire) rather than re-render; owns "class" against the morph.
        private def classPatchNow(path: Seq[String], name: String, on: Boolean): Unit =
            val el = queryByPath(path)
            if el != null then
                markOwned(el, "class")
                discard(el.classList.toggle(name, on))
        end classPatchNow

        override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
            Sync.defer(attrPatchNow(path, name, value))

        override def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async =
            Sync.defer(boolAttrPatchNow(path, name, value))

        override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
            Sync.defer(classPatchNow(path, name, on))

        override val attrPatcherNow: Maybe[(Seq[String], String, String) => Unit]      = Present(attrPatchNow)
        override val boolAttrPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit] = Present(boolAttrPatchNow)
        override val classPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit]    = Present(classPatchNow)

        def onChange(path: Seq[String], ui: UI, mount: Boolean)(using Frame): Unit < Async =
            // Render content at its nested-reactive sub-path (contentPath) so a reactive-valued region paints a
            // DISTINCT inner marker span matching SSR/walkStatic. mountSlot=true stamps the `s` flag on Mounted
            // placeholders for the mount guards below. The payload is a bare fragment: the region's own live
            // markers stay in the DOM and are never re-sent, so the path stays addressable across replacements
            // regardless of what the content is (Fragment, Text, RawHtml: none carry a path-bearing root).
            HtmlRenderer.render(ui, HtmlRenderer.contentPath(path, ui), mountSlot = true).map { html =>
                Sync.defer {
                    val pathAttr = path.mkString(".")
                    val r        = lookupRegion(pathAttr)
                    if r == null then
                        // No marker pair at this path: either an ELEMENT region (a signal-bound field like
                        // `input.value(ref)`: the region root IS the live element carrying the path; no wrapper
                        // of any kind exists) morphed 1:1 in place, or genuinely unpainted DOM, which stays a
                        // silent no-op.
                        val el = queryByPath(path)
                        if el != null && el.parentNode != null then
                            val toContainer = parseToContainer(el.parentNode.asInstanceOf[dom.Element], html)
                            val toRoot      = if toContainer != null then firstElementChildOf(toContainer) else null
                            if toRoot != null then
                                morphNode(el, toRoot)
                                val live = queryByPath(path)
                                if live != null then
                                    applyJsPropsSync(live)
                                    beginAnimationsSync(live)
                            end if
                        end if
                    // Leave the live mount region untouched ONLY when the new content is itself a mount slot
                    // (`s` = the SAME mount re-rendering, which owns its subtree). Different content or an empty
                    // gate-closed repaint falls through so the morph reconciles/empties the region.
                    else if !mount && r.mount && payloadRootIsMountSlot(html) then ()
                    // Text into text: assigning the node IS the whole patch. The slow path below would put
                    // this through a <template> parse and a morph per region, which is what makes a hundred
                    // changed labels cost a hundred HTML parses.
                    else if !mount && !r.mount && patchLoneText(r, html) then ()
                    else
                        val parent = r.start.parentNode.asInstanceOf[dom.Element]
                        // Capture focus and caret of the active element inside the replaced region (mirrors the
                        // clientJs Replace handler on the JS DOM API). Plain DOM inside the already-suspended
                        // Sync.defer; no new AllowUnsafe crossing.
                        val ae           = document.activeElement
                        val insideRegion = ae != null && (ae ne document.body) && rangeContains(r, ae)
                        // Use the active element's own data-kyo-path when it carries one (nested element),
                        // otherwise fall back to the region path so restoreFocus searches the range (common
                        // case: value-bound input inside the region has no data-kyo-path of its own).
                        val activePath =
                            if insideRegion then
                                if ae.hasAttribute("data-kyo-path") then ae.getAttribute("data-kyo-path")
                                else pathAttr
                            else null
                        val (selStart, selEnd) = if insideRegion then readSelection(ae) else (Absent, Absent)
                        val toContainer        = parseToContainer(parent, html)
                        if toContainer != null then
                            // Transition and focus-auto bookkeeping is snapshotted off the OLD range before the
                            // morph: a morph removes departing nodes just like the outerHTML replace it took over
                            // from, so leaving elements still have to be cloned into ghosts up front.
                            val oldEnter = rangePaths(r)(enterPaths)
                            // A mount cell's own republish never ghosts: the engine re-runs the mount effect
                            // whenever an enclosing region repaints, which is bookkeeping, not user-visible
                            // leaving. Placeholder and effect content may shape their children differently, so a
                            // leave-marked element's path changes across the swap, the survivor set cannot match
                            // it, and every republish would spawn a ghost. Content that genuinely leaves the page
                            // does so through an ENCLOSING patch, which is a `mount = false` region and still
                            // ghosts. Twin of the clientJs `op.Replace.mount` guard; keep in lockstep.
                            val ghosts       = if mount then Seq.empty else rangeLeaveGhosts(r, leaveSurvSetIn(toContainer))
                            val oldFocusAuto = rangePaths(r)(focusAutoPaths)
                            // Morph the marker-delimited range instead of replacing, so focus/caret/scroll/
                            // transitions on reused nodes survive.
                            morphRange(parent, r.start.nextSibling, r.end, toContainer.firstChild, null)
                            if mount && !r.mount then
                                // The mount's own first paint claims the region: the `m` flag rides the live
                                // start marker (source of truth, survives registry rebuilds); the registry
                                // entry is a cache. Client-only mutation, never in server-rendered HTML.
                                // Any `k` a parent slot render already put there is carried over: dropping
                                // it would make the next parent paint fall through instead of skipping.
                                val carried = RegionMarker.parse(r.start.data).flatMap(_.key)
                                r.mount = true
                                r.start.data = RegionMarker.openData(pathAttr, mount = true, key = carried)
                            end if
                            // A morph imports subtrees, which carries new nested-region markers with it:
                            // refresh the registry for exactly this range.
                            rescanRange(r)
                            foreachRangeElement(r) { el =>
                                applyJsPropsSync(el)
                                beginAnimationsSync(el)
                            }
                            if activePath != null then
                                restoreFocus(activePath, selStart, selEnd)
                            foreachRangeElement(r)(seedEnter(_, oldEnter))
                            // Seed AFTER restoreFocus so a newly-appeared focus-auto element wins over restore-to-trigger.
                            seedRangeFocusAuto(r, oldFocusAuto)
                            spawnGhosts(ghosts)
                            sweepFocusAuto()
                        end if
                    end if
                }
            }
    end LocalExchange

    /** Assign a text-only payload to a region that currently holds exactly one text node, and report
      * whether that applied. Both shapes have to match: a region holding elements needs the morph, and a
      * payload carrying markup needs the parser. `&` disqualifies a payload alongside `<` — the renderer
      * escapes text, and decoding entities here would duplicate the parser's job for the sake of a few
      * percent of payloads. Everything the slow path does around the morph is a no-op on this shape:
      * a range without elements has no js props, no animations, no enter/leave sets, and cannot hold focus.
      *
      * Twin of `__kyoLoneText` in clientJs; keep in lockstep.
      */
    private def patchLoneText(r: RegionRange, html: String): Boolean =
        if html.indexOf('<') >= 0 || html.indexOf('&') >= 0 then false
        else
            val first = r.start.nextSibling
            if first == null || (first eq r.end) || first.nodeType != 3 || (first.nextSibling ne r.end) then false
            else
                val t = first.asInstanceOf[dom.Text]
                if t.data != html then t.data = html
                true
            end if
    end patchLoneText

    private def readSelection(el: dom.Element): (Maybe[Int], Maybe[Int]) =
        val dyn = el.asInstanceOf[scalajs.js.Dynamic]
        def asInt(v: scalajs.js.Dynamic): Maybe[Int] =
            if scalajs.js.typeOf(v) == "number" then Present(v.asInstanceOf[Int]) else Absent
        (asInt(dyn.selectionStart), asInt(dyn.selectionEnd))
    end readSelection

    private def restoreFocus(capturedPath: String, selStart: Maybe[Int], selEnd: Maybe[Int]): Unit =
        val located = document.querySelector(pathSelector(capturedPath))
        val focusTarget: dom.Element =
            if located != null then located
            else
                // A region path has no element carrying it; resolve via the registry and take the first
                // focus-capable element in the range (mirrors the old descend-into-wrapper behavior).
                regions.get(capturedPath).orNull match
                    case null => null
                    case r =>
                        var found: dom.Element = null
                        foreachRangeElement(r) { el =>
                            if found == null then
                                val sel = "input,textarea,select,[contenteditable]"
                                if el.matches(sel) then found = el
                                else
                                    val inner = el.querySelector(sel)
                                    if inner != null then found = inner
                                end if
                        }
                        found
        if focusTarget != null then
            val _ = focusTarget.asInstanceOf[scalajs.js.Dynamic].focus()
            (selStart, selEnd) match
                case (Present(s), Present(e)) => setSelection(focusTarget, s, e)
                case _                        => ()
        end if
    end restoreFocus

    /** The set of `data-kyo-path` values of every `data-kyo-focus-auto` element inside `root`, `root` itself included.
      *
      * Callers capture this BEFORE replacing a region so that [[seedFocusAuto]] can tell a newly appeared element from
      * one that was already on screen: an echo re-render of an open overlay must not steal focus back from the user.
      */
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
      * `.focus()` on it. On the initial mount `oldSet` is empty, so any focus-auto element is seeded, like native
      * `autofocus`. Mirrors `seedFocusAuto` in HtmlRenderer.clientJs.
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
                focusReturnStack.append(
                    FocusSeed(el.getAttribute("data-kyo-path"), ret, el.hasAttribute("data-kyo-focus-restore"))
                )
            discard(el.asInstanceOf[scalajs.js.Dynamic].focus())
        }
    end seedFocusAuto

    /** Unwind stack entries whose seeded focus-auto element left the document, returning focus at most once.
      *
      * Stops at the first entry whose element is still in the document: that seed is still on screen, and
      * restoring an entry below it would move focus out of it. Below that, exactly one restore may land: a
      * deeper entry belongs to a seed that closed while a newer one stayed open, so its return target is stale
      * and must not override the one just restored. Its entry is still dropped, so it cannot fire on a later
      * sweep either. Mirrors `sweepFocusAuto` in HtmlRenderer.clientJs.
      */
    @tailrec
    private def sweepFocusAuto(restored: Boolean = false): Unit =
        focusReturnStack.lastMaybe match
            case Present(seed)
                if document.querySelector(s"""[data-kyo-path="${seed.path}"][data-kyo-focus-auto]""") == null =>
                focusReturnStack = focusReturnStack.dropLeftAndRight(0, 1)
                val landed = !restored && seed.restore && seed.returnTo.exists(retPath => focusIfPresent(retPath))
                sweepFocusAuto(restored || landed)
            case _ => ()
    end sweepFocusAuto

    /** Focus the element carrying `path`; `false` when it is no longer in the document (nothing focused). */
    private def focusIfPresent(path: String): Boolean =
        val el = document.querySelector(s"""[data-kyo-path="$path"]""")
        if el == null then false
        else
            discard(el.asInstanceOf[scalajs.js.Dynamic].focus())
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

    /** True when `start` or any of its ancestors below `document.body` declares event type `t` in `data-kyo-ev`.
      *
      * ReactiveUI.dispatchToElement bubbles an event to every ancestor that declared a handler for its type, so the
      * SPA forwarding gate must forward when ANY ancestor declared it, not just the target (checking only the
      * target's own data-kyo-ev would drop e.g. a keydown meant for an ancestor panel before bubble dispatch runs).
      */
    private[kyo] def declaredInChain(start: dom.Element, t: String): Boolean =
        var n: dom.Element = start
        var found          = false
        while !found && n != null && (n ne document.body) do
            val ev = n.getAttribute("data-kyo-ev")
            if ev != null && ev.split(",").contains(t) then found = true
            else
                n = n.parentNode match
                    case p: dom.Element => p
                    case _              => null
            end if
        end while
        found
    end declaredInChain

    /** Set up capture-phase event delegation on document.body. */
    private def setupEventDelegation(dispatch: (Seq[String], UIEvent) => Boolean < Async, events: Channel[Unit < Async])(using
        Frame
    ): Unit < Sync = Sync.defer {
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
                        clickSubmitGuard = true
                        discard(dom.window.setTimeout(() => clickSubmitGuard = false, 0))
                        Present(UIEvent.Click(path, mouse))
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
                            if files.length > 0 then
                                val reader = new dom.FileReader()
                                reader.onload = (_: dom.Event) =>
                                    val content = reader.result.asInstanceOf[String]
                                    val ev      = UIEvent.Change(path, content)
                                    fireFromJs(events, dispatch(path, ev).unit)
                                reader.readAsText(files(0))
                            end if
                            Absent
                        else
                            Present(UIEvent.Change(path, tgt.value))
                        end if
                    else if t == "submit" && evTypes.contains("submit") then
                        e.preventDefault()
                        if clickSubmitGuard then Absent
                        else
                            val submitTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                            val submitMouse = MouseEventData(
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

        Seq("click", "input", "change", "submit", "keydown", "keyup", "focus", "blur", "mouseover", "mouseout").foreach { t =>
            document.body.addEventListener(t, handler, true)
        }
        document.body.addEventListener(
            "wheel",
            handler,
            js.Dynamic.literal(capture = true, passive = false).asInstanceOf[dom.EventListenerOptions]
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

    // ---- enter/leave transition mirror (SPA transport) ----

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

    /** The set of paths of `data-kyo-leave` elements in a payload (which leave-elements survive a region replace).
      * Keyed on leave-carrying elements, NOT all `data-kyo-path`: a reactive wrapper span shares its path with the
      * (leaving) element it wraps, so an all-path set would wrongly report the element as surviving.
      *
      * Reads off the ALREADY PARSED container rather than a second `template.innerHTML`: a `<tr>` or `<option>`
      * payload only survives parsing inside its required ancestor chain, which `parseToContainer` supplies and a
      * bare template does not. Parsed bare, those rows are dropped, the survivor set comes back empty, and every
      * leaving row is then wrongly treated as removed: it gets a ghost and its live node is torn out.
      */
    private def leaveSurvSetIn(container: dom.Node): Set[String] =
        val els = container.asInstanceOf[dom.Element].querySelectorAll("[data-kyo-leave]")
        (0 until els.length).flatMap { i =>
            Maybe(els(i).asInstanceOf[dom.Element].getAttribute("data-kyo-path")).toList
        }.toSet
    end leaveSurvSetIn

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

    private def setupInputMasking()(using Frame): Unit < Sync = Sync.defer {
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
        document.body.addEventListener("beforeinput", handler, true)
        document.body.addEventListener("compositionend", compositionEndHandler, true)
    }
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
                val nv =
                    if filt != null then InputMasking.filterStr(filt, v, "")
                    else if mask != null then InputMasking.maskNormalize(mask, v)
                    else v
                if nv != v then setValue(t, nv)
            end if
        end if

    /** True when `key` is a page-scrolling navigation key a `preventScrollKeys` region should suppress on `target`.
      * Vertical keys are exempt when `target` consumes them itself (caret line movement, option change) — there the
      * browser default is not a page scroll, so there is nothing to suppress. A single-line input stays suppressed for
      * vertical keys on purpose: that is the combobox case where `ArrowDown` drives the listbox highlight.
      * Horizontal/edge keys are exempt for any text-editable target, so a filter input keeps caret movement.
      */
    private def scrollKeyPrevented(key: String, target: dom.Element): Boolean =
        val tag              = target.tagName
        def contentEditable  = target.asInstanceOf[scalajs.js.Dynamic].isContentEditable.asInstanceOf[Boolean]
        def editable         = tag == "INPUT" || tag == "TEXTAREA" || tag == "SELECT" || contentEditable
        def verticalConsumer = tag == "TEXTAREA" || tag == "SELECT" || contentEditable
        key match
            case "ArrowUp" | "ArrowDown" | "PageUp" | "PageDown" => !verticalConsumer
            case "ArrowLeft" | "ArrowRight" | "Home" | "End"     => !editable
            case _                                               => false
        end match
    end scrollKeyPrevented
    // ---- DOM morphing: patch a sibling range in place toward new HTML, preserving element identity ----
    // Replacing wholesale discards DOM-local state (focus, caret, scroll, in-flight transitions, pointer
    // capture) and node identity; morphing reuses nodes and patches only diffs. Reconciliation is
    // SIBLING-SCOPED, keyed on `data-kyo-path` for elements (unique among siblings: Foreach items get
    // `path :+ key`, element children `path :+ i`) and on the region path for marker-delimited spans, which
    // move/insert/remove as ONE logical child. A region owns the sibling range between its two comment
    // markers, so patches never touch out-of-range siblings of the same parent.
    private val SvgNs = "http://www.w3.org/2000/svg"

    // ---- pointer/drag delegation (SPA transport) ----

    // Drag-session state. Module-level mutable is safe on the single-threaded JS runtime (mutated only inside JS
    // event callbacks). A session is active between a pointerdown on a declaring element and its pointerup.
    private var ptrActive: Boolean             = false
    private var ptrEl: dom.Element             = null
    private var ptrPath: Seq[String]           = Seq.empty
    private var ptrRaf: Int                    = 0
    private var ptrPendingEv: dom.PointerEvent = null

    /** True if `start` or any ancestor up to (not including) body declares event token `t` in its data-kyo-ev. */
    private def declaredInChainAt(start: dom.Element, t: String): Boolean =
        var n: dom.Element = start
        var found          = false
        while !found && n != null && (n ne document.body) do
            val ev = n.getAttribute("data-kyo-ev")
            if ev != null && ev.split(",").contains(t) then found = true
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

    // ---- region registry: joined path -> live comment-marker range ----

    /** A live region's marker pair. `mount` mirrors the `m` flag on the start marker's data (cache only;
      * the marker text is the source of truth and survives registry rebuilds).
      */
    final private class RegionRange(val start: dom.Comment, val end: dom.Comment, var mount: Boolean)

    /** Path -> live range. Rebuilt by full scans (initial paint, stale-lookup fallback) and refreshed by
      * range-scoped rescans after every patch (a morph imports subtrees via importNode, which brings new
      * nested-region markers with them). Module-level mutable is safe on the single-threaded runtime.
      */
    private var regions: js.Dictionary[RegionRange] = js.Dictionary.empty
    private var scanRoot: dom.Node                  = null

    private def commentData(n: dom.Node): String = n.asInstanceOf[dom.Comment].data

    /** Register a scanned pair, repairing parser separation first: the whole-page parser keeps an open
      * marker that precedes a table's FIRST row as a `<table>` child while the rows and the close
      * marker land inside the implied `<tbody>` it synthesizes. Adopting the open marker into that row
      * group makes the pair share a parent again (the invariant every range walk relies on) and puts
      * the rows inside the range. Twin of `__kyoRegPair` in clientJs.
      */
    private def registerPair(path: String, start: dom.Comment, end: dom.Comment, mount: Boolean): Unit =
        if (start.parentNode ne end.parentNode) && end.parentNode != null && (end.parentNode.parentNode eq start.parentNode) then
            discard(end.parentNode.insertBefore(start, end.parentNode.firstChild))
        regions(path) = new RegionRange(start, end, mount)
    end registerPair

    /** Register every marker pair under `root` (whole-subtree TreeWalker) into `regions`. Paths are
      * document-unique, so pairing open->close by path needs no stack; unbalanced markers are dropped.
      */
    private def scanRegionsInto(root: dom.Node): Unit =
        val walker = document.createTreeWalker(root, dom.NodeFilter.SHOW_COMMENT, null, false)
        val opens  = js.Dictionary.empty[(dom.Comment, Boolean)]
        var n      = walker.nextNode()
        while n != null do
            RegionMarker.parse(commentData(n)) match
                case Present(p) =>
                    if p.isClose then
                        opens.get(p.path) match
                            case Some((start, mount)) =>
                                registerPair(p.path, start, n.asInstanceOf[dom.Comment], mount)
                                discard(opens.remove(p.path))
                            case None => ()
                    else opens(p.path) = (n.asInstanceOf[dom.Comment], p.mount)
                case Absent => ()
            end match
            n = walker.nextNode()
        end while
    end scanRegionsInto

    private def rebuildRegions(): Unit =
        regions = js.Dictionary.empty
        if scanRoot != null then scanRegionsInto(scanRoot)
    end rebuildRegions

    /** Re-register marker pairs inside `r` after a patch. A pair always shares one parent, so direct
      * comment siblings pair at this level and everything deeper is contained in element siblings.
      * Bounded by patch size, not page size.
      */
    private def rescanRange(r: RegionRange): Unit =
        val opens = js.Dictionary.empty[(dom.Comment, Boolean)]
        var n     = r.start.nextSibling
        while n != null && (n ne r.end) do
            if n.nodeType == 8 then
                RegionMarker.parse(commentData(n)) match
                    case Present(p) =>
                        if p.isClose then
                            opens.get(p.path) match
                                case Some((start, mount)) =>
                                    registerPair(p.path, start, n.asInstanceOf[dom.Comment], mount)
                                    discard(opens.remove(p.path))
                                case None => ()
                        else opens(p.path) = (n.asInstanceOf[dom.Comment], p.mount)
                    case Absent => ()
            else if n.nodeType == 1 then scanRegionsInto(n)
            end if
            n = n.nextSibling
        end while
    end rescanRange

    /** Locate a live region by joined path. Connectivity-validated; a stale or missing entry triggers
      * ONE full rescan, then retries; still missing -> null. Callers no-op on null, preserving the old
      * "querySelector returned null -> silently skip" contract patches against unpainted DOM rely on.
      */
    private def lookupRegion(pathAttr: String): RegionRange =
        def connected(r: RegionRange): Boolean =
            r != null && r.start.isConnected && r.end.isConnected
        val direct = regions.get(pathAttr).orNull
        if connected(direct) then direct
        else
            rebuildRegions()
            val retried = regions.get(pathAttr).orNull
            if connected(retried) then retried else null
        end if
    end lookupRegion

    /** True when `node` is one of the range's direct children or a descendant of one. */
    private def rangeContains(r: RegionRange, node: dom.Node): Boolean =
        var n     = r.start.nextSibling
        var found = false
        while !found && n != null && (n ne r.end) do
            if (n eq node) || (n.nodeType == 1 && n.contains(node)) then found = true
            n = n.nextSibling
        found
    end rangeContains

    private def foreachRangeElement(r: RegionRange)(f: dom.Element => Unit): Unit =
        var n = r.start.nextSibling
        while n != null && (n ne r.end) do
            if n.nodeType == 1 then f(n.asInstanceOf[dom.Element])
            n = n.nextSibling
    end foreachRangeElement

    /** Leave-ghosts for a whole region. Each top-level element of the range contributes the ghosts of its own
      * subtree, so the union covers the region the way a single region root did before markers.
      */
    private def rangeLeaveGhosts(r: RegionRange, surv: Set[String]): Seq[(dom.Element, dom.Element, String)] =
        var acc = Seq.empty[(dom.Element, dom.Element, String)]
        foreachRangeElement(r)(el => acc = acc ++ prepareLeaveGhosts(el, surv))
        acc
    end rangeLeaveGhosts

    /** Union of a path-set helper over the range's top-level elements; each helper already covers its own subtree. */
    private def rangePaths(r: RegionRange)(f: dom.Element => Set[String]): Set[String] =
        var acc = Set.empty[String]
        foreachRangeElement(r)(el => acc = acc ++ f(el))
        acc
    end rangePaths

    /** Seeds focus-auto for a region. `seedFocusAuto` seeds the FIRST newly-appeared candidate below the root it
      * is handed, so it must run on exactly one range element: the first one that actually carries a new
      * candidate. Calling it per top-level element would seed one focus per sibling instead of one per region.
      */
    private def seedRangeFocusAuto(r: RegionRange, oldSet: Set[String]): Unit =
        var seeded = false
        foreachRangeElement(r) { el =>
            if !seeded && focusAutoPaths(el).exists(p => !oldSet.contains(p)) then
                seedFocusAuto(el, oldSet)
                seeded = true
        }
    end seedRangeFocusAuto

    // ---- logical children: a marker-delimited span is ONE keyed child ----

    /** For an open marker, its matching close marker among following siblings; null when unbalanced (the
      * caller then treats the comment as a plain positional node). Pairs never nest at one level (paths
      * are unique among siblings), so a direct path match suffices.
      */
    private def spanClose(open: dom.Node, openPath: String): dom.Node =
        var n                = open.nextSibling
        var result: dom.Node = null
        while result == null && n != null do
            if n.nodeType == 8 then
                RegionMarker.parse(commentData(n)) match
                    case Present(p) if p.isClose && p.path == openPath => result = n
                    case _                                             => ()
            end if
            n = n.nextSibling
        end while
        result
    end spanClose

    /** The reconciliation key of a logical child: an element's `data-kyo-path`, an open marker's region
      * path, else null (text, plain comments, and unkeyed elements reconcile positionally). Markers are
      * NEVER matched positionally: mispairing would rewrite marker text and corrupt region identity.
      */
    private def logicalKey(node: dom.Node): String =
        if node.nodeType == 1 then
            val el = node.asInstanceOf[dom.Element]
            if el.hasAttribute("data-kyo-path") then el.getAttribute("data-kyo-path") else null
        else if node.nodeType == 8 then
            RegionMarker.parse(commentData(node)) match
                case Present(p) if !p.isClose && spanClose(node, p.path) != null => p.path
                case _                                                           => null
        else null

    /** Next logical sibling: past the whole span for an open marker, else nextSibling. */
    private def logicalNext(node: dom.Node): dom.Node =
        if node.nodeType == 8 then
            RegionMarker.parse(commentData(node)) match
                case Present(p) if !p.isClose =>
                    val close = spanClose(node, p.path)
                    if close != null then close.nextSibling else node.nextSibling
                case _ => node.nextSibling
        else node.nextSibling

    private def eachSpanNode(first: dom.Node)(f: dom.Node => Unit): Unit =
        val last =
            if first.nodeType == 8 then
                RegionMarker.parse(commentData(first)) match
                    case Present(p) if !p.isClose =>
                        val close = spanClose(first, p.path)
                        if close != null then close else first
                    case _ => first
            else first
        var n    = first
        var stop = false
        while !stop && n != null do
            val next = n.nextSibling
            stop = n eq last
            f(n)
            n = next
        end while
    end eachSpanNode

    private def moveLogicalBefore(parent: dom.Element, node: dom.Node, ref: dom.Node): Unit =
        eachSpanNode(node)(n => discard(parent.insertBefore(n, ref)))

    private def removeLogical(parent: dom.Element, node: dom.Node): Unit =
        eachSpanNode(node)(n => discard(parent.removeChild(n)))

    private def insertLogicalClone(parent: dom.Element, toNode: dom.Node, ref: dom.Node): Unit =
        eachSpanNode(toNode)(n => discard(parent.insertBefore(document.importNode(n, true), ref)))

    /** Patch matched logical children (same key). Element vs element morphs in place; span vs span
      * recurses on the two content ranges, unless the live span carries the `m` (mount root) flag, the
      * incoming one the `s` (mount slot) flag, AND both name the same mount `k`: that is the SAME mount
      * still sitting here, which owns and repaints its own subtree, so the span is opaque and its start
      * marker is never touched (the `m` flag must survive). A kind mismatch at one key replaces wholesale.
      *
      * The `k` comparison is what makes opacity safe for a KEYED mount. Without it, a key change would keep
      * the evicted instance's content on screen forever; with it, the differing key falls through to the
      * morph (which reconciles the stale content against the new placeholder) and the live marker ADOPTS
      * the incoming key on the way out, so the next paint of an unchanged key is opaque. That adoption also
      * closes the boot case: a full-page render emits no `k` (client-only flag, golden HTML unchanged), so
      * the first re-render after boot morphs once and every one after it skips.
      */
    private def patchLogical(parent: dom.Element, m: dom.Node, toNode: dom.Node): Unit =
        val fromIsSpan = m.nodeType == 8
        val toIsSpan   = toNode.nodeType == 8
        if !fromIsSpan && !toIsSpan then morphNode(m, toNode)
        else if fromIsSpan && toIsSpan then
            (RegionMarker.parse(commentData(m)), RegionMarker.parse(commentData(toNode))) match
                case (Present(f), Present(t)) =>
                    val fClose = spanClose(m, f.path)
                    val tClose = spanClose(toNode, t.path)
                    if fClose == null || tClose == null then ()
                    else if f.mount && t.slot && f.key == t.key then ()
                    else
                        morphRange(parent, m.nextSibling, fClose, toNode.nextSibling, tClose)
                        if f.mount && t.slot then
                            m.asInstanceOf[dom.Comment].data =
                                RegionMarker.openData(f.path, mount = true, key = t.key)
                    end if
                case _ => ()
        else
            insertLogicalClone(parent, toNode, m)
            removeLogical(parent, m)
        end if
    end patchLogical

    /** True when the payload's first node is an open marker carrying the `s` (mount slot) flag: the
      * region's new content root IS a mount placeholder. Bounded to the leading comment so a descendant
      * marker cannot false-match.
      */
    private def payloadRootIsMountSlot(html: String): Boolean =
        if !html.startsWith("<!--") then false
        else
            val end = html.indexOf("-->")
            end > 4 && (RegionMarker.parse(html.substring(4, end)) match
                case Present(p) => !p.isClose && p.slot
                case Absent     => false)

    private def firstElementChildOf(parent: dom.Node): dom.Element =
        var c = parent.firstChild
        while c != null && c.nodeType != 1 do c = c.nextSibling
        if c == null then null else c.asInstanceOf[dom.Element]
    end firstElementChildOf

    /** Parse a region payload (a bare content fragment, zero..n roots) in the parse context its live
      * parent dictates, returning the detached node whose childNodes are the new content (kept inside
      * the template; the morph imports nodes on insert). The context wrap keeps the fragment parser from
      * foster-parenting or silently dropping context-sensitive content: a `<tr>` payload outside a table
      * parse is discarded wholesale, an `<option>` outside select likewise, and bare SVG elements in an
      * HTML template become unknown elements. Comment markers survive every one of these parse modes,
      * which is what makes marker-delimited regions parseable at all. Twin of `__kyoParseCtx` in
      * clientJs; keep in lockstep.
      */
    private def parseToContainer(parent: dom.Element, html: String): dom.Node =
        val tpl = document.createElement("template").asInstanceOf[dom.HTMLTemplateElement]
        val tag = parent.tagName
        val (prefix, suffix, depth) =
            if parent.namespaceURI == SvgNs && tag.toLowerCase != "foreignobject" then ("<svg>", "</svg>", 1)
            else
                tag match
                    // Explicit <tbody> (not the parser's implied one) so the descent depth is fixed.
                    case "TABLE" | "THEAD" | "TBODY" | "TFOOT" => ("<table><tbody>", "</tbody></table>", 2)
                    case "TR"                                  => ("<table><tbody><tr>", "</tr></tbody></table>", 3)
                    case "SELECT" | "OPTGROUP"                 => ("<select>", "</select>", 1)
                    case _                                     => ("", "", 0)
        tpl.innerHTML = prefix + html + suffix
        var container: dom.Node = tpl.content
        var d                   = depth
        while d > 0 && container != null do
            container = firstElementChildOf(container)
            d -= 1
        container
    end parseToContainer

    private def morphNode(fromNode: dom.Node, toNode: dom.Node): Unit =
        if toNode.nodeType != 1 then
            if fromNode.nodeValue != toNode.nodeValue then fromNode.nodeValue = toNode.nodeValue
        else
            val fromEl = fromNode.asInstanceOf[dom.Element]
            val toEl   = toNode.asInstanceOf[dom.Element]
            if fromEl.tagName != toEl.tagName then
                discard(fromEl.parentNode.replaceChild(document.importNode(toEl, true), fromEl))
            else morphEl(fromEl, toEl)
    end morphNode

    private def morphEl(fromEl: dom.Element, toEl: dom.Element): Unit =
        morphAttrs(fromEl, toEl)
        // A focused contenteditable would lose its caret if its children were rewritten mid-edit; leave its
        // subtree alone (INPUT/TEXTAREA have no element children, so need no such guard).
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
            if !owns(name) && fromEl.getAttribute(name) != a.value then fromEl.setAttribute(name, a.value)
            i += 1
        end while
        // Remove attributes gone from `to`. Walk the live NamedNodeMap backward so a removal never shifts an
        // index still to be visited (no intermediate collection allocated).
        val fromAttrs = fromEl.attributes
        var j         = fromAttrs.length - 1
        while j >= 0 do
            val name = fromAttrs(j).name
            if !owns(name) && !toEl.hasAttribute(name) then fromEl.removeAttribute(name)
            j -= 1
        end while
        // Active-input preservation: two-way binding echoes each keystroke back as a re-render. Never overwrite the
        // focused field's live `.value` (its caret) with its own echo (value already matches); assign only a genuine
        // external change (submit-clear, programmatic update).
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
        morphRange(fromParent, fromParent.firstChild, null, toParent.firstChild, null)

    /** Reconcile the live sibling range [fromStart, fromEnd) of `fromParent` toward the target range
      * [toStart, toEnd) (nodes inside a detached template). Null bounds mean "to the end of the parent";
      * a region's close marker serves as the from-side sentinel, so out-of-range siblings (including a
      * region's own markers) are never visited and `insertBefore(node, sentinel)` appends at the range
      * end. Keyed lookups are sibling-scoped over LOGICAL children (keys unique among siblings); `toKeyed`
      * records which stale from-children are reused elsewhere so an unkeyed slot doesn't destroy them.
      * Twin of `__kyoMorphRange` in clientJs; keep in lockstep.
      */
    private def morphRange(
        fromParent: dom.Element,
        fromStart: dom.Node,
        fromEnd: dom.Node,
        toStart: dom.Node,
        toEnd: dom.Node
    ): Unit =
        val fromNodes = js.Array[dom.Node]()
        val fromKeys  = js.Array[String]()
        val toNodes   = js.Array[dom.Node]()
        val toKeys    = js.Array[String]()
        collectLogical(fromStart, fromEnd, fromNodes, fromKeys)
        collectLogical(toStart, toEnd, toNodes, toKeys)

        var fromKeyed: js.Dictionary[dom.Node] = null
        var toKeyed: js.Dictionary[Boolean]    = null
        var i                                  = 0
        while i < fromKeys.length do
            if fromKeys(i) != null then
                if fromKeyed == null then fromKeyed = js.Dictionary.empty[dom.Node]
                fromKeyed(fromKeys(i)) = fromNodes(i)
            i += 1
        end while
        i = 0
        while i < toKeys.length do
            if toKeys(i) != null then
                if toKeyed == null then toKeyed = js.Dictionary.empty[Boolean]
                toKeyed(toKeys(i)) = true
            i += 1
        end while

        // Two-ended keyed pass, ahead of the single-cursor walk below. That walk can only insert IN FRONT
        // OF its cursor, so a key found behind the cursor drags every sibling in between along with it: a
        // two-row swap in a thousand rows costs 997 moves. Matching both ends first relocates only the
        // children that actually changed place — 2 for that swap, 0 for a removal in the middle.
        //
        // Invariant: the children still to place are exactly fromNodes[fh..ft], a contiguous DOM run that
        // ends immediately before `tailBoundary`. Only run boundaries are ever moved, and only out to a
        // boundary, so the run stays contiguous and the snapshot stays valid.
        var fh           = 0
        var ft           = fromNodes.length - 1
        var th           = 0
        var tt           = toNodes.length - 1
        var tailBoundary = fromEnd
        var scanning     = true
        while scanning && fh <= ft && th <= tt do
            // Unkeyed at either end: positional reconciliation is the cursor walk's job, so hand over.
            if fromKeys(fh) == null || fromKeys(ft) == null || toKeys(th) == null || toKeys(tt) == null then
                scanning = false
            else if fromKeys(fh) == toKeys(th) then
                patchLogical(fromParent, fromNodes(fh), toNodes(th))
                fh += 1
                th += 1
            else if fromKeys(ft) == toKeys(tt) then
                patchLogical(fromParent, fromNodes(ft), toNodes(tt))
                tailBoundary = fromNodes(ft)
                ft -= 1
                tt -= 1
            else if fromKeys(fh) == toKeys(tt) then
                // The run's head belongs at its tail. A single remaining child already sits there.
                if fh != ft then moveLogicalBefore(fromParent, fromNodes(fh), tailBoundary)
                patchLogical(fromParent, fromNodes(fh), toNodes(tt))
                tailBoundary = fromNodes(fh)
                fh += 1
                tt -= 1
            else if fromKeys(ft) == toKeys(th) then
                if ft != fh then moveLogicalBefore(fromParent, fromNodes(ft), fromNodes(fh))
                patchLogical(fromParent, fromNodes(ft), toNodes(th))
                ft -= 1
                th += 1
            else scanning = false
            end if
        end while

        // Hand the unresolved middle to the cursor walk. The dictionaries stay whole: keys are unique among
        // siblings, so a key consumed at an end cannot be asked for again from the middle.
        val gFromStart = if fh <= ft then fromNodes(fh) else tailBoundary
        val gToEnd     = if tt + 1 < toNodes.length then toNodes(tt + 1) else toEnd
        val gToStart   = if th <= tt then toNodes(th) else gToEnd
        morphRangeCursor(fromParent, gFromStart, tailBoundary, gToStart, gToEnd, fromKeyed, toKeyed)
    end morphRange

    /** Snapshot the logical children of [start, end) into `nodes` and their keys into `keys` (null where
      * unkeyed). One pass: the two-ended walk reads keys four times per step, and `logicalKey` is not free
      * for a span, where it has to find the matching close marker.
      */
    private def collectLogical(
        start: dom.Node,
        end: dom.Node,
        nodes: js.Array[dom.Node],
        keys: js.Array[String]
    ): Unit =
        var scan = start
        while scan != null && (scan ne end) do
            discard(nodes.push(scan))
            discard(keys.push(logicalKey(scan)))
            scan = logicalNext(scan)
        end while
    end collectLogical

    /** Single-cursor reconciliation of whatever the two-ended pass left over: keyed children are pulled to
      * the cursor by key, unkeyed ones morph positionally against the first compatible live child.
      */
    private def morphRangeCursor(
        fromParent: dom.Element,
        fromStart: dom.Node,
        fromEnd: dom.Node,
        toStart: dom.Node,
        toEnd: dom.Node,
        fromKeyed: js.Dictionary[dom.Node],
        toKeyed: js.Dictionary[Boolean]
    ): Unit =
        var curFrom = fromStart
        var curTo   = toStart
        while curTo != null && (curTo ne toEnd) do
            val toNext = logicalNext(curTo)
            val tKey   = logicalKey(curTo)
            if tKey != null then
                val m = if fromKeyed != null then fromKeyed.get(tKey).orNull else null
                if m != null then
                    if m ne curFrom then moveLogicalBefore(fromParent, m, curFrom)
                    else curFrom = logicalNext(curFrom)
                    patchLogical(fromParent, m, curTo)
                else
                    insertLogicalClone(fromParent, curTo, curFrom)
                end if
            else
                var handled = false
                var loop    = true
                while loop && curFrom != null && (curFrom ne fromEnd) do
                    val fNext = logicalNext(curFrom)
                    val fKey  = logicalKey(curFrom)
                    if fKey != null then
                        // A keyed from-child at an unkeyed slot: keep it if `to` reuses it elsewhere (its own slot
                        // moves it into place), else it's stale and removed. Null toKeyed = `to` has no keyed child.
                        if toKeyed == null || !toKeyed.contains(fKey) then removeLogical(fromParent, curFrom)
                        curFrom = fNext
                    else if compatible(curFrom, curTo) then
                        morphNode(curFrom, curTo)
                        curFrom = fNext
                        handled = true
                        loop = false
                    else
                        removeLogical(fromParent, curFrom)
                        curFrom = fNext
                    end if
                end while
                if !handled then insertLogicalClone(fromParent, curTo, curFrom)
            end if
            curTo = toNext
        end while
        while curFrom != null && (curFrom ne fromEnd) do
            val fNext = logicalNext(curFrom)
            removeLogical(fromParent, curFrom)
            curFrom = fNext
        end while
    end morphRangeCursor

    /** Two nodes may be patched into each other positionally: same node kind, and for elements the same tag. */
    private def compatible(a: dom.Node, b: dom.Node): Boolean =
        a.nodeType == b.nodeType &&
            (a.nodeType != 1 || a.asInstanceOf[dom.Element].tagName == b.asInstanceOf[dom.Element].tagName)

end DomBackend
