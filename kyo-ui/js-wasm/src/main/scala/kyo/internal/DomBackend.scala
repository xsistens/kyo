package kyo.internal

import kyo.*
import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js

/** Scala.js UI backend. Mounts a UI into the browser DOM. */
private[kyo] object DomBackend:

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
            _        <- DomStyleSheet.injectBase()
            root     <- ReactiveUI.normalize(ui, Seq.empty)
            html     <- HtmlRenderer.render(ui, Seq.empty)
            _        <- Sync.defer(container.innerHTML = html)
            _        <- applyJsProps(container)
            _        <- Sync.defer(beginAnimationsSync(container))
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

    // ---- local op application for the SPA transport (Command / RequestMeasure) ----

    private def queryByPath(path: Seq[String]): dom.Element =
        document.querySelector(s"""[data-kyo-path="${path.mkString(".")}"]""")

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
                case _ => ()
            end match
        end if
    end applyVerbDom

    private def applyCommandDom(path: Seq[String], verb: String): Unit =
        applyVerbDom(queryByPath(path), verb)

    /** Self-addressing: resolve the command target by DOM id (getElementById) instead of the render path. */
    private def applyCommandDomById(id: String, verb: String): Unit =
        applyVerbDom(document.getElementById(id), verb)

    private def measureRect(el: dom.Element): Maybe[UI.Rect] =
        if el == null then Absent
        else
            val r = el.getBoundingClientRect()
            Present(UI.Rect(r.left, r.top, r.width, r.height, dom.window.innerWidth, dom.window.innerHeight))

    private def measureDom(path: Seq[String]): Maybe[UI.Rect] =
        measureRect(queryByPath(path))

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
        private def svgContextAt(path: Seq[String]): Boolean =
            ReactiveUI.findNode(root, path).map(_.svgContext).getOrElse(false)

        private def tableContextAt(path: Seq[String]): Boolean =
            ReactiveUI.findNode(root, path).map(_.tableContext).getOrElse(false)

        // In-place attr patch, ownership-marked (__kyoOwn) so a parent region's morph won't reconcile the live value back.
        override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
            Sync.defer {
                val el = queryByPath(path)
                if el != null then
                    markOwned(el, name)
                    el.setAttribute(name, value)
            }

        override def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async =
            Sync.defer {
                val el = queryByPath(path)
                if el != null then
                    markOwned(el, name)
                    if value then el.setAttribute(name, "") else el.removeAttribute(name)
            }

        // Class twin: toggle in place (so CSS transitions fire) rather than re-render; own "class" against the morph.
        override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
            Sync.defer {
                val el = queryByPath(path)
                if el != null then
                    markOwned(el, "class")
                    discard(el.classList.toggle(name, on))
            }

        def onChange(path: Seq[String], ui: UI, mount: Boolean)(using Frame): Unit < Async =
            // Render content at its nested-reactive sub-path (contentPath) so a reactive-valued region wraps a
            // DISTINCT inner wrapper matching SSR/walkStatic; the wrapper below stays at `path` so morph still
            // addresses this node. mountSlot=true stamps data-kyo-mount-slot on Mounted placeholders for the guards below.
            HtmlRenderer.render(ui, HtmlRenderer.contentPath(path, ui), mountSlot = true).map { html =>
                // Always wrap the rendered html in the reactive boundary element so the node carrying
                // data-kyo-path=path survives subsequent replacements. A Fragment, Text, or RawHtml value
                // renders without a path-carrying root, so an unwrapped replace would drop the marker and
                // the next update could not locate the node. In SVG context the boundary is a <g> (a <span>
                // is invalid inside <svg>); otherwise a <span> (CSS sets `display: contents` so it is layout-
                // transparent).
                val tag      = if svgContextAt(path) then "g" else if tableContextAt(path) then "tbody" else "span"
                val pathAttr = path.mkString(".")
                // data-kyo-mount flags a keyless mount's root so a parent morph treats it as opaque (never wipes the
                // live subtree). Client-only marker, never in the server render, so golden HTML stays byte-identical.
                val mountAttr = if mount then " data-kyo-mount" else ""
                val finalHtml = s"""<$tag data-kyo-path="$pathAttr" data-kyo-reactive$mountAttr>$html</$tag>"""
                Sync.defer {
                    val el = document.querySelector(s"""[data-kyo-path="$pathAttr"]""")
                    // Leave the live mount root untouched ONLY when the new content is itself a mount slot
                    // (data-kyo-mount-slot = the SAME mount re-rendering, which owns its subtree). Different content or
                    // an empty gate-closed repaint falls through so the morph reconciles/empties the slot.
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
                        // Morph toward finalHtml instead of replacing, so focus/caret/scroll/transitions on reused
                        // nodes survive. The capture-restore ceremony around this now largely no-ops but stays
                        // correct; morph returns the region root so the caller needn't re-query.
                        val updated = morph(el, finalHtml)
                        if updated != null then
                            applyJsPropsSync(updated)
                            beginAnimationsSync(updated)
                        if activePath != null then
                            restoreFocus(activePath, selStart, selEnd)
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
        val located = document.querySelector(s"""[data-kyo-path="$capturedPath"]""")
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

    /** Set up capture-phase event delegation on document.body. */
    private def setupEventDelegation(dispatch: (Seq[String], UIEvent) => Boolean < Async, events: Channel[Unit < Async])(using
        Frame
    ): Unit < Sync = Sync.defer {
        val handler: scalajs.js.Function1[dom.Event, Unit] = (e: dom.Event) =>
            findPathElement(e.target.asInstanceOf[dom.Element]).foreach { target =>
                val path    = parsePath(target.getAttribute("data-kyo-path"))
                val evAttr  = target.getAttribute("data-kyo-ev")
                val evTypes = if evAttr != null then evAttr.split(",").toSet else Set.empty[String]
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

    // ---- DOM morphing: patch a subtree in place toward new HTML, preserving element identity ----
    // Replacing via `el.outerHTML = html` discards DOM-local state (focus, caret, scroll, in-flight transitions,
    // pointer capture) and node identity; morphing reuses nodes and patches only diffs. Reconciliation is
    // SIBLING-SCOPED, keyed on `data-kyo-path` (unique among siblings: Foreach items get `path :+ key`, element
    // children `path :+ i`). The wrapper/inner nesting repeats a path across levels, so the keyed lookup is rebuilt
    // per child list, never global (a document-wide id set would mis-match a wrapper against its own child).
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

    /** The reconciliation key of a node: its `data-kyo-path` when it is an element carrying one, else null (text,
      * comment, and unkeyed elements reconcile positionally).
      */
    private def kyoKey(node: dom.Node): String =
        if node.nodeType == 1 then
            val el = node.asInstanceOf[dom.Element]
            if el.hasAttribute("data-kyo-path") then el.getAttribute("data-kyo-path") else null
        else null

    private def firstElementChildOf(parent: dom.Node): dom.Element =
        var c = parent.firstChild
        while c != null && c.nodeType != 1 do c = c.nextSibling
        if c == null then null else c.asInstanceOf[dom.Element]
    end firstElementChildOf

    /** A live keyless-mount content root (data-kyo-mount, set by the mount's own paint). A parent morph
      * reuses/repositions it but never reconciles its subtree; the mount owns and repaints that subtree.
      */
    private def isMountRoot(node: dom.Node): Boolean =
        node.nodeType == 1 && node.asInstanceOf[dom.Element].hasAttribute("data-kyo-mount")

    /** A top-down mount placeholder from a client region re-render (data-kyo-mount-slot): positive signal that the
      * incoming content at this slot IS a mount, vs content that collided on the positional key. Twin of
      * `__kyoIsMountSlot` in clientJs.
      */
    private def isMountSlot(node: dom.Node): Boolean =
        node.nodeType == 1 && node.asInstanceOf[dom.Element].hasAttribute("data-kyo-mount-slot")

    /** True when the FIRST tag of `html` carries data-kyo-mount-slot (the region's new root is a mount
      * placeholder). Bounded to the opening tag so a descendant marker can't false-match.
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

    /** Collapse a chain of transparent same-path reactive wrappers to one. Nested reactive regions at one position
      * each emit a `<span data-kyo-path=P data-kyo-reactive>` around the same content, and a paint carries only its
      * own depth; left as-is, morphing a shallow paint against a deep live subtree forces a tag mismatch that
      * replaces the whole subtree. Wrappers are `display: contents`, so collapsing is invisible and aligns every
      * paint at P to the same depth. A mount wrapper is never collapsed (its marker is load-bearing).
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

    /** Parse a single-wrapper HTML string to a detached root element. In SVG context parse inside a throwaway
      * `<svg>` so the parser yields real SVG elements (a bare `<g>` in an HTML template becomes an unknown element).
      */
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

    /** Patch `fromEl` in place toward `newHtml` (a single reactive-wrapper element string) and return the region
      * root: `fromEl` when identity is preserved, or the replacement node on a root-tag change. Caller reuses it
      * instead of re-querying for the path.
      */
    private def morph(fromEl: dom.Element, newHtml: String): dom.Element =
        val toEl = parseNewRoot(fromEl, newHtml)
        if toEl == null then fromEl
        else
            // Normalize the live side too, so a subtree from the server render (or an earlier paint) with redundant
            // nested same-path wrappers aligns instead of forcing a whole-subtree replace.
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
        // Sibling-scoped keyed lookups over DIRECT children only (keys unique among siblings). `toKeyed` records
        // which stale from-children are reused elsewhere so an unkeyed slot doesn't destroy them. Both dicts are
        // allocated lazily on the first keyed child; null means "no keyed children on that side".
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
                    // Opaque mount boundary: leave a live mount root's subtree alone ONLY when the incoming node is
                    // itself a mount slot (data-kyo-mount-slot = the same mount re-rendering, which repaints its own
                    // subtree). Otherwise (colliding content or an empty gate-closed slot) reconcile normally so the
                    // stale mount is replaced/emptied.
                    if !(isMountRoot(m) && isMountSlot(curTo)) then morphNode(m, curTo)
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
                        // A keyed from-child at an unkeyed slot: keep it if `to` reuses it elsewhere (its own slot
                        // moves it into place), else it's stale and removed. Null toKeyed = `to` has no keyed child.
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

end DomBackend
