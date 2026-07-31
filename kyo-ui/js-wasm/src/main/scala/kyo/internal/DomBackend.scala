package kyo.internal

import kyo.*
import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js

/** Scala.js UI backend. Mounts a UI into the browser DOM. */
private[kyo] object DomBackend:

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
        for
            _    <- DomStyleSheet.injectBase()
            root <- ReactiveUI.normalize(ui, Seq.empty)
            html <- HtmlRenderer.render(ui, Seq.empty)
            _    <- Sync.defer(container.innerHTML = html)
            _    <- applyJsProps(container)
            _    <- Sync.defer(seedEnter(container, Set.empty))
            _    <- Sync.defer(beginAnimationsSync(container))
            exchange = LocalExchange(root)
            dispatch <- ReactiveUI.subscribe(root, exchange)
            // Single-consumer drain owned by the ambient page Scope: every JS event effect is run by a
            // Fiber.init consumer (interrupted on page teardown). The single consumer preserves event ordering
            // and is scoped, so page teardown interrupt propagates to the drain via the ambient Scope.
            events <- Channel.init[Unit < Async](256)
            // runPartial captures only the Closed failure (the channel closed on page teardown -> stop draining); a
            // Panic propagates rather than being silently swallowed as a clean drain end.
            // The drain carries the session's scroll sink: a handler calling UI.scrollIntoView scrolls the
            // local document, the browser-mount counterpart of the server session's WebSocket op.
            _ <- Fiber.init(UICommands.scrollSink.let(Present(scrollLocal)) {
                Loop.foreach(Abort.runPartial[Closed](events.take).map {
                    case Result.Success(eff) => eff.andThen(Loop.continue)
                    case Result.Failure(_)   => Loop.done
                })
            })
            _ <- setupEventDelegation(dispatch.handle, events)
            _ <- Async.never
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

    /** Exchange that renders UI to HTML and applies directly to the DOM. */
    private class LocalExchange(root: ReactiveUI) extends UIExchange:
        private def svgContextAt(path: Seq[String]): Boolean =
            ReactiveUI.findNode(root, path).map(_.svgContext).getOrElse(false)

        def onChange(path: Seq[String], ui: UI)(using Frame): Unit < Async =
            HtmlRenderer.render(ui, path).map { html =>
                // Always wrap the rendered html in the reactive boundary element so the node carrying
                // data-kyo-path=path survives subsequent replacements. A Fragment, Text, or RawHtml value
                // renders without a path-carrying root, so an unwrapped replace would drop the marker and
                // the next update could not locate the node. In SVG context the boundary is a <g> (a <span>
                // is invalid inside <svg>); otherwise a <span> (CSS sets `display: contents` so it is layout-
                // transparent).
                val tag       = if svgContextAt(path) then "g" else "span"
                val pathAttr  = path.mkString(".")
                val finalHtml = s"""<$tag data-kyo-path="$pathAttr" data-kyo-reactive>$html</$tag>"""
                Sync.defer {
                    val el = document.querySelector(s"""[data-kyo-path="$pathAttr"]""")
                    if el != null && el.outerHTML != finalHtml then
                        replaceRegion(pathAttr, el, finalHtml)
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

    /** The OUTERMOST `data-kyo-leave` elements under `root` that this patch drops (path not in `surv`) — the ones
      * that actually depart. Outermost only: a leaving subtree animates as one, its leaving descendants ride along.
      * Mirrors `kyoLeaving` in HtmlRenderer.clientJs.
      */
    private def leavingElements(root: dom.Element, surv: Set[String]): Seq[dom.Element] =
        val els = root.querySelectorAll("[data-kyo-leave]")
        val cand =
            (if root.getAttribute("data-kyo-leave") != null then Seq(root) else Seq.empty) ++
                (0 until els.length).map(els(_).asInstanceOf[dom.Element])
        val removed = cand.filter { e =>
            val p = e.getAttribute("data-kyo-path")
            p == null || !surv.contains(p)
        }
        removed.filterNot(e => removed.exists(o => (o ne e) && o.contains(e)))
    end leavingElements

    /** Longest transition/animation on `el` in milliseconds, delays included. Computed `<time>` serializes in
      * seconds, but `ms` is accepted too. Mirrors `kyoDurationMs`; the timeout it feeds is the safety net for what
      * `transitionend` does not cover (zero duration, an interrupted or never-started transition).
      */
    private def durationMs(el: dom.Element): Double =
        val cs = dom.window.getComputedStyle(el).asInstanceOf[scalajs.js.Dynamic]
        def ms(v: String): Double =
            val n = v.trim.takeWhile(c => c.isDigit || c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E')
            val d = n.toDoubleOption.getOrElse(0.0)
            if v.contains("ms") then d else d * 1000.0
        end ms
        def longest(dur: String, del: String): Double =
            val ds = dur.split(",")
            val ls = (if del == null || del.isEmpty then "0s" else del).split(",")
            ds.indices.map(i => ms(ds(i)) + ms(ls(i % ls.length))).maxOption.getOrElse(0.0)
        end longest
        def str(v: scalajs.js.Dynamic): String = if scalajs.js.typeOf(v) == "string" then v.asInstanceOf[String] else ""
        math.max(
            longest(str(cs.transitionDuration), str(cs.transitionDelay)),
            longest(str(cs.animationDuration), str(cs.animationDelay))
        )
    end durationMs

    /** Add each element's leave classes, then run `done` once every one has finished animating. Mirrors `kyoRunLeave`. */
    private def runLeave(els: Seq[dom.Element], done: () => Unit): Unit =
        els.foreach { e =>
            val clsList = e.asInstanceOf[scalajs.js.Dynamic].classList
            Maybe(e.getAttribute("data-kyo-leave")).foreach(_.split("\\s+").filter(_.nonEmpty).foreach(c => clsList.add(c)))
        }
        // Measure AFTER the classes are on: the transition may be declared by the leave class itself.
        val total = els.map(durationMs).maxOption.getOrElse(0.0)
        if total <= 0 then done()
        else
            var fired   = false
            var pending = els.size
            def finish(): Unit =
                if !fired then
                    fired = true
                    done()
            els.foreach { e =>
                lazy val listener: scalajs.js.Function1[dom.Event, Unit] = (ev: dom.Event) =>
                    // Descendant transitions bubble; only this element's own end counts.
                    if ev.target eq e then
                        e.removeEventListener("transitionend", listener)
                        e.removeEventListener("animationend", listener)
                        pending -= 1
                        if pending <= 0 then finish()
                e.addEventListener("transitionend", listener)
                e.addEventListener("animationend", listener)
            }
            val to: scalajs.js.Function0[Unit] = () => finish()
            discard(dom.window.setTimeout(to, total + 50.0))
        end if
    end runLeave

    /** Remove the leave classes again, abandoning the transition wherever it got to. The CSS runs back on its own. */
    private def unleave(els: Seq[dom.Element]): Unit =
        els.foreach { e =>
            val clsList = e.asInstanceOf[scalajs.js.Dynamic].classList
            Maybe(e.getAttribute("data-kyo-leave")).foreach(_.split("\\s+").filter(_.nonEmpty).foreach(c => clsList.remove(c)))
        }

    /** Do all of `els` reappear in `surv`? Then the render that produced it brings them back. */
    private def allReturn(els: Seq[dom.Element], surv: Set[String]): Boolean =
        els.nonEmpty && els.forall { e =>
            val p = e.getAttribute("data-kyo-path")
            p != null && surv.contains(p)
        }

    /** Does `node` already render exactly what `html` describes? Compared through the DOM serializer on both sides:
      * `outerHTML` writes valueless attributes as `attr=""`, so `data-kyo-reactive` alone makes a rendered string never
      * equal a live node's `outerHTML` verbatim. Mirrors `kyoSameHtml` in HtmlRenderer.clientJs.
      */
    private def sameHtml(node: dom.Element, html: String): Boolean =
        val tpl = document.createElement("template").asInstanceOf[scalajs.js.Dynamic]
        tpl.innerHTML = html
        val first = tpl.content.asInstanceOf[scalajs.js.Dynamic].firstElementChild.asInstanceOf[dom.Element]
        first != null && first.outerHTML == node.outerHTML
    end sameHtml

    /** `data-kyo-leave-interrupt` for a set of leaving elements. Absent means Reverse; if they disagree Finish wins, so
      * no element's transition is cut short by a sibling's policy.
      */
    private def interruptOf(els: Seq[dom.Element]): UI.LeaveInterrupt =
        if els.exists(_.getAttribute("data-kyo-leave-interrupt") == "finish") then UI.LeaveInterrupt.Finish
        else UI.LeaveInterrupt.Reverse

    /** A deferred replace awaiting its leave transition. `dead` marks a leave that was abandoned, so its already-armed
      * completion callback knows its payload went with it.
      */
    final private class Deferred(var html: String, val els: Seq[dom.Element], var dead: Boolean)

    /** Pending replace per path while its leave runs. Latest wins: a newer replace supersedes the stale payload rather
      * than queueing behind it, so at most one swap ever lands per leave. Module-level mutable state is safe — all
      * mutation runs inside `Sync.defer` on the single-threaded JS runtime. Mirrors `__pending` in HtmlRenderer.clientJs.
      */
    private var pendingReplace: Map[String, Deferred] = Map.empty

    /** The single entry point for a region replace. A leave already running for this path decides first — it may be
      * abandoned (the element came back) or merely re-aimed at the newer payload. Only when no leave is pending does this
      * patch get to start one, and only when nothing is leaving does the swap apply straight away. Routing the re-render
      * through here is what makes [[UI.LeaveInterrupt]] observable: nothing is leaving on the way back in, so a plain
      * "does anything leave?" test would replace the node outright and lose both the transition and the slot.
      * Mirrors `kyoReplace` in HtmlRenderer.clientJs.
      */
    private def replaceRegion(pathAttr: String, el: dom.Element, html: String): Unit =
        if pendingReplace.contains(pathAttr) then
            val slot = pendingReplace(pathAttr)
            // Mid-leave re-render. If it brings every leaving element back and none of them asked to finish, abandon the
            // leave: drop the classes so the transition reverses, and swap only if something ELSE also changed (usually
            // nothing did, and the DOM already is the state this render describes).
            if allReturn(slot.els, leaveSurvSet(html)) && interruptOf(slot.els) == UI.LeaveInterrupt.Reverse then
                slot.dead = true
                pendingReplace = pendingReplace - pathAttr
                unleave(slot.els)
                val back = document.querySelector(s"""[data-kyo-path="$pathAttr"]""")
                if back != null && !sameHtml(back, html) then applyReplace(back, pathAttr, html)
            else slot.html = html
            end if
        else
            val leaving = leavingElements(el, leaveSurvSet(html))
            if leaving.isEmpty then applyReplace(el, pathAttr, html)
            else
                // Elements carrying data-kyo-leave that this patch drops animate out FIRST, on the live nodes, and the
                // swap waits for them. Moving them elsewhere to animate (a clone re-parented to <body>) would re-resolve
                // every percentage and inherited value on them against the new parent.
                val mine = Deferred(html, leaving, dead = false)
                pendingReplace = pendingReplace.updated(pathAttr, mine)
                runLeave(
                    leaving,
                    () =>
                        // A dead slot means the leave was abandoned; its payload went with it.
                        if !mine.dead then
                            // Never clear a newer leave's slot.
                            if pendingReplace.contains(pathAttr) && (pendingReplace(pathAttr) eq mine) then
                                pendingReplace = pendingReplace - pathAttr
                            val cur = document.querySelector(s"""[data-kyo-path="$pathAttr"]""")
                            if cur != null && cur.outerHTML != mine.html then applyReplace(cur, pathAttr, mine.html)
                )
            end if
        end if
    end replaceRegion

    /** The plain swap: capture focus/caret, replace, re-apply client-side props, restore, seed enter classes.
      * Mirrors `kyoApplyReplace` in HtmlRenderer.clientJs.
      */
    private def applyReplace(el: dom.Element, pathAttr: String, html: String): Unit =
        // Capture focus and caret of the active element inside the replaced region, keyed on data-kyo-path
        // identity. Plain DOM inside the already-suspended Sync.defer; no new AllowUnsafe crossing.
        val ae = document.activeElement
        val insideRegion = ae != null && (ae ne document.body) &&
            (ae.getAttribute("data-kyo-path") == pathAttr || el.contains(ae))
        // Use the active element's own data-kyo-path when it carries one (nested reactive region), otherwise fall
        // back to pathAttr so the region wrapper itself is queried (common case: value-bound input inside the
        // region has no data-kyo-path of its own).
        val activePath =
            if insideRegion then
                if ae.hasAttribute("data-kyo-path") then ae.getAttribute("data-kyo-path")
                else pathAttr
            else null
        val (selStart, selEnd) = if insideRegion then readSelection(ae) else (Absent, Absent)
        val oldEnter           = enterPaths(el)
        el.outerHTML = html
        val updated = document.querySelector(s"""[data-kyo-path="$pathAttr"]""")
        if updated != null then
            applyJsPropsSync(updated)
            beginAnimationsSync(updated)
        if activePath != null then
            restoreFocus(activePath, selStart, selEnd)
        if updated != null then
            seedEnter(updated, oldEnter)
    end applyReplace

end DomBackend
