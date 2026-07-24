package kyo.internal

import kyo.*

private[kyo] object UIServer:

    private def normalizePath(basePath: String): String =
        if basePath.endsWith("/") then basePath.dropRight(1) else basePath

    def handlers(basePath: String)(ui: => UI < Async)(using Frame): Seq[HttpHandler[?, ?, ?]] < Sync =
        handlers(basePath, Absent)(ui)

    def handlers(basePath: String, onHandlerError: Maybe[Throwable => Unit < Async])(ui: => UI < Async)(using
        Frame
    ): Seq[HttpHandler[?, ?, ?]] < Sync =
        val base = normalizePath(basePath)
        Sync.defer(Seq(
            getPage(base, basePath, Sync.defer(ui)),
            wsRoute(base, Sync.defer(ui), onHandlerError)
        ))
    end handlers

    private def getPage(base: String, pagePath: String, ui: => UI < Async)(using Frame): HttpHandler[?, ?, ?] =
        HttpRoute.getText(pagePath).handler { _ =>
            for
                uiTree        <- ui
                (html, rules) <- HtmlRenderer.renderWithCss(uiTree, Seq.empty)
                // Pseudo-state Style (hover/focus/active/disabled) has no inline-style channel (an
                // inline `style="..."` attribute cannot express `:hover`), so its rules are carried
                // here in a real stylesheet instead, after the base reset (renderPage already orders
                // baseCss before css).
                css  = rules.map(_._2).mkString
                page = HtmlRenderer.renderPage("kyo-ui", html, css, base)
            yield HttpResponse.ok(page)
                .addHeader("Content-Type", "text/html; charset=utf-8")
        }

    private[kyo] def serveSession(
        ws: HttpWebSocket,
        ui: => UI < Async,
        onHandlerError: Maybe[Throwable => Unit < Async] = Absent
    )(using Frame): Unit < (Async & Abort[Closed]) =
        Scope.run {
            for
                // Unit 2/3: the session's imperative op channel. `emit` serializes an HtmlOp over this session's socket
                // (a Closed mid-send just drops the op). Provided via Env.run so component handlers and mounted effects
                // resolve `UI.commands` at run time (renderer/dispatch fibers all fork inside this scope).
                commands <- UI.Commands.init(op =>
                    Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit
                )
                _ <- Env.run(commands) {
                    for
                        uiTree <- ui
                        root   <- ReactiveUI.normalize(uiTree, Seq.empty, onHandlerError = onHandlerError)
                        // Pre-seed the connection's sent-class tracking with every pseudo-state class the
                        // initial SSR page already carries (rendered once more here, discarding the HTML), so
                        // the first reactive update touching an unchanged pseudo-styled element does not
                        // redundantly re-inject a rule the page's initial <style> block already has.
                        (_, initialRules) <- HtmlRenderer.renderWithCss(uiTree, Seq.empty)
                        exchange = wsExchange(root, ws, initialRules.map(_._1).toSet)
                        sub <- ReactiveUI.subscribe(root, exchange)
                        // Upstream (#1766) session command sink: an event handler calling UI.scrollIntoView sends the
                        // op over this connection's socket, riding the same channel as the reactive updates. Nested
                        // inside the fork's Env.run(commands) so both the Local scroll sink and the richer Env[Commands]
                        // channel are live on this session. runPartial drops only a Closed (socket closed, command moot).
                        scrollSink = (id: String) =>
                            Abort.runPartial[Closed](
                                ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](HtmlOp.ScrollIntoView(id))))
                            ).unit
                        _ <- UICommands.scrollSink.let(Present(scrollSink)) {
                            Async.race(
                                ws.stream.foreach(payload => dispatchEvent(sub.handle, commands, payload)),
                                ws.onPeerClose
                            )
                        }
                    yield ()
                }
            yield ()
        }

    private def wsRoute(
        base: String,
        ui: => UI < Async,
        onHandlerError: Maybe[Throwable => Unit < Async] = Absent
    )(using Frame): HttpHandler[?, ?, ?] =
        HttpHandler.webSocket(s"$base/_kyo/ws") { (_, ws) =>
            serveSession(ws, ui, onHandlerError)
        }

    private def wsExchange(root: ReactiveUI, ws: HttpWebSocket, seenClasses: Set[String])(using Frame): UIExchange =
        new UIExchange:
            private def svgContextAt(path: Seq[String]): Boolean =
                ReactiveUI.findNode(root, path).map(_.svgContext).getOrElse(false)

            // Pseudo-state CSS classes already carried by this connection's <style> (seeded from the
            // initial SSR page, then grown by every InjectCss this exchange sends), so a later
            // re-render reusing one of these classes never re-sends its rule. Connection-scoped: each
            // WS session gets its own set, matching the session-scoped subscription tree this exchange
            // already belongs to.
            private val sentClasses = scala.collection.mutable.Set.from(seenClasses)

            def onChange(path: Seq[String], ui: UI, mount: Boolean)(using Frame): Unit < Async =
                // Content at its nested-reactive sub-path (matches SSR/walkStatic); the wrapper below
                // stays at `path`, so the Replace still targets this region's node. renderWithCss also
                // collects the region's pseudo-state rules so a class first seen in an update ships its
                // stylesheet rule alongside the Replace. `mountSlot = true` stamps data-kyo-mount-slot on
                // any Mounted placeholder in this pushed HTML so the client `__kyoMorphChildren` guard
                // distinguishes the same mount from colliding content (twin of DomBackend.onChange). Only
                // this region-push path opts in; the full-page renders (lines ~27/59) keep the default so
                // golden HTML stays byte-identical.
                HtmlRenderer.renderWithCss(ui, HtmlRenderer.contentPath(path, ui), mountSlot = true).map { (html, rules) =>
                    val newRules  = rules.filterNot(r => sentClasses.contains(r._1))
                    val finalHtml = HtmlRenderer.wrapReactiveRegion(path, svgContextAt(path), html, mount)
                    val replaceOp = HtmlOp.Replace(path, finalHtml)
                    // runPartial drops only a Closed (the socket closed mid-render -> the op is moot); a Panic
                    // propagates to the region fiber rather than being swallowed by the discard.
                    val sendReplace =
                        Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](replaceOp)))).unit
                    if newRules.isEmpty then sendReplace
                    else
                        newRules.foreach(r => sentClasses += r._1)
                        val injectOp = HtmlOp.InjectCss(newRules.map(_._2).mkString)
                        // Send the new pseudo-state rule(s) before the replace that introduces the class
                        // referencing them, so the element never paints unstyled between the two frames.
                        Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](injectOp)))).unit
                            .andThen(sendReplace)
                    end if
                }
            end onChange

            // Declarative reactive-attribute patch over the websocket: the client sets the attribute in place
            // (marking ownership) — no content Replace, no re-render.
            override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
                val op = HtmlOp.SetAttrByPath(path, name, value)
                Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit
            override def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async =
                val op = HtmlOp.SetBoolAttrByPath(path, name, value)
                Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit
            override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
                val op = HtmlOp.SetClassByPath(path, name, on)
                Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit

    private def dispatchEvent(
        handle: (Seq[String], UIEvent) => Boolean < Async,
        commands: UI.Commands,
        payload: HttpWebSocket.Payload
    )(using Frame): Unit < Async =
        payload match
            case HttpWebSocket.Payload.Text(data) =>
                Json.decode[UIEvent](data) match
                    case Result.Success(event) =>
                        event match
                            // Unit 2: the Measure reply is not an element event — route it to the session channel,
                            // which resolves the pending requestMeasure callback (not the ReactiveUI handler tree).
                            case m: UIEvent.Measure =>
                                commands.deliverMeasure(
                                    m.path,
                                    UI.Rect(m.rectX, m.rectY, m.rectW, m.rectH, m.viewportW, m.viewportH)
                                )
                            // Self-addressing: the id-addressed measure reply routes by `id` to the id-keyed pending map.
                            case m: UIEvent.MeasureById =>
                                commands.deliverMeasureById(
                                    m.id,
                                    UI.Rect(m.rectX, m.rectY, m.rectW, m.rectH, m.viewportW, m.viewportH)
                                )
                            // Element events run in their OWN detached fiber rather than inline on the receive loop:
                            // a handler that SUSPENDS — e.g. awaiting a value-returning `requestMeasure` whose reply
                            // arrives as a LATER inbound frame — must not block this same loop from reading that reply
                            // (that would deadlock). Forking also stops any one slow handler from stalling the whole
                            // session's event stream. `initUnscoped` (not `init`): handlers are short-lived and self-
                            // cleaning, so binding each to the long-lived session Scope would only pile up finalizers; a
                            // handler still parked on a reply when the socket closes is unreachable once `commands` drops
                            // (its pending map holds the only promise ref) and is GC'd. The measure replies above stay
                            // inline: they only complete a Promise / push a Signal, never suspend, and must run promptly.
                            case _ => Fiber.initUnscoped(handle(event.path, event).unit).unit
                    // A malformed inbound frame (DecodeException) is dropped: a buggy client must not be able to tear
                    // down the session. A Panic is a decoder defect, not bad input, and must propagate.
                    case Result.Failure(_) => ()
                    case Result.Panic(ex)  => Abort.panic(ex)
            case HttpWebSocket.Payload.Binary(_) => ()

end UIServer
