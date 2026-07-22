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
                uiTree <- ui
                html   <- HtmlRenderer.render(uiTree, Seq.empty)
                page = HtmlRenderer.renderPage("kyo-ui", html, "", base)
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
                        exchange = wsExchange(root, ws)
                        sub <- ReactiveUI.subscribe(root, exchange)
                        _ <- Async.race(
                            ws.stream.foreach(payload => dispatchEvent(sub.handle, commands, payload)),
                            ws.onPeerClose
                        )
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

    private def wsExchange(root: ReactiveUI, ws: HttpWebSocket)(using Frame): UIExchange =
        new UIExchange:
            private def svgContextAt(path: Seq[String]): Boolean =
                ReactiveUI.findNode(root, path).map(_.svgContext).getOrElse(false)

            def onChange(path: Seq[String], ui: UI, mount: Boolean)(using Frame): Unit < Async =
                // Content at its nested-reactive sub-path (matches SSR/walkStatic); the wrapper below
                // stays at `path`, so the Replace still targets this region's node.
                HtmlRenderer.render(ui, HtmlRenderer.contentPath(path, ui)).map { html =>
                    val finalHtml = HtmlRenderer.wrapReactiveRegion(path, svgContextAt(path), html, mount)
                    val op        = HtmlOp.Replace(path, finalHtml)
                    // runPartial drops only a Closed (the socket closed mid-render -> the op is moot); a Panic
                    // propagates to the region fiber rather than being swallowed by the discard.
                    Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit
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
                            case _ => handle(event.path, event).unit
                    // A malformed inbound frame (DecodeException) is dropped: a buggy client must not be able to tear
                    // down the session. A Panic is a decoder defect, not bad input, and must propagate.
                    case Result.Failure(_) => ()
                    case Result.Panic(ex)  => Abort.panic(ex)
            case HttpWebSocket.Payload.Binary(_) => ()

end UIServer
