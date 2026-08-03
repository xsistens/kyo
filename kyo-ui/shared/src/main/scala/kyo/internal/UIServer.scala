package kyo.internal

import kyo.*

private[kyo] object UIServer:

    private def normalizePath(basePath: String): String =
        if basePath.endsWith("/") then basePath.dropRight(1) else basePath

    def handlers(basePath: String)(ui: => UI < Async)(using Frame): Seq[HttpHandler[?, ?, ?]] < Sync =
        val base = normalizePath(basePath)
        Sync.defer(Seq(
            getPage(base, basePath, Sync.defer(ui)),
            wsRoute(base, Sync.defer(ui))
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

    private[kyo] def serveSession(ws: HttpWebSocket, ui: => UI < Async)(using Frame): Unit < (Async & Abort[Closed]) =
        Scope.run {
            for
                uiTree <- ui
                root   <- ReactiveUI.normalize(uiTree, Seq.empty)
                // Pre-seed the connection's sent-class tracking with every pseudo-state class the
                // initial SSR page already carries (rendered once more here, discarding the HTML), so
                // the first reactive update touching an unchanged pseudo-styled element does not
                // redundantly re-inject a rule the page's initial <style> block already has.
                (_, initialRules) <- HtmlRenderer.renderWithCss(uiTree, Seq.empty)
                exchange = wsExchange(root, ws, initialRules.map(_._1).toSet)
                // Session command sink: an event handler calling UI.scrollIntoView sends the op over this
                // connection's socket, riding the same channel as the reactive updates. runPartial drops
                // only a Closed (the socket closed, so the command is moot); a Panic propagates.
                scrollSink = (id: String) =>
                    Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](HtmlOp.ScrollIntoView(id))))).unit
                // The session's imperative op channel; `emit` serializes an HtmlOp over the socket (a Closed mid-send
                // drops the op). Env.run must wrap subscribe + dispatch so the forked region/mount fibers inherit
                // Env[Commands] and resolve `UI.commands` at run time.
                commands <- UI.Commands.init(op =>
                    Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit
                )
                _ <- Env.run(commands) {
                    UICommands.scrollSink.let(Present(scrollSink)) {
                        for
                            sub <- ReactiveUI.subscribe(root, exchange)
                            // Single-consumer drain: element handlers run in ARRIVAL order on one fiber, the twin of
                            // the browser mount's drain in DomBackend. A fiber forked per event let two handlers race,
                            // so a blur and the focus that followed it could be applied in either order.
                            //
                            // The reader loop still never blocks on a handler: it offers and moves on. The deadlock the
                            // fork existed to avoid stays avoided, because the measure replies in dispatchEvent are
                            // delivered INLINE on the reader; a handler parked on a reply is therefore parked on the
                            // drain, and the reader is still free to read the frame that completes it.
                            events <- Channel.init[Unit < Async](256)
                            // runPartial captures only a Closed (the channel closed with the session -> stop draining);
                            // a Panic propagates rather than passing for a clean end. Fiber.init binds the drain to the
                            // session Scope, so it is interrupted when the connection ends.
                            _ <- Fiber.init(
                                Loop.foreach(Abort.runPartial[Closed](events.take).map {
                                    case Result.Success(eff) => eff.andThen(Loop.continue)
                                    case Result.Failure(_)   => Loop.done
                                })
                            )
                            _ <- Async.race(
                                ws.stream.foreach(payload => dispatchEvent(sub.handle, commands, events, payload)),
                                ws.onPeerClose
                            )
                        yield ()
                    }
                }
            yield ()
        }

    private def wsRoute(base: String, ui: => UI < Async)(using Frame): HttpHandler[?, ?, ?] =
        HttpHandler.webSocket(s"$base/_kyo/ws") { (_, ws) =>
            serveSession(ws, ui)
        }

    private def wsExchange(root: ReactiveUI, ws: HttpWebSocket, seenClasses: Set[String])(using Frame): UIExchange =
        new UIExchange:
            // Pseudo-state CSS classes already carried by this connection's <style> (seeded from the
            // initial SSR page, then grown by every InjectCss this exchange sends), so a later
            // re-render reusing one of these classes never re-sends its rule. Connection-scoped: each
            // WS session gets its own set, matching the session-scoped subscription tree this exchange
            // already belongs to.
            private val sentClasses = scala.collection.mutable.Set.from(seenClasses)

            // runPartial drops only a Closed (the socket closed mid-render -> the op is moot); a Panic
            // propagates to the region fiber rather than being swallowed by the discard.
            private def send(op: HtmlOp)(using Frame): Unit < Async =
                Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit

            /** Send `op`, preceded by the pseudo-state rules this connection has not seen yet.
              *
              * The rules go FIRST so the element never paints unstyled between the two frames, and each is
              * recorded so a later paint reusing the class does not re-send it.
              */
            private def sendStyled(rules: Chunk[(String, String)], op: HtmlOp)(using Frame): Unit < Async =
                val newRules = rules.filterNot(r => sentClasses.contains(r._1))
                if newRules.isEmpty then send(op)
                else
                    newRules.foreach(r => sentClasses += r._1)
                    send(HtmlOp.InjectCss(newRules.map(_._2).mkString)).andThen(send(op))
                end if
            end sendStyled

            def onChange(path: Seq[String], ui: UI, mount: Boolean)(using Frame): Unit < Async =
                // Content at its nested-reactive sub-path (matches SSR/walkStatic). The payload is the region's
                // bare content fragment: the client patches between the region's live comment markers, which are
                // never re-sent. `mountSlot = true` stamps the `s` flag on Mounted placeholders so the client
                // morph tells the same mount from colliding content (twin of DomBackend.onChange).
                HtmlRenderer.renderWithCss(ui, HtmlRenderer.contentPath(path, ui), mountSlot = true).map { (html, rules) =>
                    sendStyled(rules, HtmlOp.Replace(path, html, mount))
                }

            /** Render ONLY the rows flagged changed, and send the row order alongside them.
              *
              * Same trade as `DomBackend`'s override, except the saving is bandwidth rather than parse time:
              * dropping one row of a thousand puts a few hundred bytes on the wire where the whole-fragment
              * Replace put the entire rendered list. Retained rows are named by key and never rendered, so
              * their live DOM — including in-place channel patches that were never part of any payload —
              * survives untouched.
              *
              * No fallback here, and none is possible: once the untouched rows have been left out of the
              * frame, the client has nothing to rebuild them from. `ReactiveUI` therefore only emits a list
              * patch for an emission whose rows are addressable at all (see its `addressable` gate); this
              * override is reached solely for those.
              */
            override def onListPatch(path: Seq[String], rows: Seq[ListRow])(using Frame): Unit < Async =
                val changed = rows.filter(_.changed)
                Kyo.foreach(Chunk.from(changed))(row =>
                    HtmlRenderer.renderWithCss(row.ui, path :+ row.key, mountSlot = true)
                ).map { rendered =>
                    val op = HtmlOp.PatchList(path, rows.map(_.key), changed.map(_.key), rendered.map(_._1).mkString)
                    sendStyled(rendered.flatMap(_._2), op)
                }
            end onListPatch

            override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
                send(HtmlOp.SetAttrByPath(path, name, value))
            override def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async =
                send(HtmlOp.SetBoolAttrByPath(path, name, value))
            override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
                send(HtmlOp.SetClassByPath(path, name, on))

    private def dispatchEvent(
        handle: (Seq[String], UIEvent) => Boolean < Async,
        commands: UI.Commands,
        events: Channel[Unit < Async],
        payload: HttpWebSocket.Payload
    )(using Frame): Unit < Async =
        payload match
            case HttpWebSocket.Payload.Text(data) =>
                Json.decode[UIEvent](data) match
                    case Result.Success(event) =>
                        event match
                            // Measure replies are not element events: route to the session channel to complete the
                            // pending requestMeasure reply (not the ReactiveUI handler tree).
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
                            // Element events go to the session's single-consumer drain, not inline on this loop: a
                            // handler that SUSPENDS, e.g. awaiting a value-returning `requestMeasure` whose reply arrives
                            // as a LATER inbound frame, must not block this loop from reading that reply (that would
                            // deadlock). Handing them to ONE consumer instead of forking a fiber each is what keeps their
                            // arrival order, which a blur followed by the focus that replaced it depends on. The measure
                            // replies above stay inline: they only complete a Promise, never suspend, must run promptly,
                            // and are what unparks a handler waiting on the drain. `offer` rather than `put` keeps the
                            // reader non-blocking; a full drain means a backlog of 256 handlers and drops the event, the
                            // same trade the browser mount's drain makes.
                            case _ => Abort.runPartial[Closed](events.offer(handle(event.path, event).unit)).unit
                    // A malformed inbound frame (DecodeException) is dropped: a buggy client must not be able to tear
                    // down the session. A Panic is a decoder defect, not bad input, and must propagate.
                    case Result.Failure(_) => ()
                    case Result.Panic(ex)  => Abort.panic(ex)
            case HttpWebSocket.Payload.Binary(_) => ()

end UIServer
