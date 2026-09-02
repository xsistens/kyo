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
                exchange = wsExchange(ws, initialRules.map(_._1).toSet)
                // Session command sink: an event handler calling UI.scrollIntoView sends the op over this
                // connection's socket, riding the same channel as the reactive updates. runPartial drops
                // only a Closed (the socket closed, so the command is moot); a Panic propagates.
                scrollSink = (id: String) =>
                    Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](HtmlOp.ScrollIntoView(id))))).unit
                resolveSink = (sessionId: String, decision: Drag.Decision) =>
                    Abort.runPartial[Closed](
                        ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](HtmlOp.ResolveDrag(sessionId, decision))))
                    ).unit
                files <- DragFiles.Service.init(op =>
                    Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit
                )
                // Peer close (or any session end) fails every pending file read with Disconnected.
                _ <- Scope.ensure(files.close())
                // The session's imperative op channel; `emit` serializes an HtmlOp over the socket (a Closed mid-send
                // drops the op). Env.run must wrap subscribe + dispatch so the forked region/mount fibers inherit
                // Env[Commands] and resolve `UI.commands` at run time.
                commands <- UI.Commands.init(op =>
                    Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit
                )
                _ <- Env.run(commands) {
                    UICommands.scrollSink.let(Present(scrollSink)) {
                        DragCommands.resolveSink.let(Present(resolveSink)) {
                            DragFiles.local.let(Present(files)) {
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
                                        ws.stream.foreach(payload =>
                                            dispatchEvent(sub.handleValidated, commands, events, files, payload)
                                        ),
                                        ws.onPeerClose
                                    )
                                yield ()
                            }
                        }
                    }
                }
            yield ()
        }

    private def wsRoute(base: String, ui: => UI < Async)(using Frame): HttpHandler[?, ?, ?] =
        HttpHandler.webSocket(s"$base/_kyo/ws") { (_, ws) =>
            serveSession(ws, ui)
        }

    private def wsExchange(ws: HttpWebSocket, seenClasses: Set[String])(using Frame): UIExchange =
        new UIExchange:
            // Both collections below are written CONCURRENTLY: subscribeScoped forks one fiber per reactive
            // region and every one of them calls into this same exchange, so a plain mutable Map or Set here
            // is a data race that can corrupt the table under a rehash, not merely lose an entry.

            // Pseudo-state CSS classes already carried by this connection's <style> (seeded from the
            // initial SSR page, then grown by every InjectCss this exchange sends), so a later
            // re-render reusing one of these classes never re-sends its rule. Connection-scoped: each
            // WS session gets its own set, matching the session-scoped subscription tree this exchange
            // already belongs to.
            private val sentClasses = new java.util.concurrent.ConcurrentHashMap[String, java.lang.Boolean]
            seenClasses.foreach(c => discard(sentClasses.put(c, java.lang.Boolean.TRUE)))

            /** Per reactive region, the HTML last sent for each node BELOW it, so an update re-sends only the
              * nodes whose rendered bytes actually changed.
              *
              * Rendered bytes, not the AST: a node's rendering is not a function of its AST alone. An element
              * bound to a `SignalRef` re-renders from the ref read at render time and its AST is the same
              * object on every edit, so an AST comparison would drop real updates. Connection-scoped, like
              * `sentClasses`. Each region writes only its own key and replaces that key's value wholesale, so
              * a structural change cannot leave stale paths behind.
              */
            private val sentBelowRegion =
                new java.util.concurrent.ConcurrentHashMap[Seq[String], Map[Seq[String], String]]

            def onChange(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                ui: UI
            )(using Frame): Unit < Async =
                val plan = UIDiff.plan(path, previous, ui)
                if plan.size == 1 && plan.head._1 == path then
                    // The whole region: sent unconditionally. The client compares against the LIVE DOM before
                    // applying, which is what repairs a field the user has typed into; suppressing this here
                    // would take that repair away.
                    discard(sentBelowRegion.remove(path))
                    sendRegion(region, path, contentContext, parentContext, ui)
                else
                    // Only the nodes that moved. For a chart on a 1 Hz signal that is the marks group: the
                    // background, axes, gridlines and legend render byte-identically to the tick before and
                    // are not sent at all. An unchanged re-render of the whole region sends nothing.
                    val previouslySent = Option(sentBelowRegion.get(path)).getOrElse(Map.empty)
                    Kyo.foreach(plan) { (opPath, subtree) =>
                        HtmlRenderer.renderWithCss(subtree, opPath).map((html, rules) => (opPath, html, rules))
                    }.map { rendered =>
                        discard(sentBelowRegion.put(path, rendered.map((opPath, html, _) => (opPath, html)).toMap))
                        val changed = rendered.filterNot((opPath, html, _) => previouslySent.get(opPath).contains(html))
                        Kyo.foreachDiscard(changed)((opPath, html, rules) => send(HtmlOp.Replace(opPath, html), rules))
                    }
                end if
            end onChange

            /** Answer a keyed list emission with the row ORDER and the render of only the rows that changed.
              *
              * Removing one row of two hundred put every other row on the socket to say it. Now a removal and a
              * reorder carry no rendered row at all, and a change carries the rows that changed. The changed rows
              * go as ONE payload rather than one per row, so a full replacement keeps the single bulk parse it
              * has today. `ReactiveUI` has already refused an emission whose rows are not addressable by key, so
              * there is nothing to check here and nothing to fall back to: that decision cannot be taken late,
              * because a frame that left the untouched rows out gives the client nothing to rebuild them from.
              */
            override def onListPatch(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                rows: Seq[ListRow]
            )(using Frame): Unit < Async =
                region match
                    case ReactiveRegion.HtmlRange(id) =>
                        val host = ReactiveRegion.renderHost(
                            region,
                            parentContext,
                            ReactiveRegion.tableContent(rows.iterator.map(_.ui))
                        )
                        val changedRows = rows.filter(_.changed)
                        Kyo.foreach(Chunk.from(changedRows)) { row =>
                            HtmlRenderer.renderRowWithCss(row.ui, path :+ row.key, contentContext.child(row.key), host)
                        }.map { rendered =>
                            val html  = rendered.map(_._1).mkString
                            val rules = rendered.flatMap(_._2).toSeq
                            send(
                                HtmlOp.PatchList(id, rows.map(_.key), changedRows.map(_.key), html),
                                rules
                            )
                        }
                    // An SVG region replaces its own group element; there is no row range to address.
                    case _: ReactiveRegion.SvgElement =>
                        super.onListPatch(region, path, contentContext, parentContext, previous, rows)
                end match
            end onListPatch

            override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
                val op = HtmlOp.SetAttrByPath(path, name, value)
                Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit
            override def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async =
                val op = HtmlOp.SetBoolAttrByPath(path, name, value)
                Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit
            override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
                val op = HtmlOp.SetClassByPath(path, name, on)
                Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit

            /** Render and send the whole region. The region kind picks the op: an HTML region replaces the
              * content between its comment anchors (`ReplaceRange`), so the replacement parses in its actual
              * parent context; an SVG region replaces its own `<g>` boundary at its path. A descendant sent by
              * `onChange`'s diff branch renders its own tag, with its own `data-kyo-path`, which is what the
              * client resolves the op against.
              */
            private def sendRegion(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                ui: UI
            )(using Frame): Unit < Async =
                val boundaryMode =
                    if ReactiveRegion.owns(region, contentContext) then ReactiveRegion.BoundaryMode.Suppress
                    else ReactiveRegion.BoundaryMode.Emit
                HtmlRenderer.renderRegionWithCss(ui, path, contentContext, region, parentContext, boundaryMode).map {
                    (html, rules) =>
                        val replaceOp = region match
                            case ReactiveRegion.HtmlRange(id) => HtmlOp.ReplaceRange(id, html)
                            case _: ReactiveRegion.SvgElement => HtmlOp.Replace(path, HtmlRenderer.wrapReactiveRegion(region, html))
                        send(replaceOp, rules)
                }
            end sendRegion

            /** Emit one op, preceded by any pseudo-state rule it introduces. */
            private def send(op: HtmlOp, rules: Seq[(String, String)])(using Frame): Unit < Async =
                val newRules = rules.filterNot(r => sentClasses.containsKey(r._1))
                // runPartial drops only a Closed (the socket closed mid-render -> the op is moot); a Panic
                // propagates to the region fiber rather than being swallowed by the discard.
                val sendReplace =
                    Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](op)))).unit
                if newRules.isEmpty then sendReplace
                else
                    newRules.foreach(r => discard(sentClasses.put(r._1, java.lang.Boolean.TRUE)))
                    val injectOp = HtmlOp.InjectCss(newRules.map(_._2).mkString)
                    // Send the new pseudo-state rule(s) before the replace that introduces the class
                    // referencing them, so the element never paints unstyled between the two frames.
                    Abort.runPartial[Closed](ws.put(HttpWebSocket.Payload.Text(Json.encode[HtmlOp](injectOp)))).unit
                        .andThen(sendReplace)
                end if
            end send

    private def dispatchEvent(
        handle: (Seq[String], DragProtocol.ValidatedEvent) => Boolean < Async,
        commands: UI.Commands,
        events: Channel[Unit < Async],
        files: DragFiles.Service,
        payload: HttpWebSocket.Payload
    )(using
        Frame
    ): Unit < Async =
        def dispatch(event: UIEvent): Unit < Async =
            DragProtocol.validateEventAndDomain(event, DragProtocol.Limits.default) match
                // Measure replies are not element events: they complete the pending `requestMeasure` on the
                // session's UI.Commands rather than entering the ReactiveUI handler tree. They stay INLINE on
                // the reader: they only complete a Promise, never suspend, and are what unparks a handler
                // waiting on the drain below.
                case Result.Success(DragProtocol.ValidatedEvent.Measure(m)) =>
                    commands.deliverMeasure(
                        m.path,
                        UI.Rect(m.rectX, m.rectY, m.rectW, m.rectH, m.viewportW, m.viewportH)
                    )
                // Self-addressing: the id-addressed measure reply routes by `id` to the id-keyed pending map.
                case Result.Success(DragProtocol.ValidatedEvent.MeasureById(m)) =>
                    commands.deliverMeasureById(
                        m.id,
                        UI.Rect(m.rectX, m.rectY, m.rectW, m.rectH, m.viewportW, m.viewportH)
                    )
                // Drop and sort dispatch forks: their handlers may await lazy file reads served by later
                // frames on this same socket loop, so running them inline would deadlock the session. The
                // client models concurrent decisions (AwaitingDecisionAfterEnd), and session close unblocks
                // a forked handler because the file service fails its pending reads with Disconnected.
                case Result.Success(validated: (DragProtocol.ValidatedEvent.Drop | DragProtocol.ValidatedEvent.SortMove)) =>
                    Fiber.initUnscoped(handle(event.path, validated).unit).unit
                // Every other element event goes to the session's single-consumer drain, not inline on this
                // loop: a handler that SUSPENDS, e.g. awaiting a value-returning `requestMeasure` whose reply
                // arrives as a LATER inbound frame, must not block this loop from reading that reply. Handing
                // them to ONE consumer instead of forking a fiber each is what keeps their arrival order,
                // which a blur followed by the focus that replaced it depends on. `offer` rather than `put`
                // keeps the reader non-blocking; a full drain means a backlog of 256 handlers and drops the
                // event, the same trade the browser mount's drain makes.
                case Result.Success(validated) =>
                    Abort.runPartial[Closed](events.offer(handle(event.path, validated).unit)).unit
                case _ => ()
        payload match
            case HttpWebSocket.Payload.Text(data) =>
                Json.decode[UIEvent](data) match
                    case Result.Success(event) => dispatch(event)
                    // Not a bare event: the drag runtime posts ClientMessage envelopes; unwrap Event values and
                    // route validated file transfer responses to the session's read service. A malformed inbound
                    // frame is dropped: a buggy client must not be able to tear down the session. A Panic is a
                    // decoder defect, not bad input, and must propagate.
                    case Result.Failure(_) =>
                        Json.decode[DragProtocol.ClientMessage](data) match
                            case Result.Success(DragProtocol.ClientMessage.Event(event)) => dispatch(event)
                            case Result.Success(message) =>
                                DragProtocol.validate(message, DragProtocol.Limits.default) match
                                    case Result.Success(validated) => files.deliver(validated)
                                    case _                         => ()
                            case Result.Failure(_) => ()
                            case Result.Panic(ex)  => Abort.panic(ex)
                    case Result.Panic(ex) => Abort.panic(ex)
            case HttpWebSocket.Payload.Binary(_) => ()
        end match
    end dispatchEvent

end UIServer
