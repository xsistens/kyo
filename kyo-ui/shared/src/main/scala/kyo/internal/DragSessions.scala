package kyo.internal

import ReactiveUI.*
import kyo.*
import kyo.Svg
import kyo.UI.*
import kyo.kernel.ContextEffect

private[kyo] object DragSessions:

    private type DragResolver = Maybe[(String, Drag.Decision) => Unit < Async]

    final private[internal] case class DragSession(
        event: Drag.Event,
        terminalResolved: Boolean,
        /** A reading of [[Clock.nowMonotonic]]. */
        expiresAt: Duration,
        resolver: DragResolver
    )

    final private[kyo] case class DragSessionLimits(
        maxSessions: Int = 128,
        lifetime: Duration = 5.minutes
    ):
        require(maxSessions > 0, s"DragSessionLimits.maxSessions must be positive: $maxSessions")
        require(lifetime > Duration.Zero, s"DragSessionLimits.lifetime must be positive: $lifetime")
    end DragSessionLimits

    private[internal] def dragHandle(
        handle: Handler,
        sessions: AtomicRef[Map[String, DragSession]],
        mutex: Meter,
        expiryWake: Channel[Unit],
        limits: DragSessionLimits
    )(using Frame): ValidatedHandler =
        (path, event) =>
            event match
                case event: DragProtocol.ValidatedEvent.Click          => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.ClickSelf      => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.ContextMenu    => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Input          => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Change         => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.ChangeChecked  => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.ChangeNumeric  => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Submit         => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.KeyDown        => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.KeyUp          => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Focus          => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Blur           => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Scroll         => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.ScrollPosition => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.FileSelect     => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.PointerDown    => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.PointerMove    => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.PointerUp      => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Hover          => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Unhover        => safeDispatch(handle, path, event.wire)
                case event: DragProtocol.ValidatedEvent.Attach         => safeDispatch(handle, path, event.wire)
                // A measure reply is the client's answer to a `requestMeasureById`, not an element event: the session
                // completes the pending reply on UI.Commands before dispatch, so one never reaches the handler
                // tree. Bubbling is the right answer for an event no element declared.
                case _: DragProtocol.ValidatedEvent.MeasureById | _: DragProtocol.ValidatedEvent.MeasureMissing => true
                case _: DragProtocol.ValidatedEvent.Start | _: DragProtocol.ValidatedEvent.End |
                    _: DragProtocol.ValidatedEvent.Enter | _: DragProtocol.ValidatedEvent.Leave |
                    _: DragProtocol.ValidatedEvent.Over | _: DragProtocol.ValidatedEvent.Drop |
                    _: DragProtocol.ValidatedEvent.SortMove =>
                    Abort.runPartial[Closed](
                        mutex.run(dragDispatch(handle, sessions, expiryWake, limits, path, event))
                    ).map {
                        case Result.Success(value) => value
                        case Result.Failure(_)     => true
                    }

    private def dragDispatch(
        handle: Handler,
        sessions: AtomicRef[Map[String, DragSession]],
        expiryWake: Channel[Unit],
        limits: DragSessionLimits,
        path: Seq[String],
        event: DragProtocol.ValidatedEvent
    )(using Frame): Boolean < Async =
        event match
            case DragProtocol.ValidatedEvent.Start(wire @ UIEvent.DragStart(_, data), domainItems) =>
                DragCommands.resolveSink.use { resolver =>
                    sessions.get.map { current =>
                        if current.contains(data.sessionId) then
                            DragCommands.resolve(
                                data.sessionId,
                                Drag.Decision.Reject(
                                    Drag.Rejection.Application("A drag session with this identifier is already active.")
                                )
                            ).andThen(true)
                        else if current.size >= limits.maxSessions then
                            DragCommands.resolve(
                                data.sessionId,
                                Drag.Decision.Reject(Drag.Rejection.Application("Too many active drag sessions."))
                            ).andThen(true)
                        else
                            val domain = Drag.Event(
                                data.sessionId,
                                domainItems,
                                data.operation,
                                data.sourceKey,
                                Absent,
                                Present(data.point),
                                data.modifiers,
                                Absent
                            )
                            Clock.nowMonotonic.map { now =>
                                sessions.set(current +
                                    (data.sessionId -> DragSession(
                                        domain,
                                        terminalResolved = false,
                                        now + limits.lifetime,
                                        resolver
                                    )))
                                    .andThen(wakeExpiryScheduler(expiryWake))
                                    .andThen(DragCommands.current.let(Present(DragCommands.Dispatch(
                                        DragCommands.Payload.Event(domain),
                                        Absent
                                    ))) {
                                        safeDispatch(handle, path, event.wire)
                                    })
                            }
                    }
                }
            case DragProtocol.ValidatedEvent.End(wire @ UIEvent.DragEnd(_, data)) =>
                sessions.get.map(_.get(data.sessionId)).map {
                    case Some(session) =>
                        val finalEvent = session.event.copy(operation = data.operation)
                        sessions.getAndUpdate(_ - data.sessionId).unit
                            .andThen(DragCommands.current.let(Present(DragCommands.Dispatch(
                                DragCommands.Payload.End(Drag.End(finalEvent, canceled = data.cancelled)),
                                Absent
                            ))) {
                                safeDispatch(handle, path, wire)
                            })
                    case None => true
                }
            case DragProtocol.ValidatedEvent.Enter(wire) =>
                dispatchTarget(handle, path, wire, wire.event, sessions, terminal = false)
            case DragProtocol.ValidatedEvent.Leave(wire) =>
                dispatchTarget(handle, path, wire, wire.event, sessions, terminal = false)
            case DragProtocol.ValidatedEvent.Over(wire) =>
                dispatchTarget(handle, path, wire, wire.event, sessions, terminal = false)
            case DragProtocol.ValidatedEvent.Drop(wire) =>
                dispatchTarget(handle, path, wire, wire.event, sessions, terminal = true)
            case DragProtocol.ValidatedEvent.SortMove(wire @ UIEvent.SortMove(_, sessionId, move)) =>
                sessions.get.map(_.get(sessionId)).map {
                    case Some(session) if !session.terminalResolved =>
                        for
                            decisions <- AtomicRef.init[DragCommands.DecisionState](DragCommands.DecisionState.None)
                            _         <- DragCommands.current.let(Present(DragCommands.Dispatch(
                                DragCommands.Payload.Move(move),
                                Present(decisions)
                            ))) {
                                safeDispatch(handle, path, wire)
                            }
                            decision <- finalDecision(decisions, "No sort handler accepted the move.")
                            _        <- sessions.getAndUpdate(_ + (sessionId -> session.copy(terminalResolved = true)))
                            _        <- DragCommands.resolve(sessionId, decision)
                        yield true
                    case None =>
                        DragCommands.resolve(
                            sessionId,
                            Drag.Decision.Reject(Drag.Rejection.Application("No sort handler accepted the move."))
                        ).andThen(true)
                    case _ => true
                }
            // Never reaches here: the session completes a measure reply on UI.Commands before dispatch.
            case _: DragProtocol.ValidatedEvent.MeasureById | _: DragProtocol.ValidatedEvent.MeasureMissing => true
            case _: DragProtocol.ValidatedEvent.Click | _: DragProtocol.ValidatedEvent.ClickSelf |
                _: DragProtocol.ValidatedEvent.ContextMenu |
                _: DragProtocol.ValidatedEvent.Input | _: DragProtocol.ValidatedEvent.Change |
                _: DragProtocol.ValidatedEvent.ChangeChecked | _: DragProtocol.ValidatedEvent.ChangeNumeric |
                _: DragProtocol.ValidatedEvent.Submit | _: DragProtocol.ValidatedEvent.KeyDown |
                _: DragProtocol.ValidatedEvent.KeyUp | _: DragProtocol.ValidatedEvent.Focus |
                _: DragProtocol.ValidatedEvent.Blur | _: DragProtocol.ValidatedEvent.Scroll |
                _: DragProtocol.ValidatedEvent.Hover | _: DragProtocol.ValidatedEvent.Unhover |
                _: DragProtocol.ValidatedEvent.PointerDown | _: DragProtocol.ValidatedEvent.PointerMove |
                _: DragProtocol.ValidatedEvent.PointerUp |
                _: DragProtocol.ValidatedEvent.ScrollPosition | _: DragProtocol.ValidatedEvent.FileSelect |
                _: DragProtocol.ValidatedEvent.Attach =>
                safeDispatch(handle, path, event.wire)

    private def wakeExpiryScheduler(expiryWake: Channel[Unit])(using Frame): Unit < Sync =
        Abort.runPartial[Closed](expiryWake.offer(())).unit

    private[internal] def expiryScheduler(
        sessions: AtomicRef[Map[String, DragSession]],
        mutex: Meter,
        expiryWake: Channel[Unit]
    )(using Frame): Unit < (Async & Abort[Closed]) =
        Loop.foreach {
            mutex.run(expireAndArm(sessions)).map { timer =>
                val next: Unit < (Async & Abort[Closed]) = timer match
                    case Present(timer) => Async.race(timer.get, expiryWake.take.unit)
                    case Absent         => expiryWake.take
                Abort.runPartial[Closed](next).map {
                    case Result.Success(_) => Loop.continue
                    case Result.Failure(_) => Loop.done
                }
            }
        }

    // The clock reading, the expiry pass, and the arming of the next timer form one section under the drag mutex, which
    // also serializes every session admission. A clock moved while holding the same mutex never lands between the reading
    // and the arming, so the armed deadline is exactly the earliest remaining expiry.
    private def expireAndArm(sessions: AtomicRef[Map[String, DragSession]])(using Frame): Maybe[Fiber[Unit, Any]] < Async =
        Clock.nowMonotonic.map { now =>
            expireSessions(now, sessions).andThen(sessions.get.map { current =>
                current.valuesIterator.map(_.expiresAt).reduceOption(_.min(_)) match
                    case None            => Maybe.empty
                    case Some(expiresAt) => Clock.sleep(expiresAt.minusOrZero(now)).map(Maybe(_))
            })
        }

    private def expireSessions(now: Duration, sessions: AtomicRef[Map[String, DragSession]])(using Frame): Unit < Async =
        sessions.get.map { current =>
            val (expired, retained) = current.partition((_, session) => session.expiresAt <= now)
            sessions.set(retained).andThen(Kyo.foreach(expired.toSeq) { case (sessionId, session) =>
                if session.terminalResolved then ()
                else
                    session.resolver.fold((): Unit < Async)(_(
                        sessionId,
                        Drag.Decision.Reject(Drag.Rejection.Application("The drag session expired."))
                    ))
            }.unit)
        }

    private def dispatchTarget(
        handle: Handler,
        path: Seq[String],
        wire: UIEvent,
        data: DragProtocol.TargetData,
        sessions: AtomicRef[Map[String, DragSession]],
        terminal: Boolean
    )(using Frame): Boolean < Async =
        sessions.get.map(_.get(data.sessionId)).map {
            case Some(session) if !session.terminalResolved =>
                val domain = session.event.copy(
                    operation = data.operation,
                    targetKey = data.targetKey,
                    point = Present(data.point),
                    modifiers = data.modifiers,
                    position = data.position
                )
                if terminal then
                    for
                        decisions <- AtomicRef.init[DragCommands.DecisionState](DragCommands.DecisionState.None)
                        _         <- sessions.getAndUpdate(_ + (data.sessionId -> session.copy(event = domain)))
                        _         <- DragCommands.current.let(Present(DragCommands.Dispatch(
                            DragCommands.Payload.Event(domain),
                            Present(decisions)
                        ))) {
                            safeDispatch(handle, path, wire)
                        }
                        decision <- finalDecision(decisions, "No drop handler accepted the operation.")
                        _        <- sessions.getAndUpdate(_ + (data.sessionId -> session.copy(event = domain, terminalResolved = true)))
                        _        <- DragCommands.resolve(data.sessionId, decision)
                    yield true
                else
                    sessions.getAndUpdate(_ + (data.sessionId -> session.copy(event = domain))).unit
                        .andThen(DragCommands.current.let(Present(DragCommands.Dispatch(DragCommands.Payload.Event(domain), Absent))) {
                            safeDispatch(handle, path, wire)
                        })
                end if
            case None if terminal =>
                DragCommands.resolve(
                    data.sessionId,
                    Drag.Decision.Reject(Drag.Rejection.Application("No drop handler accepted the operation."))
                ).andThen(true)
            case _ => true
        }

    private def finalDecision(ref: AtomicRef[DragCommands.DecisionState], emptyReason: String)(using Frame): Drag.Decision < Sync =
        ref.get.map {
            case DragCommands.DecisionState.None            => Drag.Decision.Reject(Drag.Rejection.Application(emptyReason))
            case DragCommands.DecisionState.Accepted        => Drag.Decision.Accept
            case DragCommands.DecisionState.Rejected(value) => value
            case DragCommands.DecisionState.Failed(value)   => value
        }

end DragSessions
