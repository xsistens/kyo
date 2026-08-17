package kyo.uic

import kyo.*
import kyo.UI.*

/** One message queued into the [[ToastService]] store — Prime's
  * `ToastMessageOptions`: severity tint, summary/detail text pair, optional
  * `life` (timed auto-dismiss), and the close-button opt-out.
  */
final case class ToastMessage(
    severity: Severity = Severity.Info,
    summary: Maybe[String] = Absent,
    detail: Maybe[String] = Absent,
    life: Maybe[Duration] = Absent,
    closable: Boolean = true
)

/** ToastService — Prime's multi-toast store (`ToastService.add(...)` queueing
  * several messages into one stacked region) as a kyo Env/Layer service.
  *
  * Provide [[ToastService.layer]] ONCE at the application root (the
  * cross-cutting-service doctrine: dependencies ride the `Env` effect,
  * discharged at the root — `Env.runAll(Memo.run(Layer.init[ToastService](
  * ToastService.layer).run))(app)`), place [[ToastService.render]]'s region
  * once in the tree, and `add` messages from anywhere that resolved the
  * service.
  *
  * Resolution point: kyo-ui event handlers erase their effect and run on the
  * event-drain fiber — in the browser transport a root `Env.runAll` reaches
  * them as fiber context, but server-push dispatch runs on kyo-http session
  * fibers WITHOUT that context. Portable code therefore resolves the service
  * at BUILD time (`Env.get[ToastService].map(svc => ...)` while constructing
  * the UI — the demo's pattern) and closes over the instance in handlers.
  *
  * `add` assigns a monotonically increasing id (a counter inside the service —
  * the render stays pure: no clock, no randomness) and, when `life` is set,
  * forks the timed removal from an unscoped fiber (`Async.sleep` then
  * `remove`). This is the service half of [[Node]]'s fiber-ownership rule: a
  * message's life spans re-renders of the region, so no render's `Scope` can own
  * it and the service owns the cancellation instead. Each queued message also
  * carries its remaining `life` as `data-uic-life` (ms) for the client runtime.
  */
trait ToastService:
    /** Read-only view of the queued messages, oldest first — the render region
      * subscribes here.
      */
    def signal: Signal[Seq[ToastService.Queued]]

    /** Queues `msg` (assigns its id) and, when `msg.life` is set, forks the
      * timed removal fiber inside the service.
      */
    def add(msg: ToastMessage)(using Frame): Unit < Async

    /** Convenience `add`: severity + summary + detail with Prime's default 3s life. */
    def add(severity: Severity, summary: String, detail: String)(using Frame): Unit < Async =
        add(ToastMessage(severity, Present(summary), Present(detail), Present(3.seconds)))

    /** Removes the queued message with `id` (no-op when already gone). */
    def remove(id: Int)(using Frame): Unit < Sync

    /** Removes every queued message. */
    def clear(using Frame): Unit < Sync
end ToastService

object ToastService:

    /** A queued toast: the service-assigned `id` plus the message. */
    final case class Queued(id: Int, message: ToastMessage) derives CanEqual

    /** The service layer — holds the queue (`SignalRef[Seq[Queued]]`) and the id
      * counter internally. Provide once at the application root.
      */
    def layer(using Frame): Layer[ToastService, Sync] = Layer(init)

    /** A fresh service instance (the layer's construction — exposed for runners
      * that wire services manually).
      */
    def init(using Frame): ToastService < Sync =
        for
            queue <- Signal.initRef(Seq.empty[Queued])
            seq   <- AtomicInt.init(0)
        yield Live(queue, seq)

    final private class Live(queue: SignalRef[Seq[Queued]], seq: AtomicInt) extends ToastService:
        def signal: Signal[Seq[Queued]] = queue

        def add(msg: ToastMessage)(using Frame): Unit < Async =
            for
                id <- seq.incrementAndGet
                _  <- queue.updateAndGet(_ :+ Queued(id, msg)).unit
                _ <- msg.life match
                    case Present(d) =>
                        Fiber.initUnscoped(Async.sleep(d).andThen(remove(id))).unit
                    case Absent => (): Unit < Async
            yield ()

        def remove(id: Int)(using Frame): Unit < Sync = queue.updateAndGet(_.filterNot(_.id == id)).unit

        def clear(using Frame): Unit < Sync = queue.set(Seq.empty)
    end Live

    /** The stacked toast region: resolves the service from `Env`, subscribes to
      * its queue, and renders every queued message into one fixed
      * `div.p-toast.p-component.p-toast-<position>` region (Prime's anatomy —
      * one region, many `div.p-toast-message` children). Place it once, near the
      * app root.
      */
    def render(position: OverlayPosition = OverlayPosition.BottomRight)(using
        Frame
    ): UI < Env[ToastService] =
        Env.get[ToastService].map(svc => wired(svc, position))

    /** The stacked region for an already-resolved service instance — for code
      * that did `Env.get[ToastService]` earlier in its build (the portable
      * build-time resolution pattern).
      */
    def render(svc: ToastService, position: OverlayPosition)(using Frame): UI =
        wired(svc, position)

    /** The subscription region for a resolved service (golden-test seam /
      * manual wiring).
      */
    private[uic] def wired(svc: ToastService, position: OverlayPosition)(using Frame): UI =
        svc.signal.render[UI] { queued =>
            body(queued, position, id => svc.remove(id))
        }

    /** Pure render of the region for a queue state (golden-test seam). */
    private[uic] def body(
        queued: Seq[Queued],
        position: OverlayPosition,
        remove: Int => Any < Async
    )(using Frame): UI =
        if queued.isEmpty then UI.empty
        else
            div
                .cssClass("p-toast")
                .cssClass("p-component")
                .cssClass(s"p-toast-${position.token}")
                .role("alert")
                .aria("live", "polite")(
                    queued.map(q => toChild(messageUI(q, position, remove)))*
                )

    private def messageUI(q: Queued, position: OverlayPosition, remove: Int => Any < Async)(using Frame): UI =
        val msg      = q.message
        val token    = Toast.severityToken(msg.severity)
        val icon: UI = GlyphSvg(Toast.severityIcon(msg.severity), "p-toast-message-icon")
        val textParts: List[UI] =
            msg.summary.toList.map(t => span.cssClass("p-toast-summary")(t)) ++
                msg.detail.toList.map(t => div.cssClass("p-toast-detail")(t))
        val text: UI = div.cssClass("p-toast-message-text")(textParts.map(toChild)*)
        val closeBtn: List[UI] =
            if !msg.closable then Nil
            else
                List(
                    button
                        .cssClass("p-toast-close-button")
                        .jsProp("type", "button")
                        .aria("label", "Close")
                        .onClick(remove(q.id))(toChild(GlyphSvg(Icons.times, "p-toast-close-icon")))
                )
        var message = div
            .cssClass("p-toast-message")
            .cssClass(s"p-toast-message-$token")
            // Enter slides a queued message in from the region's screen edge and fades;
            // removing it plays Prime's own `p-toast-message-leave-active` keyframe on
            // the leave ghost.
            .enterTransition(Toast.enterClass(position))
            .leaveTransition("p-toast-message-leave-active")
            .data("uic-toast-id", q.id.toString)
        msg.life.foreach(d => message = message.data("uic-life", d.toMillis.toString))
        message(
            toChild(
                div.cssClass("p-toast-message-content")(
                    ((icon :: text :: closeBtn)).map(toChild)*
                )
            )
        )
    end messageUI
end ToastService
