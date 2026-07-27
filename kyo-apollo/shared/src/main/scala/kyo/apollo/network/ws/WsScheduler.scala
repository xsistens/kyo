package kyo.apollo.network.ws

import kyo.*

/** Schedules a one-shot delayed task — the single timer seam the WebSocket layer
  * depends on.
  *
  * [[WebSocketNetworkTransport]] needs exactly two kinds of delay: a
  * connection-acknowledgement timeout (fail the handshake if `connection_ack`
  * never arrives) and an idle timeout (close the shared socket a while after the
  * last subscription ends). Task 5's reconnection backoff reuses the same seam.
  * Factoring it out keeps those behaviours deterministically testable — the
  * transport tests inject a manual scheduler and fire timers by hand instead of
  * waiting on wall-clock delays, exactly as the engine/connection seam lets them
  * script socket frames.
  *
  * A scheduled task returns a **cancel thunk** (`() => Unit`) so a pending timer
  * can be revoked (an idle close cancelled by a fresh subscription, an ack timer
  * cancelled by a received ack). The platform-timer machinery stays callback /
  * `Future`-based behind this seam — the effect pivot (Schritt 2.2) makes the
  * public transport contract Kyo-native (`Stream`) while the internal socket +
  * timer plumbing stays a proven single-threaded state machine, so deterministic
  * manual-scheduler tests keep firing timers by hand.
  */
trait WsScheduler:

    /** Run `task` once after `delayMillis`. Returns a thunk that cancels the
      * pending run when called before it fires (a no-op once it has fired).
      */
    def schedule(delayMillis: Long)(task: () => Unit): () => Unit
end WsScheduler

object WsScheduler:

    /** The production scheduler, backed by a `Clock`-driven kyo fiber (portable
      * across JS, Wasm, JVM and Native — no `js.timers`). `schedule` forks a fiber
      * that sleeps `delayMillis` then runs `task`; the returned thunk interrupts it,
      * so a revoked idle/ack timer never fires.
      */
    val default: WsScheduler = new WsScheduler:
        import kyo.AllowUnsafe.embrace.danger
        private given Frame = Frame.internal

        def schedule(delayMillis: Long)(task: () => Unit): () => Unit =
            val fiber = Sync.Unsafe.evalOrThrow(
                Fiber.initUnscoped(Async.delay(delayMillis.millis)(Sync.defer(task())))
            )
            () => discard(Sync.Unsafe.evalOrThrow(fiber.interrupt))
        end schedule
end WsScheduler
