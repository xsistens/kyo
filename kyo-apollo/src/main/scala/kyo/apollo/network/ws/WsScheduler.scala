package kyo.apollo.network.ws

import scala.scalajs.js.timers.clearTimeout
import scala.scalajs.js.timers.setTimeout

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

    /** The production scheduler, backed by the JS event loop's `setTimeout` /
      * `clearTimeout`. Cancelling clears the timeout so a revoked idle/ack timer
      * never fires.
      */
    val default: WsScheduler = new WsScheduler:
        def schedule(delayMillis: Long)(task: () => Unit): () => Unit =
            val handle = setTimeout(delayMillis.toDouble)(task())
            () => clearTimeout(handle)
end WsScheduler
