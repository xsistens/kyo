package kyo.apollo.network.ws

/** How long to wait before the Nth reconnection attempt after a WebSocket drop.
  *
  * The [[WebSocketNetworkTransport]] consults a `WsBackoff` each time its
  * [[WebSocketNetworkTransport reconnectWhen]] predicate has agreed to reopen a
  * dropped socket: `delayMillis(attempt)` gives the pause (armed on the same
  * [[WsScheduler]] seam the ack/idle timeouts use) before attempt number
  * `attempt` runs. Attempts are **1-based** and reset to zero once a reconnection
  * successfully re-acknowledges, so a socket that flaps briefly does not inherit
  * a long backoff from an earlier outage.
  *
  * Kept as a tiny injectable strategy (rather than hard-coded constants) so the
  * client builder can expose it and the transport tests can assert exact delays
  * against a manual scheduler.
  */
trait WsBackoff:

    /** The delay in milliseconds before reconnection `attempt` (1-based). */
    def delayMillis(attempt: Long): Long
end WsBackoff

object WsBackoff:

    /** Exponential backoff: `baseMillis * factor^(attempt-1)`, capped at
      * `maxMillis`. With the defaults this yields 1s, 2s, 4s, 8s, … up to 30s.
      *
      * @param baseMillis the delay before the first attempt
      * @param maxMillis  the ceiling the growing delay is clamped to
      * @param factor     the multiplier applied per additional attempt
      */
    def exponential(
        baseMillis: Long = 1000L,
        maxMillis: Long = 30000L,
        factor: Double = 2.0
    ): WsBackoff = new WsBackoff:
        def delayMillis(attempt: Long): Long =
            if attempt <= 1L then baseMillis
            else
                val scaled = baseMillis.toDouble * math.pow(factor, (attempt - 1).toDouble)
                math.min(scaled, maxMillis.toDouble).toLong

    /** A fixed delay before every attempt — the simplest sensible policy. */
    def constant(delayMillis: Long): WsBackoff =
        val fixed = delayMillis
        new WsBackoff:
            def delayMillis(attempt: Long): Long = fixed
    end constant

    /** The default policy: exponential backoff from 1s, doubling, capped at 30s. */
    val default: WsBackoff = exponential()
end WsBackoff
