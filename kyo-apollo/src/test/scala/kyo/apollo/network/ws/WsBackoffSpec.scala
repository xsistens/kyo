package kyo.apollo.network.ws

/** Unit-tests the pure [[WsBackoff]] policies the [[WebSocketNetworkTransport]]
  * consults before each reconnection attempt. Attempts are 1-based.
  */
class WsBackoffSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "WsBackoff" - {

        "exponential grows base * factor^(attempt-1) and clamps at the ceiling" in {
            val backoff = WsBackoff.exponential(baseMillis = 1000L, maxMillis = 30000L, factor = 2.0)
            assert(backoff.delayMillis(1) == 1000L)
            assert(backoff.delayMillis(2) == 2000L)
            assert(backoff.delayMillis(3) == 4000L)
            assert(backoff.delayMillis(4) == 8000L)
            assert(backoff.delayMillis(5) == 16000L)
            // 32000 would exceed the 30s ceiling, so it clamps.
            assert(backoff.delayMillis(6) == 30000L)
            assert(backoff.delayMillis(100) == 30000L)
        }

        "exponential treats attempt <= 1 as the base delay" in {
            val backoff = WsBackoff.exponential(baseMillis = 500L)
            assert(backoff.delayMillis(0) == 500L)
            assert(backoff.delayMillis(1) == 500L)
        }

        "constant returns the same delay for every attempt" in {
            val backoff = WsBackoff.constant(750L)
            assert(backoff.delayMillis(1) == 750L)
            assert(backoff.delayMillis(2) == 750L)
            assert(backoff.delayMillis(9) == 750L)
        }

        "the default policy is exponential from 1s" in {
            assert(WsBackoff.default.delayMillis(1) == 1000L)
            assert(WsBackoff.default.delayMillis(2) == 2000L)
        }
    }
end WsBackoffSpec
