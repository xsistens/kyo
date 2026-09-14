package kyo.apollo.network.ws

import kyo.Duration

/** JVM/Native resolution of [[WebSocketEngine.default]]: the kyo-http-backed engine.
  * The JS/Wasm source root provides the browser `WebSocket`-backed counterpart.
  */
object WebSocketEnginePlatform:
    def default(connectTimeout: Duration): WebSocketEngine = KyoHttpWebSocketEngine(connectTimeout)
end WebSocketEnginePlatform
