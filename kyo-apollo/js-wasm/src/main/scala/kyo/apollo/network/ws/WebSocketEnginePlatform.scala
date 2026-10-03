package kyo.apollo.network.ws

import kyo.Duration

/** JS/Wasm resolution of [[WebSocketEngine.default]]: the browser/Node `WebSocket`
  * engine. The JVM/Native source root provides the kyo-http-backed counterpart.
  */
object WebSocketEnginePlatform:
    def default(connectTimeout: Duration): WebSocketEngine = JsWebSocketEngine(connectTimeout)
end WebSocketEnginePlatform
