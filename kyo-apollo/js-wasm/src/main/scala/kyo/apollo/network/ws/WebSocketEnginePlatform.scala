package kyo.apollo.network.ws

/** JS/Wasm resolution of [[WebSocketEngine.default]]: the browser/Node `WebSocket`
  * engine. The JVM/Native source root provides the kyo-http-backed counterpart.
  */
object WebSocketEnginePlatform:
    def default(): WebSocketEngine = JsWebSocketEngine()
end WebSocketEnginePlatform
