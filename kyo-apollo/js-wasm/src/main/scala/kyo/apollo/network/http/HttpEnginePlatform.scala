package kyo.apollo.network.http

/** JS/Wasm resolution of [[HttpEngine.default]]: the browser/Node `fetch` engine.
  * The JVM/Native source root provides the kyo-http-backed counterpart.
  */
object HttpEnginePlatform:
    def default(): HttpEngine = FetchHttpEngine()
end HttpEnginePlatform
