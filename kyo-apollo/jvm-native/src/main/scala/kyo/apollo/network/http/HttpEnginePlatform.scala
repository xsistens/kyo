package kyo.apollo.network.http

/** JVM/Native resolution of [[HttpEngine.default]]: the kyo-http-backed engine. The
  * JS/Wasm source root provides the `fetch`-backed counterpart.
  */
object HttpEnginePlatform:
    def default(): HttpEngine = HttpClientEngine()
end HttpEnginePlatform
