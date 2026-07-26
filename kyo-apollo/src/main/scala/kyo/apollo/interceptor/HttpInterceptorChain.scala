package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

/** The continuation an [[HttpInterceptor]] calls to pass a request further down
  * the HTTP layer. Each `proceed` advances to the next interceptor, or — once
  * the interceptors are exhausted — to the terminal
  * [[kyo.apollo.network.http.HttpEngine]].
  */
trait HttpInterceptorChain:

    /** Continue processing `request` at the next link, completing with its
      * [[HttpResponse]].
      */
    def proceed(request: HttpRequest)(using Frame): HttpResponse < Async
end HttpInterceptorChain

/** The default [[HttpInterceptorChain]]: an immutable cursor over an ordered
  * `interceptors` list that terminates at `engine`.
  *
  * `proceed` invokes `interceptors(index)` with a chain positioned at the next
  * index; when `index` runs off the end, it calls `engine.execute`. Because the
  * cursor is a fresh value each step, the chain is re-entrant and carries no
  * mutable position — the same shape apollo-kotlin's `DefaultHttpInterceptorChain`
  * uses.
  *
  * @param interceptors the HTTP interceptors, applied in list order
  * @param index        the current position (start a run at `0`)
  * @param engine       the terminal wire round-trip
  */
final class DefaultHttpInterceptorChain(
    interceptors: Chunk[HttpInterceptor],
    index: Int,
    engine: HttpEngine
) extends HttpInterceptorChain:

    def proceed(request: HttpRequest)(using Frame): HttpResponse < Async =
        if index < interceptors.length then
            interceptors(index).intercept(
                request,
                DefaultHttpInterceptorChain(interceptors, index + 1, engine)
            )
        else engine.execute(request)
end DefaultHttpInterceptorChain

object HttpInterceptorChain:

    /** Present `interceptors` wrapped around a terminal `engine` as a single
      * [[HttpEngine]]. Because an interceptor chain is itself just
      * `HttpRequest => Future[HttpResponse]`, this lets the existing
      * [[kyo.apollo.network.http.HttpNetworkTransport]] run through the full HTTP
      * interceptor stack unchanged — the client (Task 7) builds the engine here
      * and hands it to the transport. With no interceptors it is `engine` verbatim.
      */
    def asEngine(
        interceptors: List[HttpInterceptor],
        engine: HttpEngine
    ): HttpEngine =
        if interceptors.isEmpty then engine
        else
            val chain = Chunk.from(interceptors)
            new HttpEngine:
                def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
                    DefaultHttpInterceptorChain(chain, 0, engine).proceed(request)
end HttpInterceptorChain
