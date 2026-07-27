package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.runtime.ResponseStream

/** The continuation an [[ApolloInterceptor]] calls to pass a request further
  * down the operation layer. Each `proceed` advances to the next interceptor;
  * the chain must be terminated by an interceptor (e.g. [[NetworkInterceptor]])
  * that produces responses without proceeding.
  */
trait ApolloInterceptorChain:

    /** Continue processing `request` at the next interceptor, returning its stream
      * of [[ApolloResponse]] values.
      */
    def proceed[D](request: ApolloRequest[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ResponseStream[D]
end ApolloInterceptorChain

/** The default [[ApolloInterceptorChain]]: an immutable cursor over an ordered
  * `interceptors` list.
  *
  * `proceed` invokes `interceptors(index)` with a chain positioned at the next
  * index. Unlike the HTTP chain there is no separate terminal engine — the last
  * interceptor is expected to be terminal (a [[NetworkInterceptor]]) and must not
  * call `chain.proceed`. Running off the end therefore signals a wiring error and
  * throws, rather than returning a value: it is a programming mistake, not a
  * network/parse failure (those still travel as `ApolloResponse.exception`).
  *
  * @param interceptors the Apollo interceptors, applied in list order
  * @param index        the current position (start a run at `0`)
  */
final class DefaultApolloInterceptorChain(
    interceptors: Chunk[ApolloInterceptor],
    index: Int
) extends ApolloInterceptorChain:

    def proceed[D](request: ApolloRequest[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ResponseStream[D] =
        if index >= interceptors.length then
            throw new IllegalStateException(
                "ApolloInterceptorChain exhausted: the final interceptor must be " +
                    "terminal (e.g. NetworkInterceptor) and must not call chain.proceed()."
            )
        end if
        interceptors(index).intercept(
            request,
            DefaultApolloInterceptorChain(interceptors, index + 1)
        )
    end proceed
end DefaultApolloInterceptorChain
