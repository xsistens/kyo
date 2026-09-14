package kyo.apollo.devtools

import kyo.*
import kyo.apollo.api.Mutation
import kyo.apollo.api.Query
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.runtime.ResponseStream

/** The head [[ApolloInterceptor]] that feeds a [[DevtoolsOperationStore]]: it
  * records each query/mutation as it flows through the chain and refreshes its
  * status/error from every response emission, without altering the stream.
  *
  * Installed first in the chain, so it observes the final (post-cache) responses.
  * Subscriptions pass through untouched — the devtools Queries/Mutations tabs
  * don't surface them.
  */
final class DevtoolsInterceptor(store: DevtoolsOperationStore) extends ApolloInterceptor:

    def intercept[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        val op = request.operation
        op match
            case _: Mutation[?] =>
                val id = request.requestUuid.toString
                store.startMutation(id, op.name, op.document, op.variables)
                chain.proceed(request).mapPure { response =>
                    store.settleMutation(id, errorOf(response))
                    response
                }
            case _: Query[?] =>
                store.upsertQuery(
                    op.name,
                    op.document,
                    op.variables,
                    networkStatus = 1,
                    error = None,
                    data = None
                )
                chain.proceed(request).mapPure { response =>
                    val data = response.data match
                        case Present(d) => Some(request.operation.dataCodec.encode(d))
                        case Absent     => None
                    store.upsertQuery(
                        op.name,
                        op.document,
                        op.variables,
                        networkStatus = if response.hasErrors then 8 else 7,
                        error = errorOf(response),
                        data = data
                    )
                    response
                }
            case _ =>
                chain.proceed(request)
        end match
    end intercept

    private def errorOf(response: ApolloResponse[?]): Option[String] =
        response.error match
            // GraphQL errors keep the devtools' one-line "; " join rather than the
            // newline-joined Apollo-JS parity message the exception itself carries.
            case Present(gql: ApolloGraphQLException) => Some(gql.errors.map(_.message).mkString("; "))
            case Present(ex)                          => Some(ex.message)
            case Absent                               => None
end DevtoolsInterceptor
