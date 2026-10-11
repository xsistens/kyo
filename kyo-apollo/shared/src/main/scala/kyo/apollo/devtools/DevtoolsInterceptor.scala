package kyo.apollo.devtools

import kyo.*
import kyo.apollo.api.Mutation
import kyo.apollo.api.Operation
import kyo.apollo.api.Query
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.runtime.ResponseStream

/** The head [[ApolloInterceptor]] that feeds a [[DevtoolsOperationStore]]: it
  * records each query/mutation as it flows through the chain and refreshes its
  * status/error from every response emission, without altering the stream.
  *
  * Installed first in the chain, so it observes the final (post-cache) responses.
  * Subscriptions pass through untouched — the devtools Queries/Mutations tabs
  * don't surface them. A query's data is recorded in its response shape, which
  * takes an [[Operation.Normalizable]] codec; the data of a query built from a
  * `.map` projection only decodes and is recorded as absent. Recording is part of the execution: an operation is
  * recorded when its stream runs, under the request id minted for that run, and
  * an intercepted stream that never runs records nothing.
  *
  * Every recorded operation's variables pass through `redact` before they reach
  * the store. The default, [[DevtoolsInterceptor.redactMutationVariables]], keeps
  * the shape of a mutation's variables and replaces every value, because a
  * mutation's variables are the credentials path (`login(email, password)`); a
  * query's variables are recorded as they are.
  *
  * WARNING: the store is what the devtools extension reads, and the devtools hook
  * exposes it to every script in the document. A custom `redact` replaces the
  * default entirely, so one that returns a mutation's variables unchanged puts
  * them there in plain text. Query data and query variables are never redacted.
  *
  * @param store  where the recorded operations go
  * @param redact the variables recorded for an operation, given the operation and
  *               its variables (defaults to [[DevtoolsInterceptor.redactMutationVariables]])
  * @see [[DevtoolsOperationStore]] the store this interceptor feeds
  * @see [[DevtoolsInterceptor.redactValues]] the value redaction the default applies
  */
final class DevtoolsInterceptor(
    store: DevtoolsOperationStore,
    redact: (Operation[?], Json) => Json = DevtoolsInterceptor.redactMutationVariables
) extends ApolloInterceptor:

    def intercept[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        val op = request.operation
        op match
            case _: Mutation[?] =>
                val id = request.requestUuid.toString
                Stream.unwrap(Sync.defer {
                    store.startMutation(id, op.name, op.document, redact(op, op.variables))
                    chain.proceed(request).map { response =>
                        Sync.defer {
                            store.settleMutation(id, errorOf(response))
                            response
                        }
                    }
                })
            case _: Query[?] =>
                Stream.unwrap(Sync.defer {
                    val variables = redact(op, op.variables)
                    store.upsertQuery(op.name, op.document, variables, networkStatus = 1, error = None, data = None)
                    chain.proceed(request).map { response =>
                        Sync.defer {
                            val data = (response.data, op) match
                                case (Present(d), normalizable: Operation.Normalizable[D]) =>
                                    Some(normalizable.dataCodec.encode(d))
                                case _ => None
                            store.upsertQuery(
                                op.name,
                                op.document,
                                variables,
                                networkStatus = if response.hasErrors then 8 else 7,
                                error = errorOf(response),
                                data = data
                            )
                            response
                        }
                    }
                })
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

object DevtoolsInterceptor:

    /** What a redacted value is recorded as. */
    val Redacted: String = "<redacted>"

    /** The default redaction: a mutation's variables with every value replaced by
      * [[Redacted]] ([[redactValues]]), any other operation's variables unchanged.
      */
    val redactMutationVariables: (Operation[?], Json) => Json =
        (operation, variables) =>
            operation match
                case _: Mutation[?] => redactValues(variables)
                case _              => variables

    /** `json` with its shape kept and its values dropped: every string, number
      * (`JInt`, `JDec`, `JNum`), boolean and upload becomes the string [[Redacted]];
      * object field names, array lengths and `null`s stay.
      */
    def redactValues(json: Json): Json =
        json match
            case Json.JObj(fields) => Json.JObj(fields.map((name, value) => name -> redactValues(value)))
            case Json.JArr(items)  => Json.JArr(items.map(redactValues))
            case Json.JNull        => Json.JNull
            case Json.JStr(_) | Json.JInt(_) | Json.JDec(_) | Json.JNum(_) | Json.JBool(_) | Json.JUpload(_) =>
                Json.JStr(Redacted)
end DevtoolsInterceptor
