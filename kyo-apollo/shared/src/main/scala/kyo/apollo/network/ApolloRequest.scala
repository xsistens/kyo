package kyo.apollo.network

import kyo.<
import kyo.Absent
import kyo.Frame
import kyo.HttpHeaders
import kyo.HttpMethod
import kyo.Maybe
import kyo.Present
import kyo.Sync
import kyo.apollo.api.Operation

/** An immutable envelope around one [[Operation]] plus the per-request execution
  * metadata the transport needs — one execution of an operation.
  *
  * This carries **no serialization logic**: the body is composed by
  * [[kyo.apollo.network.http.HttpRequestComposer]] (Task 3), which reads these
  * fields and delegates to [[kyo.apollo.api.OperationRequestBody]] — the single
  * source of request-body serialization. The APQ flags ([[sendApqExtensions]] /
  * [[sendDocument]]) are declared here so the composer can add persisted-query
  * `extensions` and optionally omit `query` without `OperationRequestBody`
  * needing to know about APQ.
  *
  * A request is a plain value: its [[requestUuid]] is an argument like any other,
  * so two requests built from equal arguments are equal. A fresh id is minted
  * when an execution starts — by [[ApolloRequest.Builder.build]], under `Sync` —
  * never by constructing the value.
  *
  * @param operation            the operation to execute
  * @param requestUuid          a correlation id echoed onto [[ApolloResponse]]
  * @param httpHeaders          per-request headers, order-preserving (duplicates kept)
  * @param httpMethod           overrides the client default when set
  * @param sendApqExtensions    emit the APQ `persistedQuery` extension
  * @param sendDocument         include the full `query` document on the wire
  * @param optimisticData       the value a normalized cache overlays while a
  *                             mutation is in flight (typed as the operation's data)
  * @param executionContext     out-of-band metadata for interceptors
  */
final case class ApolloRequest[D](
    operation: Operation[D],
    requestUuid: Uuid,
    httpHeaders: HttpHeaders = HttpHeaders.empty,
    httpMethod: Maybe[HttpMethod] = Absent,
    sendApqExtensions: Boolean = false,
    sendDocument: Boolean = true,
    optimisticData: Maybe[D] = Absent,
    executionContext: ExecutionContext = ExecutionContext.Empty
) derives CanEqual:

    /** A fluent [[ApolloRequest.Builder]] seeded with this request's values, id included. */
    def newBuilder: ApolloRequest.Builder[D] =
        ApolloRequest.Builder(
            operation,
            Present(requestUuid),
            httpHeaders,
            httpMethod,
            sendApqExtensions,
            sendDocument,
            optimisticData,
            executionContext
        )
end ApolloRequest

object ApolloRequest:

    /** Start a fluent builder for `operation` with all defaults applied and no id. */
    def builder[D](operation: Operation[D]): Builder[D] =
        Builder(operation, Absent, HttpHeaders.empty, Absent, false, true, Absent, ExecutionContext.Empty)

    /** Ergonomic, fluent construction of an [[ApolloRequest]] — the description of an
      * execution that has not started yet.
      *
      * A builder is an immutable value: each setter returns a new builder, so one
      * builder can seed any number of executions (an `ApolloCall` holds one). Calls
      * chain (`ApolloRequest.builder(op).httpMethod(HttpMethod.GET).addHttpHeader(...).build`),
      * which reads far better than one large positional/`copy` expression when headers
      * and context accumulate incrementally — this mirrors apollo-kotlin's
      * `ApolloRequest.Builder`.
      *
      * [[build]] is where an execution gets its id: with no [[requestUuid]] set, each
      * run of `build` mints a fresh one.
      */
    final class Builder[D] private[ApolloRequest] (
        val operation: Operation[D],
        uuid: Maybe[Uuid],
        headers: HttpHeaders,
        method: Maybe[HttpMethod],
        apq: Boolean,
        document: Boolean,
        optimistic: Maybe[D],
        context: ExecutionContext
    ):

        /** The context accumulated so far — what interceptors will read. */
        def executionContext: ExecutionContext = context

        /** Pin the id every request this builder builds carries. */
        def requestUuid(value: Uuid): Builder[D] = copy(uuid = Present(value))

        def httpMethod(value: HttpMethod): Builder[D] = copy(method = Present(value))

        def httpHeaders(value: HttpHeaders): Builder[D] = copy(headers = value)

        /** Append a single header, preserving any already present. */
        def addHttpHeader(name: String, value: String): Builder[D] =
            copy(headers = headers.add(name, value))

        def sendApqExtensions(value: Boolean): Builder[D] = copy(apq = value)

        def sendDocument(value: Boolean): Builder[D] = copy(document = value)

        /** The value a normalized cache overlays while the mutation is in flight. */
        def optimisticData(value: D): Builder[D] = copy(optimistic = Present(value))

        def executionContext(value: ExecutionContext): Builder[D] = copy(context = value)

        /** Merge additional context on top of what is already set. */
        def addExecutionContext(value: ExecutionContext): Builder[D] =
            copy(context = context ++ value)

        /** The request for one execution: carries the pinned [[requestUuid]] if one is
          * set, and otherwise a fresh id minted when this effect runs — so every run is
          * a distinct request.
          */
        def build(using Frame): ApolloRequest[D] < Sync =
            uuid match
                case Present(id) => withId(id)
                case Absent      => Uuid.random.map(withId)

        private def withId(id: Uuid): ApolloRequest[D] =
            ApolloRequest(operation, id, headers, method, apq, document, optimistic, context)

        private def copy(
            uuid: Maybe[Uuid] = this.uuid,
            headers: HttpHeaders = this.headers,
            method: Maybe[HttpMethod] = this.method,
            apq: Boolean = this.apq,
            document: Boolean = this.document,
            optimistic: Maybe[D] = this.optimistic,
            context: ExecutionContext = this.context
        ): Builder[D] =
            new Builder(operation, uuid, headers, method, apq, document, optimistic, context)
    end Builder
end ApolloRequest
