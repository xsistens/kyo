package kyo.apollo.network

import kyo.apollo.api.Operation

/** An immutable envelope around one [[Operation]] plus the per-request execution
  * metadata the transport needs.
  *
  * This carries **no serialization logic**: the body is composed by
  * [[kyo.apollo.network.http.HttpRequestComposer]] (Task 3), which reads these
  * fields and delegates to [[kyo.apollo.api.OperationRequestBody]] — the single
  * source of request-body serialization. The APQ flags ([[sendApqExtensions]] /
  * [[sendDocument]]) are declared here so the composer can add persisted-query
  * `extensions` and optionally omit `query` without `OperationRequestBody`
  * needing to know about APQ.
  *
  * @param operation            the operation to execute
  * @param requestUuid          a correlation id echoed onto [[ApolloResponse]]
  * @param httpHeaders          per-request headers, order-preserving
  * @param httpMethod           overrides the client default when set
  * @param sendApqExtensions    emit the APQ `persistedQuery` extension
  * @param sendDocument         include the full `query` document on the wire
  * @param executionContext     out-of-band metadata for interceptors
  */
final case class ApolloRequest[D](
    operation: Operation[D],
    requestUuid: Uuid = Uuid.random(),
    httpHeaders: List[HttpHeader] = Nil,
    httpMethod: Option[HttpMethod] = None,
    sendApqExtensions: Boolean = false,
    sendDocument: Boolean = true,
    executionContext: ExecutionContext = ExecutionContext.Empty
):

    /** A fluent [[ApolloRequest.Builder]] seeded with this request's values. */
    def newBuilder: ApolloRequest.Builder[D] = new ApolloRequest.Builder(this)
end ApolloRequest

object ApolloRequest:

    /** Start a fluent builder for `operation` with all defaults applied. */
    def builder[D](operation: Operation[D]): Builder[D] =
        new Builder(ApolloRequest(operation))

    /** Ergonomic, fluent construction over the immutable [[ApolloRequest]].
      *
      * Each setter returns `this` after `copy`-ing the wrapped request, so calls
      * chain (`ApolloRequest.builder(op).httpMethod(Post).addHttpHeader(...).build()`).
      * A builder is offered alongside `copy` because callers accumulate headers
      * and context incrementally, which reads far better as a chain than as one
      * large positional/`copy` expression — this mirrors apollo-kotlin's
      * `ApolloRequest.Builder`.
      */
    final class Builder[D] private[ApolloRequest] (private var request: ApolloRequest[D]):

        def requestUuid(value: Uuid): this.type = update(_.copy(requestUuid = value))

        def httpMethod(value: HttpMethod): this.type =
            update(_.copy(httpMethod = Some(value)))

        def httpHeaders(value: List[HttpHeader]): this.type =
            update(_.copy(httpHeaders = value))

        /** Append a single header, preserving any already present. */
        def addHttpHeader(name: String, value: String): this.type =
            update(r => r.copy(httpHeaders = r.httpHeaders :+ HttpHeader(name, value)))

        def sendApqExtensions(value: Boolean): this.type =
            update(_.copy(sendApqExtensions = value))

        def sendDocument(value: Boolean): this.type =
            update(_.copy(sendDocument = value))

        def executionContext(value: ExecutionContext): this.type =
            update(_.copy(executionContext = value))

        /** Merge additional context on top of what is already set. */
        def addExecutionContext(value: ExecutionContext): this.type =
            update(r => r.copy(executionContext = r.executionContext ++ value))

        /** The accumulated, immutable request. */
        def build(): ApolloRequest[D] = request

        private def update(f: ApolloRequest[D] => ApolloRequest[D]): this.type =
            request = f(request)
            this
    end Builder
end ApolloRequest
