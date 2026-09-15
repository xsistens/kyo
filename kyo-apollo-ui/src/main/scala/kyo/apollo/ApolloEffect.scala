package kyo.apollo

// getkyo.io library. This binding shares `package kyo.apollo` with `core` (the
// compat model — like `kyo-http` joining `kyo-core`'s `package kyo`), so
// `import kyo.*` resolves to the library root and pulls in Kyo's
// `<` / `Async` / `Abort` / `Scope` / `Frame` / `Sync`.
import kyo.*
import kyo.apollo.ApolloCall
import kyo.apollo.ApolloClient
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloExecuteFailure
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.Uuid

/** The **effect / one-shot** half of the `kyo-ui` binding (Phase 09, Task 4).
  *
  * Provides the extension methods a single `import kyo.apollo.*` brings onto
  * every [[kyo.apollo.ApolloCall]] so app code turns a prepared operation into a Kyo
  * effect with no per-operation glue:
  *
  *   - `call.data` — run once, yield the typed `data` or a typed
  *     [[kyo.apollo.exception.ApolloException]] on Kyo's `Abort` channel;
  *   - `call.response` — run once, yield the full [[ApolloResponse]]
  *     (partial data + GraphQL errors preserved), no `Abort`.
  *
  * ==How it runs==
  *
  * `core` stays Kyo-free but is already effect-native: an [[ApolloCall]]'s
  * `execute` is `ApolloResponse[D] < (Async & Scope)` over the cold interceptor
  * stream (take-the-first-emission). These extensions only discharge that call's
  * `Scope` (`Scope.run`) and shape the outcome — no `Future` bridge is involved.
  *
  * ==Failures are values, projected onto `Abort` explicitly==
  *
  * Per `core`'s contract, transport / HTTP / parse problems arrive **inside**
  * `ApolloResponse.error` (a value); the effect itself fails only for a
  * genuine wiring error (an exhausted chain, or an empty stream). Mapping a
  * response's failure onto Kyo's typed `Abort[ApolloException]` channel is
  * therefore an explicit projection we choose ([[ApolloEffect.projectData]]),
  * never a caught panic:
  *
  *   - `response` runs `call.execute` under `Scope.run` and `Abort.run[Throwable]`,
  *     folding any wiring failure/panic back into an `ApolloResponse.error`
  *     value — so its effect set is just `Async`, matching the "failures are
  *     values" contract.
  *   - `data` reuses that response and then projects: a present `error` — whether a
  *     transport failure or the server's own GraphQL errors — or an absent `data`
  *     becomes an `Abort.fail` rather than a throw. Callers that want partial data
  *     alongside its errors use `response`.
  */
extension [D](call: ApolloCall[D])

    /** Execute this operation once and yield its typed `data`.
      *
      * A one-shot effect: `D < (Async & Abort[ApolloException])`. Success yields
      * the decoded payload; a transport/parse `exception`, any GraphQL `errors`,
      * or missing `data` surface as a typed [[kyo.apollo.exception.ApolloException]] on
      * the `Abort` channel (never a thrown exception). This is the strict path —
      * for a response that keeps partial data alongside its errors, use
      * [[response]].
      *
      * Honors the call's [[kyo.apollo.ErrorPolicy]] (react `errorPolicy`): under
      * `Ignore`/`All` the GraphQL `errors` are not raised — the `data` is returned if
      * present (a transport error or absent data still raises). `None` (default) is
      * the strict behavior above.
      */
    def data(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): D < (Async & Abort[ApolloException]) =
        val policy = call.requestBuilder.executionContext.get(ErrorPolicy).getOrElse(ErrorPolicy.Default)
        call.response.map(resp => Abort.get(ApolloEffect.projectData(resp, policy)))
    end data

    /** Execute this operation once and yield its full [[ApolloResponse]].
      *
      * `ApolloResponse[D] < Async` — no `Abort`. The response preserves partial
      * `data`, GraphQL `errors`, and any transport `exception` as values, so the
      * caller inspects them directly. A genuine wiring failure of `call.execute`
      * (an exhausted interceptor chain, or a stream that completes with no
      * emission) is folded back into an `ApolloResponse.error` value, keeping
      * this path total.
      */
    def response(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ApolloResponse[D] < Async =
        // `call.execute` is now a Kyo effect (`ApolloResponse[D] < (Async & Scope)`);
        // its own `Scope` (a live watcher/subscription binds teardown there — inert
        // for a one-shot query) is discharged with `Scope.run`. A wiring failure (an
        // empty stream's `NoSuchElementException`, an exhausted chain) surfaces on the
        // async PANIC channel, so — per Slice 1's panic-fold rule — `Abort.run` folds
        // BOTH failure and panic back into an `ApolloResponse.error` value
        // (ordinary transport errors already arrive as response values).
        Abort.run[Throwable](Scope.run(call.execute)).map {
            case Result.Success(resp) => resp
            case Result.Failure(cause) =>
                Uuid.random.map(ApolloResponse.fromException[D](_, ApolloEffect.asApolloException(cause)))
            case Result.Panic(cause) =>
                Uuid.random.map(ApolloResponse.fromException[D](_, ApolloEffect.asApolloException(cause)))
        }

end extension

/** Helpers backing the effect-form extension methods, kept off the extension
  * itself so they are unit-testable and reused by the reactive form (Task 5).
  */
object ApolloEffect:

    /** Project a response onto the strict one-shot outcome the `.data` effect
      * yields, under the given [[kyo.apollo.ErrorPolicy]]: `Right(data)` for a clean
      * response, `Left(error)` when a transport failure is present or `data` is
      * absent. A transport failure always wins. The server's GraphQL `errors` map to
      * `Left` only under `ErrorPolicy.None` (the default); under `Ignore`/`All` they
      * are discarded and the `data` is returned if present. Total over `Either`
      * rather than a throw.
      *
      * The two are told apart by the shape of the single `error` channel: an
      * [[ApolloGraphQLException]] is the server having answered with errors,
      * anything else is a transport failure, which no policy suppresses.
      */
    private[kyo] def projectData[D](
        resp: ApolloResponse[D],
        errorPolicy: ErrorPolicy = ErrorPolicy.None
    )(using Frame): Either[ApolloException, D] =
        def noData = DefaultApolloException("The server did not return any data")
        resp.error match
            case Present(gql: ApolloGraphQLException) =>
                errorPolicy match
                    case ErrorPolicy.None                     => Left(gql)
                    case ErrorPolicy.Ignore | ErrorPolicy.All => resp.data.toRight(noData)
            case Present(ex) => Left(ex)
            case Absent      => resp.data.toRight(noData)
        end match
    end projectData

    /** Normalize an arbitrary wiring-failure cause (a failed/panicked `execute`)
      * into an [[kyo.apollo.exception.ApolloException]] value: an `ApolloException`
      * passes through unchanged, anything else is wrapped in a
      * [[DefaultApolloException]] carrying it as the cause. Used only for the rare
      * wiring failure, since ordinary transport errors already arrive as response
      * values.
      */
    private[kyo] def asApolloException(cause: Throwable)(using Frame): ApolloException =
        cause match
            case ae: ApolloException => ae
            case other =>
                DefaultApolloException(
                    "Apollo call failed before producing a response",
                    other
                )

    /** Narrow `effect`'s failures to the execute row a refetch registered with the
      * client's [[ActiveQueryRegistry]] must carry: an [[ApolloExecuteFailure]] passes
      * through unchanged, any other [[ApolloException]] (a cache read failure, which a
      * `NetworkOnly` fetch does not produce) becomes a [[DefaultApolloException]]
      * carrying it as the cause, and a panic stays a panic.
      */
    private[kyo] def asExecuteFailure[A, S](effect: A < (S & Abort[ApolloException]))(using
        Frame
    ): A < (S & Abort[ApolloExecuteFailure]) =
        Abort.recover[ApolloException] { (e: ApolloException) =>
            e match
                case execute: ApolloExecuteFailure => Abort.fail(execute)
                case other                         => Abort.fail(DefaultApolloException(other.message, other))
        }(effect)
end ApolloEffect

/** `Scope`-owned creation of an [[kyo.apollo.ApolloClient]].
  *
  * A client owns the shared subscription WebSocket; creating it through
  * [[ApolloClient.init]] ties that socket to the enclosing `Scope`, which closes the
  * client when it exits — rather than relying on a caller to remember `close`.
  */
object ApolloClientResource:

    /** Create a client for `config` owned by the current `Scope` — bind it inside a
      * `Scope.run { … }` and the WebSocket transport is cleaned up when that block
      * completes, even on failure. The same as [[ApolloClient.init]].
      */
    def acquire(config: ApolloClient.Config)(using Frame): ApolloClient < (Sync & Scope) =
        ApolloClient.init(config)
end ApolloClientResource
