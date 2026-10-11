package kyo.apollo

import kyo.apollo.network.ExecutionContext

/** How GraphQL `errors` that accompany a response are treated when the response is
  * reduced to its data: the react-apollo `errorPolicy`.
  *
  * Like [[kyo.apollo.cache.normalized.FetchPolicy]], an `ErrorPolicy` is an
  * [[ExecutionContext.Element]] keyed by its own companion, so it rides the
  * per-request context bag: `ApolloCall.errorPolicy(p)` attaches it and a consumer
  * reads it back with `context.get(ErrorPolicy)`. When none is attached,
  * [[ErrorPolicy.Default]] (`None`) applies.
  *
  * This module does not reduce responses itself. `ApolloCall.stream`, `execute` and
  * `watch()` always return the full [[kyo.apollo.network.ApolloResponse]] with
  * `data`, `errors` and `exception` as values, whatever the policy. The policy is
  * for a layer on top that turns a response into either its data or a failure:
  *
  *   - `None`: a response carrying GraphQL `errors` fails with them, and no data is
  *     returned (the strict default).
  *   - `Ignore`: GraphQL `errors` are discarded and the `data` is returned if
  *     present.
  *   - `All`: the `data` is returned if present, as under `Ignore`; the errors stay
  *     visible on the full response.
  *
  * A transport or parse `exception` fails under every policy.
  */
enum ErrorPolicy extends ExecutionContext.Element derives CanEqual:
    case None, Ignore, All

    /** Every case is keyed by the [[ErrorPolicy]] companion in the context bag. */
    override def key: ExecutionContext.Key[ErrorPolicy] = ErrorPolicy
end ErrorPolicy

object ErrorPolicy extends ExecutionContext.Key[ErrorPolicy]:
    /** The policy applied when a call pins none: strict, like apollo-kotlin. */
    val Default: ErrorPolicy = None
