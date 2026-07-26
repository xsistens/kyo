package kyo.apollo

import kyo.apollo.network.ExecutionContext

/** How the strict effect form (`ApolloCall.data`) treats GraphQL `errors` that
  * accompany a response — the react-apollo `errorPolicy`.
  *
  * Like [[kyo.apollo.cache.normalized.FetchPolicy]], an `ErrorPolicy` is an
  * [[ExecutionContext.Element]] keyed by its own companion, so it rides the
  * per-request context bag: `ApolloCall.errorPolicy(p)` attaches it and the
  * `.data` effect reads it back with `context.get(ErrorPolicy)`. When none is
  * attached, [[ErrorPolicy.Default]] (`None`) applies.
  *
  * It affects ONLY `.data` (the strict, `Abort`-raising path). `.response` always
  * returns the full [[kyo.apollo.network.ApolloResponse]] — `data`, `errors`, and
  * `exception` as values — regardless of policy, so it needs no `errorPolicy`.
  *
  *   - `None`   — a response carrying GraphQL `errors` raises them on `Abort`
  *                (the strict default; no data is returned).
  *   - `Ignore` — GraphQL `errors` are discarded: `.data` returns the `data` if
  *                present (raising only on a transport error or absent data).
  *   - `All`    — `.data` likewise returns the `data` if present; the errors are
  *                still observable via `.response`. (For `.data`, which yields only
  *                `D`, `All` behaves like `Ignore`; the distinction — *seeing* the
  *                errors — is served by `.response`.)
  *
  * A transport/parse `exception` always raises on `Abort`, under every policy.
  */
enum ErrorPolicy extends ExecutionContext.Element:
    case None, Ignore, All

    /** Every case is keyed by the [[ErrorPolicy]] companion in the context bag. */
    override def key: ExecutionContext.Key[ErrorPolicy] = ErrorPolicy
end ErrorPolicy

object ErrorPolicy extends ExecutionContext.Key[ErrorPolicy]:
    /** The policy applied when a call pins none — strict, like apollo-kotlin. */
    val Default: ErrorPolicy = None
