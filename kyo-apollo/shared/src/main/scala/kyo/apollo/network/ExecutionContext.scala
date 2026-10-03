package kyo.apollo.network

import kyo.Maybe

/** A type-indexed, immutable bag of context elements threaded through a request
  * and echoed onto its response.
  *
  * Interceptors (Task 5) attach out-of-band metadata here — a cache policy, a
  * request-timing marker, an auth token — without widening [[ApolloRequest]] /
  * [[ApolloResponse]] for every new concern. Each element is keyed by a typed
  * [[ExecutionContext.Key]] so retrieval is type-safe: `ctx.get(SomeKey)`
  * returns `Maybe[ThatElementType]`.
  *
  * Modeled on apollo-kotlin's `ExecutionContext` (itself a pared-down analog of
  * Kotlin's `CoroutineContext`). Deliberately minimal for Phase 03: `Empty`,
  * typed `get`, and element/context union via `+` / `++`.
  */
final class ExecutionContext private (
    private val elements: Map[ExecutionContext.Key[?], ExecutionContext.Element]
) derives CanEqual:

    /** Two contexts are equal when they hold equal elements under the same keys, so a
      * request carrying context stays comparable as a value.
      */
    override def equals(other: Any): Boolean =
        other match
            case that: ExecutionContext => elements.equals(that.elements)
            case _                      => false

    override def hashCode: Int = elements.hashCode

    /** The element registered under `key`, typed as its element type. */
    def get[E <: ExecutionContext.Element](key: ExecutionContext.Key[E]): Maybe[E] =
        Maybe.fromOption(elements.get(key)).map(_.asInstanceOf[E])

    /** True if `key` has an element in this context. */
    def contains(key: ExecutionContext.Key[?]): Boolean = elements.contains(key)

    /** Add (or replace) a single element, keyed by its own [[Element.key]]. */
    def +(element: ExecutionContext.Element): ExecutionContext =
        new ExecutionContext(elements.updated(element.key, element))

    /** Merge another context on top of this one (right-hand elements win). */
    def ++(other: ExecutionContext): ExecutionContext =
        if other.elements.isEmpty then this
        else new ExecutionContext(elements ++ other.elements)

    /** True when no elements are present. */
    def isEmpty: Boolean = elements.isEmpty
end ExecutionContext

object ExecutionContext:

    /** The context with no elements — the default for every request/response. */
    val Empty: ExecutionContext = new ExecutionContext(Map.empty)

    /** A typed lookup key for an [[Element]]. Implementations are typically the
      * element's companion `object`.
      */
    trait Key[E <: Element]

    /** A single piece of context. Each element knows its own [[key]] so it can be
      * placed into (and retrieved from) an [[ExecutionContext]].
      */
    trait Element:
        def key: Key[?]
end ExecutionContext
