package kyo.apollo.api

import kyo.apollo.json.Json

/** Root of the GraphQL operation hierarchy.
  *
  * Mirrors apollo-kotlin's `Operation` shape: a typed description of a single
  * GraphQL request. `D` is the operation's `data` payload type. The hierarchy
  * is sealed so the three operation kinds below are the only direct subtypes,
  * plus [[Operation.Normalizable]], whose only subtypes are their normalizable
  * forms.
  *
  * Every operation decodes its `data`; only a [[Operation.Normalizable]] one also
  * encodes it, and only such an operation can be written to the normalized cache.
  * An operation over a `.map` projection decodes responses and reads the cache, but
  * the cache interceptor does not normalize its responses.
  */
sealed trait Operation[D]:
    /** The operation name as it appears in the GraphQL document. */
    def name: String

    /** The full GraphQL document text sent over the wire. */
    def document: String

    /** Decodes the `data` payload of type `D` — the seam response decoding
      * ([[GraphQLResponse.parse]]) and a denormalized cache read go through. An
      * operation over a `derives Schema` type supplies [[JsonCodec.fromSchema]]; an
      * inline-query ([[SelectionBuilder]]) operation supplies its structural codec.
      */
    def dataCodec: JsonDecoder[D]

    /** The root field describing this operation's response shape.
      *
      * Its [[CompiledField.selections]] are the operation's top-level fields; the
      * whole tree lets downstream layers normalize responses and compute cache
      * keys without re-parsing the document. Mirrors apollo-kotlin's
      * `Operation.rootField()`.
      */
    def rootField: CompiledField

    /** This operation's variables as a [[Json.JObj]] of `name -> value` pairs.
      *
      * Built per-variable (not by deriving+encoding the whole operation) so the
      * wire behaviour of optionals is preserved exactly: an absent optional
      * variable is written as an explicit `"name":null` (matching the legacy
      * `Adapter[Option]`), whereas kyo-schema's case-class encode would omit a
      * `None`. Each present value is encoded through its own `Schema`. Replaces
      * the legacy `serializeVariables`.
      */
    def variables: Json
end Operation

object Operation:

    /** An operation whose `data` codec also encodes, so its typed data can be
      * normalized into the cache: what [[kyo.apollo.cache.normalized.ApolloStore]]'s
      * write and update operations take. Sealed: its only subtypes are
      * [[Query.Normalizable]], [[Mutation.Normalizable]] and
      * [[Subscription.Normalizable]], so every normalizable operation is still one of
      * the three kinds.
      */
    sealed trait Normalizable[D] extends Operation[D]:
        def dataCodec: JsonCodec[D]
end Operation

/** A read-only GraphQL query. */
trait Query[D] extends Operation[D]

object Query:
    /** A query the normalized cache can write. */
    trait Normalizable[D] extends Query[D] with Operation.Normalizable[D]

/** A GraphQL mutation. */
trait Mutation[D] extends Operation[D]

object Mutation:
    /** A mutation the normalized cache can write. */
    trait Normalizable[D] extends Mutation[D] with Operation.Normalizable[D]

/** A GraphQL subscription. */
trait Subscription[D] extends Operation[D]

object Subscription:
    /** A subscription the normalized cache can write. */
    trait Normalizable[D] extends Subscription[D] with Operation.Normalizable[D]
