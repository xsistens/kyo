package kyo.apollo.api

import kyo.Schema
import kyo.apollo.json.Json

/** Root of the GraphQL operation hierarchy.
  *
  * Mirrors apollo-kotlin's `Operation` shape: a typed description of a single
  * GraphQL request. `D` is the operation's `data` payload type. The hierarchy
  * is sealed so the three operation kinds below are the only direct subtypes.
  */
sealed trait Operation[D]:
    /** The operation name as it appears in the GraphQL document. */
    def name: String

    /** The full GraphQL document text sent over the wire. */
    def document: String

    /** kyo-schema codec that decodes/encodes the `data` payload of type `D`.
      *
      * Replaces the legacy reader/writer `kyo.apollo.adapter.Adapter`: the generated
      * `Data` case class `derives Schema`, so this is simply `summon[Schema[D]]`.
      * Schema-derived operations supply only this; the actual read/write seam is
      * [[dataCodec]], which defaults to wrapping it.
      */
    def dataSchema: Schema[D]

    /** The bidirectional JSON codec response decoding ([[GraphQLResponse.parse]])
      * and cache (de)normalization ([[kyo.apollo.cache.normalized.ApolloStore]])
      * actually go through.
      *
      * Defaults to [[JsonCodec.fromSchema]] over [[dataSchema]], so schema-derived
      * operations need only supply `dataSchema`. Inline-query ([[SelectionBuilder]])
      * operations override this directly with a structural codec — they have no
      * `Schema` for their named-tuple result — and never touch `dataSchema`.
      */
    def dataCodec: JsonCodec[D] = JsonCodec.fromSchema(using dataSchema)

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

/** A read-only GraphQL query. */
trait Query[D] extends Operation[D]

/** A GraphQL mutation. */
trait Mutation[D] extends Operation[D]

/** A GraphQL subscription. */
trait Subscription[D] extends Operation[D]
