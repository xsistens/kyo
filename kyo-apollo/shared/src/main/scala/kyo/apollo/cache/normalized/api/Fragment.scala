package kyo.apollo.cache.normalized.api

import kyo.Schema
import kyo.apollo.api.*
import kyo.apollo.json.Json
import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.Empty
import scala.collection.immutable.VectorMap

/** A compiled selection fragment — a reusable subset of an operation's selection
  * tree that can be read from or written to a *specific* [[CacheKey]] without a
  * full operation.
  *
  * Where an [[kyo.apollo.api.Operation]] is rooted at a well-known operation key
  * (`QUERY_ROOT`/…) and describes a whole response, a `Fragment` is rooted at an
  * arbitrary entity key the caller supplies (e.g. `User:1`) and describes only
  * the fields of that one object. That is exactly what powers targeted cache
  * access: [[ApolloStore.readFragment]] denormalizes the record under a
  * `CacheKey` through this fragment's selection tree, and
  * [[ApolloStore.writeFragment]] normalizes typed data straight into that record
  * (emitting change notifications so watchers react). Mirrors apollo-kotlin's
  * `Fragment` / `Executable`.
  *
  * A `Fragment` carries the same three read/write-relevant members an
  * `Operation` does — no more — so it reuses the identical [[internal.Normalizer]]
  * (write) and [[internal.CacheBatchReader]] (read) machinery, only starting at
  * the caller's `CacheKey` instead of an operation root.
  *
  * @tparam D the fragment's typed `data` payload
  */
trait Fragment[D]:

    /** kyo-schema codec for this fragment's `data` payload of type `D` — the same
      * `Schema` contract an operation's [[kyo.apollo.api.Operation.dataSchema]]
      * satisfies, used to decode a denormalized record into `D` and to encode `D`
      * back into the response map the [[internal.Normalizer]] walks.
      */
    def dataSchema: Schema[D]

    /** The bidirectional codec cache reads/writes go through — the fragment analogue
      * of [[kyo.apollo.api.Operation.dataCodec]]. Defaults to wrapping [[dataSchema]].
      */
    def dataCodec: JsonCodec[D] = JsonCodec.fromSchema(using dataSchema)

    /** The root field describing the fragment's selection set.
      *
      * Its [[CompiledField.selections]] are the fragment's fields and its leaf type
      * names the type condition the fragment applies to (e.g. `User`). Unlike an
      * operation's root field this is not an operation-level `data` field but the
      * shape of the single object the fragment reads/writes.
      */
    def rootField: CompiledField

    /** This fragment's variables as a [[Json.JObj]] of `name -> value` pairs, for
      * argument-aware [[FieldKey]]s and redirect resolution — the fragment analogue
      * of [[kyo.apollo.api.Operation.variables]]. Defaults to an empty object, since
      * most fragments select only argument-free fields.
      */
    def variables: Json = Json.JObj(VectorMap.empty)

end Fragment

object Fragment:

    /** Build a [[Fragment]] from a chainable selection — the ergonomic entry point.
      *
      * `Fragment.of[Country]("Country")(_.code.name.capital)` derives the fragment's
      * codec and `rootField` from the generated selectors (the same machinery
      * `toQuery` uses), so no hand-written `CompiledField` / `Schema` is needed.
      * `typeName` is the GraphQL type the fragment applies to (the phantom `Origin`
      * doesn't carry it at runtime).
      *
      * The `Origin` type argument is supplied; the result type `D` is inferred from
      * the selection, so only `Origin` is named (`Fragment.of[Country]`). Returns a
      * [[Builder]] whose `apply` takes the selection and delegates to
      * [[kyo.apollo.api.toFragment]].
      *
      * @tparam Origin the phantom selection origin (e.g. `Country`)
      */
    def of[Origin](typeName: String): Builder[Origin] = new Builder[Origin](typeName)

    /** The partially-applied [[of]] — carries `Origin` + `typeName` so the selection
      * infers `D`. `Fragment.of[Country]("Country")(_.code.name)` is
      * `of[Country]("Country").apply(_.code.name)`.
      */
    final class Builder[Origin](typeName: String):
        def apply[D <: AnyNamedTuple](
            build: SelectionBuilder[Origin, Empty] => SelectionBuilder[Origin, D]
        ): Fragment[D] =
            build(SelectionBuilder.empty[Origin]).toFragment(typeName)
    end Builder
end Fragment
