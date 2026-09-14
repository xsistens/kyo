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
      * `Fragment.of[Country](_.code.name.capital)` derives the fragment's codec and
      * `rootField` from the generated selectors (the same machinery `toQuery` uses),
      * so no hand-written `CompiledField` / `Schema` is needed. The GraphQL type the
      * fragment applies to comes from `given TypeName[Origin]`, which codegen emits
      * beside the phantom marker — naming it a second time as a string was redundant
      * and let a typo through to runtime.
      *
      * The `Origin` type argument is supplied; the result type `D` is inferred from
      * the selection, so only `Origin` is named. Returns a [[Builder]] whose `apply`
      * takes the selection and delegates to [[kyo.apollo.api.toFragment]].
      *
      * @tparam Origin the phantom selection origin (e.g. `Country`)
      */
    def of[Origin](using TypeName[Origin]): Builder[Origin] = new Builder[Origin]

    /** The partially-applied [[of]] — carries `Origin` so the selection infers `D`.
      * `Fragment.of[Country](_.code.name)` is `of[Country].apply(_.code.name)`.
      */
    final class Builder[Origin](using TypeName[Origin]):
        def apply[D <: AnyNamedTuple](
            build: SelectionBuilder.Fields[Origin, Empty] => SelectionBuilder.Fields[Origin, D]
        ): Fragment[D] =
            build(SelectionBuilder.empty[Origin]).toFragment
    end Builder

    /** Declare a colocated, masked fragment on an entity type — see
      * [[EntityFragment]] for the full contract. Requires the type's
      * [[CacheIdentity]] so a spread's ref always resolves to the entity record
      * (never a positional path key), and derives the fragment's name from the
      * declaration site via [[kyo.Frame]] — globally unique by construction,
      * with neither typos nor collisions expressible.
      *
      * {{{
      * object CountryCard:
      *     val fields = Fragment.entity[Country](_.name.capital)
      * }}}
      */
    def entity[Origin](using CacheIdentity[Origin], TypeName[Origin], kyo.Frame): EntityBuilder[Origin] =
        new EntityBuilder[Origin]

    final class EntityBuilder[Origin](using
        identity: CacheIdentity[Origin],
        origin: TypeName[Origin],
        frame: kyo.Frame
    ):
        def apply[D <: AnyNamedTuple](
            build: SelectionBuilder.Fields[Origin, Empty] => SelectionBuilder.Fields[Origin, D]
        ): EntityFragment[Origin, D] =
            val selection = build(SelectionBuilder.empty[Origin])
            EntityFragment(
                fragmentName = frame.className,
                typeName = origin.name,
                identity = identity,
                selection = selection,
                cacheFragment = selection.toFragment
            )
        end apply
    end EntityBuilder

    /** Declare a colocated, masked fragment on an object with no independent
      * identity (a `PageInfo`, a value object) — see [[EmbeddedFragment]]. No
      * [[CacheIdentity]] is required: the ref carries the value, and updates
      * flow through the parent's reactivity.
      */
    def embedded[Origin](using TypeName[Origin], kyo.Frame): EmbeddedBuilder[Origin] =
        new EmbeddedBuilder[Origin]

    final class EmbeddedBuilder[Origin](using origin: TypeName[Origin], frame: kyo.Frame):
        def apply[D <: AnyNamedTuple](
            build: SelectionBuilder.Fields[Origin, Empty] => SelectionBuilder.Fields[Origin, D]
        ): EmbeddedFragment[Origin, D] =
            EmbeddedFragment(
                fragmentName = frame.className,
                typeName = origin.name,
                selection = build(SelectionBuilder.empty[Origin])
            )
    end EmbeddedBuilder
end Fragment
