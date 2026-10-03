package kyo.apollo.cache.normalized.api

import kyo.apollo.api.*
import kyo.apollo.json.Json
import scala.NamedTuple.AnyNamedTuple
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

    /** The bidirectional codec cache reads/writes go through: it decodes a
      * denormalized record into `D` and encodes `D` back into the response map the
      * [[internal.Normalizer]] walks. A fragment over a `derives Schema` type supplies
      * [[kyo.apollo.api.JsonCodec.fromSchema]]; [[Fragment.of]] supplies the selection.
      */
    def dataCodec: JsonCodec[D]

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

    /** Build a [[Fragment]] from a list of field selections, the ergonomic entry point.
      *
      * `Fragment.of(Country)(_.code, _.name, _.capital)` derives the fragment's codec and `rootField` from the
      * generated selectors (the same machinery `toQuery` uses), so no hand-written `CompiledField` / `Schema` is
      * needed. The argument is the generated selector object of the type; each lambda receives it, and the
      * result type `D` is inferred from the selection. The GraphQL type the fragment applies to comes from
      * `given TypeName[Origin]`, which codegen emits beside the selector object.
      *
      * The selector object is passed as a value and every given the result needs is resolved where the
      * selection is applied (the result is a context function), so the entry point takes no `using` clause.
      * That is what keeps the lambdas typed in an IDE: a method with a `using` clause whose result is applied
      * directly to a lambda list leaves the lambdas untyped in the IntelliJ Scala plugin.
      *
      * @param selector the generated selector object of `Origin` (e.g. `Country`)
      */
    def of[Origin, S <: SelectionList[S, Origin, ?]](selector: S): Builder[Origin, S] =
        new Builder[Origin, S](selector)

    /** The partially-applied [[of]]: carries the selector object so the selection infers `D`. */
    final class Builder[Origin, S](protected val selector: S)
        extends SelectionList[S, Origin, [D <: AnyNamedTuple] =>> TypeName[Origin] ?=> Fragment[D]]:
        protected def fromFields[D <: AnyNamedTuple](sel: SelectionBuilder.Fields[Origin, D]): TypeName[Origin] ?=> Fragment[D] =
            sel.toFragment
    end Builder

    /** Declare a colocated, masked fragment on an entity type; see [[EntityFragment]] for the full contract.
      *
      * Requires the type's [[CacheIdentity]] so a spread's ref always resolves to the entity record (never a
      * positional path key), and derives the fragment's name from the declaration site via [[kyo.Frame]],
      * globally unique by construction, with neither typos nor collisions expressible. Both are resolved where
      * the selection is applied, as for [[of]].
      *
      * {{{
      * object CountryCard:
      *     val fields = Fragment.entity(Country)(
      *         _.name,
      *         _.capital,
      *     )
      * }}}
      */
    def entity[Origin, S <: SelectionList[S, Origin, ?]](selector: S): EntityBuilder[Origin, S] =
        new EntityBuilder[Origin, S](selector)

    final class EntityBuilder[Origin, S](protected val selector: S)
        extends SelectionList[
            S,
            Origin,
            [D <: AnyNamedTuple] =>> (CacheIdentity[Origin], TypeName[Origin], kyo.Frame) ?=> EntityFragment[Origin, D]
        ]:
        protected def fromFields[D <: AnyNamedTuple](
            selection: SelectionBuilder.Fields[Origin, D]
        ): (CacheIdentity[Origin], TypeName[Origin], kyo.Frame) ?=> EntityFragment[Origin, D] =
            EntityFragment(
                fragmentName = summon[kyo.Frame].className,
                typeName = summon[TypeName[Origin]].name,
                identity = summon[CacheIdentity[Origin]],
                selection = selection,
                cacheFragment = selection.toFragment
            )
    end EntityBuilder

    /** Declare a colocated, masked fragment on an object with no independent identity (a `PageInfo`, a value
      * object); see [[EmbeddedFragment]]. No [[CacheIdentity]] is required: the ref carries the value, and
      * updates flow through the parent's reactivity.
      *
      * {{{
      * object CoverImage:
      *     val fields = Fragment.embedded(Image)(_.url, _.vibrantColor)
      * }}}
      */
    def embedded[Origin, S <: SelectionList[S, Origin, ?]](selector: S): EmbeddedBuilder[Origin, S] =
        new EmbeddedBuilder[Origin, S](selector)

    final class EmbeddedBuilder[Origin, S](protected val selector: S)
        extends SelectionList[S, Origin, [D <: AnyNamedTuple] =>> (TypeName[Origin], kyo.Frame) ?=> EmbeddedFragment[Origin, D]]:
        protected def fromFields[D <: AnyNamedTuple](
            selection: SelectionBuilder.Fields[Origin, D]
        ): (TypeName[Origin], kyo.Frame) ?=> EmbeddedFragment[Origin, D] =
            EmbeddedFragment(
                fragmentName = summon[kyo.Frame].className,
                typeName = summon[TypeName[Origin]].name,
                selection = selection
            )
    end EmbeddedBuilder
end Fragment
