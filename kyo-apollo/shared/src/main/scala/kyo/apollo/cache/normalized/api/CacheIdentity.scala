package kyo.apollo.cache.normalized.api

import kyo.Chunk
import kyo.apollo.api.CompiledField
import kyo.apollo.api.SelectionBuilder
import kyo.apollo.api.TypeName
import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.Empty
import scala.compiletime.erasedValue
import scala.compiletime.summonFrom

/** The cache identity of one GraphQL object type: which fields, together with
  * the type name, key its records.
  *
  * One per type — identity is a property of the type, not of any particular
  * selection — declared as a `given` so a duplicate declaration for the same
  * type is an ambiguous implicit the compiler rejects for free. Built without
  * strings: the type name comes from the codegen-emitted `given TypeName`, and
  * the key fields from the generated selectors, so a typo is a compile error
  * and the field provably exists on the type.
  *
  * {{{
  * object CacheIdentities:
  *     given CacheIdentity[Country]     = CacheIdentity.by(_.code)
  *     given CacheIdentity[CountryEdge] = CacheIdentity.by(_.cursor)
  * }}}
  *
  * Consumed in two roles, which is the point of declaring it once: the
  * client's [[CacheKeyGenerator]] ([[CacheIdentity.generator]]) keys normalized
  * records with it, and a [[Fragment.entity]] spread forces its key fields into
  * the document. The spread does NOT compute a key of its own: a ref is resolved
  * to its record by the store's generator (`ApolloStore.keyOf`), the same one
  * that normalized the response — so there is exactly one place a key is
  * computed, and a ref cannot point at a record the store never wrote.
  */
final class CacheIdentity[Origin] private (
    val typeName: String,
    private[apollo] val keyFields: Chunk[CompiledField]
):
    private[apollo] def keyFieldNames: Chunk[String] = keyFields.map(_.responseName)

    /** This identity as the [[TypePolicy]] the key generator consumes. */
    private[apollo] def policy: TypePolicy = TypePolicy(typeName, keyFieldNames.toList)

    override def toString: String = s"CacheIdentity($typeName, ${keyFieldNames.mkString("+")})"
end CacheIdentity

object CacheIdentity:

    /** Declare `Origin`'s identity by selecting its key field(s) with the
      * generated selectors — `CacheIdentity.by(_.code)`; a composite key chains
      * (`by(_.isbn.edition)`). The captured [[CompiledField]]s carry the schema
      * field type, so a spread can force the key fields into any document it is
      * spliced into without re-declaring them.
      */
    def by[Origin, K <: AnyNamedTuple](
        select: SelectionBuilder.Fields[Origin, Empty] => SelectionBuilder[Origin, K]
    )(using origin: TypeName[Origin]): CacheIdentity[Origin] =
        val fields = select(SelectionBuilder.empty[Origin]).selections.collect { case f: CompiledField => f }
        require(
            fields.nonEmpty,
            s"CacheIdentity.by[${origin.name}] must select at least one key field"
        )
        new CacheIdentity(origin.name, fields)
    end by

    /** Declare `Origin` as a '''singleton''' type: one record per app, keyed by
      * the bare typename (Apollo Client's `keyFields: []`). For one-per-viewer
      * values like a playback state — a snapshot query, subscription events, and
      * every mutation returning the type all normalize into the same record, so
      * one cache watcher observes them all, optimistic overlays included.
      */
    def singleton[Origin](using origin: TypeName[Origin]): CacheIdentity[Origin] =
        new CacheIdentity(origin.name, Chunk.empty)

    /** A [[CacheKeyGenerator]] over the given identities — the client-build
      * counterpart of the `given` declarations. Types without an identity fall
      * back to the default id-based keying.
      */
    def generator(identities: CacheIdentity[?]*): CacheKeyGenerator =
        TypePolicyCacheKeyGenerator.of(identities.map(_.policy)*)

    /** Collect the [[CacheIdentity]] givens in scope for the types in `T` — the
      * seam a codegen-emitted `SchemaIdentities.generator` expands through: codegen
      * enumerates every schema object type once, and this picks up exactly the
      * identities the application declared, at the call site where its givens are
      * visible. A type with no identity given simply contributes nothing.
      */
    inline def collectAll[T <: Tuple]: Chunk[CacheIdentity[?]] =
        inline erasedValue[T] match
            case _: EmptyTuple => Chunk.empty
            case _: (h *: t) =>
                summonFrom {
                    case ci: CacheIdentity[`h`] => ci +: collectAll[t]
                    case _                      => collectAll[t]
                }

    /** [[collectAll]] folded into a ready [[CacheKeyGenerator]]. */
    inline def generatorOf[T <: Tuple]: CacheKeyGenerator =
        generator(collectAll[T]*)
end CacheIdentity
