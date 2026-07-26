package kyo.apollo

import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.exception.CacheMissException
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.Concat
import scala.NamedTuple.NamedTuple

/** A local `@client` field — the single unit of client-side state management:
  * declared once, embedded into any selection on its `Origin`, and written/read
  * against the normalized store, which is where the value actually lives.
  *
  * The whole point is that local state is a first-class cache citizen: `write`
  * goes through [[kyo.apollo.cache.normalized.ApolloStore.writeFragment]], which
  * merges the value into the record and publishes the changed key, so every
  * watcher/`watchSignal` that read this field re-emits — reactivity for free, the
  * same pathway a network response takes. The field is pruned from the printed
  * document (the server never sees it) and yields [[default]] on a read miss.
  *
  * Full type parity with Apollo React's local state, via the single constructor
  * [[ClientField.create]]: it takes the value type `V` directly, derives its codec
  * from `Schema[V]`, and derives its cache SHAPE from `V`'s structure — a scalar/
  * enum is an inline blob, an object/list value is normalized into cache records
  * like a server object field (shared iff a `TypePolicy` matches — see `create`).
  *
  * {{{
  * val favorite = ClientField.create[Country, Boolean]("isFavorite", default = false)
  * Queries.country("DE")(_.code.name ~ favorite.select)   // ⇒ (code, name, isFavorite)
  * favorite.write(client, "DE", true)                     // re-emits every watcher of Country:DE
  *
  * val cartOpen = ClientField.create[RootQuery, Boolean]("cartOpen", default = false)
  * cartOpen.writeRoot(client, true)                       // global state on QUERY_ROOT
  * }}}
  *
  * Codegen can generate these descriptors and chainable `_.code.isFavorite`
  * accessors from a declarations object — see `ClientFieldDsl` in the codegen module.
  *
  * @tparam Origin the GraphQL object type the field hangs off (phantom marker)
  * @tparam R      the 1-slot named tuple `(label: V)` this contributes to a selection
  * @tparam V      the field's value type, in its Scala representation
  */
final class ClientField[Origin, R <: AnyNamedTuple, V] private[apollo] (
    parentType: String,
    default: V,
    slot: SelectionBuilder[Origin, R]
):

    /** Embed this field into a selection: `Query.select ~ field.select`, or the
      * chainable `_.name.clientField(field)`.
      */
    def select: SelectionBuilder[Origin, R] = slot

    // A one-field fragment rooted at the owning object, reading/writing just this
    // field. It reuses the slot's own codec (which keys the value under the field
    // label and applies the JNull→default decode), so write/read round-trip through
    // the unchanged ApolloStore.writeFragment/readFragment machinery.
    private val fragment: Fragment[R] = new Fragment[R]:
        def dataSchema: kyo.Schema[R] =
            throw new UnsupportedOperationException("client-field fragment has no Schema")
        override def dataCodec: JsonCodec[R] = new JsonCodec[R]:
            def decode(json: Json): R  = slot.decode(json)
            def encode(value: R): Json = slot.encode(value)
        def rootField: CompiledField =
            // The write fragment's field is deliberately NOT client-marked: `Normalizer`
            // skips client fields on write-back (so a network response can't clobber
            // local state), so this explicit write must present a plain node to actually
            // persist. Reads still go through the client-marked node in the query.
            val fields = slot.selections.map {
                case f: CompiledField => f.copy(client = false)
                case other            => other
            }
            CompiledField(parentType, CompiledNamedType(parentType), selections = fields)
        end rootField

    private def row(value: V): R = Tuple1(value).asInstanceOf[R]
    private def unrow(r: R): V   = r.asInstanceOf[Tuple].productElement(0).asInstanceOf[V]
    private def readAt(client: ApolloClient, key: CacheKey)(using Frame): V < Sync =
        Sync.defer {
            try unrow(client.apolloStore.readFragment(fragment, key))
            catch case _: CacheMissException => default
        }

    /** Write the field on entity `id` (record `TypeName:id`) and re-emit every
      * watcher whose read visited that record. Returns the changed record keys.
      */
    def write(client: ApolloClient, id: String, value: V)(using Frame): Set[String] < Sync =
        Sync.defer(client.apolloStore.writeFragment(fragment, CacheKey(parentType, id), row(value)))

    /** Write the field on the `QUERY_ROOT` record — the home for global, non-entity
      * client state (`ClientField.create[RootQuery, …](...)`).
      */
    def writeRoot(client: ApolloClient, value: V)(using Frame): Set[String] < Sync =
        Sync.defer(client.apolloStore.writeFragment(fragment, CacheKey.QueryRoot, row(value)))

    /** Read the field on entity `id`; [[default]] if never written (or the record
      * is absent).
      */
    def read(client: ApolloClient, id: String)(using Frame): V < Sync =
        readAt(client, CacheKey(parentType, id))

    /** Read the global (`QUERY_ROOT`) field; [[default]] if never written. */
    def readRoot(client: ApolloClient)(using Frame): V < Sync = readAt(client, CacheKey.QueryRoot)

end ClientField

object ClientField:

    /** Declare a local `@client` field of **any** `Schema`-derivable value type `V` —
      * scalar, enum, `Option`, `List`, a case class, or arbitrary nesting — the sole
      * constructor. The codec is derived from `V`'s `Schema`, and the cache SHAPE is
      * derived from its structure ([[kyo.apollo.api.ClientFieldStructure]]): a scalar/
      * enum is an inline blob, while an object/list value is **normalized** into cache
      * records exactly like a server object field.
      *
      * {{{
      * ClientField.create[Country, Boolean]("isFavorite", default = false)
      * ClientField.create[Country, Option[Book]]("favoriteBook", default = None)  // Book derives Schema
      * ClientField.create[Country, List[String]]("tags", default = Nil)
      * ClientField.create[RootQuery, Boolean]("cartOpen", default = false)
      * }}}
      *
      * Whether an object value is **shared/deduped** across queries follows the SAME
      * `TypePolicy` config as server entities — no separate API. Sharing happens iff
      * the value's injected `__typename` (its Scala class name) matches a registered
      * `TypePolicy` and `V` carries that policy's key field (then it writes into the
      * same `Type:id` record a server query reads); otherwise the object is stored as
      * a path-keyed record embedded under its parent — the local-state default. Note:
      * `Schema.rename`d fields and self-referential `V` are unsupported.
      */
    def create[Origin, V](using tn: TypeName[Origin], sch: kyo.Schema[V]): CreateApplied[Origin, V] =
        new CreateApplied[Origin, V](tn.name, sch)

    /** Curried builder so `Origin`/`V` are explicit while the label singleton infers. */
    final class CreateApplied[Origin, V](parentType: String, sch: kyo.Schema[V]):
        def apply[L <: String & Singleton](
            label: L,
            default: V
        ): ClientField[Origin, NamedTuple[L *: EmptyTuple, V *: EmptyTuple], V] =
            val structure = sch.structure
            val sels      = ClientFieldStructure.selections(structure)
            // A composite (object) value carries its structural selections and normalizes;
            // a scalar/enum has none and is a leaf blob under a placeholder leaf type.
            val leaf =
                CompiledNamedType(
                    if sels.isEmpty then "Client" else ClientFieldStructure.leafName(structure)
                )
            // Whole-value Schema codec (handles Option/List natively), with `__typename`
            // injected at each product level so the normalizer can key objects.
            val codec = ScalarCodec[V](
                json => SchemaJson.decode[V](ClientFieldStructure.toKyoWire(json, structure))(using sch),
                value =>
                    ClientFieldStructure.injectTypenames(SchemaJson.encode[V](value)(using sch), structure)
            )
            val slot =
                SelectionBuilder.clientField[Origin, NamedTuple[L *: EmptyTuple, V *: EmptyTuple], V](
                    label,
                    leaf,
                    sels,
                    codec,
                    default
                )
            new ClientField(parentType, default, slot)
        end apply
    end CreateApplied

end ClientField

/** Chainable sugar mirroring the generated selectors: `_.code.name.clientField(favorite)`. */
extension [Origin, Acc <: AnyNamedTuple](sb: SelectionBuilder[Origin, Acc])
    def clientField[R <: AnyNamedTuple, V](
        field: ClientField[Origin, R, V]
    ): SelectionBuilder[Origin, Concat[Acc, R]] =
        sb ~ field.select
end extension
