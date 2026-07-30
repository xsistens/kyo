package kyo.apollo.cache.normalized.api

import kyo.Maybe
import kyo.apollo.api.CompiledField
import kyo.apollo.api.FieldSelector
import kyo.apollo.api.TypeName
import kyo.apollo.json.Json
import kyo.discard
import scala.annotation.implicitNotFound

/** How two stored values of the *same* field key merge when a new write lands on
  * an existing record: given the value already stored (`existing`, `None` if the
  * field is new) and the incoming value, produce the value to keep. The default
  * everywhere is "incoming wins"; a [[FieldPolicy]] overrides that for one field
  * — e.g. a connection field unions paginated edges instead of replacing them.
  */
type FieldValueMerger = (existing: Option[RecordValue], incoming: RecordValue) => RecordValue

/** What a [[FieldPolicy]] read resolver is handed about the field being read. */
final case class FieldPolicyReadContext(
    field: CompiledField,
    variables: Map[String, Json]
)

/** A declarative policy for a single field of a type, mirroring apollo-kotlin's
  * `@fieldPolicy`. It can do three independent things, any subset of which may be
  * configured:
  *
  *   - **[[keyArgs]]** — restrict which of the field's arguments form its storage
  *     [[FieldKey]]. A connection field paginated by `first`/`after` sets
  *     `keyArgs = Some(Nil)` so every page collapses onto one logical slot;
  *     a field filtered by `category` sets `keyArgs = Some(List("category"))` so
  *     each category paginates independently. `None` keeps all arguments (the
  *     default field-key behaviour).
  *   - **[[read]]** — a cache redirect: resolve the field to another record at
  *     read time (the per-field analogue of a [[CacheKeyResolver]]).
  *   - **[[merge]]** — a [[FieldValueMerger]] overriding the default
  *     "incoming wins" union for this field.
  *
  * Policies are collected into [[FieldPolicies]] and consulted by the normalizer
  * (write-side field keys), the reader (read-side field keys and redirects), and
  * the record merger (merge).
  *
  * @param typeName  the parent type this field belongs to; together with
  *                  [[fieldName]] it forms the registry key, so the policy only
  *                  applies to this type's field, never to a same-named field on
  *                  another type
  * @param fieldName the field's schema name
  * @param keyArgs   the argument names that form the field key, or `None` for all
  * @param read      an optional read redirect
  * @param merge     an optional custom merge for this field's value
  */
final case class FieldPolicy(
    typeName: String,
    fieldName: String,
    keyArgs: Option[List[String]] = None,
    read: Option[FieldPolicyReadContext => Maybe[CacheKey]] = None,
    merge: Option[FieldValueMerger] = None
)

/** Builds the [[FieldPolicy]]s for a Relay-style paginated connection field so
  * successive pages accumulate into one logical list in the cache. Mirrors the
  * behaviour of apollo-kotlin's connection/pagination support.
  *
  * It emits two policies:
  *
  *   1. on the connection field itself — `keyArgs` drops the pagination
  *      arguments (or keeps the `filterArgs` you name) so every page writes to
  *      the same connection record; and
  *   2. on the connection's `edges` field — a [[FieldValueMerger]] that unions
  *      edge references across pages (de-duplicating by referenced record key,
  *      preserving order), so reading the connection yields the concatenation of
  *      every page fetched.
  *
  * `pageInfo` needs no special policy: it is its own record reached by a stable
  * reference, so the default field-wise merge already lets the latest page's
  * cursors win. For edges to survive pagination the edge (or node) type must have
  * a stable [[TypePolicy]] key (e.g. keyed by `cursor` or the node `id`), so
  * pages do not collide on a position-based key.
  */
object ConnectionFieldPolicy:
    /** The Relay pagination arguments dropped from a connection field's key. */
    val PaginationArgs: Set[String] = Set("first", "last", "before", "after")

    /** Policies for the connection field `fieldName` on `typeName`.
      *
      * The registry is keyed by `(typeName, fieldName)`, so the edges merge must
      * be declared under the CONNECTION type (the type the `edges` field lives
      * on), which is why `connectionTypeName` is required. It must match the
      * `__typename` the connection objects carry at runtime (for a plain object
      * connection type, that is the schema type name). Prefer [[of]], which
      * derives all three names from generated selectors.
      *
      * @param typeName           the type the connection field is declared on
      * @param fieldName          the connection field's name (e.g. `feed`)
      * @param connectionTypeName the connection's own type (e.g. `FeedConnection`),
      *                           the parent type of its `edges` field
      * @param filterArgs         argument names that DO partition the connection
      *                           (each value paginates on its own); pagination args
      *                           are always dropped. Default: none — all pages
      *                           share one slot.
      * @param edgesField         the connection's edge-list field name (default
      *                           `edges`)
      */
    def apply(
        typeName: String,
        fieldName: String,
        connectionTypeName: String,
        filterArgs: List[String] = Nil,
        edgesField: String = "edges"
    ): List[FieldPolicy] =
        List(
            FieldPolicy(typeName, fieldName, keyArgs = Some(filterArgs)),
            FieldPolicy(connectionTypeName, edgesField, merge = Some(unionByReference))
        )

    /** The typed form of [[apply]]: the connection field and its edges field are
      * named by generated selectors, so both provably exist and a schema drift is
      * a compile error instead of a silently inert policy (a misspelled string
      * never matches in the registry and pagination quietly degrades to replacing
      * pages). The connection's own type name (the registry key of the edges
      * merge) comes from the codegen-emitted `TypeName[Conn]` given.
      *
      * {{{
      * ConnectionFieldPolicy.of(Playlist.tracks(), PlaylistTrackConnection.edges)
      * }}}
      *
      * The `CacheIdentity[Edge]` context bound is a compile time witness for the
      * other half of the pagination contract: [[unionByReference]] de-duplicates
      * by referenced record key, so an edge type without its own identity is
      * keyed by response position, pages collide on those keys, and every page
      * replaces the previous one. Requiring the identity here turns that runtime
      * degradation into a compile error. The query must still SELECT the
      * identity's key fields (e.g. `cursor`) inside `edges` for the written
      * records to carry them; that part stays a runtime concern.
      *
      * @param connection the connection field's generated selector, e.g.
      *                   `Playlist.tracks()` (argument values are irrelevant,
      *                   only the field is read)
      * @param edges      the edge-list field's generated selector on the
      *                   connection type, e.g. `PlaylistTrackConnection.edges`
      * @param filterArgs argument names that DO partition the connection, as in
      *                   [[apply]]
      */
    def of[Origin, Conn, Edge](
        connection: FieldSelector[Origin, Conn],
        edges: FieldSelector[Conn, Edge],
        filterArgs: List[String] = Nil
    )(using
        origin: TypeName[Origin],
        conn: TypeName[Conn],
        @implicitNotFound(
            "Connection edges union by record reference, so the edge type needs its own cache identity: " +
                "without one, edges are keyed by response position, successive pages collide on those keys, " +
                "and every page replaces the previous one. Declare " +
                "`given CacheIdentity[${Edge}] = CacheIdentity.by(_.cursor)` (or key by the node id) " +
                "where your other identities live, and make sure the query selects the key field."
        ) edgeIdentity: CacheIdentity[Edge]
    ): List[FieldPolicy] =
        discard(edgeIdentity)
        apply(origin.name, connection.fieldName, conn.name, filterArgs, edges.fieldName)
    end of

    /** Union two edge lists, appending incoming references not already present and
      * de-duplicating by referenced record key; non-list values fall back to
      * "incoming wins".
      */
    val unionByReference: FieldValueMerger = (existing, incoming) =>
        (existing, incoming) match
            case (Some(RecordValue.RList(current)), RecordValue.RList(next)) =>
                RecordValue.RList(dedupeByReference(current ++ next))
            case _ => incoming

    /** Keep the first occurrence of each referenced record key; pass non-reference
      * items through unchanged (they cannot be de-duplicated by key).
      */
    private def dedupeByReference(
        items: kyo.Chunk[RecordValue]
    ): kyo.Chunk[RecordValue] =
        val seen = scala.collection.mutable.HashSet.empty[String]
        items.filter {
            case RecordValue.Reference(ref) => seen.add(ref.key)
            case _                          => true
        }
    end dedupeByReference
end ConnectionFieldPolicy
