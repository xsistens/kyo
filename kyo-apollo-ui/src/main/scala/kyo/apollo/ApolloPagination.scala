package kyo.apollo

// getkyo.io library. This binding shares `package kyo.apollo` with `core` (the
// compat model — like `kyo-http` joining `kyo-core`'s `package kyo`), so
// `import kyo.*` resolves to the library root and pulls in Kyo's
// `<` / `Async` / `Scope` / `Signal` / `SignalRef` / `Frame` / `Tag` / `Emit` /
// `Chunk`.
import kyo.*
import kyo.apollo.ApolloCall
import kyo.apollo.ApolloClient
import kyo.apollo.api.* // SelectionBuilder + the `pagedBy` page-builder seam
import kyo.apollo.cache.normalized.FetchPolicy
import kyo.apollo.exception.ApolloException
import kyo.apollo.network.ApolloResponse
import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.Names
import scala.compiletime.constValueTuple

/** The **pagination** surface of the `kyo-ui` binding — the react-apollo
  * `fetchMore` shape (Step 6 of the react-parity build).
  *
  * React exposes pagination as `useQuery(...).fetchMore({ variables: { after } })`:
  * a live query whose result grows as you request further pages, each merged into
  * the existing list. kyo-apollo already does the *merge* declaratively — a
  * [[kyo.apollo.cache.normalized.api.ConnectionFieldPolicy]] installed on the
  * client unions successive pages' edges in the normalized cache under one
  * connection slot. This helper adds the *imperative* half: fetch the next page.
  *
  * ==One query, a cursor state, N connections==
  *
  * A query can hold several independently-paginated connections at once (two
  * feeds side by side). The general form models the query's whole cursor state as
  * a caller-chosen type `C`; the page *builder* `C => ApolloCall[D]` re-issues the
  * operation for a cursor state, and each connection advances exactly one slot in
  * `C` while preserving the others — so advancing one feed does not reset another.
  * Because the merge happens in the cache and the watcher re-reads it,
  * [[PaginatedQuery.state]] always reflects every page loaded so far.
  *
  * When `C` is a *named tuple* (one slot per connection), [[connections]] declares
  * them all in one call and returns a named tuple of handles — deriving each
  * connection's `set` from its field name, so no `.copy` is written.
  * [[relayCursor]] turns a `pageInfo` selection into the `cursorOf` (yielding the
  * `endCursor` until `hasNextPage` is false).
  *
  * {{{
  * import kyo.apollo.*
  * given ApolloClient = client
  *
  * Scope.run:
  *   for
  *     // cursor state = a named tuple, one Maybe[String] slot per connection
  *     paged <- usePaginatedQuery(initial = (users = Maybe.empty[String], posts = Maybe.empty[String]))(
  *                c => client.query(feeds(c.users, c.posts)))
  *     // one call → a named tuple of handles; `set` derived from each name
  *     nav = paged.connections((
  *       users = relayCursor(_.usersConnection.pageInfo),
  *       posts = relayCursor(_.postsConnection.pageInfo)))
  *     _   <- render(paged.state)
  *     _   <- button("More users").onClick(nav.users.fetchMore)
  *     _   <- button("More posts").onClick(nav.posts.fetchMore)
  *   yield ()
  * }}}
  *
  * The lower-level [[PaginatedQuery.connection]] `(cursorOf, set)` is still there
  * for a non-named-tuple `C`. The single-connection 90% case has a sugar overload
  * — `usePaginatedQuery(page)(cursorOf)` yielding a flat [[PaginatedQueryHandle]]
  * with a no-arg `fetchMore`.
  */

/** One connection within a [[PaginatedQuery]]: a no-arg `fetchMore` that advances
  * just this connection, plus a reactive `hasNext`.
  *
  * @param fetchMore fetch this connection's next page — read its end cursor from
  *                  the live data, write it into the shared cursor state (leaving
  *                  the other connections untouched), and re-issue the query
  *                  `NetworkOnly` (whose write-back merges the page and re-emits
  *                  [[PaginatedQuery.state]]). A no-op when the connection's
  *                  `cursorOf` yields `Absent` (nothing loaded, or the caller
  *                  encoded "no next page"). Raises an
  *                  [[kyo.apollo.exception.ApolloException]] on `Abort`.
  * @param hasNext  `true` while this connection's `cursorOf` yields a next cursor.
  *                 Encode `hasNextPage` into `cursorOf` (return `Absent` at the end)
  *                 for accurate button-gating.
  */
final case class ConnectionHandle[D](
    fetchMore: Unit < (Async & Abort[ApolloException]),
    hasNext: Signal[Boolean]
)

/** A live paginated query over a caller-chosen cursor state `C` (Step 6).
  *
  * Holds the accumulating [[state]] and mints one [[ConnectionHandle]] per
  * connection via [[connection]]. All sub-handles share one cursor-state cell, so
  * each `fetchMore` advances its own slot while keeping the others' last value —
  * re-fetching an unchanged connection is then an idempotent cache merge, never a
  * reset.
  */
final class PaginatedQuery[D, C] private[apollo] (
    val state: Signal[QueryState[D]],
    // Apply a reducer over (current cursors, latest data): `Present(next)` re-issues
    // the query for `next` and stores it; `Absent` is a no-op. Kept as a closure so
    // the shared `SignalRef[C]` stays private to `usePaginatedQuery`.
    private val advance: ((C, D) => Maybe[C]) => (Unit < (Async & Abort[ApolloException]))
):

    /** Declare a connection by its two lenses and get a sub-handle whose
      * `fetchMore` advances only this connection.
      *
      * @param cursorOf reads this connection's next-page cursor from the query data
      *                 (its `pageInfo.endCursor`); `Absent` parks `fetchMore`.
      * @param set      writes a new cursor into this connection's slot of the shared
      *                 cursor state, preserving the other slots (e.g.
      *                 `(c, cur) => c.copy(users = cur)`).
      */
    def connection(
        cursorOf: D => Maybe[String],
        set: (C, Maybe[String]) => C
    )(using Frame): ConnectionHandle[D] =
        ConnectionHandle(
            fetchMore = advance((c, d) => cursorOf(d).map(cur => set(c, Present(cur)))),
            hasNext = state.map(qs => PaginatedQuery.dataOf(qs).exists(d => cursorOf(d).isDefined))
        )
end PaginatedQuery

object PaginatedQuery:

    /** The data of a state that carries a renderable payload, else `Absent`. */
    private[apollo] def dataOf[D](qs: QueryState[D]): Maybe[D] = qs match
        case QueryState.Success(d, _, _)  => Present(d)
        case QueryState.PartialData(d, _) => Present(d)
        case _                            => Absent
end PaginatedQuery

/** Prepare a paginated query over a cursor state `C` — the general, N-connection
  * form (Step 6). The first page is `page(initial)`; its watcher seeds
  * [[PaginatedQuery.state]]. Each [[PaginatedQuery.connection]] `fetchMore` reads
  * the shared cursor state + the live data, advances its own slot, stores the new
  * state, and re-issues `page(next)` `NetworkOnly` — whose write-back unions the
  * page (per the connection's
  * [[kyo.apollo.cache.normalized.api.ConnectionFieldPolicy]]) and re-emits
  * `state`.
  *
  * The watcher `Cancelable` behind `state` is registered with the current
  * `Scope`, so binding the handle inside a `Scope.run { … }` tears the
  * subscription down when the block exits.
  *
  * @param initial the starting cursor state (typically all-`Absent` — first pages).
  * @param page    re-issues the operation for a cursor state; the same operation
  *                with only the cursor arguments varying, so pages share their
  *                connection slots in the cache. `C` must have a `CanEqual`.
  */
// `Apollo.paginatedQuery(initial)(page)` (the general cursor-state form) constructs
// a [[PaginatedQuery]]; see [[Apollo]].

/** The single-connection result — react-apollo's paginated `useQuery` shape: a
  * live [[state]] that accumulates every loaded page and a no-arg [[fetchMore]].
  *
  * @param state    the [[Signal]] of [[QueryState]] over the connection; reflects
  *                 all edges fetched so far and re-emits on each [[fetchMore]].
  * @param fetchMore fetch the next page (see [[ConnectionHandle.fetchMore]]).
  */
final case class PaginatedQueryHandle[D](
    state: Signal[QueryState[D]],
    fetchMore: Unit < (Async & Abort[ApolloException])
)

extension [D](paged: PaginatedQuery[D, Maybe[String]])
    /** Flatten a general [[PaginatedQuery]] whose cursor state IS the one connection's
      * cursor into the [[PaginatedQueryHandle]] the sugar returns.
      *
      * The sugar covers `Apollo.paginatedQuery(page)(cursorOf)` and its `skip`
      * overload, but the live-variables form
      * `Apollo.paginatedQuery(values)(initial)(page)` always yields the general
      * `PaginatedQuery` — a single-connection query that gains live variables would
      * otherwise have to re-assemble its own handle from the two lines below. It
      * cannot be a further `paginatedQuery` overload: that alternative's first
      * argument list would be `Signal[Maybe[V]]` too, and overload resolution has
      * nothing left to separate them by.
      */
    def singleConnection(cursorOf: D => Maybe[String])(using Frame): PaginatedQueryHandle[D] =
        PaginatedQueryHandle(paged.state, paged.connection(cursorOf, (_, cur) => cur).fetchMore)
end extension

/** Prepare a paginated query with a single connection — the sugar over the
  * general form. `page(Absent)` is the first page; `page(Present(cursor))` each next
  * one. Yields a flat [[PaginatedQueryHandle]] whose `fetchMore` advances that one
  * connection.
  *
  * {{{
  * usePaginatedQuery(after =>
  *   client.query(Queries.countriesConnection(first = Present(2), after = after)(
  *     _.edges(_.node(_.code.name).cursor).pageInfo(_.endCursor.hasNextPage)))
  * )(cursorOf = _.countriesConnection.pageInfo.endCursor)
  * }}}
  */
// `Apollo.paginatedQuery(page)(cursorOf)` (the single-connection sugar) constructs a
// flat [[PaginatedQueryHandle]]; see [[Apollo]].

extension [A <: AnyNamedTuple](sb: SelectionBuilder.Bidirectional[RootQuery, A])
    /** Paginate this single-connection query the react-apollo way, with no explicit
      * page plumbing — the ergonomic entry point to Step 6.
      *
      * `.paginated` reads the client from a `given ApolloClient` (like `.call`) and
      * derives the page builder from the selection itself (via
      * [[kyo.apollo.api.pagedBy]], which swaps only the `cursorArg` variable's
      * value), so the caller never threads an `after` handle. The selection must be
      * bidirectional: merging a page into the cached list writes it.
      *
      * {{{
      * given ApolloClient = client
      * Scope.run:
      *   Queries.countriesConnection(first = Present(2), after = Absent)(
      *     _.edges(_.node(_.code.name).cursor).pageInfo(_.endCursor.hasNextPage)
      *   ).paginated(_.countriesConnection.pageInfo.endCursor).map: paged =>
      *     render(paged.state)                     // Signal[QueryState] — all loaded edges
      *     button("More").onClick(paged.fetchMore) // next page → merge → re-emit
      * }}}
      *
      * For several connections in one query, use [[usePaginatedQuery]] with an
      * explicit page builder (each connection's cursor advances independently);
      * `.paginated` targets the one `cursorArg` and is the single-connection sugar.
      *
      * @param cursorOf  reads the next-page cursor from the data (the connection's
      *                  `pageInfo.endCursor`); return `Absent` to stop (encode
      *                  `hasNextPage`).
      * @param cursorArg the pagination cursor argument's name (Relay `after`).
      */
    def paginated(cursorOf: A => Maybe[String], cursorArg: String = "after")(using
        client: ApolloClient,
        frame: Frame,
        tag: Tag[Emit[Chunk[ApolloResponse[A]]]],
        canEqual: CanEqual[A, A]
    ): PaginatedQueryHandle[A] < (Async & Scope) =
        val page = sb.pagedBy(cursorArg)
        Apollo.paginatedQuery((after: Maybe[String]) => client.query(page(after)))(cursorOf)
end extension

/** Build a `cursorOf` from a Relay `pageInfo` — yield its `endCursor` while
  * `hasNextPage` is true, else `Absent`. So a connection's `fetchMore` becomes a
  * clean no-op at the last page instead of an idempotent re-fetch, and its
  * [[ConnectionHandle.hasNext]] flips to `false`.
  *
  * `f` projects the query data onto the connection's `pageInfo` selection, which
  * must include `endCursor: Maybe[String]` and `hasNextPage: Boolean`. The fields
  * are read by name, so any order and extra `pageInfo` fields are fine. Pair with
  * [[usePaginatedQuery]], [[PaginatedQuery.connection]], or [[connections]]:
  *
  * {{{
  * nav = paged.connections((
  *   users = relayCursor(_.usersConnection.pageInfo),
  *   posts = relayCursor(_.postsConnection.pageInfo)))
  * }}}
  */
inline def relayCursor[D, P <: AnyNamedTuple](f: D => P): D => Maybe[String] =
    val names = constValueTuple[Names[P]].toArray.map(_.asInstanceOf[String])
    val ecIdx = names.indexOf("endCursor")
    val hnIdx = names.indexOf("hasNextPage")
    require(
        ecIdx >= 0 && hnIdx >= 0,
        "relayCursor: the pageInfo selection needs `endCursor` and `hasNextPage` fields"
    )
    d =>
        val pi = f(d).asInstanceOf[Tuple].toArray
        if pi(hnIdx).asInstanceOf[Boolean] then pi(ecIdx).asInstanceOf[Maybe[String]]
        else Absent
end relayCursor
