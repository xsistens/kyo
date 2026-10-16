package kyo.apollo

import kyo.*
import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.Names
import scala.compiletime.constValueTuple

/** The named-tuple `connections` combinator for [[PaginatedQuery]].
  *
  * Declares several connections in one call, keyed by name, and returns a *named
  * tuple* of their handles — so two `paged.connection(cursorOf, set)` lines with
  * duplicated `.copy` become one:
  *
  * {{{
  * // cursor state C is itself a named tuple; each name is a connection
  * paged <- usePaginatedQuery(initial = (users = Maybe.empty[String], posts = Maybe.empty[String]))(
  *            c => client.query(feeds(c.users, c.posts)))
  * nav = paged.connections((
  *   users = relayCursor(_.usersConnection.pageInfo),   // only cursorOf — `set` is derived
  *   posts = relayCursor(_.postsConnection.pageInfo)
  * ))
  * nav.users.fetchMore   // ConnectionHandle, dot-accessed by name
  * nav.posts.fetchMore
  * }}}
  *
  * The per-connection `set` disappears: because `C` is a named tuple whose field
  * names match the specs, "write the `users` slot" is derived from the name — the
  * combinator updates `C`'s same-named slot. `cursorOf` lambdas infer their `D`
  * because each spec value is typed `D => Maybe[String]`.
  */
extension [D, C <: AnyNamedTuple](paged: PaginatedQuery[D, C])
    inline def connections[Ns <: Tuple](
        specs: NamedTuple.NamedTuple[Ns, Tuple.Map[Ns, [_] =>> D => Maybe[String]]]
    )(using Frame): NamedTuple.NamedTuple[Ns, Tuple.Map[Ns, [_] =>> ConnectionHandle[D]]] =
        val cNames = constValueTuple[Names[C]].toArray.map(_.asInstanceOf[String])
        val sNames = constValueTuple[Ns].toArray
        val sVals  = specs.asInstanceOf[Tuple].toArray
        val out    = new Array[Object](sVals.length)
        var j      = 0
        while j < sVals.length do
            val name = sNames(j).asInstanceOf[String]
            val idx  = cNames.indexOf(name)
            require(idx >= 0, s"connections: `$name` is not a field of the cursor state")
            val cursorOf = sVals(j).asInstanceOf[D => Maybe[String]]
            out(j) = paged.connection(cursorOf, (c, cur) => ApolloConnections.updateSlot(c, idx, cur))
            j += 1
        end while
        Tuple
            .fromArray(out)
            .asInstanceOf[NamedTuple.NamedTuple[Ns, Tuple.Map[Ns, [_] =>> ConnectionHandle[D]]]]
end extension

private object ApolloConnections:
    /** Replace the `idx`-th slot of a named-tuple cursor state (erased to a Tuple at
      * runtime), preserving the others — the by-name `set` the combinator derives.
      */
    def updateSlot[C](c: C, idx: Int, value: Maybe[String]): C =
        val arr = c.asInstanceOf[Tuple].toArray
        arr(idx) = value.asInstanceOf[Object]
        Tuple.fromArray(arr).asInstanceOf[C]
    end updateSlot
end ApolloConnections
