package kyo.apollo.cache

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.network.ApolloResponse

/** The civolution live-walk shape: a query watcher over an entity (`LobbyView`)
  * whose selection nests an entity LIST (`players`), plus a subscription whose
  * events normalize into the SAME entity — all three operations built the way
  * generated code builds them (`SelectionBuilder.obj`/`scalar` + `.mapInto`).
  *
  * Reference behavior (Apollo Client JS): a subscription result is written at
  * `ROOT_SUBSCRIPTION`, every entity inside normalizes into the SAME
  * `__typename:id` record a query uses, and `broadcastWatches()` re-emits every
  * watcher whose read depended on a touched record. In particular the nested
  * players merge into `LobbyPlayer:<id>` records — they are never re-keyed under
  * the writing operation's root path.
  */
class SubscriptionWatcherSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- generated-style schema selectors (mirrors apollo-codegen output) -------

    // Codegen puts the type-name given in the phantom's companion so it auto-summons;
    // these hand-written markers mirror that.
    sealed trait LobbyPlayerT
    object LobbyPlayerT:
        given TypeName[LobbyPlayerT] = TypeName("LobbyPlayer")

    sealed trait LobbyViewT
    object LobbyViewT:
        given TypeName[LobbyViewT] = TypeName("LobbyView")

    object GPlayer:
        def id: SelectionBuilder[LobbyPlayerT, (id: String)] =
            SelectionBuilder.scalar("id", CompiledNamedType("PlayerId").notNull, ScalarCodec.string)
        def color: SelectionBuilder[LobbyPlayerT, (color: String)] =
            SelectionBuilder.scalar("color", CompiledNamedType("String").notNull, ScalarCodec.string)
    end GPlayer

    object GLobby:
        def id: SelectionBuilder[LobbyViewT, (id: String)] =
            SelectionBuilder.scalar("id", CompiledNamedType("LobbyId").notNull, ScalarCodec.string)
        def name: SelectionBuilder[LobbyViewT, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)
        def players[A](
            sel: SelectionBuilder[LobbyPlayerT, A]
        ): SelectionBuilder[LobbyViewT, (players: List[A])] =
            SelectionBuilder.obj(
                "players",
                CompiledNamedType("LobbyPlayer").notNull.list.notNull,
                Nil,
                sel,
                SelectionBuilder.Nesting.Listed(SelectionBuilder.Nesting.Leaf)
            )
        def startedGameId: SelectionBuilder[LobbyViewT, (startedGameId: Option[String])] =
            SelectionBuilder.scalar(
                "startedGameId",
                CompiledNamedType("GameId"),
                ScalarCodec.option(ScalarCodec.string)
            )
    end GLobby

    // --- the page's view model (mirrors LobbyDetailPage.Model) ------------------

    final case class Player(id: String, color: String) derives Schema
    final case class Lobby(
        id: String,
        name: String,
        players: List[Player],
        startedGameId: Option[String]
    ) derives Schema

    /** The shared selection: query, mutation and subscription all use it, so every
      * reply/event must update the same records (civolution `Model.lobby`). The
      * nullable `startedGameId` guards the `Option = None` write → read round-trip
      * (kyo-schema encodes `None` as an ABSENT field; the cache must repair it to
      * an explicit `null` or every later read misses).
      */
    private def playerSel: SelectionBuilder[LobbyPlayerT, Player] =
        (GPlayer.id ~ GPlayer.color).mapInto[Player]

    private def lobbySel: SelectionBuilder[LobbyViewT, Lobby] =
        (GLobby.id ~ GLobby.name ~ GLobby.players(playerSel) ~ GLobby.startedGameId).mapInto[Lobby]

    private def lobbyArg(id: String): List[SelectionBuilder.Arg] =
        List(
            SelectionBuilder.Arg(
                "id",
                CompiledNamedType("LobbyId").notNull,
                ScalarCodec.string.encode(id)
            )
        )

    private def lobbyQuerySel(id: String): SelectionBuilder[RootQuery, (lobby: Lobby)] =
        SelectionBuilder.obj(
            "lobby",
            CompiledNamedType("LobbyView").notNull,
            lobbyArg(id),
            lobbySel,
            SelectionBuilder.Nesting.Leaf
        )

    private def lobbyQuery(id: String): Query[(lobby: Lobby)] =
        lobbyQuerySel(id).toQuery()

    private def lobbySubscription(id: String): Subscription[(lobbyUpdates: Option[Lobby])] =
        val sel: SelectionBuilder[RootSubscription, (lobbyUpdates: Option[Lobby])] =
            SelectionBuilder.obj(
                "lobbyUpdates",
                CompiledNamedType("LobbyView"),
                lobbyArg(id),
                lobbySel,
                SelectionBuilder.Nesting.Nullable(SelectionBuilder.Nesting.Leaf)
            )
        sel.toSubscription()
    end lobbySubscription

    private val p1                      = Player("p1", "Red")
    private val p2                      = Player("p2", "Blue")
    private def lobby(players: Player*) = Lobby("L1", "Alpha", players.toList, None)

    // --- fixture ----------------------------------------------------------------

    private val onePlayerBody =
        """{"data":{"lobby":{"__typename":"LobbyView","id":"L1","name":"Alpha",""" +
            """"players":[{"__typename":"LobbyPlayer","id":"p1","color":"Red"}],"startedGameId":null}}}"""

    final private class LobbyEngine extends kyo.apollo.network.http.HttpEngine:
        var calls = 0
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            calls += 1
            kyo.apollo.network.http.HttpResponse(200, Nil, onePlayerBody)
        end execute
    end LobbyEngine

    private def cachedClient(engine: LobbyEngine = LobbyEngine()): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl("https://example.com/graphql")
            .httpEngine(engine)
            .normalizedCache(MemoryCache(), IdCacheKeyGenerator(List("id")))
            .build()

    /** Fetch the query once over the (fake) network, then run `body` with the
      * client and a pull over a `CacheOnly` watch — the WatcherSpec harness, on the
      * civolution selection shape.
      */
    private def watching(
        body: (ApolloClient, StreamProbe.Pull[ApolloResponse[(lobby: Lobby)]]) => Unit <
            (Async & Scope)
    )(using Frame): Unit < (Async & Scope) =
        val client = cachedClient()
        for
            _ <- client.query(lobbyQuery("L1")).fetchPolicy(FetchPolicy.NetworkOnly).execute
            pull <- StreamProbe.Pull.open(
                client.query(lobbyQuery("L1")).fetchPolicy(FetchPolicy.CacheOnly).watch()
            )
            _ <- body(client, pull)
        yield ()
        end for
    end watching

    // --- tests ------------------------------------------------------------------

    "subscription write-back (store level)" - {

        "nested entities keep their entity keys — never re-keyed under the writer's root" in {
            val store = cachedClient().apolloStore
            val _     = store.writeOperation(lobbyQuery("L1"), (lobby = lobby(p1)))
            val changed =
                store.writeOperation(lobbySubscription("L1"), (lobbyUpdates = Some(lobby(p1, p2))))

            // Reference (Apollo JS): players normalize into `LobbyPlayer:<id>` on every
            // write path; the lobby's `players` field always references those records.
            val all = store.cache.allRecords()
            assert(all.contains("LobbyPlayer:p1"), s"expected LobbyPlayer:p1 in ${all.keySet}")
            assert(all.contains("LobbyPlayer:p2"), s"expected LobbyPlayer:p2 in ${all.keySet}")
            assert(changed.contains("LobbyView:L1"))
            succeed
        }

        "a query re-read after the subscription write sees the pushed players" in {
            val store     = cachedClient().apolloStore
            val _         = store.writeOperation(lobbyQuery("L1"), (lobby = lobby(p1)))
            val (_, deps) = store.readOperationWithKeys(lobbyQuery("L1"))
            val changed =
                store.writeOperation(lobbySubscription("L1"), (lobbyUpdates = Some(lobby(p1, p2))))

            // The watcher predicate: the subscription's changed keys must intersect the
            // query read's dependent keys, and the re-read must yield the new list.
            assert(changed.intersect(deps).nonEmpty, s"changed=$changed deps=$deps")
            val after = store.readOperation(lobbyQuery("L1"))
            assert(after.lobby == lobby(p1, p2))
            succeed
        }

        "a fragment write that CREATES an entity record still satisfies a later query read" in {
            // The nested-object stamp (Normalizer.compositeValue) does not cover a
            // fragment's ROOT object — writeFragment stamps it itself. Scenario: the
            // entity record exists ONLY from an imperative fragment write (cache
            // repair / client-authored entity), and a query read reaches it by
            // reference — its players node compiles the implicit `__typename`
            // selection, so the record must carry the field.
            val playerFragment: Fragment[(id: String, color: String)] =
                (GPlayer.id ~ GPlayer.color).toFragment

            val store = cachedClient().apolloStore
            val _     = store.writeOperation(lobbyQuery("L1"), (lobby = lobby(p1)))
            val _     = store.remove("LobbyPlayer:p1")
            val changed =
                store.writeFragment(playerFragment, CacheKey("LobbyPlayer:p1"), (id = "p1", color = "Red"))
            assert(changed.contains("LobbyPlayer:p1"))
            assert(store.cache.loadRecord("LobbyPlayer:p1").exists(_.get("__typename").isDefined))
            assert(store.readOperation(lobbyQuery("L1")).lobby == lobby(p1))
            succeed
        }

        "an Option = None field round-trips as an explicit null, and a push can flip it" in {
            val store = cachedClient().apolloStore
            // kyo-schema encodes `None` as an ABSENT field; without the mapInto
            // null-repair every later read of the record misses on `startedGameId`.
            val _ = store.writeOperation(lobbyQuery("L1"), (lobby = lobby(p1)))
            assert(store.readOperation(lobbyQuery("L1")).lobby.startedGameId == None)

            // The civolution start-game push: the subscription event carries the id;
            // a query re-read (the redirect observer's input) must see it.
            val started = lobby(p1).copy(startedGameId = Some("G9"))
            val _       = store.writeOperation(lobbySubscription("L1"), (lobbyUpdates = Some(started)))
            assert(store.readOperation(lobbyQuery("L1")).lobby.startedGameId == Some("G9"))
            succeed
        }
    }

    "subscription write-back (watcher level)" - {

        "a subscription event re-emits a query watcher on the same entity" in {
            watching { (client, pull) =>
                for
                    first <- pull.next
                    _ = assert(first.data.map(_.lobby) == Present(lobby(p1)))
                    // What CacheInterceptor.writeBack does for every streamed subscription
                    // event: normalize + merge + publish, on the subscription operation.
                    _ <- Sync.defer(
                        client.apolloStore
                            .writeOperation(lobbySubscription("L1"), (lobbyUpdates = Some(lobby(p1, p2))))
                    )
                    second <- pull.next
                yield assert(second.data.map(_.lobby) == Present(lobby(p1, p2)))
            }
        }

        "constructing a call effect fires no request — effects are inert until run" in {
            val engine = LobbyEngine()
            val client = cachedClient(engine)
            // A held effect (e.g. a handle's `refetch`) must not touch the network at
            // construction: the interceptor chain walks only on consumption.
            val _ = client.query(lobbyQuery("L1")).fetchPolicy(FetchPolicy.NetworkOnly).stream
            assert(engine.calls == 0, s"expected no network call at construction, got ${engine.calls}")
            succeed
        }

        "a CacheAndNetwork watch on an empty cache fetches exactly once (no refetch churn)" in {
            val engine = LobbyEngine()
            val client = cachedClient(engine)
            for
                pull <- StreamProbe.Pull.open(
                    client.query(lobbyQuery("L1")).fetchPolicy(FetchPolicy.CacheAndNetwork).watch()
                )
                first <- pull.next
                _ = assert(first.data.map(_.lobby) == Present(lobby(p1)))
                // The write-back publish must not re-trigger this watcher's own fetch,
                // and no second emission is pending.
                maybeMore <- pull.tryNext
            yield
                assert(maybeMore == Absent)
                assert(engine.calls == 1, s"expected one network call, got ${engine.calls}")
            end for
        }

        "a watcher survives a transient miss — a later write revives it (Apollo JS keeps watching)" in {
            watching { (client, pull) =>
                for
                    _ <- pull.next
                    // Evict the entity: the watcher re-emits a miss (already covered by
                    // WatcherSpec) …
                    _    <- Sync.defer(client.apolloStore.remove("LobbyView:L1"))
                    miss <- pull.next
                    _ = assert(miss.data.isEmpty)
                    // … and once data lands again, the watcher must come back — Apollo JS
                    // watchers stay registered across incomplete diffs.
                    _ <- Sync.defer(
                        client.apolloStore.writeOperation(lobbyQuery("L1"), (lobby = lobby(p1, p2)))
                    )
                    revived <- pull.next
                yield assert(revived.data.map(_.lobby) == Present(lobby(p1, p2)))
            }
        }
    }
end SubscriptionWatcherSpec
