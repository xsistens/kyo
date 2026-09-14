package kyo.apollo.cache

import kyo.*
import kyo.apollo.ApolloClient
import kyo.apollo.ClientField
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.TypePolicy
import kyo.apollo.cache.normalized.api.TypePolicyCacheKeyGenerator
import kyo.apollo.network.ApolloResponse
import scala.collection.mutable.ListBuffer

/** The runtime behaviour of a [[ClientField]] on a NON-ROOT origin — local state
  * that lives on an entity record rather than on `QUERY_ROOT`.
  *
  * `ClientFieldSpec` in the codegen module covers what is *emitted*; nothing here
  * covered what the emitted descriptor then *does*, which is the half every
  * consumer of local state actually stands on. Each case below pins one link of
  * that chain:
  *
  *   1. an unwritten field reads its default, and a write round-trips,
  *   2. the record it lands on is the very one the NORMALIZER built for the same
  *      entity — the only failure mode here is silent, so it is asserted on both
  *      sides rather than on the write alone,
  *   3. a watcher whose read visited that record re-emits after the write,
  *   4. a server write-back does not clobber the local value,
  *   5. the field is pruned from the printed document,
  *   6. writing the same value twice publishes nothing the second time, and the
  *      plural `writeAll` marks N entities in ONE broadcast where a loop makes N
  *      (asserted as the contrast, since the broadcast count IS the cost),
  *   7. a NETWORK emission carries the default, because the wire has no such
  *      field — pinned as KNOWN, not as correct (see the case's comment).
  *
  * The fixture is deliberately shaped like a Relay edge keyed by its cursor — a
  * non-`id` key field, which is what makes case 2 worth asserting.
  */
class ClientFieldSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- fixture: an `Edge` entity keyed by `cursor`, with one client field -----

    sealed trait Edge
    object Edge:
        given TypeName[Edge] = TypeName("Edge")

        def cursor: SelectionBuilder[Edge, (cursor: String)] =
            SelectionBuilder.scalar("cursor", CompiledNamedType("String").notNull, ScalarCodec.string)

        def label: SelectionBuilder[Edge, (label: String)] =
            SelectionBuilder.scalar("label", CompiledNamedType("String").notNull, ScalarCodec.string)
    end Edge

    private val selected = ClientField.create[Edge, Boolean]("selected", default = false)

    private type EdgeRow  = (cursor: String, label: String, selected: Boolean)
    private type EdgeData = (edge: EdgeRow)

    private def edgeSelection: SelectionBuilder[Edge, EdgeRow] =
        Edge.cursor ~ Edge.label ~ selected.select

    private def edgeQuery: Query[EdgeData] =
        SelectionBuilder
            .obj[RootQuery, EdgeData, EdgeRow](
                "edge",
                CompiledNamedType("Edge").notNull,
                Chunk.empty,
                edgeSelection,
                SelectionBuilder.Nesting.Leaf
            )
            .toQuery("EdgeQ")

    /** The server knows `cursor` and `label` and nothing about `selected`. */
    final private class Engine(labelOf: () => String) extends kyo.apollo.network.http.HttpEngine:
        val bodies = ListBuffer.empty[String]
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            bodies ++= request.body
            kyo.apollo.network.http.HttpResponse(
                200,
                Nil,
                s"""{"data":{"edge":{"__typename":"Edge","cursor":"c1","label":"${labelOf()}"}}}"""
            )
        end execute
    end Engine

    private def cachedClient(labelOf: () => String = () => "one")(using Frame): (ApolloClient, Engine) < (Sync & Scope) =
        val engine = Engine(labelOf)
        ApolloClient.init(
            ApolloClient.Config("https://example.com/graphql")
                .httpEngine(engine)
                .normalizedCache(
                    MemoryCache(),
                    TypePolicyCacheKeyGenerator.of(TypePolicy("Edge", List("cursor")))
                )
        ).map((_, engine))
    end cachedClient

    private def rowOf(r: ApolloResponse[EdgeData]): Maybe[EdgeRow] = r.data.map(_.edge)

    "a client field on an entity" - {

        // --- 1. default, then round-trip ---------------------------------------

        "reads its default before anything is written, and round-trips a write" in {
            for
                (client, _) <- cachedClient()
                first       <- client.query(edgeQuery).fetchPolicy(FetchPolicy.NetworkOnly).execute
                _ = assert(rowOf(first).map(_.selected) == Present(false), "unwritten field reads the default")
                // The imperative read agrees with the one the query decoded.
                before <- selected.read(client, "c1")
                _ = assert(!before)
                _     <- selected.write(client, "c1", true)
                after <- selected.read(client, "c1")
                _ = assert(after)
                reread <- client.query(edgeQuery).fetchPolicy(FetchPolicy.CacheOnly).execute
                _ = assert(rowOf(reread).map(_.selected) == Present(true))
                // The server's own fields are untouched by the local write.
                _ = assert(rowOf(reread).map(_.label) == Present("one"))
            yield assert(true)
            end for
        }

        "reads the default for an entity that was never in the cache at all" in {
            cachedClient().map { (client, _) =>
                selected.read(client, "never-loaded").map(v => assert(!v))
            }
        }

        // --- 2. the write lands on the normalizer's own record ------------------

        "writes into the record the normalizer built for the same entity" in {
            for
                (client, _) <- cachedClient()
                _           <- client.query(edgeQuery).fetchPolicy(FetchPolicy.NetworkOnly).execute
                // What the NORMALIZER keyed the edge as, read off the cache itself
                // rather than assumed: `TypePolicy("Edge", List("cursor"))` composes
                // `Edge:<cursor>`.
                normalized <- client.apolloStore.cache.allRecords.map(_.keySet.filter(_.render.startsWith("Edge")))
                _ = assert(normalized == Set(CacheKey("Edge", "c1")), s"normalizer keyed the edge as $normalized")
                // What the WRITE says it changed. These two agreeing is the whole
                // assertion: a mismatch would write to a record nothing reads, and
                // nothing anywhere would report it.
                changed <- selected.write(client, "c1", true)
                _ = assert(changed == Set("Edge:c1"), s"client-field write reported $changed")
            yield assert(true)
            end for
        }

        // --- 3. the write re-emits a watcher that read the record ---------------

        "re-emits a watcher whose read visited the record" in {
            for
                (client, _) <- cachedClient()
                _           <- client.query(edgeQuery).fetchPolicy(FetchPolicy.NetworkOnly).execute
                watch       <- ObservedWatch.open(client.query(edgeQuery).fetchPolicy(FetchPolicy.CacheOnly))
                pull = watch.pull
                first <- pull.next
                _     <- watch.awaitEstablished
                _ = assert(rowOf(first).map(_.selected) == Present(false))
                // The dependent keys are what makes the next step work; assert them
                // so a failure says WHICH half broke.
                _ = assert(first.cacheInfo.map(_.dependentKeys).exists(_.contains(CacheKey("Edge", "c1"))))
                _      <- Sync.defer(selected.write(client, "c1", true))
                second <- pull.next
                _ = assert(rowOf(second).map(_.selected) == Present(true))
                // A change to an unrelated key does not wake it: that publish and the next
                // write both react on this fiber, in order, so a wake-up would come first.
                _      <- Sync.defer(client.apolloStore.publish(Set(CacheKey("Edge", "c99"))))
                _      <- Sync.defer(selected.write(client, "c1", false))
                third  <- pull.next
                silent <- pull.tryNext
            yield
                assert(rowOf(third).map(_.selected) == Present(false), s"the unrelated publish woke the watcher: $third")
                assert(silent == Absent)
            end for
        }

        // --- 4. a server write-back leaves local state alone --------------------

        "survives a network write-back over the same record" in {
            var label = "one"
            for
                (client, _) <- cachedClient(() => label)
                _           <- client.query(edgeQuery).fetchPolicy(FetchPolicy.NetworkOnly).execute
                _           <- selected.write(client, "c1", true)
                // A refetch rewrites Edge:c1 from a response that has no `selected`.
                _ = label = "two"
                refetched <- client.query(edgeQuery).fetchPolicy(FetchPolicy.NetworkOnly).execute
                _ = assert(rowOf(refetched).map(_.label) == Present("two"), "server field did update")
                // The local flag is still in the record the refetch merged into.
                cached <- client.query(edgeQuery).fetchPolicy(FetchPolicy.CacheOnly).execute
                _ = assert(rowOf(cached).map(_.selected) == Present(true), "local state survived the write-back")
                _ = assert(rowOf(cached).map(_.label) == Present("two"))
            yield assert(true)
            end for
        }

        // --- 5. the server never sees it ----------------------------------------

        "is pruned from the printed document" in {
            cachedClient().map { (client, engine) =>
                client.query(edgeQuery).fetchPolicy(FetchPolicy.NetworkOnly).execute.map { _ =>
                    val sent = engine.bodies.mkString
                    assert(sent.contains("cursor"), "a server field is printed")
                    assert(!sent.contains("selected"), s"client field leaked onto the wire: $sent")
                }
            }
        }

        // --- 6. an identical write is not a change ------------------------------

        "publishes nothing when the value does not change" in {
            for
                (client, _) <- cachedClient()
                _           <- client.query(edgeQuery).fetchPolicy(FetchPolicy.NetworkOnly).execute
                first       <- selected.write(client, "c1", true)
                _ = assert(first == Set(CacheKey("Edge", "c1")))
                again <- selected.write(client, "c1", true)
                _ = assert(again.isEmpty, s"identical re-write reported $again")
            yield assert(true)
            end for
        }

        // --- 6b. the plural write is ONE broadcast ------------------------------

        "writeAll marks many entities in a single broadcast, where a loop makes N" in {
            // The whole point of the plural form, asserted as the difference it makes
            // rather than as its result: a broadcast wakes every watcher whose last
            // read touched one of these records, and each of those re-reads its WHOLE
            // operation. Counting the broadcasts is counting those re-reads.
            cachedClient().map { (client, _) =>
                val batches = ListBuffer.empty[Set[CacheKey]]
                for
                    _       <- client.apolloStore.addChangedKeysListener(ks => discard(batches += ks))
                    changed <- selected.writeAll(client, Seq("c1" -> true, "c2" -> true, "c3" -> true))
                    _ = assert(changed == Set(CacheKey("Edge", "c1"), CacheKey("Edge", "c2"), CacheKey("Edge", "c3")))
                    _ = assert(batches.size == 1, s"writeAll published ${batches.size} times")
                    _ = assert(batches.head == changed)
                    // The values really landed — a single broadcast of nothing would also
                    // satisfy the count above.
                    values <- Kyo.foreach(Seq("c1", "c2", "c3"))(selected.read(client, _))
                    _ = assert(values == Seq(true, true, true))
                    // The contrast, measured rather than asserted from the docs.
                    _ = batches.clear()
                    _ <- Kyo.foreachDiscard(Seq("c1", "c2", "c3"))(selected.write(client, _, false))
                    _ = assert(batches.size == 3, s"three separate writes published ${batches.size} times")
                yield assert(true)
                end for
            }
        }

        "writeAll of nothing writes nothing and publishes nothing" in {
            // A broadcast with no change behind it is exactly the cost this exists to
            // remove, so the empty batch must not make one.
            cachedClient().map { (client, _) =>
                val batches = ListBuffer.empty[Set[CacheKey]]
                for
                    _       <- client.apolloStore.addChangedKeysListener(ks => discard(batches += ks))
                    changed <- selected.writeAll(client, Seq.empty)
                    _ = assert(changed.isEmpty)
                    _ = assert(batches.isEmpty, s"empty writeAll published $batches")
                yield assert(true)
                end for
            }
        }

        "writeAll collapses a repeated id the way one response would, later wins" in {
            for
                (client, _) <- cachedClient()
                changed     <- selected.writeAll(client, Seq("c1" -> true, "c1" -> false))
                _ = assert(changed == Set(CacheKey("Edge", "c1")))
                value <- selected.read(client, "c1")
                _ = assert(!value, "the later entry won")
            yield assert(true)
            end for
        }

        // --- 7. the one hole, pinned as known -----------------------------------

        "a NETWORK emission carries the default, because the wire has no such field" in {
            // KNOWN, not correct. A network response is offered verbatim before the
            // cache write-back can correct it, and the wire never carries a client
            // field — so `NetworkOnly` (and any policy that emits the network frame:
            // CacheAndNetwork, NetworkFirst) reports the default for one frame even
            // when the store holds a written value. The default `CacheFirst` never
            // takes that path, which is why no caller has met it. This case exists so
            // that a change in the behaviour is a failing test rather than a surprise.
            for
                (client, _) <- cachedClient()
                _           <- client.query(edgeQuery).fetchPolicy(FetchPolicy.NetworkOnly).execute
                _           <- selected.write(client, "c1", true)
                fresh       <- client.query(edgeQuery).fetchPolicy(FetchPolicy.NetworkOnly).execute
                _ = assert(rowOf(fresh).map(_.selected) == Present(false), "network frame reports the default")
                // …and the store was right all along.
                stored <- selected.read(client, "c1")
                _ = assert(stored, "the written value is still in the store")
            yield assert(true)
            end for
        }
    }

    // --- a recursive value type: normalized one level, blob below ----------------

    private val tree = ClientField.create[RootQuery, CfTreeNode]("tree", default = CfTreeNode("", Nil))

    "a client field holding a recursive value" - {

        "round-trips a three-level tree through the normalized store" in {
            // The selection tree cuts where `CfTreeNode` re-enters itself, so only the
            // first level becomes a record; the tail is a blob under its `children`
            // leaf. The whole-value codec must still return the identical graph — the
            // cut only steers normalization depth, never what is stored.
            cachedClient().map { (client, _) =>
                val value = CfTreeNode(
                    "root",
                    List(CfTreeNode("child", List(CfTreeNode("grandchild", Nil))), CfTreeNode("sibling", Nil))
                )
                for
                    _    <- tree.writeRoot(client, value)
                    back <- tree.readRoot(client)
                    _ = assert(back == value, s"read back $back")
                    treeRecords <- client.apolloStore.cache.allRecords.map(_.keySet.filter(_.render.contains("tree")))
                    _ = assert(treeRecords.size == 1, s"exactly the first level is a record: $treeRecords")
                yield assert(true)
                end for
            }
        }
    }
end ClientFieldSpec

/** A self-referential value type for the recursive round-trip case (file-level so
  * the derived `Schema` is a stable given while kyo-schema ties the by-name knot).
  */
case class CfTreeNode(name: String, children: List[CfTreeNode]) derives CanEqual, Schema
