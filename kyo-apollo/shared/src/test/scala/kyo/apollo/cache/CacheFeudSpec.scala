package kyo.apollo.cache

import kyo.*
import kyo.apollo.ApolloClient
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Two `RefetchPolicy.CacheFirst` watches whose writes make each other's read
  * incomplete — Apollo Client's "cache feud". One selects `viewer { id name }` (keyed
  * `User:1`), the other `viewer { email }` without the key field (keyed by its path),
  * so every write re-points `QUERY_ROOT.viewer` and the other watch's re-read misses.
  * Apollo Client 4.3 (#13406) stops refetching once a query sees the same incomplete
  * result again, and so does a watch here.
  *
  * No step waits on time. A watch answers a miss with a refetch, observed as the end
  * of its flight ([[ObservedWatch.awaitRefetchEnded]]), or with the feud stop,
  * observed as the diagnostic, which is reported on the fiber whose write caused the
  * miss before that write returns.
  */
class CacheFeudSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class NamedViewer(__typename: String, id: String, name: String) derives Schema
    final case class NamedData(viewer: NamedViewer) derives Schema

    final case class MailViewer(__typename: String, email: String) derives Schema
    final case class MailData(viewer: MailViewer) derives Schema

    private def leaf(name: String) = CompiledField(name, CompiledNamedType("String"))

    private def viewerRoot(fields: String*) =
        CompiledField(
            "data",
            CompiledNamedType("Query"),
            selections = Chunk(CompiledField("viewer", CompiledNamedType("User"), selections = Chunk.from(fields.map(leaf))))
        )

    final case class ViewerNameQuery() extends Query.Normalizable[NamedData]:
        def name                            = "ViewerName"
        def document                        = "query ViewerName { viewer { __typename id name } }"
        val dataCodec: JsonCodec[NamedData] = JsonCodec.fromSchema[NamedData]
        def rootField: CompiledField        = viewerRoot("__typename", "id", "name")
        def variables: Json                 = Json.JObj(VectorMap.empty)
    end ViewerNameQuery

    final case class ViewerMailQuery() extends Query.Normalizable[MailData]:
        def name                           = "ViewerMail"
        def document                       = "query ViewerMail { viewer { __typename email } }"
        val dataCodec: JsonCodec[MailData] = JsonCodec.fromSchema[MailData]
        def rootField: CompiledField       = viewerRoot("__typename", "email")
        def variables: Json                = Json.JObj(VectorMap.empty)
    end ViewerMailQuery

    private val pathKeyedViewer = CacheKey.fromPath(List("QUERY_ROOT", "viewer"))

    /** Answers both queries and counts each. `holdNextName` parks the next `ViewerName`
      * on `release`, completing `held` once it is parked. More than
      * [[FeudEngine.Runaway]] requests complete `runaway`, so a refetch loop fails the
      * leaf instead of spinning.
      */
    final private class FeudEngine extends kyo.apollo.network.http.HttpEngine:
        private given AllowUnsafe = AllowUnsafe.embrace.danger
        val nameCalls             = AtomicInt.Unsafe.init(0)
        val mailCalls             = AtomicInt.Unsafe.init(0)
        val holdNextName          = AtomicBoolean.Unsafe.init(false)
        val held                  = Promise.Unsafe.init[Unit, Any]()
        val release               = Promise.Unsafe.init[Unit, Any]()
        val runaway               = Promise.Unsafe.init[String, Any]()

        def calls: Int = nameCalls.get() + mailCalls.get()

        def execute(
            request: kyo.apollo.network.http.HttpEngine.Request
        )(using Frame): kyo.apollo.network.http.HttpEngine.Response < Async =
            val isName = request.fields.body.text.exists(_.contains("ViewerName"))
            if isName then discard(nameCalls.incrementAndGet()) else discard(mailCalls.incrementAndGet())
            if calls > FeudEngine.Runaway then runaway.completeDiscard(Result.succeed(s"refetch loop: $calls requests"))
            if !isName then ok("""{"data":{"viewer":{"__typename":"User","email":"alice@example.com"}}}""")
            else if holdNextName.compareAndSet(true, false) then
                held.completeDiscard(Result.succeed(()))
                release.safe.get.andThen(ok(nameBody))
            else ok(nameBody)
            end if
        end execute

        private val nameBody = """{"data":{"viewer":{"__typename":"User","id":"1","name":"Alice"}}}"""

        private def ok(body: String): kyo.apollo.network.http.HttpEngine.Response < Async =
            kyo.apollo.network.http.HttpEngine.response(HttpStatus.OK, body)
    end FeudEngine

    private object FeudEngine:
        val Runaway = 20

    /** A client over a [[FeudEngine]] whose cache diagnostics land in `warnings`; the
      * first warning, or a runaway engine, completes `firstWarning`.
      */
    final private class Feud(
        val client: ApolloClient,
        val store: ApolloStore,
        val engine: FeudEngine,
        warnings: AtomicRef.Unsafe[Chunk[String]],
        val firstWarning: Promise.Unsafe[String, Any]
    ):
        def warned: Chunk[String] = warnings.get()(using AllowUnsafe.embrace.danger)

        def watchName(using Frame) =
            ObservedWatch.open(client.query(ViewerNameQuery()).refetchPolicy(RefetchPolicy.CacheFirst))

        def watchMail(using Frame) =
            ObservedWatch.open(client.query(ViewerMailQuery()).refetchPolicy(RefetchPolicy.CacheFirst))

        def writeMail(using Frame): Unit < Sync =
            store.writeOperation(ViewerMailQuery(), MailData(MailViewer("User", "alice@example.com"))).unit

        def writeName(name: String)(using Frame): Unit < Sync =
            store.writeOperation(ViewerNameQuery(), NamedData(NamedViewer("User", "1", name))).unit

        /** Re-read every watch of `QUERY_ROOT` without changing it. */
        def touchRoot(using Frame): Unit < Sync = store.publish(Set(CacheKey.QueryRoot))
    end Feud

    private def feud(using Frame): Feud < (Sync & Scope) = feud(MemoryCache())

    private def feud(cache: MemoryCache)(using Frame): Feud < (Sync & Scope) =
        val engine        = FeudEngine()
        given AllowUnsafe = AllowUnsafe.embrace.danger
        val warnings      = AtomicRef.Unsafe.init(Chunk.empty[String])
        val first         = Promise.Unsafe.init[String, Any]()
        engine.runaway.onComplete(r => first.completeDiscard(r))
        ApolloClient.init(
            ApolloClient.Config("https://example.com/graphql")
                .httpEngine(engine)
                .normalizedCache(
                    cache,
                    IdCacheKeyGenerator(List("id")),
                    diagnostics = CacheDiagnostics.to { msg =>
                        discard(warnings.updateAndGet(_ :+ msg))
                        first.completeDiscard(Result.succeed(msg))
                    }
                )
        ).map(client => Feud(client, client.apolloStore, engine, warnings, first))
    end feud

    "cache feud" - {

        "two CacheFirst watches that invalidate each other stop refetching" in {
            for
                f       <- feud
                named   <- f.watchName
                _       <- named.awaitEstablished
                mail    <- f.watchMail
                _       <- mail.awaitEstablished
                warning <- f.firstWarning.safe.get
            yield
                assert(!warning.startsWith("refetch loop"), warning)
                // Both initial fetches, then one refetch each before the repeat is dropped.
                assert(f.engine.calls == 4, s"${f.engine.calls} requests before the feud stopped")
            end for
        }

        "an imperative removal re-arms the refetch for the same miss" in {
            for
                f     <- feud
                named <- f.watchName
                _     <- named.awaitEstablished
                _     <- f.writeMail
                _     <- named.awaitRefetchEnded
                // `viewer` points at `User:1` again, so the path-keyed record the mail write
                // left is unreferenced: removing it notifies nobody, it only counts a removal.
                removed <- f.store.remove(pathKeyedViewer)
                _ = assert(removed, "the path-keyed viewer record was not there to remove")
                // The very miss the refetch answered, which without the removal would be the
                // feud stop (see "the warning repeats only after the suppression was reset").
                _ <- f.writeMail
                _ = assert(f.warned.isEmpty, s"a miss after a removal is no feud: ${f.warned}")
                _ <- named.awaitRefetchEnded
            yield assert(f.engine.nameCalls.get()(using AllowUnsafe.embrace.danger) == 3)
            end for
        }

        "a miss that recurs because the answer expired is refetched again" in {
            // Every field is stamped when written and lives `maxAge` on the cache's own clock. Expiry
            // empties `QUERY_ROOT`, the refetch answers that miss, and the answer expires in turn: the
            // same miss again, but caused by the cache dropping data, not by a feuding write.
            given AllowUnsafe = AllowUnsafe.embrace.danger
            val maxAge        = 1000L
            val now           = AtomicLong.Unsafe.init(0L)
            def expire        = Sync.defer(discard(now.addAndGet(maxAge + 1)))
            for
                f     <- feud(MemoryCache(maxAge = maxAge, nowMillis = () => now.get()))
                named <- f.watchName
                _     <- named.awaitEstablished
                _     <- expire
                _     <- f.touchRoot
                _     <- named.awaitRefetchEnded
                _     <- expire
                _     <- f.touchRoot
                _ = assert(f.warned.isEmpty, s"an expired answer is no feud: ${f.warned}")
                _ <- named.awaitRefetchEnded
            yield assert(f.engine.nameCalls.get() == 3, s"${f.engine.nameCalls.get()} ViewerName requests")
            end for
        }

        "a complete read caused by another write ends the suppression" in {
            for
                f     <- feud
                named <- f.watchName
                _     <- named.awaitEstablished
                _     <- f.writeMail
                _     <- named.awaitRefetchEnded
                _     <- f.writeName("Bob") // a mutation-like write that leaves the read complete
                _     <- f.writeMail
                _ = assert(f.warned.isEmpty, s"the same miss later is not a feud: ${f.warned}")
                _ <- named.awaitRefetchEnded
            yield assert(f.engine.nameCalls.get()(using AllowUnsafe.embrace.danger) == 3)
            end for
        }
    }

    "feud diagnostic" - {

        "a stopped feud warns once, naming the operation and the missing field" in {
            for
                f       <- feud
                named   <- f.watchName
                _       <- named.awaitEstablished
                mail    <- f.watchMail
                _       <- mail.awaitEstablished
                warning <- f.firstWarning.safe.get
            yield
                assert(f.warned == Chunk(warning), s"expected one warning, got: ${f.warned}")
                assert(warning.contains("stopped refetching 'ViewerName'"), warning)
                assert(warning.contains("id"), warning)
            end for
        }

        "the warning repeats only after the suppression was reset" in {
            for
                f     <- feud
                named <- f.watchName
                _     <- named.awaitEstablished
                _     <- f.writeMail
                _     <- named.awaitRefetchEnded
                _     <- f.writeMail
                _     <- f.touchRoot
                _ = assert(f.warned.size == 1, s"two dropped misses, one warning: ${f.warned}")
                _ <- f.writeName("Bob") // a complete read from another write resets the suppression
                _ <- f.writeMail
                _ <- named.awaitRefetchEnded
                _ <- f.writeMail
            yield assert(f.warned.size == 2, s"the next stopped feud warns again: ${f.warned}")
            end for
        }

        "a repeat of the miss while its refetch is still running is no feud" in {
            // Two writes in quick succession (a mutation, then a rollback) both break the
            // read before the refetch answers: the running refetch covers both, silently.
            given AllowUnsafe = AllowUnsafe.embrace.danger
            for
                f     <- feud
                named <- f.watchName
                _     <- named.awaitEstablished
                _ = f.engine.holdNextName.set(true)
                _ <- f.writeMail
                _ <- f.engine.held.safe.get
                _ <- f.touchRoot
                _ = assert(f.warned.isEmpty, s"expected silence while the refetch runs: ${f.warned}")
                _ = f.engine.release.completeDiscard(Result.succeed(()))
                _ <- named.awaitRefetchEnded
            yield assert(f.engine.nameCalls.get() == 2, s"one refetch covers both misses: ${f.engine.nameCalls.get()}")
            end for
        }

        "no warning without a feud" in {
            for
                f     <- feud
                named <- f.watchName
                _     <- named.awaitEstablished
                _     <- f.store.remove(CacheKey("User", "1"))
                _     <- named.awaitRefetchEnded
            yield assert(f.warned.isEmpty, s"expected silence, got: ${f.warned}")
            end for
        }
    }
end CacheFeudSpec
