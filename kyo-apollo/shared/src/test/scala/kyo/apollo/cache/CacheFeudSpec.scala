package kyo.apollo.cache

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.AllowUnsafe.embrace.danger
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloResponse
import scala.collection.immutable.VectorMap

/** Two `RefetchPolicy.CacheFirst` watchers whose writes make each other's cache read
  * incomplete — Apollo Client's "cache feud". One selects `viewer { id name }` (keyed
  * `User:1`), the other `viewer { email }` without the key field (keyed by path), so
  * every write re-points `QUERY_ROOT.viewer` and the other watcher's re-read misses.
  * Apollo Client 4.3 (#13406) stops refetching once a query sees the same incomplete
  * result again.
  *
  * No step waits on time. A watcher's answer to a miss is one of three: a refetch,
  * observed as its network emission; a stopped refetch, observed as the diagnostic,
  * which is reported synchronously inside the write that caused the miss; or a silent
  * drop, which the tests only assert where a wrong answer would be one of the other two.
  */
class CacheFeudSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class NamedViewer(__typename: String, id: String, name: String) derives Schema
    final case class NamedData(viewer: NamedViewer) derives Schema

    final case class MailViewer(__typename: String, email: String) derives Schema
    final case class MailData(viewer: MailViewer) derives Schema

    private def leaf(name: String) = CompiledField(name, CompiledNamedType("String"))

    final case class ViewerNameQuery() extends Query[NamedData]:
        def name                          = "ViewerName"
        def document                      = "query ViewerName { viewer { __typename id name } }"
        def dataSchema: Schema[NamedData] = summon[Schema[NamedData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = List(
                    CompiledField("viewer", CompiledNamedType("User"), selections = List(leaf("__typename"), leaf("id"), leaf("name")))
                )
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end ViewerNameQuery

    final case class ViewerMailQuery() extends Query[MailData]:
        def name                         = "ViewerMail"
        def document                     = "query ViewerMail { viewer { __typename email } }"
        def dataSchema: Schema[MailData] = summon[Schema[MailData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = List(
                    CompiledField("viewer", CompiledNamedType("User"), selections = List(leaf("__typename"), leaf("email")))
                )
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end ViewerMailQuery

    /** Answers `ViewerName` and `ViewerMail`, counting each. `failNextName` turns the next
      * `ViewerName` into an HTTP 500; `holdNextName` parks it until `release`, completing
      * `held` once it is parked. More than [[FeudEngine.Runaway]] requests report a
      * refetch loop through `runaway` instead of letting it spin.
      */
    final private class FeudEngine extends kyo.apollo.network.http.HttpEngine:
        val nameCalls                                = AtomicInt.Unsafe.init(0)
        val mailCalls                                = AtomicInt.Unsafe.init(0)
        @volatile var failNextName                   = false
        @volatile var holdNextName                   = false
        val held: Promise.Unsafe[Unit, Any]          = Promise.Unsafe.init()
        val release: Promise.Unsafe[Unit, Any]       = Promise.Unsafe.init()
        val runaway: Promise.Unsafe[String, Any]     = Promise.Unsafe.init()

        def calls: Int = nameCalls.get() + mailCalls.get()

        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            val response =
                if request.body.exists(_.contains("ViewerName")) then
                    discard(nameCalls.incrementAndGet())
                    if holdNextName then
                        holdNextName = false
                        held.completeDiscard(Result.succeed(()))
                        release.safe.get.andThen(nameResponse())
                    else nameResponse(): kyo.apollo.network.http.HttpResponse < Async
                else
                    discard(mailCalls.incrementAndGet())
                    ok("""{"data":{"viewer":{"__typename":"User","email":"alice@example.com"}}}"""): kyo.apollo.network.http.HttpResponse < Async
            if calls > FeudEngine.Runaway then
                runaway.completeDiscard(Result.succeed(s"refetch loop: $calls requests"))
            response
        end execute

        private def nameResponse(): kyo.apollo.network.http.HttpResponse =
            if failNextName then
                failNextName = false
                kyo.apollo.network.http.HttpResponse(500, Nil, "")
            else ok("""{"data":{"viewer":{"__typename":"User","id":"1","name":"Alice"}}}""")

        private def ok(body: String) = kyo.apollo.network.http.HttpResponse(200, Nil, body)
    end FeudEngine

    private object FeudEngine:
        val Runaway = 20

    /** A client over a [[FeudEngine]] whose cache diagnostics land in `warnings`; the
      * first one also completes `firstWarning`, as does a runaway engine.
      */
    final private class Feud:
        val engine   = FeudEngine()
        val warnings = scala.collection.mutable.ListBuffer.empty[String]
        val firstWarning: Promise.Unsafe[String, Any] = Promise.Unsafe.init()
        engine.runaway.onComplete(r => firstWarning.completeDiscard(r))
        val client = ApolloClient
            .builder()
            .serverUrl("https://example.com/graphql")
            .httpEngine(engine)
            .normalizedCache(
                MemoryCache(),
                IdCacheKeyGenerator(List("id")),
                diagnostics = CacheDiagnostics.to { msg =>
                    warnings.synchronized(warnings.append(msg))
                    firstWarning.completeDiscard(Result.succeed(msg))
                }
            )
            .build()
        val store = client.apolloStore

        def warned: List[String] = warnings.synchronized(warnings.toList)

        def watchName(using Frame) =
            StreamProbe.Pull.open(client.query(ViewerNameQuery()).refetchPolicy(RefetchPolicy.CacheFirst).watch())

        def watchMail(using Frame) =
            StreamProbe.Pull.open(client.query(ViewerMailQuery()).refetchPolicy(RefetchPolicy.CacheFirst).watch())

        def writeMail(using Frame): Unit < Sync = Sync.defer {
            discard(store.writeOperation(ViewerMailQuery(), MailData(MailViewer("User", "alice@example.com"))))
        }

        def writeName(name: String)(using Frame): Unit < Sync = Sync.defer {
            discard(store.writeOperation(ViewerNameQuery(), NamedData(NamedViewer("User", "1", name))))
        }

        /** Re-read every watcher of `QUERY_ROOT` without changing it. */
        def touchRoot(using Frame): Unit < Sync = Sync.defer(store.publish(Set(CacheKey.QueryRoot.key)))
    end Feud

    /** Skip the cache re-reads and return the next emission that came off the network. */
    private def nextFromNetwork[D](pull: StreamProbe.Pull[ApolloResponse[D]])(using Frame): ApolloResponse[D] < Async =
        pull.next.map { response =>
            if response.cacheInfo.exists(_.isCacheHit) then nextFromNetwork(pull) else response
        }

    "cache feud" - {

        "two CacheFirst watchers that invalidate each other stop refetching" in {
            val f = Feud()
            for
                named   <- f.watchName
                _       <- named.next
                mail    <- f.watchMail
                _       <- mail.next
                warning <- f.firstWarning.safe.get
            yield
                assert(!warning.startsWith("refetch loop"), warning)
                // Both initial fetches, then one refetch each before the repeat is dropped.
                assert(f.engine.calls == 4, s"${f.engine.calls} requests before the feud stopped")
            end for
        }

        "an imperative removal re-arms the refetch for the same miss" in {
            val f = Feud()
            for
                named <- f.watchName
                _     <- named.next
                _     <- f.writeMail
                _     <- nextFromNetwork(named)
                // `viewer` points at `User:1` again, so the path-keyed record the mail write
                // left is unreferenced: removing it notifies nobody, it only counts a removal.
                removed <- Sync.defer(f.store.remove("QUERY_ROOT.viewer"))
                _ = assert(removed, s"no path-keyed viewer in ${f.store.cache.allRecords().keySet}")
                // The very miss the refetch answered, which without the removal would be the
                // feud stop (see "the warning repeats only after the suppression was reset").
                _ <- f.writeMail
                _ = assert(f.warned.isEmpty, s"a miss after a removal is no feud: ${f.warned}")
                _ <- nextFromNetwork(named)
            yield assert(f.engine.nameCalls.get() == 3)
            end for
        }

        "a failed refetch does not suppress the next identical miss" in {
            val f = Feud()
            for
                named <- f.watchName
                _     <- named.next
                _     <- Sync.defer(f.engine.failNextName = true)
                _     <- f.writeMail
                failed <- nextFromNetwork(named)
                _ = assert(failed.hasTransportError, s"expected the HTTP 500: $failed")
                // Nothing completes the read in between: the re-read hits the same miss.
                _         <- f.touchRoot
                refetched <- nextFromNetwork(named)
            yield
                assert(refetched.data.isDefined, s"the retry answers: $refetched")
                assert(f.engine.nameCalls.get() == 3)
                assert(f.warned.isEmpty, s"a retry is no feud: ${f.warned}")
            end for
        }

        "a complete read caused by another write ends the suppression" in {
            val f = Feud()
            for
                named <- f.watchName
                _     <- named.next
                _     <- f.writeMail
                _     <- nextFromNetwork(named)
                _     <- f.writeName("Bob") // a mutation-like write that leaves the read complete
                _     <- f.writeMail
                _ = assert(f.warned.isEmpty, s"the same miss later is not a feud: ${f.warned}")
                _ <- nextFromNetwork(named)
            yield assert(f.engine.nameCalls.get() == 3)
            end for
        }
    }

    "feud diagnostic" - {

        "a stopped feud warns once, naming the operation and the missing field" in {
            val f = Feud()
            for
                named   <- f.watchName
                _       <- named.next
                mail    <- f.watchMail
                _       <- mail.next
                warning <- f.firstWarning.safe.get
            yield
                assert(f.warned == List(warning), s"expected one warning, got: ${f.warned}")
                assert(warning.contains("stopped refetching 'ViewerName'"), warning)
                assert(warning.contains("'id'"), warning)
            end for
        }

        "the warning repeats only after the suppression was reset" in {
            val f = Feud()
            for
                named <- f.watchName
                _     <- named.next
                _     <- f.writeMail
                _     <- nextFromNetwork(named)
                _     <- f.writeMail
                _     <- f.touchRoot
                _ = assert(f.warned.size == 1, s"two dropped misses, one warning: ${f.warned}")
                _ <- f.writeName("Bob") // a complete read from another write resets the suppression
                _ <- f.writeMail
                _ <- nextFromNetwork(named)
                _ <- f.writeMail
            yield assert(f.warned.size == 2, s"the next stopped feud warns again: ${f.warned}")
            end for
        }

        "a repeat of the miss while its refetch is still running is no feud" in {
            // Two writes in quick succession (a mutation, then a rollback) both break the
            // read before the refetch answers: the running refetch covers both, silently.
            val f = Feud()
            for
                named <- f.watchName
                _     <- named.next
                _     <- Sync.defer(f.engine.holdNextName = true)
                _     <- f.writeMail
                _     <- f.engine.held.safe.get
                _     <- f.touchRoot
                _ = assert(f.warned.isEmpty, s"expected silence while the refetch runs: ${f.warned}")
                _ <- Sync.defer(f.engine.release.completeDiscard(Result.succeed(())))
                _ <- nextFromNetwork(named)
            yield assert(f.engine.nameCalls.get() == 2, s"one refetch covers both misses: ${f.engine.nameCalls.get()}")
            end for
        }

        "no warning without a feud" in {
            val f = Feud()
            for
                named <- f.watchName
                _     <- named.next
                _     <- Sync.defer(discard(f.store.remove("User:1")))
                _     <- nextFromNetwork(named)
            yield assert(f.warned.isEmpty, s"expected silence, got: ${f.warned}")
            end for
        }
    }
end CacheFeudSpec
