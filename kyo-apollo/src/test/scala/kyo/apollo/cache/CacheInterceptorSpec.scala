package kyo.apollo.cache

import kyo.*
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.exception.CacheMissException
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloResponse
import scala.collection.immutable.VectorMap

/** End-to-end tests for the Phase 04 fetch-policy wiring: a real [[ApolloClient]]
  * with a [[MemoryCache]] installed via `normalizedCache`, driven through a fake
  * [[kyo.apollo.network.http.HttpEngine]] so each [[FetchPolicy]]'s emission sequence,
  * write-back behaviour, and `cacheInfo` stamping can be observed without a
  * network.
  */
class CacheInterceptorSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- fixtures: the CountriesQuery, keyed by `code` ------------------------

    // `__typename` field named verbatim to match the GraphQL wire form (kyo-schema
    // encodes field names as-is; the old ObjectAdapter mapped `typename` onto it).
    final case class Country(__typename: String, code: String, name: String) derives Schema
    final case class CountriesData(countries: List[Country]) derives Schema

    final case class CountriesQuery() extends Query[CountriesData]:
        def name                              = "Countries"
        def document                          = "query Countries { countries { __typename code name } }"
        def dataSchema: Schema[CountriesData] = summon[Schema[CountriesData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = List(
                    CompiledField(
                        "countries",
                        CompiledListType(CompiledNamedType("Country")),
                        selections = List(
                            CompiledField("__typename", CompiledNamedType("String")),
                            CompiledField("code", CompiledNamedType("String")),
                            CompiledField("name", CompiledNamedType("String"))
                        )
                    )
                )
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end CountriesQuery

    private val body =
        """{"data":{"countries":[""" +
            """{"__typename":"Country","code":"DE","name":"Germany"},""" +
            """{"__typename":"Country","code":"FR","name":"France"}]}}"""

    private val sampleData = CountriesData(
        List(Country("Country", "DE", "Germany"), Country("Country", "FR", "France"))
    )

    /** A fake engine that counts calls and returns a canned body/status; both are
      * mutable so a test can flip to an error mid-run.
      */
    final private class CountingEngine(var status: Int = 200)
        extends kyo.apollo.network.http.HttpEngine:
        var calls = 0
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            calls += 1
            kyo.apollo.network.http.HttpResponse(status, Nil, body)
        end execute
    end CountingEngine

    private def cachedClient(engine: CountingEngine): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl("https://example.com/graphql")
            .httpEngine(engine)
            .normalizedCache(MemoryCache(), IdCacheKeyGenerator(List("code")))
            .build()

    private def call(client: ApolloClient) = client.query(CountriesQuery())

    /** Collect every emission of a call into a list. */
    private def collectAll(
        c: kyo.apollo.runtime.ApolloCall[CountriesData]
    ): List[ApolloResponse[CountriesData]] < (Async & Scope) =
        StreamProbe.collect(c.stream)

    "fetch policies" - {

        // --- CacheFirst ---------------------------------------------------------

        "CacheFirst serves the network first, then the cache on the second call" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            for
                r1 <- call(client).fetchPolicy(FetchPolicy.CacheFirst).execute
                r2 <- call(client).fetchPolicy(FetchPolicy.CacheFirst).execute
            yield
                assert(r1.data == Present(sampleData))
                assert(r1.cacheInfo.map(_.fromCache) == Present(false))
                assert(r2.data == Present(sampleData))
                assert(r2.cacheInfo.map(_.isCacheHit) == Present(true))
                assert(engine.calls == 1) // second call served from cache, no new fetch
            end for
        }

        "the default policy (no .fetchPolicy) is CacheFirst" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            for
                _  <- call(client).execute
                r2 <- call(client).execute
            yield
                assert(engine.calls == 1)
                assert(r2.cacheInfo.exists(_.isCacheHit))
            end for
        }

        // --- NetworkOnly --------------------------------------------------------

        "NetworkOnly always hits the network and writes back to the cache" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            for
                r1 <- call(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                r2 <- call(client).fetchPolicy(FetchPolicy.CacheOnly).execute
            yield
                assert(r1.cacheInfo.map(_.fromCache) == Present(false))
                assert(engine.calls == 1)
                assert(r2.data == Present(sampleData)) // proves NetworkOnly wrote back
                assert(r2.cacheInfo.map(_.isCacheHit) == Present(true))
            end for
        }

        // --- CacheOnly ----------------------------------------------------------

        "CacheOnly on an empty store emits a CacheMissException value, no network" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            call(client).fetchPolicy(FetchPolicy.CacheOnly).execute.map { r =>
                assert(engine.calls == 0)
                assert(r.data == Absent)
                assert(r.exception.exists(_.isInstanceOf[CacheMissException]))
                assert(r.cacheInfo.exists(_.cacheMissException.isDefined))
            }
        }

        // --- CacheAndNetwork ----------------------------------------------------

        "CacheAndNetwork emits the cache response then the network response" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            for
                _         <- call(client).fetchPolicy(FetchPolicy.NetworkOnly).execute // populate
                emissions <- collectAll(call(client).fetchPolicy(FetchPolicy.CacheAndNetwork))
            yield
                assert(emissions.length == 2)
                assert(emissions(0).cacheInfo.map(_.fromCache) == Present(true))
                assert(emissions(0).cacheInfo.map(_.isCacheHit) == Present(true))
                assert(emissions(1).cacheInfo.map(_.fromCache) == Present(false))
                assert(emissions.map(_.data) == List(Present(sampleData), Present(sampleData)))
                assert(engine.calls == 2) // one populate + one network in cache-and-network
            end for
        }

        "CacheAndNetwork on an empty store emits the network response only" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            collectAll(call(client).fetchPolicy(FetchPolicy.CacheAndNetwork)).map { emissions =>
                assert(emissions.length == 1)
                assert(emissions.head.cacheInfo.map(_.fromCache) == Present(false))
                assert(engine.calls == 1)
            }
        }

        // --- NetworkFirst -------------------------------------------------------

        "NetworkFirst serves the network on success and writes back" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            for
                r1 <- call(client).fetchPolicy(FetchPolicy.NetworkFirst).execute
                r2 <- call(client).fetchPolicy(FetchPolicy.CacheOnly).execute
            yield
                assert(r1.cacheInfo.map(_.fromCache) == Present(false))
                assert(engine.calls == 1)
                assert(r2.data == Present(sampleData)) // written back
            end for
        }

        "NetworkFirst falls back to the cache when the network errors" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            for
                _ <- call(client).fetchPolicy(FetchPolicy.NetworkOnly).execute // populate at 200
                _ <- Sync.defer { engine.status = 500 }
                r <- call(client).fetchPolicy(FetchPolicy.NetworkFirst).execute
            yield
                assert(r.data == Present(sampleData)) // served from cache after network error
                assert(r.cacheInfo.exists(_.isCacheHit))
                assert(engine.calls == 2)
            end for
        }
    }
end CacheInterceptorSpec
