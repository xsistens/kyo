package kyo.apollo

import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

/** `Apollo.fragment(ref)` — the masked `useFragment`: seeds from the cache the
  * network response was normalized into, re-emits when a targeted write touches
  * the entity, and stays total (the ref's own slice covers a cache-less read).
  */
class MaskedFragmentSignalSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- generated-style fixture -------------------------------------------------

    sealed trait CountryT
    object CountryT:
        given TypeName[CountryT] = TypeName("Country")

    object GCountry:
        def code: SelectionBuilder[CountryT, (code: String)] =
            SelectionBuilder.scalar("code", CompiledNamedType("ID").notNull, ScalarCodec.string)
        def name: SelectionBuilder[CountryT, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)
    end GCountry

    given CacheIdentity[CountryT] = CacheIdentity.by(e => e ~ GCountry.code)

    object CountryCard:
        val fields = Fragment.entity[CountryT](e => e ~ GCountry.name)

    private def countryField[A](sel: SelectionBuilder[CountryT, A]): SelectionBuilder[RootQuery, (country: A)] =
        SelectionBuilder.obj(
            "country",
            CompiledNamedType("Country").notNull,
            Nil,
            sel,
            SelectionBuilder.Nesting.Leaf
        )

    private def countryQuery =
        countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Country")

    final private class StaticEngine(body: String) extends HttpEngine:
        def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
            HttpResponse(200, Nil, body)

    private def client(body: String): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl("https://example.invalid/graphql")
            .httpEngine(StaticEngine(body))
            .normalizedCache(MemoryCache(), CacheIdentity.generator(summon[CacheIdentity[CountryT]]))
            .build()

    private val germanyBody =
        """{"data":{"country":{"__typename":"Country","code":"DE","name":"Germany"}}}"""

    "Apollo.fragment(ref)" - {

        "seeds from the normalized record the response landed in" in {
            given ApolloClient = client(germanyBody)
            Scope.run {
                for
                    data    <- summon[ApolloClient].query(countryQuery).data
                    sig     <- Apollo.fragment(data.country.countryCard)
                    current <- sig.current
                yield assert(current == (name = "Germany"))
            }
        }

        "re-emits when a targeted write touches the entity" in {
            given c: ApolloClient = client(germanyBody)
            Scope.run {
                for
                    data <- c.query(countryQuery).data
                    sig  <- Apollo.fragment(data.country.countryCard)
                    _ <- Sync.defer {
                        c.apolloStore.writeFragment(
                            CountryCard.fields.cacheFragment,
                            data.country.countryCard.key,
                            (name = "Deutschland")
                        )
                    }
                    _       <- Async.sleep(30L.millis)
                    current <- sig.current
                yield assert(current == (name = "Deutschland"))
            }
        }

        "stays total on a cache-less client: the ref's own slice seeds the signal" in {
            given ApolloClient = ApolloClient
                .builder()
                .serverUrl("https://example.invalid/graphql")
                .httpEngine(StaticEngine(germanyBody))
                .build()
            Scope.run {
                for
                    data    <- summon[ApolloClient].query(countryQuery).data
                    sig     <- Apollo.fragment(data.country.countryCard)
                    current <- sig.current
                yield assert(current == (name = "Germany"))
            }
        }
    }
end MaskedFragmentSignalSpec
