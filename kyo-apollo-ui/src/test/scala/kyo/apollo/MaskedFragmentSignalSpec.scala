package kyo.apollo

import CountryFixture.awaitSignal
import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.network.http.HttpEngine

/** `Apollo.fragment(ref)` — the masked `useFragment`: seeds from the cache the
  * network response was normalized into, re-emits when a targeted write touches
  * the entity, holds its record while it lives, and stays total (the ref's own
  * slice covers a cache-less read and a type the store gives no identity).
  */
class MaskedFragmentSignalSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- generated-style fixture -------------------------------------------------

    sealed trait CountryT
    object CountryT:
        given TypeName[CountryT] = TypeName("Country")

    object GCountry:
        def code: SelectionBuilder.Deferrable[CountryT, (code: String)] =
            SelectionBuilder.scalar("code", CompiledNamedType("ID").notNull, ScalarCodec.string)
        def name: SelectionBuilder.Deferrable[CountryT, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)
    end GCountry

    given CacheIdentity[CountryT] = CacheIdentity.by(e => e ~ GCountry.code)

    object CountryCard:
        val fields = Fragment.entity[CountryT](e => e ~ GCountry.name)

    private def countryField[A](
        sel: SelectionBuilder.Bidirectional[CountryT, A]
    ): SelectionBuilder.Deferrable[RootQuery, (country: A)] =
        SelectionBuilder.obj(
            "country",
            CompiledNamedType("Country").notNull,
            Chunk.empty,
            sel,
            SelectionBuilder.Nesting.Leaf
        )

    private def countryQuery =
        countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Country")

    final private class StaticEngine(body: String) extends HttpEngine:
        def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < Async =
            HttpEngine.response(HttpStatus.OK, body)

    private val serverUrl = "https://example.invalid/graphql"

    /** A config with a fresh store that keys a country by its `code`, the identity the
      * fragment declares. Every client created from one such config shares its store.
      */
    private def identified: ApolloClient.Config =
        ApolloClient.Config(serverUrl).normalizedCache(MemoryCache(), CacheIdentity.generator(summon[CacheIdentity[CountryT]]))

    private def client(body: String): ApolloClient < (Sync & Scope) =
        ApolloClient.init(identified.httpEngine(StaticEngine(body)))

    private val germanyBody =
        """{"data":{"country":{"__typename":"Country","code":"DE","name":"Germany"}}}"""

    private val franceBody =
        """{"data":{"country":{"__typename":"Country","code":"FR","name":"France"}}}"""

    "Apollo.fragment(ref)" - {

        "seeds from the normalized record the response landed in" in {
            for
                c    <- client(germanyBody)
                data <- c.query(countryQuery).data
                sig <-
                    given ApolloClient = c
                    Apollo.fragment(data.country.countryCard)
                current <- sig.current
            yield assert(current == (name = "Germany"))
        }

        "re-emits when a targeted write touches the entity" in {
            for
                c    <- client(germanyBody)
                data <- c.query(countryQuery).data
                ref = data.country.countryCard
                sig <-
                    given ApolloClient = c
                    Apollo.fragment(ref)
                key     <- c.apolloStore.keyOf(ref.typeName, ref.raw)
                _       <- c.apolloStore.writeFragment(CountryCard.fields.cacheFragment, key, (name = "Deutschland"))
                current <- awaitSignal(sig)(_ == (name = "Deutschland"))
            yield assert(current == (name = "Deutschland"))
        }

        "holds its record through garbage collection until its Scope closes" in {
            val shared = identified
            for
                germany <- ApolloClient.init(shared.httpEngine(StaticEngine(germanyBody)))
                // Created from the same config: the second client shares the store.
                france <- ApolloClient.init(shared.httpEngine(StaticEngine(franceBody)))
                data   <- germany.query(countryQuery).data
                ref = data.country.countryCard
                key <- germany.apolloStore.keyOf(ref.typeName, ref.raw)
                removedWhileOpen <- Scope.run {
                    given ApolloClient = germany
                    for
                        _ <- Apollo.fragment(ref)
                        // The query now points at France: no root reaches Germany's record.
                        _       <- france.query(countryQuery).fetchPolicy(FetchPolicy.NetworkOnly).data
                        removed <- germany.apolloStore.garbageCollect
                    yield removed
                    end for
                }
                removedAfterClose <- germany.apolloStore.garbageCollect
            yield
                assert(!removedWhileOpen.contains(key), "a live fragment signal lost its record to garbage collection")
                assert(removedAfterClose.contains(key), "the record stayed retained after the signal's Scope closed")
            end for
        }

        "stays total on a cache-less client: the ref's own slice seeds the signal" in {
            for
                c    <- ApolloClient.init(ApolloClient.Config(serverUrl).httpEngine(StaticEngine(germanyBody)))
                data <- c.query(countryQuery).data
                sig <-
                    given ApolloClient = c
                    Apollo.fragment(data.country.countryCard)
                current <- sig.current
            yield
                assert(c.normalizedStore.isEmpty)
                assert(current == (name = "Germany"))
        }

        "stays total for a type the store gives no identity: the ref's own slice seeds the signal" in {
            for
                // The default generator keys objects by `id`, which a country does not have.
                c <- ApolloClient.init(
                    ApolloClient.Config(serverUrl).httpEngine(StaticEngine(germanyBody)).normalizedCache(MemoryCache())
                )
                data <- c.query(countryQuery).data
                sig <-
                    given ApolloClient = c
                    Apollo.fragment(data.country.countryCard)
                current <- sig.current
            yield assert(current == (name = "Germany"))
        }
    }
end MaskedFragmentSignalSpec
