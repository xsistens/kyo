package kyo.apollo

import CountryFixture.awaitSignal
import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.network.http.HttpEngine

/** `Apollo.fragment(ref)`, the masked `useFragment`: seeds from the cache the
  * network response was normalized into, re-emits when a targeted write touches
  * the entity, holds its record while it lives, and stays total (the ref's own
  * slice covers a cache-less read and a type the store gives no identity). Also
  * `Apollo.fragment(fragment, key)`, the unmasked read of one record.
  */
class MaskedFragmentSignalSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- generated-style fixture -------------------------------------------------

    sealed trait CountryT
    object CountryT:
        given TypeName[CountryT] = TypeName("Country")

    object GCountry
        extends SelectionList[GCountry.type, CountryT, [D <: scala.NamedTuple.AnyNamedTuple] =>> SelectionBuilder.Fields[CountryT, D]]:
        protected def selector: GCountry.type = this
        protected def fromFields[D <: scala.NamedTuple.AnyNamedTuple](sel: SelectionBuilder.Fields[CountryT, D])
            : SelectionBuilder.Fields[CountryT, D]                      = sel
        def code: SelectionBuilder.Deferrable[CountryT, (code: String)] =
            SelectionBuilder.scalar("code", CompiledNamedType("ID").notNull, ScalarCodec.string)
        def name: SelectionBuilder.Deferrable[CountryT, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)
    end GCountry

    given CacheIdentity[CountryT] = CacheIdentity.by(GCountry)(_.code)

    object CountryCard:
        val fields = Fragment.entity(GCountry)(_.name)

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

    /** A client over `cache` that keys a country by its `code`. */
    private def trapped(cache: NormalizedCache): ApolloClient < (Sync & Scope) =
        ApolloClient.init(
            ApolloClient.Config(serverUrl)
                .normalizedCache(cache, CacheIdentity.generator(summon[CacheIdentity[CountryT]]))
                .httpEngine(StaticEngine(germanyBody))
        )

    /** Rename the country under `key` with a targeted store write. */
    private def rename(c: ApolloClient, key: CacheKey, name: String)(using Frame): Unit < Sync =
        c.apolloStore.writeFragment(CountryCard.fields.cacheFragment, key, (name = name)).unit

    private val germanyBody =
        """{"data":{"country":{"__typename":"Country","code":"DE","name":"Germany"}}}"""

    private val franceBody =
        """{"data":{"country":{"__typename":"Country","code":"FR","name":"France"}}}"""

    "Apollo.fragment(ref)" - {

        "seeds from the normalized record the response landed in" in {
            for
                c    <- client(germanyBody)
                data <- CallData.of(c.query(countryQuery))
                sig  <-
                    given ApolloClient = c
                    Apollo.fragment(data.country.countryCard)
                current <- sig.current
            yield assert(current == (name = "Germany"))
        }

        "re-emits when a targeted write touches the entity" in {
            for
                c    <- client(germanyBody)
                data <- CallData.of(c.query(countryQuery))
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
                data   <- CallData.of(germany.query(countryQuery))
                ref = data.country.countryCard
                key              <- germany.apolloStore.keyOf(ref.typeName, ref.raw)
                removedWhileOpen <- Scope.run {
                    given ApolloClient = germany
                    for
                        _ <- Apollo.fragment(ref)
                        // The query now points at France: no root reaches Germany's record.
                        _       <- CallData.of(france.query(countryQuery).fetchPolicy(FetchPolicy.NetworkOnly))
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
                data <- CallData.of(c.query(countryQuery))
                sig  <-
                    given ApolloClient = c
                    Apollo.fragment(data.country.countryCard)
                current <- sig.current
            yield
                assert(c.normalizedStore.isEmpty)
                assert(current == (name = "Germany"))
        }

        "sees a write that lands between its first read and its subscription to the store" in {
            // The write runs on the reading fiber right after the first read loaded
            // `Country:DE`, before the signal exists. A signal that subscribes only after
            // that read would show Germany until some later write touched the record.
            val cache = TrapCache(MemoryCache())
            for
                c    <- trapped(cache)
                data <- CallData.of(c.query(countryQuery))
                ref = data.country.countryCard
                key <- c.apolloStore.keyOf(ref.typeName, ref.raw)
                _   <- cache.arm(key)(rename(c, key, "Deutschland"))
                sig <-
                    given ApolloClient = c
                    Apollo.fragment(ref)
                current <- sig.current
            yield assert(current == (name = "Deutschland"), s"the write during the first read was lost: $current")
            end for
        }

        "stays total for a type the store gives no identity: the ref's own slice seeds the signal" in {
            for
                // The default generator keys objects by `id`, which a country does not have.
                c <- ApolloClient.init(
                    ApolloClient.Config(serverUrl).httpEngine(StaticEngine(germanyBody)).normalizedCache(MemoryCache())
                )
                data <- CallData.of(c.query(countryQuery))
                sig  <-
                    given ApolloClient = c
                    Apollo.fragment(data.country.countryCard)
                current <- sig.current
            yield assert(current == (name = "Germany"))
        }
    }

    "Apollo.fragment(fragment, key)" - {

        "sees a write that lands between its first read and its subscription to the store" in {
            val cache = TrapCache(MemoryCache())
            for
                c    <- trapped(cache)
                data <- CallData.of(c.query(countryQuery))
                ref = data.country.countryCard
                key <- c.apolloStore.keyOf(ref.typeName, ref.raw)
                _   <- cache.arm(key)(rename(c, key, "Deutschland"))
                sig <-
                    given ApolloClient = c
                    Apollo.fragment(CountryCard.fields.cacheFragment, key)
                current <- sig.current
            yield assert(current == Present((name = "Deutschland")), s"the write during the first read was lost: $current")
            end for
        }

        "follows the record, and blanks while it is gone" in {
            for
                c    <- client(germanyBody)
                data <- CallData.of(c.query(countryQuery))
                ref = data.country.countryCard
                key <- c.apolloStore.keyOf(ref.typeName, ref.raw)
                sig <-
                    given ApolloClient = c
                    Apollo.fragment(CountryCard.fields.cacheFragment, key)
                first   <- sig.current
                _       <- rename(c, key, "Deutschland")
                renamed <- awaitSignal(sig)(_ == Present((name = "Deutschland")))
                _       <- c.apolloStore.evict(key)
                gone    <- awaitSignal(sig)(_ == Absent)
            yield
                assert(first == Present((name = "Germany")))
                assert(renamed == Present((name = "Deutschland")))
                assert(gone == Absent)
            end for
        }
    }
end MaskedFragmentSignalSpec
