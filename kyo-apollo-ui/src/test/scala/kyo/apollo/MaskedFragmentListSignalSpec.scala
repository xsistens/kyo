package kyo.apollo

import CountryFixture.awaitSignal
import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.exception.CacheReadFailure
import kyo.apollo.network.http.HttpEngine

/** `Apollo.fragments` — the masked read for a LIST of refs, through one store
  * listener rather than one per row: it seeds every row from the cache, re-emits
  * when a targeted write touches any of them, follows the caller's list when it
  * grows (and watches what the new rows depend on), and carries elements that are
  * not refs at all through untouched.
  */
class MaskedFragmentListSignalSpec extends kyo.test.Test[Any]:

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

    private def countriesField[A](
        sel: SelectionBuilder.Bidirectional[CountryT, A]
    ): SelectionBuilder.Deferrable[RootQuery, (countries: Chunk[A])] =
        SelectionBuilder.obj(
            "countries",
            CompiledNamedType("Country").notNull.list.notNull,
            Chunk.empty,
            sel,
            SelectionBuilder.Nesting.Listed(SelectionBuilder.Nesting.Leaf)
        )

    private def countriesQuery =
        countriesField(GCountry.code ~ CountryCard.fields.spread).toQuery("Countries")

    final private class StaticEngine(body: String) extends HttpEngine:
        def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < Async =
            HttpEngine.response(HttpStatus.OK, body)

    private val serverUrl = "https://example.invalid/graphql"

    private def client(body: String): ApolloClient < (Sync & Scope) =
        ApolloClient.init(
            ApolloClient.Config(serverUrl)
                .httpEngine(StaticEngine(body))
                .normalizedCache(MemoryCache(), CacheIdentity.generator(summon[CacheIdentity[CountryT]]))
        )

    private val body =
        """{"data":{"countries":[
          |{"__typename":"Country","code":"DE","name":"Germany"},
          |{"__typename":"Country","code":"FR","name":"France"},
          |{"__typename":"Country","code":"IT","name":"Italy"}]}}""".stripMargin.replace("\n", "")

    /** The app-side row: a masked country, or a plain heading that carries no ref.
      * The mixed list is the point — a table of two kinds of row is what forced
      * this API to keep the caller's carrier instead of taking bare refs.
      */
    private enum Entry derives CanEqual:
        case Country(ref: CountryCard.fields.Ref)
        case Heading(text: String)

    private def refOf(e: Entry): Maybe[CountryCard.fields.Ref] =
        e match
            case Entry.Country(r) => Present(r)
            case Entry.Heading(_) => Absent

    private def label(e: Entry, d: (name: String)): String = d.name

    private def heading(e: Entry): String =
        e match
            case Entry.Heading(t) => s"— $t —"
            case Entry.Country(_) => "?"

    /** Rename the country behind `ref` with a targeted store write. */
    private def rename(c: ApolloClient, ref: CountryCard.fields.Ref, name: String)(using
        Frame
    ): Unit < (Sync & Abort[CacheReadFailure]) =
        c.apolloStore.keyOf(ref.typeName, ref.raw).map { key =>
            c.apolloStore.writeFragment(CountryCard.fields.cacheFragment, key, (name = name)).unit
        }

    "Apollo.fragments" - {

        "seeds every row from the records the response landed in, in list order" in {
            for
                c    <- client(body)
                data <- c.query(countriesQuery).data
                refs <- Signal.initRef(data.countries.map(e => Entry.Country(e.countryCard)): Seq[Entry])
                sig <-
                    given ApolloClient = c
                    Apollo.fragmentRows(refs)(refOf)(label, heading)
                now <- sig.current
            yield assert(now == Seq("Germany", "France", "Italy"))
        }

        "re-emits when a targeted write touches ONE of the rows" in {
            for
                c    <- client(body)
                data <- c.query(countriesQuery).data
                refs <- Signal.initRef(data.countries.map(e => Entry.Country(e.countryCard)): Seq[Entry])
                sig <-
                    given ApolloClient = c
                    Apollo.fragmentRows(refs)(refOf)(label, heading)
                _   <- rename(c, data.countries(1).countryCard, "Frankreich")
                now <- awaitSignal(sig)(_ == Seq("Germany", "Frankreich", "Italy"))
            yield assert(now == Seq("Germany", "Frankreich", "Italy"))
        }

        "follows the caller's list when it grows, and watches what the new row depends on" in {
            for
                c    <- client(body)
                data <- c.query(countriesQuery).data
                all = data.countries.map(e => Entry.Country(e.countryCard))
                // Start with a first page of one row, then append the rest.
                refs <- Signal.initRef(all.take(1): Seq[Entry])
                sig <-
                    given ApolloClient = c
                    Apollo.fragmentRows(refs)(refOf)(label, heading)
                first <- sig.current
                _     <- refs.set(all)
                grown <- awaitSignal(sig)(_ == Seq("Germany", "France", "Italy"))
                // A write to a row that only arrived with the second page must
                // still reach the signal: the key set is re-derived per emission.
                _     <- rename(c, data.countries(2).countryCard, "Italia")
                after <- awaitSignal(sig)(_ == Seq("Germany", "France", "Italia"))
            yield
                assert(first == Seq("Germany"))
                assert(grown == Seq("Germany", "France", "Italy"))
                assert(after == Seq("Germany", "France", "Italia"))
        }

        "carries elements that hold no ref through untouched" in {
            for
                c    <- client(body)
                data <- c.query(countriesQuery).data
                mixed = Entry.Heading("Europe") +: data.countries.map(e => Entry.Country(e.countryCard))
                refs <- Signal.initRef(mixed: Seq[Entry])
                sig <-
                    given ApolloClient = c
                    Apollo.fragmentRows(refs)(refOf)(label, heading)
                now <- sig.current
            yield assert(now == Seq("— Europe —", "Germany", "France", "Italy"))
        }

        "the bare-ref form needs no carrier at all" in {
            for
                c    <- client(body)
                data <- c.query(countriesQuery).data
                refs <- Signal.initRef(data.countries.map(_.countryCard): Seq[CountryCard.fields.Ref])
                sig <-
                    given ApolloClient = c
                    Apollo.fragments(CountryCard.fields)(refs)
                now   <- sig.current
                _     <- rename(c, data.countries.head.countryCard, "Deutschland")
                after <- awaitSignal(sig)(_.map(_.name) == Seq("Deutschland", "France", "Italy"))
            yield
                assert(now.map(_.name) == Seq("Germany", "France", "Italy"))
                assert(after.map(_.name) == Seq("Deutschland", "France", "Italy"))
        }

        "stays total on a cache-less client: each ref's own slice is its value" in {
            for
                c    <- ApolloClient.init(ApolloClient.Config(serverUrl).httpEngine(StaticEngine(body)))
                data <- c.query(countriesQuery).data
                refs <- Signal.initRef(data.countries.map(e => Entry.Country(e.countryCard)): Seq[Entry])
                sig <-
                    given ApolloClient = c
                    Apollo.fragmentRows(refs)(refOf)(label, heading)
                now <- sig.current
            yield assert(now == Seq("Germany", "France", "Italy"))
        }
    }
end MaskedFragmentListSignalSpec
