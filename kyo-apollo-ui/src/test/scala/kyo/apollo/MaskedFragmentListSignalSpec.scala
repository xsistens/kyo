package kyo.apollo

import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

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
        def code: SelectionBuilder[CountryT, (code: String)] =
            SelectionBuilder.scalar("code", CompiledNamedType("ID").notNull, ScalarCodec.string)
        def name: SelectionBuilder[CountryT, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)
    end GCountry

    given CacheIdentity[CountryT] = CacheIdentity.by(e => e ~ GCountry.code)

    object CountryCard:
        val fields = Fragment.entity[CountryT](e => e ~ GCountry.name)

    private def countriesField[A](sel: SelectionBuilder[CountryT, A]): SelectionBuilder[RootQuery, (countries: List[A])] =
        SelectionBuilder.obj(
            "countries",
            CompiledNamedType("Country").notNull.list.notNull,
            Nil,
            sel,
            SelectionBuilder.Nesting.Listed(SelectionBuilder.Nesting.Leaf)
        )

    private def countriesQuery =
        countriesField(GCountry.code ~ CountryCard.fields.spread).toQuery("Countries")

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

    "Apollo.fragments" - {

        "seeds every row from the records the response landed in, in list order" in {
            given ApolloClient = client(body)
            Scope.run {
                for
                    data <- summon[ApolloClient].query(countriesQuery).data
                    refs <- Signal.initRef(data.countries.map(c => Entry.Country(c.countryCard)): Seq[Entry])
                    sig  <- Apollo.fragmentRows(refs)(refOf)(label, heading)
                    now  <- sig.current
                yield assert(now == Seq("Germany", "France", "Italy"))
            }
        }

        "re-emits when a targeted write touches ONE of the rows" in {
            given c: ApolloClient = client(body)
            Scope.run {
                for
                    data <- c.query(countriesQuery).data
                    refs <- Signal.initRef(data.countries.map(e => Entry.Country(e.countryCard)): Seq[Entry])
                    sig  <- Apollo.fragmentRows(refs)(refOf)(label, heading)
                    _ <- Sync.defer {
                        c.apolloStore.writeFragment(
                            CountryCard.fields.cacheFragment,
                            data.countries(1).countryCard.key,
                            (name = "Frankreich")
                        )
                    }
                    _   <- Async.sleep(50L.millis)
                    now <- sig.current
                yield assert(now == Seq("Germany", "Frankreich", "Italy"))
            }
        }

        "follows the caller's list when it grows, and watches what the new row depends on" in {
            given c: ApolloClient = client(body)
            Scope.run {
                for
                    data <- c.query(countriesQuery).data
                    all = data.countries.map(e => Entry.Country(e.countryCard))
                    // Start with a first page of one row, then append the rest.
                    refs  <- Signal.initRef(all.take(1): Seq[Entry])
                    sig   <- Apollo.fragmentRows(refs)(refOf)(label, heading)
                    first <- sig.current
                    _     <- refs.set(all)
                    _     <- Async.sleep(50L.millis)
                    grown <- sig.current
                    // A write to a row that only arrived with the second page must
                    // still reach the signal: the key set is re-derived per emission.
                    _ <- Sync.defer {
                        c.apolloStore.writeFragment(
                            CountryCard.fields.cacheFragment,
                            data.countries(2).countryCard.key,
                            (name = "Italia")
                        )
                    }
                    _     <- Async.sleep(50L.millis)
                    after <- sig.current
                yield
                    assert(first == Seq("Germany"))
                    assert(grown == Seq("Germany", "France", "Italy"))
                    assert(after == Seq("Germany", "France", "Italia"))
            }
        }

        "carries elements that hold no ref through untouched" in {
            given ApolloClient = client(body)
            Scope.run {
                for
                    data <- summon[ApolloClient].query(countriesQuery).data
                    mixed = Entry.Heading("Europe") +: data.countries.map(e => Entry.Country(e.countryCard))
                    refs <- Signal.initRef(mixed: Seq[Entry])
                    sig  <- Apollo.fragmentRows(refs)(refOf)(label, heading)
                    now  <- sig.current
                yield assert(now == Seq("— Europe —", "Germany", "France", "Italy"))
            }
        }

        "the bare-ref form needs no carrier at all" in {
            given c: ApolloClient = client(body)
            Scope.run {
                for
                    data <- c.query(countriesQuery).data
                    refs <- Signal.initRef(data.countries.map(_.countryCard): Seq[CountryCard.fields.Ref])
                    sig  <- Apollo.fragments(CountryCard.fields)(refs)
                    now  <- sig.current
                    _ <- Sync.defer {
                        c.apolloStore.writeFragment(
                            CountryCard.fields.cacheFragment,
                            data.countries.head.countryCard.key,
                            (name = "Deutschland")
                        )
                    }
                    _     <- Async.sleep(50L.millis)
                    after <- sig.current
                yield
                    assert(now.map(_.name) == Seq("Germany", "France", "Italy"))
                    assert(after.map(_.name) == Seq("Deutschland", "France", "Italy"))
            }
        }

        "stays total on a cache-less client: each ref's own slice is its value" in {
            given ApolloClient = ApolloClient
                .builder()
                .serverUrl("https://example.invalid/graphql")
                .httpEngine(StaticEngine(body))
                .build()
            Scope.run {
                for
                    data <- summon[ApolloClient].query(countriesQuery).data
                    refs <- Signal.initRef(data.countries.map(e => Entry.Country(e.countryCard)): Seq[Entry])
                    sig  <- Apollo.fragmentRows(refs)(refOf)(label, heading)
                    now  <- sig.current
                yield assert(now == Seq("Germany", "France", "Italy"))
            }
        }
    }
end MaskedFragmentListSignalSpec
