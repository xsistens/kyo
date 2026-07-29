package kyo.apollo.cache

import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser
import scala.collection.immutable.VectorMap

/** Colocated masked fragments (Apollo Client 4 fragment colocation + data
  * masking): a spread contributes exactly one opaque ref element, the ref's key
  * matches the record the normalizer produces, the captured slice keeps a cache
  * write of decoded data lossless, and — the §2.5 composition — fragments read
  * from records accumulated across separate operations.
  */
class MaskedFragmentSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- generated-style schema fixture (mirrors apollo-codegen output) ---------

    sealed trait CountryT
    object CountryT:
        given TypeName[CountryT] = TypeName("Country")

    sealed trait PageInfoT
    object PageInfoT:
        given TypeName[PageInfoT] = TypeName("PageInfo")

    object GCountry:
        def code: SelectionBuilder[CountryT, (code: String)] =
            SelectionBuilder.scalar("code", CompiledNamedType("ID").notNull, ScalarCodec.string)
        def name: SelectionBuilder[CountryT, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)
        def capital: SelectionBuilder[CountryT, (capital: Option[String])] =
            SelectionBuilder.scalar(
                "capital",
                CompiledNamedType("String"),
                ScalarCodec.option(ScalarCodec.string)
            )
        def emoji: SelectionBuilder[CountryT, (emoji: String)] =
            SelectionBuilder.scalar("emoji", CompiledNamedType("String").notNull, ScalarCodec.string)
    end GCountry

    object GPageInfo:
        def hasNextPage: SelectionBuilder[PageInfoT, (hasNextPage: Boolean)] =
            SelectionBuilder.scalar("hasNextPage", CompiledNamedType("Boolean").notNull, ScalarCodec.boolean)
        def endCursor: SelectionBuilder[PageInfoT, (endCursor: Option[String])] =
            SelectionBuilder.scalar(
                "endCursor",
                CompiledNamedType("String"),
                ScalarCodec.option(ScalarCodec.string)
            )
    end GPageInfo

    given CacheIdentity[CountryT] = CacheIdentity.by(e => e ~ GCountry.code)

    // --- colocated fragments, declared the way components would -----------------

    object CountryCard:
        val fields = Fragment.entity[CountryT](e => e ~ GCountry.name ~ GCountry.capital)

    object CountryFlag:
        val fields = Fragment.entity[CountryT](e => e ~ GCountry.emoji)

    object PageBadge:
        val fields = Fragment.embedded[PageInfoT](e => e ~ GPageInfo.hasNextPage ~ GPageInfo.endCursor)

    // --- root query plumbing -----------------------------------------------------

    private def countryField[A](sel: SelectionBuilder[CountryT, A]): SelectionBuilder[RootQuery, (country: A)] =
        SelectionBuilder.obj(
            "country",
            CompiledNamedType("Country").notNull,
            Nil,
            sel,
            SelectionBuilder.Nesting.Leaf
        )

    private def pageInfoField[A](sel: SelectionBuilder[PageInfoT, A]): SelectionBuilder[RootQuery, (pageInfo: A)] =
        SelectionBuilder.obj(
            "pageInfo",
            CompiledNamedType("PageInfo").notNull,
            Nil,
            sel,
            SelectionBuilder.Nesting.Leaf
        )

    private def store(): ApolloStore =
        new ApolloStore(
            MemoryCache(),
            cacheKeyGenerator = CacheIdentity.generator(summon[CacheIdentity[CountryT]])
        )

    private def parse(s: String): Json = JsonParser.parse(s)

    private val germanyBody =
        """{"country":{"__typename":"Country","code":"DE","name":"Germany","capital":"Berlin"}}"""

    "spread" - {

        "contributes one element whose label is derived from the declaration site" in {
            val q       = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")
            val decoded = q.dataCodec.decode(parse(germanyBody))
            // The macro derived `countryCard` from `object CountryCard` — no string named it.
            val ref: CountryCard.fields.Ref = decoded.country.countryCard
            assert(decoded.country.code == "DE")
            assert(ref.key == CacheKey("Country", "DE"))
        }

        "forces __typename and the identity key fields into the document" in {
            val q = countryField(CountryCard.fields.spread).toQuery("Q")
            assert(q.document.contains("__typename"))
            assert(q.document.contains("code"))
            assert(q.document.contains("name"))
            assert(q.document.contains("capital"))
        }

        "the ref is masked: it renders identity, never field values" in {
            val q   = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")
            val ref = q.dataCodec.decode(parse(germanyBody)).country.countryCard
            assert(!ref.toString.contains("Germany"))
            assert(!ref.toString.contains("Berlin"))
            assert(ref.toString.contains("Country:DE"))
        }

        "the ref keys by the RESPONSE __typename, so an interface fragment keys the concrete type" in {
            val q = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")
            val body =
                """{"country":{"__typename":"SpecialCountry","code":"DE","name":"Germany","capital":null}}"""
            val ref = q.dataCodec.decode(parse(body)).country.countryCard
            assert(ref.key == CacheKey("SpecialCountry", "DE"))
        }

        "fragmentName is derived from the declaration site" in {
            assert(CountryCard.fields.fragmentName.nonEmpty)
        }
    }

    "cache round-trip" - {

        "a write of decoded data keeps the masked fields — the slice re-encodes them" in {
            val q       = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")
            val decoded = q.dataCodec.decode(parse(germanyBody))
            val s       = store()
            val _       = s.writeOperation(q, decoded)

            // The masked fields landed in the entity record even though the decoded
            // value carried them only inside the opaque ref.
            val frag = s.readFragment(CountryCard.fields.cacheFragment, CacheKey("Country", "DE"))
            assert(frag.name == "Germany")
            assert(frag.capital == Some("Berlin"))
        }

        "write → read round-trips the ref by value" in {
            val q       = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")
            val decoded = q.dataCodec.decode(parse(germanyBody))
            val s       = store()
            val _       = s.writeOperation(q, decoded)
            val back    = s.readOperation(q)
            assert(back == decoded)
        }

        "two fragments of the same type with overlapping fields merge into one record" in {
            val q = countryField(
                GCountry.code ~ CountryCard.fields.spread ~ CountryFlag.fields.spread
            ).toQuery("Q")
            val body =
                """{"country":{"__typename":"Country","code":"DE","name":"Germany","capital":"Berlin","emoji":"DE-FLAG"}}"""
            val decoded = q.dataCodec.decode(parse(body))
            val s       = store()
            val _       = s.writeOperation(q, decoded)

            assert(decoded.country.countryCard.key == decoded.country.countryFlag.key)
            val card = s.readFragment(CountryCard.fields.cacheFragment, CacheKey("Country", "DE"))
            val flag = s.readFragment(CountryFlag.fields.cacheFragment, CacheKey("Country", "DE"))
            assert(card.name == "Germany")
            assert(flag.emoji == "DE-FLAG")
        }

        "a parent selecting a field the fragment also selects stays a single stored field" in {
            val q       = countryField(GCountry.code ~ GCountry.name ~ CountryCard.fields.spread).toQuery("Q")
            val decoded = q.dataCodec.decode(parse(germanyBody))
            assert(decoded.country.name == "Germany") // the parent's own, explicit dependency
            val s = store()
            val _ = s.writeOperation(q, decoded)
            assert(s.readOperation(q) == decoded)
        }
    }

    "cross-operation composition (§2.5)" - {

        "a fragment reads from a record accumulated by two separate operations" in {
            // Query A fetches {code,name}; query B fetches {code,capital}. Neither
            // alone satisfies CountryCard's {name,capital} — the accumulated record does.
            val qa = countryField(GCountry.code ~ GCountry.name).toQuery("A")
            val qb = countryField(GCountry.code ~ GCountry.capital).toQuery("B")
            val s  = store()

            val _ = s.writeOperation(
                qa,
                qa.dataCodec.decode(parse("""{"country":{"__typename":"Country","code":"DE","name":"Germany"}}"""))
            )
            // A's fields alone must NOT satisfy the fragment yet.
            val premature =
                try
                    val _ = s.readFragment(CountryCard.fields.cacheFragment, CacheKey("Country", "DE"))
                    false
                catch case _: kyo.apollo.exception.CacheMissException => true
            assert(premature)

            val changedByB = s.writeOperation(
                qb,
                qb.dataCodec.decode(parse("""{"country":{"__typename":"Country","code":"DE","capital":"Berlin"}}"""))
            )
            // B extends the record rather than replacing it…
            assert(changedByB.contains("Country:DE"))
            // …and the fragment now assembles across both operations' contributions.
            val frag = s.readFragment(CountryCard.fields.cacheFragment, CacheKey("Country", "DE"))
            assert(frag.name == "Germany")
            assert(frag.capital == Some("Berlin"))
            // A's own read survives B's write: old fields were unioned, not clobbered.
            assert(s.readOperation(qa).country.name == "Germany")
        }
    }

    "embedded fragments" - {

        "spread decodes to a value-carrying ref with no cache key" in {
            val q = pageInfoField(GPageInfo.hasNextPage ~ PageBadge.fields.spread).toQuery("Q")
            val body =
                """{"pageInfo":{"__typename":"PageInfo","hasNextPage":true,"endCursor":"c42"}}"""
            val decoded                   = q.dataCodec.decode(parse(body))
            val ref: PageBadge.fields.Ref = decoded.pageInfo.pageBadge
            assert(decoded.pageInfo.hasNextPage == true)
            assert(ref.value == (hasNextPage = true, endCursor = Some("c42")))
            assert(!ref.toString.contains("c42"))
        }
    }

    "spreadAs" - {

        "names the element explicitly when the derived label is not wanted" in {
            val q       = countryField(GCountry.code ~ CountryCard.fields.spreadAs["card"]).toQuery("Q")
            val decoded = q.dataCodec.decode(parse(germanyBody))
            assert(decoded.country.card.key == CacheKey("Country", "DE"))
        }
    }
end MaskedFragmentSpec
