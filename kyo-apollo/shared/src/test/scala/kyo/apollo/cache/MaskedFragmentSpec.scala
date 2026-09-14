package kyo.apollo.cache

import kyo.Abort
import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import kyo.Result
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.exception.CacheReadFailure
import kyo.apollo.exception.NoCacheIdentityException
import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser
import scala.collection.immutable.VectorMap

/** Colocated masked fragments (Apollo Client 4 fragment colocation + data
  * masking): a spread contributes exactly one opaque ref element, the store keys
  * the ref to the record its own generator normalized, the captured slice keeps a cache
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
        def code: SelectionBuilder.Deferrable[CountryT, (code: String)] =
            SelectionBuilder.scalar("code", CompiledNamedType("ID").notNull, ScalarCodec.string)
        def name: SelectionBuilder.Deferrable[CountryT, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)
        def capital: SelectionBuilder.Deferrable[CountryT, (capital: Maybe[String])] =
            SelectionBuilder.scalar(
                "capital",
                CompiledNamedType("String"),
                ScalarCodec.maybe(ScalarCodec.string)
            )
        def emoji: SelectionBuilder.Deferrable[CountryT, (emoji: String)] =
            SelectionBuilder.scalar("emoji", CompiledNamedType("String").notNull, ScalarCodec.string)
    end GCountry

    object GPageInfo:
        def hasNextPage: SelectionBuilder.Deferrable[PageInfoT, (hasNextPage: Boolean)] =
            SelectionBuilder.scalar("hasNextPage", CompiledNamedType("Boolean").notNull, ScalarCodec.boolean)
        def endCursor: SelectionBuilder.Deferrable[PageInfoT, (endCursor: Maybe[String])] =
            SelectionBuilder.scalar(
                "endCursor",
                CompiledNamedType("String"),
                ScalarCodec.maybe(ScalarCodec.string)
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

    private def countryField[A](sel: SelectionBuilder[CountryT, A]): SelectionBuilder.Deferrable[RootQuery, (country: A)] =
        SelectionBuilder.obj(
            "country",
            CompiledNamedType("Country").notNull,
            Chunk.empty,
            sel,
            SelectionBuilder.Nesting.Leaf
        )

    private def pageInfoField[A](sel: SelectionBuilder[PageInfoT, A]): SelectionBuilder.Deferrable[RootQuery, (pageInfo: A)] =
        SelectionBuilder.obj(
            "pageInfo",
            CompiledNamedType("PageInfo").notNull,
            Chunk.empty,
            sel,
            SelectionBuilder.Nesting.Leaf
        )

    private def store(): ApolloStore =
        new ApolloStore(
            MemoryCache(),
            cacheKeyGenerator = CacheIdentity.generator(summon[CacheIdentity[CountryT]])
        )

    /** The key `s` gives an object — the outcome of `keyOf`, run. */
    private def keyed(s: ApolloStore, typeName: String, raw: Map[String, Json]): Result[CacheReadFailure, CacheKey] =
        Abort.run[CacheReadFailure](s.keyOf(typeName, raw)).eval

    private def parse(s: String): Json = JsonParser.parse(s).getOrThrow

    private val germanyBody =
        """{"country":{"__typename":"Country","code":"DE","name":"Germany","capital":"Berlin"}}"""

    "spread" - {

        "contributes one element whose label is derived from the declaration site" in {
            val q       = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")
            val decoded = q.dataCodec.decode(parse(germanyBody))
            // The macro derived `countryCard` from `object CountryCard` — no string named it.
            val ref: CountryCard.fields.Ref = decoded.country.countryCard
            assert(decoded.country.code == "DE")
            assert(keyed(store(), ref.typeName, ref.raw) == Result.succeed(CacheKey("Country", "DE")))
        }

        "forces __typename and the identity key fields into the document" in {
            val q = countryField(CountryCard.fields.spread).toQuery("Q")
            assert(q.document.contains("__typename"))
            assert(q.document.contains("code"))
            assert(q.document.contains("name"))
            assert(q.document.contains("capital"))
        }

        "a ref exposes no field values through Product or toString" in {
            val q   = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")
            val ref = q.dataCodec.decode(parse(germanyBody)).country.countryCard
            assert(!ref.toString.contains("Germany"))
            assert(!ref.toString.contains("Berlin"))
            assert(ref.toString == s"Ref(${CountryCard.fields.fragmentName})")
            // Not a case class: no Product view hands the masked slice to a generic printer.
            assert(!(ref: Any).isInstanceOf[Product])
            typeCheckFailure("ref.productIterator")("productIterator")
            // And no key of its own: the store computes it.
            typeCheckFailure("ref.key")("key")
        }

        "the ref carries the RESPONSE __typename, so an interface fragment keys the concrete type" in {
            val q = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")
            val body =
                """{"country":{"__typename":"SpecialCountry","code":"DE","name":"Germany","capital":null}}"""
            val ref = q.dataCodec.decode(parse(body)).country.countryCard
            assert(ref.typeName == "SpecialCountry")
            val special = new ApolloStore(
                MemoryCache(),
                cacheKeyGenerator = TypePolicyCacheKeyGenerator.of(TypePolicy("SpecialCountry", List("code")))
            )
            assert(keyed(special, ref.typeName, ref.raw) == Result.succeed(CacheKey("SpecialCountry", "DE")))
        }

        "two refs are equal exactly when they capture the same slice of the same fragment" in {
            val q       = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")
            val germany = q.dataCodec.decode(parse(germanyBody)).country.countryCard
            val again   = q.dataCodec.decode(parse(germanyBody)).country.countryCard
            val france = q.dataCodec.decode(
                parse("""{"country":{"__typename":"Country","code":"FR","name":"France","capital":"Paris"}}""")
            ).country.countryCard
            assert(germany == again)
            assert(germany.hashCode == again.hashCode)
            assert(germany != france)
        }

        "a missing key field names the field, not the row" in {
            val q         = countryField(CountryCard.fields.spread).toQuery("Q")
            val codeless  = """{"country":{"__typename":"Country","name":"Germany","capital":"Berlin"}}"""
            val failure   = Result.catching[ApolloParseException](q.dataCodec.decode(parse(codeless)))
            val refResult = CountryCard.fields.refFromRow(Map("__typename" -> Json.JStr("Country"), "capital" -> Json.JStr("Berlin")))
            failure match
                case Result.Failure(e) =>
                    assert(e.getMessage.contains("code"), e.getMessage)
                    assert(e.getMessage.contains("Country"), e.getMessage)
                    assert(e.getMessage.contains(CountryCard.fields.fragmentName), e.getMessage)
                    assert(!e.getMessage.contains("Berlin"), e.getMessage)
                    assert(!e.getMessage.contains("Germany"), e.getMessage)
                case other => fail(s"expected a parse failure naming the missing key field, got $other")
            end match
            assert(refResult.isFailure)
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
            for
                _ <- s.writeOperation(q, decoded)
                // The masked fields landed in the entity record even though the decoded
                // value carried them only inside the opaque ref.
                frag <- s.readFragment(CountryCard.fields.cacheFragment, CacheKey("Country", "DE"))
            yield
                assert(frag.name == "Germany")
                assert(frag.capital == Present("Berlin"))
            end for
        }

        "write → read round-trips the ref by value" in {
            val q       = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")
            val decoded = q.dataCodec.decode(parse(germanyBody))
            val s       = store()
            for
                _    <- s.writeOperation(q, decoded)
                back <- s.readOperation(q)
            yield assert(back == decoded)
            end for
        }

        "two fragments of the same type with overlapping fields merge into one record" in {
            val q = countryField(
                GCountry.code ~ CountryCard.fields.spread ~ CountryFlag.fields.spread
            ).toQuery("Q")
            val body =
                """{"country":{"__typename":"Country","code":"DE","name":"Germany","capital":"Berlin","emoji":"DE-FLAG"}}"""
            val decoded = q.dataCodec.decode(parse(body))
            val s       = store()
            val cardRef = decoded.country.countryCard
            val flagRef = decoded.country.countryFlag
            assert(keyed(s, cardRef.typeName, cardRef.raw) == keyed(s, flagRef.typeName, flagRef.raw))
            for
                _    <- s.writeOperation(q, decoded)
                card <- s.readFragment(CountryCard.fields.cacheFragment, CacheKey("Country", "DE"))
                flag <- s.readFragment(CountryFlag.fields.cacheFragment, CacheKey("Country", "DE"))
            yield
                assert(card.name == "Germany")
                assert(flag.emoji == "DE-FLAG")
            end for
        }

        "a parent selecting a field the fragment also selects stays a single stored field" in {
            val q       = countryField(GCountry.code ~ GCountry.name ~ CountryCard.fields.spread).toQuery("Q")
            val decoded = q.dataCodec.decode(parse(germanyBody))
            assert(decoded.country.name == "Germany") // the parent's own, explicit dependency
            val s = store()
            for
                _    <- s.writeOperation(q, decoded)
                back <- s.readOperation(q)
            yield assert(back == decoded)
            end for
        }
    }

    "cross-operation composition (§2.5)" - {

        "a fragment reads from a record accumulated by two separate operations" in {
            // Query A fetches {code,name}; query B fetches {code,capital}. Neither
            // alone satisfies CountryCard's {name,capital} — the accumulated record does.
            val qa = countryField(GCountry.code ~ GCountry.name).toQuery("A")
            val qb = countryField(GCountry.code ~ GCountry.capital).toQuery("B")
            val s  = store()
            for
                _ <- s.writeOperation(
                    qa,
                    qa.dataCodec.decode(parse("""{"country":{"__typename":"Country","code":"DE","name":"Germany"}}"""))
                )
                // A's fields alone must NOT satisfy the fragment yet.
                premature <- Abort.run[CacheReadFailure](
                    s.readFragment(CountryCard.fields.cacheFragment, CacheKey("Country", "DE"))
                )
                changedByB <- s.writeOperation(
                    qb,
                    qb.dataCodec.decode(parse("""{"country":{"__typename":"Country","code":"DE","capital":"Berlin"}}"""))
                )
                // …and the fragment now assembles across both operations' contributions.
                frag  <- s.readFragment(CountryCard.fields.cacheFragment, CacheKey("Country", "DE"))
                readA <- s.readOperation(qa)
            yield
                assert(premature.isFailure)
                // B extends the record rather than replacing it…
                assert(changedByB.contains(CacheKey("Country", "DE")))
                assert(frag.name == "Germany")
                assert(frag.capital == Present("Berlin"))
                // A's own read survives B's write: old fields were unioned, not clobbered.
                assert(readA.country.name == "Germany")
            end for
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
            assert(ref.value == (hasNextPage = true, endCursor = Present("c42")))
            assert(!ref.toString.contains("c42"))
            assert(!(ref: Any).isInstanceOf[Product])
            typeCheckFailure("ref.productIterator")("productIterator")
        }
    }

    "the store keys a ref (K-HIGH-52)" - {

        "a ref reads through the store's own key generator, even when the store keys the type differently" in {
            // The fragment's CacheIdentity keys Country by `code`; this store keys it by
            // `name`. The response is normalized into `Country:Germany`. A key the ref
            // computed from its own identity (`Country:DE`) names a record that was never
            // written — before K-HIGH-52 exactly that read was a CacheMissException.
            val s = new ApolloStore(
                MemoryCache(),
                cacheKeyGenerator = TypePolicyCacheKeyGenerator.of(TypePolicy("Country", List("name")))
            )
            val q       = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")
            val decoded = q.dataCodec.decode(parse(germanyBody))
            val ref     = decoded.country.countryCard
            assert(keyed(s, ref.typeName, ref.raw) == Result.succeed(CacheKey("Country", "Germany")))
            for
                _    <- s.writeOperation(q, decoded)
                frag <- s.keyOf(ref.typeName, ref.raw).map(s.readFragment(CountryCard.fields.cacheFragment, _))
            yield
                assert(frag.name == "Germany")
                assert(frag.capital == Present("Berlin"))
            end for
        }

        "a store with no identity for the type fails the ref's read with NoCacheIdentityException, never a positional key" in {
            // The default generator keys by `id`/`_id`; Country has neither. A ref has no
            // response path, so the positional fallback cannot apply: there is no key, and
            // the read of the ref fails on the same row as a miss.
            val s       = new ApolloStore(MemoryCache())
            val q       = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")
            val decoded = q.dataCodec.decode(parse(germanyBody))
            val ref     = decoded.country.countryCard
            for
                _ <- s.writeOperation(q, decoded)
                read <- Abort.run[CacheReadFailure](
                    s.keyOf(ref.typeName, ref.raw).map(s.readFragment(CountryCard.fields.cacheFragment, _))
                )
            yield read match
                case Result.Failure(e: NoCacheIdentityException) => assert(e.typeName == "Country")
                case other                                       => fail(s"expected NoCacheIdentityException, got $other")
            end for
        }
    }

    "CacheIdentity.generatorOf" - {

        "collects exactly the identities in scope — the SchemaIdentities expansion" in {
            // The shape codegen emits: enumerate ALL object types; only CountryT has a
            // given here, so PageInfoT contributes nothing and falls back to id keying.
            val gen = CacheIdentity.generatorOf[CountryT *: PageInfoT *: EmptyTuple]
            val key = gen.cacheKeyForObject(
                Map("__typename" -> Json.JStr("Country"), "code" -> Json.JStr("DE")),
                CacheKeyGeneratorContext(CompiledField("country", CompiledNamedType("Country")))
            )
            assert(key.contains(CacheKey("Country", "DE")))
        }
    }

    "spreadAs" - {

        "names the element explicitly when the derived label is not wanted" in {
            val q       = countryField(GCountry.code ~ CountryCard.fields.spreadAs["card"]).toQuery("Q")
            val decoded = q.dataCodec.decode(parse(germanyBody))
            val ref     = decoded.country.card
            assert(keyed(store(), ref.typeName, ref.raw) == Result.succeed(CacheKey("Country", "DE")))
        }
    }
end MaskedFragmentSpec
