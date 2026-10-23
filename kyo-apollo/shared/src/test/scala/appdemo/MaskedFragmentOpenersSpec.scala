package appdemo

import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.ApolloStore
import kyo.apollo.cache.normalized.MemoryCache
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.json.JsonParser

/** A masked ref as application code sees it. The suite lives OUTSIDE `kyo.apollo` on
  * purpose: from here the ref's fields are out of reach, and the store's openers
  * `readFragment(ref)` and `watchFragment(ref)` are the way to them.
  */
class MaskedFragmentOpenersSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

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

    private def countryField[A](sel: SelectionBuilder.Bidirectional[CountryT, A]): SelectionBuilder.Deferrable[RootQuery, (country: A)] =
        SelectionBuilder.obj("country", CompiledNamedType("Country").notNull, Chunk.empty, sel, SelectionBuilder.Nesting.Leaf)

    private val query = countryField(GCountry.code ~ CountryCard.fields.spread).toQuery("Q")

    private val germany =
        query.dataCodec.decode(
            JsonParser.parse("""{"country":{"__typename":"Country","code":"DE","name":"Germany"}}""").getOrThrow
        ).getOrThrow

    "a ref's fields are out of reach outside kyo-apollo" in {
        val ref = germany.country.countryCard
        typeCheckFailure("ref.decoded")("decoded")
        typeCheckFailure("ref.raw")("raw")
        typeCheckFailure("ref.value")("value")
        typeCheckFailure("ref.name")("name")
        assert(ref.toString == s"Ref(${CountryCard.fields.fragmentName})")
    }

    "the store's openers read it" in {
        val store = new ApolloStore(MemoryCache(), cacheKeyGenerator = CacheIdentity.generator(summon[CacheIdentity[CountryT]]))
        val ref   = germany.country.countryCard
        for
            _       <- store.writeOperation(query, germany)
            read    <- store.readFragment(ref)
            watched <- Scope.run(store.watchFragment(ref).take(1).run)
        yield
            assert(read == (name = "Germany"))
            assert(watched == Chunk((name = "Germany")))
        end for
    }
end MaskedFragmentOpenersSpec
