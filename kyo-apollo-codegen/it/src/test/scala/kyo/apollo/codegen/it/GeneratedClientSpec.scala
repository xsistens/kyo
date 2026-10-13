package kyo.apollo.codegen.it

import java.time.Instant
import kyo.*
import kyo.apollo.ApolloClient
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheIdentity
import kyo.apollo.codegen.it.generated.*
import kyo.apollo.testing.*

/** The runtime half of the codegen gate. This project's main sources are generated
  * from `codegenExample/schema.graphql` on every compile (`apolloGenerate`), so they
  * compile against kyo-apollo or the build fails; these leaves drive the generated
  * selectors through the real client — HTTP transport, response decoding and the
  * normalized cache — against canned JSON from [[TestHttpEngine]].
  */
class GeneratedClientSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    given CacheIdentity[Country] = CacheIdentity.by(Country)(_.code)

    /** A client over `engine` whose normalized cache keys records through the generated
      * `SchemaIdentities` (Country by code, every other type by id).
      */
    private def cachedClient(engine: TestHttpEngine)(using Frame): ApolloClient < (Sync & Scope) =
        Sync.defer(
            ApolloClient.Config(TestApolloClient.DefaultServerUrl)
                .httpEngine(engine)
                .normalizedCache(MemoryCache(), SchemaIdentities.generator)
        ).map(ApolloClient.init)

    "generated selectors against the client" - {

        "interface fields select through implementors" in {
            val query = Queries.node("u1")(
                _.onUser(
                    _.name,
                    _.favoriteColor,
                    _.nicknames,
                    _.visited(_.code)
                ),
                _.onReport(_.title),
                _.id
            ).toQuery()
            val body =
                """{"data":{"node":{"__typename":"User","name":"Ada","favoriteColor":"GREEN","nicknames":["Countess",null],"visited":[{"__typename":"Country","code":"DE"}],"id":"u1"}}}"""
            val user = (
                name = "Ada",
                favoriteColor = Present(Color.GREEN),
                nicknames = Present(Chunk(Present("Countess"), Absent)),
                visited = Chunk((code = "DE"))
            )
            val node = (onUser = Present(user), onReport = Absent, id = "u1")
            for
                engine  <- TestHttpEngine.returning(body)
                client  <- cachedClient(engine)
                network <- client.query(query).execute
                cached  <- client.query(query).fetchPolicy(FetchPolicy.CacheOnly).execute
            yield
                assert(query.document.contains("... on User { name favoriteColor nicknames visited {"), query.document)
                assert(query.document.contains("... on Report { title }"), query.document)
                assert(network.error.isEmpty, network.toString)
                assert(network.data.map(_.node) == Present(Present(node)), network.toString)
                assert(cached.error.isEmpty, cached.toString)
                assert(cached.data.map(_.node) == Present(Present(node)), cached.toString)
            end for
        }

        "an unknown enum value decodes to Unknown__, and encodes back as its name" in {
            val query = Queries.usersByColor(Color.Unknown__("TEAL"))(_.id, _.favoriteColor).toQuery()
            val body  =
                """{"data":{"usersByColor":[{"__typename":"User","id":"u1","favoriteColor":"TEAL"},{"__typename":"User","id":"u2","favoriteColor":"RED"}]}}"""
            val colors = Present(Chunk(Present(Color.Unknown__("TEAL")), Present(Color.RED)))
            for
                engine  <- TestHttpEngine.returning(body)
                client  <- cachedClient(engine)
                network <- client.query(query).execute
                request <- engine.lastRequest
                cached  <- client.query(query).fetchPolicy(FetchPolicy.CacheOnly).execute
            yield
                assert(network.error.isEmpty, network.toString)
                assert(network.data.map(_.usersByColor.map(_.favoriteColor)) == colors, network.toString)
                // The argument went out as the raw name, and the cache wrote and read it back.
                assert(request.exists(_.fields.body.text.exists(_.contains("\"TEAL\""))), request.toString)
                assert(cached.data.map(_.usersByColor.map(_.favoriteColor)) == colors, cached.toString)
                assert(Color.values == Chunk(Color.RED, Color.GREEN, Color.BLUE))
                assert(Color.valueOf("RED") == Present(Color.RED))
                assert(Color.valueOf("TEAL") == Absent)
            end for
        }

        "union branches, a mapped custom scalar and a nested object decode from one response" in {
            val query = Queries.search("de")(
                _.onCountry(_.code, _.updatedAt),
                _.onReport(_.title, _.country(_.name))
            ).toQuery()
            val body =
                """{"data":{"search":[{"__typename":"Country","code":"DE","updatedAt":"2026-09-15T10:00:00Z"},{"__typename":"Report","title":"Berlin","country":{"__typename":"Country","name":"Germany"}}]}}"""
            val hits = Chunk(
                (onCountry = Present((code = "DE", updatedAt = Present(Instant.parse("2026-09-15T10:00:00Z")))), onReport = Absent),
                (onCountry = Absent, onReport = Present((title = "Berlin", country = (name = "Germany"))))
            )
            for
                engine   <- TestHttpEngine.returning(body)
                client   <- TestApolloClient.cacheless(engine)
                response <- client.query(query).execute
            yield
                assert(response.error.isEmpty, response.toString)
                assert(response.data.map(_.search) == Present(hits), response.toString)
            end for
        }

        "a root list selects several root fields, a client field among them, in one operation" in {
            val query = Queries(
                _.country("DE")(_.code, _.name),
                _.cartOpen,
                _.usersByColor(Color.RED)(_.id)
            ).toQuery("Home")
            val body =
                """{"data":{"country":{"__typename":"Country","code":"DE","name":"Germany"},"usersByColor":[{"__typename":"User","id":"u1"}]}}"""
            for
                engine   <- TestHttpEngine.returning(body)
                client   <- cachedClient(engine)
                response <- client.query(query).execute
            yield
                // The client field is pruned from the document and decodes to its default.
                assert(query.document.contains("country(code: $code) { __typename code name }"), query.document)
                assert(!query.document.contains("cartOpen"), query.document)
                assert(response.error.isEmpty, response.toString)
                assert(
                    response.data == Present((
                        country = Present((code = "DE", name = "Germany")),
                        cartOpen = false,
                        usersByColor = Chunk((id = "u1"))
                    )),
                    response.toString
                )
            end for
        }

        "a list entry is a single field: chaining a second field onto it does not compile" in {
            val errors = scala.compiletime.testing.typeCheckErrors(
                """kyo.apollo.codegen.it.generated.Queries.country("DE")(_.code.name)"""
            )
            assert(errors.nonEmpty)
            assert(errors.exists(_.message.contains("name is not a member")), errors.map(_.message).mkString("\n"))
        }

        "a list types as the concatenation of its entries, identical to the `~` value form" in {
            type Expected = (country: Maybe[(code: String, name: String, capital: Maybe[String])])
            val listed = Queries.country("DE")(_.code, _.name, _.capital)
            val joined = Queries.country("DE")(Country.code ~ Country.name ~ Country.capital)
            summon[listed.type <:< SelectionBuilder.Deferrable[RootQuery, Expected]]
            summon[joined.type <:< SelectionBuilder.Deferrable[RootQuery, Expected]]
            assert(listed.toQuery("C").document == joined.toQuery("C").document)
        }

        "a list with an expected type resolves: every arity's result type is checked against it first" in {
            // Regression: with nested `Concat[Concat[D1, D2], D3]` result types, this
            // ascription made overloading resolution unfold the arity-22 candidate past
            // the compiler's recursion limit ("Recursion limit exceeded").
            type Row = (code: String, name: String, capital: Maybe[String])
            val ascribed: SelectionBuilder.Fields[Country, Row] = Country(_.code, _.name, _.capital)
            val nested: SelectionBuilder.Fields[
                RootQuery,
                (search: Chunk[(onCountry: Maybe[(code: String)], onReport: Maybe[(title: String)])])
            ] =
                Queries(_.search("x")(_.onCountry(_.code), _.onReport(_.title)))
            assert(ascribed.selections.size == 3)
            assert(nested.selections.size == 1)
        }
    }
end GeneratedClientSpec
