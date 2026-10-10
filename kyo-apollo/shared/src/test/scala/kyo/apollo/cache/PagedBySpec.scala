package kyo.apollo.cache

import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser

/** `pagedBy` turns a single-connection query into a page builder: every page has one
  * document and differs only in the cursor variable, and with a
  * [[ConnectionFieldPolicy]] the pages accumulate in one cache slot that the first
  * page's query reads back whole.
  */
class PagedBySpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    sealed trait CountryT
    object CountryT:
        given TypeName[CountryT]                                        = TypeName("Country")
        def code: SelectionBuilder.Deferrable[CountryT, (code: String)] =
            SelectionBuilder.scalar("code", CompiledNamedType("ID").notNull, ScalarCodec.id)
    end CountryT

    sealed trait EdgeT
    object EdgeT:
        given TypeName[EdgeT]                                            = TypeName("CountryEdge")
        def cursor: SelectionBuilder.Deferrable[EdgeT, (cursor: String)] =
            SelectionBuilder.scalar("cursor", CompiledNamedType("String").notNull, ScalarCodec.string)
        def node[A](sel: SelectionBuilder.Bidirectional[CountryT, A]): SelectionBuilder.Deferrable[EdgeT, (node: A)] =
            SelectionBuilder.obj("node", CompiledNamedType("Country").notNull, Chunk.empty, sel, SelectionBuilder.Nesting.Leaf)
    end EdgeT

    sealed trait PageInfoT
    object PageInfoT:
        given TypeName[PageInfoT]                                                       = TypeName("PageInfo")
        def hasNextPage: SelectionBuilder.Deferrable[PageInfoT, (hasNextPage: Boolean)] =
            SelectionBuilder.scalar("hasNextPage", CompiledNamedType("Boolean").notNull, ScalarCodec.boolean)
        def endCursor: SelectionBuilder.Deferrable[PageInfoT, (endCursor: Maybe[String])] =
            SelectionBuilder.scalar("endCursor", CompiledNamedType("String"), ScalarCodec.maybe(ScalarCodec.string))
    end PageInfoT

    sealed trait ConnectionT
    object ConnectionT:
        given TypeName[ConnectionT] = TypeName("CountryConnection")
        def edges[A](sel: SelectionBuilder.Bidirectional[EdgeT, A]): SelectionBuilder.Deferrable[ConnectionT, (edges: Chunk[A])] =
            SelectionBuilder.obj(
                "edges",
                CompiledNamedType("CountryEdge").notNull.list.notNull,
                Chunk.empty,
                sel,
                SelectionBuilder.Nesting.Listed(SelectionBuilder.Nesting.Leaf)
            )
        def pageInfo[A](sel: SelectionBuilder.Bidirectional[PageInfoT, A]): SelectionBuilder.Deferrable[ConnectionT, (pageInfo: A)] =
            SelectionBuilder.obj("pageInfo", CompiledNamedType("PageInfo").notNull, Chunk.empty, sel, SelectionBuilder.Nesting.Leaf)
    end ConnectionT

    private def countriesPage[A](first: Int, after: Maybe[String])(
        sel: SelectionBuilder.Bidirectional[ConnectionT, A]
    ): SelectionBuilder.Deferrable[RootQuery, (countriesPage: A)] =
        SelectionBuilder.obj(
            "countriesPage",
            CompiledNamedType("CountryConnection").notNull,
            Chunk(
                SelectionBuilder.Arg("first", CompiledNamedType("Int").notNull, ScalarCodec.int.encode(first)),
                SelectionBuilder.Arg("after", CompiledNamedType("String"), ScalarCodec.maybe(ScalarCodec.string).encode(after))
            ),
            sel,
            SelectionBuilder.Nesting.Leaf
        )

    given CacheIdentity[CountryT] = CacheIdentity.by(_ ~ CountryT.code)
    given CacheIdentity[EdgeT]    = CacheIdentity.by(_ ~ EdgeT.cursor)

    private val page =
        countriesPage(first = 2, after = Absent)(
            ConnectionT.edges(EdgeT.cursor ~ EdgeT.node(CountryT.code)) ~
                ConnectionT.pageInfo(PageInfoT.hasNextPage ~ PageInfoT.endCursor)
        ).pagedBy()

    private def store(): ApolloStore =
        new ApolloStore(
            MemoryCache(),
            cacheKeyGenerator = CacheIdentity.generator(summon[CacheIdentity[CountryT]], summon[CacheIdentity[EdgeT]]),
            fieldPolicies = FieldPolicies.fromList(ConnectionFieldPolicy("Query", "countriesPage", "CountryConnection"))
        )

    private def body(codes: Seq[String], next: Maybe[String]): Json =
        val edges = codes.map(c => s"""{"__typename":"CountryEdge","cursor":"c$c","node":{"__typename":"Country","code":"$c"}}""")
        val end   = next.fold("null")(c => s"\"$c\"")
        JsonParser.parse(
            s"""{"countriesPage":{"__typename":"CountryConnection","edges":[${edges.mkString(",")}],""" +
                s""""pageInfo":{"__typename":"PageInfo","hasNextPage":${next.isDefined},"endCursor":$end}}}"""
        ).getOrThrow
    end body

    "pagedBy" - {

        "every page has the same document and differs only in the cursor variable" in {
            val first  = page(Absent)
            val second = page(Present("cFR"))
            assert(first.document == second.document)
            assert(first.variables != second.variables)
            assert(second.variables.toString.contains("cFR"))
        }

        "a query without the cursor argument fails fast" in {
            val noCursor = SelectionBuilder.obj[RootQuery, (country: (code: String)), (code: String)](
                "country",
                CompiledNamedType("Country").notNull,
                Chunk.empty,
                CountryT.code,
                SelectionBuilder.Nesting.Leaf
            )
            assert(scala.util.Try(noCursor.pagedBy()).isFailure)
        }

        "with a ConnectionFieldPolicy, the first page's query reads every page written" in {
            val s = store()
            for
                _    <- s.writeOperation(page(Absent), page(Absent).dataCodec.decode(body(Seq("DE", "FR"), Present("cFR"))).getOrThrow)
                _    <- s.writeOperation(page(Present("cFR")), page(Absent).dataCodec.decode(body(Seq("IT", "ES"), Absent)).getOrThrow)
                read <- s.readOperation(page(Absent))
            yield
                assert(read.countriesPage.edges.map(_.node.code) == Chunk("DE", "FR", "IT", "ES"))
                assert(read.countriesPage.pageInfo.hasNextPage == false)
            end for
        }

        "a watch on the first page re-emits with the longer list when the next page lands" in {
            // The server answers the first page, or the second when the request carries its cursor.
            val engine = new kyo.apollo.network.http.HttpEngine:
                def execute(request: kyo.apollo.network.http.HttpEngine.Request)(using
                    Frame
                ): kyo.apollo.network.http.HttpEngine.Response < Async =
                    val second = request.fields.body.text.exists(_.contains("cFR"))
                    val data   = if second then body(Seq("IT", "ES"), Absent) else body(Seq("DE", "FR"), Present("cFR"))
                    kyo.apollo.network.http.HttpEngine.response(HttpStatus.OK, s"""{"data":${data.render}}""")
                end execute
            for
                client <- kyo.apollo.ApolloClient.init(
                    kyo.apollo.ApolloClient.Config("https://example.com/graphql")
                        .httpEngine(engine)
                        .normalizedCache(
                            MemoryCache(),
                            CacheIdentity.generator(summon[CacheIdentity[CountryT]], summon[CacheIdentity[EdgeT]]),
                            fieldPolicies = FieldPolicies.fromList(ConnectionFieldPolicy("Query", "countriesPage", "CountryConnection"))
                        )
                )
                emitted <- Channel.init[Chunk[String]](4)
                watch   <- Fiber.init(client.query(page(Absent)).watch().take(2).foreach { r =>
                    emitted.put(r.data.fold(Chunk.empty[String])(_.countriesPage.edges.map(_.node.code)))
                })
                first  <- emitted.take
                _      <- client.query(page(Present("cFR"))).fetchPolicy(FetchPolicy.NetworkOnly).execute
                second <- emitted.take
                _      <- watch.get
            yield
                assert(first == Chunk("DE", "FR"))
                assert(second == Chunk("DE", "FR", "IT", "ES"))
            end for
        }
    }
end PagedBySpec
