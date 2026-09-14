package kyo.apollo.api

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.api.JsonCodec
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.interceptor.DefaultApolloInterceptorChain
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.TestIds
import kyo.apollo.runtime.ResponseStream
import scala.collection.immutable.VectorMap

/** Tests that [[OperationRequestBody]] composes a deterministic wire body and
  * that [[Operation.variables]] carries the operation's variable object (with
  * custom-scalar variables encoded through their `Schema`).
  */
class OperationRequestBodySpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** A `DateTime`-like custom scalar used to prove variables are encoded through
      * their `Schema` (a string transform) rather than as a raw value.
      */
    final case class Stamp(iso: String)
    given Schema[Stamp] = Schema.stringSchema.transform[Stamp](Stamp.apply)(_.iso)

    /** Minimal hand-written query with an `Int` variable. */
    final case class MiniQuery(limit: Int) extends Query.Normalizable[Int]:
        def name: String              = "Mini"
        def document: String          = "query Mini($limit: Int!) { x }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema[Int]
        def rootField: CompiledField  = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json = Json.JObj(
            VectorMap("limit" -> SchemaJson.encode(limit))
        )
    end MiniQuery

    /** Query carrying a custom-scalar variable alongside a built-in one. */
    final case class SearchQuery(limit: Int, after: Stamp) extends Query.Normalizable[Int]:
        def name: String              = "Search"
        def document: String          = "query Search($limit: Int!, $after: DateTime) { y }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema[Int]
        def rootField: CompiledField  = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json = Json.JObj(
            VectorMap(
                "limit" -> SchemaJson.encode(limit),
                "after" -> SchemaJson.encode(after)
            )
        )
    end SearchQuery

    /** One `hello(limit:)` root field per operation kind, built through the selection DSL. */
    sealed trait Greeting
    given TypeName[Greeting] = TypeName("Greeting")

    private def hello[Origin]: SelectionBuilder.Deferrable[Origin, (hello: String)] =
        SelectionBuilder.scalar(
            "hello",
            CompiledNamedType("String").notNull,
            ScalarCodec.string,
            Chunk(SelectionBuilder.Arg("limit", CompiledNamedType("Int").notNull, Json.JInt(3)))
        )

    /** A terminal interceptor standing in for the wire: it reads the request's document
      * three times — as an APQ hash, a request body and a retry would — and records each
      * string it got.
      */
    final class ThreeDocumentReads(reads: AtomicRef[Chunk[String]]) extends ApolloInterceptor:
        def intercept[D](
            request: ApolloRequest[D],
            chain: ApolloInterceptorChain
        )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
            Stream.unwrap(
                reads
                    .updateAndGet(_ ++ Chunk(request.operation.document, request.operation.document, request.operation.document))
                    .andThen(Stream.init(Chunk(ApolloResponse[D](request.requestUuid))))
            )
    end ThreeDocumentReads

    "OperationRequestBody" - {

        "request body matches expected JSON with deterministic field order" in {
            assert(
                OperationRequestBody.render(MiniQuery(5)) ==
                    """{"query":"query Mini($limit: Int!) { x }","operationName":"Mini","variables":{"limit":5}}"""
            )
        }

        "extensions are omitted entirely when empty" in {
            val body = OperationRequestBody.apply(MiniQuery(1))
            body match
                case Json.JObj(fields) => assert(!fields.contains("extensions"))
                case other             => fail(s"expected an object, got ${other.render}")
        }

        "extensions are appended last when present" in {
            assert(
                OperationRequestBody.render(
                    MiniQuery(1),
                    extensions = Map("tracing" -> Json.JBool(true))
                ) ==
                    """{"query":"query Mini($limit: Int!) { x }","operationName":"Mini","variables":{"limit":1},"extensions":{"tracing":true}}"""
            )
        }

        "custom-scalar variables are encoded through their Schema" in {
            assert(
                OperationRequestBody.render(SearchQuery(3, Stamp("2026-07-10"))) ==
                    """{"query":"query Search($limit: Int!, $after: DateTime) { y }","operationName":"Search","variables":{"limit":3,"after":"2026-07-10"}}"""
            )
        }

        "variables carries the operation's variable object" in {
            assert(MiniQuery(9).variables.render == """{"limit":9}""")
        }
    }

    "an operation built from a selection" - {

        val operations: Seq[(String, Operation[?])] = Seq(
            "query"                -> hello[RootQuery].toQuery(),
            "decode-only query"    -> hello[RootQuery].map(_.hello).toQuery(),
            "mutation"             -> hello[RootMutation].toMutation(),
            "decode-only mutation" -> hello[RootMutation].map(_.hello).toMutation(),
            "subscription"         -> hello[RootSubscription].toSubscription(),
            "decode-only sub"      -> hello[RootSubscription].map(_.hello).toSubscription()
        )

        "renders its document once: every read of document, rootField and variables is the same reference" in {
            operations.foreach { (kind, op) =>
                assert(op.document eq op.document, kind)
                assert(op.rootField eq op.rootField, kind)
                assert(op.variables eq op.variables, kind)
                assert(op.document.contains("hello(limit: $limit)"), op.document)
            }
            val fragment: Fragment[(hello: String)] = hello[Greeting].toFragment
            assert(fragment.rootField eq fragment.rootField)
        }

        "a transport reading request.operation.document three times sees one string" in {
            val query = hello[RootQuery].toQuery()
            for
                reads <- AtomicRef.init(Chunk.empty[String])
                _ <- Scope.run(StreamProbe.first(
                    DefaultApolloInterceptorChain(Chunk(ThreeDocumentReads(reads)), 0)
                        .proceed(ApolloRequest(query, TestIds.requestUuid))
                ))
                seen <- reads.get
            yield
                assert(seen.size == 3)
                assert(seen.forall(_ eq query.document))
                OperationRequestBody(query) match
                    case Json.JObj(fields) => assert(fields.get("query").exists(_ == Json.JStr(query.document)))
                    case other             => fail(s"expected an object, got ${other.render}")
                end match
            end for
        }
    }
end OperationRequestBodySpec
