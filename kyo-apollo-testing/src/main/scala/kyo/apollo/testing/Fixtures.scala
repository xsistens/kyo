package kyo.apollo.testing

import kyo.Chunk
import kyo.Schema
import kyo.apollo.api.*
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import scala.collection.immutable.VectorMap

/** The shared GraphQL fixtures the specs re-declared inline across the suite
  * (ADR §2b), promoted into one place: the `{ "value": Int }` operation family
  * (from `WsTestSupport`/`ResilienceInterceptorSpec`) and the `User:1` record
  * family (from `MutationWatcherSpec`/`ReactivitySpec`/`KyoTestSupport`).
  *
  * Two shapes cover nearly every double:
  *
  *   - [[ValueQuery]] / [[ValueSubscription]] — a single scalar `{ value }`, so
  *     assertions read as plain integers; used by the HTTP-transport and
  *     WebSocket specs that only need "did a value arrive".
  *   - [[CurrentUserQuery]] over the [[User]] record — a normalizable entity with
  *     a stable `User:1` key, so a query read and a store write share one record;
  *     the seam every cache/watcher/reactivity assertion flows through.
  *
  * `__typename` is the Scala field name (not `typename`) because kyo-schema
  * `derives Schema` encodes field names verbatim, and normalization / an
  * `IdCacheKeyGenerator` need the wire key to be `__typename`.
  */
object Fixtures:

    // --- the `{ value: Int }` family ------------------------------------------

    /** The `{ "value": Int }` object shape the `value` operations' `data` decodes
      * from; the operation's `D` stays `Int` by transforming this derived object
      * schema, so assertions read as plain integers.
      */
    final case class ValueData(value: Int) derives Schema, CanEqual

    private def valueSchema: Schema[Int] =
        summon[Schema[ValueData]].transform[Int](_.value)(ValueData.apply)

    /** A query whose `data` is a single `{ "value": Int }` object. */
    final case class ValueQuery() extends Query[Int]:
        def name: String             = "Value"
        def document: String         = "query Value { value }"
        def dataSchema: Schema[Int]  = valueSchema
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end ValueQuery

    /** A subscription whose `data` is a single `{ "value": Int }` object; the
      * emitted value is the streamed event so assertions read as plain integers.
      */
    final case class ValueSubscription() extends Subscription[Int]:
        def name: String             = "Value"
        def document: String         = "subscription Value { value }"
        def dataSchema: Schema[Int]  = valueSchema
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Subscription"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end ValueSubscription

    /** An [[ApolloRequest]] for the `value` subscription (mirrors the promoted
      * `WsTestSupport.request()` convenience).
      */
    def valueRequest(): ApolloRequest[Int] = ApolloRequest(ValueSubscription())

    // --- the `User:1` record family -------------------------------------------

    /** A normalizable `User` entity keyed as `User:1`, so a query result and an
      * imperative store write share the one record — the seam reactive re-emission
      * flows through.
      */
    final case class User(__typename: String, id: String, name: String) derives Schema, CanEqual
    final case class UserData(user: User) derives Schema, CanEqual

    private def userSelections: Chunk[CompiledSelection] = Chunk(
        CompiledField("__typename", CompiledNamedType("String")),
        CompiledField("id", CompiledNamedType("String")),
        CompiledField("name", CompiledNamedType("String"))
    )

    final case class CurrentUserQuery() extends Query[UserData]:
        def name: String                 = "CurrentUser"
        def document: String             = "query CurrentUser { user { __typename id name } }"
        def dataSchema: Schema[UserData] = summon[Schema[UserData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections =
                    Chunk(CompiledField("user", CompiledNamedType("User"), selections = userSelections))
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end CurrentUserQuery

    /** The `UserData` for a `User:1` named `name`. */
    def userData(name: String): UserData = UserData(User("User", "1", name))

    /** A clean `{ data: { user } }` payload naming the user via `name`. */
    def body(name: String): String =
        s"""{"data":{"user":{"__typename":"User","id":"1","name":"$name"}}}"""

    /** A partial payload: `data` present *and* a GraphQL `errors` entry. */
    def partialBody(name: String, error: String): String =
        s"""{"data":{"user":{"__typename":"User","id":"1","name":"$name"}},""" +
            s""""errors":[{"message":"$error"}]}"""
end Fixtures
