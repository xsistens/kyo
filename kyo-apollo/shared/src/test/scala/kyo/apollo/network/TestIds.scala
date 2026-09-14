package kyo.apollo.network

/** Fixed request ids for tests that need an [[ApolloRequest]] value but do not
  * exercise how an execution mints its id.
  */
object TestIds:

    /** The id of a request built directly in a test. */
    val requestUuid: Uuid = Uuid("00000000-0000-4000-8000-000000000001")

    /** A second, distinct id. */
    val otherUuid: Uuid = Uuid("00000000-0000-4000-8000-000000000002")
end TestIds
