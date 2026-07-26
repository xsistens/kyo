package kyo.apollo.api

/** The GraphQL type name a phantom `Origin` selection-root denotes at runtime.
  *
  * A [[SelectionBuilder]]'s `Origin` is a compile-time-only marker (`Country`,
  * `RootQuery`, …) that carries no name at runtime. A [[kyo.apollo.ClientField]]
  * needs that name to build the owning entity's [[kyo.apollo.cache.normalized.api.CacheKey]]
  * and its write/read fragment, so it summons a `given TypeName[Origin]`.
  *
  * Codegen emits one per schema object type (`given TypeName[Country] =
  * TypeName("Country")`), right beside the phantom marker it generates; the three
  * universal operation roots are provided here.
  */
final case class TypeName[Origin](name: String)

object TypeName:
    given TypeName[RootQuery]        = TypeName("Query")
    given TypeName[RootMutation]     = TypeName("Mutation")
    given TypeName[RootSubscription] = TypeName("Subscription")
end TypeName
