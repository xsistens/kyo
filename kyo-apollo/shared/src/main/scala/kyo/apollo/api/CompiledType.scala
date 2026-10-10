package kyo.apollo.api

/** A GraphQL type reference as it appears in a selection set.
  *
  * Mirrors apollo-kotlin's `CompiledType`: the nullable/list structure of a
  * field's type, built from three wrappers. A GraphQL type like `[String!]!`
  * is modelled as
  * `CompiledNotNullType(CompiledListType(CompiledNotNullType(CompiledNamedType("String"))))`.
  *
  * The wrappers matter downstream: normalization and cache-key computation walk
  * this structure to know whether a value is a list, whether `null` is legal,
  * and which named type a field ultimately resolves to.
  */
sealed trait CompiledType:
    /** The named type at the centre of this reference, unwrapping every
      * non-null and list wrapper.
      */
    def leafType: CompiledNamedType

    /** This type wrapped as non-null (`T` becomes `T!`). */
    def notNull: CompiledNotNullType = CompiledNotNullType(this)

    /** This type wrapped in a list (`T` becomes `[T]`). */
    def list: CompiledListType = CompiledListType(this)
end CompiledType

/** A non-null wrapper: the GraphQL `T!`. */
final case class CompiledNotNullType(ofType: CompiledType) extends CompiledType:
    def leafType: CompiledNamedType = ofType.leafType

/** A list wrapper: the GraphQL `[T]`. */
final case class CompiledListType(ofType: CompiledType) extends CompiledType:
    def leafType: CompiledNamedType = ofType.leafType

/** A named GraphQL type — a scalar, object, interface, union or enum by name.
  * This is always the leaf of a [[CompiledType]] reference.
  */
final case class CompiledNamedType(name: String) extends CompiledType:
    def leafType: CompiledNamedType = this
