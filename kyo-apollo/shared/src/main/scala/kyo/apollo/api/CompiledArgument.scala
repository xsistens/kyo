package kyo.apollo.api

import kyo.apollo.json.Json

/** The value supplied to a [[CompiledArgument]].
  *
  * A compiled argument is either an inline literal baked into the document at
  * codegen time, or a reference to one of the operation's variables that is
  * resolved when the request is serialized. Mirrors apollo-kotlin, which stores
  * an argument value as either a plain value or a `CompiledVariable`.
  */
sealed trait CompiledArgumentValue

object CompiledArgumentValue:
    /** An inline literal, already lowered to the [[kyo.apollo.json.Json]] AST. */
    final case class Literal(value: Json) extends CompiledArgumentValue

    /** A reference to operation variable `name` (the GraphQL `$name`). */
    final case class Variable(name: String) extends CompiledArgumentValue
end CompiledArgumentValue

/** A single argument on a [[CompiledField]], e.g. `launches(limit: $limit)`.
  *
  * @param name  the argument name as declared on the field
  * @param value the literal or variable reference supplied for it
  */
final case class CompiledArgument(
    name: String,
    value: CompiledArgumentValue
)

object CompiledArgument:
    /** Convenience: an argument bound to operation variable `variableName`. */
    def variable(name: String, variableName: String): CompiledArgument =
        CompiledArgument(name, CompiledArgumentValue.Variable(variableName))

    /** Convenience: an argument bound to variable of the same `name`. */
    def variable(name: String): CompiledArgument =
        variable(name, name)

    /** Convenience: an argument with an inline literal value. */
    def literal(name: String, value: Json): CompiledArgument =
        CompiledArgument(name, CompiledArgumentValue.Literal(value))
end CompiledArgument
