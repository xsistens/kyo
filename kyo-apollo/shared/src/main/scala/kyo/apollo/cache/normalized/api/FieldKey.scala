package kyo.apollo.cache.normalized.api

import kyo.apollo.api.CompiledArgument
import kyo.apollo.api.CompiledArgumentValue
import kyo.apollo.api.CompiledField
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Computes the key a field's value is stored under within a [[Record]].
  *
  * A field with no arguments is stored under its plain name. A field *with*
  * arguments is stored under `name({canonical-args})`, so `user(id: 1)` and
  * `user(id: 2)` occupy distinct slots in the same record and never clobber
  * each other. Arguments are resolved against the operation's variables and
  * rendered canonically (object keys sorted recursively) so the same logical
  * arguments always produce the same key regardless of declaration order.
  * Mirrors apollo-kotlin's `CompiledField.nameWithArguments`.
  *
  * The field's schema `name` is used, not its response alias: two aliases of the
  * same field+arguments address the same stored value.
  */
object FieldKey:
    /** The storage key for `field`, resolving any variable arguments against
      * `variables` (a map of variable name → already-encoded JSON value, as
      * produced by `Operation.variables`). Defaults to no variables.
      */
    def apply(
        field: CompiledField,
        variables: Map[String, Json] = Map.empty
    ): String =
        render(field.name, field.arguments, variables)

    /** The storage key for `field` using only the arguments named in `keyArgs`
      * (declaration order preserved), so a field can collapse arguments that must
      * not partition its cache slot — e.g. a connection field dropping its
      * pagination arguments so every page addresses one logical value. An empty
      * `keyArgs` yields the bare field name. Arguments not listed are ignored.
      */
    def apply(
        field: CompiledField,
        variables: Map[String, Json],
        keyArgs: List[String]
    ): String =
        render(field.name, field.arguments.filter(arg => keyArgs.contains(arg.name)), variables)

    /** Render a field name plus its (already-selected) arguments into a storage
      * key: the bare name when there are no arguments, otherwise
      * `name({canonical-args})` with object keys sorted recursively.
      */
    private def render(
        name: String,
        arguments: List[CompiledArgument],
        variables: Map[String, Json]
    ): String =
        if arguments.isEmpty then name
        else
            val resolved  = arguments.map(arg => arg.name -> resolve(arg.value, variables))
            val canonical = Json.JObj(VectorMap.from(resolved.sortBy(_._1)))
            s"$name(${sortKeys(canonical).render})"

    /** Resolve an argument value to concrete JSON: literals pass through, variable
      * references are looked up (absent variables render as `null`).
      */
    private def resolve(
        value: CompiledArgumentValue,
        variables: Map[String, Json]
    ): Json = value match
        case CompiledArgumentValue.Literal(json)  => json
        case CompiledArgumentValue.Variable(name) => variables.getOrElse(name, Json.JNull)

    /** Recursively sort object keys so structurally-equal argument objects render
      * identically regardless of the order fields were written in.
      */
    private def sortKeys(json: Json): Json = json match
        case Json.JObj(fields) =>
            Json.JObj(VectorMap.from(fields.toList.sortBy(_._1).map((k, v) => k -> sortKeys(v))))
        case Json.JArr(items) => Json.JArr(items.map(sortKeys))
        case scalar           => scalar
end FieldKey
