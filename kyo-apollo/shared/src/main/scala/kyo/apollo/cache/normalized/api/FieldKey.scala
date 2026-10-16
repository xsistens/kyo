package kyo.apollo.cache.normalized.api

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.apollo.api.CompiledArgument
import kyo.apollo.api.CompiledArgumentValue
import kyo.apollo.api.CompiledField
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** The key a field's value is stored under within a [[Record]] — an opaque type
  * over the storage-key string, so a field key can never be confused with a
  * [[CacheKey]] or an arbitrary string.
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
opaque type FieldKey = String

object FieldKey:
    /** The storage key of the implicit `__typename` field every normalized object
      * carries.
      */
    val Typename: FieldKey = "__typename"

    /** The storage key for `field`, resolving any variable arguments against
      * `variables` (a map of variable name → already-encoded JSON value, as
      * produced by `Operation.variables`).
      *
      * `keyArgs` restricts the arguments that take part (declaration order
      * preserved), so a field can collapse arguments that must not partition its
      * cache slot — e.g. a connection field dropping its pagination arguments so
      * every page addresses one logical value. `Present(Chunk.empty)` yields the
      * bare field name; `Absent` (the default) keeps every argument.
      */
    def apply(
        field: CompiledField,
        variables: Map[String, Json] = Map.empty,
        keyArgs: Maybe[Chunk[String]] = Absent
    ): FieldKey =
        val arguments = keyArgs.fold(field.arguments)(kept => field.arguments.filter(arg => kept.contains(arg.name)))
        render(field.name, arguments, variables)
    end apply

    extension (key: FieldKey)
        /** The key's string form — for messages, diagnostics and serialization
          * boundaries (e.g. the devtools cache dump). Never feed it back as a key.
          */
        def render: String = key

        /** The field name without its normalized `(args)` suffix, so a policy keyed
          * by field name matches whether or not the stored field carries arguments.
          */
        def baseName: String = key.takeWhile(_ != '(')
    end extension

    given CanEqual[FieldKey, FieldKey] = CanEqual.derived

    /** Render a field name plus its (already-selected) arguments into a storage
      * key: the bare name when there are no arguments, otherwise
      * `name({canonical-args})` with object keys sorted recursively.
      */
    private def render(
        name: String,
        arguments: Chunk[CompiledArgument],
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
