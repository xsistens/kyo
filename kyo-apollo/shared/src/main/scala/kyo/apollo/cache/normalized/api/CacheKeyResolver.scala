package kyo.apollo.cache.normalized.api

import kyo.Absent
import kyo.Maybe
import kyo.Present
import kyo.apollo.api.CompiledArgumentValue
import kyo.apollo.api.CompiledField
import kyo.apollo.json.Json

/** The read-side counterpart to [[CacheKeyGenerator]]: given a field being read,
  * decide which record (if any) to redirect the read to.
  *
  * This powers *cache redirects* — reading a field whose value was never written
  * under that field, but which can be answered from an existing record. The
  * canonical case: a `book(id: "42"): Book` field can be served from the
  * `Book:42` record written by an earlier `books` list query, so a detail screen
  * hits the cache without its own network round-trip. Returning `None` means "no
  * redirect" — the reader resolves the field the normal way, from the parent
  * record's own stored value. Mirrors apollo-kotlin's `CacheKeyResolver`.
  */
trait CacheKeyResolver:
    /** The record `field` should be read from instead of its parent's stored
      * value, or `None` to resolve `field` normally. `variables` are the
      * operation's variables (name → already-encoded JSON), used to resolve any
      * variable arguments on `field`.
      */
    def cacheKeyForField(
        field: CompiledField,
        variables: Map[String, Json]
    ): Maybe[CacheKey]
end CacheKeyResolver

object CacheKeyResolver:
    /** The default resolver: never redirects, so every field is read from its
      * parent record. This is the identity policy a store uses until a redirect
      * is configured.
      */
    val default: CacheKeyResolver = (_, _) => Absent

    /** A resolver that redirects any field carrying an `idArgument` to the record
      * `<FieldLeafType>:<idValue>`, e.g. `book(id: "42"): Book` → `Book:42`.
      *
      * The typename comes from the field's own return type
      * ([[CompiledField.fieldType]]'s leaf), and the id from the named argument
      * resolved against `variables`. Fields without that argument are not
      * redirected. This is the common id-based redirect most schemas want.
      *
      * @param idArgument the argument naming the target record's id (default `id`)
      */
    def byIdArgument(idArgument: String = "id"): CacheKeyResolver =
        (field, variables) =>
            Maybe
                .fromOption(field.arguments.find(_.name == idArgument))
                .flatMap(arg => argumentString(arg.value, variables))
                .map(id => CacheKey(field.fieldType.leafType.name, id))

    /** Resolve an argument value to its raw id string: literals pass through,
      * variable references are looked up, and the value is rendered by
      * [[CacheKey.scalarString]] — the same rendering the key generators use, so a
      * redirect names the record the normalizer wrote. Composite/`null` values
      * are not usable as ids and yield `Absent`.
      */
    private def argumentString(
        value: CompiledArgumentValue,
        variables: Map[String, Json]
    ): Maybe[String] =
        value match
            case CompiledArgumentValue.Literal(json)  => CacheKey.scalarString(json)
            case CompiledArgumentValue.Variable(name) => CacheKey.scalarString(variables.getOrElse(name, Json.JNull))
end CacheKeyResolver
