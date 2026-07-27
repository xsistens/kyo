package kyo.apollo.cache.normalized.api

import kyo.Absent
import kyo.Maybe
import kyo.Present
import kyo.apollo.api.CompiledField
import kyo.apollo.json.Json

/** The information a [[CacheKeyGenerator]] gets about the object it is keying.
  *
  * Mirrors apollo-kotlin's `CacheKeyGeneratorContext`, plus [[path]] so a
  * generator can fall back to a stable, position-based key when an object
  * carries no id (apollo-kotlin computes that fallback inside the normalizer;
  * here it is handed to the generator so key policy lives in one place).
  *
  * @param field     the [[CompiledField]] whose selection produced this object
  * @param variables the operation's variables (name → already-encoded JSON), as
  *                  produced by `Operation.variables`
  * @param path      the response path to this object from the root record,
  *                  already rooted (e.g. `List("QUERY_ROOT", "countries", "0")`);
  *                  used only for the id-less path fallback
  */
final case class CacheKeyGeneratorContext(
    field: CompiledField,
    variables: Map[String, Json] = Map.empty,
    path: List[String] = Nil
)

/** Computes the [[CacheKey]] a normalized object is stored under.
  *
  * The normalizer calls this once per object it encounters while walking a
  * response: the returned key decides which [[Record]] the object's fields land
  * in, and therefore whether two occurrences of the same entity dedupe to one
  * record. Returning `None` means "no stable key" — the normalizer is then free
  * to key the object by position. Mirrors apollo-kotlin's `CacheKeyGenerator`.
  */
trait CacheKeyGenerator:
    /** The key for `obj` (an object's own field map, e.g. the contents of a
      * `Json.JObj`) given its [[CacheKeyGeneratorContext]], or `None` when the
      * generator cannot produce a stable key for it.
      */
    def cacheKeyForObject(
        obj: Map[String, Json],
        context: CacheKeyGeneratorContext
    ): Maybe[CacheKey]
end CacheKeyGenerator

object CacheKeyGenerator:
    /** The default generator: [[IdCacheKeyGenerator]] over `id`/`_id`. */
    val default: CacheKeyGenerator = IdCacheKeyGenerator()

/** Keys an object by its `__typename` plus the first present of `keyFields`,
  * e.g. `{ __typename: "Country", code: "DE" }` with `keyFields = List("code")`
  * becomes `Country:DE`. When neither the typename nor any key field is present,
  * it falls back to the object's response [[CacheKeyGeneratorContext.path]] so
  * id-less objects still get a stable, collision-free record key. Mirrors
  * apollo-kotlin's `IdCacheKeyGenerator`.
  *
  * @param keyFields the candidate id fields to try in order (default `id`,
  *                  then `_id`); the first one present and scalar wins
  */
final class IdCacheKeyGenerator(
    keyFields: List[String] = IdCacheKeyGenerator.DefaultKeyFields
) extends CacheKeyGenerator:

    def cacheKeyForObject(
        obj: Map[String, Json],
        context: CacheKeyGeneratorContext
    ): Maybe[CacheKey] =
        idKey(obj).orElse(pathKey(context))

    /** `Typename:id` when both `__typename` and a scalar key field are present. */
    private def idKey(obj: Map[String, Json]): Maybe[CacheKey] =
        for
            typename <- Maybe.fromOption(obj.get("__typename").collect { case Json.JStr(t) => t })
            id <- Maybe.fromOption(
                keyFields.iterator
                    .flatMap(obj.get)
                    .flatMap(IdCacheKeyGenerator.scalarString(_).iterator)
                    .nextOption()
            )
        yield CacheKey(typename, id)

    /** The rooted response path as a key, when one is available. */
    private def pathKey(context: CacheKeyGeneratorContext): Maybe[CacheKey] =
        if context.path.nonEmpty then Present(CacheKey.fromPath(context.path)) else Absent
end IdCacheKeyGenerator

object IdCacheKeyGenerator:
    /** The id fields tried, in order, when none is configured explicitly. */
    val DefaultKeyFields: List[String] = List("id", "_id")

    /** Render a JSON scalar as its raw id string (`42`, not `"42"` or `42.0`);
      * composites and `null` yield `None` and are not usable as ids.
      */
    private def scalarString(json: Json): Maybe[String] = json match
        case Json.JStr(s)  => Present(s)
        case Json.JNum(_)  => Present(json.render)
        case Json.JBool(b) => Present(b.toString)
        case _             => Absent
end IdCacheKeyGenerator
