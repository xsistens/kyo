package kyo.apollo.cache

import kyo.Absent
import kyo.Chunk
import kyo.Present
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.json.Json

/** Unit tests for Phase 04 cache-key generation: [[CacheKeyGenerator]] /
  * [[IdCacheKeyGenerator]] (write side) and [[CacheKeyResolver]] (read side).
  */
class CacheKeyGenerationSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def field(
        name: String,
        typeName: String = "X",
        args: Chunk[CompiledArgument] = Chunk.empty
    ): CompiledField =
        CompiledField(name, CompiledNamedType(typeName), arguments = args)

    private def ctx(
        path: List[String] = Nil,
        variables: Map[String, Json] = Map.empty
    ): CacheKeyGeneratorContext =
        CacheKeyGeneratorContext(field("countries"), variables, path)

    // --- IdCacheKeyGenerator: id-based keys -----------------------------------

    private val gen = CacheKeyGenerator.default

    // --- CacheKeyResolver: id-argument redirect -------------------------------

    private val redirect = CacheKeyResolver.byIdArgument()

    "cache-key generation" - {

        "keys an object by __typename and its id field" in {
            val obj = Map("__typename" -> Json.JStr("Book"), "id" -> Json.JStr("42"))
            assert(gen.cacheKeyForObject(obj, ctx()) == Present(CacheKey("Book", "42")))
        }

        "numeric ids render without a trailing .0" in {
            val obj = Map("__typename" -> Json.JStr("Book"), "id" -> Json.JNum(42))
            assert(gen.cacheKeyForObject(obj, ctx()) == Present(CacheKey("Book:42")))
        }

        "falls back to _id when id is absent" in {
            val obj = Map("__typename" -> Json.JStr("User"), "_id" -> Json.JStr("u1"))
            assert(gen.cacheKeyForObject(obj, ctx()) == Present(CacheKey("User", "u1")))
        }

        "prefers id over _id when both are present" in {
            val obj = Map(
                "__typename" -> Json.JStr("User"),
                "id"         -> Json.JStr("primary"),
                "_id"        -> Json.JStr("secondary")
            )
            assert(gen.cacheKeyForObject(obj, ctx()) == Present(CacheKey("User", "primary")))
        }

        "configurable key fields are tried in order" in {
            val custom = IdCacheKeyGenerator(List("code"))
            val obj    = Map("__typename" -> Json.JStr("Country"), "code" -> Json.JStr("DE"))
            assert(custom.cacheKeyForObject(obj, ctx()) == Present(CacheKey("Country", "DE")))
        }

        // --- IdCacheKeyGenerator: path fallback -----------------------------------

        "falls back to a path key when the object has no id" in {
            val obj = Map("__typename" -> Json.JStr("Stats"), "views" -> Json.JNum(10))
            assert(
                gen.cacheKeyForObject(obj, ctx(path = List("QUERY_ROOT", "stats"))) ==
                    Present(CacheKey("QUERY_ROOT.stats"))
            )
        }

        "falls back to a path key when __typename is missing" in {
            val obj = Map("id" -> Json.JStr("42"))
            assert(
                gen.cacheKeyForObject(obj, ctx(path = List("QUERY_ROOT", "book"))) ==
                    Present(CacheKey("QUERY_ROOT.book"))
            )
        }

        "returns None when there is neither an id nor a path" in {
            val obj = Map("views" -> Json.JNum(10))
            assert(gen.cacheKeyForObject(obj, ctx()) == Absent)
        }

        "a present-but-null id does not produce a key" in {
            val obj = Map("__typename" -> Json.JStr("Book"), "id" -> Json.JNull)
            assert(gen.cacheKeyForObject(obj, ctx()) == Absent)
        }

        // --- CacheKey.fromPath ----------------------------------------------------

        "fromPath joins rooted path segments with dots" in {
            assert(
                CacheKey.fromPath(List("QUERY_ROOT", "countries", "0")) ==
                    CacheKey("QUERY_ROOT.countries.0")
            )
        }

        // --- CacheKeyResolver: default (no redirect) ------------------------------

        "the default resolver never redirects" in {
            val f = field("book", "Book", Chunk(CompiledArgument.literal("id", Json.JStr("42"))))
            assert(CacheKeyResolver.default.cacheKeyForField(f, Map.empty) == Absent)
        }

        "byIdArgument redirects book(id:) to the Book:id record" in {
            val f = field("book", "Book", Chunk(CompiledArgument.literal("id", Json.JStr("42"))))
            assert(redirect.cacheKeyForField(f, Map.empty) == Present(CacheKey("Book", "42")))
        }

        "byIdArgument resolves a variable id argument against variables" in {
            val f = field("book", "Book", Chunk(CompiledArgument.variable("id", "bookId")))
            assert(
                redirect.cacheKeyForField(f, Map("bookId" -> Json.JStr("99"))) ==
                    Present(CacheKey("Book", "99"))
            )
        }

        "byIdArgument uses the field's own return type as the typename" in {
            val f = field("favouriteAuthor", "Author", Chunk(CompiledArgument.literal("id", Json.JNum(7))))
            assert(redirect.cacheKeyForField(f, Map.empty) == Present(CacheKey("Author", "7")))
        }

        "byIdArgument does not redirect a field without the id argument" in {
            val f = field("books", "Book", Chunk(CompiledArgument.literal("limit", Json.JNum(10))))
            assert(redirect.cacheKeyForField(f, Map.empty) == Absent)
        }

        "byIdArgument honours a custom id argument name" in {
            val resolver = CacheKeyResolver.byIdArgument("code")
            val f        = field("country", "Country", Chunk(CompiledArgument.literal("code", Json.JStr("FR"))))
            assert(resolver.cacheKeyForField(f, Map.empty) == Present(CacheKey("Country", "FR")))
        }
    }
end CacheKeyGenerationSpec
