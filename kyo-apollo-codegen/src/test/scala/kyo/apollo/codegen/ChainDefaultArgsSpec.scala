package kyo.apollo.codegen

/** Guards the chain-accessor emission for fields with DEFAULTED arguments: an
  * extension method desugars into the same `object Origin` namespace as the
  * plain selector `def field(args…)`, and Scala forbids two overloads that BOTH
  * declare default arguments. The writer therefore hoists the args of such a
  * field onto the sole `apply` of a dedicated `` `field$chain` `` class reached
  * through a parameterless accessor (the extension-level mirror of the
  * `` `field$sel` `` Option-D trick), keeping the `_.field(args…)(sub)` call
  * shape. Surfaced by the spotify-showcase schema, whose connection fields
  * (`tracks(first, after)`) sit on non-root object types — the countries schema
  * never had one.
  */
class ChainDefaultArgsSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val schema = SchemaLoader.fromString(
        """type Query { album(id: ID!): Album }
      |type Album { name: String! tracks(first: Int, after: String): TrackPage! trackCount(mode: String): Int! }
      |type TrackPage { total: Int! }
      |""".stripMargin
    )

    private def albumSrc(using kyo.test.AssertScope) =
        ApolloClientWriter
            .writeSelectors(schema, CodegenConfig(packageName = "test.gen"))
            .find(_.fileName == "Album.scala")
            .getOrElse(fail("Album.scala not emitted"))
            .contents

    "chain accessors for fields with defaulted args" - {

        "an object field with optional args gets a `$chain` class + parameterless accessor" in {
            val src = albumSrc
            // The chain class carries the defaults on its sole `apply`.
            assert(src.contains("final class `tracks$chain`"), src)
            assert(
                src.contains(
                    "def apply[B](first: Maybe[Int] = Absent, after: Maybe[String] = Absent)"
                ),
                src
            )
            // The extension accessor itself is parameterless (no second default set).
            assert(src.contains("def tracks: `tracks$chain`[Acc]"), src)
        }

        "a scalar field with an optional arg gets the same treatment" in {
            val src = albumSrc
            assert(src.contains("final class `trackCount$chain`"), src)
            assert(src.contains("def trackCount: `trackCount$chain`[Acc]"), src)
        }

        "a no-arg field keeps the direct accessor (no chain class)" in {
            val src = albumSrc
            assert(!src.contains("`name$chain`"), src)
            assert(src.contains("def name: SelectionBuilder.Deferrable[Album"), src)
        }

        "generated selectors speak kyo types and import them (never Option/List/None/Nil)" in {
            val src = albumSrc
            assert(src.contains("import kyo.Absent"), src)
            assert(src.contains("import kyo.Chunk"), src)
            assert(src.contains("import kyo.Maybe"), src)
            assert(src.contains("selArgs: Chunk[SelectionBuilder.Arg]"), src)
            assert(src.contains("ScalarCodec.maybe(ScalarCodec.int).encode(first)"), src)
            assert(!src.contains("Option["), src)
            assert(!src.contains("List["), src)
            assert(!src.contains("= None"), src)
            assert(!src.contains("Nil"), src)
        }
    }
end ChainDefaultArgsSpec
