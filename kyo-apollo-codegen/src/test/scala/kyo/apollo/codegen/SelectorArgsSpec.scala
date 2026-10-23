package kyo.apollo.codegen

/** Guards how a field's arguments reach the generated selectors. Scala forbids two
  * overloads that BOTH declare default arguments, and the nested-selection forms
  * (`SelectionList` arities plus the value form) are overloads of `apply`, so the
  * args live on the single, non-overloaded selector method that returns the
  * `` `field$sel` `` object: `_.tracks(Present(50), Absent)(_.name)`. A scalar
  * field with args is a plain method with defaults. Connection fields such as
  * `tracks(first, after)` sit on non-root object types.
  */
class SelectorArgsSpec extends kyo.test.Test[Any]:

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

    "selector arguments" - {

        "an object field with optional args carries the defaults on the method returning its `$sel` object" in {
            val src = albumSrc
            assert(src.contains("def tracks(first: Maybe[Int] = Absent, after: Maybe[String] = Absent): `tracks$sel` ="), src)
            // The selection forms come from `SelectionList` and the value form, with no args of their own.
            assert(src.contains("with SelectionList[TrackPage.type, TrackPage,"), src)
            assert(src.contains("def apply[A](sel: SelectionBuilder.Bidirectional[TrackPage, A])"), src)
            assert(!src.contains("$chain"), src)
        }

        "a scalar field with an optional arg is a plain method with a default" in {
            val src = albumSrc
            assert(src.contains("def trackCount(mode: Maybe[String] = Absent): SelectionBuilder.Deferrable[Album, (trackCount: Int)]"), src)
        }

        "a no-arg field is a parameterless method" in {
            val src = albumSrc
            assert(src.contains("def name: SelectionBuilder.Deferrable[Album, (name: String)]"), src)
        }

        "generated selectors speak kyo types and import them (never Option/List/None/Nil)" in {
            val src = albumSrc
            assert(src.contains("import kyo.Absent"), src)
            assert(src.contains("import kyo.Chunk"), src)
            assert(src.contains("import kyo.Maybe"), src)
            assert(src.contains("selArgs: Chunk[SelectionBuilder.Arg]"), src)
            assert(src.contains("ScalarCodec.maybe(ScalarCodec.int).encode(first)"), src)
            assert(!src.contains("Option["), src)
            // `SelectionList[` is the library trait, not the stdlib collection.
            assert(!"(?<![A-Za-z])List\\[".r.findFirstIn(src).isDefined, src)
            assert(!src.contains("= None"), src)
            assert(!src.contains("Nil"), src)
        }
    }
end SelectorArgsSpec
