package kyo.apollo.codegen

/** Guards the `streamed`-name reservation: the `.streamed` `@stream` marker is a
  * generic extension in the same namespace as the generated chain accessors, so a
  * schema field literally named `streamed` must NOT emit a bare `def streamed`
  * accessor (which would shadow the marker). The writer aliases it to `streamed$`
  * — collision-proof because `$` can never appear in a GraphQL name — while the
  * wire name stays `streamed`. Sibling of [[DeferReservationSpec]].
  */
class StreamReservationSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val schema = SchemaLoader.fromString(
        """type Query { thing: Thing }
      |type Thing { streamed: String name: String }
      |""".stripMargin
    )

    "codegen streamed-name reservation" - {

        "a field named `streamed` is emitted as `streamed$` (wire name unchanged)" in {
            val src = ApolloClientWriter
                .writeSelectors(schema, CodegenConfig(packageName = "test.gen"))
                .find(_.fileName == "Thing.scala")
                .getOrElse(fail("Thing.scala not emitted"))
                .contents

            // Scala accessor aliased to `streamed$`, and the result named-tuple field too.
            assert(src.contains("def streamed$"), src)
            assert(src.contains("(streamed$: Maybe[String])"), src)
            // The wire name stays `streamed` — only the Scala accessor is aliased.
            assert(src.contains("SelectionBuilder.scalar(\"streamed\""), src)
            // A sibling field is untouched.
            assert(src.contains("def name"), src)
        }
    }
end StreamReservationSpec
