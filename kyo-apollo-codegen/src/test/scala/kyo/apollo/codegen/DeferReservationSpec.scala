package kyo.apollo.codegen

/** Guards the `deferred`-name reservation: the `.deferred` `@defer` marker is a
  * generic extension in the same namespace as the generated chain accessors, so a
  * schema field literally named `deferred` must NOT emit a bare `def deferred`
  * accessor (which would shadow the marker). The writer aliases it to `deferred$`
  * — collision-proof because `$` can never appear in a GraphQL name — while the
  * wire name stays `deferred`.
  */
class DeferReservationSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val schema = SchemaLoader.fromString(
        """type Query { thing: Thing }
      |type Thing { deferred: String name: String }
      |""".stripMargin
    )

    "codegen deferred-name reservation" - {

        "a field named `deferred` is emitted as `deferred$` (wire name unchanged)" in {
            val src = ApolloClientWriter
                .writeSelectors(schema, CodegenConfig(packageName = "test.gen"))
                .find(_.fileName == "Thing.scala")
                .getOrElse(fail("Thing.scala not emitted"))
                .contents

            // Scala accessor aliased to `deferred$`, and the result named-tuple field too.
            assert(src.contains("def deferred$"), src)
            assert(src.contains("(deferred$: Maybe[String])"), src)
            // The wire name stays `deferred` — only the Scala accessor is aliased.
            assert(src.contains("SelectionBuilder.scalar(\"deferred\""), src)
            // A sibling field is untouched.
            assert(src.contains("def name"), src)
        }
    }
end DeferReservationSpec
