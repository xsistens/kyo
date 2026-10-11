package kyo.apollo.codegen

/** A schema field named `deferred` or `streamed` is emitted verbatim. The names
  * used to be reserved (aliased to `deferred$`/`streamed$`) while selections were
  * chained: the chain accessors lived in the same extension namespace as the
  * `.deferred`/`.streamed` incremental-delivery markers, so a bare `def deferred`
  * shadowed the marker. A selection lambda now receives the selector OBJECT, whose
  * `deferred` is a plain member, and the markers stay extensions on the field
  * selection a member returns (`_.deferred.deferred` defers the field named
  * `deferred`) — no namespace is shared, so nothing needs aliasing.
  */
class MarkerNameSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val schema = SchemaLoader.fromString(
        """type Query { thing: Thing }
      |type Thing { deferred: String streamed: [String!]! name: String }
      |""".stripMargin
    )

    "codegen marker names" - {

        "fields named `deferred` and `streamed` are emitted under their own names" in {
            val src = ApolloClientWriter
                .writeSelectors(schema, CodegenConfig(packageName = "test.gen"))
                .find(_.fileName == "Thing.scala")
                .getOrElse(fail("Thing.scala not emitted"))
                .contents

            assert(src.contains("def deferred: SelectionBuilder.Deferrable[Thing, (deferred: Maybe[String])]"), src)
            assert(src.contains("def streamed: SelectionBuilder.Deferrable[Thing, (streamed: Chunk[String])]"), src)
            assert(!src.contains("deferred$"), src)
            assert(!src.contains("streamed$"), src)
            assert(src.contains("def name"), src)
        }
    }
end MarkerNameSpec
