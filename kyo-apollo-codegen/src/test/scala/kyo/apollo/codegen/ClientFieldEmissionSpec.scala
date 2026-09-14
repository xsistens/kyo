package kyo.apollo.codegen

import kyo.*
import scala.io.Source
import scala.util.Using

/** Emission tests for local `@client` fields: the `ClientFields` descriptor object,
  * the chainable accessors on the selector objects, and the name-clash / unknown-type
  * validation. Assertions are on the emitted source text (the emitter's contract).
  */
class ClientFieldEmissionSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def resource(name: String): String =
        val stream = Option(getClass.getResourceAsStream(s"/codegenExample/$name"))
            .getOrElse(sys.error(s"fixture not found: $name"))
        Using.resource(Source.fromInputStream(stream, "UTF-8"))(_.mkString)
    end resource

    private val schema = SchemaLoader.fromString(resource("schema.graphql"))

    private val decls = List(
        ClientFieldDecl("Country", "isFavorite", "Boolean", "false"),
        ClientFieldDecl("Country", "tags", "Chunk[String]", "Chunk.empty"),
        ClientFieldDecl("Query", "cartOpen", "Boolean", "false")
    )
    private val cfg = CodegenConfig(packageName = "gen", clientFields = decls)

    private def sourceText(sources: List[GeneratedSource], file: String): String =
        sources.find(_.fileName == file).map(_.contents).getOrElse(sys.error(s"$file not emitted"))

    "client-field emission" - {

        "ClientFields object holds a `create` descriptor per field, grouped by type" in {
            val src = sourceText(ApolloClientWriter.writeClientFields(schema, cfg), "ClientFields.scala")
            assert(src.contains("object ClientFields:"), src)
            assert(src.contains("object Country:"), src)
            assert(
                src.contains(
                    """val isFavorite = ClientField.create[Country, Boolean]("isFavorite", default = false)"""
                ),
                src
            )
            assert(
                src.contains(
                    """val tags = ClientField.create[Country, Chunk[String]]("tags", default = Chunk.empty)"""
                ),
                src
            )
            // Global state on the Query root maps to the `RootQuery` origin.
            assert(
                src.contains(
                    """val cartOpen = ClientField.create[RootQuery, Boolean]("cartOpen", default = false)"""
                ),
                src
            )
        }

        "selector objects gain chainable accessors delegating to ClientFields" in {
            val country = sourceText(ApolloClientWriter.writeSelectors(schema, cfg), "Country.scala")
            assert(
                country.contains(
                    "def isFavorite: SelectionBuilder[Country, scala.NamedTuple.Concat[Acc, (isFavorite: Boolean)]]"
                ),
                country
            )
            assert(country.contains("sb.clientField(ClientFields.Country.isFavorite)"), country)
            assert(country.contains("import kyo.apollo.*"), country)
        }

        "no client fields → no ClientFields source and no client import" in {
            val plain = CodegenConfig(packageName = "gen")
            assert(ApolloClientWriter.writeClientFields(schema, plain).isEmpty)
        }

        "a client field clashing with a server field is a hard codegen error" in {
            val clash =
                cfg.copy(clientFields = List(ClientFieldDecl("Country", "name", "Boolean", "false")))
            val threw =
                try
                    ApolloClientWriter.writeClientFields(schema, clash); false
                catch case _: CodegenException => true
            assert(threw)
        }

        "a client field on an unknown type is a hard codegen error" in {
            val bad = cfg.copy(clientFields = List(ClientFieldDecl("Nope", "x", "Boolean", "false")))
            val threw =
                try
                    ApolloClientWriter.writeClientFields(schema, bad); false
                catch case _: CodegenException => true
            assert(threw)
        }
    }
end ClientFieldEmissionSpec
