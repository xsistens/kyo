package kyo.apollo.codegen

import caliban.parsing.Parser
import kyo.*
import scala.io.Source
import scala.util.Using

/** Wiring smoke test for the Phase 08 `codegen` module (Task 3).
  *
  * It asserts the two things this module-creation task delivers:
  *
  *   1. The `caliban-tools` dependency (and, transitively, `caliban`'s parser)
  *      is on the classpath and USABLE — we actually parse an operation document
  *      through `caliban.parsing.Parser`, the reuse boundary ADR-001 committed to.
  *   2. The `codegenExample` schema + operation documents are packaged as
  *      resources and readable, so downstream tasks have fixtures to generate
  *      from.
  *
  * The real emitter behaviour is covered later by Task 8; this only proves the
  * scaffolding is sound.
  */
class CodegenModuleSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** Read a `codegenExample` fixture off the test classpath. */
    private def resource(name: String): String =
        val path = s"/codegenExample/$name"
        val stream = Option(getClass.getResourceAsStream(path))
            .getOrElse(sys.error(s"fixture not found on classpath: $path"))
        Using.resource(Source.fromInputStream(stream, "UTF-8"))(_.mkString)
    end resource

    "codegen module wiring" - {

        "codegenExample fixtures are packaged as resources" in {
            val fixtures = List(
                "schema.graphql",
                "getCountries.graphql",
                "getCountry.graphql",
                "updateCountry.graphql",
                "countryUpdated.graphql"
            )
            fixtures.foreach: name =>
                assert(resource(name).nonEmpty, s"$name should be non-empty")
        }

        "caliban's parser is on the classpath and parses an operation document" in {
            // Proves the caliban-tools dependency resolved and the reuse boundary works.
            val query = resource("getCountry.graphql")
            Parser.parseQuery(query) match
                case Right(doc)  => assert(doc.definitions.nonEmpty)
                case Left(error) => fail(s"expected getCountry.graphql to parse, got: $error")
        }

        "caliban's parser reports each operation kind in the fixtures" in {
            // One query, one mutation, one subscription must all parse cleanly.
            List("getCountries.graphql", "updateCountry.graphql", "countryUpdated.graphql")
                .foreach: name =>
                    assert(
                        Parser.parseQuery(resource(name)).isRight,
                        s"$name should parse without error"
                    )
        }
    }
end CodegenModuleSpec
