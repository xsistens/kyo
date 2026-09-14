package kyo.apollo.codegen

import kyo.*
import scala.io.Source
import scala.util.Using

/** Task 5 tests: scalar, enum, and input-object handling.
  *
  * Covers the three Task 5 deliverables against the `codegenExample` fixtures:
  *   - GraphQL enums → Scala 3 `enum`s with an `Adapter`;
  *   - custom scalars → configurable Scala types via `scalarMappings`, with a
  *     generated `CustomScalars` registry and `Adapter.forScalar` references;
  *   - input object types → case classes with an `ObjectAdapter`-based `Adapter`.
  *
  * Assertions are on the emitted source text (the emitter's contract), matching
  * [[ApolloClientWriterSpec]]'s style; compiling the generated code against `core`
  * is Task 8.
  */
class ScalarEnumInputSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def resource(name: String): String =
        val path = s"/codegenExample/$name"
        val stream = Option(getClass.getResourceAsStream(path))
            .getOrElse(sys.error(s"fixture not found on classpath: $path"))
        Using.resource(Source.fromInputStream(stream, "UTF-8"))(_.mkString)
    end resource

    private val schema = SchemaLoader.fromString(resource("schema.graphql"))

    private def schemaTypes(config: CodegenConfig): Map[String, String] =
        ApolloClientWriter.writeSchemaTypes(schema, config).map(s => s.fileName -> s.contents).toMap

    "scalar, enum, and input-object handling" - {

        // -- enums ------------------------------------------------------------------

        "enum: Continent → a Scala 3 enum with all values and a string-transform Schema" in {
            val src = schemaTypes(CodegenConfig(packageName = "kyo.apollo.example.generated"))
                .getOrElse("Continent.scala", fail("Continent.scala not emitted"))
            assert(src.contains("package kyo.apollo.example.generated"), src)
            assert(src.contains("enum Continent:"), src)
            assert(
                src.contains(
                    "case AFRICA, ANTARCTICA, ASIA, EUROPE, NORTH_AMERICA, OCEANIA, SOUTH_AMERICA"
                ),
                src
            )
            assert(src.contains("object Continent:"), src)
            // (de)serialised by GraphQL name via a string transform (NOT sum-type derivation).
            assert(src.contains("given Schema[Continent] ="), src)
            assert(
                src.contains("Schema.stringSchema.transform[Continent](Continent.valueOf)(_.toString)"),
                src
            )
        }

        // -- input objects ----------------------------------------------------------

        "input: UpdateCountryInput → a case class deriving Schema" in {
            val src = schemaTypes(CodegenConfig(packageName = "kyo.apollo.example.generated"))
                .getOrElse("UpdateCountryInput.scala", fail("UpdateCountryInput.scala not emitted"))
            // required ID → String, nullable fields → Maybe[String]; the codec is derived.
            assert(
                src.contains(
                    "final case class UpdateCountryInput(code: String, capital: Maybe[String], emoji: Maybe[String]) derives Schema"
                ),
                src
            )
            assert(!src.contains("ObjectAdapter"), src)
        }

        "input: CountryFilter references the generated enum type" in {
            val src = schemaTypes(CodegenConfig(packageName = "kyo.apollo.example.generated"))
                .getOrElse("CountryFilter.scala", fail("CountryFilter.scala not emitted"))
            assert(
                src.contains(
                    "final case class CountryFilter(continent: Maybe[Continent], search: Maybe[String]) derives Schema"
                ),
                src
            )
        }

        // -- custom scalars ---------------------------------------------------------

        "scalar: unmapped custom scalar falls back to String with no CustomScalars file" in {
            val types = schemaTypes(CodegenConfig(packageName = "kyo.apollo.example.generated"))
            assert(!types.contains("CustomScalars.scala"), types.keySet.toString)
        }

        "scalar: a mapped custom scalar drives the selector type, a given Schema, and the import" in {
            val config = CodegenConfig(
                packageName = "kyo.apollo.example.generated",
                scalarMappings = Map("DateTime" -> "java.time.Instant")
            )
            // The Country selector's `updatedAt` picks up the mapped Scala type, and the
            // file imports the scalar's given Schema.
            val opSrc = ApolloClientWriter
                .writeSelectors(schema, config)
                .find(_.fileName == "Country.scala")
                .getOrElse(fail("Country.scala not emitted"))
                .contents
            assert(
                opSrc.contains(
                    "updatedAt: SelectionBuilder.Deferrable[Country, (updatedAt: Maybe[java.time.Instant])]"
                ),
                opSrc
            )
            assert(opSrc.contains("import kyo.apollo.example.generated.CustomScalars.given"), opSrc)
            // A CustomScalars object is emitted with a given Schema per mapped scalar.
            val registry = schemaTypes(config)
                .getOrElse("CustomScalars.scala", fail("CustomScalars.scala not emitted"))
            assert(registry.contains("object CustomScalars:"), registry)
            assert(
                registry.contains(
                    "given Schema[java.time.Instant] = Schema.stringSchema.transform[java.time.Instant](java.time.Instant.parse)(_.toString)"
                ),
                registry
            )
        }

        "scalar: mapping to a builtin-schema type emits no CustomScalars given and no import" in {
            // e.g. Caliban's `Long` scalar mapped as `Long=Long`: the companion
            // `Schema.longSchema` given resolves at every `ScalarCodec.fromSchema[Long]`
            // call site, so no CustomScalars registry (and no import) is needed.
            val config = CodegenConfig(
                packageName = "kyo.apollo.example.generated",
                scalarMappings = Map("DateTime" -> "Long")
            )
            val opSrc = ApolloClientWriter
                .writeSelectors(schema, config)
                .find(_.fileName == "Country.scala")
                .getOrElse(fail("Country.scala not emitted"))
                .contents
            assert(
                opSrc.contains("updatedAt: SelectionBuilder.Deferrable[Country, (updatedAt: Maybe[Long])]"),
                opSrc
            )
            assert(opSrc.contains("ScalarCodec.fromSchema[Long]"), opSrc)
            assert(!opSrc.contains("CustomScalars.given"), opSrc)
            assert(!schemaTypes(config).contains("CustomScalars.scala"), schemaTypes(config).keySet.toString)
        }

        "scalar: mapping to a type with its own companion given emits no CustomScalars and no import" in {
            // A project opaque/Iron id (e.g. `civolution.common.ids.GameId`) carries its
            // own `given Schema` in its companion. Codegen has no recipe for it, so it is
            // treated like a builtin: the selector is typed to the FQN and its codec is
            // `ScalarCodec.fromSchema[FQN]` (resolving the companion given) — no registry,
            // no import.
            val config = CodegenConfig(
                packageName = "kyo.apollo.example.generated",
                scalarMappings = Map("DateTime" -> "com.example.ids.GameId")
            )
            val opSrc = ApolloClientWriter
                .writeSelectors(schema, config)
                .find(_.fileName == "Country.scala")
                .getOrElse(fail("Country.scala not emitted"))
                .contents
            assert(
                opSrc.contains("updatedAt: SelectionBuilder.Deferrable[Country, (updatedAt: Maybe[com.example.ids.GameId])]"),
                opSrc
            )
            assert(opSrc.contains("ScalarCodec.fromSchema[com.example.ids.GameId]"), opSrc)
            assert(!opSrc.contains("CustomScalars.given"), opSrc)
            assert(!schemaTypes(config).contains("CustomScalars.scala"), schemaTypes(config).keySet.toString)
        }
    }
end ScalarEnumInputSpec
