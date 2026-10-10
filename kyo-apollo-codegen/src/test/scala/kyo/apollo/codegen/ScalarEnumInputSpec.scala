package kyo.apollo.codegen

import kyo.*
import scala.io.Source
import scala.util.Using

/** Scalar, enum, and input-object handling, against the `codegenExample` fixtures:
  *   - GraphQL enums → Scala 3 `enum`s with a string-transform `Schema`, and an
  *     `Unknown__` case for a value the schema does not know;
  *   - custom scalars → configurable Scala types via `scalarMappings`, with a
  *     generated `CustomScalars` object of `given Schema`s where one is needed;
  *   - input object types → case classes deriving `Schema`.
  *
  * Assertions are on the emitted source text (the emitter's contract), matching
  * [[ApolloClientWriterSpec]]'s style; `kyo-apollo-codegen-it` compiles the
  * generated code against kyo-apollo.
  */
class ScalarEnumInputSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def resource(name: String): String =
        val path   = s"/codegenExample/$name"
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
            assert(src.contains("enum Continent derives CanEqual:"), src)
            assert(
                src.contains(
                    "case AFRICA, ANTARCTICA, ASIA, EUROPE, NORTH_AMERICA, OCEANIA, SOUTH_AMERICA"
                ),
                src
            )
            assert(src.contains("object Continent:"), src)
            // (de)serialised by GraphQL name via a string transform (NOT sum-type derivation).
            assert(src.contains("given Schema[Continent] ="), src)
            assert(src.contains("Schema.stringSchema.transform[Continent]"), src)
            assert(!src.contains("transform[Continent](Continent.valueOf)"), src)
        }

        "enum: a value the schema does not know decodes to Unknown__ and encodes as its name" in {
            val src = schemaTypes(CodegenConfig(packageName = "kyo.apollo.example.generated"))
                .getOrElse("Continent.scala", fail("Continent.scala not emitted"))
            assert(src.contains("case Unknown__(raw: String)"), src)
            assert(
                src.contains(
                    "val values: Chunk[Continent] = Chunk(AFRICA, ANTARCTICA, ASIA, EUROPE, NORTH_AMERICA, OCEANIA, SOUTH_AMERICA)"
                ),
                src
            )
            assert(src.contains("def valueOf(name: String): Maybe[Continent]"), src)
            assert(src.contains("valueOf(raw).getOrElse(Unknown__(raw))"), src)
            assert(src.contains("case Unknown__(raw) => raw"), src)
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
            // A project opaque/Iron id (e.g. `com.example.ids.GameId`) carries its
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

        // -- the Upload scalar --------------------------------------------------------

        val uploadSchema = SchemaLoader.fromString(
            """scalar Upload
              |type Country { code: ID! }
              |type Query { country(code: ID!): Country }
              |type Mutation {
              |  uploadFlag(code: ID!, file: Upload!): Country!
              |  uploadFlags(files: [Upload!]!): Country!
              |}
              |""".stripMargin
        )

        def mutations(config: CodegenConfig)(using kyo.test.AssertScope): String =
            ApolloClientWriter.writeSelectors(uploadSchema, config)
                .find(_.contents.contains("uploadFlag"))
                .getOrElse(fail("no source with the uploadFlag selector"))
                .contents

        "scalar: an unmapped Upload is kyo.apollo.Upload sent through ScalarCodec.upload" in {
            val src = mutations(CodegenConfig(packageName = "kyo.apollo.example.generated"))
            assert(src.contains("file: kyo.apollo.Upload"), src)
            assert(src.contains("ScalarCodec.upload.encode(file)"), src)
            assert(src.contains("files: Chunk[kyo.apollo.Upload]"), src)
            assert(src.contains("ScalarCodec.chunk(ScalarCodec.upload)"), src)
        }

        "scalar: an explicit mapping of Upload takes precedence" in {
            val src = mutations(CodegenConfig(
                packageName = "kyo.apollo.example.generated",
                scalarMappings = Map("Upload" -> "com.example.File")
            ))
            assert(src.contains("file: com.example.File"), src)
            assert(src.contains("ScalarCodec.fromSchema[com.example.File]"), src)
            assert(!src.contains("ScalarCodec.upload"), src)
        }

        "scalar: an Upload field inside an input object stops generation, naming the field" in {
            val withInput = SchemaLoader.fromString(
                """scalar Upload
                  |input FlagInput { code: ID!, file: Upload! }
                  |type Country { code: ID! }
                  |type Query { country(code: ID!): Country }
                  |type Mutation { setFlag(input: FlagInput!): Country! }
                  |""".stripMargin
            )
            scala.util.Try(ApolloClientWriter.writeSchemaTypes(withInput, CodegenConfig(packageName = "p"))) match
                case scala.util.Failure(e: CodegenException.UploadInInputObject) =>
                    assert(e.input == "FlagInput" && e.field == "file", e.getMessage)
                case other => fail(s"expected UploadInInputObject, got $other")
            end match
        }
    }
end ScalarEnumInputSpec
