package kyo.apollo.codegen

import java.nio.file.Files
import kyo.*
import scala.io.Source
import scala.util.Using

/** Tests the build-time driver [[CodegenRunner]] end to end over the
  * `codegenExample` schema: it must emit the shared schema types plus the inline
  * selector layer, and its on-disk [[CodegenRunner.run]] must write those files
  * (and be idempotent — an unchanged rerun rewrites nothing).
  */
class CodegenRunnerSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def resource(name: String): String =
        val path = s"/codegenExample/$name"
        val stream = Option(getClass.getResourceAsStream(path))
            .getOrElse(sys.error(s"fixture not found on classpath: $path"))
        Using.resource(Source.fromInputStream(stream, "UTF-8"))(_.mkString)
    end resource

    private val codegenConfig =
        CodegenConfig("kyo.apollo.generated", Map("DateTime" -> "java.time.Instant"))

    private def parsedSchema = SchemaLoader.fromString(resource("schema.graphql"))

    "CodegenRunner.generate / run / main" - {

        "generate emits shared schema types plus the inline selector layer" in {
            val sources   = CodegenRunner.generate(parsedSchema, codegenConfig)
            val fileNames = sources.map(_.fileName).toSet

            // One selector object per object type + the three operation roots.
            val selectorFiles =
                Set("Country.scala", "Queries.scala", "Mutations.scala", "Subscriptions.scala")
            // Shared schema types, emitted once for the whole run.
            val sharedFiles = Set(
                "Continent.scala",
                "CountryFilter.scala",
                "UpdateCountryInput.scala",
                "CustomScalars.scala"
            )

            selectorFiles.foreach(f => assert(fileNames.contains(f), s"missing selector source $f"))
            sharedFiles.foreach(f => assert(fileNames.contains(f), s"missing shared source $f"))

            // No source is emitted twice.
            val names = sources.map(_.fileName)
            assert(names.distinct.size == names.size, s"duplicate generated sources: $names")

            // Every source declares the configured package.
            sources.foreach(s =>
                assert(s.contents.startsWith("package kyo.apollo.generated"), s.fileName)
            )
        }

        "run writes the generated sources to disk" in {
            val schemaFile = Files.createTempFile("schema", ".graphql")
            Files.writeString(schemaFile, resource("schema.graphql"))
            val outDir = Files.createTempDirectory("apollo-codegen-out")

            val written = CodegenRunner.run(schemaFile, outDir, codegenConfig)

            assert(written.nonEmpty, "expected files to be written")
            written.foreach(p => assert(Files.exists(p), s"$p should exist"))
            assert(Files.exists(outDir.resolve("Country.scala")))
            assert(Files.exists(outDir.resolve("Queries.scala")))
            assert(Files.exists(outDir.resolve("Continent.scala")))
        }

        "run is idempotent — an unchanged rerun does not rewrite files" in {
            val schemaFile = Files.createTempFile("schema", ".graphql")
            Files.writeString(schemaFile, resource("schema.graphql"))
            val outDir = Files.createTempDirectory("apollo-codegen-idem")

            CodegenRunner.run(schemaFile, outDir, codegenConfig)
            val target        = outDir.resolve("Country.scala")
            val firstModified = Files.getLastModifiedTime(target)

            // Rerun: identical output must leave the existing file (and its mtime) untouched.
            CodegenRunner.run(schemaFile, outDir, codegenConfig)
            assert(Files.getLastModifiedTime(target) == firstModified)
        }

        "unknown leaf types fail generation loudly" in {
            val schema = SchemaLoader.fromString("type Query { mystery: Mystery }")
            val e      = intercept[CodegenException](CodegenRunner.generate(schema, codegenConfig))
            assert(e.getMessage.contains("Mystery"), e.getMessage)
            assert(e.getMessage.contains("Query.mystery"), e.getMessage)
        }

        "named options write the generated sources" in {
            val schemaFile = Files.createTempFile("schema", ".graphql")
            Files.writeString(schemaFile, resource("schema.graphql"))
            val outDir = Files.createTempDirectory("apollo-codegen-cli")
            CodegenRunner.main(Array(
                "--schema",
                schemaFile.toString,
                "--out",
                outDir.toString,
                "--package",
                "cli.gen",
                "--scalar",
                "DateTime=java.time.Instant"
            ))
            val country = Files.readString(outDir.resolve("Country.scala"))
            assert(country.startsWith("package cli.gen"), country)
            assert(country.contains("Maybe[java.time.Instant]"), country)
        }

        "a missing --schema is reported by name" in {
            val outDir = Files.createTempDirectory("apollo-codegen-cli-missing")
            val e = intercept[CodegenException](
                CodegenRunner.main(Array("--out", outDir.toString, "--package", "cli.gen"))
            )
            assert(e.getMessage.contains("--schema"), e.getMessage)
        }

        "repeatable options accumulate, and a stray argument is reported by name" in {
            val options = CodegenRunner.parseArgs(List(
                "--schema",
                "s.graphql",
                "--out",
                "out",
                "--package",
                "p",
                "--scalar",
                "DateTime=java.time.Instant",
                "--scalar",
                "URL=java.net.URI",
                "--client-field",
                "Country.isFavorite: Boolean = false",
                "--client-field",
                "Query.cartOpen: Boolean=false"
            ))
            assert(options.scalarMappings == Map("DateTime" -> "java.time.Instant", "URL" -> "java.net.URI"))
            assert(options.clientFields == List(
                ClientFieldDecl("Country", "isFavorite", "Boolean", "false"),
                ClientFieldDecl("Query", "cartOpen", "Boolean", "false")
            ))
            val positional = intercept[CodegenException](CodegenRunner.parseArgs(List("out", "p", "-", "s.graphql")))
            assert(positional == CodegenException.UnknownOption("out"))
            val repeated = intercept[CodegenException](
                CodegenRunner.parseArgs(List("--schema", "a", "--schema", "b", "--out", "o", "--package", "p"))
            )
            assert(repeated == CodegenException.RepeatedOption("--schema"))
            val dangling = intercept[CodegenException](CodegenRunner.parseArgs(List("--schema", "a", "--out")))
            assert(dangling == CodegenException.MissingValue("--out"))
            assert(intercept[CodegenException](CodegenRunner.parseScalarMapping("bogus")) ==
                CodegenException.MalformedScalarMapping("bogus"))
        }

        "a client field declaration reads like a Scala field" in {
            assert(ClientFieldDecl.parse("Country.tags: Chunk[String] = Chunk.empty") ==
                ClientFieldDecl("Country", "tags", "Chunk[String]", "Chunk.empty"))
            assert(ClientFieldDecl.parse("Query.onPick: String => Unit = _ => ()") ==
                ClientFieldDecl("Query", "onPick", "String => Unit", "_ => ()"))
            assert(intercept[CodegenException](ClientFieldDecl.parse("isFavorite: Boolean")) ==
                CodegenException.MalformedClientField("isFavorite: Boolean"))
        }

        "a rerun deletes the sources of types that left the schema, and nothing else" in {
            val schemaFile = Files.createTempFile("schema", ".graphql")
            val outDir     = Files.createTempDirectory("apollo-codegen-prune")
            Files.writeString(schemaFile, "enum Color { RED }\ntype Query { color: Color }")
            CodegenRunner.run(schemaFile, outDir, codegenConfig)
            assert(Files.exists(outDir.resolve("Color.scala")))
            val handWritten = outDir.resolve("Notes.scala")
            Files.writeString(handWritten, "object Notes")

            Files.writeString(schemaFile, "type Query { name: String }")
            CodegenRunner.run(schemaFile, outDir, codegenConfig)
            assert(!Files.exists(outDir.resolve("Color.scala")))
            assert(Files.exists(outDir.resolve("Queries.scala")))
            assert(Files.exists(handWritten))
        }
    }
end CodegenRunnerSpec
