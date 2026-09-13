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

    "CodegenRunner.generate / run / parseScalarMappings" - {

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

        "parseScalarMappings parses entries and treats `-`/empty as none" in {
            assert(CodegenRunner.parseScalarMappings("-") == Map.empty[String, String])
            assert(CodegenRunner.parseScalarMappings("") == Map.empty[String, String])
            assert(
                CodegenRunner.parseScalarMappings("DateTime=java.time.Instant,URL=java.net.URI")
                    == Map("DateTime" -> "java.time.Instant", "URL" -> "java.net.URI")
            )
            val _ = intercept[CodegenException](CodegenRunner.parseScalarMappings("bogus"))
        }
    }
end CodegenRunnerSpec
