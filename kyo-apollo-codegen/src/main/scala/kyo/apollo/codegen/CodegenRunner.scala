package kyo.apollo.codegen

import caliban.parsing.adt.Document
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

/** Build-time driver that ties the codegen pieces together: it loads a schema
  * (via [[SchemaLoader]]) and one or more `.graphql` operation documents (via
  * [[DocumentParser]]), runs [[ApolloClientWriter]] over them, and writes the
  * emitted Scala sources to disk.
  *
  * This is the entry point the `codegenGenerate` sbt task (Phase 08 Task 6)
  * invokes. Because the `codegen` module is a separate build unit from the sbt
  * meta-build, the task cannot call [[ApolloClientWriter]] in-process; instead it
  * runs [[main]] through sbt's cross-module `runner` over the codegen classpath.
  * Arguments are passed positionally (see [[main]]).
  *
  * The pure [[generate]] helper (no filesystem access) is exposed so tests can
  * assert on the emitted source set directly, and [[run]] performs the load →
  * emit → write pipeline idempotently (a file is only rewritten when its bytes
  * change, so regeneration on every `example/compile` does not churn timestamps
  * and force needless recompiles).
  */
object CodegenRunner:

    /** Every generated source for `schema`: the shared schema types (enums, input
      * objects, and the custom-scalar registry), then the inline-query selector layer
      * (one selector object per schema object type plus the
      * `Queries`/`Mutations`/`Subscriptions` roots). Purely schema-driven — no
      * operation documents, no I/O.
      */
    def generate(schema: Document, config: CodegenConfig): List[GeneratedSource] =
        ApolloClientWriter.writeSchemaTypes(schema, config) ++
            ApolloClientWriter.writeSelectors(schema, config) ++
            ApolloClientWriter.writeClientFields(schema, config)

    /** Load the schema from disk, emit the sources, and write them into `outputDir`
      * (created if absent). Returns the paths written, in emission order. A source
      * whose on-disk bytes are already identical is left untouched.
      *
      * @throws CodegenException on any parse/emit/type-resolution failure.
      */
    def run(
        schemaPath: Path,
        outputDir: Path,
        config: CodegenConfig
    ): List[Path] =
        val schema  = SchemaLoader.fromFile(schemaPath)
        val sources = generate(schema, config)
        Files.createDirectories(outputDir)
        sources.map(src => writeIfChanged(outputDir.resolve(src.fileName), src.contents))
    end run

    /** Write `contents` to `target` only when it differs from what is already
      * there, so unchanged regenerations keep a stable modification time.
      */
    private def writeIfChanged(target: Path, contents: String): Path =
        val bytes = contents.getBytes("UTF-8")
        val unchanged =
            Files.exists(target) && java.util.Arrays.equals(Files.readAllBytes(target), bytes)
        if !unchanged then Files.write(target, bytes): Unit
        target
    end writeIfChanged

    /** sbt `runner` entry point. Positional arguments:
      *
      * {{{
      * args(0)  outputDir         directory to write generated sources into
      * args(1)  packageName       package the generated sources declare
      * args(2)  scalarMappings    comma-separated `Name=fully.qualified.Type`, or `-` for none
      * args(3)  schemaPath        GraphQL schema SDL file
      * args(4)  clientFieldsClass FQN of an object with a `fields: List[ClientFieldDecl]`
      *                            (local `@client` state), or `-`/omitted for none
      * }}}
      */
    def main(args: Array[String]): Unit =
        if args.length < 4 then
            throw CodegenException(
                "usage: CodegenRunner <outputDir> <packageName> <scalarMappings|-> " +
                    "<schema.graphql> [clientFieldsClass|-]"
            )
        end if
        val outputDir      = Paths.get(args(0))
        val packageName    = args(1)
        val scalarMappings = parseScalarMappings(args(2))
        val schemaPath     = Paths.get(args(3))
        val clientFields   = loadClientFields(if args.length > 4 then args(4) else "-")
        val written =
            run(schemaPath, outputDir, CodegenConfig(packageName, scalarMappings, clientFields))
        written.foreach(path => println(s"[apollo-codegen] wrote $path"))
    end main

    /** Reflectively read `fields: List[ClientFieldDecl]` off the object named `fqn`
      * (`-`/empty → none). The runner executes on the codegen classpath, so the
      * declarations object (and its `ClientFieldDecl` values) share this classloader
      * — the returned values need no marshalling.
      */
    private[codegen] def loadClientFields(fqn: String): List[ClientFieldDecl] =
        if fqn == "-" || fqn.trim.isEmpty then Nil
        else
            try
                val clazz  = Class.forName(fqn.trim + "$")
                val module = clazz.getField("MODULE$").get(null)
                clazz.getMethod("fields").invoke(module) match
                    case seq: Seq[?] => seq.collect { case d: ClientFieldDecl => d }.toList
                    case other =>
                        throw CodegenException(
                            s"$fqn.fields must be a List[ClientFieldDecl]; got ${other.getClass.getName}"
                        )
                end match
            catch
                case e: CodegenException => throw e
                case e: ReflectiveOperationException =>
                    throw CodegenException(
                        s"Could not load client-field declarations from `$fqn`: ${e.getMessage}. " +
                            "Expected an `object` with a `val fields: List[ClientFieldDecl]` on the codegen classpath."
                    )

    /** Parse the `Name=Type,Name2=Type2` scalar-mapping argument (`-`/empty → none). */
    private[codegen] def parseScalarMappings(raw: String): Map[String, String] =
        if raw == "-" || raw.trim.isEmpty then Map.empty
        else
            raw
                .split(",")
                .toList
                .map(_.trim)
                .filter(_.nonEmpty)
                .map { entry =>
                    entry.split("=", 2) match
                        case Array(name, tpe) if name.trim.nonEmpty && tpe.trim.nonEmpty =>
                            name.trim -> tpe.trim
                        case _ =>
                            throw CodegenException(
                                s"Malformed scalar mapping '$entry'; expected `Name=fully.qualified.Type`."
                            )
                }
                .toMap
end CodegenRunner
