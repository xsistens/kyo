package kyo.apollo.codegen

import caliban.parsing.Parser
import caliban.parsing.adt.Document
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.util.Using

/** Loads a GraphQL *schema* (SDL) into Caliban's typed [[caliban.parsing.adt.Document]] AST.
  *
  * Phase 08 Task 4. Per ADR-001 ([[docs/architecture/codegen-strategy.md]] /
  * `[[Caliban-Codegen-Overview]]`) we reuse Caliban's *parse* side out of the box
  * and emit our own client. The two SDL-file entrypoints ([[fromString]] /
  * [[fromFile]]) call [[caliban.parsing.Parser]] directly — no ZIO, so schema
  * loading in tests and the sbt task is a plain, synchronous call. The
  * introspection entrypoint ([[fromIntrospectionUrl]]) delegates to
  * `caliban.tools.SchemaLoader`, which owns the sttp round-trip, and runs the
  * resulting ZIO effect on the default runtime.
  *
  * All three produce the SAME `Document` type the rest of codegen walks, so the
  * downstream emitter is agnostic to how the schema was obtained.
  */
object SchemaLoader:

    /** Parse an in-memory SDL string into a schema [[Document]].
      *
      * @throws CodegenException if the SDL fails to parse.
      */
    def fromString(sdl: String): Document = parse(sdl, "schema SDL")

    /** Load and parse a local `.graphql` SDL file into a schema [[Document]].
      *
      * @throws CodegenException if the file is unreadable or the SDL fails to parse.
      */
    def fromFile(path: Path): Document =
        if !Files.isReadable(path) then throw CodegenException.UnreadableFile(path)
        val sdl = Using.resource(scala.io.Source.fromFile(path.toFile, "UTF-8"))(_.mkString)
        parse(sdl, path.toString)
    end fromFile

    private def parse(sdl: String, source: String): Document =
        Parser.parseQuery(sdl) match
            case Right(document) => document
            case Left(error)     => throw CodegenException.ParseFailure(source, error.msg)

    /** Load and parse a local `.graphql` SDL file given as a path string. */
    def fromFile(path: String): Document = fromFile(Paths.get(path))

    /** Load a schema by GraphQL introspection against a running server.
      *
      * Delegates to `caliban.tools.SchemaLoader.fromIntrospection`, which issues the
      * introspection query over its own sttp backend, and runs the resulting ZIO
      * effect on the default runtime (this is a build-time tool, so blocking on the
      * effect is appropriate).
      *
      * @param url     the GraphQL endpoint to introspect
      * @param headers extra request headers (e.g. an auth token), by name
      * @throws CodegenException if introspection fails.
      */
    def fromIntrospectionUrl(
        url: String,
        headers: Map[String, String] = Map.empty
    ): Document =
        val calibanHeaders =
            if headers.isEmpty then None
            else Some(headers.toList.map((n, v) => caliban.tools.Header(n, v)))
        val loader = caliban.tools.SchemaLoader.fromIntrospection(url, calibanHeaders)
        try
            zio.Unsafe.unsafe { implicit unsafe =>
                zio.Runtime.default.unsafe.run(loader.load).getOrThrowFiberFailure()
            }
        catch
            case error: Throwable => throw CodegenException.IntrospectionFailure(url, error)
        end try
    end fromIntrospectionUrl
end SchemaLoader
