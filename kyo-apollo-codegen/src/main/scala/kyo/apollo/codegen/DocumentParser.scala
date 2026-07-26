package kyo.apollo.codegen

import caliban.parsing.Parser
import caliban.parsing.adt.Definition.ExecutableDefinition.FragmentDefinition
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Document
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.util.Using

/** Parses `.graphql` *operation* documents (queries / mutations / subscriptions)
  * into Caliban's operation AST.
  *
  * Phase 08 Task 4. This is the operation-document counterpart to
  * [[SchemaLoader]]: it turns executable `.graphql` files into the
  * [[caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition]]
  * (and [[FragmentDefinition]]) ASTs that [[ApolloClientWriter]] walks to derive
  * per-operation `kyo.apollo.api.Operation` sources. Uses the same
  * [[caliban.parsing.Parser]] as [[SchemaLoader]] — the reuse boundary ADR-001
  * committed to — so schema SDL and operation documents share one parser.
  */
object DocumentParser:

    /** Parse an in-memory operation document string into a [[Document]].
      *
      * @throws CodegenException if the document fails to parse.
      */
    def parse(source: String): Document =
        Parser.parseQuery(source) match
            case Right(document) => document
            case Left(error) =>
                throw CodegenException(s"Failed to parse GraphQL operation document: ${error.msg}")

    /** Load and parse a local `.graphql` operation-document file. */
    def parseFile(path: Path): Document =
        if !Files.isReadable(path) then
            throw CodegenException(s"Operation document not readable: $path")
        val source = Using.resource(scala.io.Source.fromFile(path.toFile, "UTF-8"))(_.mkString)
        parse(source)
    end parseFile

    /** Load and parse a local `.graphql` operation-document file by path string. */
    def parseFile(path: String): Document = parseFile(Paths.get(path))

    /** The executable operation definitions in `document`, in declaration order. */
    def operations(document: Document): List[OperationDefinition] =
        document.operationDefinitions

    /** The fragment definitions in `document`, keyed by fragment name. */
    def fragments(document: Document): Map[String, FragmentDefinition] =
        document.fragmentDefinitions.map(fragment => fragment.name -> fragment).toMap
end DocumentParser
