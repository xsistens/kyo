package kyo.apollo.codegen

import java.nio.file.Path
import scala.util.control.NoStackTrace

/** Every failure the code generator reports: a schema it cannot generate from, a
  * malformed command line, an invalid `@client` field declaration.
  *
  * The generator is a build tool, so each of these is a construction-time error
  * for the build author rather than a tracked runtime failure: generation stops
  * and the message names the offending schema coordinate, option or entry. Each
  * leaf carries its subject as typed fields and builds its message from them.
  *
  * This hierarchy stands apart from `kyo.apollo.exception.ApolloException`: the
  * generator deliberately does not depend on the client it emits code for, so it
  * cannot see that hierarchy. Stack traces are suppressed because the location
  * that matters is in the user's schema or build, never inside the generator.
  */
sealed abstract class CodegenException(message: String) extends RuntimeException(message) with NoStackTrace

object CodegenException:

    /** A field, argument or input field refers to a type the schema does not
      * declare. `coordinate` is the GraphQL schema coordinate of the reference:
      * `Type.field`, `Type.field(arg:)` or `Input.field`.
      */
    final case class UnknownType(typeName: String, coordinate: String)
        extends CodegenException(s"unknown type `$typeName` for field `$coordinate`")

    /** An argument or input field whose type is an object, interface or union type. */
    final case class NotAnInputType(typeName: String, coordinate: String)
        extends CodegenException(
            s"`$coordinate` has type `$typeName`, which is not an input type (a scalar, enum or input object)"
        )

    /** A field whose type is an input object type. */
    final case class NotAnOutputType(typeName: String, coordinate: String)
        extends CodegenException(s"field `$coordinate` has input object type `$typeName`, which a field cannot return")

    /** A required command-line option was not given. */
    final case class MissingOption(name: String)
        extends CodegenException(s"missing required option $name")

    /** An option was given as the last argument, without its value. */
    final case class MissingValue(name: String)
        extends CodegenException(s"option $name needs a value")

    /** An argument that is not one of the generator's options. */
    final case class UnknownOption(name: String)
        extends CodegenException(
            s"unknown option `$name`; expected --schema, --out, --package, --scalar or --client-field"
        )

    /** A single-valued option was given more than once. */
    final case class RepeatedOption(name: String)
        extends CodegenException(s"option $name was given more than once")

    /** A `--scalar` value that is not `Name=fully.qualified.Type`. */
    final case class MalformedScalarMapping(entry: String)
        extends CodegenException(s"malformed scalar mapping `$entry`; expected `Name=fully.qualified.Type`")

    /** A `--client-field` value that is not `Type.field: ScalaType = default`. */
    final case class MalformedClientField(entry: String)
        extends CodegenException(s"malformed client field `$entry`; expected `Type.field: ScalaType = default`")

    /** A `@client` field declared on a type that is neither an object type nor an operation root. */
    final case class ClientFieldOnUnknownType(onType: String, field: String)
        extends CodegenException(
            s"client field `$field` is declared on `$onType`, which is not a GraphQL object or operation-root type"
        )

    /** A `@client` field whose name is also a server field of its type; the normalized
      * cache would merge the two under one key.
      */
    final case class ClientFieldClash(onType: String, field: String)
        extends CodegenException(
            s"client field `$onType.$field` clashes with a server field of the same name; " +
                "client field names must be disjoint from schema fields on that type"
        )

    /** GraphQL text Caliban's parser rejects. `source` names what was parsed (a file
      * path, or `schema SDL` / `operation document` for in-memory text).
      */
    final case class ParseFailure(source: String, parserMessage: String)
        extends CodegenException(s"cannot parse $source: $parserMessage")

    /** A schema or document path that does not name a readable file. */
    final case class UnreadableFile(path: Path)
        extends CodegenException(s"file not readable: $path")

    /** Loading a schema by introspection from `url` failed; `failure` is the cause. */
    final case class IntrospectionFailure(url: String, failure: Throwable)
        extends CodegenException(s"cannot load the schema by introspection from $url: ${failure.getClass.getName}"):
        override def getCause: Throwable = failure
end CodegenException
