package kyo.apollo.codegen

import kyo.*
import scala.io.Source
import scala.util.Using

/** `SchemaIdentities` emission: the schema-wide enumeration of non-root object
  * types that an application's `given CacheIdentity[...]` declarations are
  * collected against at the client-build site.
  */
class SchemaIdentitiesSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def resource(name: String): String =
        val path = s"/codegenExample/$name"
        val stream = Option(getClass.getResourceAsStream(path))
            .getOrElse(sys.error(s"fixture not found on classpath: $path"))
        Using.resource(Source.fromInputStream(stream, "UTF-8"))(_.mkString)
    end resource

    private val schema = SchemaLoader.fromString(resource("schema.graphql"))

    private def identities(config: CodegenConfig): Map[String, String] =
        ApolloClientWriter.writeSchemaIdentities(schema, config).map(s => s.fileName -> s.contents).toMap

    "SchemaIdentities emission" - {

        "emits an inline generator over every non-root object type" in {
            val src = identities(CodegenConfig(packageName = "kyo.apollo.example.generated"))
                .getOrElse("SchemaIdentities.scala", fail("SchemaIdentities.scala not emitted"))
            assert(src.contains("package kyo.apollo.example.generated"), src)
            assert(src.contains("object SchemaIdentities:"), src)
            assert(src.contains("inline def generator: CacheKeyGenerator ="), src)
            assert(src.contains("CacheIdentity.generatorOf["), src)
            // The fixture schema's object types are enumerated; its operation roots are not.
            assert(src.contains("Country *:"), src)
            assert(src.contains("*: EmptyTuple"), src)
            assert(!src.contains("Query *:"), src)
        }

        "is part of the full generate() output" in {
            val all = CodegenRunner
                .generate(schema, CodegenConfig(packageName = "kyo.apollo.example.generated"))
                .map(_.fileName)
            assert(all.contains("SchemaIdentities.scala"), all.toString)
        }
    }
end SchemaIdentitiesSpec
