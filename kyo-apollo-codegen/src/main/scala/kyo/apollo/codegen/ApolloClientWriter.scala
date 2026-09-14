package kyo.apollo.codegen

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.EnumTypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.InputObjectTypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.UnionTypeDefinition
import caliban.parsing.adt.Document
import caliban.parsing.adt.Type

/** Raised for any recoverable failure inside the codegen pipeline (parse errors,
  * an unresolvable schema type, an unsupported document feature, …). Keeping one
  * exception type lets the sbt task (Task 6) report codegen failures uniformly.
  */
final class CodegenException(message: String) extends RuntimeException(message)

object CodegenException:
    def apply(message: String): CodegenException = new CodegenException(message)

/** Configuration for a codegen run.
  *
  * @param packageName    the package the generated operation sources are emitted into
  * @param scalarMappings GraphQL custom-scalar name → fully-qualified Scala type
  *                       (e.g. `"DateTime" -> "java.time.Instant"`). A mapped
  *                       custom scalar's fields/variables are typed as that Scala
  *                       type and (de)serialised through a compile-time
  *                       `given Schema[ScalaType]`. Where codegen has a built-in
  *                       recipe for the type (see `schemaRecipe`), that given is
  *                       emitted into the generated `CustomScalars` object and
  *                       referencing sources import it. Otherwise the type is
  *                       assumed to ship its own companion `given Schema` — kyo
  *                       builtins (`Long`) or a project's own opaque/Iron id whose
  *                       companion carries the given — and codegen emits neither a
  *                       `CustomScalars` entry nor an import; `ScalarCodec.fromSchema`
  *                       resolves the companion given (a compile error if none
  *                       exists). An unmapped custom scalar falls back to `String`
  *                       (raw wire passthrough, no registration required).
  */
final case class CodegenConfig(
    packageName: String = "kyo.apollo.generated",
    scalarMappings: Map[String, String] = Map.empty,
    clientFields: List[ClientFieldDecl] = Nil
)

/** One generated Scala source file. */
final case class GeneratedSource(
    /** File name, e.g. `GetCountry.scala`. */
    fileName: String,
    /** Package the source declares. */
    packageName: String,
    /** The full source text. */
    contents: String
)

/** The schema-driven emitter: turns a parsed GraphQL SDL into typed Scala sources
  * targeting THIS project's `core` API (option (b) of ADR-001 — a custom writer,
  * NOT a bridge over `caliban.client`).
  *
  * There are two entry points, both schema-wide and document-independent:
  *
  *   - [[writeSchemaTypes]] emits the shared schema types once per run: each
  *     GraphQL `enum` → a Scala 3 `enum` + a string-transform `given Schema`; each
  *     input object → a case class `derives Schema`; and, when custom scalars are
  *     mapped, a `CustomScalars` object holding a `given Schema` per mapped scalar.
  *   - [[writeSelectors]] emits the inline-query selector layer: a phantom `Origin`
  *     marker + a selector `object` per GraphQL object type, and the
  *     `Queries`/`Mutations`/`Subscriptions` roots. Each field becomes a
  *     `SelectionBuilder`-returning method, so a query is written as ordinary Scala
  *     with a named-tuple result — no per-operation codegen.
  */
object ApolloClientWriter:

    private val BuiltInScalars: Map[String, String] =
        Map(
            "Int"     -> "Int",
            "Float"   -> "Double",
            "String"  -> "String",
            "Boolean" -> "Boolean",
            "ID"      -> "String"
        )

    private val ReservedWords: Set[String] = Set(
        "type",
        "val",
        "var",
        "def",
        "class",
        "object",
        "trait",
        "given",
        "match",
        "case",
        "if",
        "else",
        "then",
        "for",
        "yield",
        "new",
        "with",
        "extends",
        "import",
        "package",
        "return",
        "throw",
        "try",
        "catch",
        "finally",
        "while",
        "do",
        "lazy",
        "implicit",
        "override",
        "final",
        "sealed",
        "abstract",
        "private",
        "protected",
        "this",
        "super",
        "null",
        "true",
        "false",
        "enum",
        "export"
    )

    /** Emit the shared schema types referenced by generated operations, once per
      * run (Task 5): a Scala 3 `enum` + string-transform `given Schema` per GraphQL
      * enum, a case class `derives Schema` per input object, and — when any custom
      * scalar is mapped — a `CustomScalars` object of `given Schema`s. These are
      * schema-wide (independent of which operations use them), so they are emitted
      * here rather than per document to avoid duplicate definitions across operation
      * files.
      */
    def writeSchemaTypes(schema: Document, config: CodegenConfig): List[GeneratedSource] =
        new Emitter(schema, config).emitSchemaTypes

    /** Emit the inline-query selector layer for the whole schema (Phase 3): a
      * phantom `Origin` marker + a selector `object` per GraphQL object type, and
      * `Queries`/`Mutations`/`Subscriptions` root objects. Each object field becomes
      * a `SelectionBuilder`-returning selector method, so a query is written as
      * ordinary Scala (`Queries.country(code)(Country.name ~ Country.capital)`) with
      * full autocomplete and a named-tuple result — no per-query codegen. Schema-wide
      * and document-independent, like [[writeSchemaTypes]].
      */
    def writeSelectors(schema: Document, config: CodegenConfig): List[GeneratedSource] =
        new Emitter(schema, config).emitSelectors

    /** Emit the `ClientFields` object of local `@client` field descriptors declared
      * in [[CodegenConfig.clientFields]] (empty when none). The matching chainable
      * accessors are emitted by [[writeSelectors]] into each selector object.
      */
    def writeClientFields(schema: Document, config: CodegenConfig): List[GeneratedSource] =
        new Emitter(schema, config).emitClientFields

    /** Emit the `SchemaIdentities` object: the enumeration of every non-root schema
      * object type, folded into an `inline def generator` that collects the
      * application's `given CacheIdentity[...]` declarations at the CALL site
      * (where those givens are visible) and yields the client's key generator. The
      * schema knows the types; only the application knows which of them have
      * identities — the inline expansion is the seam joining the two without a
      * single string.
      */
    def writeSchemaIdentities(schema: Document, config: CodegenConfig): List[GeneratedSource] =
        new Emitter(schema, config).emitSchemaIdentities

    // -- internal, per-run state ------------------------------------------------

    final private class Emitter(schema: Document, config: CodegenConfig):

        private val objectNames: Set[String] = schema.objectTypeDefinitions.map(_.name).toSet
        private val enumNames: Set[String]   = schema.enumTypeDefinitions.map(_.name).toSet
        private val inputNames: Set[String]  = schema.inputObjectTypeDefinitions.map(_.name).toSet
        private val unionMembers: Map[String, List[String]] =
            schema.unionTypeDefinitions.map(u => u.name -> u.memberTypes).toMap

        /** A leaf that takes a nested selection: an object type or a union (whose
          * selection is composed of `on<Member>` inline-fragment branches).
          */
        private def isComposite(leafName: String): Boolean =
            objectNames(leafName) || unionMembers.contains(leafName)
        private val customScalarNames: Set[String] =
            schema.scalarTypeDefinitions.map(_.name).toSet.diff(BuiltInScalars.keySet)

        /** The `given Schema` body codegen synthesizes for a mapped custom-scalar Scala
          * type, or `None` when it has no built-in recipe. `None` is the "external
          * given" case: the mapped type is assumed to ship its own `given Schema` in its
          * companion — a kyo builtin (`Schema.longSchema`, …) or a project's own
          * opaque/Iron id whose companion carries the given. For those, codegen emits
          * neither a `CustomScalars` entry nor an import; `ScalarCodec.fromSchema[T]`
          * resolves the companion given directly. Add a case here to teach codegen a new
          * recipe.
          */
        private def schemaRecipe(scalaType: String): Option[String] = scalaType match
            case "java.time.Instant" =>
                Some("Schema.stringSchema.transform[java.time.Instant](java.time.Instant.parse)(_.toString)")
            case _ => None

        /** The Scala types of every mapped custom scalar for which codegen synthesizes a
          * `CustomScalars` given (used to decide when a generated source must import the
          * `CustomScalars` givens). External-given targets — kyo builtins and project
          * opaque/Iron ids, which resolve their companion given — are excluded.
          */
        private val mappedScalarTypes: Set[String] =
            customScalarNames
                .filter(config.scalarMappings.contains)
                .map(config.scalarMappings)
                .filter(t => schemaRecipe(t).isDefined)

        /** A field of a generated data / input case class. */
        final private case class DataField(
            scalaName: String,
            responseName: String,
            scalaType: String
        )

        /** Build a [[DataField]] for a selected/input field of GraphQL type `gqlType`. */
        private def dataField(responseName: String, gqlType: Type): DataField =
            DataField(
                scalaName = sanitize(responseName),
                responseName = responseName,
                scalaType = scalaTypeOf(gqlType)
            )

        // -- schema-type emission (Task 5) ----------------------------------------

        /** All shared schema types: enums, input objects, and the custom-scalar
          * `given Schema`s (when any custom scalar is mapped).
          */
        def emitSchemaTypes: List[GeneratedSource] =
            schema.enumTypeDefinitions.map(emitEnum) ++
                schema.inputObjectTypeDefinitions.map(emitInput) ++
                emitCustomScalars.toList

        /** A GraphQL enum → a Scala 3 `enum` plus a `given Schema` that (de)serialises
          * it by its GraphQL name (a JSON string). NOT a sum-type `derives Schema`:
          * that emits a tagged union and breaks the Scala.js linker at kyo RC5.
          */
        private def emitEnum(e: EnumTypeDefinition): GeneratedSource =
            val cases = e.enumValuesDefinition.map(v => sanitize(v.enumValue)).mkString(", ")
            val contents =
                s"""package ${config.packageName}
           |
           |import kyo.Schema
           |
           |// GENERATED by apollo-codegen from enum `${e.name}`. DO NOT EDIT.
           |enum ${e.name}:
           |  case $cases
           |
           |object ${e.name}:
           |  /** GraphQL enums are bare-name strings on the wire — a string transform,
           |    * not sum-type derivation (which breaks the Scala.js linker at kyo RC5). */
           |  given Schema[${e.name}] =
           |    Schema.stringSchema.transform[${e.name}](${e.name}.valueOf)(_.toString)
           |""".stripMargin
            GeneratedSource(s"${e.name}.scala", config.packageName, contents)
        end emitEnum

        /** A GraphQL input object → a case class `derives Schema` (its codec is
          * derived; nested enum/scalar codecs resolve from their companion / the
          * imported `CustomScalars` givens).
          */
        private def emitInput(in: InputObjectTypeDefinition): GeneratedSource =
            val fields       = in.fields.map(f => dataField(f.name, f.ofType))
            val params       = fields.map(f => s"${f.scalaName}: ${f.scalaType}").mkString(", ")
            val scalarImport = customScalarImport(fields.map(_.scalaType))
            val kyoImports   = kyoTypeImports(params)
            val contents =
                s"""package ${config.packageName}
           |
           |import kyo.Schema$kyoImports$scalarImport
           |
           |// GENERATED by apollo-codegen from input object `${in.name}`. DO NOT EDIT.
           |final case class ${in.name}($params) derives Schema
           |""".stripMargin
            GeneratedSource(s"${in.name}.scala", config.packageName, contents)
        end emitInput

        /** A `CustomScalars` object holding a `given Schema` per mapped custom scalar
          * that codegen has a recipe for — a string transform over `Schema.stringSchema`.
          * Generated sources that reference such a scalar `import <pkg>.CustomScalars.given`.
          * Mapped types with an external companion given (kyo builtins, project opaque/Iron
          * ids) are excluded — they need neither an entry nor an import. Emitted only when
          * at least one mapped scalar has a recipe.
          */
        private def emitCustomScalars: Option[GeneratedSource] =
            val mapped = customScalarNames
                .filter(config.scalarMappings.contains)
                .filter(name => schemaRecipe(config.scalarMappings(name)).isDefined)
                .toList
                .sorted
            if mapped.isEmpty then None
            else
                val givens = mapped
                    .map { name =>
                        val scalaType = config.scalarMappings(name)
                        s"given Schema[$scalaType] = ${schemaRecipe(scalaType).get}"
                    }
                    .mkString("\n")
                val contents =
                    s"""package ${config.packageName}
             |
             |import kyo.Schema
             |
             |// GENERATED by apollo-codegen. DO NOT EDIT.
             |object CustomScalars:
             |${indent(givens, 2)}
             |""".stripMargin
                Some(GeneratedSource("CustomScalars.scala", config.packageName, contents))
            end if
        end emitCustomScalars

        /** An `import <pkg>.CustomScalars.given` line (leading newline) when any of
          * `scalaTypes` is a mapped custom scalar, so the file's `derives Schema` can
          * resolve the scalar codec; empty otherwise.
          */
        private def customScalarImport(scalaTypes: Iterable[String]): String =
            val used = mappedScalarTypes.exists(t => scalaTypes.exists(_.contains(t)))
            if used then s"\nimport ${config.packageName}.CustomScalars.given" else ""

        // -- selector emission (Phase 3) ------------------------------------------

        /** A field of an object type, reduced to what selector emission needs. */
        final private case class ArgSpec(name: String, ofType: Type)
        final private case class FieldSpec(name: String, ofType: Type, args: List[ArgSpec])

        /** Root object type name → (phantom `Origin` marker, selector object name). */
        private def rootSelectorMap: Map[String, (String, String)] =
            val q = schema.schemaDefinition.flatMap(_.query).getOrElse("Query")
            val m = schema.schemaDefinition.flatMap(_.mutation).getOrElse("Mutation")
            val s = schema.schemaDefinition.flatMap(_.subscription).getOrElse("Subscription")
            Map(
                q -> ("RootQuery", "Queries"),
                m -> ("RootMutation", "Mutations"),
                s -> ("RootSubscription", "Subscriptions")
            )
        end rootSelectorMap

        /** All selector sources: one selector object per non-root object type, plus the
          * `Queries`/`Mutations`/`Subscriptions` roots.
          */
        def emitSelectors: List[GeneratedSource] =
            validateClientFields()
            val roots = rootSelectorMap
            schema.objectTypeDefinitions.map { obj =>
                val fields = obj.fields.map(f =>
                    FieldSpec(f.name, f.ofType, f.args.map(a => ArgSpec(a.name, a.ofType)))
                )
                roots.get(obj.name) match
                    // Roots use the universal `RootQuery`/… markers from `core` (imported via
                    // the wildcard), so no per-schema phantom trait is emitted for them.
                    case Some((origin, objectName)) =>
                        renderSelectorObject(objectName, origin, obj.name, fields, emitTrait = false)
                    case None => renderSelectorObject(obj.name, obj.name, obj.name, fields, emitTrait = true)
                end match
            } ++ schema.unionTypeDefinitions.map(renderUnionObject)
        end emitSelectors

        /** Render the selector object of a GraphQL union: its own phantom `Origin`
          * marker plus one `on<Member>` inline-fragment branch per member type. A
          * branch decodes to `Maybe[A]` (`Present` iff the object's `__typename` is
          * that member), and branches compose with `~`/chaining like fields, so a
          * union selection reads `PlayableItem.onTrack(_.name) ~
          * PlayableItem.onEpisode(_.name)` or, chained inside a parent lambda,
          * `_.item(_.onTrack(_.name).onEpisode(_.name))`. Both the value form and
          * the lambda form are plain overloads — no `$sel` indirection, because
          * branches take no arguments (so no default-argument collision exists).
          */
        private def renderUnionObject(u: UnionTypeDefinition): GeneratedSource =
            val name = u.name
            val branches = u.memberTypes.map { member =>
                val label = s"on$member"
                s"""def $label[A](sel: SelectionBuilder[$member, A]): SelectionBuilder.Fields[$name, ($label: Maybe[A])] =
           |  SelectionBuilder.onType("$member", sel)
           |
           |def $label[A](build: SelectionBuilder.Fields[$member, scala.NamedTuple.Empty] => SelectionBuilder[$member, A]): SelectionBuilder.Fields[$name, ($label: Maybe[A])] =
           |  SelectionBuilder.onType("$member", build(SelectionBuilder.empty))""".stripMargin
            }.mkString("\n\n")
            val chainAccessors = u.memberTypes.map { member =>
                val label = s"on$member"
                s"""def $label[B](sub: SelectionBuilder.Fields[$member, scala.NamedTuple.Empty] => SelectionBuilder[$member, B]): SelectionBuilder.Fields[$name, scala.NamedTuple.Concat[Acc, ($label: Maybe[B])]] =
           |  sb ~ $name.$label(sub(SelectionBuilder.empty))""".stripMargin
            }.mkString("\n")
            val chainBlock =
                s"""extension [Acc <: scala.NamedTuple.AnyNamedTuple](sb: SelectionBuilder.Fields[$name, Acc])
           |${indent(chainAccessors, 2)}""".stripMargin
            val body = s"given TypeName[$name] = TypeName(\"$name\")\n\n$branches\n\n$chainBlock"
            val contents =
                s"""package ${config.packageName}
           |
           |import kyo.Maybe
           |import kyo.apollo.api.*
           |
           |// GENERATED by apollo-codegen: union selectors for `$name`. DO NOT EDIT.
           |sealed trait $name
           |
           |object $name:
           |${indent(body, 2)}
           |""".stripMargin
            GeneratedSource(s"$name.scala", config.packageName, contents)
        end renderUnionObject

        /** The `SchemaIdentities` source, or nothing when the schema has no non-root
          * object types (there would be no identities to collect).
          */
        def emitSchemaIdentities: List[GeneratedSource] =
            val roots = rootSelectorMap
            val types = schema.objectTypeDefinitions.map(_.name).filterNot(roots.contains)
            if types.isEmpty then Nil
            else
                // `A *: B *: EmptyTuple` rather than `(A, B)` so the one-type case
                // renders as a valid tuple type too.
                val tuple = types.mkString("", " *: ", " *: EmptyTuple")
                val contents =
                    s"""package ${config.packageName}
               |
               |import kyo.apollo.cache.normalized.api.CacheIdentity
               |import kyo.apollo.cache.normalized.api.CacheKeyGenerator
               |
               |// GENERATED by apollo-codegen: schema-wide CacheIdentity collector. DO NOT EDIT.
               |object SchemaIdentities:
               |
               |  /** The client's key generator, assembled from the `given CacheIdentity[...]`
               |    * declarations visible at the call site — import them before calling:
               |    *
               |    * {{{
               |    * import CacheIdentities.given
               |    * Apollo.client(_.serverUrl(url).normalizedCache(MemoryCache(), SchemaIdentities.generator))
               |    * }}}
               |    *
               |    * Types without a declared identity fall back to default id-based keying.
               |    */
               |  inline def generator: CacheKeyGenerator =
               |    CacheIdentity.generatorOf[$tuple]
               |""".stripMargin
                List(GeneratedSource("SchemaIdentities.scala", config.packageName, contents))
            end if
        end emitSchemaIdentities

        /** Render an `object <objectName>` of selector methods rooted in `originName`,
          * optionally preceded by its own `sealed trait <originName>` phantom marker
          * (object types own theirs; the operation roots share `core`'s).
          */
        private def renderSelectorObject(
            objectName: String,
            originName: String,
            gqlType: String,
            fields: List[FieldSpec],
            emitTrait: Boolean
        ): GeneratedSource =
            val methods = fields.map(selectorMethod(originName, _)).mkString("\n\n")
            // Non-root object types (those owning a phantom trait) also get a `select`
            // entry point + one chainable accessor per field, so `Country.select.code`
            // and the lambda `_.code` read fields without repeating the type name.
            // They also carry a `given TypeName[Origin]` (in the trait's companion, so
            // it is auto-summoned) that names the type at runtime for `ClientField`
            // (local `@client` state); the operation roots get theirs from `core`.
            val typeNameGiven =
                if emitTrait then s"given TypeName[$originName] = TypeName(\"$originName\")\n\n" else ""
            // Client `@client` accessors (`_.code.isFavorite`) as a separate extension
            // block, appended for both object types and the operation roots.
            val clientBlock = clientAccessorBlock(originName, gqlType).map("\n\n" + _).getOrElse("")
            val serverBody =
                if emitTrait then s"$typeNameGiven$methods\n\n${chainingBlock(originName, fields)}"
                else methods
            val objectBody = s"$serverBody$clientBlock"
            val scalaTypes =
                fields.flatMap(f => scalaTypeOf(f.ofType) :: f.args.map(a => scalaTypeOf(a.ofType)))
            val scalarImport = customScalarImport(scalaTypes)
            // Client accessors call `sb.clientField(...)` and reference `ClientFields`,
            // both reachable through `kyo.apollo.*` (the extension lives in `core`).
            val clientImport = if clientFieldsFor(gqlType).nonEmpty then "\nimport kyo.apollo.*" else ""
            val traitDecl    = if emitTrait then s"sealed trait $originName\n\n" else ""
            // `Maybe`/`Chunk`/`Absent` for nullable/list fields and argument defaults.
            val kyoImports = kyoTypeImports(objectBody)
            val contents =
                s"""package ${config.packageName}
           |
           |import kyo.apollo.api.*$kyoImports$scalarImport$clientImport
           |
           |// GENERATED by apollo-codegen: schema selectors for `$objectName`. DO NOT EDIT.
           |${traitDecl}object $objectName:
           |${indent(objectBody, 2)}
           |""".stripMargin
            GeneratedSource(s"$objectName.scala", config.packageName, contents)
        end renderSelectorObject

        /** A single selector method: a scalar leaf field becomes a `def` returning a
          * `SelectionBuilder.Deferrable`; an object field takes a nested selection and
          * wraps its result named tuple per the field's list/nullable structure. Both
          * are single fields, so `.deferred`/`.streamed` apply to them.
          */
        private def selectorMethod(originName: String, field: FieldSpec): String =
            val label    = accessorLabel(field.name)
            val leafName = Type.innerType(field.ofType)
            val argParams = field.args.map { a =>
                val default = if isNonNull(a.ofType) then "" else " = Absent"
                s"${sanitize(a.name)}: ${scalaTypeOf(a.ofType)}$default"
            }
            val argList =
                if field.args.isEmpty then "Chunk.empty"
                else
                    val entries = field.args.map { a =>
                        s"""SelectionBuilder.Arg("${a.name}", ${compiledTypeExpr(a.ofType)}, ${scalarCodecExpr(
                                a.ofType
                            )}.encode(${sanitize(a.name)}))"""
                    }
                    s"Chunk(${entries.mkString(", ")})"

            if isComposite(leafName) then
                // `A` is deliberately unbounded so a `map`/`mapInto`-projected child
                // selection (whose result is not a named tuple) still nests here.
                val argClause = if field.args.isEmpty then "" else s"(${argParams.mkString(", ")})"
                val returnType =
                    s"SelectionBuilder.Deferrable[$originName, ($label: ${wrappedTypeExpr(field.ofType, "A")})]"
                val compiled = compiledTypeExpr(field.ofType)
                val nesting  = nestingExpr(field.ofType)
                // Scala forbids two overloads that BOTH declare default arguments, so the
                // field's optional args cannot live on an overloaded method. Instead the
                // args live on a single, non-overloaded selector method that returns a
                // per-field selector object; the two ways to pass a selection are then
                // overloads of THAT object's `apply` (which carry no args → no default
                // collision). Both forms keep the arg defaults, so `Queries.country("DE")`
                // works with the value form (`Country.code ~ …`) and the lambda form
                // (`_.code.name`) identically — no more explicit `Absent` on the lambda.
                // The `$sel` suffix cannot clash with any field (GraphQL names exclude
                // `$`); backticks keep it valid when the field name is a reserved word.
                val selClass = s"`${field.name}$$sel`"
                // `A` is deliberately unbounded so a `map`/`mapInto`-projected child
                // selection (whose result is not a named tuple) still nests here.
                // Extending `FieldSelector` lets the selector value double as a typed
                // field handle for library configuration (`ConnectionFieldPolicy.of`).
                val selDef =
                    s"""final class $selClass(selArgs: Chunk[SelectionBuilder.Arg]) extends FieldSelector[$originName, $leafName]:
             |  def fieldName: String = "${field.name}"
             |  // Value form: a prebuilt selection (`Country.code ~ …`) passed directly.
             |  def apply[A](sel: SelectionBuilder[$leafName, A]): $returnType =
             |    SelectionBuilder.obj("${field.name}", $compiled, selArgs, sel, $nesting)
             |  // Lambda form: folds fields onto the empty selection (`_.code.name`). A
             |  // `SelectionBuilder` value is never a `Function1`, so overload resolution
             |  // picks value vs. lambda unambiguously.
             |  def apply[A](build: SelectionBuilder.Fields[$leafName, scala.NamedTuple.Empty] => SelectionBuilder[$leafName, A]): $returnType =
             |    SelectionBuilder.obj("${field.name}", $compiled, selArgs, build(SelectionBuilder.empty), $nesting)""".stripMargin
                val outerDef =
                    s"def $label$argClause: $selClass =\n  new $selClass($argList)"
                s"$selDef\n\n$outerDef"
            else
                val paramLists = if field.args.isEmpty then "" else s"(${argParams.mkString(", ")})"
                val returnType = s"SelectionBuilder.Deferrable[$originName, ($label: ${scalaTypeOf(field.ofType)})]"
                val argArg     = if field.args.isEmpty then "" else s", $argList"
                val body =
                    s"""SelectionBuilder.scalar("${field.name}", ${compiledTypeExpr(
                            field.ofType
                        )}, ${scalarCodecExpr(field.ofType)}$argArg)"""
                s"def $label$paramLists: $returnType =\n  $body"
            end if
        end selectorMethod

        /** The chainable-selection block appended to a non-root selector object: an
          * `extension` on `SelectionBuilder.Fields[Origin, Acc]` carrying one accessor per
          * field. Each accessor folds its field onto the accumulated selection via
          * `~`, so a chain reads a field's siblings off the first field —
          * `Country.code.name.capital` (a factorable value, same type as
          * `Country.code ~ Country.name ~ Country.capital`) and the inline lambda
          * `_.code.name` both accumulate `Concat[Acc, (field: T)]` step by step.
          *
          * There is deliberately no `select`/empty entry point as a public value: a
          * chain rooted in `SelectionBuilder.empty` types as `Concat[Empty, …]`, which
          * a match type fails to reduce once frozen in a `val`. The lambda form roots
          * in `empty` too but only ever appears inline, where reduction succeeds.
          */
        private def chainingBlock(originName: String, fields: List[FieldSpec]): String =
            val parts     = fields.map(chainAccessor(originName, _))
            val classes   = parts.flatMap(_._1)
            val accessors = parts.map(_._2).mkString("\n")
            val ext =
                s"""extension [Acc <: scala.NamedTuple.AnyNamedTuple](sb: SelectionBuilder.Fields[$originName, Acc])
         |${indent(accessors, 2)}""".stripMargin
            if classes.isEmpty then ext
            else classes.mkString("\n\n") + "\n\n" + ext
        end chainingBlock

        /** One chainable accessor: `sb.field` appends `field` to the accumulated
          * selection. Scalar leaves add `(field: T)`; object leaves take a nested
          * chainable builder `sub` and add `(field: Wrap[B])`. Both reuse the plain
          * `Origin.field` selector under `~`, keeping wire output identical.
          *
          * Fields with DEFAULTED args need one indirection: an extension method
          * desugars into the same `object Origin` namespace as the plain selector
          * `def field(args…)`, and Scala forbids two overloads that BOTH declare
          * default arguments. Same Option-D trick as the `` `field$sel` `` selector
          * classes — hoist the args onto the sole `apply` of a dedicated
          * `` `field$chain` `` class reached through a parameterless accessor. The
          * call site keeps the exact `_.field(args…)(sub)` shape. Returns the
          * optional chain-class declaration plus the accessor.
          */
        private def chainAccessor(originName: String, field: FieldSpec): (Option[String], String) =
            val label    = accessorLabel(field.name)
            val leafName = Type.innerType(field.ofType)
            val argParams = field.args.map { a =>
                val default = if isNonNull(a.ofType) then "" else " = Absent"
                s"${sanitize(a.name)}: ${scalaTypeOf(a.ofType)}$default"
            }
            val argClause = if field.args.isEmpty then "" else s"(${argParams.mkString(", ")})"
            val argNames =
                if field.args.isEmpty then ""
                else s"(${field.args.map(a => sanitize(a.name)).mkString(", ")})"
            val hasDefaultedArgs = field.args.exists(a => !isNonNull(a.ofType))
            if isComposite(leafName) then
                val chained =
                    s"scala.NamedTuple.Concat[Acc, ($label: ${wrappedTypeExpr(field.ofType, "B")})]"
                if hasDefaultedArgs then
                    val cls =
                        s"""final class `$label$$chain`[Acc <: scala.NamedTuple.AnyNamedTuple](sb: SelectionBuilder.Fields[$originName, Acc]):
               |  def apply[B]$argClause(sub: SelectionBuilder.Fields[$leafName, scala.NamedTuple.Empty] => SelectionBuilder[$leafName, B]): SelectionBuilder.Deferrable[$originName, $chained] =
               |    sb ~ $originName.$label$argNames(sub(SelectionBuilder.empty))""".stripMargin
                    val acc =
                        s"""def $label: `$label$$chain`[Acc] =
               |  new `$label$$chain`(sb)""".stripMargin
                    (Some(cls), acc)
                else
                    val acc =
                        s"""def $label[B]$argClause(sub: SelectionBuilder.Fields[$leafName, scala.NamedTuple.Empty] => SelectionBuilder[$leafName, B]): SelectionBuilder.Deferrable[$originName, $chained] =
               |  sb ~ $originName.$label$argNames(sub(SelectionBuilder.empty))""".stripMargin
                    (None, acc)
                end if
            else
                val chained = s"scala.NamedTuple.Concat[Acc, ($label: ${scalaTypeOf(field.ofType)})]"
                if hasDefaultedArgs then
                    val cls =
                        s"""final class `$label$$chain`[Acc <: scala.NamedTuple.AnyNamedTuple](sb: SelectionBuilder.Fields[$originName, Acc]):
               |  def apply$argClause: SelectionBuilder.Deferrable[$originName, $chained] =
               |    sb ~ $originName.$label$argNames""".stripMargin
                    val acc =
                        s"""def $label: `$label$$chain`[Acc] =
               |  new `$label$$chain`(sb)""".stripMargin
                    (Some(cls), acc)
                else
                    val acc =
                        s"""def $label$argClause: SelectionBuilder.Deferrable[$originName, $chained] =
               |  sb ~ $originName.$label$argNames""".stripMargin
                    (None, acc)
                end if
            end if
        end chainAccessor

        /** The `ScalarCodec` expression for a scalar/enum/input/custom-scalar type
          * reference, honouring list/nullable wrappers.
          */
        // -- local `@client` field emission ---------------------------------------

        /** The client fields declared on GraphQL type `gqlType`, in declaration order. */
        private def clientFieldsFor(gqlType: String): List[ClientFieldDecl] =
            config.clientFields.filter(_.onType == gqlType)

        /** GraphQL type name → Scala `Origin` marker (`RootQuery`/… for the roots). */
        private def clientOrigin(gqlType: String): String =
            rootSelectorMap.get(gqlType).map(_._1).getOrElse(gqlType)

        /** The `ClientField.create` descriptor expression stored under `ClientFields`.
          * The value type is the declared Scala type verbatim; its codec is derived
          * from `Schema[V]` at the generated call site.
          */
        private def clientDescriptorExpr(decl: ClientFieldDecl): String =
            s"""ClientField.create[${clientOrigin(
                    decl.onType
                )}, ${decl.tpe}]("${decl.name}", default = ${decl.default})"""

        /** Reject a client field on an unknown type or one clashing with a server field
          * of the same name (the normalized cache merges same-named fields first-wins).
          */
        private def validateClientFields(): Unit =
            config.clientFields.foreach { decl =>
                val t = decl.onType
                if !rootSelectorMap.contains(t) && !objectNames(t) then
                    throw CodegenException(
                        s"client field `${decl.name}` is declared on `$t`, which is not a GraphQL " +
                            "object or operation-root type."
                    )
                end if
                val schemaFields =
                    schema.objectTypeDefinitions
                        .find(_.name == t)
                        .map(_.fields.map(_.name).toSet)
                        .getOrElse(Set.empty)
                if schemaFields.contains(decl.name) then
                    throw CodegenException(
                        s"client field `$t.${decl.name}` clashes with a server field of the same name; " +
                            "client field names must be disjoint from schema fields on that type."
                    )
                end if
            }

        /** The chainable client accessors for `gqlType` (`_.code.name.isFavorite`),
          * a separate `extension` block on the selector object, or `None` if it has no
          * client fields.
          */
        private def clientAccessorBlock(originName: String, gqlType: String): Option[String] =
            val cfs = clientFieldsFor(gqlType)
            if cfs.isEmpty then None
            else
                val accessors = cfs
                    .map { decl =>
                        val label = accessorLabel(decl.name)
                        s"""def $label: SelectionBuilder.Fields[$originName, scala.NamedTuple.Concat[Acc, ($label: ${decl.tpe})]] =
               |  sb.clientField(ClientFields.$gqlType.$label)""".stripMargin
                    }
                    .mkString("\n")
                Some(
                    s"""extension [Acc <: scala.NamedTuple.AnyNamedTuple](sb: SelectionBuilder.Fields[$originName, Acc])
             |${indent(accessors, 2)}""".stripMargin
                )
            end if
        end clientAccessorBlock

        /** The `ClientFields` object holding every client-field descriptor, grouped by
          * GraphQL type (`ClientFields.Country.isFavorite`), or nothing when none are
          * declared. Kept OUT of the selector objects so its per-type sub-objects can
          * never clash with a server field accessor.
          */
        def emitClientFields: List[GeneratedSource] =
            if config.clientFields.isEmpty then Nil
            else
                validateClientFields()
                val types = config.clientFields.map(_.onType).distinct
                val objects = types
                    .map { gqlType =>
                        val vals =
                            clientFieldsFor(gqlType)
                                .map(d => s"val ${sanitize(d.name)} = ${clientDescriptorExpr(d)}")
                                .mkString("\n")
                        s"""object $gqlType:
               |${indent(vals, 2)}""".stripMargin
                    }
                    .mkString("\n\n")
                // Declared value types / defaults are verbatim user text and may name
                // the kyo data types unqualified (`Chunk[String]`, `Absent`).
                val kyoImports = kyoTypeImports(objects)
                val contents =
                    s"""package ${config.packageName}
             |
             |import kyo.apollo.api.*$kyoImports
             |import kyo.apollo.*
             |
             |// GENERATED by apollo-codegen: local @client field descriptors. DO NOT EDIT.
             |object ClientFields:
             |${indent(objects, 2)}
             |""".stripMargin
                List(GeneratedSource("ClientFields.scala", config.packageName, contents))

        private def scalarCodecExpr(t: Type): String = t match
            case Type.NamedType(name, nonNull) =>
                val leaf = leafCodecExpr(name)
                if nonNull then leaf else s"ScalarCodec.maybe($leaf)"
            case Type.ListType(ofType, nonNull) =>
                val listed = s"ScalarCodec.chunk(${scalarCodecExpr(ofType)})"
                if nonNull then listed else s"ScalarCodec.maybe($listed)"

        /** The leaf `ScalarCodec` for a named GraphQL type. Enums, input objects and
          * mapped custom scalars reuse their `given Schema` via `ScalarCodec.fromSchema`
          * (a string transform / derived codec, never sum-type derivation).
          */
        private def leafCodecExpr(name: String): String = name match
            case "Int"     => "ScalarCodec.int"
            case "Float"   => "ScalarCodec.double"
            case "String"  => "ScalarCodec.string"
            case "Boolean" => "ScalarCodec.boolean"
            case "ID"      => "ScalarCodec.id"
            case other =>
                if enumNames(other) || inputNames(other) then s"ScalarCodec.fromSchema[$other]"
                else if customScalarNames(other) then
                    config.scalarMappings.get(other) match
                        case Some(scala) => s"ScalarCodec.fromSchema[$scala]"
                        case None        => "ScalarCodec.string"
                else "ScalarCodec.string"

        /** The Scala value type for a field whose object leaf named tuple is `a`,
          * honouring list/nullable wrappers, e.g. `[Country!]` → `Maybe[Chunk[A]]`
          * (kyo types: a nullable field is `Maybe`, a list field is `Chunk`).
          */
        private def wrappedTypeExpr(t: Type, a: String): String = t match
            case Type.NamedType(_, nonNull) => if nonNull then a else s"Maybe[$a]"
            case Type.ListType(ofType, nonNull) =>
                val inner = s"Chunk[${wrappedTypeExpr(ofType, a)}]"
                if nonNull then inner else s"Maybe[$inner]"

        /** The `SelectionBuilder.Nesting` expression describing how an object field
          * wraps its child named tuple, e.g. `[Country!]` → `Nullable(Listed(Leaf))`.
          */
        private def nestingExpr(t: Type): String = t match
            case Type.NamedType(_, nonNull) =>
                if nonNull then "SelectionBuilder.Nesting.Leaf"
                else "SelectionBuilder.Nesting.Nullable(SelectionBuilder.Nesting.Leaf)"
            case Type.ListType(ofType, nonNull) =>
                val listed = s"SelectionBuilder.Nesting.Listed(${nestingExpr(ofType)})"
                if nonNull then listed else s"SelectionBuilder.Nesting.Nullable($listed)"

        // -- type mapping ---------------------------------------------------------

        /** The Scala type for a GraphQL type reference, honouring list/nullable
          * wrappers and the leaf's kind.
          */
        private def scalaTypeOf(t: Type): String = t match
            case Type.NamedType(name, nonNull) =>
                wrapNullable(leafScalaType(name), nonNull)
            case Type.ListType(ofType, nonNull) =>
                wrapNullable(s"Chunk[${scalaTypeOf(ofType)}]", nonNull)

        private def wrapNullable(scala: String, nonNull: Boolean): String =
            if nonNull then scala else s"Maybe[$scala]"

        /** The `import kyo.…` lines a generated source needs for the kyo data types
          * it references (`Absent`, `Chunk`, `Maybe`), each on its own line with a
          * leading newline; empty when the source uses none of them.
          */
        private def kyoTypeImports(body: String): String =
            val word = (name: String) => s"(?<![\\w.])$name\\b".r
            List("Absent", "Chunk", "Maybe")
                .filter(name => word(name).findFirstIn(body).isDefined)
                .map(name => s"\nimport kyo.$name")
                .mkString
        end kyoTypeImports

        private def leafScalaType(name: String): String =
            BuiltInScalars.get(name) match
                case Some(scala) => scala
                case None =>
                    if enumNames(name) then name // Scala 3 `enum` emitted by writeSchemaTypes.
                    else if customScalarNames(name) then config.scalarMappings.getOrElse(name, "String")
                    else if inputNames(name) then name // input case class emitted by writeSchemaTypes.
                    else if objectNames(name) then name
                    else if unionMembers.contains(name) then name // union marker trait emitted by emitSelectors.
                    else "String"                                 // unknown leaf: safest wire-compatible default.

        /** Whether a GraphQL type reference is non-null (a required variable). */
        private def isNonNull(t: Type): Boolean = t match
            case Type.NamedType(_, nonNull) => nonNull
            case Type.ListType(_, nonNull)  => nonNull

        /** The `kyo.apollo.api.CompiledType` expression for a GraphQL type reference. */
        private def compiledTypeExpr(t: Type): String = t match
            case Type.NamedType(name, nonNull) =>
                val base = s"""CompiledNamedType("$name")"""
                if nonNull then s"$base.notNull" else base
            case Type.ListType(ofType, nonNull) =>
                val base = s"${compiledTypeExpr(ofType)}.list"
                if nonNull then s"$base.notNull" else base

        // -- small utilities ------------------------------------------------------

        /** A valid Scala identifier for a GraphQL name (backticking reserved words). */
        private def sanitize(name: String): String =
            if ReservedWords(name) then s"`$name`" else name

        /** The Scala accessor name for a selection field. Reserves `deferred` and
          * `streamed` for the `.deferred` / `.streamed` incremental-delivery markers
          * (generic extensions in the same namespace as the chain accessors): a schema
          * field literally named `deferred`/`streamed` is emitted as `deferred$`/
          * `streamed$`. Collision-proof because `$` can never appear in a GraphQL name,
          * and the wire name (`field.name`) is unchanged — only the Scala accessor is
          * aliased, exactly like the `field$sel` convention.
          */
        private def accessorLabel(name: String): String =
            val s = sanitize(name)
            if s == "deferred" || s == "streamed" then s"$s$$" else s

        private def escape(s: String): String =
            s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\t", "\\t")

        private def indent(text: String, spaces: Int): String =
            val pad = " " * spaces
            text.linesIterator.map(line => if line.isEmpty then line else pad + line).mkString("\n")
    end Emitter
end ApolloClientWriter
