package kyo.apollo.api

import kyo.apollo.json.Json

/** Renders a GraphQL document string from a compiled selection tree — the runtime
  * has no GraphQL printer (caliban's renderer is JVM-only, and the inline-query
  * path never parses a `.graphql` document to begin with).
  *
  * Given an operation keyword, name, its collected arguments (for the `$var: Type`
  * header) and its root selection set, it emits e.g.
  * `query GetCountry($code: ID!) { country(code: $code) { name capital } }`.
  */
object DocumentPrinter:

    /** Render a full operation document. `args` are the operation's variables (each
      * a `$name: Type` in the header); `selections` is the root selection set.
      */
    def render(
        keyword: String,
        operationName: String,
        args: List[SelectionBuilder.Arg],
        selections: List[CompiledSelection]
    ): String =
        val header =
            if args.isEmpty then ""
            else args.map(a => s"$$${a.name}: ${renderType(a.typeRef)}").mkString("(", ", ", ")")
        s"$keyword $operationName$header ${renderSelectionSet(pruneClient(selections))}"
    end render

    /** Drop local `@client` fields (and their whole subtree) from the printed
      * document — the server must never see them. Recurses so a client field at any
      * nesting depth is removed. The variable header needs no adjustment: client
      * fields carry no arguments, so they contribute nothing to `args`.
      */
    private def pruneClient(selections: List[CompiledSelection]): List[CompiledSelection] =
        selections.flatMap {
            case field: CompiledField if field.client => Nil
            case field: CompiledField                 => List(field.copy(selections = pruneClient(field.selections)))
            case fragment: CompiledFragment =>
                List(fragment.copy(selections = pruneClient(fragment.selections)))
        }

    private def renderSelectionSet(selections: List[CompiledSelection]): String =
        s"{ ${selections.map(renderSelection).mkString(" ")} }"

    private def renderSelection(selection: CompiledSelection): String = selection match
        case field: CompiledField =>
            val alias = field.alias.fold("")(a => s"$a: ")
            val args =
                if field.arguments.isEmpty then ""
                else field.arguments.map(renderArgument).mkString("(", ", ", ")")
            val directive = field.stream.fold("")(renderStream)
            val sub =
                if field.selections.isEmpty then ""
                else s" ${renderSelectionSet(field.selections)}"
            s"$alias${field.name}$args$directive$sub"
        case fragment: CompiledFragment =>
            val head =
                if fragment.typeCondition.isEmpty then "..."
                else s"... on ${fragment.typeCondition}"
            val directive = fragment.defer.fold("")(renderDefer)
            s"$head$directive ${renderSelectionSet(fragment.selections)}"

    /** Render a `@defer(label: "…"[, if: $var])` directive on a fragment. */
    private def renderDefer(directive: DeferDirective): String =
        val args = List(
            Some(s"label: ${Json.JStr(directive.label).render}"),
            directive.`if`.map(v => s"if: $$$v")
        ).flatten
        s" @defer(${args.mkString(", ")})"
    end renderDefer

    /** Render a `@stream(initialCount: N, label: "…"[, if: $var])` directive on a
      * list field. `initialCount` comes first to match the GraphQL spec's argument
      * ordering convention.
      */
    private def renderStream(directive: StreamDirective): String =
        val args = List(
            Some(s"initialCount: ${directive.initialCount}"),
            Some(s"label: ${Json.JStr(directive.label).render}"),
            directive.`if`.map(v => s"if: $$$v")
        ).flatten
        s" @stream(${args.mkString(", ")})"
    end renderStream

    private def renderArgument(argument: CompiledArgument): String =
        val value = argument.value match
            case CompiledArgumentValue.Variable(name) => s"$$$name"
            case CompiledArgumentValue.Literal(json)  => renderLiteral(json)
        s"${argument.name}: $value"
    end renderArgument

    /** Render an inline literal argument value as GraphQL syntax (distinct from JSON:
      * object keys are unquoted).
      */
    private def renderLiteral(json: Json): String = json match
        case Json.JNull       => "null"
        case Json.JBool(v)    => v.toString
        case Json.JNum(v)     => if v.isWhole then v.toLong.toString else v.toString
        case Json.JStr(v)     => Json.JStr(v).render // reuse JSON string escaping
        case Json.JArr(items) => items.map(renderLiteral).mkString("[", ", ", "]")
        case Json.JObj(fields) =>
            fields.map((k, v) => s"$k: ${renderLiteral(v)}").mkString("{ ", ", ", " }")
        case Json.JUpload(_, _) => "null" // uploads are never inline literals; render defensively

    /** Render a [[CompiledType]] as a GraphQL type reference, e.g. `[Country!]!`. */
    private def renderType(t: CompiledType): String = t match
        case CompiledNamedType(name) => name
        case CompiledNotNullType(of) => s"${renderType(of)}!"
        case CompiledListType(of)    => s"[${renderType(of)}]"
end DocumentPrinter
