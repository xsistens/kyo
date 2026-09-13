package kyo.apollo.api

import kyo.Schema
import kyo.apollo.ApolloCall
import kyo.apollo.ApolloClient
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.call
import kyo.apollo.json.Json
import scala.NamedTuple.AnyNamedTuple
import scala.annotation.targetName
import scala.collection.immutable.VectorMap

/** Terminal and projection operations that turn a [[SelectionBuilder]] into
  * something [[kyo.apollo.ApolloClient]] can run. These are top-level extensions,
  * so a single `import kyo.apollo.api.*` brings them into scope alongside the
  * builder itself.
  *
  *   - `map` / `mapInto` project a selection's result into an arbitrary type (a
  *     plain function, or a `derives Schema` case class). Because they keep the
  *     same wire selection, a projected child still nests inside a parent selector.
  *   - `toQuery` / `toMutation` / `toSubscription` build the typed [[Operation]]:
  *     rendering the document, collecting variables, and installing the builder's
  *     structural codec as the operation's [[Operation.dataCodec]].
  *   - `toCall` fuses the above with [[kyo.apollo.call]]: with a `given
  *     ApolloClient` in scope it goes straight from selection to [[ApolloCall]].
  */

extension [Origin, A](sb: SelectionBuilder[Origin, A])
    /** Project the result with `f`. Decode-only: the resulting operation cannot be
      * written back into the normalized cache (arbitrary `f` has no inverse) — use
      * [[mapInto]] with a `derives Schema` target for that.
      */
    def map[B](f: A => B): SelectionBuilder[Origin, B] =
        SelectionBuilder.project(
            sb,
            new JsonCodec[B]:
                def decode(json: Json): B = f(sb.decode(json))
                def encode(value: B): Json =
                    throw UnsupportedOperationException(
                        "`.map` projections are decode-only; use `.mapInto[C]` (C derives Schema) for cache writes"
                    )
        )

    /** Project the result into a `derives Schema` case class `B` whose field names
      * match the selection. Bidirectional (decodes responses *and* encodes for the
      * cache), reusing kyo-schema directly on the response shape — with one
      * write-side repair: kyo-schema omits `None` fields, but the cache stores the
      * RESPONSE shape, so selected nullable fields the codec dropped are restored
      * as explicit `null`s (see [[SelectionBuilder.fillAbsentNullables]]). Without
      * this, a write → read round-trip of any `Option = None` field would abort
      * with a cache miss.
      */
    def mapInto[B](using Schema[B]): SelectionBuilder[Origin, B] =
        val schemaCodec = JsonCodec.fromSchema[B]
        SelectionBuilder.project(
            sb,
            new JsonCodec[B]:
                def decode(json: Json): B = schemaCodec.decode(json)
                def encode(value: B): Json =
                    SelectionBuilder.fillAbsentNullables(schemaCodec.encode(value), sb.selections)
        )
    end mapInto
end extension

extension [A <: AnyNamedTuple](sb: SelectionBuilder[RootQuery, A])
    def toQuery(operationName: String = deriveOperationName(sb.selections)): Query[A] =
        buildQuery(operationName, sb.selections, sb.argEntries, codecOf(sb))

    /** Shortcut fusing [[toQuery]] with [[kyo.apollo.call]]: build the operation and
      * bind it to the `given ApolloClient` in one step, yielding the [[ApolloCall]]
      * the fluent chain hangs off (`.fetchPolicy` / `.data` / `.watchSignal` / …).
      * Two overloads (derived name / explicit name) instead of a default argument,
      * because the phantom root in `SelectionBuilder[Root*, A]` erases: the three
      * root variants are same-named overloads (disambiguated via `@targetName`),
      * and overloaded variants may not declare default arguments.
      */
    def toCall(using ApolloClient): ApolloCall[A] =
        sb.toQuery().call

    def toCall(operationName: String)(using ApolloClient): ApolloCall[A] =
        sb.toQuery(operationName).call
end extension

extension [A <: AnyNamedTuple](sb: SelectionBuilder[RootQuery, A])
    /** Turn a single-connection query into a *page builder* — the seam the kyo-ui
      * `.paginated` sugar sits on. Yields `Option[String] => Query[A]`: the same
      * document on every call, with only the pagination cursor argument's value
      * swapped (`None` → `null` for the first page, `Some(c)` → that cursor).
      *
      * Since the [[SelectionBuilder]] binds an argument to a variable, only the
      * variable's *value* changes across pages — the document (and its APQ hash) is
      * stable. `cursorArg` names the field argument that carries the cursor
      * (Relay's `after`); the query must declare it, else this fails fast. Intended
      * for a *single* connection — for several connections in one query, use
      * [[kyo.apollo.usePaginatedQuery]] with an explicit page builder, so each
      * connection's cursor is set independently.
      */
    def pagedBy(
        cursorArg: String = "after",
        operationName: String = deriveOperationName(sb.selections)
    ): Option[String] => Query[A] =
        require(
            sb.argEntries.exists(_.name == cursorArg),
            s"pagedBy: no `$cursorArg` argument on this query — add it (e.g. after = ...)"
        )
        val sels  = sb.selections
        val codec = codecOf(sb)
        val base  = sb.argEntries
        after =>
            val args = base.map(a =>
                if a.name == cursorArg then a.copy(value = after.fold(Json.JNull)(Json.JStr(_)))
                else a
            )
            buildQuery(operationName, sels, args, codec)
end extension

extension [A <: AnyNamedTuple](sb: SelectionBuilder[RootMutation, A])
    def toMutation(operationName: String = deriveOperationName(sb.selections)): Mutation[A] =
        buildMutation(operationName, sb.selections, sb.argEntries, codecOf(sb))

    @targetName("toMutationCall")
    def toCall(using ApolloClient): ApolloCall[A] =
        sb.toMutation().call

    @targetName("toMutationCall")
    def toCall(operationName: String)(using ApolloClient): ApolloCall[A] =
        sb.toMutation(operationName).call
end extension

extension [A <: AnyNamedTuple](sb: SelectionBuilder[RootSubscription, A])
    def toSubscription(operationName: String = deriveOperationName(sb.selections)): Subscription[A] =
        buildSubscription(operationName, sb.selections, sb.argEntries, codecOf(sb))

    @targetName("toSubscriptionCall")
    def toCall(using ApolloClient): ApolloCall[A] =
        sb.toSubscription().call

    @targetName("toSubscriptionCall")
    def toCall(operationName: String)(using ApolloClient): ApolloCall[A] =
        sb.toSubscription(operationName).call
end extension

extension [Origin, A <: AnyNamedTuple](sb: SelectionBuilder[Origin, A])
    /** Build a cache-access [[Fragment]] from this selection — the
      * targeted-read/write analogue of [[toQuery]].
      *
      * The GraphQL type the fragment applies to (naming the record's type
      * condition) comes from `given TypeName[Origin]`, which codegen already emits
      * beside every phantom marker. It used to be a `String` parameter, which was
      * both redundant and unchecked: a typo compiled fine and produced a fragment
      * addressing a type the schema does not have. Now the phantom `Origin` decides
      * it, and a type with no generated marker simply does not compile.
      *
      * Like `toQuery`, the builder's structural codec is installed as the
      * fragment's [[Fragment.dataCodec]] (its [[Fragment.dataSchema]] is unused,
      * matching the inline-operation path), and the selection's fields become the
      * fragment's `rootField` selections — so a `Fragment.of` reads/writes exactly
      * what an equivalent query selection would.
      */
    def toFragment(using origin: TypeName[Origin]): Fragment[A] =
        val typeName = origin.name
        new Fragment[A]:
            override def dataCodec: JsonCodec[A] = codecOf(sb)
            def dataSchema: Schema[A]            = throw noSchema(s"fragment on $typeName")
            def rootField: CompiledField =
                CompiledField(typeName, CompiledNamedType(typeName), selections = sb.selections)
        end new
    end toFragment
end extension

// -- internals ----------------------------------------------------------------

/** Derive a PascalCase operation name from the selection's root field name(s),
  * used as the default when the caller omits one — GraphQL operation names are
  * optional for a single-operation document, and each root field carries a
  * meaningful name (`country` → `Country`, `updateCountry` → `UpdateCountry`,
  * `countryUpdated` → `CountryUpdated`). Only schema field names contribute
  * (inline fragments are skipped); multiple root fields concatenate; the
  * degenerate empty case falls back to `Operation`. The name is a label only
  * (observability / APQ readability), so cross-document collisions are harmless.
  */
private def deriveOperationName(selections: List[CompiledSelection]): String =
    val parts = selections.collect { case f: CompiledField => capitalize(f.name) }
    if parts.isEmpty then "Operation" else parts.mkString

private def capitalize(s: String): String =
    if s.isEmpty then s else s"${s.head.toUpper}${s.tail}"

/** Wrap a builder's decode/encode as a [[JsonCodec]] for the operation. */
private def codecOf[A](sb: SelectionBuilder[?, A]): JsonCodec[A] =
    new JsonCodec[A]:
        def decode(json: Json): A  = sb.decode(json)
        def encode(value: A): Json = sb.encode(value)

private def variablesOf(args: List[SelectionBuilder.Arg]): Json =
    Json.JObj(VectorMap.from(args.map(a => a.name -> a.value)))

/** Give every operation variable a unique name so a document that repeats a
  * field-argument name across fields stays valid GraphQL.
  *
  * The GraphQL field-argument name and the operation-variable name are
  * independent: two connections each paginated by an `after` argument must bind
  * to *distinct* variables (`$after`, `$after2`) — `usersConnection(after: $after)`
  * and `postsConnection(after: $after2)` — or the document declares `$after`
  * twice (invalid) and the variables map collapses both values into one. The
  * [[SelectionBuilder]] binds each argument to a variable of the *same* name, so
  * repeats collide. This rewrites BOTH halves in lockstep: the argument's
  * variable reference in the field tree and the operation-variable entry (header
  * + variables). First occurrence keeps the name; each later collision is
  * suffixed (`after`, `after2`, `after3`, …).
  *
  * The `argEntries` list and the field tree are flattened from the same builder
  * in identical pre-order (a node's own arguments before its children, siblings
  * left-to-right), so a single synchronized pre-order walk keeps the two aligned.
  * A no-op — byte-identical output — when no argument name repeats, so every
  * existing single-variable document is unchanged.
  */
private def uniquifyVariables(
    sels: List[CompiledSelection],
    args: List[SelectionBuilder.Arg]
): (List[CompiledSelection], List[SelectionBuilder.Arg]) =
    val used    = scala.collection.mutable.HashSet.empty[String]
    val renamed = scala.collection.mutable.ListBuffer.empty[SelectionBuilder.Arg]
    val argIter = args.iterator

    def fresh(base: String): String =
        if used.add(base) then base
        else
            var i = 2
            while !used.add(s"$base$i") do i += 1
            s"$base$i"

    def rewriteArg(a: CompiledArgument): CompiledArgument = a.value match
        case CompiledArgumentValue.Variable(_) =>
            val arg  = argIter.next() // corresponding Arg — same pre-order as the tree
            val name = fresh(arg.name)
            renamed += arg.copy(name = name)
            CompiledArgument(a.name, CompiledArgumentValue.Variable(name))
        case _ => a // an inline literal carries no operation variable

    def rewriteSel(s: CompiledSelection): CompiledSelection = s match
        case f: CompiledField =>
            // A node's own arguments FIRST, then descend — matching both the
            // `argEntries` flattening and the DocumentPrinter's render order.
            val newArgs = f.arguments.map(rewriteArg)
            f.copy(arguments = newArgs, selections = f.selections.map(rewriteSel))
        case frag: CompiledFragment =>
            frag.copy(selections = frag.selections.map(rewriteSel))

    (sels.map(rewriteSel), renamed.toList)
end uniquifyVariables

private def buildQuery[D](
    opName: String,
    sels: List[CompiledSelection],
    args: List[SelectionBuilder.Arg],
    dc: JsonCodec[D]
): Query[D] =
    val (usels, uargs) = uniquifyVariables(sels, args)
    new Query[D]:
        def name: String     = opName
        def document: String = DocumentPrinter.render("query", opName, uargs, usels)
        def rootField: CompiledField =
            CompiledField("data", CompiledNamedType("Query"), selections = usels)
        def variables: Json                  = variablesOf(uargs)
        def dataSchema: Schema[D]            = throw noSchema(opName)
        override def dataCodec: JsonCodec[D] = dc
    end new
end buildQuery

private def buildMutation[D](
    opName: String,
    sels: List[CompiledSelection],
    args: List[SelectionBuilder.Arg],
    dc: JsonCodec[D]
): Mutation[D] =
    val (usels, uargs) = uniquifyVariables(sels, args)
    new Mutation[D]:
        def name: String     = opName
        def document: String = DocumentPrinter.render("mutation", opName, uargs, usels)
        def rootField: CompiledField =
            CompiledField("data", CompiledNamedType("Mutation"), selections = usels)
        def variables: Json                  = variablesOf(uargs)
        def dataSchema: Schema[D]            = throw noSchema(opName)
        override def dataCodec: JsonCodec[D] = dc
    end new
end buildMutation

private def buildSubscription[D](
    opName: String,
    sels: List[CompiledSelection],
    args: List[SelectionBuilder.Arg],
    dc: JsonCodec[D]
): Subscription[D] =
    val (usels, uargs) = uniquifyVariables(sels, args)
    new Subscription[D]:
        def name: String     = opName
        def document: String = DocumentPrinter.render("subscription", opName, uargs, usels)
        def rootField: CompiledField =
            CompiledField("data", CompiledNamedType("Subscription"), selections = usels)
        def variables: Json                  = variablesOf(uargs)
        def dataSchema: Schema[D]            = throw noSchema(opName)
        override def dataCodec: JsonCodec[D] = dc
    end new
end buildSubscription

private def noSchema(opName: String): UnsupportedOperationException =
    UnsupportedOperationException(
        s"inline SelectionBuilder operation '$opName' has no kyo Schema; its dataCodec is used instead"
    )
