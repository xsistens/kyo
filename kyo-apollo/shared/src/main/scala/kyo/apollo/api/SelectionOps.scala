package kyo.apollo.api

import kyo.Chunk
import kyo.Frame
import kyo.Maybe
import kyo.Result
import kyo.Schema
import kyo.apollo.ApolloCall
import kyo.apollo.ApolloClient
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.call
import kyo.apollo.exception.ApolloParseException
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
  *     plain function, or a `derives Schema` case class). `mapInto` keeps an
  *     encoder, so its result still nests inside a parent selector and writes to the
  *     cache; `map` decodes only, so it projects the whole selection an operation is
  *     built from.
  *   - `toQuery` / `toMutation` / `toSubscription` build the typed [[Operation]]:
  *     rendering the document, collecting variables, and installing the builder
  *     itself as the operation's [[Operation.dataCodec]]. A bidirectional selection
  *     builds a normalizable operation (`Query.Normalizable`, …), which is what the
  *     normalized cache writes; a `map` projection builds a decode-only one.
  *   - `toCall` fuses the above with [[kyo.apollo.call]]: with a `given
  *     ApolloClient` in scope it goes straight from selection to [[ApolloCall]].
  *
  * The terminal builders come as a derived-name and an explicit-name overload rather
  * than with a default argument, because their normalizable forms are overloads too:
  * members of [[SelectionBuilder.Bidirectional]] (an extension there would be
  * ambiguous with these), and overloaded variants may not both declare defaults.
  */

extension [Origin, A](sb: SelectionBuilder[Origin, A])
    /** Project the result with `f`. Decode-only: arbitrary `f` has no inverse, so
      * the projection neither nests into a parent field nor combines with `~`, and an
      * operation built from it is not normalizable — the cache interceptor decodes
      * its responses without writing them. Use [[mapInto]] with a `derives Schema`
      * target for a projection the cache can write.
      */
    def map[B](f: A => B): SelectionBuilder[Origin, B] =
        SelectionBuilder.projectDecode(sb, f)

    /** Project the result into a `derives Schema` case class `B` whose field names
      * match the selection. Bidirectional (decodes responses *and* encodes for the
      * cache), reusing kyo-schema directly on the response shape — with one
      * write-side repair: kyo-schema omits empty `Maybe`/`Option` fields, but the
      * cache stores the RESPONSE shape, so selected nullable fields the codec
      * dropped are restored as explicit `null`s (see
      * [[SelectionBuilder.fillAbsentNullables]]). Without this, a write → read
      * round-trip of any `Absent` field would abort with a cache miss.
      */
    def mapInto[B](using Schema[B]): SelectionBuilder.Bidirectional[Origin, B] =
        val schemaCodec = JsonCodec.fromSchema[B]
        SelectionBuilder.projectCodec(
            sb,
            new JsonCodec[B]:
                def decode(json: Json)(using Frame): Result[ApolloParseException, B] = schemaCodec.decode(json)
                def encode(value: B): Json =
                    SelectionBuilder.fillAbsentNullables(schemaCodec.encode(value), sb.selections)
        )
    end mapInto
end extension

extension [A](sb: SelectionBuilder[RootQuery, A])
    /** Build the query, named after its root field(s). */
    def toQuery(): Query[A] = sb.toQuery(deriveOperationName(sb.selections))

    /** Build the query named `operationName`. A selection that also encodes builds a
      * [[Query.Normalizable]] even where its static type does not say so, so the cache
      * interceptor normalizes its responses; a `map` projection builds a decode-only
      * query.
      */
    def toQuery(operationName: String): Query[A] =
        sb match
            case codec: SelectionBuilder.Bidirectional[RootQuery, A] =>
                buildNormalizableQuery(operationName, codec.selections, codec.argEntries, codec)
            case _ => buildQuery(operationName, sb.selections, sb.argEntries, sb)

    /** Shortcut fusing [[toQuery]] with [[kyo.apollo.call]]: build the operation and
      * bind it to the `given ApolloClient` in one step, yielding the [[ApolloCall]]
      * the fluent chain hangs off (`.fetchPolicy` / `.data` / `.watchSignal` / …).
      * The phantom root in `SelectionBuilder[Root*, A]` erases, so the three root
      * variants are same-named overloads disambiguated via `@targetName`.
      */
    def toCall(using ApolloClient): ApolloCall[A] =
        sb.toQuery().call

    def toCall(operationName: String)(using ApolloClient): ApolloCall[A] =
        sb.toQuery(operationName).call
end extension

extension [A <: AnyNamedTuple](sb: SelectionBuilder.Bidirectional[RootQuery, A])
    /** Turn a single-connection query into a *page builder* — the seam the kyo-ui
      * `.paginated` sugar sits on. Yields `Maybe[String] => Query.Normalizable[A]`:
      * the same document on every call, with only the pagination cursor argument's
      * value swapped (`Absent` → `null` for the first page, `Present(c)` → that
      * cursor). Normalizable, because merging a page into the cached list writes it.
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
    ): Maybe[String] => Query.Normalizable[A] =
        require(
            sb.argEntries.exists(_.name == cursorArg),
            s"pagedBy: no `$cursorArg` argument on this query — add it (e.g. after = ...)"
        )
        val sels = sb.selections
        val base = sb.argEntries
        after =>
            val args = base.map(a =>
                if a.name == cursorArg then a.copy(value = after.fold(Json.JNull)(Json.JStr(_)))
                else a
            )
            buildNormalizableQuery(operationName, sels, args, sb)
    end pagedBy
end extension

extension [A](sb: SelectionBuilder[RootMutation, A])
    /** Build the mutation, named after its root field(s). */
    def toMutation(): Mutation[A] = sb.toMutation(deriveOperationName(sb.selections))

    /** Build the mutation named `operationName`; normalizable when the selection also
      * encodes (see `toQuery`).
      */
    def toMutation(operationName: String): Mutation[A] =
        sb match
            case codec: SelectionBuilder.Bidirectional[RootMutation, A] =>
                buildNormalizableMutation(operationName, codec.selections, codec.argEntries, codec)
            case _ => buildMutation(operationName, sb.selections, sb.argEntries, sb)

    @targetName("toMutationCall")
    def toCall(using ApolloClient): ApolloCall[A] =
        sb.toMutation().call

    @targetName("toMutationCall")
    def toCall(operationName: String)(using ApolloClient): ApolloCall[A] =
        sb.toMutation(operationName).call
end extension

extension [A](sb: SelectionBuilder[RootSubscription, A])
    /** Build the subscription, named after its root field(s). */
    def toSubscription(): Subscription[A] = sb.toSubscription(deriveOperationName(sb.selections))

    /** Build the subscription named `operationName`; normalizable when the selection
      * also encodes (see `toQuery`).
      */
    def toSubscription(operationName: String): Subscription[A] =
        sb match
            case codec: SelectionBuilder.Bidirectional[RootSubscription, A] =>
                buildNormalizableSubscription(operationName, codec.selections, codec.argEntries, codec)
            case _ => buildSubscription(operationName, sb.selections, sb.argEntries, sb)

    @targetName("toSubscriptionCall")
    def toCall(using ApolloClient): ApolloCall[A] =
        sb.toSubscription().call

    @targetName("toSubscriptionCall")
    def toCall(operationName: String)(using ApolloClient): ApolloCall[A] =
        sb.toSubscription(operationName).call
end extension

extension [Origin, A <: AnyNamedTuple](sb: SelectionBuilder.Bidirectional[Origin, A])
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
      * Like `toQuery`, the builder itself is installed as the fragment's
      * [[Fragment.dataCodec]], and the selection's fields become the fragment's
      * `rootField` selections — so a `Fragment.of` reads/writes exactly what an
      * equivalent query selection would. Only a bidirectional selection builds a
      * fragment, since writing a fragment encodes its data.
      */
    def toFragment(using origin: TypeName[Origin]): Fragment[A] =
        val typeName = origin.name
        new Fragment[A]:
            def dataCodec: JsonCodec[A] = sb
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
private[api] def deriveOperationName(selections: Chunk[CompiledSelection]): String =
    val parts = selections.collect { case f: CompiledField => capitalize(f.name) }
    if parts.isEmpty then "Operation" else parts.mkString

private def capitalize(s: String): String =
    if s.isEmpty then s else s"${s.head.toUpper}${s.tail}"

private def variablesOf(args: Chunk[SelectionBuilder.Arg]): Json =
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
    sels: Chunk[CompiledSelection],
    args: Chunk[SelectionBuilder.Arg]
): (Chunk[CompiledSelection], Chunk[SelectionBuilder.Arg]) =
    val used    = scala.collection.mutable.HashSet.empty[String]
    val renamed = Chunk.newBuilder[SelectionBuilder.Arg]
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

    (sels.map(rewriteSel), renamed.result())
end uniquifyVariables

/** The name, document, root field and variables of an operation built from a
  * selection whose variables [[uniquifyVariables]] has renamed — everything a built
  * `Query`/`Mutation`/`Subscription` has besides its data codec.
  */
abstract private class Built(
    kind: String,
    rootType: String,
    opName: String,
    parts: (Chunk[CompiledSelection], Chunk[SelectionBuilder.Arg])
):
    def name: String     = opName
    def document: String = DocumentPrinter.render(kind, opName, parts._2, parts._1)
    def rootField: CompiledField =
        CompiledField("data", CompiledNamedType(rootType), selections = parts._1)
    def variables: Json = variablesOf(parts._2)
end Built

private def buildQuery[D](
    opName: String,
    sels: Chunk[CompiledSelection],
    args: Chunk[SelectionBuilder.Arg],
    dc: JsonDecoder[D]
): Query[D] =
    new Built("query", "Query", opName, uniquifyVariables(sels, args)) with Query[D]:
        def dataCodec: JsonDecoder[D] = dc

private[api] def buildNormalizableQuery[D](
    opName: String,
    sels: Chunk[CompiledSelection],
    args: Chunk[SelectionBuilder.Arg],
    dc: JsonCodec[D]
): Query.Normalizable[D] =
    new Built("query", "Query", opName, uniquifyVariables(sels, args)) with Query.Normalizable[D]:
        def dataCodec: JsonCodec[D] = dc

private def buildMutation[D](
    opName: String,
    sels: Chunk[CompiledSelection],
    args: Chunk[SelectionBuilder.Arg],
    dc: JsonDecoder[D]
): Mutation[D] =
    new Built("mutation", "Mutation", opName, uniquifyVariables(sels, args)) with Mutation[D]:
        def dataCodec: JsonDecoder[D] = dc

private[api] def buildNormalizableMutation[D](
    opName: String,
    sels: Chunk[CompiledSelection],
    args: Chunk[SelectionBuilder.Arg],
    dc: JsonCodec[D]
): Mutation.Normalizable[D] =
    new Built("mutation", "Mutation", opName, uniquifyVariables(sels, args)) with Mutation.Normalizable[D]:
        def dataCodec: JsonCodec[D] = dc

private def buildSubscription[D](
    opName: String,
    sels: Chunk[CompiledSelection],
    args: Chunk[SelectionBuilder.Arg],
    dc: JsonDecoder[D]
): Subscription[D] =
    new Built("subscription", "Subscription", opName, uniquifyVariables(sels, args)) with Subscription[D]:
        def dataCodec: JsonDecoder[D] = dc

private[api] def buildNormalizableSubscription[D](
    opName: String,
    sels: Chunk[CompiledSelection],
    args: Chunk[SelectionBuilder.Arg],
    dc: JsonCodec[D]
): Subscription.Normalizable[D] =
    new Built("subscription", "Subscription", opName, uniquifyVariables(sels, args))
        with Subscription.Normalizable[D]:
        def dataCodec: JsonCodec[D] = dc
