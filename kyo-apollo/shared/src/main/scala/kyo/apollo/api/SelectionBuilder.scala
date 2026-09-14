package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.Frame
import kyo.Maybe
import kyo.Present
import kyo.Result
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.Json
import kyo.discard
import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.Concat
import scala.NamedTuple.DropNames
import scala.NamedTuple.Empty
import scala.NamedTuple.NamedTuple
import scala.NamedTuple.Names
import scala.annotation.implicitNotFound
import scala.collection.immutable.VectorMap

/** A type-safe, composable GraphQL selection, rooted in the object type `Origin`
  * and decoding to `A`.
  *
  * This is the inline alternative to a `.graphql` document + per-operation
  * codegen: a query is written as ordinary Scala against generated schema
  * selector objects (`Queries.country(code)(Country.name ~ Country.capital)`), so
  * the IDE autocompletes every field while you write it, and the result type is
  * the named tuple `(name: String, capital: Maybe[String])` — inferred, not
  * hand-written. Nullable fields decode to `Maybe`, list fields to `Chunk`; the
  * stdlib `Option`/`List` never appear in a result type.
  *
  *   - `Origin` is a phantom scoping type (one empty marker per GraphQL object
  *     type, plus `RootQuery`/`RootMutation`/`RootSubscription` from `core`). It
  *     has no instances and no runtime footprint; it only stops a `Country`
  *     selector from being combined into a `Continent` selection.
  *   - `A` is the selection's result type — a named tuple for the composable
  *     ([[SelectionBuilder.Fields]]) selectors, or an arbitrary type once projected
  *     with `map`/`mapInto`.
  *
  * What a selection can do is its type, not a check at run time:
  *   - every selection decodes; only a [[SelectionBuilder.Bidirectional]] one also
  *     encodes, which is what nesting it into a parent field and writing its
  *     operation to the normalized cache need. A `map` projection decodes only;
  *     `mapInto` and every named-tuple selection are bidirectional;
  *   - only [[SelectionBuilder.Fields]] combine with `~`;
  *   - only [[SelectionBuilder.Deferrable]] — a selection whose last-added operand
  *     is a single field — take `.deferred` and `.streamed`, and `.streamed` also
  *     needs that field to be a list.
  *
  * A misuse is therefore a compile error at the call site.
  */
sealed trait SelectionBuilder[Origin, A] extends JsonDecoder[A]:

    /** The compiled selections of this selection set, in declaration order. A
      * selection is usually a [[CompiledField]], but a defer group contributes a
      * [[CompiledFragment]] — hence the widened element type. Doubles as the
      * `rootField`-tree source.
      */
    def selections: Chunk[CompiledSelection]

    /** Every argument in this selection subtree, in encounter order. */
    private[api] def argEntries: Chunk[SelectionBuilder.Arg]

    /** Decode a GraphQL response object into the result `A`. A response of the wrong
      * shape is an [[ApolloParseException]] failure naming the shape expected and the
      * JSON type found; anything a codec throws beyond that is a panic.
      */
    def decode(json: Json)(using Frame): Result[ApolloParseException, A]
end SelectionBuilder

object SelectionBuilder:

    /** A field argument captured at the call site: its GraphQL name, its type (for
      * rendering the `$var: Type` header), and its already-encoded value.
      */
    final case class Arg(name: String, typeRef: CompiledType, value: Json)

    /** The response object an encode writes its fields into, in selection order. */
    private[api] type RowBuilder = scala.collection.mutable.Builder[(String, Json), VectorMap[String, Json]]

    /** How a nested object field wraps its child selection's result. Recursive, so
      * it models arbitrarily deep list/nullable structures, e.g. `[Country!]`
      * decodes as `Nullable(Listed(Leaf))` → `Maybe[Chunk[A]]`.
      */
    enum Nesting derives CanEqual:
        case Leaf
        case Nullable(inner: Nesting)
        case Listed(inner: Nesting)

        private[api] def decode(json: Json, child: Json => Result[ApolloParseException, Any])(using
            Frame
        ): Result[ApolloParseException, Any] =
            this match
                case Leaf => child(json)
                case Nullable(inner) =>
                    json match
                        case Json.JNull => Result.succeed(Absent)
                        case other      => inner.decode(other, child).map(Present(_))
                case Listed(inner) =>
                    json match
                        case Json.JArr(items) => Result.collect(items.map(inner.decode(_, child))).map(Chunk.from)
                        case other            => Result.fail(ApolloParseException(other, "a GraphQL list"))

        private[api] def encode(value: Any, child: Any => Json): Json = this match
            case Leaf => child(value)
            case Nullable(inner) =>
                value.asInstanceOf[Maybe[Any]] match
                    case Absent     => Json.JNull
                    case Present(v) => inner.encode(v, child)
            case Listed(inner) =>
                Json.JArr(value.asInstanceOf[Chunk[Any]].map(inner.encode(_, child)))
    end Nesting

    /** A selection that also encodes its result back into the response shape — what
      * a parent field needs to nest it and what the normalized cache needs to write
      * its operation. Every [[Fields]] selection is one, and so is a `mapInto`
      * projection; a `map` projection is not, since an arbitrary function has no
      * inverse.
      */
    sealed trait Bidirectional[Origin, A] extends SelectionBuilder[Origin, A] with JsonCodec[A]:

        /** Encode a result value back into a response-shaped [[Json]] object. */
        def encode(value: A): Json

        // The normalizable terminal forms are members, not extensions: Scala 3 reports an
        // extension overload on this type and one on `SelectionBuilder` as ambiguous, and a
        // member is chosen before any extension is tried.

        /** Build the query of this root selection, named after its root field(s); the
          * normalized cache can write it.
          */
        def toQuery()(using Origin =:= RootQuery): Query.Normalizable[A] =
            toQuery(deriveOperationName(selections))

        /** Build the query of this root selection named `operationName`; the normalized
          * cache can write it.
          */
        def toQuery(operationName: String)(using Origin =:= RootQuery): Query.Normalizable[A] =
            buildNormalizableQuery(operationName, selections, argEntries, this)

        /** Build the mutation of this root selection, named after its root field(s); the
          * normalized cache can write it.
          */
        def toMutation()(using Origin =:= RootMutation): Mutation.Normalizable[A] =
            toMutation(deriveOperationName(selections))

        /** Build the mutation of this root selection named `operationName`; the normalized
          * cache can write it.
          */
        def toMutation(operationName: String)(using Origin =:= RootMutation): Mutation.Normalizable[A] =
            buildNormalizableMutation(operationName, selections, argEntries, this)

        /** Build the subscription of this root selection, named after its root field(s);
          * the normalized cache can write it.
          */
        def toSubscription()(using Origin =:= RootSubscription): Subscription.Normalizable[A] =
            toSubscription(deriveOperationName(selections))

        /** Build the subscription of this root selection named `operationName`; the
          * normalized cache can write it.
          */
        def toSubscription(operationName: String)(using Origin =:= RootSubscription): Subscription.Normalizable[A] =
            buildNormalizableSubscription(operationName, selections, argEntries, this)
    end Bidirectional

    /** A named-tuple-shaped selection: the composable form. Only these combine with
      * `~` (the extension below). The runtime tuple carries no names, so every node
      * below takes the named tuple it decodes to as a type argument its constructor
      * or combinator pins.
      *
      * A selection's structure — its selections, arguments and arity — is computed
      * once, when it is built. Decoding a response allocates one array of `arity`
      * slots, which every node fills at its own offset, and one tuple over it;
      * encoding reads the tuple's elements by offset into one object builder.
      */
    sealed trait Fields[Origin, A <: AnyNamedTuple] extends Bidirectional[Origin, A]:

        /** The number of slots this selection contributes to its result tuple. */
        private[api] def arity: Int

        /** Decode this node's slots from `row` into `out`, starting at `offset`. */
        private[api] def decodeInto(row: Map[String, Json], out: Array[Any], offset: Int)(using
            Frame
        ): Result[ApolloParseException, Unit]

        /** Encode this node's slots of `value`, starting at `offset`, into `out`. */
        private[api] def encodeInto(value: Product, offset: Int, out: RowBuilder): Unit

        final def decode(json: Json)(using Frame): Result[ApolloParseException, A] = json match
            case Json.JObj(row) =>
                val out = new Array[Any](arity)
                decodeInto(row, out, 0).map(_ => Tuple.fromArray(out).asInstanceOf[A])
            case other => Result.fail(ApolloParseException(other, "a GraphQL object"))

        final def encode(value: A): Json =
            val out = VectorMap.newBuilder[String, Json]
            encodeInto(value.asInstanceOf[Product], 0, out)
            Json.JObj(out.result())
        end encode
    end Fields

    /** A selection whose LAST-added operand is a single field — the operand
      * `.deferred` and `.streamed` act on. A field selector is one; `a ~ b` is one
      * exactly when `b` is. The empty selection, a `@defer` group, an inline-fragment
      * branch, a fragment spread and a `@client` field are [[Fields]] only.
      */
    sealed trait Deferrable[Origin, A <: AnyNamedTuple] extends Fields[Origin, A]:

        /** Wrap the last-added field in an anonymous `@defer` group, auto-labelled by
          * its response name — the runtime of `.deferred`, which pins `R` to
          * `DeferLast[A]`. In a chain (`~` is left-associative) the last field is the
          * outer combination's right operand, always arity 1.
          */
        private[api] def deferLast[R <: AnyNamedTuple]: Fields[Origin, R]

        /** Mark the last-added field with `@stream`, auto-labelled by its response
          * name — the runtime of `.streamed`. The result type is unchanged (`@stream`
          * grows a `Chunk[T]` in place), and the last operand is still that field.
          */
        private[api] def streamLast(initialCount: Int, condition: Maybe[String]): Deferrable[Origin, A]
    end Deferrable

    /** Evidence that the last element of the named tuple `A` is a list (`Chunk[T]`,
      * nullable or not) — what `.streamed` requires, since `@stream` applies to list
      * fields only.
      */
    @implicitNotFound("`.streamed` applies to a list field, but the last field of ${A} is not a list")
    final class EndsInList[A <: AnyNamedTuple] private ()

    object EndsInList:
        given [A <: AnyNamedTuple](using Tuple.Last[DropNames[A]] <:< (Chunk[?] | Maybe[Chunk[?]])): EndsInList[A] =
            new EndsInList[A]

    /** A single selected field: its compiled metadata + arguments + a value codec. */
    final private class Field[Origin, A <: AnyNamedTuple](
        compiled: CompiledField,
        ownArgs: Chunk[Arg],
        childArgs: Chunk[Arg],
        decodeValue: (Json, Frame) => Result[ApolloParseException, Any],
        encodeValue: Any => Json
    ) extends Deferrable[Origin, A]:
        private[api] val arity: Int              = 1
        val selections: Chunk[CompiledSelection] = Chunk(compiled)
        private[api] val argEntries: Chunk[Arg]  = ownArgs ++ childArgs
        private[api] def deferLast[R <: AnyNamedTuple]: Fields[Origin, R] =
            Deferred[Origin, R](compiled.responseName, Absent, this, single = true)
        private[api] def streamLast(initialCount: Int, condition: Maybe[String]): Deferrable[Origin, A] =
            Field[Origin, A](
                compiled.copy(stream =
                    Present(StreamDirective(compiled.responseName, initialCount, condition))
                ),
                ownArgs,
                childArgs,
                decodeValue,
                encodeValue
            )
        private[api] def decodeInto(row: Map[String, Json], out: Array[Any], offset: Int)(using
            frame: Frame
        ): Result[ApolloParseException, Unit] =
            decodeValue(row.getOrElse(compiled.responseName, Json.JNull), frame).map(value => out(offset) = value)
        private[api] def encodeInto(value: Product, offset: Int, out: RowBuilder): Unit =
            discard(out += compiled.responseName -> encodeValue(value.productElement(offset)))
    end Field

    /** Two selections combined: `right`'s slots follow `left`'s. `R` is the named tuple
      * the combinator pins (`Concat` of the operands' for `~`).
      */
    sealed abstract private class Pair[Origin, R <: AnyNamedTuple](
        left: Fields[Origin, ? <: AnyNamedTuple],
        right: Fields[Origin, ? <: AnyNamedTuple]
    ) extends Fields[Origin, R]:
        private val leftArity: Int               = left.arity
        private[api] val arity: Int              = leftArity + right.arity
        val selections: Chunk[CompiledSelection] = left.selections ++ right.selections
        private[api] val argEntries: Chunk[Arg]  = left.argEntries ++ right.argEntries
        private[api] def decodeInto(row: Map[String, Json], out: Array[Any], offset: Int)(using
            Frame
        ): Result[ApolloParseException, Unit] =
            left.decodeInto(row, out, offset).flatMap(_ => right.decodeInto(row, out, offset + leftArity))
        private[api] def encodeInto(value: Product, offset: Int, out: RowBuilder): Unit =
            left.encodeInto(value, offset, out)
            right.encodeInto(value, offset + leftArity, out)
    end Pair

    /** A combination whose right operand is not a single field. */
    final private class Combine[Origin, R <: AnyNamedTuple](
        left: Fields[Origin, ? <: AnyNamedTuple],
        right: Fields[Origin, ? <: AnyNamedTuple]
    ) extends Pair[Origin, R](left, right)

    /** A combination whose right operand ends in a single field, so `.deferred` and
      * `.streamed` recurse into it and leave `left` untouched.
      */
    final private class CombineDeferrable[Origin, R <: AnyNamedTuple](
        left: Fields[Origin, ? <: AnyNamedTuple],
        right: Deferrable[Origin, ? <: AnyNamedTuple]
    ) extends Pair[Origin, R](left, right) with Deferrable[Origin, R]:
        private[api] def deferLast[R2 <: AnyNamedTuple]: Fields[Origin, R2] =
            Combine[Origin, R2](left, right.deferLast[AnyNamedTuple])
        private[api] def streamLast(initialCount: Int, condition: Maybe[String]): Deferrable[Origin, R] =
            CombineDeferrable[Origin, R](left, right.streamLast(initialCount, condition))
    end CombineDeferrable

    /** The empty selection: the neutral starting point of a chainable selection
      * (`Country.select.code.name`). Selects no fields (arity 0), so combining it
      * with `~` reduces away — `Concat[Empty, B] =:= B` at the type level and
      * `EmptyTuple ++ row =:= row` at runtime — leaving exactly the chained field.
      */
    final private class EmptySel[Origin] extends Fields[Origin, Empty]:
        private[api] val arity: Int                                                     = 0
        val selections: Chunk[CompiledSelection]                                        = Chunk.empty
        private[api] val argEntries: Chunk[Arg]                                         = Chunk.empty
        private[api] def encodeInto(value: Product, offset: Int, out: RowBuilder): Unit = ()
        private[api] def decodeInto(row: Map[String, Json], out: Array[Any], offset: Int)(using
            Frame
        ): Result[ApolloParseException, Unit] =
            Result.unit
    end EmptySel

    /** A `@defer`ed inline group. Contributes its child's fields to the *parent*
      * object (an anonymous [[CompiledFragment]] with a `@defer` directive, spliced
      * unconditionally by the field collector — no nesting or `__typename`, unlike
      * [[obj]]), but exposes the child's result as a single `Maybe[S]` element:
      * `Absent` until the deferred payload arrives, `Present` once its fields appear
      * in the row. `label` correlates the incremental patch (auto-derived from a
      * field's response name for `.deferred`, explicit for a `defer` group). `R` is
      * the 1-ary named tuple the caller ascribes, e.g. `(details: Maybe[S])`.
      */
    final private class Deferred[Origin, R <: AnyNamedTuple](
        label: String,
        condition: Maybe[String],
        child: Fields[Origin, ? <: AnyNamedTuple],
        single: Boolean
    ) extends Fields[Origin, R]:
        private[api] val arity: Int = 1
        val selections: Chunk[CompiledSelection] =
            Chunk(
                CompiledFragment("", Chunk.empty, child.selections, defer = Present(DeferDirective(label, condition)))
            )
        private[api] val argEntries: Chunk[Arg] = child.argEntries
        // The group counts as arrived once any of its own fields is in the row.
        private val responseNames: Chunk[String] = child.selections.collect { case f: CompiledField => f.responseName }
        // `single` (from `.deferred`) exposes the sole field's *value* as `Maybe[V]`;
        // a `defer` group exposes the whole child tuple as `Maybe[S]`.
        private[api] def decodeInto(row: Map[String, Json], out: Array[Any], offset: Int)(using
            Frame
        ): Result[ApolloParseException, Unit] =
            if !responseNames.exists(row.contains) then
                out(offset) = Absent
                Result.unit
            else
                val slots = new Array[Any](child.arity)
                child.decodeInto(row, slots, 0).map { _ =>
                    out(offset) = Present(if single then slots(0) else Tuple.fromArray(slots))
                }
            end if
        end decodeInto
        private[api] def encodeInto(value: Product, offset: Int, out: RowBuilder): Unit =
            value.productElement(offset).asInstanceOf[Maybe[Any]] match
                case Present(v) =>
                    child.encodeInto(if single then Tuple1(v) else v.asInstanceOf[Product], 0, out)
                case Absent => ()
    end Deferred

    /** A selection whose result a `map` projected to an arbitrary `B`. It keeps the
      * same wire selection and arguments and decodes only: it neither combines with
      * `~` nor nests into a parent field, and its operation is not normalizable.
      */
    final private class Mapped[Origin, A, B](
        under: SelectionBuilder[Origin, A],
        f: A => B
    ) extends SelectionBuilder[Origin, B]:
        val selections: Chunk[CompiledSelection]                             = under.selections
        private[api] val argEntries: Chunk[Arg]                              = under.argEntries
        def decode(json: Json)(using Frame): Result[ApolloParseException, B] = under.decode(json).map(f)
    end Mapped

    /** A selection whose result a `mapInto` projected to a `B` with a codec that also
      * encodes: it nests into a parent field and its operation is normalizable, but
      * it does not combine with `~`.
      */
    final private class MappedInto[Origin, B](
        under: SelectionBuilder[Origin, ?],
        codec: JsonCodec[B]
    ) extends Bidirectional[Origin, B]:
        val selections: Chunk[CompiledSelection]                             = under.selections
        private[api] val argEntries: Chunk[Arg]                              = under.argEntries
        def decode(json: Json)(using Frame): Result[ApolloParseException, B] = codec.decode(json)
        def encode(value: B): Json                                           = codec.encode(value)
    end MappedInto

    /** Decode one leaf value. [[ScalarCodec]]'s contract has no frame to build a
      * failure with, so a codec still signals a wrong shape by raising
      * `ScalarDecodeException` (or, for a schema-backed leaf, `ApolloParseException`);
      * this is the one place that turns it into the failure value. Anything else a
      * codec raises stays a panic.
      */
    private def leaf[V](codec: ScalarCodec[V], json: Json)(using Frame): Result[ApolloParseException, V] =
        Result.catching[ScalarDecodeException | ApolloParseException](codec.decode(json)).mapFailure {
            case parse: ApolloParseException   => parse
            case scalar: ScalarDecodeException => ApolloParseException(json, s"a GraphQL ${scalar.expected}")
        }

    /** Bind a field's captured arguments to same-named operation variables. */
    private def bind(args: Chunk[Arg]): Chunk[CompiledArgument] =
        args.map(a => CompiledArgument.variable(a.name))

    /** The implicit `__typename` every object selection carries — requested in the
      * document and re-emitted on encode so the normalized cache can key records by
      * `__typename` + id, exactly as the legacy per-operation `Data` classes did.
      */
    private val TypenameField: CompiledField =
        CompiledField("__typename", CompiledNamedType("String").notNull)

    /** Prepend `__typename` to an encoded object so cache normalization can key it. */
    private[api] def withTypename(json: Json, typeName: String): Json = json match
        case Json.JObj(fields) if !fields.contains("__typename") =>
            Json.JObj(VectorMap("__typename" -> Json.JStr(typeName)) ++ fields)
        case other => other

    /** Restore the RESPONSE shape of a Schema-encoded projection: a selected
      * nullable field the codec omitted is written back as an explicit `null`.
      *
      * kyo-schema encodes optional (`Maybe`/`Option`) fields as *absent* when
      * empty, but the wire response (and therefore the normalized cache, whose
      * reader treats a selected-but-absent field as a cache miss) carries an
      * explicit `null` — Apollo Client normalizes the raw response JSON, so the two
      * never diverge there. A `mapInto` value is deterministically complete
      * (absence can only mean "empty"), so the fill is always sound. Composite
      * fields recurse (through lists) with their sub-selections; fragments are
      * left untouched — a deferred group's fields legitimately stay absent until
      * the payload arrives, and an inline fragment's applicability depends on the
      * concrete runtime type this static walk cannot know.
      */
    private[api] def fillAbsentNullables(json: Json, selections: Chunk[CompiledSelection]): Json =
        json match
            case Json.JObj(fields) =>
                val out = selections.foldLeft(fields) {
                    case (acc, field: CompiledField) if !field.client =>
                        acc.get(field.responseName) match
                            case Some(value) if field.selections.nonEmpty =>
                                acc.updated(field.responseName, fillNested(value, field.selections))
                            case Some(_) => acc
                            case None =>
                                field.fieldType match
                                    case _: CompiledNotNullType => acc
                                    case _                      => acc.updated(field.responseName, Json.JNull)
                    case (acc, _) => acc
                }
                Json.JObj(out)
            case other => other

    /** Recurse [[fillAbsentNullables]] through a composite field's value: objects
      * are filled against the field's sub-selections, lists element-wise; scalars
      * and `null` pass through.
      */
    private def fillNested(value: Json, selections: Chunk[CompiledSelection]): Json = value match
        case obj: Json.JObj   => fillAbsentNullables(obj, selections)
        case Json.JArr(items) => Json.JArr(items.map(fillNested(_, selections)))
        case other            => other

    /** Build a scalar/enum leaf selector. `R` is the 1-ary named tuple the caller
      * (generated selector) ascribes, e.g. `(name: String)`.
      */
    def scalar[Origin, R <: AnyNamedTuple, V](
        name: String,
        fieldType: CompiledType,
        codec: ScalarCodec[V],
        arguments: Chunk[Arg] = Chunk.empty
    ): Deferrable[Origin, R] =
        Field[Origin, R](
            CompiledField(name = name, fieldType = fieldType, arguments = bind(arguments)),
            arguments,
            Chunk.empty,
            (json, frame) => leaf(codec, json)(using frame),
            value => codec.encode(value.asInstanceOf[V])
        )

    /** Build a nested-object selector, wrapping the child selection's result per
      * `nesting` (identity / `Maybe` / `Chunk`, arbitrarily deep). The child is a
      * bidirectional selection — a named-tuple one or a `mapInto` projection — since
      * the field re-encodes it; a `map` projection applies to the whole selection.
      */
    def obj[Origin, R <: AnyNamedTuple, A](
        name: String,
        fieldType: CompiledType,
        arguments: Chunk[Arg],
        child: Bidirectional[?, A],
        nesting: Nesting
    ): Deferrable[Origin, R] =
        val typeName = fieldType.leafType.name
        Field[Origin, R](
            CompiledField(
                name = name,
                fieldType = fieldType,
                arguments = bind(arguments),
                selections = TypenameField +: child.selections
            ),
            arguments,
            child.argEntries,
            (json, frame) => nesting.decode(json, child.decode(_)(using frame))(using frame),
            value => nesting.encode(value, a => withTypename(child.encode(a.asInstanceOf[A]), typeName))
        )
    end obj

    /** Build a local `@client` field node. The compiled field is marked
      * `client = true` (stored in and read from the normalized cache, pruned from
      * the printed document, and emitting no operation variables), and a read miss
      * (surfaced as `Json.JNull`, see `CacheBatchReader.readField`) decodes to
      * `default` instead of throwing. `selections` — derived from the value's
      * `Schema` structure by [[kyo.apollo.ClientField]] — is empty for a scalar leaf
      * (blob) or a composite object tree (`__typename` + fields) that normalizes;
      * `codec` is the whole-value codec (which already handles `Maybe`/`Chunk`
      * wrapping). `R` is the 1-ary named tuple the caller ascribes, e.g. `(count: Int)`.
      * Not [[Deferrable]]: the document prunes a `@client` field, so deferring it
      * alone would print an empty `@defer` group.
      */
    def clientField[Origin, R <: AnyNamedTuple, V](
        name: String,
        fieldType: CompiledType,
        selections: Chunk[CompiledSelection],
        codec: ScalarCodec[V],
        default: V
    ): Fields[Origin, R] =
        def decodeValue(json: Json, frame: Frame): Result[ApolloParseException, Any] =
            json match
                case Json.JNull => Result.succeed(default)
                case other      => leaf(codec, other)(using frame)
        Field[Origin, R](
            CompiledField(name = name, fieldType = fieldType, selections = selections, client = true),
            Chunk.empty,
            Chunk.empty,
            decodeValue,
            value => codec.encode(value.asInstanceOf[V])
        )
    end clientField

    /** Build an inline-fragment branch of a union (or interface) selection:
      * `... on <typeName> { <child> }`. The branch contributes a typed
      * [[CompiledFragment]] to the parent selection set and decodes to
      * `Maybe[A]` — `Present` when the object's `__typename` matches `typeName`
      * (the parent object selector always requests `__typename`), `Absent`
      * otherwise. Branches compose with `~` like any field, so a full union
      * read is `PlayableItem.onTrack(…) ~ PlayableItem.onEpisode(…)` with the
      * result `(onTrack: Maybe[…], onEpisode: Maybe[…])`, exactly one of
      * which is `Present`. `R` is the 1-ary named tuple the caller (generated
      * union selector) ascribes.
      */
    def onType[Origin, R <: AnyNamedTuple, A](
        typeName: String,
        child: Bidirectional[?, A]
    ): Fields[Origin, R] =
        new Fields[Origin, R]:
            private[api] val arity: Int = 1
            val selections: Chunk[CompiledSelection] =
                Chunk(CompiledFragment(typeName, Chunk(typeName), child.selections))
            private[api] val argEntries: Chunk[Arg] = child.argEntries
            private[api] def decodeInto(row: Map[String, Json], out: Array[Any], offset: Int)(using
                Frame
            ): Result[ApolloParseException, Unit] =
                row.get("__typename") match
                    case Some(Json.JStr(`typeName`)) => child.decode(Json.JObj(row)).map(v => out(offset) = Present(v))
                    case _ =>
                        out(offset) = Absent
                        Result.unit
            private[api] def encodeInto(value: Product, offset: Int, out: RowBuilder): Unit =
                value.productElement(offset).asInstanceOf[Maybe[A]] match
                    case Absent => ()
                    case Present(v) =>
                        withTypename(child.encode(v), typeName) match
                            case Json.JObj(fields) => discard(out ++= fields)
                            // A non-object projection cannot be spliced back into the
                            // parent row; the branch's fields simply stay absent.
                            case _ => ()

    /** Project a selection's result to `B` with `f`, decoding only (backs `map`), reusing
      * its wire selection/arguments. An exception raised by `f` is a panic of the decode.
      */
    private[api] def projectDecode[Origin, A, B](
        under: SelectionBuilder[Origin, A],
        f: A => B
    ): SelectionBuilder[Origin, B] =
        Mapped(under, f)

    /** Project a selection's result to `B` with a codec that also encodes (backs
      * `mapInto`), reusing its wire selection/arguments.
      */
    private[api] def projectCodec[Origin, B](
        under: SelectionBuilder[Origin, ?],
        codec: JsonCodec[B]
    ): Bidirectional[Origin, B] =
        MappedInto(under, codec)

    /** A raw arity-1 node whose single element is computed from the PARENT row
      * rather than one response field — the seam a masked fragment spread plugs
      * into. `compiled` is what it contributes to the wire selection (spliced
      * flat into the parent, merged with sibling duplicates downstream);
      * `decodeRow` sees the whole parent object and builds the element (a
      * fragment ref) with the decoding call's frame; `encodeValue` re-emits the
      * element's response fields so a cache write of decoded data stays lossless.
      * Arguments are harvested from `argsFrom` (the underlying child selection).
      */
    private[apollo] def rawLeaf[Origin, R <: AnyNamedTuple](
        compiled: Chunk[CompiledSelection],
        argsFrom: SelectionBuilder[?, ?],
        decodeRow: (Map[String, Json], Frame) => Result[ApolloParseException, Any],
        encodeValue: Any => Chunk[(String, Json)]
    ): Fields[Origin, R] =
        new Fields[Origin, R]:
            private[api] val arity: Int              = 1
            val selections: Chunk[CompiledSelection] = compiled
            private[api] val argEntries: Chunk[Arg]  = argsFrom.argEntries
            private[api] def decodeInto(row: Map[String, Json], out: Array[Any], offset: Int)(using
                frame: Frame
            ): Result[ApolloParseException, Unit] =
                decodeRow(row, frame).map(value => out(offset) = value)
            private[api] def encodeInto(value: Product, offset: Int, out: RowBuilder): Unit =
                discard(out ++= encodeValue(value.productElement(offset)))

    /** The empty selection for `Origin`: selects nothing, and is the neutral
      * starting point a chainable selection folds fields onto — the lambda form
      * `_.code.name` of a generated selector is applied to it, so it reads
      * `SelectionBuilder.empty ~ Country.code ~ Country.name`. It has no field to
      * defer or stream.
      */
    def empty[Origin]: Fields[Origin, Empty] = EmptySel()

    /** Combine two named-tuple selections (backs `~` with a right operand that is not a single field). */
    private[api] def combine[Origin, A <: AnyNamedTuple, B <: AnyNamedTuple](
        a: Fields[Origin, A],
        b: Fields[Origin, B]
    ): Fields[Origin, Concat[A, B]] =
        Combine[Origin, Concat[A, B]](a, b)

    /** Combine two named-tuple selections whose right operand ends in a single field (backs `~`). */
    private[api] def combineDeferrable[Origin, A <: AnyNamedTuple, B <: AnyNamedTuple](
        a: Fields[Origin, A],
        b: Deferrable[Origin, B]
    ): Deferrable[Origin, Concat[A, B]] =
        CombineDeferrable[Origin, Concat[A, B]](a, b)

    /** Wrap a child selection in a `@defer` group (backs the `defer` function). */
    private[api] def deferGroup[Origin, S <: AnyNamedTuple, R <: AnyNamedTuple](
        label: String,
        condition: Maybe[String],
        child: Fields[Origin, S]
    ): Fields[Origin, R] =
        Deferred[Origin, R](label, condition, child, single = false)
end SelectionBuilder

/** Combine two selections on the same `Origin`, concatenating their result named
  * tuples: `Country.name ~ Country.capital` is
  * `SelectionBuilder.Deferrable[Country, (name: String, capital: Maybe[String])]`.
  * The combination is [[SelectionBuilder.Deferrable]] exactly when `that` is.
  */
extension [Origin, A <: AnyNamedTuple](sb: SelectionBuilder.Fields[Origin, A])
    infix def ~[B <: AnyNamedTuple](
        that: SelectionBuilder.Fields[Origin, B]
    ): SelectionBuilder.Fields[Origin, Concat[A, B]] =
        SelectionBuilder.combine(sb, that)

    infix def ~[B <: AnyNamedTuple](
        that: SelectionBuilder.Deferrable[Origin, B]
    ): SelectionBuilder.Deferrable[Origin, Concat[A, B]] =
        SelectionBuilder.combineDeferrable(sb, that)
end extension

/** Defer an explicitly-labelled group of sibling fields — the `@defer` form for an
  * *anonymous* group with no intrinsic name. `defer("details", Country.capital ~
  * Country.currency)` exposes the group as a single `details: Maybe[(…)]` element,
  * `Absent` until the deferred payload arrives. For a single field, prefer
  * `.deferred` (no label — the field's own name is reused). A group is not a field:
  * it neither defers again nor streams.
  */
def defer[Origin, S <: AnyNamedTuple, L <: String & Singleton](
    label: L,
    child: SelectionBuilder.Fields[Origin, S],
    `if`: Maybe[String] = Absent
): SelectionBuilder.Fields[Origin, NamedTuple[L *: EmptyTuple, Maybe[S] *: EmptyTuple]] =
    SelectionBuilder.deferGroup(label, `if`, child)

/** A named-tuple selection with its LAST element's value wrapped in `Maybe` — the
  * result of deferring the last-added field via `.deferred`.
  */
type DeferLast[A <: AnyNamedTuple] =
    NamedTuple[Names[A], Tuple.Append[Tuple.Init[DropNames[A]], Maybe[Tuple.Last[DropNames[A]]]]]

extension [Origin, A <: AnyNamedTuple](sb: SelectionBuilder.Deferrable[Origin, A])
    /** Defer the last-added field of a chained/combined selection: its value becomes
      * `Maybe[…]` (absent until the deferred payload arrives) with its name unchanged,
      * and an auto-derived `@defer(label:)` — the field's response name — rides the
      * document. No string needed. Chainable: `_.a.b.deferred.c.deferred` defers `b`
      * and `c`; standalone via `~`: `Country.code ~ Country.capital.deferred`. The
      * result's last operand is a `@defer` group, so it does not defer again.
      */
    def deferred: SelectionBuilder.Fields[Origin, DeferLast[A]] =
        sb.deferLast[DeferLast[A]]

    /** Mark the last-added *list* field of a selection with `@stream(initialCount:)`:
      * the server delivers the first `initialCount` items in the initial response and
      * appends the rest over `multipart/mixed`, so the list grows across emissions. The
      * result type is UNCHANGED (`Chunk[T]` stays `Chunk[T]` — no wrapper, unlike
      * `.deferred`); the label rides the document auto-derived from the field's response
      * name. No string needed. `Country.code ~ Continent.countries(_.name).streamed(2)`.
      */
    def streamed(initialCount: Int, `if`: Maybe[String] = Absent)(using
        SelectionBuilder.EndsInList[A]
    ): SelectionBuilder.Deferrable[Origin, A] =
        sb.streamLast(initialCount, `if`)
end extension

/** Phantom `Origin` markers for the three operation roots. Universal (not
  * schema-specific), so they live in `core`; generated `Queries`/`Mutations`/
  * `Subscriptions` selector objects root their selections in these, and the
  * terminal `toQuery`/`toMutation`/`toSubscription` extensions key on them.
  */
sealed trait RootQuery
sealed trait RootMutation
sealed trait RootSubscription
