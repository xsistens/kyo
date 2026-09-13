package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import kyo.apollo.json.Json
import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.Concat
import scala.NamedTuple.DropNames
import scala.NamedTuple.Empty
import scala.NamedTuple.NamedTuple
import scala.NamedTuple.Names
import scala.collection.immutable.VectorMap

/** A type-safe, composable GraphQL selection, rooted in the object type `Origin`
  * and decoding to `A`.
  *
  * This is the inline alternative to a `.graphql` document + per-operation
  * codegen: a query is written as ordinary Scala against generated schema
  * selector objects (`Queries.country(code)(Country.name ~ Country.capital)`), so
  * the IDE autocompletes every field while you write it, and the result type is
  * the named tuple `(name: String, capital: Option[String])` — inferred, not
  * hand-written.
  *
  *   - `Origin` is a phantom scoping type (one empty marker per GraphQL object
  *     type, plus `RootQuery`/`RootMutation`/`RootSubscription` from `core`). It
  *     has no instances and no runtime footprint; it only stops a `Country`
  *     selector from being combined into a `Continent` selection.
  *   - `A` is the selection's result type — a named tuple for the composable
  *     ([[SelectionBuilder.Tuples]]) selectors, or an arbitrary type once projected
  *     with `map`/`mapInto`.
  *
  * The builder carries a **bidirectional** codec (both `decode` and `encode`),
  * because the runtime uses the operation's data codec to normalize into *and*
  * read back from the cache, not only to decode responses.
  */
sealed trait SelectionBuilder[Origin, A]:

    /** The compiled selections of this selection set, in declaration order. A
      * selection is usually a [[CompiledField]], but a defer group contributes a
      * [[CompiledFragment]] — hence the widened element type. Doubles as the
      * `rootField`-tree source.
      */
    def selections: List[CompiledSelection]

    /** Every argument in this selection subtree, in encounter order. */
    private[api] def argEntries: List[SelectionBuilder.Arg]

    /** Decode a GraphQL response object into the result `A`. */
    def decode(json: Json): A

    /** Encode a result value back into a response-shaped [[Json]] object. */
    def encode(value: A): Json
end SelectionBuilder

object SelectionBuilder:

    /** A field argument captured at the call site: its GraphQL name, its type (for
      * rendering the `$var: Type` header), and its already-encoded value.
      */
    final case class Arg(name: String, typeRef: CompiledType, value: Json)

    /** How a nested object field wraps its child selection's result. Recursive, so
      * it models arbitrarily deep list/nullable structures, e.g. `[Country!]`
      * decodes as `Nullable(Listed(Leaf))` → `Option[List[A]]`.
      */
    enum Nesting derives CanEqual:
        case Leaf
        case Nullable(inner: Nesting)
        case Listed(inner: Nesting)

        private[api] def decode(json: Json, child: Json => Any): Any = this match
            case Leaf => child(json)
            case Nullable(inner) =>
                json match
                    case Json.JNull => None
                    case other      => Some(inner.decode(other, child))
            case Listed(inner) =>
                json match
                    case Json.JArr(items) => items.toList.map(inner.decode(_, child))
                    case other            => throw SelectionDecodeException(other)

        private[api] def encode(value: Any, child: Any => Json): Json = this match
            case Leaf => child(value)
            case Nullable(inner) =>
                value.asInstanceOf[Option[Any]] match
                    case None    => Json.JNull
                    case Some(v) => inner.encode(v, child)
            case Listed(inner) =>
                Json.JArr(Chunk.from(value.asInstanceOf[List[Any]].map(inner.encode(_, child))))
    end Nesting

    /** A named-tuple-shaped selection: the composable form. Only these support `~`
      * (via the extension below) and only these are combined by [[Combine]].
      */
    sealed trait Tuples[Origin, A <: AnyNamedTuple] extends SelectionBuilder[Origin, A]:
        private[api] def arity: Int
        private[api] def decodeRaw(row: Map[String, Json]): Tuple
        private[api] def encodeRaw(value: Tuple): List[(String, Json)]

        /** Wrap this selection's LAST-added field in an anonymous `@defer` group,
          * auto-labelled by that field's response name — the runtime of `.deferred`.
          * In a chain (`~` is left-associative) the last field is the outer
          * `Combine`'s right operand, always arity 1.
          */
        private[api] def deferLast: Tuples[Origin, ? <: AnyNamedTuple]

        /** Mark this selection's LAST-added field with `@stream`, auto-labelled by that
          * field's response name — the runtime of `.streamed`. Unlike [[deferLast]],
          * the result type is unchanged (`@stream` grows a `List[T]` in place, no
          * wrapper), so the return type stays `Tuples[Origin, A]`.
          */
        private[api] def streamLast(initialCount: Int, condition: Option[String]): Tuples[Origin, A]

        final def decode(json: Json): A = json match
            case Json.JObj(row) => decodeRaw(row).asInstanceOf[A]
            case other          => throw SelectionDecodeException(other)

        final def encode(value: A): Json =
            Json.JObj(VectorMap.from(encodeRaw(value.asInstanceOf[Tuple])))
    end Tuples

    /** A single selected field: its compiled metadata + arguments + a value codec. */
    final private class Field[Origin, A <: AnyNamedTuple](
        compiled: CompiledField,
        ownArgs: List[Arg],
        childArgs: List[Arg],
        decodeValue: Json => Any,
        encodeValue: Any => Json
    ) extends Tuples[Origin, A]:
        private[api] def arity: Int             = 1
        def selections: List[CompiledSelection] = List(compiled)
        private[api] def argEntries: List[Arg]  = ownArgs ++ childArgs
        private[api] def deferLast: Tuples[Origin, ? <: AnyNamedTuple] =
            Deferred(compiled.responseName, None, this, single = true)
        private[api] def streamLast(initialCount: Int, condition: Option[String]): Tuples[Origin, A] =
            Field[Origin, A](
                compiled.copy(stream =
                    Some(StreamDirective(compiled.responseName, initialCount, condition))
                ),
                ownArgs,
                childArgs,
                decodeValue,
                encodeValue
            )
        private[api] def decodeRaw(row: Map[String, Json]): Tuple =
            Tuple1(decodeValue(row.getOrElse(compiled.responseName, Json.JNull)))
        private[api] def encodeRaw(value: Tuple): List[(String, Json)] =
            List(compiled.responseName -> encodeValue(value.productElement(0)))
    end Field

    /** Two selections combined; splits the tuple by left arity when encoding. */
    final private class Combine[Origin, A <: AnyNamedTuple, B <: AnyNamedTuple](
        left: Tuples[Origin, A],
        right: Tuples[Origin, B]
    ) extends Tuples[Origin, Concat[A, B]]:
        private[api] def arity: Int             = left.arity + right.arity
        def selections: List[CompiledSelection] = left.selections ++ right.selections
        private[api] def argEntries: List[Arg]  = left.argEntries ++ right.argEntries
        // Defer only the last-added field: recurse into the right operand, leaving
        // `left` untouched. The `B` cast is erased-safe (runtime tuples carry no names).
        private[api] def deferLast: Tuples[Origin, ? <: AnyNamedTuple] =
            Combine(left, right.deferLast.asInstanceOf[Tuples[Origin, B]])
        // Stream only the last-added field: recurse into the right operand, leave
        // `left` untouched. Type-preserving, so the `Combine` type is unchanged.
        private[api] def streamLast(
            initialCount: Int,
            condition: Option[String]
        ): Tuples[Origin, Concat[A, B]] =
            Combine(left, right.streamLast(initialCount, condition))
        private[api] def decodeRaw(row: Map[String, Json]): Tuple =
            left.decodeRaw(row) ++ right.decodeRaw(row)
        private[api] def encodeRaw(value: Tuple): List[(String, Json)] =
            val (l, r) = value.toArray.splitAt(left.arity)
            left.encodeRaw(Tuple.fromArray(l)) ++ right.encodeRaw(Tuple.fromArray(r))
    end Combine

    /** The empty selection: the neutral starting point of a chainable selection
      * (`Country.select.code.name`). Selects no fields (arity 0), so combining it
      * with `~` reduces away — `Concat[Empty, B] =:= B` at the type level and
      * `EmptyTuple ++ row =:= row` at runtime — leaving exactly the chained field.
      */
    final private class EmptySel[Origin] extends Tuples[Origin, Empty]:
        private[api] def arity: Int                                    = 0
        def selections: List[CompiledSelection]                        = Nil
        private[api] def argEntries: List[Arg]                         = Nil
        private[api] def decodeRaw(row: Map[String, Json]): Tuple      = EmptyTuple
        private[api] def encodeRaw(value: Tuple): List[(String, Json)] = Nil
        private[api] def deferLast: Tuples[Origin, ? <: AnyNamedTuple] =
            throw IllegalStateException("`.deferred` requires at least one selected field to defer")
        private[api] def streamLast(
            initialCount: Int,
            condition: Option[String]
        ): Tuples[Origin, Empty] =
            throw IllegalStateException("`.streamed` requires a selected list field to stream")
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
    final private class Deferred[Origin, S <: AnyNamedTuple, R <: AnyNamedTuple](
        label: String,
        condition: Option[String],
        child: Tuples[Origin, S],
        single: Boolean
    ) extends Tuples[Origin, R]:
        private[api] def arity: Int                                    = 1
        private[api] def deferLast: Tuples[Origin, ? <: AnyNamedTuple] = this
        private[api] def streamLast(initialCount: Int, condition: Option[String]): Tuples[Origin, R] =
            throw IllegalStateException("`.streamed` cannot be applied to a `@defer` group")
        def selections: List[CompiledSelection] =
            List(
                CompiledFragment("", Nil, child.selections, defer = Some(DeferDirective(label, condition)))
            )
        private[api] def argEntries: List[Arg] = child.argEntries
        // `single` (from `.deferred`) exposes the sole field's *value* as `Maybe[V]`;
        // a `defer` group exposes the whole child tuple as `Maybe[S]`.
        private[api] def decodeRaw(row: Map[String, Json]): Tuple =
            val present = child.selections.exists {
                case f: CompiledField => row.contains(f.responseName)
                case _                => false
            }
            if !present then Tuple1(Absent)
            else
                val decoded = child.decodeRaw(row)
                Tuple1(Present(if single then decoded.productElement(0) else decoded))
            end if
        end decodeRaw
        private[api] def encodeRaw(value: Tuple): List[(String, Json)] =
            value.productElement(0).asInstanceOf[Maybe[Any]] match
                case Present(v) =>
                    child.encodeRaw(if single then Tuple1(v) else v.asInstanceOf[Tuple])
                case Absent => Nil
    end Deferred

    /** A selection whose result has been projected to an arbitrary `B` (via `map` /
      * `mapInto`). It keeps the same wire selection and arguments; only the
      * decode/encode target differs, and it can no longer be combined with `~`.
      */
    final private class Mapped[Origin, B](
        under: SelectionBuilder[Origin, ?],
        codec: JsonCodec[B]
    ) extends SelectionBuilder[Origin, B]:
        def selections: List[CompiledSelection] = under.selections
        private[api] def argEntries: List[Arg]  = under.argEntries
        def decode(json: Json): B               = codec.decode(json)
        def encode(value: B): Json              = codec.encode(value)
    end Mapped

    /** Bind a field's captured arguments to same-named operation variables. */
    private def bind(args: List[Arg]): List[CompiledArgument] =
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
      * kyo-schema encodes `Option` fields as *absent* when `None`, but the wire
      * response (and therefore the normalized cache, whose reader treats a
      * selected-but-absent field as a cache miss) carries an explicit `null` —
      * Apollo Client normalizes the raw response JSON, so the two never diverge
      * there. A `mapInto` value is deterministically complete (absence can only
      * mean `None`), so the fill is always sound. Composite fields recurse
      * (through lists) with their sub-selections; fragments are left untouched —
      * a deferred group's fields legitimately stay absent until the payload
      * arrives, and an inline fragment's applicability depends on the concrete
      * runtime type this static walk cannot know.
      */
    private[api] def fillAbsentNullables(json: Json, selections: List[CompiledSelection]): Json =
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
    private def fillNested(value: Json, selections: List[CompiledSelection]): Json = value match
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
        arguments: List[Arg] = Nil
    ): SelectionBuilder[Origin, R] =
        Field[Origin, R](
            CompiledField(name = name, fieldType = fieldType, arguments = bind(arguments)),
            arguments,
            Nil,
            json => codec.decode(json),
            value => codec.encode(value.asInstanceOf[V])
        )

    /** Build a nested-object selector, wrapping the child selection's result per
      * `nesting` (identity / `Option` / `List`, arbitrarily deep). The child may be
      * any selection — a named-tuple one or a `map`/`mapInto` projection.
      */
    def obj[Origin, R <: AnyNamedTuple, A](
        name: String,
        fieldType: CompiledType,
        arguments: List[Arg],
        child: SelectionBuilder[?, A],
        nesting: Nesting
    ): SelectionBuilder[Origin, R] =
        val typeName = fieldType.leafType.name
        Field[Origin, R](
            CompiledField(
                name = name,
                fieldType = fieldType,
                arguments = bind(arguments),
                selections = TypenameField :: child.selections
            ),
            arguments,
            child.argEntries,
            json => nesting.decode(json, child.decode),
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
      * `codec` is the whole-value codec (which already handles `Option`/`List`
      * wrapping). `R` is the 1-ary named tuple the caller ascribes, e.g. `(count: Int)`.
      */
    def clientField[Origin, R <: AnyNamedTuple, V](
        name: String,
        fieldType: CompiledType,
        selections: List[CompiledSelection],
        codec: ScalarCodec[V],
        default: V
    ): SelectionBuilder[Origin, R] =
        Field[Origin, R](
            CompiledField(name = name, fieldType = fieldType, selections = selections, client = true),
            Nil,
            Nil,
            {
                case Json.JNull => default
                case other      => codec.decode(other)
            },
            value => codec.encode(value.asInstanceOf[V])
        )

    /** Build an inline-fragment branch of a union (or interface) selection:
      * `... on <typeName> { <child> }`. The branch contributes a typed
      * [[CompiledFragment]] to the parent selection set and decodes to
      * `Option[A]` — `Some` when the object's `__typename` matches `typeName`
      * (the parent object selector always requests `__typename`), `None`
      * otherwise. Branches compose with `~` like any field, so a full union
      * read is `PlayableItem.onTrack(…) ~ PlayableItem.onEpisode(…)` with the
      * result `(onTrack: Option[…], onEpisode: Option[…])`, exactly one of
      * which is `Some`. `R` is the 1-ary named tuple the caller (generated
      * union selector) ascribes.
      */
    def onType[Origin, R <: AnyNamedTuple, A](
        typeName: String,
        child: SelectionBuilder[?, A]
    ): SelectionBuilder[Origin, R] =
        new Tuples[Origin, R]:
            private[api] def arity: Int = 1
            def selections: List[CompiledSelection] =
                List(CompiledFragment(typeName, List(typeName), child.selections))
            private[api] def argEntries: List[Arg] = child.argEntries
            private[api] def decodeRaw(row: Map[String, Json]): Tuple =
                row.get("__typename") match
                    case Some(Json.JStr(`typeName`)) => Tuple1(Some(child.decode(Json.JObj(row))))
                    case _                           => Tuple1(None)
            private[api] def encodeRaw(value: Tuple): List[(String, Json)] =
                value.productElement(0).asInstanceOf[Option[A]] match
                    case None => Nil
                    case Some(v) =>
                        withTypename(child.encode(v), typeName) match
                            case Json.JObj(fields) => fields.toList
                            // A non-object projection cannot be spliced back into the
                            // parent row; the branch's fields simply stay absent.
                            case _ => Nil
            private[api] def deferLast: Tuples[Origin, ? <: AnyNamedTuple] =
                throw IllegalStateException(
                    "`.deferred` cannot be applied to an inline-fragment branch"
                )
            private[api] def streamLast(initialCount: Int, condition: Option[String]): Tuples[Origin, R] =
                throw IllegalStateException(
                    "`.streamed` cannot be applied to an inline-fragment branch"
                )

    /** Project a selection's result to `B`, reusing its wire selection/arguments. */
    private[api] def project[Origin, B](
        under: SelectionBuilder[Origin, ?],
        codec: JsonCodec[B]
    ): SelectionBuilder[Origin, B] =
        Mapped(under, codec)

    /** A raw arity-1 node whose single element is computed from the PARENT row
      * rather than one response field — the seam a masked fragment spread plugs
      * into. `compiled` is what it contributes to the wire selection (spliced
      * flat into the parent, merged with sibling duplicates downstream);
      * `decodeRow` sees the whole parent object and builds the element (a
      * fragment ref); `encodeValue` re-emits the element's response fields so a
      * cache write of decoded data stays lossless. Arguments are harvested from
      * `argsFrom` (the underlying child selection).
      */
    private[apollo] def rawLeaf[Origin, R <: AnyNamedTuple](
        compiled: List[CompiledSelection],
        argsFrom: SelectionBuilder[?, ?],
        decodeRow: Map[String, Json] => Any,
        encodeValue: Any => List[(String, Json)]
    ): SelectionBuilder[Origin, R] =
        new Tuples[Origin, R]:
            private[api] def arity: Int             = 1
            def selections: List[CompiledSelection] = compiled
            private[api] def argEntries: List[Arg]  = argsFrom.argEntries
            private[api] def decodeRaw(row: Map[String, Json]): Tuple =
                Tuple1(decodeRow(row))
            private[api] def encodeRaw(value: Tuple): List[(String, Json)] =
                encodeValue(value.productElement(0))
            private[api] def deferLast: Tuples[Origin, ? <: AnyNamedTuple] =
                throw IllegalStateException("`.deferred` cannot be applied to a fragment spread")
            private[api] def streamLast(initialCount: Int, condition: Option[String]): Tuples[Origin, R] =
                throw IllegalStateException("`.streamed` cannot be applied to a fragment spread")

    /** The empty selection for `Origin`: selects nothing, and is the neutral
      * starting point a chainable selection folds fields onto — the generated
      * `Country.select` returns this, so `Country.select.code.name` is
      * `SelectionBuilder.empty ~ Country.code ~ Country.name`.
      */
    def empty[Origin]: SelectionBuilder[Origin, Empty] = EmptySel()

    /** Combine two named-tuple selections (backs the `~` extension). */
    private[api] def combine[Origin, A <: AnyNamedTuple, B <: AnyNamedTuple](
        a: SelectionBuilder[Origin, A],
        b: SelectionBuilder[Origin, B]
    ): SelectionBuilder[Origin, Concat[A, B]] =
        Combine(a.asInstanceOf[Tuples[Origin, A]], b.asInstanceOf[Tuples[Origin, B]])

    /** Wrap a child selection in a `@defer` group (backs the `defer` function). */
    private[api] def deferGroup[Origin, S <: AnyNamedTuple, R <: AnyNamedTuple](
        label: String,
        condition: Option[String],
        child: SelectionBuilder[Origin, S]
    ): SelectionBuilder[Origin, R] =
        Deferred(label, condition, child.asInstanceOf[Tuples[Origin, S]], single = false)
end SelectionBuilder

/** Combine two selections on the same `Origin`, concatenating their result named
  * tuples: `Country.name ~ Country.capital` is
  * `SelectionBuilder[Country, (name: String, capital: Option[String])]`.
  */
extension [Origin, A <: AnyNamedTuple](sb: SelectionBuilder[Origin, A])
    infix def ~[B <: AnyNamedTuple](
        that: SelectionBuilder[Origin, B]
    ): SelectionBuilder[Origin, Concat[A, B]] =
        SelectionBuilder.combine(sb, that)
end extension

/** Defer an explicitly-labelled group of sibling fields — the `@defer` form for an
  * *anonymous* group with no intrinsic name. `defer("details", Country.capital ~
  * Country.currency)` exposes the group as a single `details: Maybe[(…)]` element,
  * `Absent` until the deferred payload arrives. For a single field, prefer
  * `.deferred` (no label — the field's own name is reused).
  */
def defer[Origin, S <: AnyNamedTuple, L <: String & Singleton](
    label: L,
    child: SelectionBuilder[Origin, S],
    `if`: Option[String] = None
): SelectionBuilder[Origin, NamedTuple[L *: EmptyTuple, Maybe[S] *: EmptyTuple]] =
    SelectionBuilder.deferGroup(label, `if`, child)

/** A named-tuple selection with its LAST element's value wrapped in `Maybe` — the
  * result of deferring the last-added field via `.deferred`.
  */
type DeferLast[A <: AnyNamedTuple] =
    NamedTuple[Names[A], Tuple.Append[Tuple.Init[DropNames[A]], Maybe[Tuple.Last[DropNames[A]]]]]

/** Defer the last-added field of a chained/combined selection: its value becomes
  * `Maybe[…]` (absent until the deferred payload arrives) with its name unchanged,
  * and an auto-derived `@defer(label:)` — the field's response name — rides the
  * document. No string needed. Chainable: `_.a.b.deferred.c.deferred` defers `b`
  * and `c`; standalone via `~`: `Country.code ~ Country.capital.deferred`. Applies
  * to field/chain selections (not `map`/`mapInto` projections).
  */
extension [Origin, A <: AnyNamedTuple](sb: SelectionBuilder[Origin, A])
    def deferred: SelectionBuilder[Origin, DeferLast[A]] =
        sb.asInstanceOf[SelectionBuilder.Tuples[Origin, A]]
            .deferLast
            .asInstanceOf[SelectionBuilder[Origin, DeferLast[A]]]
end extension

/** Mark the last-added *list* field of a selection with `@stream(initialCount:)`:
  * the server delivers the first `initialCount` items in the initial response and
  * appends the rest over `multipart/mixed`, so the list grows across emissions. The
  * result type is UNCHANGED (`List[T]` stays `List[T]` — no wrapper, unlike
  * `.deferred`); the label rides the document auto-derived from the field's response
  * name. No string needed. `Country.code ~ Continent.countries(_.name).streamed(2)`.
  * Applies to field/chain selections (not `map`/`mapInto` projections).
  */
extension [Origin, A <: AnyNamedTuple](sb: SelectionBuilder[Origin, A])
    def streamed(initialCount: Int, `if`: Option[String] = None): SelectionBuilder[Origin, A] =
        sb.asInstanceOf[SelectionBuilder.Tuples[Origin, A]].streamLast(initialCount, `if`)

/** Phantom `Origin` markers for the three operation roots. Universal (not
  * schema-specific), so they live in `core`; generated `Queries`/`Mutations`/
  * `Subscriptions` selector objects root their selections in these, and the
  * terminal `toQuery`/`toMutation`/`toSubscription` extensions key on them.
  */
sealed trait RootQuery
sealed trait RootMutation
sealed trait RootSubscription

/** Raised when a selection expects a GraphQL object (or list) but the response
  * value has a different JSON shape.
  */
final class SelectionDecodeException(got: Json)
    extends RuntimeException(s"Expected a GraphQL object but got: ${got.render}")
