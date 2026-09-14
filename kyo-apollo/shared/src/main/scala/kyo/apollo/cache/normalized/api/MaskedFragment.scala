package kyo.apollo.cache.normalized.api

import kyo.Chunk
import kyo.Frame
import kyo.Maybe
import kyo.Result
import kyo.apollo.api.*
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.Json
import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.NamedTuple
import scala.collection.immutable.VectorMap

/** A colocated, **masked** fragment on an entity type — the unit a component
  * declares its own data dependency with (Apollo Client 4's fragment
  * colocation + data masking, reached without a fragment registry because
  * fragments here are values you import).
  *
  * Declared once next to the component that renders it:
  *
  * {{{
  * object CountryCard:
  *     val fields = Fragment.entity[Country](_.name.capital)
  *
  *     def view(ref: fields.Ref)(using ApolloClient, Frame) =
  *         Apollo.fragment(ref).map(render)
  * }}}
  *
  * Spreading it into a parent selection contributes exactly ONE named-tuple
  * element — the [[Ref]] — regardless of how many fields the fragment selects,
  * and the parent cannot reach those fields through it: the ref's contents are
  * `private[apollo]`, and only `Apollo.fragment` opens them. When a parent
  * legitimately needs a field the child also shows, it selects that field
  * itself (`Country.name ~ CountryCard.fields.spread`) — fetched once (the
  * duplicate merges), but the parent's dependency is explicit rather than
  * borrowed. That is data masking: a child adding a field to its fragment can
  * never silently become load-bearing for a parent.
  *
  * The spread also forces the entity's `__typename` and [[CacheIdentity]] key
  * fields into the document, so the ref always carries what a key generator
  * needs. The ref computes no key itself: the store resolves it with
  * `ApolloStore.keyOf(ref.typeName, ref.raw)`, through the very generator that
  * normalized the response, so the ref and the record cannot disagree about
  * where the entity lives. Publicly a ref is opaque — no field values, no
  * `Product` view — and equal to another ref of the same fragment exactly when
  * both captured the same response slice.
  *
  * An entity spread never produces a path-keyed ref: requiring a declared
  * [[CacheIdentity]] is what rules out capturing a positional
  * `QUERY_ROOT.xs.3`-style key that a list insertion would silently repoint at
  * a different entity. An object with no independent identity belongs in
  * [[EmbeddedFragment]] instead, whose ref carries the value and whose updates
  * flow through the parent's reactivity.
  */
final class EntityFragment[Origin, D <: AnyNamedTuple] private[apollo] (
    val fragmentName: String,
    private[apollo] val typeName: String,
    private[apollo] val identity: CacheIdentity[Origin],
    private[apollo] val selection: SelectionBuilder.Fields[Origin, D],
    private[apollo] val cacheFragment: Fragment[D]
):
    outer =>

    /** The masked handle a spread decodes to. Publicly it is opaque — an
      * `equals`/`hashCode` over the fragment and the captured slice, and a
      * `toString` naming only the fragment — while the response fields it
      * captured ride along `private[apollo]` so a cache write of decoded data
      * stays lossless and `Apollo.fragment` can seed without a cache round-trip.
      *
      * A plain `final class`, not a case class: a case class is a `Product`, and
      * `productIterator` would hand the masked fields out to any generic printer.
      * It carries no cache key either — the store computes that from [[typeName]]
      * and [[raw]] with its own generator (`ApolloStore.keyOf`).
      *
      * Path-dependent (`CountryCard.fields.Ref`), so a ref cannot be handed to a
      * different fragment whose field set merely overlaps — the component's
      * signature pins exactly the fragment it declared.
      *
      * @param raw      the response fields this fragment selected (plus
      *                 `__typename` and the identity's key fields)
      * @param typeName the entity's concrete type: the response `__typename`,
      *                 the fragment's declared type when the response has none
      */
    final class Ref private[apollo] (
        private[apollo] val raw: VectorMap[String, Json],
        private[apollo] val typeName: String
    ):
        private[apollo] def definition: EntityFragment[Origin, D] = outer

        /** The fragment's fields decoded from the captured response slice; a slice
          * missing or mistyping one of them is a failure.
          */
        private[apollo] def decoded(using Frame): Result[ApolloParseException, D] = selection.decode(Json.JObj(raw))

        override def equals(other: Any): Boolean = other match
            case that: EntityFragment[?, ?]#Ref => (that.definition eq outer) && that.raw == raw
            case _                              => false

        override def hashCode: Int = raw.hashCode * 31 + fragmentName.hashCode

        /** Masked: never prints field values. */
        override def toString: String = s"Ref($fragmentName)"
    end Ref

    /** What a spread contributes to the wire selection: `__typename` and the
      * identity's key fields (so the ref is always constructible) plus the
      * fragment's own fields. Spliced flat; duplicates with sibling selections
      * merge downstream (FieldCollector by response name, the server per GraphQL
      * §5.3.2).
      */
    private[apollo] val spreadSelections: Chunk[CompiledSelection] =
        (MaskedFragment.TypenameField +: identity.keyFields) ++ selection.selections

    /** The response names this fragment's ref captures from the parent row. */
    private val ownResponseNames: Set[String] =
        ((MaskedFragment.TypenameField.responseName +: identity.keyFieldNames) ++
            selection.selections.collect { case f: CompiledField => f.responseName }).toSet

    /** Build a [[Ref]] from the parent response object. The ref's type name is the
      * response `__typename` — not the declared type — so a fragment on an
      * interface is keyed by the concrete runtime type, matching what the
      * normalizer stored.
      *
      * A row missing an identity key field (or carrying a non-scalar one) is a
      * failure naming the fragment, the type and the missing fields — never the
      * row's values, which are the masked data.
      */
    private[apollo] def refFromRow(row: Map[String, Json])(using Frame): Result[ApolloParseException, Ref] =
        val tn = row.get("__typename") match
            case Some(Json.JStr(t)) => t
            case _                  => typeName
        val missing = identity.keyFieldNames.filter(field => Maybe.fromOption(row.get(field)).flatMap(CacheKey.scalarString).isEmpty)
        if missing.nonEmpty then
            Result.fail(ApolloParseException(
                Json.JObj(VectorMap.from(missing.map(_ -> Json.JNull))),
                s"scalar values for the key field(s) ${missing.mkString(", ")} of $tn in masked fragment $fragmentName"
            ))
        else Result.succeed(Ref(VectorMap.from(row.view.filterKeys(ownResponseNames).toSeq), tn))
        end if
    end refFromRow

    /** [[spread]] with the named-tuple label supplied explicitly — the non-macro
      * form (`fields.spreadAs["countryCard"]`), and what the macro expands to.
      */
    def spreadAs[L <: String]: SelectionBuilder.Fields[Origin, NamedTuple[L *: EmptyTuple, Ref *: EmptyTuple]] =
        SelectionBuilder.rawLeaf(
            spreadSelections,
            selection,
            (row, frame) => refFromRow(row)(using frame),
            value => Chunk.from(value.asInstanceOf[Ref].raw)
        )

end EntityFragment

/** Spread an entity fragment into a parent selection as one masked element.
  *
  * The element's label is derived at compile time from the reference the caller
  * wrote — `CountryCard.fields.spread` contributes `(countryCard: fields.Ref)` —
  * so no string names it and two different fragments can never collide on a
  * label by accident. Use [[EntityFragment.spreadAs]] to name it explicitly.
  * (An extension with an `inline` receiver rather than a method: the macro needs
  * the caller's own prefix term to read the declaration site off, and `this`
  * inside an inline method only ever shows it a synthetic proxy.)
  */
extension [Origin, D <: AnyNamedTuple](inline self: EntityFragment[Origin, D])
    transparent inline def spread: SelectionBuilder.Fields[Origin, ? <: AnyNamedTuple] =
        ${ MaskedFragment.spreadEntityImpl[Origin, D]('self) }
end extension

/** A colocated, masked fragment on an object with **no independent identity**
  * (a `PageInfo`, a value object): the ref carries the decoded value instead of
  * a cache key, and its updates flow through the parent's reactivity — the
  * semantically correct behaviour for an object that only exists inside its
  * parent, and something Apollo does not support at all. See [[EntityFragment]]
  * for the masking contract, which is identical.
  */
final class EmbeddedFragment[Origin, D <: AnyNamedTuple] private[apollo] (
    val fragmentName: String,
    private[apollo] val typeName: String,
    private[apollo] val selection: SelectionBuilder.Fields[Origin, D]
):
    outer =>

    /** The masked handle: carries the captured response slice, opens only for
      * `Apollo.fragment`. No cache key — the object has no identity. A plain
      * `final class` for the same reason as [[EntityFragment.Ref]]: no `Product`
      * view onto the masked fields.
      */
    final class Ref private[apollo] (
        private[apollo] val raw: VectorMap[String, Json]
    ):
        private[apollo] def definition: EmbeddedFragment[Origin, D] = outer

        /** The fragment's fields decoded from the captured response slice; a slice
          * missing or mistyping one of them is a failure.
          */
        private[apollo] def value(using Frame): Result[ApolloParseException, D] = selection.decode(Json.JObj(raw))

        override def equals(other: Any): Boolean = other match
            case that: EmbeddedFragment[?, ?]#Ref => (that.definition eq outer) && that.raw == raw
            case _                                => false

        override def hashCode: Int = raw.hashCode * 31 + fragmentName.hashCode

        /** Masked: never prints field values. */
        override def toString: String = s"Ref($fragmentName)"
    end Ref

    private val ownResponseNames: Set[String] =
        selection.selections.collect { case f: CompiledField => f.responseName }.toSet

    private[apollo] def refFromRow(row: Map[String, Json]): Ref =
        Ref(VectorMap.from(row.view.filterKeys(ownResponseNames).toSeq))

    /** See [[EntityFragment.spreadAs]]. */
    def spreadAs[L <: String]: SelectionBuilder.Fields[Origin, NamedTuple[L *: EmptyTuple, Ref *: EmptyTuple]] =
        SelectionBuilder.rawLeaf(
            selection.selections,
            selection,
            (row, _) => Result.succeed(refFromRow(row)),
            value => Chunk.from(value.asInstanceOf[Ref].raw)
        )

end EmbeddedFragment

/** See the [[spread]] extension on [[EntityFragment]] — same contract, no key. */
extension [Origin, D <: AnyNamedTuple](inline self: EmbeddedFragment[Origin, D])
    transparent inline def spread: SelectionBuilder.Fields[Origin, ? <: AnyNamedTuple] =
        ${ MaskedFragment.spreadEmbeddedImpl[Origin, D]('self) }
end extension

private[apollo] object MaskedFragment:
    import scala.quoted.*

    /** The implicit `__typename` a spread forces into the parent selection. */
    val TypenameField: CompiledField =
        CompiledField("__typename", CompiledNamedType("String").notNull)

    def spreadEntityImpl[Origin: Type, D <: AnyNamedTuple: Type](
        self: Expr[EntityFragment[Origin, D]]
    )(using Quotes): Expr[SelectionBuilder.Fields[Origin, ? <: AnyNamedTuple]] =
        import quotes.reflect.*
        spreadLabel(self.asTerm) match
            case '[type l <: String; l] => '{ $self.spreadAs[l] }
    end spreadEntityImpl

    def spreadEmbeddedImpl[Origin: Type, D <: AnyNamedTuple: Type](
        self: Expr[EmbeddedFragment[Origin, D]]
    )(using Quotes): Expr[SelectionBuilder.Fields[Origin, ? <: AnyNamedTuple]] =
        import quotes.reflect.*
        spreadLabel(self.asTerm) match
            case '[type l <: String; l] => '{ $self.spreadAs[l] }
    end spreadEmbeddedImpl

    /** The spread's named-tuple label as a constant type, derived from the
      * declaration site: for the idiomatic `CountryCard.fields.spread` the label
      * is the enclosing object's name decapitalized (`countryCard`); for a bare
      * local `val frag = ...; frag.spread` it is the val's own name. No string is
      * ever written, so labels are unique by construction wherever declarations
      * are.
      */
    private def spreadLabel(using Quotes)(term: quotes.reflect.Term): Type[?] =
        import quotes.reflect.*
        // The label comes from the fragment val's DECLARING object (`val fields`
        // inside `object CountryCard` → `countryCard`) — read off the SYMBOL's
        // owner, not the reference syntax: a stable path collapses to a bare
        // `Ident(fields)` when the enclosing object's prefix is elided (sibling
        // references inside the same object), and the syntactic qualifier is then
        // simply gone. A fragment held in a local val has a method owner instead;
        // its own name is the honest label there.
        def nameOf(t: Term): String = t match
            case Inlined(_, _, inner) => nameOf(inner)
            case Typed(inner, _)      => nameOf(inner)
            case t =>
                val sym = t.symbol
                if sym.exists && sym.owner.exists && sym.owner.flags.is(Flags.Module) then
                    sym.owner.name
                else if sym.exists then sym.name
                else "fragment"
                end if
        val base  = nameOf(term).stripSuffix("$")
        val label = if base.isEmpty then "fragment" else base.head.toLower.toString + base.tail
        ConstantType(StringConstant(label)).asType
    end spreadLabel
end MaskedFragment
