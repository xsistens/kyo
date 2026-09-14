package kyo.apollo.api

import kyo.Structure
import kyo.Tag
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Derives a local `@client` field's cache SHAPE from the value type's
  * `kyo.Schema` structure, so [[kyo.apollo.ClientField.create]] normalizes an
  * object value like a server object field (a [[Structure.Type.Product]] becomes
  * its own cache record, shared iff a `TypePolicy` + key field match) while a
  * scalar/enum stays an inline blob — with no per-field API choice.
  *
  * A **polymorphic Sum** (a `sealed trait` / ADT `enum` whose cases carry data) is
  * treated like a GraphQL interface/union: each variant becomes an inline
  * [[CompiledFragment]], and a variant that carries an id + a matching `TypePolicy`
  * normalizes into its own shared `Variant:id` record. kyo-schema encodes a variant
  * as `{"Circle": {…fields}}`; the codec flattens that to `{__typename: "Circle",
  * …fields}` on the way into the cache (so the normalizer keys it) and re-wraps it
  * on the way out. A *param-less* enum stays a leaf blob (a string).
  *
  * **Recursive types** (`Node(children: List[Node])`, `A → B → A`, a Sum whose
  * variant refers back to it) normalize one level deep: kyo-schema holds
  * `Field.fieldType` by-name precisely so such graphs construct, and every walk
  * here carries the `Tag`s of the Products/Sums on its current path. A type seen
  * again on that path is a leaf — its value is stored as a blob, in plain kyo wire,
  * and the whole-value codec still round-trips it. [[selections]],
  * [[injectTypenames]] and [[toKyoWire]] cut at the same place, so the selection
  * tree and the encoded value always agree on where normalization stops.
  *
  * The value's `encode`/`decode` are the whole-value `SchemaJson` codec (which
  * already handles `Option`/`List` wrapping); this object supplies the selection
  * tree the normalizer/reader walk, plus the `__typename` (un)wrapping.
  */
private[apollo] object ClientFieldStructure:

    private val TypenameField: CompiledField =
        CompiledField("__typename", CompiledNamedType("String").notNull)

    /** The Products/Sums on the current descent path, keyed by their `Tag` (the
      * exact type incl. type arguments; `Structure.Type.name` is only the simple
      * class name). Re-entering one of them is the recursion cut.
      */
    private type Visited = Set[Tag[Any]]

    /** A polymorphic Sum (ADT with data-carrying variants) — normalized per variant;
      * a param-less enum (only `enumValues`) is NOT (it stays a leaf string blob).
      */
    private def isPolymorphic(s: Structure.Type.Sum): Boolean =
        s.enumValues.isEmpty && s.variants.nonEmpty &&
            s.variants.forall(_.variantType.isInstanceOf[Structure.Type.Product])

    /** The fields of a Product as selections (no leading `__typename`). */
    private def productFields(p: Structure.Type.Product, visited: Visited): List[CompiledSelection] =
        p.fields.toList.map { f =>
            CompiledField(
                name = f.name,
                fieldType = CompiledNamedType(leafName(f.fieldType)),
                selections = selections(f.fieldType, visited)
            )
        }

    /** The cache selection tree for a value of structure `t`. A `Product` → a
      * composite object (`__typename` + its fields); a polymorphic `Sum` →
      * `__typename` + one inline fragment per variant; `Optional`/`Collection` unwrap
      * to the inner shape (the codec handles the wrapper); a param-less enum /
      * `Primitive` / `Mapping` / `Open` → `Nil` (leaf, stored as a blob). A
      * `Product`/`Sum` re-entered on its own path (a recursive type) → `Nil` as well.
      */
    def selections(t: Structure.Type): List[CompiledSelection] = selections(t, Set.empty)

    private def selections(t: Structure.Type, visited: Visited): List[CompiledSelection] = t match
        case p: Structure.Type.Product if !visited.contains(p.tag) =>
            TypenameField :: productFields(p, visited + p.tag)
        case s: Structure.Type.Sum if isPolymorphic(s) && !visited.contains(s.tag) =>
            TypenameField :: s.variants.toList.map { v =>
                val fields = v.variantType match
                    case p: Structure.Type.Product => productFields(p, visited + s.tag + p.tag)
                    case _                         => Nil
                CompiledFragment(typeCondition = v.name, possibleTypes = List(v.name), selections = fields)
            }
        case o: Structure.Type.Optional   => selections(o.innerType, visited)
        case c: Structure.Type.Collection => selections(c.elementType, visited)
        case _                            => Nil

    /** The leaf type name for `t` (a `Product`/`Sum`/`Primitive` name, unwrapping
      * `Option`/`List`) — the `CompiledType` name the derived field carries.
      */
    def leafName(t: Structure.Type): String = t match
        case o: Structure.Type.Optional   => leafName(o.innerType)
        case c: Structure.Type.Collection => leafName(c.elementType)
        case other                        => other.name

    /** Inject `__typename` at every Product/variant level of an encoded value, so the
      * normalizer can key each object. For a **Product**: prepend `__typename` and
      * fill every declared field (kyo omits `None` optionals — an absent field would
      * make a later read cache-miss, so it becomes explicit `null`). For a
      * polymorphic **Sum** (kyo `{"Circle": {…}}`): flatten to `{__typename:"Circle",
      * …fields}` so it stores as a normal, keyable object. `Option`/`List` recurse.
      * Below the recursion cut of [[selections]] the value stays as encoded.
      */
    def injectTypenames(json: Json, t: Structure.Type): Json = injectTypenames(json, t, Set.empty)

    private def injectTypenames(json: Json, t: Structure.Type, visited: Visited): Json = t match
        case p: Structure.Type.Product if !visited.contains(p.tag) =>
            json match
                case Json.JObj(fields) => Json.JObj(fillProduct(p, fields, p.name, visited + p.tag))
                case other             => other
        case s: Structure.Type.Sum if isPolymorphic(s) && !visited.contains(s.tag) =>
            json match
                case Json.JObj(fields) if fields.size == 1 =>
                    val (variantName, inner) = fields.head
                    s.variants.find(_.name == variantName).map(_.variantType) match
                        case Some(p: Structure.Type.Product) =>
                            inner match
                                case Json.JObj(innerFields) =>
                                    Json.JObj(fillProduct(p, innerFields, variantName, visited + s.tag + p.tag))
                                case _ => json
                        case _ => json
                    end match
                case other => other
        case o: Structure.Type.Optional =>
            json match
                case Json.JNull => Json.JNull
                case other      => injectTypenames(other, o.innerType, visited)
        case c: Structure.Type.Collection =>
            json match
                case Json.JArr(items) => Json.JArr(items.map(injectTypenames(_, c.elementType, visited)))
                case other            => other
        case _ => json

    /** `{__typename: typeName, <every declared field, absent → null, recursed>}`. */
    private def fillProduct(
        p: Structure.Type.Product,
        fields: Map[String, Json],
        typeName: String,
        visited: Visited
    ): VectorMap[String, Json] =
        val base = VectorMap("__typename" -> Json.JStr(typeName))
        p.fields.foldLeft(base) { (acc, f) =>
            acc.updated(f.name, fields.get(f.name).fold(Json.JNull)(v => injectTypenames(v, f.fieldType, visited)))
        }
    end fillProduct

    /** Invert [[injectTypenames]] on the read side: turn the cache's flat
      * `{__typename: "Circle", …fields}` back into kyo's `{"Circle": {…fields}}` so
      * `SchemaJson.decode` accepts it, recursing through Product/Option/List. Products
      * drop `__typename` (kyo tolerates its absence). A no-op for leaves, and for
      * everything below the recursion cut of [[selections]] (already kyo wire).
      */
    def toKyoWire(json: Json, t: Structure.Type): Json = toKyoWire(json, t, Set.empty)

    private def toKyoWire(json: Json, t: Structure.Type, visited: Visited): Json = t match
        case p: Structure.Type.Product if !visited.contains(p.tag) =>
            json match
                case Json.JObj(fields) => Json.JObj(kyoFields(p, fields, visited + p.tag))
                case other             => other
        case s: Structure.Type.Sum if isPolymorphic(s) && !visited.contains(s.tag) =>
            json match
                case Json.JObj(fields) =>
                    fields.get("__typename") match
                        case Some(Json.JStr(variantName)) =>
                            s.variants.find(_.name == variantName).map(_.variantType) match
                                case Some(p: Structure.Type.Product) =>
                                    Json.JObj(VectorMap(variantName -> Json.JObj(kyoFields(p, fields, visited + s.tag + p.tag))))
                                case _ => json
                        case _ => json
                case other => other
        case o: Structure.Type.Optional =>
            json match
                case Json.JNull => Json.JNull
                case other      => toKyoWire(other, o.innerType, visited)
        case c: Structure.Type.Collection =>
            json match
                case Json.JArr(items) => Json.JArr(items.map(toKyoWire(_, c.elementType, visited)))
                case other            => other
        case _ => json

    /** The declared fields of `p` from `fields` (recursed, `__typename` dropped). */
    private def kyoFields(
        p: Structure.Type.Product,
        fields: Map[String, Json],
        visited: Visited
    ): VectorMap[String, Json] =
        p.fields.foldLeft(VectorMap.empty[String, Json]) { (acc, f) =>
            fields.get(f.name).fold(acc)(v => acc.updated(f.name, toKyoWire(v, f.fieldType, visited)))
        }
end ClientFieldStructure
