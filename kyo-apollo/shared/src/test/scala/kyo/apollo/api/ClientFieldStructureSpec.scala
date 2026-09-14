package kyo.apollo.api

import kyo.Chunk
import kyo.Schema
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson

// The fixtures live at file level: kyo-schema builds a recursive type graph by
// holding `Field.fieldType` by-name, which needs the derived `Schema` to be a
// stable top-level given rather than a class member initialized mid-derivation.

/** Direct self-reference: `Node → List[Node]`. */
case class CfsNode(name: String, children: List[CfsNode]) derives CanEqual, Schema

/** Indirect recursion through two products: `A → Option[B] → List[A]`. */
case class CfsA(name: String, b: Option[CfsB]) derives CanEqual, Schema
case class CfsB(name: String, as: List[CfsA]) derives CanEqual, Schema

/** Recursion through a polymorphic Sum: a variant refers back to the Sum. */
sealed trait CfsTree derives CanEqual, Schema
case class CfsLeaf(value: Int)                      extends CfsTree
case class CfsBranch(left: CfsTree, right: CfsTree) extends CfsTree

/** [[ClientFieldStructure]] derives a `@client` field's cache shape from a
  * `Schema` structure. kyo-schema deliberately supports recursive type graphs
  * (`Field.fieldType` is by-name), so the structural descent must terminate on
  * them: a Product (or polymorphic Sum) already on the current path is a leaf
  * blob — normalization stops there, the whole-value codec still carries it.
  * The typename injection and its inverse cut at the same place, so the blob
  * holds plain kyo wire in both directions.
  */
class ClientFieldStructureSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def fieldNamed(sels: Chunk[CompiledSelection], name: String): CompiledField =
        sels.collectFirst { case f: CompiledField if f.name == name => f }
            .getOrElse(throw new NoSuchElementException(s"no field '$name' in $sels"))

    private def names(sels: Chunk[CompiledSelection]): Chunk[String] =
        sels.collect { case f: CompiledField => f.name }

    "ClientFieldStructure.selections" - {

        "a self-referential product terminates and normalizes one level" in {
            val sels = ClientFieldStructure.selections(summon[Schema[CfsNode]].structure)
            assert(names(sels) == Chunk("__typename", "name", "children"))
            val children = fieldNamed(sels, "children")
            assert(children.fieldType == CompiledNamedType("CfsNode"))
            assert(children.selections.isEmpty, "the recursive tail is a leaf blob")
        }

        "mutual recursion A -> B -> A cuts where A re-enters" in {
            val sels = ClientFieldStructure.selections(summon[Schema[CfsA]].structure)
            assert(names(sels) == Chunk("__typename", "name", "b"))
            val b = fieldNamed(sels, "b")
            // B is new on this path: it normalizes with its own fields ...
            assert(names(b.selections) == Chunk("__typename", "name", "as"))
            // ... and its `as: List[A]` re-enters A, so it is the leaf blob.
            assert(fieldNamed(b.selections, "as").selections.isEmpty)
        }

        "recursion through a polymorphic Sum cuts at the Sum's re-entry" in {
            val sels = ClientFieldStructure.selections(summon[Schema[CfsTree]].structure)
            assert(names(sels) == Chunk("__typename"))
            val fragments = sels.collect { case f: CompiledFragment => f }
            assert(fragments.map(_.typeCondition) == Chunk("CfsLeaf", "CfsBranch"))
            val branch = fragments.find(_.typeCondition == "CfsBranch").get
            assert(names(branch.selections) == Chunk("left", "right"))
            assert(branch.selections.collect { case f: CompiledField => f.selections } == Chunk(Chunk.empty, Chunk.empty))
        }
    }

    "typename injection and its inverse" - {

        "stop at the same cut, so the blob is plain kyo wire and round-trips" in {
            val structure = summon[Schema[CfsNode]].structure
            val value     = CfsNode("root", List(CfsNode("child", List(CfsNode("grandchild", Nil)))))
            val wire      = SchemaJson.encode(value)
            val injected  = ClientFieldStructure.injectTypenames(wire, structure)

            val Json.JObj(top) = injected: @unchecked
            assert(top.get("__typename") == Some(Json.JStr("CfsNode")))
            // Below the cut, `children` carries the encoded value untouched.
            val Some(Json.JArr(children)) = top.get("children"): @unchecked
            val Json.JObj(child)          = children.head: @unchecked
            assert(!child.contains("__typename"), s"blob must not carry __typename: $child")
            assert(top.get("children") == wire.asInstanceOf[Json.JObj].fields.get("children"))

            assert(ClientFieldStructure.toKyoWire(injected, structure) == wire)
            assert(SchemaJson.decode[CfsNode](ClientFieldStructure.toKyoWire(injected, structure)) == value)
        }
    }
end ClientFieldStructureSpec
