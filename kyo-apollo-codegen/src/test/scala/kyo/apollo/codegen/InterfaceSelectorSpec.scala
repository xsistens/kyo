package kyo.apollo.codegen

/** Guards interface emission: a GraphQL `interface` gets a selector source like a
  * union's (marker trait, `TypeName`, `on<Implementor>` branches in value and
  * lambda form, chainable) plus direct selectors for the fields it declares, and a
  * field whose type is an interface is an object selector taking a nested
  * selection. Before, interfaces were never read and such a field fell through
  * to a `String` scalar without a selection set. Compiling the output against the
  * client is `kyo-apollo-codegen-it`'s job; this spec pins the emitted shape.
  */
class InterfaceSelectorSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val schema = SchemaLoader.fromString(
        """interface Node { id: ID! }
      |interface Named { name: String! }
      |type User implements Node & Named { id: ID! name: String! }
      |type Post implements Node { id: ID! title: String! }
      |type Query { node(id: ID!): Node, nodes: [Node!]! }
      |""".stripMargin
    )

    private def sources = ApolloClientWriter.writeSelectors(schema, CodegenConfig(packageName = "test.gen"))

    private def sourceOf(fileName: String)(using kyo.test.AssertScope) =
        sources.find(_.fileName == fileName).getOrElse(fail(s"$fileName not emitted")).contents

    "interface selector emission" - {

        "an interface gets its own selector source with marker trait and TypeName" in {
            val src = sourceOf("Node.scala")
            assert(src.contains("sealed trait Node"), src)
            assert(src.contains("given TypeName[Node] = TypeName(\"Node\")"), src)
            assert(src.contains("import kyo.Maybe"), src)
        }

        "the fields every implementor shares are direct selectors on the interface" in {
            val src = sourceOf("Node.scala")
            assert(src.contains("def id: SelectionBuilder.Deferrable[Node, (id: String)]"), src)
            assert(src.contains("def id: SelectionBuilder.Deferrable[Node, scala.NamedTuple.Concat[Acc, (id: String)]]"), src)
        }

        "each object implementor gets value + lambda `on<Implementor>` branches, chainable" in {
            val src = sourceOf("Node.scala")
            assert(
                src.contains(
                    "def onUser[A](sel: SelectionBuilder.Bidirectional[User, A]): SelectionBuilder.Fields[Node, (onUser: Maybe[A])]"
                ),
                src
            )
            assert(src.contains("SelectionBuilder.onType(\"Post\", build(SelectionBuilder.empty))"), src)
            assert(src.contains("sb ~ Node.onUser(sub(SelectionBuilder.empty))"), src)
            val named = sourceOf("Named.scala")
            assert(named.contains("def onUser["), named)
            assert(!named.contains("onPost"), named)
        }

        "an interface-typed field is an object selector, not a String scalar" in {
            val src = sourceOf("Queries.scala")
            assert(src.contains("final class `node$sel`(selArgs: Chunk[SelectionBuilder.Arg]) extends FieldSelector[RootQuery, Node]"), src)
            assert(
                src.contains(
                    "def apply[A](sel: SelectionBuilder.Bidirectional[Node, A]): SelectionBuilder.Deferrable[RootQuery, (nodes: Chunk[A])]"
                ),
                src
            )
            assert(!src.contains("SelectionBuilder.scalar(\"node\""), src)
        }
    }
end InterfaceSelectorSpec
