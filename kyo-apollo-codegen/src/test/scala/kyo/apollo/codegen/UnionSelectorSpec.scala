package kyo.apollo.codegen

/** Guards union emission: a GraphQL `union` gets its own selector source (a
  * phantom marker trait + one `on<Member>` inline-fragment branch per member,
  * value + lambda overloads, plus chainable accessors), and a field whose leaf
  * type is a union is emitted as an OBJECT selector (nested selection) — not
  * the scalar-`String` fallback unknown leaves get. Surfaced by the
  * spotify-showcase schema (`union PlayableItem = Track | Episode`); the
  * countries schema has no unions, so this path was previously dead.
  */
class UnionSelectorSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val schema = SchemaLoader.fromString(
        """type Query { playback: PlaybackState }
      |type PlaybackState { isPlaying: Boolean! item: PlayableItem }
      |type Track { name: String! durationMs: Int! }
      |type Episode { name: String! }
      |union PlayableItem = Track | Episode
      |""".stripMargin
    )

    private def sources = ApolloClientWriter.writeSelectors(schema, CodegenConfig(packageName = "test.gen"))

    private def sourceOf(fileName: String)(using kyo.test.AssertScope) =
        sources.find(_.fileName == fileName).getOrElse(fail(s"$fileName not emitted")).contents

    "union selector emission" - {

        "a union gets its own selector source with marker trait and TypeName" in {
            val src = sourceOf("PlayableItem.scala")
            assert(src.contains("sealed trait PlayableItem"), src)
            assert(src.contains("given TypeName[PlayableItem] = TypeName(\"PlayableItem\")"), src)
        }

        "each member gets value + lambda `on<Member>` branches delegating to onType" in {
            val src = sourceOf("PlayableItem.scala")
            assert(src.contains("def onTrack[A](sel: SelectionBuilder[Track, A])"), src)
            assert(src.contains("def onEpisode[A](sel: SelectionBuilder[Episode, A])"), src)
            assert(src.contains("SelectionBuilder.onType(\"Track\", sel)"), src)
            assert(src.contains("SelectionBuilder.onType(\"Episode\", build(SelectionBuilder.empty))"), src)
        }

        "branches are chainable on an accumulated union selection" in {
            val src = sourceOf("PlayableItem.scala")
            assert(src.contains("extension [Acc <: scala.NamedTuple.AnyNamedTuple](sb: SelectionBuilder[PlayableItem, Acc])"), src)
            assert(src.contains("sb ~ PlayableItem.onTrack(sub(SelectionBuilder.empty))"), src)
        }

        "a union-typed field is an object selector, not a String scalar" in {
            val src = sourceOf("PlaybackState.scala")
            assert(src.contains("final class `item$sel`"), src)
            assert(src.contains("SelectionBuilder[PlaybackState, (item: Option[A])]"), src)
            assert(!src.contains("SelectionBuilder.scalar(\"item\""), src)
        }
    }
end UnionSelectorSpec
