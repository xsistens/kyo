package kyo.apollo.codegen

/** Guards union emission: a GraphQL `union` gets its own selector source (a
  * phantom marker trait + one `on<Member>` inline-fragment branch per member,
  * value + lambda overloads, plus chainable accessors), and a field whose leaf
  * type is a union is emitted as an OBJECT selector (nested selection), never a
  * scalar. Surfaced by the spotify-showcase schema (`union PlayableItem = Track |
  * Episode`).
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

        "each member gets an `on<Member>` selector object: a SelectionList over the member plus the value form" in {
            val src = sourceOf("PlayableItem.scala")
            assert(
                src.contains(
                    "final class `onTrack$sel` extends SelectionList[Track.type, Track, [B <: scala.NamedTuple.AnyNamedTuple] =>> SelectionBuilder.Fields[PlayableItem, (onTrack: Maybe[B])]]:"
                ),
                src
            )
            assert(src.contains("protected def selector: Track.type = Track"), src)
            assert(
                src.contains(
                    "def apply[A](sel: SelectionBuilder.Bidirectional[Track, A]): SelectionBuilder.Fields[PlayableItem, (onTrack: Maybe[A])]"
                ),
                src
            )
            assert(src.contains("SelectionBuilder.onType(\"Track\", sel)"), src)
            assert(src.contains("def onEpisode: `onEpisode$sel` ="), src)
        }

        "the selector object is what a lambda receives: no accumulator extension block" in {
            val src = sourceOf("PlayableItem.scala")
            assert(!src.contains("extension ["), src)
            assert(!src.contains("SelectionBuilder.empty"), src)
            // What the library entry points take (`Fragment.entity(PlayableItem)(...)`): the object as a
            // `SelectionList` over itself, with its `TypeName` in the companion's implicit scope.
            assert(src.contains("object PlayableItem extends SelectionList[PlayableItem.type, PlayableItem, "), src)
            assert(src.contains("given TypeName[PlayableItem] = TypeName(\"PlayableItem\")"), src)
            assert(!src.contains("SelectorOf"), src)
        }

        "a union-typed field is an object selector, not a String scalar" in {
            val src = sourceOf("PlaybackState.scala")
            assert(src.contains("final class `item$sel`"), src)
            assert(
                src.contains(
                    "def apply[A](sel: SelectionBuilder.Bidirectional[PlayableItem, A]): SelectionBuilder.Deferrable[PlaybackState, (item: Maybe[A])]"
                ),
                src
            )
            assert(!src.contains("SelectionBuilder.scalar(\"item\""), src)
        }
    }
end UnionSelectorSpec
