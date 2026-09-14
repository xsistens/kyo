package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Guards [[SelectionBuilder.onType]], the inline-fragment branch a generated
  * union selector delegates to: the branch renders as `... on <Type> { … }`,
  * decodes to `Present` exactly when the object's `__typename` matches, and a
  * decoded value encodes back to the same response shape (with the concrete
  * member's `__typename`) so cache normalization keys the member type, not the
  * union. The hand-written layer below mimics what the generator emits for
  * `union PlayableItem = Track | Episode`.
  */
class UnionSelectionSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- Hand-written "generated" union layer ----------------------------------

    sealed trait RootQuery
    sealed trait Track
    sealed trait Episode
    sealed trait PlayableItem

    object Track:
        def name: SelectionBuilder.Deferrable[Track, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)
        def durationMs: SelectionBuilder.Deferrable[Track, (durationMs: Int)] =
            SelectionBuilder.scalar("durationMs", CompiledNamedType("Int").notNull, ScalarCodec.int)
    end Track

    object Episode:
        def name: SelectionBuilder.Deferrable[Episode, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)

    object PlayableItem:
        def onTrack[A](sel: SelectionBuilder[Track, A]): SelectionBuilder.Fields[PlayableItem, (onTrack: Maybe[A])] =
            SelectionBuilder.onType("Track", sel)
        def onEpisode[A](sel: SelectionBuilder[Episode, A]): SelectionBuilder.Fields[PlayableItem, (onEpisode: Maybe[A])] =
            SelectionBuilder.onType("Episode", sel)
    end PlayableItem

    object Queries:
        def item[A](sel: SelectionBuilder[PlayableItem, A]): SelectionBuilder.Deferrable[RootQuery, (item: Maybe[A])] =
            SelectionBuilder.obj(
                "item",
                CompiledNamedType("PlayableItem"),
                Chunk.empty,
                sel,
                SelectionBuilder.Nesting.Nullable(SelectionBuilder.Nesting.Leaf)
            )
    end Queries

    private def sel =
        Queries.item(PlayableItem.onTrack(Track.name ~ Track.durationMs) ~ PlayableItem.onEpisode(Episode.name))

    // --- Tests -----------------------------------------------------------------

    "union selection via onType" - {

        "renders inline fragments with type conditions" in {
            val doc = DocumentPrinter.render("query", "Q", Chunk.empty, sel.selections)
            assert(
                doc == "query Q { item { __typename ... on Track { name durationMs } ... on Episode { name } } }",
                doc
            )
        }

        "decodes the matching branch to Present, the others to Absent" in {
            val response = Json.JObj(VectorMap(
                "item" -> Json.JObj(VectorMap(
                    "__typename" -> Json.JStr("Track"),
                    "name"       -> Json.JStr("Glass Season"),
                    "durationMs" -> Json.JInt(215000)
                ))
            ))
            val decoded = sel.decode(response)
            val item    = decoded.item.getOrElse(fail("item was null"))
            assert(item.onTrack == Present((name = "Glass Season", durationMs = 215000)), item.toString)
            assert(item.onEpisode == Absent, item.toString)
        }

        "decodes an Episode value through the other branch" in {
            val response = Json.JObj(VectorMap(
                "item" -> Json.JObj(VectorMap(
                    "__typename" -> Json.JStr("Episode"),
                    "name"       -> Json.JStr("Signal Path #3")
                ))
            ))
            val decoded = sel.decode(response)
            val item    = decoded.item.getOrElse(fail("item was null"))
            assert(item.onTrack == Absent, item.toString)
            assert(item.onEpisode == Present((name = "Signal Path #3")), item.toString)
        }

        "encodes a decoded value back to the member-typed response shape" in {
            val response = Json.JObj(VectorMap(
                "item" -> Json.JObj(VectorMap(
                    "__typename" -> Json.JStr("Track"),
                    "name"       -> Json.JStr("Glass Season"),
                    "durationMs" -> Json.JInt(215000)
                ))
            ))
            assert(sel.encode(sel.decode(response)) == response, sel.encode(sel.decode(response)).toString)
        }

        "decodes a null union field to Absent without touching the branches" in {
            val decoded = sel.decode(Json.JObj(VectorMap("item" -> Json.JNull)))
            assert(decoded.item == Absent, decoded.toString)
        }
    }
end UnionSelectionSpec
