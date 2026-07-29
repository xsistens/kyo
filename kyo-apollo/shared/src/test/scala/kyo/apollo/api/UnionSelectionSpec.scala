package kyo.apollo.api

import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Guards [[SelectionBuilder.onType]], the inline-fragment branch a generated
  * union selector delegates to: the branch renders as `... on <Type> { … }`,
  * decodes to `Some` exactly when the object's `__typename` matches, and a
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
        def name: SelectionBuilder[Track, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)
        def durationMs: SelectionBuilder[Track, (durationMs: Int)] =
            SelectionBuilder.scalar("durationMs", CompiledNamedType("Int").notNull, ScalarCodec.int)
    end Track

    object Episode:
        def name: SelectionBuilder[Episode, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)

    object PlayableItem:
        def onTrack[A](sel: SelectionBuilder[Track, A]): SelectionBuilder[PlayableItem, (onTrack: Option[A])] =
            SelectionBuilder.onType("Track", sel)
        def onEpisode[A](sel: SelectionBuilder[Episode, A]): SelectionBuilder[PlayableItem, (onEpisode: Option[A])] =
            SelectionBuilder.onType("Episode", sel)
    end PlayableItem

    object Queries:
        def item[A](sel: SelectionBuilder[PlayableItem, A]): SelectionBuilder[RootQuery, (item: Option[A])] =
            SelectionBuilder.obj(
                "item",
                CompiledNamedType("PlayableItem"),
                Nil,
                sel,
                SelectionBuilder.Nesting.Nullable(SelectionBuilder.Nesting.Leaf)
            )
    end Queries

    private def sel =
        Queries.item(PlayableItem.onTrack(Track.name ~ Track.durationMs) ~ PlayableItem.onEpisode(Episode.name))

    // --- Tests -----------------------------------------------------------------

    "union selection via onType" - {

        "renders inline fragments with type conditions" in {
            val doc = DocumentPrinter.render("query", "Q", Nil, sel.selections)
            assert(
                doc == "query Q { item { __typename ... on Track { name durationMs } ... on Episode { name } } }",
                doc
            )
        }

        "decodes the matching branch to Some, the others to None" in {
            val response = Json.JObj(VectorMap(
                "item" -> Json.JObj(VectorMap(
                    "__typename" -> Json.JStr("Track"),
                    "name"       -> Json.JStr("Glass Season"),
                    "durationMs" -> Json.JNum(215000)
                ))
            ))
            val decoded = sel.decode(response)
            val item    = decoded.item.getOrElse(fail("item was null"))
            assert(item.onTrack == Some((name = "Glass Season", durationMs = 215000)), item.toString)
            assert(item.onEpisode == None, item.toString)
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
            assert(item.onTrack == None, item.toString)
            assert(item.onEpisode == Some((name = "Signal Path #3")), item.toString)
        }

        "encodes a decoded value back to the member-typed response shape" in {
            val response = Json.JObj(VectorMap(
                "item" -> Json.JObj(VectorMap(
                    "__typename" -> Json.JStr("Track"),
                    "name"       -> Json.JStr("Glass Season"),
                    "durationMs" -> Json.JNum(215000)
                ))
            ))
            assert(sel.encode(sel.decode(response)) == response, sel.encode(sel.decode(response)).toString)
        }

        "decodes a null union field to None without touching the branches" in {
            val decoded = sel.decode(Json.JObj(VectorMap("item" -> Json.JNull)))
            assert(decoded.item == None, decoded.toString)
        }
    }
end UnionSelectionSpec
