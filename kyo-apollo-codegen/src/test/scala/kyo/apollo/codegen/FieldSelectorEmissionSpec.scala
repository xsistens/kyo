package kyo.apollo.codegen

/** Guards the `FieldSelector` emission: every generated `` `field$sel` `` class
  * extends `FieldSelector[Origin, Leaf]` and exposes the field's schema name, so
  * selector values double as typed field handles for library configuration
  * (`ConnectionFieldPolicy.of(Playlist.tracks(), PlaylistTrackConnection.edges)`).
  */
class FieldSelectorEmissionSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val schema = SchemaLoader.fromString(
        """type Query { album(id: ID!): Album }
      |type Album { name: String! tracks(first: Int, after: String): TrackPage! }
      |type TrackPage { edges: [TrackEdge!]! }
      |type TrackEdge { cursor: String! }
      |""".stripMargin
    )

    private def src(fileName: String)(using kyo.test.AssertScope) =
        ApolloClientWriter
            .writeSelectors(schema, CodegenConfig(packageName = "test.gen"))
            .find(_.fileName == fileName)
            .getOrElse(fail(s"$fileName not emitted"))
            .contents

    "FieldSelector emission" - {

        "an object field's `$sel` class extends FieldSelector[Origin, Leaf] with its name" in {
            val album = src("Album.scala")
            assert(
                album.contains(
                    "final class `tracks$sel`(selArgs: Chunk[SelectionBuilder.Arg]) extends FieldSelector[Album, TrackPage]:"
                ),
                album
            )
            assert(album.contains("def fieldName: String = \"tracks\""), album)
        }

        "a list field's `$sel` class uses the element type as Leaf" in {
            val page = src("TrackPage.scala")
            assert(
                page.contains(
                    "final class `edges$sel`(selArgs: Chunk[SelectionBuilder.Arg]) extends FieldSelector[TrackPage, TrackEdge]:"
                ),
                page
            )
            assert(page.contains("def fieldName: String = \"edges\""), page)
        }

        "a root field's `$sel` class selects on the root origin" in {
            val queries = src("Queries.scala")
            assert(
                queries.contains(
                    "final class `album$sel`(selArgs: Chunk[SelectionBuilder.Arg]) extends FieldSelector[RootQuery, Album]:"
                ),
                queries
            )
            assert(queries.contains("def fieldName: String = \"album\""), queries)
        }
    }
end FieldSelectorEmissionSpec
