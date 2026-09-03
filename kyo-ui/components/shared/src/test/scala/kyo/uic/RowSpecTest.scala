package kyo.uic

import kyo.*

/** The two properties of the `<tr>` stream that a keyed body will rest on.
  *
  * A table body is not one element per row: an expanded row is two, a group contributes rows that
  * belong to no record, and a windowed body pads with spacers and draws slots. `RowSpec` is that
  * stream, one entry per `<tr>`, and both properties below are conditions of reuse rather than of
  * correctness — which is exactly why they need their own tests. Break either one and the markup is
  * still right; the body simply renders everything again, silently, the way it does today.
  *
  *   - **One entry, one element.** A keyed region reuses a child only if it paints as a single
  *     `Element`; a `Fragment` falls back to rebuilding the list. This is the failure mode that
  *     looks like success.
  *   - **Distinct keys within an emission.** Duplicates make the region warn and rebuild the whole
  *     list, and the warning goes to the console rather than to the page.
  */
class RowSpecTest extends UicTest:

    final case class Item(id: String, name: String) derives CanEqual

    private val items = List(Item("a", "A"), Item("b", "B"))

    /** Every entry a body could emit, over one table that has every feature turned on at once.
      *
      * Built by hand rather than taken off a render: the point is to cover combinations a single
      * configuration cannot show — a table is either windowed or grouped, never both — and the
      * properties hold per entry, so the table around them only has to be able to render one.
      */
    private def specs(t: uic.DataTable[Item]): List[RowSpec[Item]] =
        val data = RowSpec.Data[Item](
            items.head,
            0,
            Set("a"),
            Present("a"),
            Set.empty,
            Set("a"),
            3,
            Map.empty,
            EditState(Set.empty, Absent),
            NavState[Item](),
            FrozenPlan(),
            MoveState[Item](),
            SelectState()
        )
        List(
            data,
            data.copy(a = items(1), index = 1, sel = Set.empty, ctx = Absent, exp = Set.empty),
            RowSpec.Expansion(items.head, 3),
            RowSpec.GroupHead(uic.RowGroup[Item, String](_.name), GroupPath(List("A")), items, 3, true, true),
            RowSpec.GroupFoot((_, rows) => UI.span(rows.size.toString), GroupPath(List("A")), items, 3),
            RowSpec.Spacer(120, Present((4, 10))),
            RowSpec.Spacer(80),
            RowSpec.Slot(4, 32, 0, 1, 0, FrozenPlan()),
            RowSpec.Slot(5, 32, 0, 1, 0, FrozenPlan()),
            RowSpec.Empty(3)
        )
    end specs

    private def table(using Frame): uic.DataTable[Item] =
        uic.DataTable[Item]().rows(items).rowKey(_.id)
            .columns(uic.column("Name")(_.name))
            .rowExpansionTemplate(i => UI.span(s"more about ${i.name}"))

    "every entry paints as exactly one tr" in {
        val t = table
        Kyo.foreach(Chunk.from(specs(t))) { spec =>
            val painted = t.renderTr(spec)
            // Rendered rather than inspected: "one element" is a property of the MARKUP, and an
            // Ast node that wrapped its row would still be one node here.
            Kyo.foreach(Chunk.from(painted))(ui => kyo.internal.HtmlRenderer.render(ui, Seq("t")))
                .map(html => (t.rowSpecKey(spec), painted.size, html.mkString))
        }.map { rendered =>
            rendered.foreach { (key, count, html) =>
                assert(count == 1, s"$key painted $count nodes")
                assert(html.startsWith("<tr"), s"$key painted $html")
                assert(html.endsWith("</tr>"), s"$key painted $html")
                assert(
                    html.drop(1).indexOf("<tr") < 0,
                    s"$key painted more than one row: $html"
                )
            }
            succeed
        }
    }

    "the entries of one emission have distinct keys" in {
        val t    = table
        val keys = specs(t).map(t.rowSpecKey)
        assert(keys.distinct.size == keys.size, s"repeated: ${keys.diff(keys.distinct).mkString(", ")}")
    }

    "a row and its expansion are told apart, and so are the two spacers" in {
        val t = table
        assert(t.rowSpecKey(RowSpec.Expansion(items.head, 3)) != t.rowSpecKey(specs(t).head))
        assert(t.rowSpecKey(RowSpec.Spacer(0, Present((0, 0)))) == "sp:top")
        assert(t.rowSpecKey(RowSpec.Spacer(0)) == "sp:bottom")
    }

end RowSpecTest
