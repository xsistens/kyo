package kyo.uic

import kyo.*
import kyo.UI.*

/** What a [[TieredMenu]] keeps while its list changes under it: the submenu the reader opened and the row they
  * highlighted belong to an ITEM, so an item the signal adds before it must not hand either to a neighbour.
  */
class TieredMenuTest extends UicTest:

    private def renderHtml(ui: UI)(using Frame): String < Async =
        UI.runRender(ui).take(1).run.map(_.mkString)

    /** The labels of the root rows carrying `cls`. */
    private def rowsWith(ui: UI, cls: String)(using Frame): List[String] < Async =
        renderHtml(ui).map { html =>
            html.split("""<li """).toList.drop(1).collect {
                case row if row.takeWhile(_ != '>').contains(cls) =>
                    """p-tieredmenu-item-label[^>]*>([^<]*)<""".r.findFirstMatchIn(row).map(_.group(1)).getOrElse("")
            }
        }

    "an item the signal adds before an open submenu leaves it open, and the highlight on its row" in {
        for
            live  <- Signal.initRef(Seq.empty[uic.MenuItem])
            subs  <- Submenus.init()
            focus <- Anchored.init(List.empty[Int])
            ui = uic.TieredMenu().items(
                _.item("File").items(_.item("New").onSelect(())),
                live,
                _.item("Help").items(_.item("About").onSelect(()))
            ).wired(subs, focus, "tm")
            _      <- elementWithClass(ui, "p-tieredmenu-root-list").map(press(_, UI.Keyboard.ArrowDown))
            _      <- elementWithClass(ui, "p-tieredmenu-root-list").map(press(_, UI.Keyboard.ArrowDown))
            links  <- elementsWithClass(ui, "p-tieredmenu-item-link")
            _      <- click(links(1))
            before <- rowsWith(ui, "p-tieredmenu-item-active")
            focusB <- rowsWith(ui, "p-focus")
            _      <- live.set(Seq(uic.MenuItem("Bookmark").onSelect(())))
            html   <- renderHtml(ui)
            active <- rowsWith(ui, "p-tieredmenu-item-active")
            focus2 <- rowsWith(ui, "p-focus")
        yield
            assert(before == List("Help") && focusB == List("Help"), s"Help is open and highlighted: $before $focusB")
            assert(html.contains("About") && active == List("Help"), s"Help stays open: $active")
            assert(focus2 == List("Help"), s"and highlighted: $focus2")
        end for
    }

end TieredMenuTest
