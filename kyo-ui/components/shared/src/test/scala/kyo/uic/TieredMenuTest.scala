package kyo.uic

import kyo.*
import kyo.UI.*

/** What a [[TieredMenu]] keeps while its list changes under it: the submenu the reader opened and the row they
  * highlighted belong to an ITEM, so an item the signal adds before it must not hand either to a neighbour.
  */
class TieredMenuTest extends ReactiveValueContract:

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

    private def tiered(using Frame) = uic.TieredMenu().items(uic.MenuItem("New").onSelect(()), uic.MenuItem("Open").onSelect(()))

    private def links(t: UI)(using Frame)  = elementsWithClass(t, "p-tieredmenu-item-link").map(_.filter(declaresClick))
    private def panels(t: UI)(using Frame) = elementsWithClass(t, "p-tieredmenu-overlay")

    // A popup closes on a pick, an outside click or Escape: the rows are the control a gesture uses, and a fixed popup
    // is one nothing outside dismisses, since a pick still runs the item's own action.
    holds(new SlotCase[TieredMenu, Boolean](
        name = "popup",
        bare = Present(tiered.popup()),
        bind = tiered.popup(_),
        hear = (m, f) => m.onPopupChange(f),
        event = "onPopupChange",
        render = _.render,
        start = true,
        next = false,
        control = links(_).map(_.head),
        shows = panels(_).map(_.nonEmpty),
        isFixed = Present(elementsWithClass(_, "p-uic-overlay-backdrop").map(_.isEmpty)),
        ready = Present(panels(_).map(_.nonEmpty))
    ))

end TieredMenuTest
