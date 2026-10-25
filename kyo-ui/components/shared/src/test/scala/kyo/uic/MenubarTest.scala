package kyo.uic

import kyo.*
import kyo.UI.*

/** What a [[Menubar]] keeps while its list changes under it: the submenu the reader opened and the row they
  * highlighted belong to an ITEM, so an item the signal adds or removes before it must not hand either to a
  * neighbour.
  */
class MenubarTest extends UicTest:

    private def renderHtml(ui: UI)(using Frame): String < Async =
        UI.runRender(ui).take(1).run.map(_.mkString)

    private def bar(live: Signal[Seq[MenuItem]])(using Frame) =
        uic.Menubar().items(
            _.item("File").items(_.item("New").onSelect(())),
            live,
            _.item("Help").items(_.item("About").onSelect(()))
        )

    /** The text of the rows carrying `cls`, in order. */
    private def rowsWith(ui: UI, cls: String)(using Frame): List[String] < Async =
        renderHtml(ui).map { html =>
            html.split("""<li """).toList.drop(1).collect {
                case row if row.takeWhile(_ != '>').contains(cls) =>
                    """p-menubar-item-label[^>]*>([^<]*)<""".r.findFirstMatchIn(row).map(_.group(1)).getOrElse("")
            }
        }

    /** Presses `key` on the element carrying `cls` as the ui draws it NOW: a key handler closes over the render it came
      * from, so pressing an element read before the last key would repeat that key from the same place.
      */
    private def pressOn(ui: UI, cls: String, key: UI.Keyboard)(using Frame): Any < Async =
        elementWithClass(ui, cls).map(press(_, key))

    "an item the signal adds before an open submenu leaves that submenu open, and the highlight on its row" in {
        for
            live  <- Signal.initRef(Seq.empty[uic.MenuItem])
            subs  <- Submenus.init()
            focus <- Anchored.init(List.empty[Int])
            ui = bar(live).wired(subs, focus, "mb")
            _           <- pressOn(ui, "p-menubar-root-list", UI.Keyboard.ArrowRight)
            _           <- pressOn(ui, "p-menubar-root-list", UI.Keyboard.ArrowRight)
            links       <- elementsWithClass(ui, "p-menubar-item-link")
            _           <- click(links(1))
            before      <- renderHtml(ui)
            focusBefore <- rowsWith(ui, "p-focus")
            _           <- live.set(Seq(uic.MenuItem("Plugin").onSelect(())))
            after       <- renderHtml(ui)
            active      <- rowsWith(ui, "p-menubar-item-active")
            focused     <- rowsWith(ui, "p-focus")
        yield
            assert(before.contains("About") && focusBefore == List("Help"), s"Help is open and highlighted: $focusBefore")
            assert(after.contains("About"), "Help's submenu stays open once Plugin arrives before it")
            assert(active == List("Help"), s"and it is Help's row that is open: $active")
            assert(focused == List("Help"), s"the highlight stays on Help: $focused")
        end for
    }

    "an open submenu whose item the signal removes closes, and no neighbour opens in its place" in {
        for
            live  <- Signal.initRef(Seq(uic.MenuItem("Plugin").items(uic.MenuItem("Tools").onSelect(()))))
            subs  <- Submenus.init()
            focus <- Anchored.init(List.empty[Int])
            ui = bar(live).wired(subs, focus, "mb")
            links  <- elementsWithClass(ui, "p-menubar-item-link")
            _      <- click(links(1))
            before <- renderHtml(ui)
            _      <- live.set(Seq.empty)
            after  <- renderHtml(ui)
            active <- rowsWith(ui, "p-menubar-item-active")
        yield
            assert(before.contains("Tools"), "Plugin's submenu is open")
            assert(!after.contains("Tools") && !after.contains("About"), "nothing is open once Plugin is gone")
            assert(active.isEmpty, s"no row is open: $active")
        end for
    }

end MenubarTest
