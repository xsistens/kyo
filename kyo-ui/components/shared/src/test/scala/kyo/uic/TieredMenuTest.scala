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

    private def tiered(using Frame) = uic.TieredMenu().items(uic.MenuItem("New").onSelect(()), uic.MenuItem("Open").onSelect(()))

    private def links(t: UI)(using Frame)  = elementsWithClass(t, "p-tieredmenu-item-link").map(_.filter(declaresClick))
    private def panels(t: UI)(using Frame) = elementsWithClass(t, "p-tieredmenu-overlay")

    /** The popup as its mount publishes it, the rows wired. */
    private def mountedPopup(ui: UI)(using Frame, kyo.test.AssertScope): UI < Async =
        publishedAfterReady(ui, t => panels(t).map(_.nonEmpty))().map(_.getOrElse(throw new AssertionError("never mounted")))

    "popup() keeps the open state itself and starts open, so a pick closes it" in {
        for
            _     <- mountedPopup(tiered.popup().render)
            after <- publishedAfterReady(tiered.popup().render, t => links(t).map(_.nonEmpty), until = t => panels(t).map(_.isEmpty))(t =>
                links(t).map(ls => click(ls.head))
            )
            card <- elementsWithClass(tiered.popup().render, "p-uic-key-error")
        yield
            assert(after.nonEmpty, "the popup never closed")
            assert(card.isEmpty, s"state the menu keeps itself: ${card.map(textOf)}")
    }

    "a constant nobody hears is fixed: the popup stays, and nothing outside dismisses it" in {
        for
            t    <- mountedPopup(tiered.popup(true).render)
            out  <- elementsWithClass(t, "p-uic-overlay-backdrop")
            card <- elementsWithClass(tiered.popup(true).render, "p-uic-key-error")
        yield
            assert(out.isEmpty, "no backdrop takes an outside click")
            assert(card.isEmpty, s"a fixed state is not a mistake: ${card.map(textOf)}")
    }

    "a signal bound one way is followed, and a pick is reported without being written" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            t    <- mountedPopup(tiered.popup(held.readOnly).onPopupChange(v => seen.set(Present(v))).render)
            ls   <- links(t)
            _    <- click(ls.head)
            h    <- held.get
            s    <- seen.get
            deaf <- elementsWithClass(tiered.popup(held.readOnly).render, "p-uic-key-error")
        yield
            assert(h && s == Present(false), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onPopupChange"), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

    "a ref is written before the menu reports it" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef((true, true))
            t    <- mountedPopup(tiered.popup(held).onPopupChange(v => held.get.map(h => seen.set((v, h)))).render)
            ls   <- links(t)
            _    <- click(ls.head)
            s    <- seen.get
        yield assert(s == (false, false), s"reported, held: $s")
    }

end TieredMenuTest
