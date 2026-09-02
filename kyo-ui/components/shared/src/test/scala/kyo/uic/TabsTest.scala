package kyo.uic

import kyo.*
import kyo.UI.*

/** A tablist is ONE tab stop: the roving tabindex takes every inactive header out of the Tab
  * order, so the arrows are the only way back to them. Before this the headers were simply
  * unreachable from the keyboard.
  *
  * The wiring lives in a mount, which a pure render shows only as its placeholder, so these drive
  * the `wired` seam with a recording focus function instead of a `UI.Commands`.
  */
class TabsTest extends UicTest:

    private val ids = List("t0", "t1", "t2")

    private def strip(using Frame) =
        uic.Tabs().tabs(
            uic.Tab("One", p("first"), "one"),
            uic.Tab("Two", p("second"), "two", disabled = true),
            uic.Tab("Three", p("third"), "three")
        )

    /** Presses `key` on the header at `at` and returns the id focus was moved to, if any. */
    private def move(at: Int, key: UI.Keyboard)(using Frame): Maybe[String] < Async =
        for
            moved <- Signal.initRef(Absent: Maybe[String])
            ui = strip.wired(ids, id => moved.set(Present(id)))
            headers <- elementsWithClass(ui, "p-tab")
            _       <- press(headers(at), key)
            to      <- moved.get
        yield to

    "ArrowRight moves to the next header" in move(0, UI.Keyboard.ArrowRight).map(to => assert(to == Present("t2")))

    "and steps over a disabled one on the way" in {
        // Position 1 is disabled, so ArrowRight from 0 lands on 2 rather than on it.
        move(0, UI.Keyboard.ArrowRight).map(to => assert(to == Present("t2")))
    }

    "ArrowLeft moves back" in move(2, UI.Keyboard.ArrowLeft).map(to => assert(to == Present("t0")))

    "the ends wrap, as Prime's tablist does" in {
        for
            forward <- move(2, UI.Keyboard.ArrowRight)
            back    <- move(0, UI.Keyboard.ArrowLeft)
        yield assert(forward == Present("t0") && back == Present("t2"))
    }

    "Home and End reach the ends directly" in {
        for
            home <- move(2, UI.Keyboard.Home)
            end  <- move(0, UI.Keyboard.End)
        yield assert(home == Present("t0") && end == Present("t2"))
    }

    "the vertical arrows belong to the page, not to a horizontal tablist" in {
        for
            down <- move(0, UI.Keyboard.ArrowDown)
            up   <- move(0, UI.Keyboard.ArrowUp)
        yield assert(down == Absent && up == Absent)
    }

    "Enter and Space move nothing, since the header is a button the browser activates" in {
        for
            enter <- move(0, UI.Keyboard.Enter)
            space <- move(0, UI.Keyboard.Space)
        yield assert(enter == Absent && space == Absent)
    }

    // ── the selection slot: one setter, three bindings ───────────────────────────

    /** The id the strip marks active, read off the rendered headers. */
    private def activeId(ui: UI)(using Frame): Maybe[Int] < Sync =
        elementsWithClass(ui, "p-tab").map { hs =>
            Maybe.fromOption(hs.indexWhere(_.attrs.cssClasses.contains("p-tab-active")) match
                case -1 => None
                case i  => Some(i))
        }

    "a constant selection names the active tab" in {
        for
            ui <- Kyo.lift(strip.selected("three").render)
            at <- activeId(ui)
            none = strip.render
            head <- activeId(none)
        yield assert(at == Present(2) && head == Present(0), "and with nothing bound the first tab is active")
    }

    "a derived signal binds ONE way: the strip follows it and never writes back" in {
        // `map` is what a route-shaped selection looks like — a Signal that is not a
        // SignalRef, so there is nothing to write into even in principle. Before this
        // slot took a union it could not be handed over at all.
        for
            src <- Signal.initRef(2)
            derived = src.map(i => List("one", "two", "three")(i))
            ui      = strip.selected(derived).render
            before <- activeId(ui)
            _      <- src.set(0)
            after  <- activeId(ui)
        yield assert(before == Present(2) && after == Present(0))
    }

    "clicking a one-way tab fires onTabSelect and changes nothing" in {
        for
            src   <- Signal.initRef("three")
            fired <- Signal.initRef(Absent: Maybe[String])
            // `.readOnly` is the documented opt-out from two-way binding, and the
            // runtime class is what the dispatch reads — so this stays one-way.
            ui = strip.selected(src.readOnly).onTabSelect(id => fired.set(Present(id))).render
            headers <- elementsWithClass(ui, "p-tab")
            _       <- click(headers(0))
            held    <- src.get
            saw     <- fired.get
        yield assert(held == "three" && saw == Present("one"), "the source of truth is untouched")
    }

    "a writable ref still binds two way" in {
        for
            ref <- Signal.initRef("three")
            ui = strip.selected(ref).render
            headers <- elementsWithClass(ui, "p-tab")
            _       <- click(headers(0))
            held    <- ref.get
        yield assert(held == "one")
    }

    // ── url headers: a navigational strip, not a panel switcher ──────────────────

    private def navStrip(using Frame) =
        uic.Tabs().tabs(
            uic.Tab("One", p("first"), "one").url("/one"),
            uic.Tab("Two", p("second"), "two", disabled = true).url("/two"),
            uic.Tab("Three", p("third"), "three").url("/three")
        )

    "a url header is a real anchor, and keeps the tablist vocabulary" in {
        for
            headers <- elementsWithClass(navStrip.selected("one").render, "p-tab")
        yield
            val first = headers(0)
            assert(first.isInstanceOf[UI.Ast.Anchor], "the header is an <a>, not a <button>")
            assert(
                first.asInstanceOf[UI.Ast.Anchor].href.contains(Href.Path("/one")),
                "carrying the href a button could never have"
            )
            assert(first.attrs.ariaAttrs.get("selected").contains("true"))
            assert(first.attrs.tabIndex.contains(0), "and the roving tabindex is unchanged")
            assert(headers(2).attrs.tabIndex.contains(-1))
    }

    "a disabled url header drops its href, since an anchor has no native disabled" in {
        for
            headers <- elementsWithClass(navStrip.selected("one").render, "p-tab")
        yield
            val off = headers(1)
            assert(off.asInstanceOf[UI.Ast.Anchor].href.isEmpty, "nothing to navigate to")
            assert(off.attrs.ariaAttrs.get("disabled").contains("true"), "and it says so")
            assert(off.attrs.cssClasses.contains("p-disabled"))
            assert(off.attrs.tabIndex.isEmpty, "out of the Tab order, like the button form")
    }

    "clicking a url header does NOT write back — the navigation is the selection" in {
        // Writing would race the navigation: the ref would hold the new id while the
        // location, and anything derived from it, still held the old one.
        for
            ref   <- Signal.initRef("three")
            fired <- Signal.initRef(Absent: Maybe[String])
            ui = navStrip.selected(ref).onTabSelect(id => fired.set(Present(id))).render
            headers <- elementsWithClass(ui, "p-tab")
            _       <- click(headers(0))
            held    <- ref.get
            saw     <- fired.get
        yield assert(held == "three" && saw == Present("one"), "but onTabSelect still fires")
    }

    "a url header declares no click handler unless there is something for it to do" in {
        // Not a micro-optimisation. kyo-ui prevent-defaults a click on ANY anchor that
        // carries a kyo click handler, with no exemption for Ctrl/Cmd/Shift/Alt or the
        // middle button — so an empty handler would silently cost the very thing an
        // anchor was chosen for. Verified in a browser both ways.
        for
            plain <- elementsWithClass(navStrip.selected("one").render, "p-tab")
            withSelect <- elementsWithClass(
                navStrip.selected("one").onTabSelect(_ => ()).render,
                "p-tab"
            )
            noUrls <- elementsWithClass(strip.selected("one").render, "p-tab")
        yield
            assert(plain(0).attrs.onClick.isEmpty, "a pure navigation strip stays a plain link")
            assert(withSelect(0).attrs.onClick.isDefined, "onTabSelect is honoured when asked for")
            assert(noUrls(0).attrs.onClick.isDefined, "and a button header always has one")
    }

    "a strip without urls still writes back, so the panel switcher is unchanged" in {
        for
            ref <- Signal.initRef("three")
            ui = strip.selected(ref).render
            headers <- elementsWithClass(ui, "p-tab")
            _       <- click(headers(0))
            held    <- ref.get
        yield assert(held == "one")
    }

    // ── the id base, and what derives from it ────────────────────────────────────

    "header ids derive from a caller's base, using each tab's own logical id" in {
        for
            headers <- elementsWithClass(strip.id("library").render, "p-tab")
        yield assert(
            headers.map(_.attrs.identifier).toList ==
                List(Present("library-one"), Present("library-two"), Present("library-three"))
        )
    }

    "without a base the ids are minted, so a pure render has none at all" in {
        // The minting needs a mount, and a pure render shows only the placeholder — which is
        // why a golden render and an SSG page had no header ids before a base could be given.
        for
            headers <- elementsWithClass(strip.render, "p-tab")
        yield assert(headers.forall(_.attrs.identifier.isEmpty))
    }

    "a disabled header is still addressable, though it is out of the arrow wiring" in {
        // The id used to be assigned inside the not-disabled branch, because it existed only
        // to move the arrow focus and the arrows skip a disabled header. A CHOSEN id is a
        // different thing: naming a header is often exactly how a caller asserts it is
        // disabled.
        for
            headers <- elementsWithClass(strip.id("library").render, "p-tab")
        yield
            assert(headers(1).attrs.identifier == Present("library-two"), "named")
            assert(headers(1).attrs.tabIndex.isEmpty, "but out of the Tab order")
            assert(headers(1).attrs.onKeyDown.isEmpty, "and out of the arrow wiring")
    }

    "only the active header is in the Tab order, and a disabled one is in neither" in {
        for
            active  <- Signal.initRef("three")
            ui      <- Kyo.lift(strip.selected(active).wired(ids, _ => ()))
            headers <- elementsWithClass(ui, "p-tab")
        yield
            assert(headers(0).attrs.tabIndex.contains(-1), "an inactive header is out of the Tab order")
            assert(headers(2).attrs.tabIndex.contains(0), "the active one is the tab stop")
            assert(headers(1).attrs.tabIndex.isEmpty, "and a disabled one carries no tabindex at all")
            assert(headers(1).attrs.onKeyDown.isEmpty, "nor a key handler")
    }

end TabsTest
