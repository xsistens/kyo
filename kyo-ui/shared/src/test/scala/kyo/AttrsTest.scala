package kyo

import kyo.Browser.*
import kyo.Length.*
import kyo.UI.*
import kyo.UI.Ast.*

class AttrsTest extends UITest:

    // ---- id ----

    "id findable" in {
        withUI(UI.div("X").id("myDiv")) {
            for
                _ <- Browser.assertVisible(Selector.id("myDiv"))
                _ <- Browser.assertText(Selector.id("myDiv"), "X")
            yield ()
        }
    }

    "two ids both findable" in {
        withUI(UI.div(UI.button("A").id("a"), UI.span("B").id("b"))) {
            for
                _ <- Browser.assertText(Selector.id("a"), "A")
                _ <- Browser.assertText(Selector.id("b"), "B")
            yield ()
        }
    }

    // ---- hidden ----

    "hidden true" in {
        withUI(UI.div(UI.div("content").hidden(true).id("d"))) {
            for
                _ <- Browser.assertAttribute(Selector.id("d"), "hidden", "")
            yield ()
        }
    }

    "hidden false" in {
        withUI(UI.div(UI.div("content").hidden(false).id("d"))) {
            for
                _ <- Browser.assertText(Selector.id("d"), "content")
            yield ()
        }
    }

    "no hidden set" in {
        withUI(UI.div(UI.div("content").id("d"))) {
            for
                _ <- Browser.assertText(Selector.id("d"), "content")
            yield ()
        }
    }

    "hidden signal toggle" in {
        val app: UI < Async =
            for flag <- Signal.initRef(false)
            yield UI.div(
                UI.button("Toggle").id("t").onClick(flag.getAndUpdate(!_).unit),
                UI.div("content").id("d").hidden(flag)
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("d"), "content")
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.assertAttribute(Selector.id("d"), "hidden", "")
            yield ()
        }
    }

    // ---- style ----

    "style bold" in {
        withUI(UI.div(UI.div("X").style(Style.bold).id("d"))) {
            Browser.assertAttributeSatisfies(Selector.id("d"), "style", "ignore")(_.contains("font-weight")).unit
        }
    }

    "style italic" in {
        withUI(UI.div(UI.div("X").style(Style.italic).id("d"))) {
            Browser.assertText(Selector.id("d"), "X").unit
        }
    }

    "style bg color" in {
        withUI(UI.div(UI.div("X").style(Style.bg(Style.Color.red)).id("d"))) {
            Browser.assertText(Selector.id("d"), "X").unit
        }
    }

    "style width" in {
        withUI(UI.div(UI.div.style(Style.width(Length.Px(200))).id("d"))) {
            Browser.assertAttributeSatisfies(Selector.id("d"), "style", "ignore")(_.contains("200px")).unit
        }
    }

    "style row" in {
        withUI(UI.div(UI.div.style(Style.row).id("d"))) {
            Browser.assertAttributeSatisfies(Selector.id("d"), "style", "ignore")(_.contains("flex-direction: row")).unit
        }
    }

    "style composed bold + italic" in {
        withUI(UI.div(UI.div("X").style(Style.bold ++ Style.italic).id("d"))) {
            for
                _ <- Browser.assertAttributeSatisfies(Selector.id("d"), "style", "ignore")(_.contains("font-weight"))
                _ <- Browser.assertAttributeSatisfies(Selector.id("d"), "style", "ignore")(_.contains("font-style"))
            yield ()
        }
    }

    "empty style no attribute" in {
        withUI(UI.div(UI.div.id("d"))) {
            for
                _ <- Browser.assertAttributeSatisfies(Selector.id("d"), "style", "no style attribute")(_.isEmpty)
            yield ()
        }
    }

    // ---- Merged from ReactiveAttrsTest ----

    "style signal initial" in {
        val app: UI < Async =
            for ref <- Signal.initRef(Style.bold)
            yield UI.div(ref.map(s => UI.div("X").style(s).id("d")))
        withUI(app) {
            for
                _ <- Browser.assertAttributeSatisfies(Selector.id("d"), "style", "ignore")(_.contains("font-weight"))
                _ <- Browser.assertText(Selector.id("d"), "X")
            yield ()
        }
    }

    "style signal toggle bold to italic" in {
        val app: UI < Async =
            for ref <- Signal.initRef(Style.bold)
            yield UI.div(
                UI.button("Toggle").id("t").onClick(ref.set(Style.italic)),
                ref.map(s => UI.div("X").style(s).id("d"))
            )
        withUI(app) {
            for
                _ <- Browser.assertAttributeSatisfies(Selector.id("d"), "style", "ignore")(_.contains("font-weight"))
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.assertAttributeSatisfies(Selector.id("d"), "style", "ignore")(_.contains("font-style"))
            yield ()
        }
    }

    "style signal toggle on off" in {
        val app: UI < Async =
            for ref <- Signal.initRef(Style.bold)
            yield UI.div(
                UI.button("Off").id("t").onClick(ref.set(Style.empty)),
                ref.map(s => UI.div("X").style(s).id("d"))
            )
        withUI(app) {
            for
                _ <- Browser.assertAttributeSatisfies(Selector.id("d"), "style", "ignore")(_.nonEmpty)
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.assertAttributeSatisfies(Selector.id("d"), "style", "ignore")(_.isEmpty)
            yield ()
        }
    }

    "style lambda" in {
        withUI(UI.div(UI.div("X").style(s => s.bold ++ s.italic).id("d"))) {
            for
                _ <- Browser.assertAttributeSatisfies(Selector.id("d"), "style", "ignore")(_.contains("font-weight"))
                _ <- Browser.assertAttributeSatisfies(Selector.id("d"), "style", "ignore")(_.contains("font-style"))
            yield ()
        }
    }

    "radio checked signal" in {
        val app: UI < Async =
            for flag <- Signal.initRef(false)
            yield UI.div(
                UI.button("Check").id("t").onClick(flag.set(true)),
                flag.map(v => UI.radio.checked(v).id("r"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.assertChecked(Selector.id("r"))
            yield ()
        }
    }

    "hidden signal true not rendered" in {
        val app: UI < Async =
            for ref <- Signal.initRef(true)
            yield UI.div(ref.map(h => UI.div("hidden content").id("d").hidden(h)))
        withUI(app) {
            Browser.assertAttribute(Selector.id("d"), "hidden", "").unit
        }
    }

    "hidden signal toggle (reactive)" in {
        val app: UI < Async =
            for flag <- Signal.initRef(false)
            yield UI.div(
                UI.button("Toggle").id("t").onClick(flag.getAndUpdate(!_).unit),
                flag.map(h => UI.div("content").id("d").hidden(h))
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("d"), "content")
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.assertAttribute(Selector.id("d"), "hidden", "")
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.assertText(Selector.id("d"), "content")
            yield ()
        }
    }

    "hidden signal with interactive child" in {
        val app: UI < Async =
            for
                hidden  <- Signal.initRef(true)
                counter <- Signal.initRef(0)
            yield UI.div(
                UI.button("Show").id("show").onClick(hidden.set(false)),
                UI.when(hidden.map(!_))(UI.button("Click").id("btn").onClick(counter.getAndUpdate(_ + 1).unit)),
                counter.map(n => UI.span(n.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("show"))
                _ <- Browser.click(Selector.id("btn"))
                _ <- Browser.assertText(Selector.id("v"), "1")
            yield ()
        }
    }

    "anchor href signal initial" in {
        val app: UI < Async =
            for ref <- Signal.initRef(Href.Absolute(HttpUrl.parse("https://example.com").getOrThrow): Href)
            yield UI.div(ref.map(url => UI.a.href(url).id("a")("Link")))
        withUI(app) {
            for
                _ <- Browser.assertAttributeSatisfies(Selector.id("a"), "href", "ignore")(_.contains("example.com"))
                _ <- Browser.assertText(Selector.id("a"), "Link")
            yield ()
        }
    }

    "anchor href signal change" in {
        val app: UI < Async =
            for ref <- Signal.initRef(Href.Absolute(HttpUrl.parse("https://old.com").getOrThrow): Href)
            yield UI.div(
                UI.button("Change").id("t").onClick(ref.set(Href.Absolute(HttpUrl.parse("https://new.com").getOrThrow))),
                ref.map(url => UI.a.href(url).id("a")("Link"))
            )
        withUI(app) {
            for
                _ <- Browser.assertAttributeSatisfies(Selector.id("a"), "href", "ignore")(_.contains("old.com"))
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.assertAttributeSatisfies(Selector.id("a"), "href", "ignore")(_.contains("new.com"))
            yield ()
        }
    }

    "img src signal initial" in {
        val app: UI < Async =
            for ref <- Signal.initRef(ImgSrc.Path("img1.png"): ImgSrc)
            yield UI.div(ref.map(s => UI.img(s, "photo").id("i")))
        withUI(app) {
            Browser.assertAttributeSatisfies(Selector.id("i"), "src", "ignore")(_.contains("img1.png")).unit
        }
    }

    "img src signal change" in {
        val app: UI < Async =
            for ref <- Signal.initRef(ImgSrc.Path("img1.png"): ImgSrc)
            yield UI.div(
                UI.button("Change").id("t").onClick(ref.set(ImgSrc.Path("img2.png"))),
                ref.map(s => UI.img(s, "photo").id("i"))
            )
        withUI(app) {
            for
                _ <- Browser.assertAttributeSatisfies(Selector.id("i"), "src", "ignore")(_.contains("img1.png"))
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.assertAttributeSatisfies(Selector.id("i"), "src", "ignore")(_.contains("img2.png"))
            yield ()
        }
    }

    "opt selected true" in {
        withUI(UI.div(UI.select.id("s")(
            UI.option.value("a")("A"),
            UI.option.value("b").selected(true)("B")
        ))) {
            Browser.assertAttribute(Selector.id("s"), "value", "b").unit
        }
    }

    "opt selected signal toggle" in {
        val app: UI < Async =
            for flag <- Signal.initRef(false)
            yield UI.div(
                UI.button("Select").id("t").onClick(flag.set(true)),
                flag.map { v =>
                    UI.select.id("s")(
                        UI.option.value("a")("A"),
                        UI.option.value("b").selected(v)("B")
                    )
                }
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.assertAttribute(Selector.id("s"), "value", "b")
            yield ()
        }
    }

    // ---- title ----

    "title from a constant is the title attribute, from a signal a channel, from Absent nothing" in {
        val constant: Prop[String]    = "Save the draft"
        val none: Maybe[Prop[String]] = Absent
        val el                        = UI.span
        for
            ref  <- Signal.initRef("Saved")
            html <- kyo.internal.HtmlRenderer.render(UI.div(UI.span.title(constant)), Seq.empty)
            live = UI.span.title(Present(ref): Maybe[Prop[String]])
        yield
            assert(html.contains("""title="Save the draft""""), html)
            assert(live.attrs.reactiveAttrs.get("title").exists(_ eq ref), s"${live.attrs}")
            assert(el.title(none) eq el)
        end for
    }

    // ---- a Boolean aria value ----

    "a Boolean aria value writes \"true\" and \"false\", from a constant or a signal" in {
        def html(ui: UI)(using Frame) = kyo.internal.HtmlRenderer.render(UI.div(ui), Seq.empty)
        for
            on     <- Signal.initRef(true)
            off    <- Signal.initRef(false)
            yes    <- html(UI.span.aria(Aria.disabled -> true))
            no     <- html(UI.span.aria(Aria.disabled -> false))
            sigYes <- html(UI.span.aria(Aria.disabled -> (on: Signal[Boolean])))
            sigNo  <- html(UI.span.aria(Aria.disabled -> (off: Signal[Boolean])))
        yield
            assert(yes.contains("""aria-disabled="true"""") && sigYes.contains("""aria-disabled="true""""), s"$yes / $sigYes")
            assert(no.contains("""aria-disabled="false"""") && sigNo.contains("""aria-disabled="false""""), s"$no / $sigNo")
        end for
    }

    // ---- silent ----

    private val clickEv    = """ data-kyo-ev="click""""
    private val dropdownEv = """ data-kyo-ev="click,keydown,change""""

    "a constantly silent or disabled element writes no data-kyo-ev: the bytes of the same element with no handler" in {
        def html(ui: UI)(using Frame) = kyo.internal.HtmlRenderer.render(UI.div(ui), Seq.empty)
        for
            silentSpan   <- html(UI.span("x").onClick(Kyo.unit).silent(true))
            plainSpan    <- html(UI.span("x"))
            silentButton <- html(UI.button("Go").onClick(Kyo.unit).silent(true))
            plainButton  <- html(UI.button("Go"))
            offButton    <- html(UI.button("Go").onClick(Kyo.unit).disabled(true))
            offPlain     <- html(UI.button("Go").disabled(true))
            heard        <- html(UI.span("x").onClick(Kyo.unit).silent(false))
            handler      <- html(UI.span("x").onClick(Kyo.unit))
        yield
            assert(silentSpan == plainSpan, s"$silentSpan / $plainSpan")
            assert(silentButton == plainButton, s"$silentButton / $plainButton")
            assert(offButton == offPlain, s"$offButton / $offPlain")
            assert(heard == handler && heard.contains(clickEv), s"$heard / $handler")
        end for
    }

    "a partly silent element writes data-kyo-ev with only the events it keeps, from a constant or a signal" in {
        def html(ui: UI)(using Frame) = kyo.internal.HtmlRenderer.render(UI.div(ui), Seq.empty)
        val button                    = UI.button("Go").onClick(Kyo.unit).onBlur(Kyo.unit)
        for
            on       <- Signal.initRef(true)
            off      <- Signal.initRef(false)
            heard    <- html(button.silentExcept(false, "blur"))
            kept     <- html(button.silentExcept(true, "blur"))
            muted    <- html(button.silentExcept(on: Signal[Boolean], "blur"))
            unmuted  <- html(button.silentExcept(off: Signal[Boolean], "blur"))
            nothing  <- html(button.silentExcept(true, "keyup"))
            allQuiet <- html(button.silentExcept(true, "blur").silent(true))
        yield
            assert(heard.contains(""" data-kyo-ev="click,blur""""), s"not silent: $heard")
            assert(kept.contains(""" data-kyo-ev="blur""""), s"constant partial: $kept")
            assert(muted.contains(""" data-kyo-ev="blur""""), s"signal partial on: $muted")
            assert(unmuted.contains(""" data-kyo-ev="click,blur""""), s"signal partial off: $unmuted")
            assert(!nothing.contains("data-kyo-ev"), s"no kept event writes no attribute: $nothing")
            assert(!allQuiet.contains("data-kyo-ev"), s"fully silent writes no attribute: $allQuiet")
        end for
    }

    "a partly silent text field that keeps blur also keeps blurtext, so the blur still carries its text" in {
        def html(ui: UI)(using Frame) = kyo.internal.HtmlRenderer.render(UI.div(ui), Seq.empty)
        val field                     = UI.input.onBlur((_, _) => Kyo.unit)
        for
            heard <- html(field)
            kept  <- html(field.silentExcept(true, "blur"))
        yield
            assert(heard.contains(""" data-kyo-ev="blur,blurtext""""), s"not silent: $heard")
            assert(kept.contains(""" data-kyo-ev="blur,blurtext""""), s"partly silent drops blurtext: $kept")
        end for
    }

    "silent or disabled from a signal writes data-kyo-ev for the current state, and the path either way" in {
        def html(ui: UI)(using Frame) = kyo.internal.HtmlRenderer.render(UI.div(ui), Seq.empty)
        for
            on       <- Signal.initRef(true)
            off      <- Signal.initRef(false)
            handler  <- html(UI.span("x").onClick(Kyo.unit))
            quiet    <- html(UI.span("x").onClick(Kyo.unit).silent(on: Signal[Boolean]))
            heard    <- html(UI.span("x").onClick(Kyo.unit).silent(off: Signal[Boolean]))
            disabled <- html(UI.button("Go").onClick(Kyo.unit).disabled(on: Signal[Boolean]))
            enabled  <- html(UI.button("Go").onClick(Kyo.unit).disabled(off: Signal[Boolean]))
            fixedOff <- html(UI.button("Go").onClick(Kyo.unit).silent(off: Signal[Boolean]).disabled(true))
        yield
            assert(handler.contains(clickEv) && handler.contains("data-kyo-path"), handler)
            assert(quiet == handler.replace(clickEv, ""), s"$quiet / $handler")
            assert(heard == handler, s"$heard / $handler")
            assert(!disabled.contains("data-kyo-ev") && disabled.contains(" disabled"), disabled)
            assert(enabled.contains(clickEv) && !enabled.contains(" disabled"), enabled)
            assert(!fixedOff.contains("data-kyo-ev"), fixedOff)
        end for
    }

    "a silent or disabled dropdown writes no data-kyo-ev on its wrapper, from a constant or a signal" in {
        def html(ui: UI)(using Frame) = kyo.internal.HtmlRenderer.render(UI.div(ui), Seq.empty)
        for
            on       <- Signal.initRef(true)
            off      <- Signal.initRef(false)
            heard    <- html(UI.dropdown("A" -> "a").id("d"))
            quiet    <- html(UI.dropdown("A" -> "a").id("d").silent(true))
            follows  <- html(UI.dropdown("A" -> "a").id("d").silent(on: Signal[Boolean]))
            listens  <- html(UI.dropdown("A" -> "a").id("d").silent(off: Signal[Boolean]))
            disabled <- html(UI.dropdown("A" -> "a").id("d").disabled(true))
        yield
            assert(heard.contains(dropdownEv), heard)
            assert(quiet == heard.replace(dropdownEv, ""), s"$quiet / $heard")
            assert(follows == quiet, s"$follows / $quiet")
            assert(listens == heard, s"$listens / $heard")
            assert(!disabled.contains("data-kyo-ev") && !disabled.contains("data-kyo-disabled"), disabled)
        end for
    }

    "a dropdown that turns silent while its list is open sends no option pick, by a click or by Enter, and a listening one's does" in {
        // Every message the page sends over its socket, recorded, so what the client forwards is seen apart from what the server
        // would do with it.
        val record =
            """window.__kyoSent=[];var __o=WebSocket.prototype.send;WebSocket.prototype.send=function(m){window.__kyoSent.push(String(m));return __o.apply(this,arguments);};"""
        def changes(using Frame) = Browser.evalJson[Int]("""window.__kyoSent.filter(function(m){return m.indexOf("Change")>=0;}).length""")
        val app: UI < Async      =
            for
                quiet <- Signal.initRef("a")
                heard <- Signal.initRef("a")
                mute  <- Signal.initRef(false)
            yield UI.div(
                // Silent through a signal that turns on once the list is open: a silent dropdown does not open, so this is the
                // way a pick can still be made on one.
                UI.dropdown("Alpha" -> "a", "Beta" -> "b").id("q").value(quiet).silent(mute: Signal[Boolean]),
                UI.button("mute").id("m").onClick(mute.set(true)),
                UI.button("unmute").id("u").onClick(mute.set(false)),
                mute.map(v => UI.span(s"muted:$v").id("mv")),
                UI.dropdown("Alpha" -> "a", "Beta" -> "b").id("h").value(heard),
                heard.map(v => UI.span(s"heard:$v").id("hv"))
            )
        withUI(app) {
            for
                _      <- Browser.evalDiscard(record)
                _      <- Browser.click(Selector.id("q-trigger"))
                _      <- Browser.click(Selector.id("m"))
                _      <- Browser.assertText(Selector.id("mv"), "muted:true")
                _      <- Browser.click(Selector.css("#q-options [data-kyo-dropdown-opt='1']"))
                _      <- Browser.click(Selector.id("u"))
                _      <- Browser.assertText(Selector.id("mv"), "muted:false")
                _      <- Browser.click(Selector.id("q-trigger"))
                _      <- Browser.click(Selector.id("m"))
                _      <- Browser.assertText(Selector.id("mv"), "muted:true")
                muted  <- Browser.evalJson[Boolean]("""!document.getElementById("q").hasAttribute("data-kyo-ev")""")
                _      <- Browser.press(Selector.id("q-trigger"), Key.ArrowDown)
                _      <- Browser.press(Selector.id("q-trigger"), Key.Enter)
                silent <- changes
                _      <- Browser.click(Selector.id("h-trigger"))
                _      <- Browser.click(Selector.css("#h-options [data-kyo-dropdown-opt='1']"))
                _      <- Browser.assertText(Selector.id("hv"), "heard:b")
                heard  <- changes
            yield assert(
                muted && silent == 0 && heard == 1,
                s"muted wrapper lists no events: $muted; Change messages: silent dropdown $silent, listening one ${heard - silent}"
            )
        }
    }

    "a silent or disabled dropdown's list does not open, by a click or by Space, and a listening one's does" in {
        def shown(id: String)(using Frame) =
            Browser.evalJson[Boolean](s"""!document.getElementById("$id-options").hidden""")
        val app: UI < Async =
            for
                quiet <- Signal.initRef("a")
                heard <- Signal.initRef("a")
                gate  <- Signal.initRef(true)
            yield UI.div(
                UI.dropdown("Alpha" -> "a", "Beta" -> "b").id("q").value(quiet).silent(true),
                UI.dropdown("Alpha" -> "a", "Beta" -> "b").id("d").value(quiet).disabled(true),
                UI.dropdown("Alpha" -> "a", "Beta" -> "b").id("s").value(quiet).silent(gate: Signal[Boolean]),
                UI.dropdown("Alpha" -> "a", "Beta" -> "b").id("h").value(heard)
            )
        withUI(app) {
            for
                _      <- Browser.click(Selector.id("q-trigger"))
                qClick <- shown("q")
                _      <- Browser.press(Selector.id("q-trigger"), Key.Space)
                qSpace <- shown("q")
                _      <- Browser.click(Selector.id("d-trigger"))
                dClick <- shown("d")
                _      <- Browser.click(Selector.id("s-trigger"))
                sClick <- shown("s")
                _      <- Browser.click(Selector.id("h-trigger"))
                hClick <- shown("h")
            yield assert(
                !qClick && !qSpace && !dClick && !sClick && hClick,
                s"open after: silent click $qClick, silent Space $qSpace, disabled click $dClick, signal-silent click $sClick, listening click $hClick"
            )
        }
    }

    // ---- class name channel ----

    "cssClass.matching over a signal of class names writes the class it holds, after the fixed ones, and none for Absent" in {
        for
            size <- Signal.initRef[Maybe[String]](Present("p-sm"))
            none <- Signal.initRef[Maybe[String]](Absent)
            held <- kyo.internal.HtmlRenderer.render(
                UI.div(UI.span.cssClass("p-x").cssClass.matching(size: Signal[Maybe[String]])(identity)),
                Seq.empty
            )
            empty <- kyo.internal.HtmlRenderer.render(
                UI.div(UI.span.cssClass("p-x").cssClass.matching(none: Signal[Maybe[String]])(identity)),
                Seq.empty
            )
        yield
            assert(held.contains("""class="p-x p-sm""""), held)
            assert(empty.contains("""class="p-x"""") && !empty.contains("p-sm"), empty)
        end for
    }

    // ---- a button's form ----

    "a button's form is the native form attribute, the id of the form it submits" in {
        def html(ui: UI)(using Frame) = kyo.internal.HtmlRenderer.render(UI.div(ui), Seq.empty)
        for
            withForm <- html(UI.button("Save").form("profile"))
            plain    <- html(UI.button("Save"))
        yield
            assert(withForm.contains(""" form="profile""""), withForm)
            assert(!plain.contains(" form="), plain)
        end for
    }

end AttrsTest
