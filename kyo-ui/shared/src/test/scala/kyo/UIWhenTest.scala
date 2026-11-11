package kyo

import kyo.UI.*
import kyo.UI.Ast.*

/** `Element.when`: a setter under a condition, or over an optional value, kept in the chain. */
class UIWhenTest extends kyo.test.Test[Any]:

    "when(true) applies the setter and keeps the element's type" in {
        val b: Button = UI.button("x").when(true)(_.id("on"))
        assert(b.attrs.identifier == Present("on"), s"${b.attrs}")
    }

    "when(false) leaves the element as it is" in {
        val el = UI.button("x")
        assert(el.when(false)(_.id("on")) eq el)
    }

    "when(Present) hands the setter the value" in {
        val name: Maybe[String] = Present("Save")
        val b                   = UI.button("x").when(name)((e, n) => e.aria(AriaKey.label -> n))
        assert(b.attrs.ariaAttrs.get("label").contains("Save"), s"${b.attrs}")
    }

    "when(Absent) leaves the element as it is" in {
        val el                  = UI.button("x")
        val name: Maybe[String] = Absent
        assert(el.when(name)((e, n) => e.aria(AriaKey.label -> n)) eq el)
    }

    "when chains: each condition decides its own setter" in {
        val b = UI.div.when(true)(_.cssClass("a")).when(false)(_.cssClass("b")).when(Present("c"))(_.cssClass(_))
        assert(b.attrs.cssClasses == Chunk("a", "c"), s"${b.attrs.cssClasses}")
    }

    // ---- when over a Prop[Boolean]: the Toggle ----

    private def html(ui: UI)(using Frame): String < Sync = kyo.internal.HtmlRenderer.render(UI.div(ui), Seq.empty)

    /** The attributes of the first `<tag` in `html`, `data-kyo-path` left out, as a set: a channel writes its attribute after the
      * element's own, so the order differs from a constant's while the attributes are the same.
      */
    private def attrSet(html: String, tag: String): Set[String] =
        val start = html.indexOf(s"<$tag")
        val end   = html.indexOf('>', start)
        val text  = html.substring(start + tag.length + 1, end).stripSuffix("/")
        """\s([a-zA-Z-]+)(="[^"]*")?""".r.findAllMatchIn(text).map(_.matched.trim).filterNot(_.startsWith("data-kyo-path")).toSet
    end attrSet

    private def noPath(s: String): String = s.replaceAll(""" data-kyo-path="[^"]*"""", "")

    "a Boolean picks when(Boolean), whose setters are all of the element's; a signal or a prop picks the toggle" in {
        typeCheck("""
            import kyo.*
            val flag: Boolean = true
            UI.button("x").when(flag)(_.onClick(Kyo.unit))
        """)
        typeCheckFailure("""
            import kyo.*
            def f(s: Signal[Boolean]) = UI.button("x").when(s)(_.onClick(Kyo.unit))
        """)("value onClick is not a member of kyo.UI.Ast.Toggle")
        typeCheckFailure("""
            import kyo.*
            def f(p: UI.Prop[Boolean]) = UI.div.when(p)(_.style(_.bold))
        """)("value style is not a member of kyo.UI.Ast.Toggle")
    }

    "disabled, readOnly and required are on a toggle only where the element has them" in {
        typeCheck("""
            import kyo.*
            def f(s: Signal[Boolean]) = (UI.button("x").when(s)(_.disabled(true)), UI.input.when(s)(_.readOnly(true).required(true)))
        """)
        typeCheckFailure("""
            import kyo.*
            def f(s: Signal[Boolean]) = UI.div.when(s)(_.disabled(true))
        """)("Cannot prove that kyo.UI.Ast.Div <:< kyo.UI.Ast.HasDisabled")
        typeCheckFailure("""
            import kyo.*
            def f(s: Signal[Boolean]) = UI.button("x").when(s)(_.readOnly(true))
        """)("Cannot prove that kyo.UI.Ast.Button <:< kyo.UI.Ast.TextInput")
    }

    "over a constant prop the toggle is the element's own setters: true the bytes of when(true), false the element as it is" in {
        val f = (t: Toggle[Button]) =>
            t.cssClass("x", "y").aria(AriaKey.pressed -> "true").data(data"data-k" -> "v").role("switch").disabled(true)
        val g = (b: Button) =>
            b.cssClass("x", "y").aria(AriaKey.pressed -> "true").data(data"data-k" -> "v").role("switch").disabled(true)
        val on  = true: Prop[Boolean]
        val off = false: Prop[Boolean]
        val el  = UI.button("Go")
        for
            viaProp <- html(el.when(on)(f))
            viaBool <- html(el.when(true)(g))
        yield
            assert(viaProp == viaBool, s"$viaProp | $viaBool")
            assert(el.when(off)(f) eq el)
        end for
    }

    "classes over a signal: in place, the bytes of the hand-written channels, the constant's bytes but for the path" in {
        val f    = (t: Toggle[Div]) => t.cssClass("p-fluid", "p-wide")
        val base = UI.div("x").cssClass("p-root")
        for
            on   <- Signal.initRef(true)
            off  <- Signal.initRef(false)
            sOn  <- html(base.when(on: Signal[Boolean])(f))
            sOff <- html(base.when(off: Signal[Boolean])(f))
            hOn  <- html(base.withReactiveClass("p-fluid", on).withReactiveClass("p-wide", on))
            hOff <- html(base.withReactiveClass("p-fluid", off).withReactiveClass("p-wide", off))
            cOn  <- html(base.when(true)(_.cssClass("p-fluid", "p-wide")))
            cOff <- html(base.when(false)(_.cssClass("p-fluid", "p-wide")))
        yield
            assert(sOn == hOn && sOff == hOff, s"$sOn / $hOn | $sOff / $hOff")
            assert(noPath(sOn) == cOn && noPath(sOff) == cOff, s"$sOn / $cOn | $sOff / $cOff")
            assert(sOn.contains("""class="p-root p-fluid p-wide"""") && sOff.contains("""class="p-root""""), s"$sOn | $sOff")
        end for
    }

    "a toggled class is written after the fixed ones, sorted, and one the element carries stays on with no channel" in {
        for
            s <- Signal.initRef(true)
            sig   = s: Signal[Boolean]
            later = UI.div("x").cssClass("z").when(sig)(_.cssClass("b")).cssClass("a")
            kept  = UI.div("x").cssClass("a").when(sig)(_.cssClass("a"))
            laterHtml <- html(later)
            keptHtml  <- html(kept)
        yield
            assert(laterHtml.contains("""class="z a b""""), laterHtml)
            assert(kept.attrs.reactiveClasses.isEmpty && keptHtml.contains("""class="a""""), keptHtml)
        end for
    }

    "aria, data, role and disabled over a signal: the attributes of the constant in each state, one of each name" in {
        val f    = (t: Toggle[Button]) => t.aria(AriaKey.pressed -> "true").data(data"data-k" -> "on").role("switch").disabled(true)
        val g    = (b: Button) => b.aria(AriaKey.pressed -> "true").data(data"data-k" -> "on").role("switch").disabled(true)
        val base = UI.button("Go").aria(AriaKey.pressed -> "false").data(data"data-k" -> "off")
        for
            on   <- Signal.initRef(true)
            off  <- Signal.initRef(false)
            sOn  <- html(base.when(on: Signal[Boolean])(f))
            sOff <- html(base.when(off: Signal[Boolean])(f))
            cOn  <- html(base.when(true)(g))
            cOff <- html(base.when(false)(g))
        yield
            assert(attrSet(sOn, "button") == attrSet(cOn, "button"), s"$sOn / $cOn")
            assert(attrSet(sOff, "button") == attrSet(cOff, "button"), s"$sOff / $cOff")
            assert(sOn.split("aria-pressed").length == 2 && sOff.split("aria-pressed").length == 2, s"$sOn | $sOff")
            assert(sOn.split("data-k=").length == 2 && sOff.split("data-k=").length == 2, s"$sOn | $sOff")
        end for
    }

    "a change patches class, aria and disabled in place, with nothing drawn again" in {
        import kyo.internal.ReactiveUI
        import kyo.internal.RecordingExchange
        import kyo.internal.RecordingExchange.Op
        def ops(exchange: RecordingExchange): Chunk[String] = exchange.recorded.collect {
            case op: Op.Attr     => s"${op.name}=${op.value}"
            case op: Op.BoolAttr => s"${op.name} ${if op.value then "on" else "off"}"
            case op: Op.Class    => s"class ${op.name} ${if op.on then "on" else "off"}"
            case _: Op.Paint     => "paint"
        }
        Scope.run {
            Kyo.foreachDiscard(Chunk(true, false)) { fast =>
                val exchange = new RecordingExchange(synchronous = fast)
                for
                    s <- Signal.initRef(false)
                    ui = UI.button("Go").when(s: Signal[Boolean])(_.cssClass("x").aria(AriaKey.pressed -> "true").disabled(true))
                    root <- ReactiveUI.normalize(UI.div(ui), Seq.empty)
                    _    <- ReactiveUI.subscribe(root, exchange)
                    _    <- s.set(true)
                    _    <- assertEventually(Sync.defer(ops(exchange).size == 3))
                    _    <- s.set(false)
                    _    <- assertEventually(Sync.defer(ops(exchange).size == 6))
                yield
                    val seen = ops(exchange)
                    assert(
                        seen.take(3).toSet == Set("class x on", "aria-pressed=true", "disabled on") &&
                            seen.drop(3).toSet == Set("class x off", "aria-pressed off", "disabled off") &&
                            !seen.contains("paint"),
                        s"fast=$fast: $seen"
                    )
                end for
            }
        }
    }

    "a channel a setter bound is the value in both states: the toggle sets nothing there, as a constant shows nothing there" in {
        for
            chan <- Signal.initRef(false)
            name <- Signal.initRef("bound")
            s    <- Signal.initRef(true)
            base    = UI.button("Go").disabled(chan: Signal[Boolean]).aria(AriaKey.label -> (name: Signal[String]))
            toggled = base.when(s: Signal[Boolean])(_.disabled(true).aria(AriaKey.label -> "fixed"))
            sOn   <- html(toggled)
            cOn   <- html(base.when(true)(_.disabled(true).aria(AriaKey.label -> "fixed")))
            plain <- html(base)
        yield
            assert(toggled.attrs.toggled.isEmpty, s"${toggled.attrs}")
            assert(sOn == plain && cOn == plain, s"$sOn / $cOn / $plain")
        end for
    }

    "two toggles on one name compose as written: the later one's value while its signal holds, else the earlier one's" in {
        given CanEqual[Maybe[String], Maybe[String]] = CanEqual.derived
        def pressed(html: String): Maybe[String]     =
            """aria-pressed="([^"]*)"""".r.findFirstMatchIn(html).fold(Absent: Maybe[String])(m => Present(m.group(1)))
        for
            a <- Signal.initRef(false)
            b <- Signal.initRef(false)
            ui =
                UI.button(
                    "Go"
                ).when(a: Signal[Boolean])(_.aria(AriaKey.pressed -> "true")).when(b: Signal[Boolean])(_.aria(AriaKey.pressed -> "mixed"))
            none   <- html(ui)
            _      <- a.set(true)
            first  <- html(ui)
            _      <- b.set(true)
            both   <- html(ui)
            _      <- a.set(false)
            second <- html(ui)
        yield assert(
            Chunk(pressed(none), pressed(first), pressed(both), pressed(second)) ==
                Chunk[Maybe[String]](Absent, Present("true"), Present("mixed"), Present("mixed")),
            s"$none | $first | $both | $second"
        )
        end for
    }

    "silent over a signal is the silence channel, the constant's bytes but for the path" in {
        val base = UI.span("x").onClick(Kyo.unit)
        for
            on   <- Signal.initRef(true)
            off  <- Signal.initRef(false)
            sOn  <- html(base.when(on: Signal[Boolean])(_.silent(true)))
            sOff <- html(base.when(off: Signal[Boolean])(_.silent(true)))
            cOn  <- html(base.when(true)(_.silent(true)))
            cOff <- html(base.when(false)(_.silent(true)))
        yield
            assert(base.when(on: Signal[Boolean])(_.silent(true)).attrs.silentSignal.nonEmpty)
            assert(noPath(sOn) == noPath(cOn) && noPath(sOff) == noPath(cOff), s"$sOn/$cOn | $sOff/$cOff")
        end for
    }

    "hidden, readOnly and required over a signal: the attributes of the constant in each state" in {
        val f    = (t: Toggle[Input]) => t.hidden(true).readOnly(true).required(true)
        val g    = (i: Input) => i.hidden(true).readOnly(true).required(true)
        val base = UI.input
        for
            on   <- Signal.initRef(true)
            off  <- Signal.initRef(false)
            sOn  <- html(base.when(on: Signal[Boolean])(f))
            sOff <- html(base.when(off: Signal[Boolean])(f))
            cOn  <- html(base.when(true)(g))
            cOff <- html(base.when(false)(g))
        yield assert(
            attrSet(sOn, "input") == attrSet(cOn, "input") && attrSet(sOff, "input") == attrSet(cOff, "input"),
            s"$sOn/$cOn | $sOff/$cOff"
        )
        end for
    }

    // ---- entries on a toggle ----

    /** The value of `aria-$name` in `html`, if written. */
    private def ariaOf(name: String)(html: String): Maybe[String] =
        s"""aria-$name="([^"]*)"""".r.findFirstMatchIn(html).fold(Absent: Maybe[String])(m => Present(m.group(1)))

    "a signal entry under a signal condition is one channel: its value while the condition holds, the element's while not" in {
        given CanEqual[Maybe[String], Maybe[String]] = CanEqual.derived
        for
            s     <- Signal.initRef(false)
            label <- Signal.initRef("a")
            ui = UI.button("Go").aria(AriaKey.label -> "base").when(s: Signal[Boolean])(_.aria(AriaKey.label -> (label: Signal[String])))
            off   <- html(ui)
            _     <- s.set(true)
            on    <- html(ui)
            _     <- label.set("b")
            moved <- html(ui)
            _     <- s.set(false)
            back  <- html(ui)
        yield
            assert(ui.attrs.toggled.contains("aria-label") && ui.attrs.ariaAttrs.isEmpty, s"${ui.attrs}")
            assert(
                Chunk(off, on, moved, back).map(ariaOf("label")) ==
                    Chunk[Maybe[String]](Present("base"), Present("a"), Present("b"), Present("base")),
                s"$off | $on | $moved | $back"
            )
        end for
    }

    "a signal of a Maybe that holds Absent while the condition holds leaves the element's value" in {
        given CanEqual[Maybe[String], Maybe[String]] = CanEqual.derived
        for
            s     <- Signal.initRef(true)
            label <- Signal.initRef[Maybe[String]](Absent)
            ui = UI.button("Go").aria(AriaKey.label -> "base").when(s: Signal[Boolean])(_.aria(AriaKey.label ->
                (label: Signal[Maybe[String]])))
            absent <- html(ui)
            _      <- label.set(Present("x"))
            held   <- html(ui)
        yield assert(
            Chunk(absent, held).map(ariaOf("label")) == Chunk[Maybe[String]](Present("base"), Present("x")),
            s"$absent | $held"
        )
        end for
    }

    "a later toggle with a signal entry composes with an earlier one as a constant does" in {
        given CanEqual[Maybe[String], Maybe[String]] = CanEqual.derived
        for
            a     <- Signal.initRef(true)
            b     <- Signal.initRef(false)
            mixed <- Signal.initRef("mixed")
            ui = UI.button("Go").when(a: Signal[Boolean])(_.aria(AriaKey.pressed -> true)).when(b: Signal[Boolean])(
                _.aria(AriaKey.pressed -> (mixed: Signal[String]))
            )
            first <- html(ui)
            _     <- b.set(true)
            both  <- html(ui)
            _     <- a.set(false)
            later <- html(ui)
        yield assert(
            Chunk(first, both, later).map(ariaOf("pressed")) == Chunk[Maybe[String]](Present("true"), Present("mixed"), Present("mixed")),
            s"$first | $both | $later"
        )
        end for
    }

    "over a constant condition a signal entry is the element's own channel, and Absent sets nothing" in {
        val none: Maybe[String] = Absent
        for
            label <- Signal.initRef("a")
            on  = true: Prop[Boolean]
            off = false: Prop[Boolean]
            el  = UI.button("Go")
        yield
            val bound =
                el.when(on)(_.aria(AriaKey.label -> (label: Signal[String]), AriaKey.describedby -> none).data(data"data-n" -> 3))
            assert(bound.attrs.reactiveAttrs.get("aria-label").exists(_ eq label) && bound.attrs.toggled.isEmpty, s"${bound.attrs}")
            assert(bound.attrs.ariaAttrs.isEmpty && bound.attrs.dataAttrs == Map("n" -> "3"), s"${bound.attrs}")
            assert(el.when(off)(_.aria(AriaKey.label -> (label: Signal[String]))) eq el)
        end for
    }

    "Boolean, number and Absent entries over a signal: the constant's attributes in each state" in {
        val none: Maybe[String] = Absent
        val f                   = (t: Toggle[Button]) => t.aria(AriaKey.busy -> true, AriaKey.label -> none).data(data"data-n" -> 3)
        val g                   = (b: Button) => b.aria(AriaKey.busy -> true, AriaKey.label -> none).data(data"data-n" -> 3)
        val base                = UI.button("Go")
        for
            on   <- Signal.initRef(true)
            off  <- Signal.initRef(false)
            sOn  <- html(base.when(on: Signal[Boolean])(f))
            sOff <- html(base.when(off: Signal[Boolean])(f))
            cOn  <- html(base.when(true)(g))
            cOff <- html(base.when(false)(g))
        yield
            assert(attrSet(sOn, "button") == attrSet(cOn, "button") && attrSet(sOff, "button") == attrSet(cOff, "button"), s"$sOn | $sOff")
            assert(cOn.contains("""aria-busy="true"""") && cOn.contains("""data-n="3"""") && !cOn.contains("aria-label"), cOn)
        end for
    }

    "a reserved data name is refused on a toggle as on the element" in {
        for s <- Signal.initRef(false)
        yield
            val kyoX  = "kyo-x"
            val error = intercept[IllegalArgumentException](UI.div.when(s: Signal[Boolean])(_.data(data"data-$kyoX" -> "v")))
            assert(error.getMessage.contains("kyo- prefix is reserved"), error.getMessage)
    }
end UIWhenTest
