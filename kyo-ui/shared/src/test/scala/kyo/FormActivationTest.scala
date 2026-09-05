package kyo

import kyo.internal.ButtonActivation
import kyo.internal.HtmlRenderer
import kyo.internal.KeyboardEventData
import kyo.internal.MouseEventData
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.UIEvent
import kyo.internal.UIExchange

/** Which activations submit a form — the rule, and the three transports that have to
  * answer it identically for the same markup (GAPS.md F-33).
  *
  * The dispatcher used to ask only whether the click target was a `Button`, never what
  * its `type` said, so every `uic.Button` inside a `UI.form` fired that form's `onSubmit`
  * — `uic.Button` defaults to `type="button"`, which submits nothing in a browser. Enter
  * was worse: it submitted for anything that was not a `Select`.
  *
  * The expectations below are not read off the specification. They were measured in
  * Chrome on a plain HTML page, one `element.click()` per row against a `submit`
  * listener; [[ButtonActivation]] carries the table.
  */
class FormActivationTest extends kyo.test.Test[Any]:

    override def config = super.config.sequential

    private class NoopExchange extends UIExchange:
        def onChange(
            region: ReactiveRegion,
            path: Seq[String],
            context: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            ui: UI
        )(using Frame): Unit < Async = ()
    end NoopExchange

    private def withDispatch[A](ui: UI)(f: ((Seq[String], UIEvent) => Boolean < Async) => A < (Async & Scope))(using
        Frame
    ): A < Async =
        Scope.run {
            for
                root         <- ReactiveUI.normalize(ui, Seq.empty)
                subscription <- ReactiveUI.subscribe(root, new NoopExchange)
                result       <- f(subscription.handle)
            yield result
        }

    /** A form whose only child is a button carrying `declaredType` (omitted when null),
      * dispatched at the button's path. Returns (onSubmit count, onClick count).
      */
    private def activate(declaredType: String, event: (Seq[String]) => UIEvent)(using Frame): (Int, Int) < Async =
        for
            submits <- AtomicInt.init(0)
            clicks  <- AtomicInt.init(0)
            button = UI.button("go").onClick(clicks.getAndUpdate(_ + 1).unit)
            typed  = if declaredType == null then button else button.jsProp("type", declaredType)
            _ <- withDispatch(UI.form.onSubmit(submits.getAndUpdate(_ + 1).unit)(typed)) { dispatch =>
                dispatch(Seq("0"), event(Seq("0")))
            }
            s <- submits.get
            c <- clicks.get
        yield (s, c)

    private def click(path: Seq[String]): UIEvent =
        UIEvent.Click(path, MouseEventData(UI.Modifiers.none, Absent))

    private def enter(path: Seq[String]): UIEvent =
        UIEvent.KeyDown(path, KeyboardEventData("Enter", UI.Modifiers.none, Absent))

    // The measured table, as data: (declared type, does activating it submit?)
    private val measured: Seq[(String, Boolean)] = Seq(
        (null, true), // missing value default
        ("submit", true),
        ("SUBMIT", true), // keywords match ASCII case-insensitively
        ("bogus", true),  // invalid value default
        ("", true),
        (" button ", true), // unrecognised: the keyword is not trimmed
        ("button", false),
        ("BUTTON", false),
        ("reset", false),
        ("ReSeT", false)
    )

    "ButtonActivation.submits agrees with the browser on every measured spelling" in {
        val wrong = measured.filter((declared, expected) => ButtonActivation.submits(declared) != expected)
        assert(wrong.isEmpty, s"disagrees with the measurement for: $wrong")
    }

    "a click submits only from a button whose type submits" in {
        Kyo.foreachDiscard(measured) { (declared, expected) =>
            activate(declared, click).map { (submitted, clicked) =>
                assert(clicked == 1, s"type=$declared: the button's own onClick must fire either way")
                assert(
                    submitted == (if expected then 1 else 0),
                    s"type=$declared: onSubmit fired $submitted time(s), browser submits = $expected"
                )
            }
        }
    }

    "Enter on a button follows the same type, and no longer submits from every element" in {
        Kyo.foreachDiscard(measured) { (declared, expected) =>
            activate(declared, enter).map { (submitted, _) =>
                assert(
                    submitted == (if expected then 1 else 0),
                    s"type=$declared: Enter produced $submitted submit(s), browser submits = $expected"
                )
            }
        }
    }

    "Enter in a text field is still implicit submission, which has no button to ask" in {
        for
            submits <- AtomicInt.init(0)
            _ <- withDispatch(UI.form.onSubmit(submits.getAndUpdate(_ + 1).unit)(UI.input.id("t"))) { dispatch =>
                dispatch(Seq("0"), enter(Seq("0")))
            }
            s <- submits.get
        yield assert(s == 1)
        end for
    }

    "KNOWN: a browser's Enter delivers keydown AND a click, and the form hears both" in {
        // Not fixed here, and not a regression: this is what the two DOM transports have always
        // delivered for Enter on a SUBMITTING button. `KeyPolicy.doubleActivates` leaves the
        // browser's native activation alone in exactly that case (`!submits`), so the browser
        // fires its own click, which is posted alongside the keydown — and the dispatcher answers
        // both. The pure transport (this harness, and the TUI) sends only the keydown and submits
        // once. Pinned as KNOWN so a change in it is a failing test rather than a surprise;
        // GAPS.md F-36 carries the decision.
        for
            submits <- AtomicInt.init(0)
            button = UI.button("go").jsProp("type", "submit")
            _ <- withDispatch(UI.form.onSubmit(submits.getAndUpdate(_ + 1).unit)(button)) { dispatch =>
                dispatch(Seq("0"), enter(Seq("0"))).andThen(dispatch(Seq("0"), click(Seq("0"))))
            }
            s <- submits.get
        yield assert(s == 2, s"expected the known double, got $s")
        end for
    }

    "the server-push client script is built from the same rule" in {
        // The KeyPolicy arrangement: neither transport can drift without failing here.
        val page = HtmlRenderer.renderPage("t", "<div></div>", "", "/app")
        assert(page.contains(ButtonActivation.jsSubmits("__et")))
        assert(page.contains("""var __et=(__pt!==null?__pt:(__at!==null?__at:"")).toLowerCase();"""))
        // The old comparison must be gone: it called `type="bogus"` non-submitting.
        assert(!page.contains("""__et==="submit""""))
    }
end FormActivationTest
