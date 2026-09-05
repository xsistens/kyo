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

    private def space(path: Seq[String]): UIEvent =
        UIEvent.KeyDown(path, KeyboardEventData(" ", UI.Modifiers.none, Absent))

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

    "Space on a submitting button submits, since the clients suppress the browser's own activation" in {
        // Space is on the keydown path only because of that suppression. A browser activates a
        // submitting button with Enter AND Space; once the clients preventDefault on both, this
        // path is the only one that submits, so it has to answer both or Space would silently
        // stop submitting (GAPS.md F-36).
        for
            submits <- AtomicInt.init(0)
            button = UI.button("go").jsProp("type", "submit")
            _ <- withDispatch(UI.form.onSubmit(submits.getAndUpdate(_ + 1).unit)(button)) { dispatch =>
                dispatch(Seq("0"), space(Seq("0")))
            }
            s <- submits.get
        yield assert(s == 1)
        end for
    }

    "Space anywhere else does not submit, because a space is a character there" in {
        for
            submits <- AtomicInt.init(0)
            _ <- withDispatch(UI.form.onSubmit(submits.getAndUpdate(_ + 1).unit)(UI.input.id("t"))) { dispatch =>
                dispatch(Seq("0"), space(Seq("0")))
            }
            s <- submits.get
        yield assert(s == 0)
        end for
    }

    "Space on a NON-submitting button does not submit either" in {
        for
            submits <- AtomicInt.init(0)
            button = UI.button("go").jsProp("type", "button")
            _ <- withDispatch(UI.form.onSubmit(submits.getAndUpdate(_ + 1).unit)(button)) { dispatch =>
                dispatch(Seq("0"), space(Seq("0")))
            }
            s <- submits.get
        yield assert(s == 0)
        end for
    }

    "the browser's Enter no longer delivers a twin, so the form hears it once" in {
        // This was pinned as a KNOWN double at 2. Both DOM transports used to leave the browser's
        // native activation of a SUBMITTING button alone, so the browser fired its own click
        // alongside the keydown and the dispatcher answered both. Now the dispatcher synthesizes the
        // click itself and the clients preventDefault, which the script assertions below hold them
        // to. The pure transport, which never had a twin, is unchanged.
        for
            submits <- AtomicInt.init(0)
            button = UI.button("go").jsProp("type", "submit")
            _ <- withDispatch(UI.form.onSubmit(submits.getAndUpdate(_ + 1).unit)(button)) { dispatch =>
                dispatch(Seq("0"), enter(Seq("0")))
            }
            s <- submits.get
        yield assert(s == 1, s"expected one submit, got $s")
        end for
    }

    "the server-push client suppresses the browser's activation without asking for a type" in {
        // The client script used to read the button's type here and skip the preventDefault when it
        // submitted. Both the read and the exemption must be gone: the type it produced is what made
        // the two transports disagree, and it is the one thing the dispatcher does not need told.
        val page = HtmlRenderer.renderPage("t", "<div></div>", "", "/app")
        assert(page.contains("""var __act=(__tg==="BUTTON")?("""))
        assert(page.contains("""var __inf=!!(e.target.closest&&e.target.closest("form"));"""))
        assert(page.contains("""if(__act)e.preventDefault();"""))
        assert(!page.contains("__sub"))
        assert(!page.contains("__et"))
    }
end FormActivationTest
