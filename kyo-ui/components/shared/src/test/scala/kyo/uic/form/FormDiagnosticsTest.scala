package kyo.uic.form

import kyo.*
import kyo.UI.*
import kyo.uic
import kyo.uic.UicTest

/** The three things a form says out loud about its own ids — and the one it deliberately
  * stays quiet about.
  *
  * Each of these used to be silent, and silent in the worst way: the form still rendered, the
  * markup still looked right, and the only symptom was that a failed submit moved focus
  * nowhere. So what these assert is not that the form works but that it COMPLAINS, and the
  * last case is the other half of that — a card that fired when nothing was wrong would train
  * the reader to ignore all of them.
  *
  * Driven at `FormDiagnostics.cards` rather than through `Form.mountedWith`, for the same
  * reason the rest of this suite builds its forms by hand: a mount's effect does not run
  * under `UI.runRender`, so a test that went through the mount would assert against an empty
  * region and pass for the wrong reason.
  */
class FormDiagnosticsTest extends UicTest:

    private def mkForm(using Frame): Form < Sync =
        for
            fieldsR    <- AtomicRef.init(Chunk.empty[FormField[?]])
            childrenR  <- AtomicRef.init(Chunk.empty[Form])
            formErrs   <- Signal.initRef(Chunk.empty[Form.ErrorEntry])
            count      <- Signal.initRef(0)
            submitting <- Signal.initRef(false)
            arrays     <- AtomicRef.init(Chunk.empty[FieldArray[?]])
            counter    <- AtomicInt.init(0)
        yield new Form(
            ErrorTranslator.default,
            fieldsR,
            childrenR,
            formErrs,
            count,
            submitting,
            arrays,
            focusInvalidOnSubmit = true,
            () => counter.incrementAndGet.map(n => s"minted$n"),
            _ => Kyo.unit
        )

    /** The cards a form would render for this tree, flattened to one string. */
    private def complaints(build: Form => UI < (Async & Scope))(using Frame): String < (Async & Scope) =
        for
            form  <- mkForm
            ui    <- build(form)
            cards <- FormDiagnostics.cards(form, ui)
            html  <- Kyo.foreach(cards)(c => UI.runRender(c).take(1).run)
        yield html.map(_.mkString).mkString

    "an id set on a control before bind is replaced, and the form says so" in {
        for
            said <- complaints { form =>
                for f <- form.field("").declare
                yield div(uic.Input().id("login-username").bind(f))
            }
        yield
            assert(said.contains("p-uic-key-error"), "the replacement is reported")
            assert(said.contains("login-username"), "and the card names the id that was lost")
            assert(said.contains("domId"), "and the setter that would have worked")
    }

    "a control bound with no id of its own says nothing" in {
        for
            said <- complaints { form =>
                for f <- form.field("").declare
                yield div(uic.Input().bind(f))
            }
        yield assert(said.isEmpty, s"the ordinary case is quiet; got: $said")
    }

    "a field that chose its own id says nothing either — that is the sanctioned way" in {
        for
            said <- complaints { form =>
                for f <- form.field("").domId("login-username").declare
                yield div(uic.Label("User").forId(f.domId), uic.Input().bind(f))
            }
        yield assert(said.isEmpty, s"nothing is reported; got: $said")
    }

    "two fields claiming one id is reported — getElementById would answer for one of them" in {
        for
            said <- complaints { form =>
                for
                    a <- form.field("").domId("dup").declare
                    b <- form.field("").domId("dup").declare
                yield div(uic.Input().bind(a), uic.Input().bind(b))
            }
        yield
            assert(said.contains("p-uic-key-error"))
            assert(said.contains("dup"))
    }

    // The one that catches `.id(...)` AFTER bind, which bind itself cannot see because it has
    // already returned. The label was written from the field's id and now points at nothing.
    "a label pointing at an id nothing carries is reported" in {
        for
            said <- complaints { form =>
                for f <- form.field("").domId("email").declare
                yield div(uic.Label("Email").forId(f.domId), uic.Input().bind(f).id("something-else"))
            }
        yield
            assert(said.contains("p-uic-key-error"), "the dangling label is reported")
            assert(said.contains("email"), "named by the id it points at")
    }

    // The check that was NOT written, asserted from the outside: a field bound inside a closed
    // reactive branch is absent from the tree for a good reason. A "your field is not in the
    // document" card would fire here, and be wrong.
    "a field bound inside a branch that is currently closed produces no card" in {
        for
            said <- complaints { form =>
                for
                    open <- Signal.initRef(false)
                    f    <- form.field("").domId("hidden-field").declare
                yield div(open.render(o => if o then div(uic.Input().bind(f)) else div("nothing here")))
            }
        yield assert(said.isEmpty, s"absence is not evidence of a mistake; got: $said")
    }
end FormDiagnosticsTest
