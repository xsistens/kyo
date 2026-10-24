package kyo.uic.form

import java.time.LocalDate
import kyo.*
import kyo.UI.*
import kyo.uic.DateKind.given
import kyo.uic.UicTest

/** Deterministic checks for the validation layer's error-lifecycle contract: the
  * behaviour that keeps a server verdict from deadlocking the submit button, plus the
  * typed-field path. It drives the signal graph directly, with no DOM, which is exactly
  * where the value-scoping lives.
  *
  * Every body is an effect rather than a blocking discharge, so the suite runs on all
  * four platforms. That matters most for the date and number cases: off the JVM their
  * `java.time` and locale data comes from scala-java-time, and a missing database throws
  * only at run time, where neither compile nor link can see it.
  */
class FormValidationTest extends UicTest:

    /** The options of the multi-choice field below: a field holds the options themselves, not keys standing in for them. */
    private enum Lang derives CanEqual:
        case De, En

    // `inline val` keeps every argument to the inline `typeCheck` a compile-time constant.
    private inline val submitPreamble = "import kyo.*\nimport kyo.uic.form.*\n"

    /** A text field's rule that parses the number it holds. */
    private val digits: Rule[String, Int] = Validator.refine[String, Int]("not-a-number")(s => Maybe.fromOption(s.toIntOption))

    /** A form built the way `Form.mounted` builds one, minus the `UI.mounted` wrapper,
      * so a test can hold the instance and read its signals.
      */
    private def mkForm(using Frame): Form < Sync = mkFormWith(ErrorTranslator.default)

    /** [[mkForm]] with the translator a test is about. */
    private def mkFormWith(translator: ErrorTranslator)(using Frame): Form < Sync =
        for
            fieldsR    <- AtomicRef.init(Chunk.empty[FormField[?]])
            childrenR  <- AtomicRef.init(Chunk.empty[Form])
            formErrs   <- Signal.initRef(Chunk.empty[Form.ErrorEntry])
            count      <- Signal.initRef(0)
            submitting <- Signal.initRef(false)
            arrays     <- AtomicRef.init(Chunk.empty[FieldArray[?]])
            counter    <- AtomicInt.init(0)
        yield new Form(
            translator,
            fieldsR,
            childrenR,
            formErrs,
            count,
            submitting,
            arrays,
            focusInvalidOnSubmit = true,
            () => counter.incrementAndGet.map(n => s"id$n"), // distinct ids (field domIds + array row keys)
            _ => Kyo.unit,                                   // focus is a no-op off-DOM
            ownsSubmit = true
        )

    /** A standalone typed field with a caller-held value ref, so the test can simulate
      * edits. `touched` starts open (as after a submit) so the error is displayable.
      */
    private def mkField[A](initial: A, rules: Validator[A], value: A)(using Frame, CanEqual[A, A]): (FormField[A], SignalRef[A]) < Sync =
        for
            valueRef   <- Signal.initRef(value)
            baseline   <- Signal.initRef(initial)
            touched    <- Signal.initRef(true)
            validating <- Signal.initRef(false)
            client     <- Signal.initRef(ClientVerdict.Checked(Absent): ClientVerdict)
            serverErr  <- Signal.initRef(Absent: Maybe[ServerError[A]])
            epoch      <- AtomicInt.init(0)
            resetWrite <- AtomicRef.init(Maybe.empty[A])
            count      <- Signal.initRef(0)
            named      <- Signal.initRef(false)
            owner      <- mkForm
            field      <- FormField.declared[A, A](
                valueRef,
                initial,
                baseline,
                "test-field-id",
                touched,
                validating,
                client,
                serverErr,
                epoch,
                resetWrite,
                rules,
                Present(Set[Activation.Field](Activation.Submit)),
                Duration.Zero,
                Reveal.WhenTouched,
                true,
                ErrorTranslator.default,
                count,
                named,
                owner
            )
        yield (field, valueRef)

    /** A form whose id minting is a real counter (so fields get distinct ids) and whose
      * focus command appends the targeted id to `focused`, which lets a test assert exactly
      * which field submit would focus.
      */
    private def mkFocusForm(focused: SignalRef[Chunk[String]], focus: Boolean = true)(using Frame): Form < Sync =
        for
            counter    <- AtomicInt.init(0)
            fieldsR    <- AtomicRef.init(Chunk.empty[FormField[?]])
            childrenR  <- AtomicRef.init(Chunk.empty[Form])
            formErrs   <- Signal.initRef(Chunk.empty[Form.ErrorEntry])
            count      <- Signal.initRef(0)
            submitting <- Signal.initRef(false)
            arrays     <- AtomicRef.init(Chunk.empty[FieldArray[?]])
        yield new Form(
            ErrorTranslator.default,
            fieldsR,
            childrenR,
            formErrs,
            count,
            submitting,
            arrays,
            focusInvalidOnSubmit = focus,
            () => counter.incrementAndGet.map(n => s"f$n"),
            id => focused.updateAndGet(_ :+ id).unit,
            ownsSubmit = true
        )

    "submit focuses the first invalid field in declaration order" in {
        for
            (focused, firstId) <-
                for
                    sink <- Signal.initRef(Chunk.empty[String])
                    form <- mkFocusForm(sink)
                    a    <- form.field.rules(Validator.required()).on(Activation.Submit).declare // empty → invalid
                    _    <- form.field.rules(Validator.required()).on(Activation.Submit).declare // also invalid, but later
                    _    <- form.submit(Kyo.unit)
                    f    <- sink.get
                yield (f, a.domId)
        yield assert(focused.headMaybe.contains(firstId), s"focus goes to the first invalid field; got $focused want $firstId")
    }

    "focusable(false) excludes a field, so focus lands on the next invalid one" in {
        for
            (focused, secondId) <-
                for
                    sink <- Signal.initRef(Chunk.empty[String])
                    form <- mkFocusForm(sink)
                    _    <- form.field.rules(Validator.required()).on(Activation.Submit).focusable(false).declare
                    b    <- form.field.rules(Validator.required()).on(Activation.Submit).declare
                    _    <- form.submit(Kyo.unit)
                    f    <- sink.get
                yield (f, b.domId)
        yield assert(focused.headMaybe.contains(secondId), s"focus skips focusable(false); got $focused want $secondId")
    }

    "focusInvalidOnSubmit = false suppresses focus entirely" in {
        for
            noFocus <-
                for
                    sink <- Signal.initRef(Chunk.empty[String])
                    form <- mkFocusForm(sink, focus = false)
                    _    <- form.field.rules(Validator.required()).on(Activation.Submit).declare
                    _    <- form.submit(Kyo.unit)
                    f    <- sink.get
                yield f.isEmpty
        yield assert(noFocus, "no focus command is emitted when the behaviour is disabled")
    }

    "an errorSummary jump row says type=button, so it cannot submit a form it sits in" in {
        for
            (links, linkType, linkIsButton, plainIsButton) <-
                for
                    form <- mkForm
                    _    <- form.field.rules(Validator.required()).on(Activation.Submit).declare
                    // A failed submit is what opens the summary: it fills the entry AND lifts submitCount off 0.
                    _   <- form.submit(Kyo.unit)
                    _   <- form.raise(FieldError("form-level")) // no anchor → a plain row, nothing to jump to
                    ui  <- Form.errorSummary(form)
                    els <- elements(ui)
                yield
                    val jump  = els.filter(_.attrs.cssClasses.contains("p-uic-error-link"))
                    val plain = els.filter(e =>
                        e.attrs.cssClasses.contains("p-uic-invalid-message") && !e.attrs.cssClasses.contains("p-uic-error-link")
                    )
                    (
                        jump.size,
                        jump.headMaybe.map(_.attrs.jsProps.getOrElse("type", "<none>")).getOrElse("<no row>"),
                        jump.forall(_.isInstanceOf[UI.Ast.Button]),
                        plain.exists(_.isInstanceOf[UI.Ast.Button])
                    )
        yield
            assert(links == 1, s"one field error → one jump row; got $links")
            assert(linkIsButton, "the jump row is a <button>")
            // Without this the browser default applies and the row is a SUBMIT button: inside a native
            // <form> a click meant to move focus submits instead, and Enter in a text field picks it as
            // the form's default button.
            assert(linkType == "button", s"the jump row declares type=button; got $linkType")
            assert(!plainIsButton, "a form-level row has nothing to jump to and stays a plain element")
        end for
    }

    "dirty tracking: edit → dirty, markPristine → clean, reset returns to the rebaselined baseline" in {
        for
            (d0, d1, d2, d3, resetVal, d4) <-
                for
                    form <- mkForm
                    f    <- form.field("a").on(Activation.Submit).declare
                    a0   <- f.isDirty.current // clean: value == baseline "a"
                    _    <- f.valueRef.set("b")
                    a1   <- f.isDirty.current // dirty
                    _    <- form.markPristine // baseline := "b"
                    a2   <- f.isDirty.current // clean again at the new baseline
                    _    <- f.valueRef.set("c")
                    a3   <- f.isDirty.current // dirty past the new baseline
                    _    <- form.reset        // value := baseline "b" (NOT initial "a")
                    rv   <- f.valueRef.get
                    a4   <- f.isDirty.current // clean
                yield (a0, a1, a2, a3, rv, a4)
        yield
            assert(!d0, "clean at construction")
            assert(d1, "dirty after an edit")
            assert(!d2, "clean after markPristine")
            assert(d3, "dirty after editing past the new baseline")
            assert((resetVal == "b"), "reset returns to the rebaselined baseline, not the original initial")
            assert(!d4, "clean after reset")
    }

    "form.isDirty aggregates the subtree, including child scopes" in {
        for
            (clean, dirtyChild, cleanAfterPristine) <-
                for
                    form  <- mkForm
                    _     <- form.field("x").on(Activation.Submit).declare
                    child <- form.child()
                    cf    <- child.field("y").on(Activation.Submit).declare
                    sig   <- form.isDirty // read after fields + child are declared
                    c0    <- sig.current  // false
                    _     <- cf.valueRef.set("z")
                    c1    <- sig.current  // true: a dirty child field bubbles up
                    _     <- form.markPristine
                    c2    <- sig.current  // false
                yield (c0, c1, c2)
        yield
            assert(!clean, "a pristine form is not dirty")
            assert(dirtyChild, "a dirty child field makes the whole form dirty")
            assert(!cleanAfterPristine, "markPristine clears the whole subtree")
    }

    "field-array rows participate in submit validation and error aggregation" in {
        for
            (emptyErrs, filledErrs) <-
                for
                    f1 <- mkForm
                    _  <- f1.fieldArray(2)((row, _) =>
                        row.field.rules(Validator.required()).on(Activation.Submit).declare.map(_ => (fragment(): UI))
                    )
                    s1 <- f1.allErrors
                    _  <- f1.submit(Kyo.unit) // reveals: both empty required rows fail
                    e1 <- s1.current

                    f2 <- mkForm
                    _  <- f2.fieldArray(2)((row, _) =>
                        row.field("ok").rules(Validator.required()).on(Activation.Submit).declare.map(_ => (fragment(): UI))
                    )
                    s2 <- f2.allErrors
                    _  <- f2.submit(Kyo.unit) // both rows already satisfy required
                    e2 <- s2.current
                yield (e1.length, e2.length)
        yield
            assert((emptyErrs == 2), s"two empty required rows → 2 aggregated errors, got $emptyErrs")
            assert((filledErrs == 0), s"two filled rows → 0 errors, got $filledErrs")
    }

    "add / remove change the aggregation live; a dirty row makes the form dirty" in {
        for
            (d0, d1, d2, d3) <-
                for
                    captured <- AtomicRef.init(Chunk.empty[FormField[String]])
                    form     <- mkForm
                    arr      <- form.fieldArray(0) { (row, _) =>
                        for
                            fld <- row.field("x").rules(Validator.required()).on(Activation.Submit).declare
                            _   <- captured.updateAndGet(_ :+ fld)
                        yield (fragment(): UI)
                    }
                    dirtySig <- form.isDirty
                    a0       <- dirtySig.current            // no rows → clean
                    _        <- arr.add                     // fresh row at its baseline "x" → still clean
                    a1       <- dirtySig.current
                    flds     <- captured.get
                    _        <- flds.head.valueRef.set("y") // edit the row's field → dirty
                    a2       <- dirtySig.current
                    _        <- arr.removeAt(0)             // remove the dirty row → clean again
                    a3       <- dirtySig.current
                yield (a0, a1, a2, a3)
        yield
            assert(!d0, "empty array → clean")
            assert(!d1, "a fresh row at its baseline → still clean")
            assert(d2, "editing a row field makes the form dirty (array folds into isDirty)")
            assert(!d3, "removing the dirty row makes the form clean again")
    }

    // ---- what a field DECLARES, and what it declares by saying nothing ----
    //
    // Every slot of a declaration has a default, and the defaults are the whole reason the
    // builder exists: the old constructor form made a caller spell "no rules" as
    // `Validator.all[String]()` and "no triggers" as `Activation.Submit`, both of which read
    // as omissions. These pin what saying nothing now means.

    "a field that declares nothing is touched by the blur and by a change" in {
        for
            form <- mkForm
            f    <- form.field("").declare
        yield
            assert(f.touchedBy(Activation.Blur), "the reader leaving the field is a verdict")
            assert(f.touchedBy(Activation.Change), "and so is every edit after that")
    }

    // The other half of the default is Reveal.WhenTouched, which is what keeps the FIRST
    // keystroke silent: activation says when a verdict is recomputed, reveal says when it is
    // shown. Together they are 'reward early, punish late'.
    "the default keeps an untouched field quiet, and speaks once it has been touched" in {
        for
            form   <- mkForm
            f      <- form.field("").rules(Validator.required()).declare
            _      <- f.revalidate
            before <- f.visibleError.current
            _      <- f.onEvent("", Activation.Blur)
            after  <- f.visibleError.current
        yield
            assert(before.isEmpty, "invalid from the start, and says nothing about it")
            assert(after.exists(_.code == "required"), "once touched, the same verdict is shown")
    }

    "on(Activation.Submit) is how a field opts OUT: no event of its own touches it" in {
        for
            form <- mkForm
            f    <- form.field("").rules(Validator.required()).on(Activation.Submit).declare
            // Submit validates unconditionally whatever the set says, which is why declaring
            // Submit alone means 'never re-check while the reader is in the field'.
            v <- f.validateForSubmit
        yield
            assert(!f.touchedBy(Activation.Blur), "not the blur")
            assert(!f.touchedBy(Activation.Change), "not a change")
            assert(v.exists(_.code == "required"), "and submit still checks it")
    }

    "a field that declares no rules passes every value" in {
        for
            form <- mkForm
            f    <- form.field("").declare
            v    <- f.validateForSubmit
        yield assert(v.isEmpty, "Validator.none is the default, so there is nothing to fail")
    }

    "a chosen domId is the one bind stamps, and the one focus-first-invalid goes to" in {
        import kyo.uic
        for
            sink <- Signal.initRef(Chunk.empty[String])
            form <- mkFocusForm(sink)
            f    <- form.field("").rules(Validator.required()).domId("login-username").declare
            html <- UI.runRender(uic.Input().bind(f)).take(1).run
            _    <- form.submit(Kyo.unit)
            went <- sink.get
        yield
            assert(f.domId == "login-username", "the mint is skipped when the caller chose")
            assert(html.mkString.contains("""id="login-username""""), "and bind stamps what the caller chose")
            // The half the refused workaround (`.id(...)` after bind) broke silently.
            assert(went.headMaybe.contains("login-username"), "submit can still reach the field")
        end for
    }

    "touched and validating are the same kind of member on FormField and NumberField" in {
        // A def is not a stable path, so its singleton type does not exist; a val's does. Both fields answer the same way,
        // which is what a tool reading the two classes side by side sees.
        typeCheckFailure("import kyo.*\nimport kyo.uic.form.*\ndef x(f: FormField[Int]): f.touched.type = f.touched")("singleton")
        typeCheckFailure("import kyo.*\nimport kyo.uic.form.*\ndef x(f: FormField[Int]): f.validating.type = f.validating")("singleton")
        typeCheckFailure("import kyo.*\nimport kyo.uic.form.*\ndef x(f: NumberField[Int]): f.touched.type = f.touched")("singleton")
        typeCheckFailure("import kyo.*\nimport kyo.uic.form.*\ndef x(f: NumberField[Int]): f.validating.type = f.validating")("singleton")
    }

    "an invalid Select is focused through its id: the id bind stamps is on the combobox, not on its label" in {
        import kyo.uic
        for
            sink <- Signal.initRef(Chunk.empty[String])
            form <- mkFocusForm(sink)
            f    <- form.field(Maybe.empty[Lang]).rules(Validator.chosen()).on(Activation.Submit).declare
            sel = uic.Select[Lang]().options(Lang.values.toSeq)(_.toString).bind(f)
            _    <- form.submit(Kyo.unit)
            went <- sink.get
            el   <- elementWithId(sel.render, f.domId)
        yield
            assert(went == Chunk(f.domId), "submit asks for the Select by its field id")
            assert(el.attrs.cssClasses.contains("p-select"), s"and that id is on the field root, got ${el.attrs.cssClasses}")
            assert(el.attrs.tabIndex.contains(0) && el.attrs.role.contains("combobox"), "the element focus can land on")
        end for
    }

    "the controls that could not join a form now bind end to end" in {
        import kyo.uic
        // Slider over a numeric field: bind wires the value ref, the field's id and the
        // gated message, so a failing rule reaches the rendered control.
        for
            (beforeSubmit, afterSubmit) <-
                for
                    form  <- mkForm
                    field <- form.field(3.0).rules(Validator.satisfy[Double]("min-volume")(_ >= 5.0)).declare
                    bound = uic.Slider().min(0).max(10).bind(field)
                    pre <- UI.runRender(bound).take(1).run
                    // Marks the field touched AND validates, the path a real Blur takes, so the
                    // error is both recorded and revealed.
                    _    <- field.onEvent(3.0, Activation.Blur)
                    post <- UI.runRender(bound).take(1).run
                yield (pre.mkString, post.mkString)

            // FileUpload over the picked payloads: a size rule reads the metadata it was
            // handed, which is the whole point of binding the payloads rather than names.
            (small, tooBig) <-
                for
                    form  <- mkForm
                    field <- form.field(Seq.empty[UI.FilePayload])
                        .rules(Validator.satisfy[Seq[UI.FilePayload]]("file-too-large")(_.forall(_.size <= 2048)))
                        .on(Activation.Submit)
                        .declare
                    _   <- field.valueRef.set(Seq(UI.FilePayload("cv.pdf", 1024L, "application/pdf", "…")))
                    ok  <- field.validateForSubmit
                    _   <- field.valueRef.set(Seq(UI.FilePayload("huge.pdf", 99999L, "application/pdf", "…")))
                    bad <- field.validateForSubmit
                    _   <- UI.runRender(uic.FileUpload().bind(field)).take(1).run
                yield (ok, bad)
        yield
            assert(beforeSubmit.contains("""value="3""""), "bind wires the field's value onto the slider")
            assert(beforeSubmit.contains("""id="id1""""), "bind stamps the field's minted id (focus-first-invalid target)")
            assert(!beforeSubmit.contains("p-uic-invalid-message"), "no message before the rule has run")
            assert(afterSubmit.contains("p-uic-invalid-message"), "the failing rule's message reaches the slider")
            assert(afterSubmit.contains("min-volume"), "and it is the rule's own message")
            assert(small.isEmpty, "a file within the cap passes")
            assert(tooBig.isDefined, "a file over the cap fails, judged on the bound payload's size")
        end for
    }

    "Activation.Change validates a field-array row's field, and stops once the row is removed" in {
        // A row is added from an event handler, with no ambient Scope, so its Change
        // observer runs on an unscoped fiber the array owns. Both halves matter: the
        // trigger has to fire at all (it used to compile and do nothing), and it has to
        // stop when the row goes away rather than validating a detached field forever.
        for
            (beforeEdit, afterEdit, afterRemoval) <-
                for
                    captured <- AtomicRef.init(Chunk.empty[FormField[String]])
                    form     <- mkForm
                    arr      <- form.fieldArray(0) { (row, _) =>
                        for
                            fld <- row.field("ok").rules(Validator.required()).on(Activation.Change).declare
                            _   <- captured.updateAndGet(_ :+ fld)
                        yield (fragment(): UI)
                    }
                    _    <- arr.add
                    flds <- captured.get
                    fld = flds.head
                    e0 <- fld.error.current
                    // The observer runs on its own fiber: wait until it listens, then until the edit's verdict lands.
                    _  <- assertEventually(fld.valueRef.waiters.map(_ == 1))
                    _  <- fld.valueRef.set("")
                    _  <- assertEventually(fld.error.current.map(_.isDefined))
                    e1 <- fld.error.current
                    _  <- arr.removeAt(0)
                    // A live observer would revalidate this back to VALID and clear the error;
                    // a cancelled one leaves the error exactly where the last edit put it.
                    _  <- assertEventually(fld.valueRef.waiters.map(_ == 0))
                    _  <- fld.valueRef.set("valid again")
                    e2 <- fld.error.current
                yield (e0.isDefined, e1.isDefined, e2.isDefined)
        yield
            assert(!beforeEdit, "a row field at its valid initial value starts without an error")
            assert(afterEdit, "Activation.Change on a row field validates on every value change")
            assert(afterRemoval, "removing the row cancelled its observer: a later edit is not revalidated")
    }

    "move / moveDown reorder rows, and row.index reflects the new positions" in {
        for
            (i0, i1, i0b, i1b) <-
                for
                    ctls <- AtomicRef.init(Chunk.empty[FieldArray.Row])
                    form <- mkForm
                    arr  <- form.fieldArray(0) { (row, ctl) =>
                        ctls.updateAndGet(_ :+ ctl).andThen(row.field.on(Activation.Submit).declare.map(_ =>
                            (fragment(): UI)
                        ))
                    }
                    _  <- arr.add
                    _  <- arr.add
                    cs <- ctls.get
                    a0 <- cs(0).index.current // 0
                    b0 <- cs(1).index.current // 1
                    _  <- arr.move(0, 1)      // move first row to the end
                    a1 <- cs(0).index.current // now 1
                    b1 <- cs(1).index.current // now 0
                yield (a0, b0, a1, b1)
        yield
            assert((i0 == 0 && i1 == 1), s"initial order (0,1), got ($i0,$i1)")
            assert((i0b == 1 && i1b == 0), s"after move(0,1) order (1,0), got ($i0b,$i1b)")
    }

    "fieldArrayOf: a cross-row check validates over all rows and flags duplicates per row (value-scoped)" in {
        for
            (uniqueClean, dupFormErr, dupRowErr, clearedOnEdit) <-
                for
                    form <- mkForm
                    arr  <- form.fieldArrayOf[FormField[String]](2) { (row, _) =>
                        row.field.on(Activation.Submit).declare.map(f => ((fragment(): UI), f))
                    }
                    _ = arr.satisfy("dup") { rows =>
                        for
                            vals <- Kyo.foreach(rows)(_.value.current)
                            dups = vals.groupBy(identity).filter(_._2.size > 1).keySet
                            _ <- Kyo.foreach(rows)(r =>
                                r.value.current.map(v => r.setError(if dups(v) then Present(FieldError("dup-row")) else Absent))
                            )
                        yield dups.isEmpty
                    }
                    hs  <- arr.currentRows
                    _   <- hs(0).valueRef.set("a")
                    _   <- hs(1).valueRef.set("b") // distinct → clean
                    _   <- form.submit(Kyo.unit)
                    fe1 <- form.errors.current
                    _   <- hs(1).valueRef.set("a") // now a duplicate
                    _   <- form.submit(Kyo.unit)
                    fe2 <- form.errors.current     // form-level "dup" raised
                    re2 <- hs(1).error.current     // the duplicate row flagged inline
                    _   <- hs(1).valueRef.set("c") // edit the flagged row
                    re3 <- hs(1).error.current     // value-scoped error clears itself
                yield (fe1.isEmpty, fe2.nonEmpty, re2.isDefined, re3.isEmpty)
        yield
            assert(uniqueClean, "distinct rows → no cross-row error")
            assert(dupFormErr, "duplicate rows → form-level 'dup' raised and submit blocked")
            assert(dupRowErr, "the duplicate row is flagged inline via setError")
            assert(clearedOnEdit, "editing the flagged row clears its value-scoped error without re-running the check")
    }

    "built-in rules: pattern / url / numeric min-max / matchesField (empty passes where required owns it)" in {
        for
            checks <-
                for
                    other  <- Signal.initRef("secret")
                    patF   <- Validator.pattern("[a-z0-9-]+".r).run("Bad Slug")
                    patO   <- Validator.pattern("[a-z0-9-]+".r).run("good-slug-1")
                    patE   <- Validator.pattern("[0-9]+".r).run("") // empty passes
                    urlF   <- Validator.url().run("not a url")
                    urlO   <- Validator.url().run("https://example.com/p?q=1")
                    minF   <- Validator.min(3).run(2)
                    minO   <- Validator.min(3).run(3)
                    maxF   <- Validator.max(5.0).run(6.5)
                    maxO   <- Validator.max(5.0).run(5.0)
                    matchF <- Validator.matchesField(other: Signal[String]).run("nope")
                    matchO <- Validator.matchesField(other: Signal[String]).run("secret")
                yield Seq(
                    patF.failure.isDefined,
                    patO.failure.isEmpty,
                    patE.failure.isEmpty,
                    urlF.failure.isDefined,
                    urlO.failure.isEmpty,
                    minF.failure.isDefined,
                    minO.failure.isEmpty,
                    maxF.failure.isDefined,
                    maxO.failure.isEmpty,
                    matchF.failure.isDefined,
                    matchO.failure.isEmpty
                )
        yield assert(checks.forall(identity), s"every built-in rule case holds; got $checks")
    }

    "field.satisfy: post-hoc predicate rule (sugar over addRule) registers and validates" in {
        for
            (invalidWhenBad, validWhenGood) <-
                for
                    form <- mkForm
                    f    <- form.field.on(Activation.Submit).declare
                    _    <- f.satisfy("must-be-ok")(_ == "ok") // chainable, mirrors form.satisfy
                    _    <- f.valueRef.set("nope")
                    e1   <- f.validateForSubmit
                    _    <- f.valueRef.set("ok")
                    e2   <- f.validateForSubmit
                yield (e1.exists(_.code == "must-be-ok"), e2.isEmpty)
        yield
            assert(invalidWhenBad, "field.satisfy fails when the predicate is false")
            assert(validWhenGood, "passes when the predicate is true")
    }

    "isValid: ungated tracks raw field validity; onlyVisible gates on touched/submit-reveal" in {
        for
            (ungatedInvalid, visibleStillValid, ungatedValid, visibleValid) <-
                for
                    form    <- mkForm
                    f       <- form.field.rules(Validator.required()).on(Activation.Submit).declare // touched=false, submitCount=0
                    ungated <- form.isValid()                                                       // onlyVisible = false
                    visible <- form.isValid(onlyVisible = true)
                    _  <- f.validateForSubmit // records the required error, but the field stays untouched / unrevealed
                    u1 <- ungated.current     // raw error present → invalid
                    s1 <- visible.current     // error not DISPLAYED (untouched) → still "valid"
                    _  <- f.valueRef.set("x")
                    _  <- f.validateForSubmit // clears the error
                    u2 <- ungated.current
                    s2 <- visible.current
                yield (u1, s1, u2, s2)
        yield
            assert(!ungatedInvalid, "ungated: a recorded-but-hidden required error makes isValid() false")
            assert(visibleStillValid, "onlyVisible: the same hidden error does NOT count (not displayed)")
            assert(ungatedValid, "ungated: fixing the field makes it valid")
            assert(visibleValid, "onlyVisible: valid too")
    }

    "Activation.Change: mount validates silently; a real change reveals inline and re-validates live" in {
        for
            (rawAtMount, hiddenAtMount, revealedAfterChange, clearedWhenValid) <-
                for
                    form <- mkForm
                    f    <- form.field.rules(Validator.required()).on(Activation.Change).declare // touched = false
                    _  <- f.revalidate                      // the observer's mount emission → validate SILENTLY
                    e0 <- f.error.current                   // raw error computed
                    v0 <- f.visibleError.current            // but NOT displayed (untouched, no submit)
                    _  <- f.onEvent("", Activation.Change)  // a real change (still empty) → touched + validate
                    v1 <- f.visibleError.current            // now revealed inline
                    _  <- f.valueRef.set("x")
                    _  <- f.onEvent("x", Activation.Change) // change to a valid value → re-validates, clears
                    v2 <- f.visibleError.current
                yield (e0.isDefined, v0.isEmpty, v1.isDefined, v2.isEmpty)
        yield
            assert(rawAtMount, "mount: raw error is computed")
            assert(hiddenAtMount, "mount: the error is NOT revealed (silent)")
            assert(revealedAfterChange, "a real change reveals the error inline (touched)")
            assert(clearedWhenValid, "typing a valid value clears it live")
    }

    "errorEntries: field errors carry their field id; form-level errors carry the scope anchor (else Absent)" in {
        for
            (fieldTagged, fieldIdMatches, formNoAnchor, formWithAnchor) <-
                for
                    form <- mkForm
                    f    <- form.field.rules(Validator.required()).on(Activation.Submit).declare
                    _    <- form.submit(Kyo.unit)               // reveals the field's required error
                    _    <- form.raise(FieldError("no-anchor")) // anchor not set yet → Absent
                    es1  <- form.errorEntries
                    e1   <- es1.current
                    _ = form.anchor("group-1") // set the scope anchor
                    _   <- form.raise(FieldError("anchored")) // now inherits the anchor
                    es2 <- form.errorEntries
                    e2  <- es2.current
                yield
                    val fieldEntry = e1.find(_.fieldId.isDefined)
                    (
                        fieldEntry.isDefined,
                        fieldEntry.exists(_.fieldId.contains(f.domId)),
                        e1.find(_.error.code == "no-anchor").exists(_.fieldId.isEmpty),
                        e2.find(_.error.code == "anchored").exists(_.fieldId.contains("group-1"))
                    )
        yield
            assert(fieldTagged, "a field error is present and field-tagged")
            assert(fieldIdMatches, "the field error carries its field's domId (for focus-jump)")
            assert(formNoAnchor, "a form-level error with no scope anchor carries Absent")
            assert(formWithAnchor, "after form.anchor(id), a form-level error carries that id")
    }

    "reactive invariant: feeds isValid live, and displays per Reveal mode" in {
        for
            (invalidWhenOff, validWhenMet, hiddenPreSubmit, shownPostSubmit, immediateShown) <-
                for
                    form <- mkForm
                    a    <- form.field(0).on(Activation.Submit).declare
                    b    <- form.field(0).on(Activation.Submit).declare
                    _ = form.satisfy("sum-10")(Signal.combineLatestAll(Seq(a.value, b.value)).map(_.sum == 10))
                    validSig <- form.isValid()
                    v0       <- validSig.current      // 0+0 ≠ 10 → invariant fails → invalid
                    _        <- a.valueRef.set(4)
                    _        <- b.valueRef.set(6)
                    v1       <- validSig.current      // 4+6 = 10 → valid
                    _        <- a.valueRef.set(1)     // sum 7 ≠ 10 → failing again
                    es       <- form.errorEntries
                    pre      <- es.current            // Reveal.OnSubmit + no submit → hidden
                    _        <- form.submit(Kyo.unit) // submit (fails) → submitCount > 0
                    post     <- es.current            // now revealed

                    form2 <- mkForm
                    x     <- form2.field(0).on(Activation.Submit).declare
                    _ = form2.satisfy("x-pos", Reveal.Immediate)(x.value.map(_ > 0))
                    es2 <- form2.errorEntries
                    imm <- es2.current // Reveal.Immediate + failing → shown at once
                yield (
                    v0,
                    v1,
                    pre.exists(_.error.code == "sum-10"),
                    post.exists(_.error.code == "sum-10"),
                    imm.exists(_.error.code == "x-pos")
                )
        yield
            assert(!invalidWhenOff, "a failing invariant makes isValid false")
            assert(validWhenMet, "a holding invariant makes isValid true")
            assert(!hiddenPreSubmit, "Reveal.OnSubmit invariant is hidden before submit")
            assert(shownPostSubmit, "Reveal.OnSubmit invariant is shown after submit")
            assert(immediateShown, "Reveal.Immediate invariant is shown as soon as it fails")
    }

    "reset after a successful submit leaves the form as it loaded: no error shown on the emptied field" in {
        for
            (count, shown, entries, value) <-
                for
                    form    <- mkForm
                    f       <- form.field.rules(Validator.required() and Validator.minLength(3)).declare
                    _       <- f.valueRef.set("a comment")
                    _       <- form.submit(form.reset) // the valid submit runs onValid, which empties the form
                    count   <- form.submitCount.current
                    shown   <- f.visibleError.current
                    es      <- form.errorEntries
                    entries <- es.current
                    value   <- f.valueRef.get
                yield (count, shown, entries, value)
        yield
            assert(value == "", "reset empties the field")
            assert(count == 0, s"reset closes the submit reveal, submitCount is $count")
            assert(shown.isEmpty, s"the emptied field shows no error before anyone touches or submits it: $shown")
            assert(entries.isEmpty, s"the error summary shows nothing either: $entries")
    }

    "reset does not count as an edit: a field that validates on change stays untouched and quiet" in {
        Scope.run {
            for
                form <- mkForm
                f    <- form.field.rules(Validator.required() and Validator.minLength(3)).declare // Blur + Change, the default
                _    <- f.wireChange                                                              // as Form.mountedWith does
                _    <- f.error.streamChanges.filter(_.nonEmpty).take(1).run                      // its silent first check ran
                _    <- f.valueRef.set("a comment")                                               // the reader types
                _    <- f.touched.streamChanges.filter(identity).take(1).run                      // the observer saw the edit
                _    <- form.submit(form.reset)
                // The observer validates the value the reset wrote: the raw error is back, and nothing may show it.
                _       <- f.error.streamChanges.filter(_.exists(_.code == "required")).take(1).run
                touched <- f.touched.current
                shown   <- f.visibleError.current
                // Submitting the emptied form again is refused and shows the error, as on a fresh form.
                ran    <- AtomicBoolean.init(false)
                _      <- form.submit(ran.set(true))
                ranV   <- ran.get
                again  <- form.submitCount.current
                shown2 <- f.visibleError.current
            yield
                assert(!touched, "the value reset wrote is not the reader's edit, so the field is not touched")
                assert(shown.isEmpty, s"the emptied field shows no error: $shown")
                assert(!ranV, "the emptied form does not submit")
                assert(again == 1, s"the refused submit opens the gate again, submitCount is $again")
                assert(shown2.exists(_.code == "required"), s"and the field shows its error: $shown2")
        }
    }

    "a reset no observer saw does not swallow the reader's next edit" in {
        Scope.run {
            for
                form <- mkForm
                f    <- form.field.rules(Validator.required() and Validator.minLength(3)).declare
                _    <- f.valueRef.set("a comment")
                _    <- form.reset // no change observer is wired yet, as before the field is mounted
                _    <- f.wireChange
                _     <- f.error.streamChanges.filter(_.nonEmpty).take(1).run // its silent first check ran
                _     <- f.valueRef.set("ok")                                 // the reader types
                _     <- f.touched.streamChanges.filter(identity).take(1).run // the edit marks the field touched
                _     <- f.error.streamChanges.filter(_.exists(_.code == "min-length")).take(1).run
                shown <- f.visibleError.current
            yield assert(shown.exists(_.code == "min-length"), s"the reader's short comment shows its error: $shown")
        }
    }

    /** A form's ungated validity, whether a rule of its one field has not judged the value, and the field's raw error. */
    private type Judgement = (Boolean, Boolean, Maybe[String])

    /** How a form that holds `f` alone judges it: its ungated validity, whether a rule has not judged the value, and the raw
      * error.
      */
    private def judged(form: Form, f: FormField[String])(using Frame): Judgement < Sync =
        for
            valid <- form.isValid()
            v     <- valid.current
            n     <- f.notChecked.current
            e     <- f.error.current
        yield (v, n, e.map(_.code))

    /** Wait until the field's change observer is parked again, `runs` reached `n` and the run that made it `n` has published.
      * Bounded, so a run that never comes reports here rather than at the suite's timeout.
      */
    private def judgedAgain(f: FormField[String], runs: AtomicInt, n: Int)(using Frame, kyo.test.AssertScope): Unit < Async =
        assertEventually(
            f.valueRef.waiters.map(_ == 1)
                .map(parked => if parked then runs.get.map(_ == n) else false)
                .map(ran => if ran then f.validating.current.map(!_) else false),
            within = 30.seconds
        )

    /** Mount the field `declare` builds, whose first rule counts its runs and is due on every change; reset it from the value
      * it mounted with; edit it and leave it; reset it from the edited value. Hands back how it is judged after each step, and
      * how often the counting rule ran.
      */
    private def resets(declare: (Form, AtomicInt) => FormField[String] < Sync)(using
        Frame,
        kyo.test.AssertScope
    ): (Judgement, Judgement, Judgement, Judgement, Int) < Async =
        Scope.run {
            for
                runs    <- AtomicInt.init(0)
                form    <- mkForm
                f       <- declare(form, runs)
                _       <- f.wireChange
                _       <- judgedAgain(f, runs, 1)
                atMount <- judged(form, f)
                _       <- form.reset // the value is the one it mounted with, so the reset writes nothing
                _       <- judgedAgain(f, runs, 2)
                unmoved <- judged(form, f)
                _       <- f.valueRef.set("abc")
                _       <- judgedAgain(f, runs, 3)
                _       <- f.onEvent("abc", Activation.Blur)
                edited  <- judged(form, f)
                _       <- form.reset // writes the empty value back
                _       <- judgedAgain(f, runs, 4)
                moved   <- judged(form, f)
                n       <- runs.get
            yield (atMount, unmoved, edited, moved, n)
        }

    "a reset judges a field whose rule waits for its blur as the mount did, whether or not the value moved" in {
        for
            (atMount, unmoved, edited, moved, runs) <- resets((form, runs) =>
                form.field("").rules(countedPlain(runs).on(Activation.Change) and Validator.required()).on(Activation.Blur).declare
            )
        yield
            assert(atMount == (false, true, Absent), s"the mount left the blur's rule not yet checked: $atMount")
            assert(edited == (true, false, Absent), s"the blur judged the edited value: $edited")
            assert(unmoved == atMount, s"a reset from the value it mounted with is judged as the mount was: $unmoved")
            assert(moved == atMount, s"and so is a reset from an edited value: $moved")
            assert(runs == 4, s"one run each for the mount, the edit and the two resets: $runs")
        end for
    }

    "a reset judges a field with an async rule as the mount did, whether or not the value moved" in {
        for
            (atMount, unmoved, edited, moved, runs) <- resets((form, runs) =>
                form.field("").rules(countedPlain(runs).on(Activation.Change) and Validator.async[String](v =>
                    (if v.isEmpty then Present(FieldError("required")) else Absent): Maybe[FieldError]
                )).declare
            )
        yield
            assert(atMount == (false, true, Absent), s"the mount left the async rule, held to the blur, not yet checked: $atMount")
            assert(edited == (true, false, Absent), s"the blur judged the edited value: $edited")
            assert(unmoved == atMount, s"a reset from the value it mounted with is judged as the mount was: $unmoved")
            assert(moved == atMount, s"and so is a reset from an edited value: $moved")
            assert(runs == 4, s"one run each for the mount, the edit and the two resets: $runs")
        end for
    }

    "a reset judges a field whose rules all run on the change as the mount did, whether or not the value moved" in {
        for
            (atMount, unmoved, edited, moved, runs) <- resets((form, runs) =>
                form.field("").rules(countedPlain(runs).on(Activation.Change) and Validator.required()).declare
            )
        yield
            assert(atMount == (false, false, Present("required")), s"the mount judged the empty value: $atMount")
            assert(edited == (true, false, Absent), s"the edit judged the new value: $edited")
            assert(unmoved == atMount, s"a reset from the value it mounted with is judged as the mount was: $unmoved")
            assert(moved == atMount, s"and so is a reset from an edited value: $moved")
            assert(runs == 4, s"one run each for the mount, the edit and the two resets: $runs")
        end for
    }

    "a bound field that turns invalid and valid again while the reader types keeps its element" in {
        // The keystroke that flips a field invalid must land in the element on the page. A control rebuilt by its
        // validity replaced the textarea at that moment, and the text went into the discarded one.
        Scope.run {
            for
                form <- mkForm
                f    <- form.field.rules(Validator.required() and Validator.minLength(3)).declare
                _    <- f.wireChange                                         // as Form.mountedWith does
                _    <- f.error.streamChanges.filter(_.nonEmpty).take(1).run // its silent first check ran
                ui = uic.TextArea().bind(f).render
                regions  <- regionsAbove(ui, "p-textarea")
                before   <- elementWithClass(ui, "p-textarea")
                _        <- f.valueRef.set("ab")                                 // the reader types
                _        <- f.touched.streamChanges.filter(identity).take(1).run // the edit marks the field touched
                _        <- f.error.streamChanges.filter(_.exists(_.code == "min-length")).take(1).run
                shownOn  <- f.visibleError.current
                invalid  <- elementWithClass(ui, "p-textarea")
                rowsOn   <- elementsWithClass(ui, "p-uic-invalid-message")
                redOn    <- Kyo.foreach(invalid.attrs.reactiveClasses.get("p-invalid").toSeq)(_.current)
                _        <- f.valueRef.set("abcd")
                _        <- f.error.streamChanges.filter(_.isEmpty).take(1).run
                shownOff <- f.visibleError.current
                valid    <- elementWithClass(ui, "p-textarea")
                rowsOff  <- elementsWithClass(ui, "p-uic-invalid-message")
                redOff   <- Kyo.foreach(valid.attrs.reactiveClasses.get("p-invalid").toSeq)(_.current)
            yield
                assert(shownOn.exists(_.code == "min-length") && shownOff.isEmpty, s"the error shows, then clears: $shownOn, $shownOff")
                assert(regions == 0, s"the textarea sits under $regions validity region(s)")
                assert(before eq invalid, "turning invalid rebuilt the textarea")
                assert(invalid eq valid, "turning valid again rebuilt the textarea")
                assert(redOn == Chunk(true) && redOff == Chunk(false), s"the red state is patched on the one element: $redOn, $redOff")
                assert(rowsOn.size == 1 && rowsOff.isEmpty, "only the message row comes and goes")
        }
    }

    "reset on a sub-form after a failed submit leaves the rest of the form showing what the submit found" in {
        for
            (count, shown) <-
                for
                    form  <- mkForm
                    name  <- form.field.rules(Validator.required()).declare
                    sub   <- form.child()
                    _     <- sub.field.rules(Validator.required()).declare
                    _     <- form.submit(Kyo.unit) // both empty: fails and opens the gate
                    _     <- sub.reset
                    count <- form.submitCount.current
                    shown <- name.visibleError.current
                yield (count, shown)
        yield
            assert(count == 1, s"a sub-form's reset keeps the gate open, submitCount is $count")
            assert(shown.exists(_.code == "required"), s"the root's empty field still shows its error: $shown")
    }

    "a server error is value-scoped: shown while the value is unchanged, hidden after an edit" in {
        for
            (shown, afterEdit) <-
                for
                    (f, valueRef) <- mkField("", Validator.all(), "bob")
                    _             <- f.setError(Present(FieldError("username-taken")))
                    ve1           <- f.visibleError.current
                    _             <- valueRef.set("bobby")
                    ve2           <- f.visibleError.current
                yield (ve1.isDefined, ve2.isDefined)
        yield
            assert(shown, "server error is visible while the value matches")
            assert(!afterEdit, "server error disappears the moment the value changes")
    }

    "form-level raise errors do NOT disable submit (they cannot be fixed client-side)" in {
        for
            disabled <-
                for
                    form <- mkForm
                    _    <- form.field.rules(Validator.required()).on(Activation.Submit).declare
                    _    <- form.raise(FieldError("invalid-credentials"))
                    gate <- form.submitDisabled
                    d    <- gate.signal.current
                yield d
        yield assert(!disabled, "a raised form-level error must leave the submit button enabled")
    }

    "a displayed FIELD error still disables submit, then re-enables once the value changes" in {
        for
            (blocked, unblocked) <-
                for
                    (f, valueRef) <- mkField("", Validator.all(), "bob")
                    _             <- f.setError(Present(FieldError("username-taken")))
                    c1            <- f.visibleErrorChunk.current
                    _             <- valueRef.set("bobby")
                    c2            <- f.visibleErrorChunk.current
                yield (c1.nonEmpty, c2.nonEmpty)
        yield
            assert(blocked, "field error contributes to the gate while shown")
            assert(!unblocked, "editing the field drops it from the gate")
    }

    "a typed Boolean field validates against its own type and value-scopes its server error" in {
        for
            (invalidWhenFalse, validWhenTrue, serverShown, serverHidden) <-
                for
                    (f, valueRef) <- mkField(false, Validator.satisfy[Boolean]("must-accept")(b => b), false)
                    r1            <- f.validateForSubmit                         // false → the accept rule fails
                    _             <- valueRef.set(true)
                    r2            <- f.validateForSubmit                         // true  → the accept rule passes
                    _             <- f.setError(Present(FieldError("server-x"))) // scoped to `true`
                    s1            <- f.visibleError.current
                    _             <- valueRef.set(false)                         // flip → value diverges from the scope
                    s2            <- f.visibleError.current
                yield (r1.isDefined, r2.isDefined, s1.isDefined, s2.isDefined)
        yield
            assert(invalidWhenFalse, "false fails the accept rule")
            assert(!validWhenTrue, "true passes the accept rule")
            assert(serverShown, "server error visible while the Boolean is unchanged")
            assert(!serverHidden, "server error hides once the Boolean flips")
    }

    "a typed Double field (InputNumber) validates and value-scopes across a numeric edit" in {
        for
            (tooLow, okAtEdge, serverShown, serverHidden) <-
                for
                    (f, valueRef) <- mkField(0.0, Validator.satisfy[Double]("min-age")(_ >= 18.0), 0.0)
                    r1            <- f.validateForSubmit                         // 0.0 → below the threshold
                    _             <- valueRef.set(21.0)
                    r2            <- f.validateForSubmit                         // 21.0 → passes
                    _             <- f.setError(Present(FieldError("server-x"))) // scoped to 21.0
                    s1            <- f.visibleError.current
                    _             <- valueRef.set(22.0)                          // any numeric edit diverges from the scope
                    s2            <- f.visibleError.current
                yield (r1.isDefined, r2.isDefined, s1.isDefined, s2.isDefined)
        yield
            assert(tooLow, "0.0 fails the minimum rule")
            assert(!okAtEdge, "21.0 passes the minimum rule")
            assert(serverShown, "server error visible while the number is unchanged")
            assert(!serverHidden, "server error hides once the number changes")
    }

    "a typed Set[A] field (MultiSelect over its options) validates on selection and value-scopes on change" in {
        for
            (emptyFails, nonEmptyOk, serverShown, serverHidden) <-
                for
                    (f, valueRef) <- mkField(Set.empty[Lang], Validator.satisfy[Set[Lang]]("pick-one")(_.nonEmpty), Set.empty[Lang])
                    r1            <- f.validateForSubmit                         // {} → nothing selected
                    _             <- valueRef.set(Set(Lang.De))
                    r2            <- f.validateForSubmit                         // {De} → passes
                    _             <- f.setError(Present(FieldError("server-x"))) // scoped to {De}
                    s1            <- f.visibleError.current
                    _             <- valueRef.set(Set(Lang.De, Lang.En))         // selection change diverges from the scope
                    s2            <- f.visibleError.current
                yield (r1.isDefined, r2.isDefined, s1.isDefined, s2.isDefined)
        yield
            assert(emptyFails, "empty selection fails the pick-one rule")
            assert(!nonEmptyOk, "a non-empty selection passes")
            assert(serverShown, "server error visible while the selection is unchanged")
            assert(!serverHidden, "server error hides once the selection changes")
    }

    "a typed Int numberField derives the value from the bound Double and rounds defensively" in {
        for
            (initial, tooLow, ok, rounded) <-
                for
                    form <- mkForm
                    nf   <- form.numberField[Int](1).rules(Validator.satisfy[Int]("min-3")(_ >= 3)).on(Activation.Submit).declare
                    v0   <- nf.value.current                // initial 1
                    r0   <- nf.underlying.validateForSubmit // 1 < 3 → fails
                    _    <- nf.underlying.valueRef.set(5.0)
                    r1   <- nf.underlying.validateForSubmit // 5 ≥ 3 → passes
                    _    <- nf.underlying.valueRef.set(3.5) // defensive: masking normally prevents this
                    v1   <- nf.value.current
                yield (v0, r0.isDefined, r1.isEmpty, v1)
        yield
            assert((initial == 1), "initial Double derives to Int 1")
            assert(tooLow, "1 fails the min-3 rule")
            assert(ok, "5 passes")
            assert((rounded == 4), "a stray 3.5 rounds to 4 (rint), never a fractional Int")
    }

    "a FormField[Maybe[LocalDate]] binds a DatePicker, and Validator.chosen is its required" in {
        import kyo.uic
        for
            (emptyErr, picked, pickedErr, blurred) <-
                for
                    form    <- mkForm
                    field   <- form.field(Maybe.empty[LocalDate]).rules(Validator.chosen()).on(Activation.Blur).declare
                    r0      <- field.validateForSubmit // nothing picked: the required rule fails
                    oref    <- Signal.initRef(true)
                    mref    <- Signal.initRef("2026-07")
                    cur     <- Signal.initRef("")
                    vref    <- Signal.initRef(uic.DatePickerView.Date)
                    seed    <- Signal.initRef(false)
                    restore <- Signal.initRef(Maybe.empty[uic.DatePicker.Restore])
                    draft   <- Signal.initRef(Maybe.empty[String])
                    refs = uic.DatePicker.Refs(oref, uic.DatePicker.ShownMonth.Own(mref), cur, vref, seed, restore, draft)
                    ui   = uic.DatePicker[LocalDate]().bind(field).wired(refs, "dp", _ => ())
                    days <- elements(ui).map(_.filter(d =>
                        d.attrs.cssClasses.contains("p-datepicker-day") && d.children.exists(_ == UI.Ast.Text("9"))
                    ))
                    _     <- click(days.head)
                    v1    <- field.valueRef.get // the pick wrote the field's own ref, typed
                    r1    <- field.validateForSubmit
                    input <- elements(ui).map(_.find(_.attrs.cssClasses.contains("p-datepicker-input")).get)
                    _     <- input.attrs.onBlur match
                        case Present(eff) => handled(eff)
                        case Absent       => throw new AssertionError("bind wires no blur trigger")
                    t <- field.touched.current
                yield (r0.map(_.code), v1, r1, t)
        yield
            assert(emptyErr.contains("required"), "an empty picker fails the required rule")
            assert(picked == Present(LocalDate.of(2026, 7, 9)), "the pick is the field's value, no string behind it")
            assert(pickedErr.isEmpty, "a picked date passes")
            assert(blurred, "the picker's blur is the field's Blur trigger")
        end for
    }

    "Validator.ifPresent states a rule over the date without making it required" in {
        val cutoff = LocalDate.of(2020, 1, 1)
        for
            (empty, early, late) <-
                for
                    form  <- mkForm
                    field <- form.field(Maybe.empty[LocalDate])
                        .rules(Validator.ifPresent(Validator.satisfy[LocalDate]("after-2020")(_.isAfter(cutoff))))
                        .on(Activation.Submit)
                        .declare
                    r0 <- field.validateForSubmit
                    _  <- field.valueRef.set(Present(LocalDate.of(2019, 6, 1)))
                    r1 <- field.validateForSubmit
                    _  <- field.valueRef.set(Present(LocalDate.of(2021, 6, 1)))
                    r2 <- field.validateForSubmit
                yield (r0, r1.map(_.code), r2)
        yield
            assert(empty.isEmpty, "an empty field passes: the rule is about the date, not about having one")
            assert(early.contains("after-2020"))
            assert(late.isEmpty)
        end for
    }

    "a submit whose handler fails still ends submitting, and the failure reaches the rules" in {
        for
            seen             <- Promise.init[Any, Any]
            (outcome, still) <-
                kyo.internal.Failures.within(
                    kyo.internal.FailureFrame.Boundary(UI.boundary.onNotice { case UI.Failed(raw) =>
                        seen.completeDiscard(Result.succeed(raw))
                    })
                ) {
                    for
                        form    <- mkForm
                        outcome <- Abort.run[Any](form.submit(Abort.fail("server said no")))
                        still   <- form.submitting.current
                    yield (outcome, still)
                }
            noticed <- seen.get
        yield
            assert(outcome.isSuccess, "the form reported the failure itself, so its own handler completes")
            assert(!still, "a failed handler leaves the form open for the next submit")
            assert(String.valueOf(noticed) == "server said no", "the failure reached the onNotice sink")
    }

    // ---- submit hands over the validated values ---------------------------------------------

    "submit hands the handler what the field's rules refined to" in {
        val day = LocalDate.of(2026, 7, 9)
        for
            got <-
                for
                    form <- mkForm
                    due  <- form.field(Maybe.empty[LocalDate]).rules(Validator.chosen()).on(Activation.Submit).declare
                    _    <- due.valueRef.set(Present(day))
                    sink <- AtomicRef.init(Maybe.empty[LocalDate])
                    // The ascription is the point: the handler receives the LocalDate, not the Maybe the field holds.
                    _   <- form.submit(due)((d: LocalDate) => sink.set(Present(d)))
                    got <- sink.get
                yield got
        yield assert(got == Present(day), s"the handler received the picked date: $got")
        end for
    }

    "the refined type reaches the handler at compile time" in {
        typeCheck(
            submitPreamble +
                "def x(f: Form)(using Frame) = f.field(Maybe.empty[java.time.LocalDate]).rules(Validator.chosen()).declare" +
                ".map(due => f.submit(due)(d => { val day: java.time.LocalDate = d; Kyo.unit }))"
        )
        typeCheckFailure(
            submitPreamble +
                "def x(f: Form)(using Frame) = f.field(Maybe.empty[java.time.LocalDate]).rules(Validator.chosen()).declare" +
                ".map(due => f.submit(due)(d => { val held: Maybe[java.time.LocalDate] = d; Kyo.unit }))"
        )
        // With no rules the field validates to its value type.
        typeCheck(submitPreamble +
            "def x(f: Form)(using Frame) = f.field.declare.map(s => f.submit(s)(t => { val text: String = t; Kyo.unit }))")
    }

    "submit with several fields hands each its validated value, in the order named" in {
        for
            got <-
                for
                    form <- mkForm
                    age  <- form.field("42").rules(Validator.required() and digits).on(Activation.Submit).declare
                    lang <- form.field(Maybe.empty[Lang]).rules(Validator.chosen()).on(Activation.Submit).declare
                    _    <- lang.valueRef.set(Present(Lang.En))
                    sink <- AtomicRef.init(Maybe.empty[(Int, Lang)])
                    _    <- form.submit(age, lang)((n: Int, l: Lang) => sink.set(Present((n, l))))
                    got  <- sink.get
                yield got
        yield assert(got == Present((42, Lang.En)), s"both refined values arrive: $got")
        end for
    }

    "submit with four fields takes a number field too, refined through its own rules" in {
        for
            got <-
                for
                    form <- mkForm
                    name <- form.field("Ada").rules(Validator.required()).on(Activation.Submit).declare
                    qty  <- form.numberField(3).rules(Validator.min(1) and
                        Validator.refine[Int, Long]("odd")(n => if n % 2 == 1 then Present(n.toLong) else Absent)).declare
                    lang  <- form.field(Maybe.empty[Lang]).rules(Validator.chosen()).on(Activation.Submit).declare
                    stock <- form.field(true).declare
                    _     <- lang.valueRef.set(Present(Lang.De))
                    sink  <- AtomicRef.init(Maybe.empty[(String, Long, Lang, Boolean)])
                    _   <- form.submit(name, qty, lang, stock)((n: String, q: Long, l: Lang, s: Boolean) => sink.set(Present((n, q, l, s))))
                    got <- sink.get
                yield got
        yield assert(got == Present(("Ada", 3L, Lang.De, true)), s"every value arrives, the number as its refined Long: $got")
        end for
    }

    "submit does not call the handler when a named field is invalid, and focuses it" in {
        for
            (ran, focused, dueId, shown) <-
                for
                    sink  <- Signal.initRef(Chunk.empty[String])
                    form  <- mkFocusForm(sink)
                    name  <- form.field("Ada").rules(Validator.required()).on(Activation.Submit).declare
                    due   <- form.field(Maybe.empty[LocalDate]).rules(Validator.chosen()).on(Activation.Submit).declare
                    ran   <- AtomicBoolean.init(false)
                    _     <- form.submit(name, due)((_: String, _: LocalDate) => ran.set(true))
                    r     <- ran.get
                    f     <- sink.get
                    shown <- due.visibleError.current
                yield (r, f, due.domId, shown)
        yield
            assert(!ran, "an empty required date keeps the handler from running")
            assert(focused == Chunk(dueId), s"focus goes to the invalid named field: $focused")
            assert(shown.map(_.code) == Present("required"), s"and it shows its error: $shown")
        end for
    }

    "submit does not call the handler when a field it does not name is invalid" in {
        for
            ran <-
                for
                    form <- mkForm
                    name <- form.field("Ada").rules(Validator.required()).on(Activation.Submit).declare
                    _    <- form.field.rules(Validator.required()).on(Activation.Submit).declare
                    ran  <- AtomicBoolean.init(false)
                    _    <- form.submit(name)(_ => ran.set(true))
                    r    <- ran.get
                yield r
        yield assert(!ran, "the rest of the form still has to hold")
        end for
    }

    "a field from another form, named in a submit, shows the error that blocked it" in {
        for
            (ran, touchedShown, onSubmitShown, touchedMsg, untouched, unnamedShown, afterReset) <-
                for
                    formA     <- mkForm
                    formB     <- mkForm
                    whenTouch <- formB.field.rules(Validator.required()).on(Activation.Submit).declare
                    onSubmit  <- formB.field.rules(Validator.required()).on(Activation.Submit).revealWhen(Reveal.OnSubmit).declare
                    unnamed   <- formB.field.rules(Validator.required()).on(Activation.Submit).declare
                    ran       <- AtomicBoolean.init(false)
                    _         <- formA.submit(whenTouch, onSubmit)((_, _) => ran.set(true))
                    r         <- ran.get
                    e1        <- whenTouch.visibleError.current
                    e2        <- onSubmit.visibleError.current
                    m1        <- whenTouch.message.current
                    t         <- whenTouch.touched.current
                    u         <- unnamed.visibleError.current
                    _         <- formB.reset
                    gone      <- Kyo.foreach(Chunk(whenTouch, onSubmit))(_.visibleError.current)
                yield (r, e1.map(_.code), e2.map(_.code), m1, t, u, gone)
        yield
            assert(!ran, "the empty foreign field blocks the handler")
            assert(touchedShown == Present("required"), s"a WhenTouched field the submit named shows why: $touchedShown")
            assert(onSubmitShown == Present("required"), s"so does an OnSubmit one: $onSubmitShown")
            assert(touchedMsg == Present("required"), s"its message row too: $touchedMsg")
            assert(!untouched, "naming a field in a submit does not pretend the reader touched it")
            assert(unnamedShown.isEmpty, s"the form the field belongs to was not submitted, so its other fields stay quiet: $unnamedShown")
            assert(afterReset == Chunk(Absent, Absent), s"its own form's reset closes the gate again: $afterReset")
        end for
    }

    "a named field's rules run once per submit" in {
        for
            (named, plain) <-
                for
                    runs <- AtomicInt.init(0)
                    form <- mkForm
                    f    <- form.field(
                        "x"
                    ).rules(Validator.async[String](_ => runs.incrementAndGet.andThen(Absent))).on(Activation.Submit).declare
                    _ <- form.submit(f)(_ => Kyo.unit)
                    n <- runs.get
                    _ <- form.submit(Kyo.unit)
                    p <- runs.get
                yield (n, p - n)
        yield
            assert(named == 1, s"the named submit validated the field once: $named runs")
            assert(plain == 1, s"as the plain submit does: $plain runs")
        end for
    }

    "the handler receives the value the submit validated, not one written while it validated" in {
        for
            (got, now) <-
                for
                    form <- mkForm
                    f    <- form.field("first").rules(Validator.required()).on(Activation.Submit).declare
                    // A check that runs inside the submit's validation and edits the field meanwhile,
                    // the way a reader types while an async rule is in flight.
                    _    <- f.addRule(Validator.async[String](_ => f.valueRef.set("later").andThen(Absent)))
                    sink <- AtomicRef.init("")
                    _    <- form.submit(f)(v => sink.set(v))
                    got  <- sink.get
                    now  <- f.valueRef.get
                yield (got, now)
        yield
            assert(now == "later", "the edit landed")
            assert(got == "first", s"but the handler got the value the rules judged: $got")
        end for
    }

    // ---- text that needs no code ---------------------------------------------------------------

    "a rule's message is what the default translator shows" in {
        for
            (empty, short) <-
                for
                    form <- mkForm
                    body <- form.field
                        .rules(
                            Validator.required().message("Write something first.") and
                                Validator.minLength(3).message("A comment has at least 3 characters.")
                        )
                        .on(Activation.Submit)
                        .declare
                    _     <- form.submit(Kyo.unit)
                    empty <- body.message.current
                    _     <- body.valueRef.set("ab")
                    _     <- form.submit(Kyo.unit)
                    short <- body.message.current
                yield (empty, short)
        yield
            assert(empty == Present("Write something first."), s"$empty")
            assert(short == Present("A comment has at least 3 characters."), s"$short")
        end for
    }

    "a table entry speaks for its code over a rule's message; a rule with its own code keeps its text" in {
        for
            (byDefaultCode, byOwnCode, unlisted) <-
                for
                    form <- mkFormWith(ErrorTranslator.englishMap(Map("required" -> "Required.")))
                    a    <- form.field.rules(Validator.required().message("Write something first.")).on(Activation.Submit).declare
                    b    <- form.field.rules(Validator.required("comment-empty").message("Write something first.")).on(Activation.Submit)
                        .declare
                    c  <- form.field.rules(Validator.minLength(3).message("Three at least.")).on(Activation.Submit).declare
                    _  <- c.valueRef.set("ab")
                    _  <- form.submit(Kyo.unit)
                    ma <- a.message.current
                    mb <- b.message.current
                    mc <- c.message.current
                yield (ma, mb, mc)
        yield
            assert(byDefaultCode == Present("Required."), s"the translator has the first word: $byDefaultCode")
            assert(byOwnCode == Present("Write something first."), s"a code the table does not list shows the rule's text: $byOwnCode")
            assert(unlisted == Present("Three at least."), s"$unlisted")
        end for
    }

    "raise takes text, under the one message code" in {
        for
            (errors, shown) <-
                for
                    form   <- mkForm
                    _      <- form.raise("The server is down, try again in a minute.")
                    errors <- form.errors.current
                    shown  <- form.translator.translate(errors.head).current
                yield (errors, shown)
        yield
            assert(errors == Chunk(FieldError(FieldError.messageCode, fallback = Present("The server is down, try again in a minute."))))
            assert(shown == "The server is down, try again in a minute.")
        end for
    }

    "a field's server error takes text, value-scoped like any other" in {
        for
            (shown, code, afterEdit) <-
                for
                    (f, valueRef) <- mkField("", Validator.all(), "bob")
                    _             <- f.setError("That name is taken.")
                    shown         <- f.message.current
                    err           <- f.visibleError.current
                    _             <- valueRef.set("bobby")
                    after         <- f.message.current
                yield (shown, err.map(_.code), after)
        yield
            assert(shown == Present("That name is taken."), s"$shown")
            assert(code == Present(FieldError.messageCode))
            assert(afterEdit.isEmpty, "the first edit hides it")
        end for
    }

    // ---- when a rule runs ----------------------------------------------------------------------
    //
    // Each rule carries the events it runs on: its own `on`, else the field's, else its default
    // (Blur + Change, Blur for Validator.async). A rule not due for an event keeps its last result
    // while its input is unchanged and is "not yet checked" otherwise, which stops the chain and
    // keeps the form invalid. A submit runs everything.

    /** A check that counts its runs and fails on the value `bad`, built the way an async rule is. */
    private def counted(runs: AtomicInt, bad: String = "taken")(using Frame): Validator[String] =
        Validator.async[String](v =>
            runs.incrementAndGet.map(_ => (if v == bad then Present(FieldError(bad)) else Absent): Maybe[FieldError])
        )

    /** A check that counts its runs and passes everything, built with `Validator(...)`. */
    private def countedPlain(runs: AtomicInt)(using Frame): Validator[String] =
        Validator[String](_ => runs.incrementAndGet.andThen(Absent))

    "an async rule on(Blur) does not run while the reader types, and runs once when they leave" in {
        for
            (typing, left) <-
                for
                    runs <- AtomicInt.init(0)
                    form <- mkForm
                    f    <- form.field("").rules(counted(runs).on(Activation.Blur)).declare
                    _    <- f.valueRef.set("a")
                    _    <- f.onEvent("a", Activation.Change)
                    _    <- f.valueRef.set("ab")
                    _    <- f.onEvent("ab", Activation.Change)
                    t    <- runs.get
                    _    <- f.onEvent("ab", Activation.Blur)
                    l    <- runs.get
                yield (t, l)
        yield
            assert(typing == 0, s"two keystrokes ran it $typing times")
            assert(left == 1, s"leaving the field ran it $left times")
        end for
    }

    "Validator.async waits for the blur by default, and Validator(...) checks every change" in {
        for
            (asyncTyping, plainTyping, asyncLeft) <-
                for
                    asyncRuns <- AtomicInt.init(0)
                    plainRuns <- AtomicInt.init(0)
                    form      <- mkForm
                    a         <- form.field("").rules(counted(asyncRuns)).declare
                    p         <- form.field("").rules(countedPlain(plainRuns)).declare
                    _         <- a.onEvent("x", Activation.Change)
                    _         <- p.onEvent("x", Activation.Change)
                    at        <- asyncRuns.get
                    pt        <- plainRuns.get
                    _         <- a.onEvent("x", Activation.Blur)
                    al        <- asyncRuns.get
                yield (at, pt, al)
        yield
            assert(asyncTyping == 0, s"an async rule ran $asyncTyping times while the reader typed")
            assert(plainTyping == 1, s"a plain rule ran $plainTyping times on one change")
            assert(asyncLeft == 1, s"the blur ran the async rule $asyncLeft times")
        end for
    }

    "a deferred rule's error stays while the value is unchanged, and goes with the next keystroke" in {
        for
            (typed, left, again, edited, runs) <-
                for
                    runs <- AtomicInt.init(0)
                    form <- mkForm
                    f    <- form.field("").rules(counted(runs)).declare
                    _    <- f.valueRef.set("taken")
                    _    <- f.onEvent("taken", Activation.Change)
                    e0   <- f.error.current
                    _    <- f.onEvent("taken", Activation.Blur)
                    e1   <- f.error.current
                    _    <- f.revalidate // a change event over the same value, as a reset write sends one
                    e2   <- f.error.current
                    _    <- f.valueRef.set("taken!")
                    _    <- f.onEvent("taken!", Activation.Change)
                    e3   <- f.error.current
                    n    <- runs.get
                yield (e0, e1.map(_.code), e2.map(_.code), e3, n)
        yield
            assert(typed == Absent, s"not checked yet, so there is nothing to say: $typed")
            assert(left == Present("taken"), s"the blur checked it: $left")
            assert(again == Present("taken"), s"the same value keeps the verdict: $again")
            assert(edited == Absent, s"a new value drops the verdict about the old one: $edited")
            assert(runs == 1, s"and only the blur ran the rule: $runs runs")
        end for
    }

    "a rule after one not yet checked does not run, and the form is not valid until it is" in {
        for
            (initially, laterTyping, ungatedTyping, gatedTyping, errTyping, laterLeft, ungatedLeft, gatedLeft) <-
                for
                    deferred <- AtomicInt.init(0)
                    later    <- AtomicInt.init(0)
                    form     <- mkForm
                    f        <- form.field("").rules(counted(deferred) and countedPlain(later)).declare
                    ungated  <- form.isValid()
                    gated    <- form.isValid(onlyVisible = true)
                    u0       <- ungated.current
                    _        <- f.onEvent("x", Activation.Change)
                    l1       <- later.get
                    u1       <- ungated.current
                    g1       <- gated.current
                    e1       <- f.error.current
                    _        <- f.onEvent("x", Activation.Blur)
                    l2       <- later.get
                    u2       <- ungated.current
                    g2       <- gated.current
                yield (u0, l1, u1, g1, e1, l2, u2, g2)
        yield
            assert(initially, "nothing has run yet, and nothing is wrong")
            assert(laterTyping == 0, s"the rule after the deferred one ran $laterTyping times")
            assert(!ungatedTyping, "isValid() does not count an unchecked rule as passed")
            assert(gatedTyping, "isValid(onlyVisible = true) counts only what is shown, and an unchecked rule shows nothing")
            assert(errTyping == Absent, s"an unchecked rule shows no error: $errTyping")
            assert(laterLeft == 1, s"once the deferred rule passed, the next one ran: $laterLeft")
            assert(ungatedLeft && gatedLeft, "and the form is valid")
        end for
    }

    "submit runs every rule, whatever it waits for, and hands over what they refined to" in {
        for
            (before, got, blurRuns, submitRuns) <-
                for
                    blur   <- AtomicInt.init(0)
                    submit <- AtomicInt.init(0)
                    form   <- mkForm
                    f      <- form.field("").rules(
                        counted(blur) and countedPlain(submit).on(Activation.Submit) and digits
                    ).declare
                    _    <- f.valueRef.set("42")
                    _    <- f.onEvent("42", Activation.Change)
                    b0   <- blur.get
                    s0   <- submit.get
                    sink <- AtomicRef.init(Maybe.empty[Int])
                    _    <- form.submit(f)(v => sink.set(Present(v)))
                    got  <- sink.get
                    b1   <- blur.get
                    s1   <- submit.get
                yield ((b0, s0), got, b1, s1)
        yield
            assert(before == (0, 0), s"typing ran neither deferred rule: $before")
            assert(got == Present(42), s"the handler got the refined value: $got")
            assert(blurRuns == 1 && submitRuns == 1, s"the submit ran both: $blurRuns, $submitRuns")
        end for
    }

    "a rule on(Submit) runs at submit and at no other time" in {
        for
            (beforeSubmit, afterSubmit) <-
                for
                    runs <- AtomicInt.init(0)
                    form <- mkForm
                    f    <- form.field("").rules(countedPlain(runs).on(Activation.Submit)).declare
                    _    <- f.valueRef.set("x")
                    _    <- f.onEvent("x", Activation.Change)
                    _    <- f.onEvent("x", Activation.Blur)
                    _    <- f.revalidate
                    n0   <- runs.get
                    _    <- f.validateForSubmit
                    n1   <- runs.get
                yield (n0, n1)
        yield
            assert(beforeSubmit == 0, s"a change, a blur and a silent revalidation ran it $beforeSubmit times")
            assert(afterSubmit == 1, s"the submit ran it: $afterSubmit")
        end for
    }

    "a rule's own on beats the field's" in {
        for
            (asksChange, asksBlur) <-
                for
                    r1   <- AtomicInt.init(0)
                    r2   <- AtomicInt.init(0)
                    form <- mkForm
                    f1   <- form.field("").rules(countedPlain(r1).on(Activation.Change)).on(Activation.Blur).declare
                    f2   <- form.field("").rules(countedPlain(r2).on(Activation.Blur)).on(Activation.Change).declare
                    _    <- f1.onEvent("x", Activation.Change)
                    _    <- f2.onEvent("x", Activation.Change)
                    n1   <- r1.get
                    n2   <- r2.get
                yield (n1, n2)
        yield
            assert(asksChange == 1, s"a rule on(Change) in a field on(Blur) runs on a change: $asksChange")
            assert(asksBlur == 0, s"a rule on(Blur) in a field on(Change) does not: $asksBlur")
        end for
    }

    "the field's on beats a rule's default, Validator.async's included" in {
        for
            (asyncInChangeField, plainInBlurField) <-
                for
                    r1   <- AtomicInt.init(0)
                    r2   <- AtomicInt.init(0)
                    form <- mkForm
                    f1   <- form.field("").rules(counted(r1)).on(Activation.Change).declare
                    f2   <- form.field("").rules(countedPlain(r2)).on(Activation.Blur).declare
                    _    <- f1.onEvent("x", Activation.Change)
                    _    <- f2.onEvent("x", Activation.Change)
                    n1   <- r1.get
                    n2   <- r2.get
                yield (n1, n2)
        yield
            assert(asyncInChangeField == 1, s"an async rule in a field on(Change) runs on a change: $asyncInChangeField")
            assert(plainInBlurField == 0, s"a plain rule in a field on(Blur) does not: $plainInBlurField")
        end for
    }

    "the nearer on wins: an outer on only fills the rules that state none" in {
        for
            (onChange, onBlur) <-
                for
                    a    <- AtomicInt.init(0)
                    b    <- AtomicInt.init(0)
                    form <- mkForm
                    f    <- form.field("").rules((countedPlain(a).on(Activation.Change) and countedPlain(b)).on(Activation.Blur)).declare
                    _    <- f.onEvent("x", Activation.Change)
                    a1   <- a.get
                    b1   <- b.get
                    _    <- f.onEvent("x", Activation.Blur)
                    a2   <- a.get
                    b2   <- b.get
                yield ((a1, b1), (a2, b2))
        yield
            assert(onChange == (1, 0), s"the change ran the rule that asked for it and held back the one the outer Blur fills: $onChange")
            assert(onBlur == (1, 1), s"the blur reused the first rule's verdict for the same value and ran the second: $onBlur")
        end for
    }

    "a field that declares only Blur still sees the changes a rule asks for, and is not touched by them" in {
        for
            (runs, touched) <-
                for
                    runs <- AtomicInt.init(0)
                    form <- mkForm
                    f    <- form.field("").rules(countedPlain(runs).on(Activation.Change)).on(Activation.Blur).declare
                    out  <- Scope.run {
                        for
                            _ <- f.wireChange
                            // The observer checks the mount value, then waits on the ref again.
                            _ <- assertEventually(f.valueRef.waiters.map(_ == 1))
                            _ <- f.valueRef.set("x")
                            _ <- assertEventually(runs.get.map(_ == 2))
                            n <- runs.get
                            t <- f.touched.current
                        yield (n, t)
                    }
                yield out
        yield
            assert(runs == 2, s"the mount value and the edit both ran the rule: $runs")
            assert(!touched, "touched follows the field's own events, and this field declared only Blur")
        end for
    }

    "a field-array row whose rule waits for the blur keeps the form invalid until it has run" in {
        for
            (whileUnchecked, afterBlur, runs) <-
                for
                    runs     <- AtomicInt.init(0)
                    captured <- AtomicRef.init(Chunk.empty[FormField[String]])
                    form     <- mkForm
                    arr      <- form.fieldArray(0) { (row, _) =>
                        for
                            fld <- row.field("ok").rules(counted(runs)).declare
                            _   <- captured.updateAndGet(_ :+ fld)
                        yield (fragment(): UI)
                    }
                    valid <- form.isValid()
                    _     <- arr.add
                    flds  <- captured.get
                    fld = flds.head
                    // The row's change observer checks the mount value, then waits on the ref again.
                    _  <- assertEventually(fld.valueRef.waiters.map(_ == 1))
                    v0 <- valid.current
                    _  <- fld.onEvent("ok", Activation.Blur)
                    v1 <- valid.current
                    n  <- runs.get
                    _  <- arr.removeAt(0)
                yield (v0, v1, n)
        yield
            assert(!whileUnchecked, "a row's unchecked rule keeps the form invalid")
            assert(afterBlur, "and the blur that checks it makes it valid")
            assert(runs == 1, s"only the blur ran it: $runs")
        end for
    }

    "a number field's rules keep their activation through the codec" in {
        for
            (typing, left) <-
                for
                    runs <- AtomicInt.init(0)
                    form <- mkForm
                    n    <- form.numberField(1).rules(Validator[Int](_ => runs.incrementAndGet.andThen(Absent)).on(Activation.Blur)).declare
                    _    <- n.underlying.onEvent(2.0, Activation.Change)
                    c    <- runs.get
                    _    <- n.underlying.onEvent(2.0, Activation.Blur)
                    b    <- runs.get
                yield (c, b)
        yield
            assert(typing == 0, s"a change ran it $typing times")
            assert(left == 1, s"the blur ran it $left times")
        end for
    }

    "an appended rule and the combinators carry the activation through" in {
        for
            (typing, left) <-
                for
                    added   <- AtomicInt.init(0)
                    wrapped <- AtomicInt.init(0)
                    form    <- mkForm
                    f       <- form.field("").declare
                    _       <- f.addRule(counted(added))
                    g       <- form.field(Maybe.empty[String]).rules(
                        Validator.ifPresent(Validator.when[String](_ => true)(counted(wrapped)).message("Checked on blur."))
                    ).declare
                    _  <- f.onEvent("x", Activation.Change)
                    _  <- g.onEvent(Present("x"), Activation.Change)
                    a0 <- added.get
                    w0 <- wrapped.get
                    _  <- f.onEvent("x", Activation.Blur)
                    _  <- g.onEvent(Present("x"), Activation.Blur)
                    a1 <- added.get
                    w1 <- wrapped.get
                yield ((a0, w0), (a1, w1))
        yield
            assert(typing == (0, 0), s"addRule, ifPresent, when and message kept the async default: $typing")
            assert(left == (1, 1), s"and the blur ran both: $left")
        end for
    }

    // ---- a dependency runs every rule ----------------------------------------------------------
    //
    // A field that `dependsOn` another re-checks when the other changes, and its own value has not
    // moved, so a rule not due for the event would answer the verdict it stored for that value: the
    // one about the OTHER field's old value. A dependency therefore runs the whole chain.

    /** Wait until the field's runs reached `n` and the run that made it `n` has published. `runs` counts a rule that
      * runs on every event, so it moves once per run of the chain, and `validating` drops after the publish.
      */
    private def settled(f: FormField[String], runs: AtomicInt, n: Int)(using Frame, kyo.test.AssertScope): Unit < Async =
        assertEventually(runs.get.map(_ == n).map(ok => if ok then f.validating.current.map(!_) else false))

    "a dependency that comes to match clears the error a field on(Blur) showed for the old value" in {
        for
            (shown, cleared, visible, touched, plainRuns) <-
                for
                    runs     <- AtomicInt.init(0)
                    password <- Signal.initRef("secret")
                    form     <- mkForm
                    confirm  <- form.field("").rules(countedPlain(runs).on(Activation.Blur, Activation.Change)).on(Activation.Blur).declare
                    _        <- confirm.matches(password)
                    out      <- Scope.run {
                        for
                            _  <- confirm.wireDependencies
                            _  <- assertEventually(password.waiters.map(_ == 1))
                            _  <- confirm.valueRef.set("secrex")
                            _  <- confirm.onEvent("secrex", Activation.Blur)
                            e0 <- confirm.visibleError.current
                            _  <- password.set("secrex")
                            _  <- assertEventually(quiet(confirm, Absent))
                            e1 <- confirm.error.current
                            v1 <- confirm.visibleError.current
                            t1 <- confirm.touched.current
                            n  <- runs.get
                        yield (e0.map(_.code), e1, v1, t1, n)
                    }
                yield out
        yield
            assert(shown == Present("must-match"), s"the blur judged the mismatch: $shown")
            assert(cleared == Absent, s"the passwords match now, so the error goes: $cleared")
            assert(visible == Absent, s"and nothing is shown: $visible")
            assert(touched, "the blur touched the field")
            assert(plainRuns == 1, s"the rule that does not read the password kept its verdict: $plainRuns runs")
        end for
    }

    "a dependency that stops matching shows the error of a rule on(Blur) at once" in {
        for
            (before, after) <-
                for
                    runs     <- AtomicInt.init(0)
                    password <- Signal.initRef("secret")
                    form     <- mkForm
                    confirm  <- form.field("").rules(countedPlain(runs)).declare
                    out      <- Scope.run {
                        for
                            _  <- confirm.addRule(Validator.matchesField(password).on(Activation.Blur))
                            _  <- confirm.dependsOn(password)
                            _  <- settled(confirm, runs, 1)
                            _  <- confirm.valueRef.set("secret")
                            _  <- confirm.onEvent("secret", Activation.Blur)
                            e0 <- confirm.visibleError.current
                            _  <- password.set("secret2")
                            _  <- settled(confirm, runs, 3)
                            e1 <- confirm.visibleError.current
                        yield (e0, e1.map(_.code))
                    }
                yield out
        yield
            assert(before == Absent, s"the blur found a match: $before")
            assert(after == Present("must-match"), s"the new password no longer matches, and the touched field says so: $after")
        end for
    }

    "a dependency judges an untouched field without touching it, and later events reuse that verdict" in {
        for
            (raw, visible, touched, reused, runsAfter) <-
                for
                    runs     <- AtomicInt.init(0)
                    password <- Signal.initRef("secret")
                    form     <- mkForm
                    confirm  <- form.field("").rules(countedPlain(runs).on(Activation.Change)).declare
                    out      <- Scope.run {
                        for
                            _  <- confirm.addRule(Validator.matchesField(password).on(Activation.Submit))
                            _  <- confirm.dependsOn(password)
                            _  <- settled(confirm, runs, 1)
                            _  <- password.set("other")
                            _  <- settled(confirm, runs, 2)
                            e0 <- confirm.error.current
                            v0 <- confirm.visibleError.current
                            t0 <- confirm.touched.current
                            _  <- confirm.revalidate // a change event over the same value: the rule on(Submit) is not due
                            e1 <- confirm.error.current
                            n  <- runs.get
                        yield (e0.map(_.code), v0, t0, e1.map(_.code), n)
                    }
                yield out
        yield
            assert(raw == Present("must-match"), s"the dependency ran the rule that waits for the submit: $raw")
            assert(visible == Absent, s"an untouched field shows nothing: $visible")
            assert(!touched, "a dependency does not touch the field")
            assert(reused == Present("must-match"), s"the next event reuses the verdict the dependency stored: $reused")
            assert(runsAfter == 3, s"three runs of the chain: $runsAfter")
        end for
    }

    // ---- rules that read another field ---------------------------------------------------------
    //
    // A rule built with `reading` names the signals it reads, and the field subscribes to them. A
    // change of one is an event for the field: a leaf that reads it with `Dep.recheck` runs, every
    // other leaf is not due, and the dependency values are part of the input a verdict is kept for.

    /** True once the field's last run has published and its raw error is `expected`. */
    private def quiet(f: FormField[String], expected: Maybe[FieldError])(using Frame): Boolean < Sync =
        f.error.current.map(e => if e == expected then f.validating.current.map(!_) else false)

    /** True once the field's last run has published a rule that is not yet checked. */
    private def unchecked(f: FormField[String])(using Frame): Boolean < Sync =
        f.notChecked.current.map(n => if n then f.validating.current.map(!_) else false)

    /** A rule reading the longest length a value may have; its error carries the limit it was given. */
    private def tooLong(s: String, n: Int): Maybe[FieldError] =
        if s.length > n then Present(FieldError("too-long", Map("max" -> n.toString))) else Absent

    /** [[tooLong]] as an async rule that counts its runs. */
    private def countedTooLong(runs: AtomicInt)(s: String, n: Int)(using Frame): Maybe[FieldError] < Async =
        runs.incrementAndGet.map(_ => tooLong(s, n))

    "a rule reading another field runs again when it changes, with the new value, and does not touch the field" in {
        for
            (first, wider, narrower, value, touched) <-
                for
                    maxLen <- Signal.initRef(5)
                    form   <- mkForm
                    f      <- form.field("abcdef").rules(Validator.reading(maxLen)(tooLong)).declare
                    out    <- Scope.run {
                        for
                            _  <- f.wireDependencies
                            _  <- assertEventually(maxLen.waiters.map(_ == 1))
                            _  <- f.revalidate
                            e0 <- f.error.current
                            _  <- maxLen.set(10)
                            _  <- assertEventually(quiet(f, Absent))
                            e1 <- f.error.current
                            _  <- maxLen.set(3)
                            _  <- assertEventually(quiet(f, Present(FieldError("too-long", Map("max" -> "3")))))
                            e2 <- f.error.current
                            v  <- f.value.current
                            t  <- f.touched.current
                        yield (e0, e1, e2, v, t)
                    }
                yield out
        yield
            assert(first == Present(FieldError("too-long", Map("max" -> "5"))), s"the mount judged against 5: $first")
            assert(wider == Absent, "a longer limit clears the error at once")
            assert(narrower == Present(FieldError("too-long", Map("max" -> "3"))), s"and the rule received the new limit: $narrower")
            assert(value == "abcdef", s"the field's own value never moved: $value")
            assert(!touched, "a dependency does not touch the field")
        end for
    }

    "an async reading rule drops its verdict when the dependency changes, and the submit runs it" in {
        for
            (runsAfterChange, errorAfterChange, ungated, gated, runsAfterSubmit, errorAfterSubmit, validAfterSubmit) <-
                for
                    runs     <- AtomicInt.init(0)
                    maxLen   <- Signal.initRef(5)
                    form     <- mkForm
                    f        <- form.field("abcdef").rules(Validator.reading(maxLen).async(countedTooLong(runs))).declare
                    ungatedV <- form.isValid()
                    gatedV   <- form.isValid(onlyVisible = true)
                    out      <- Scope.run {
                        for
                            _  <- f.wireDependencies
                            _  <- assertEventually(maxLen.waiters.map(_ == 1))
                            _  <- f.onEvent("abcdef", Activation.Blur)
                            _  <- maxLen.set(10)
                            _  <- assertEventually(unchecked(f))
                            r0 <- runs.get
                            e0 <- f.error.current
                            u0 <- ungatedV.current
                            g0 <- gatedV.current
                            _  <- form.submit(Kyo.unit)
                            r1 <- runs.get
                            e1 <- f.error.current
                            u1 <- ungatedV.current
                        yield (r0, e0, u0, g0, r1, e1, u1)
                    }
                yield out
        yield
            assert(runsAfterChange == 1, s"only the blur ran it, not the dependency: $runsAfterChange")
            assert(errorAfterChange == Absent, s"the verdict about the old limit is gone: $errorAfterChange")
            assert(!ungated, "the rule has not judged the new limit, so the form is not valid")
            assert(gated, "and nothing is shown, so the gated view is")
            assert(runsAfterSubmit == 2, s"the submit ran it: $runsAfterSubmit")
            assert(errorAfterSubmit == Absent, s"with the current limit, which the value fits: $errorAfterSubmit")
            assert(validAfterSubmit, "and the form is valid again")
        end for
    }

    "a dependency changed back to the value a verdict was kept for reuses it without a run" in {
        for
            (runs, restored) <-
                for
                    runs   <- AtomicInt.init(0)
                    maxLen <- Signal.initRef(5)
                    form   <- mkForm
                    f      <- form.field("abcdef").rules(Validator.reading(maxLen).async(countedTooLong(runs))).declare
                    out    <- Scope.run {
                        for
                            _ <- f.wireDependencies
                            _ <- assertEventually(maxLen.waiters.map(_ == 1))
                            _ <- f.onEvent("abcdef", Activation.Blur)
                            _ <- maxLen.set(10)
                            _ <- assertEventually(unchecked(f))
                            _ <- maxLen.set(5)
                            _ <- assertEventually(quiet(f, Present(FieldError("too-long", Map("max" -> "5")))))
                            n <- runs.get
                            e <- f.error.current
                        yield (n, e)
                    }
                yield out
        yield
            assert(runs == 1, s"the rule ran once, at the blur: $runs")
            assert(restored == Present(FieldError("too-long", Map("max" -> "5"))), s"the kept verdict is back: $restored")
        end for
    }

    "each dependency of one rule reacts its own way" in {
        for
            (afterDiscard, uncheckedAfterDiscard, afterRecheck, error) <-
                for
                    runs <- AtomicInt.init(0)
                    min  <- Signal.initRef(1)
                    max  <- Signal.initRef(5)
                    form <- mkForm
                    rule = Validator.reading(Dep.discard(min), Dep.recheck(max)).async((s: String, d: (Int, Int)) =>
                        runs.incrementAndGet.map(_ =>
                            if s.length < d._1 || s.length > d._2 then Present(FieldError("length", Map("range" -> s"${d._1}..${d._2}")))
                            else Absent
                        )
                    )
                    f   <- form.field("abc").rules(rule).declare
                    out <- Scope.run {
                        for
                            _  <- f.wireDependencies
                            _  <- assertEventually(min.waiters.map(_ == 1).map(a => if a then max.waiters.map(_ == 1) else false))
                            _  <- f.onEvent("abc", Activation.Blur)
                            _  <- min.set(4)
                            _  <- assertEventually(unchecked(f))
                            r0 <- runs.get
                            c0 <- f.notChecked.current
                            _  <- max.set(6)
                            _  <- assertEventually(quiet(f, Present(FieldError("length", Map("range" -> "4..6")))))
                            r1 <- runs.get
                            e  <- f.error.current
                        yield (r0, c0, r1, e)
                    }
                yield out
        yield
            assert(afterDiscard == 1, s"the discarded dependency ran nothing: $afterDiscard")
            assert(uncheckedAfterDiscard, "and left the rule not yet checked")
            assert(afterRecheck == 2, s"the rechecked one ran it: $afterRecheck")
            assert(error == Present(FieldError("length", Map("range" -> "4..6"))), s"with both current values: $error")
        end for
    }

    "two rules reading the same signal each react their own way" in {
        for
            (rechecked, discarded, unchecked0, blockedRecheck) <-
                for
                    rechecks  <- AtomicInt.init(0)
                    discards  <- AtomicInt.init(0)
                    rechecks2 <- AtomicInt.init(0)
                    maxLen    <- Signal.initRef(5)
                    form      <- mkForm
                    f         <- form.field("abc").rules(
                        Validator.reading(Dep.recheck(maxLen)).async(countedTooLong(rechecks)) and
                            Validator.reading(Dep.discard(maxLen)).async(countedTooLong(discards))
                    ).declare
                    // The same two rules the other way round: the discarded one stops the chain first.
                    g <- form.field("abc").rules(
                        Validator.reading(Dep.discard(maxLen)).async((s: String, n: Int) => tooLong(s, n)) and
                            Validator.reading(Dep.recheck(maxLen)).async(countedTooLong(rechecks2))
                    ).declare
                    out <- Scope.run {
                        for
                            _  <- f.wireDependencies
                            _  <- g.wireDependencies
                            _  <- assertEventually(maxLen.waiters.map(_ == 2))
                            _  <- f.onEvent("abc", Activation.Blur)
                            _  <- g.onEvent("abc", Activation.Blur)
                            _  <- maxLen.set(10)
                            _  <- assertEventually(rechecks.get.map(_ == 2).map(a => if a then unchecked(f) else false))
                            _  <- assertEventually(unchecked(g))
                            r  <- rechecks.get
                            d  <- discards.get
                            c  <- f.notChecked.current
                            r2 <- rechecks2.get
                        yield (r, d, c, r2)
                    }
                yield out
        yield
            assert(rechecked == 2, s"the rule that rechecks ran on the change: $rechecked")
            assert(discarded == 1, s"the one that discards did not: $discarded")
            assert(unchecked0, "and the field is not yet checked, by the second rule")
            assert(blockedRecheck == 1, s"after a rule not yet checked, a rechecking rule does not run: $blockedRecheck")
        end for
    }

    "a reading rule's on governs only the field's events, and its dependency still rechecks it" in {
        for
            (typing, left, dependency, typingAgain) <-
                for
                    runs   <- AtomicInt.init(0)
                    maxLen <- Signal.initRef(5)
                    form   <- mkForm
                    f      <- form.field("").rules(Validator.reading(Dep.recheck(maxLen)).async(countedTooLong(runs)).on(Activation.Blur))
                        .on(Activation.Change).declare
                    out <- Scope.run {
                        for
                            _  <- f.wireDependencies
                            _  <- assertEventually(maxLen.waiters.map(_ == 1))
                            _  <- f.valueRef.set("ab")
                            _  <- f.onEvent("ab", Activation.Change)
                            t0 <- runs.get
                            _  <- f.onEvent("ab", Activation.Blur)
                            l  <- runs.get
                            _  <- maxLen.set(1)
                            _  <- assertEventually(quiet(f, Present(FieldError("too-long", Map("max" -> "1")))))
                            d  <- runs.get
                            _  <- f.valueRef.set("abc")
                            _  <- f.onEvent("abc", Activation.Change)
                            t1 <- runs.get
                        yield (t0, l, d, t1)
                    }
                yield out
        yield
            assert(typing == 0, s"the field's Change does not run a rule held to the blur: $typing")
            assert(left == 1, s"the blur does: $left")
            assert(dependency == 2, s"a change of what it reads does too, whatever its on: $dependency")
            assert(typingAgain == 2, s"and typing still does not: $typingAgain")
        end for
    }

    "a rule that does not read the dependency keeps its verdict when it changes" in {
        for
            (plain, reading) <-
                for
                    plainRuns   <- AtomicInt.init(0)
                    readingRuns <- AtomicInt.init(0)
                    maxLen      <- Signal.initRef(5)
                    form        <- mkForm
                    f           <- form.field("abc").rules(
                        countedPlain(plainRuns) and Validator.reading(Dep.recheck(maxLen)).async(countedTooLong(readingRuns))
                    ).declare
                    out <- Scope.run {
                        for
                            _ <- f.wireDependencies
                            _ <- assertEventually(maxLen.waiters.map(_ == 1))
                            _ <- f.onEvent("abc", Activation.Blur)
                            _ <- maxLen.set(2)
                            _ <- assertEventually(quiet(f, Present(FieldError("too-long", Map("max" -> "2")))))
                            p <- plainRuns.get
                            r <- readingRuns.get
                        yield (p, r)
                    }
                yield out
        yield
            assert(plain == 1, s"the rule before it kept the verdict it found at the blur: $plain")
            assert(reading == 2, s"the reading rule ran again: $reading")
        end for
    }

    "matches carries its dependency: a field on(Blur) follows the other field at once, both ways" in {
        for
            (shown, cleared, reshown, plainRuns) <-
                for
                    runs     <- AtomicInt.init(0)
                    password <- Signal.initRef("secret")
                    form     <- mkForm
                    confirm  <- form.field("").rules(countedPlain(runs)).on(Activation.Blur).declare
                    _        <- confirm.matches(password)
                    out      <- Scope.run {
                        for
                            _  <- confirm.wireDependencies
                            _  <- assertEventually(password.waiters.map(_ == 1))
                            _  <- confirm.valueRef.set("secrex")
                            _  <- confirm.onEvent("secrex", Activation.Blur)
                            e0 <- confirm.visibleError.current
                            _  <- password.set("secrex")
                            _  <- assertEventually(quiet(confirm, Absent))
                            e1 <- confirm.visibleError.current
                            _  <- password.set("secret")
                            _  <- assertEventually(quiet(confirm, Present(FieldError("must-match"))))
                            e2 <- confirm.visibleError.current
                            n  <- runs.get
                        yield (e0.map(_.code), e1, e2.map(_.code), n)
                    }
                yield out
        yield
            assert(shown == Present("must-match"), s"the blur judged the mismatch: $shown")
            assert(cleared == Absent, s"the password came to match, and the error went at once: $cleared")
            assert(reshown == Present("must-match"), s"it stopped matching, and the touched field says so at once: $reshown")
            assert(plainRuns == 1, s"neither change ran the field's other rule: $plainRuns")
        end for
    }

    "a reading rule's subscription ends with the mount, and a row's with the row" in {
        for
            (mounted, unmounted, rowWired, rowError, rowRemoved) <-
                for
                    maxLen   <- Signal.initRef(5)
                    rowMax   <- Signal.initRef(5)
                    captured <- AtomicRef.init(Chunk.empty[FormField[String]])
                    form     <- mkForm
                    _        <- form.field("abc").rules(Validator.reading(maxLen)(tooLong)).declare
                    m        <- Scope.run {
                        form.wireChangeActivations.andThen(assertEventually(maxLen.waiters.map(_ == 1))).andThen(maxLen.waiters)
                    }
                    _   <- assertEventually(maxLen.waiters.map(_ == 0))
                    u   <- maxLen.waiters
                    arr <- form.fieldArray(0) { (row, _) =>
                        for
                            fld <- row.field("abc").rules(Validator.reading(rowMax)(tooLong)).declare
                            _   <- captured.updateAndGet(_ :+ fld)
                        yield (fragment(): UI)
                    }
                    _    <- arr.add
                    flds <- captured.get
                    fld = flds.head
                    _  <- assertEventually(rowMax.waiters.map(_ == 1))
                    w  <- rowMax.waiters
                    _  <- rowMax.set(2)
                    _  <- assertEventually(quiet(fld, Present(FieldError("too-long", Map("max" -> "2")))))
                    re <- fld.error.current
                    _  <- arr.removeAt(0)
                    _  <- assertEventually(rowMax.waiters.map(_ == 0))
                    rr <- rowMax.waiters
                yield (m, u, w, re, rr)
        yield
            assert(mounted == 1, s"the mount subscribed to what the rule reads: $mounted")
            assert(unmounted == 0, s"and the unmount let it go: $unmounted")
            assert(rowWired == 1, s"a row subscribes too: $rowWired")
            assert(rowError == Present(FieldError("too-long", Map("max" -> "2"))), s"and its rule rechecks on the change: $rowError")
            assert(rowRemoved == 0, s"removing the row lets the signal go: $rowRemoved")
        end for
    }

    /** A rule reading the limit `maxLen` that rechecks on its change and runs on the field's every change and blur too,
      * counting its runs.
      */
    private def countedReading(runs: AtomicInt, maxLen: Signal[Int])(using Frame): Validator[String] =
        Validator.reading(Dep.recheck(maxLen)).async(countedTooLong(runs)).on(Activation.Change, Activation.Blur)

    "a reading rule appended after the mount follows its dependency, until the mount ends" in {
        for
            (onAdd, onChange, error, held, released) <-
                for
                    runs   <- AtomicInt.init(0)
                    maxLen <- Signal.initRef(5)
                    form   <- mkForm
                    f      <- form.field("abcd").declare
                    out    <- Scope.run {
                        for
                            _  <- form.wireChangeActivations
                            _  <- assertEventually(f.valueRef.waiters.map(_ == 1))
                            _  <- f.addRule(countedReading(runs, maxLen))
                            r0 <- runs.get
                            _  <- maxLen.set(3)
                            _  <- assertEventually(quiet(f, Present(FieldError("too-long", Map("max" -> "3")))))
                            r1 <- runs.get
                            e  <- f.error.current
                            w  <- maxLen.waiters
                        yield (r0, r1, e, w)
                    }
                    _ <- assertEventually(maxLen.waiters.map(_ == 0))
                    u <- maxLen.waiters
                yield (out._1, out._2, out._3, out._4, u)
        yield
            assert(onAdd == 0, s"appending the rule does not run it: $onAdd")
            assert(onChange == 1, s"a change of what it reads does: $onChange")
            assert(error == Present(FieldError("too-long", Map("max" -> "3"))), s"with the new limit: $error")
            assert(held == 1, s"the field subscribed to it once: $held")
            assert(released == 0, s"and the unmount let it go: $released")
        end for
    }

    "a reading rule appended to a row after it was added follows its dependency, until the row is removed" in {
        for
            (onChange, error, held, released) <-
                for
                    runs     <- AtomicInt.init(0)
                    rowMax   <- Signal.initRef(5)
                    captured <- AtomicRef.init(Chunk.empty[FormField[String]])
                    form     <- mkForm
                    arr      <- form.fieldArray(0) { (row, _) =>
                        for
                            fld <- row.field("abcd").declare
                            _   <- captured.updateAndGet(_ :+ fld)
                        yield (fragment(): UI)
                    }
                    _    <- arr.add
                    flds <- captured.get
                    fld = flds.head
                    _  <- assertEventually(fld.valueRef.waiters.map(_ == 1))
                    _  <- fld.addRule(countedReading(runs, rowMax))
                    _  <- rowMax.set(2)
                    _  <- assertEventually(quiet(fld, Present(FieldError("too-long", Map("max" -> "2")))))
                    r  <- runs.get
                    e  <- fld.error.current
                    w  <- rowMax.waiters
                    _  <- arr.removeAt(0)
                    _  <- assertEventually(rowMax.waiters.map(_ == 0))
                    rr <- rowMax.waiters
                yield (r, e, w, rr)
        yield
            assert(onChange == 1, s"the row's rule ran on the change: $onChange")
            assert(error == Present(FieldError("too-long", Map("max" -> "2"))), s"with the new limit: $error")
            assert(held == 1, s"the row subscribed to it once: $held")
            assert(released == 0, s"removing the row lets the signal go: $released")
        end for
    }

    "a rule reading its own field's value runs once per edit" in {
        for
            (atMount, afterEdit, error) <-
                for
                    runs <- AtomicInt.init(0)
                    form <- mkForm
                    f    <- form.field("ab").declare
                    _    <- f.addRule(
                        Validator.reading(Dep.recheck(f.value)).async((s: String, own: String) =>
                            runs.incrementAndGet.map(_ => if own.length > 2 then Present(FieldError("too-long")) else Absent)
                        ).on(Activation.Change, Activation.Blur)
                    )
                    out <- Scope.run {
                        for
                            _  <- form.wireChangeActivations
                            _  <- assertEventually(f.valueRef.waiters.map(_ >= 1).map(a => if a then quiet(f, Absent) else false))
                            r0 <- runs.get
                            _  <- f.valueRef.set("abc")
                            _  <- assertEventually(runs.get.map(_ >= 2).map(a =>
                                if a then quiet(f, Present(FieldError("too-long"))) else false
                            ))
                            _  <- assertEventually(f.valueRef.waiters.map(_ == 1))
                            r1 <- runs.get
                            e  <- f.error.current
                        yield (r0, r1, e)
                    }
                yield out
        yield
            assert(atMount == 1, s"the mount ran it once: $atMount")
            assert(afterEdit == 2, s"and the edit once more, not a second time as a change of what it reads: $afterEdit")
            assert(error == Present(FieldError("too-long")), s"judging the new value: $error")
        end for
    }

    "a dependency event over what the rule already judged does not run it again" in {
        for
            (deliveredLate, deliveredStale, error, sameValue) <-
                for
                    runs   <- AtomicInt.init(0)
                    maxLen <- Signal.initRef(5)
                    form   <- mkForm
                    f      <- form.field("abcdef").rules(Validator.reading(Dep.recheck(maxLen)).async(countedTooLong(runs))).declare
                    _      <- f.onEvent("abcdef", Activation.Blur)
                    _      <- maxLen.set(4)
                    _      <- f.onEvent("abcdef", Activation.Blur)
                    // The observer delivers the 4 the blur has already judged, as it does when its fiber runs after the blur.
                    _  <- f.dependencyChanged(maxLen)
                    r0 <- runs.get
                    // A delivery that is behind the signal: it went on to 7 and back to the 4 the rule judged before the
                    // delivery of the 7 arrived.
                    _  <- maxLen.set(7)
                    _  <- maxLen.set(4)
                    _  <- f.dependencyChanged(maxLen)
                    r1 <- runs.get
                    e  <- f.error.current
                    r2 <- Scope.run {
                        for
                            _ <- f.wireDependencies
                            _ <- assertEventually(maxLen.waiters.map(_ == 1))
                            _ <- maxLen.set(4)
                            _ <- assertEventually(quiet(f, Present(FieldError("too-long", Map("max" -> "4")))))
                            n <- runs.get
                        yield n
                    }
                yield (r0, r1, e, r2)
        yield
            assert(deliveredLate == 2, s"the two blurs ran it, the late delivery did not: $deliveredLate")
            assert(deliveredStale == 2, s"nor did a delivery of a value the signal no longer holds: $deliveredStale")
            assert(error == Present(FieldError("too-long", Map("max" -> "4"))), s"the verdict about the current limit stays: $error")
            assert(sameValue == 2, s"subscribing to it and setting it to the same value ran nothing either: $sameValue")
        end for
    }

    "a dependency that moves between the mount's run and the subscription reaches the rule" in {
        for
            (atMount, subscribed) <-
                for
                    maxLen <- Signal.initRef(5)
                    form   <- mkForm
                    f      <- form.field("abcdef").rules(Validator.reading(maxLen)(tooLong)).declare
                    _      <- f.revalidate   // the mount's own run, which reads 5
                    e0     <- f.error.current
                    _      <- maxLen.set(10) // before the field observes it
                    e1     <- Scope.run {
                        for
                            _ <- f.wireDependencies
                            _ <- assertEventually(maxLen.waiters.map(_ == 1))
                            e <- f.error.current
                        yield e
                    }
                yield (e0, e1)
        yield
            assert(atMount == Present(FieldError("too-long", Map("max" -> "5"))), s"the mount judged against 5: $atMount")
            assert(subscribed == Absent, s"the value at subscription is a change, and the rule judged 10: $subscribed")
        end for
    }

    "a mount whose dependency does not move runs each rule as often as without one" in {
        for
            (plain, rechecks, onBlur, pending) <-
                for
                    plainRuns <- AtomicInt.init(0)
                    readRuns  <- AtomicInt.init(0)
                    blurRuns  <- AtomicInt.init(0)
                    maxLen    <- Signal.initRef(5)
                    form      <- mkForm
                    f         <- form.field("abc").rules(
                        countedPlain(plainRuns) and countedReading(readRuns, maxLen) and
                            Validator.reading(Dep.recheck(maxLen)).async(countedTooLong(blurRuns))
                    ).declare
                    out <- Scope.run {
                        for
                            _ <- form.wireChangeActivations
                            _ <- assertEventually(maxLen.waiters.map(_ == 1))
                            // The change observer ran the mount and waits on the ref again; without this the dependency's run
                            // alone can publish not yet checked before the mount's run has started.
                            _ <- assertEventually(f.valueRef.waiters.map(_ == 1))
                            _ <- assertEventually(unchecked(f))
                            p <- plainRuns.get
                            r <- readRuns.get
                            b <- blurRuns.get
                            c <- f.notChecked.current
                        yield (p, r, b, c)
                    }
                yield out
        yield
            assert(plain == 1, s"the mount ran the plain rule once: $plain")
            assert(rechecks == 1, s"and the reading rule due on a change once: $rechecks")
            assert(onBlur == 0, s"the reading rule held to the blur not at all: $onBlur")
            assert(pending, "so it is not yet checked")
        end for
    }

    "a dependency a field event read first still rechecks a rule that event was not due for, when it reaches the field" in {
        for
            (blurred, typed, delivered, pending) <-
                for
                    other <- Signal.initRef("secret")
                    form  <- mkForm
                    f     <- form.field("secret").rules(Validator.matchesField(other).on(Activation.Blur)).declare
                    _     <- f.onEvent("secret", Activation.Blur)
                    e0    <- f.error.current
                    _     <- other.set("public")
                    // A keystroke the rule is not due for runs before the field hears of the change, and reads the new value.
                    _   <- f.valueRef.set("secrets")
                    _   <- f.onEvent("secrets", Activation.Change)
                    n0  <- f.notChecked.current
                    out <- Scope.run {
                        for
                            _ <- f.wireDependencies
                            _ <- assertEventually(other.waiters.map(_ == 1))
                            e <- f.error.current
                            n <- f.notChecked.current
                        yield (e, n)
                    }
                yield (e0, n0, out._1, out._2)
        yield
            assert(blurred == Absent, s"the blur found a match: $blurred")
            assert(typed, "the keystroke left the rule held to the blur not yet checked")
            assert(delivered == Present(FieldError("must-match")), s"the change of the other field rechecked the rule: $delivered")
            assert(!pending, "so it is checked again")
        end for
    }

    "a late delivery of a dependency a field event already read still rechecks a rule that event was not due for" in {
        for
            (typed, delivered, pending) <-
                for
                    other <- Signal.initRef("secret")
                    form  <- mkForm
                    f     <- form.field("secret").rules(Validator.matchesField(other).on(Activation.Blur)).declare
                    _     <- f.onEvent("secret", Activation.Blur)
                    _     <- other.set("public")
                    _     <- f.valueRef.set("public")
                    _     <- f.onEvent("public", Activation.Change)
                    n0    <- f.notChecked.current
                    // What the observer of `other` delivers once its fiber runs, after the keystroke.
                    _ <- f.dependencyChanged(other)
                    e <- f.error.current
                    n <- f.notChecked.current
                yield (n0, e, n)
        yield
            assert(typed, "the keystroke left the rule held to the blur not yet checked")
            assert(delivered == Absent, s"the delivery rechecked the rule, and the two fields match: $delivered")
            assert(!pending, "so it is checked again")
        end for
    }

    // ---- what validating shows ----------------------------------------------------------------
    //
    // `validating` and the debounce belong to a run that runs a rule. An event no rule is due for
    // only recalls verdicts, so it publishes at once and leaves both alone.

    /** Runs `edits` on a fiber and waits until it has finished or `validating` has turned true, whichever comes first,
      * collecting every value `validating` took meanwhile. `onChange` on a ref runs inside the write, so no value is
      * missed. The edits are forked because a debounced run sleeps on the controlled clock, which nothing advances here.
      */
    private def validatingDuring(f: FormField[String])(edits: Unit < Async)(using
        Frame,
        kyo.test.AssertScope
    ): Chunk[Boolean] < (Async & Scope) =
        for
            seen <- AtomicRef.init(Chunk.empty[Boolean])
            _    <- f.validating.onChange(v => seen.updateAndGet(_ :+ v).unit)
            fib  <- Fiber.init(edits)
            _    <- assertEventually(seen.get.map(_.contains(true)).map(t => if t then true else fib.done))
            all  <- seen.get
        yield all

    "a field whose rules wait for the submit shows no validating while the reader types, and the old error goes at once" in {
        Clock.withTimeControl { _ =>
            for
                (submitted, first, afterFirst, second, afterSecond) <-
                    for
                        form <- mkForm
                        f    <- form.field("").rules(Validator.required().on(Activation.Submit)).debounce(300.millis).declare
                        _    <- f.validateForSubmit
                        e0   <- f.error.current
                        out  <- Scope.run {
                            for
                                s1 <- validatingDuring(f)(f.valueRef.set("a").andThen(f.onEvent("a", Activation.Change)))
                                e1 <- f.error.current
                                s2 <- validatingDuring(f)(f.valueRef.set("ab").andThen(f.onEvent("ab", Activation.Change)))
                                e2 <- f.error.current
                            yield (s1, e1, s2, e2)
                        }
                    yield (e0.map(_.code), out._1, out._2, out._3, out._4)
            yield
                assert(submitted == Present("required"), s"the submit judged the empty value: $submitted")
                assert(!first.contains(true), s"no rule is due for a change, so nothing validates: $first")
                assert(afterFirst == Absent, s"the error about the empty value went with the first edit: $afterFirst")
                assert(!second.contains(true), s"nor for the next one: $second")
                assert(afterSecond == Absent, s"and nothing came back: $afterSecond")
            end for
        }
    }

    "a field on(Blur) shows no validating while the reader types, and the blur's error goes at once" in {
        Clock.withTimeControl { clock =>
            for
                (blurred, typing, edited) <-
                    for
                        form <- mkForm
                        f    <- form.field("").rules(Validator.required()).on(Activation.Blur).debounce(300.millis).declare
                        out  <- Scope.run {
                            for
                                blur <- Fiber.init(f.onEvent("", Activation.Blur))
                                _    <- clock.awaitPendingSleeper(300.millis)
                                _    <- clock.advance(300.millis)
                                _    <- blur.get
                                e0   <- f.error.current
                                s    <- validatingDuring(f)(f.valueRef.set("a").andThen(f.onEvent("a", Activation.Change)))
                                e1   <- f.error.current
                            yield (e0.map(_.code), s, e1)
                        }
                    yield out
            yield
                assert(blurred == Present("required"), s"the blur judged the empty value: $blurred")
                assert(!typing.contains(true), s"no rule is due for a change, so nothing validates: $typing")
                assert(edited == Absent, s"the error about the empty value went with the edit: $edited")
            end for
        }
    }

    "a field with a rule due on every change still shows validating while its debounce runs" in {
        Clock.withTimeControl { clock =>
            for
                (during, after, error) <-
                    for
                        form <- mkForm
                        f    <- form.field("x").rules(Validator.required()).debounce(300.millis).declare
                        out  <- Scope.run {
                            for
                                _    <- f.valueRef.set("")
                                edit <- Fiber.init(f.onEvent("", Activation.Change))
                                _    <- clock.awaitPendingSleeper(300.millis)
                                v0   <- f.validating.current
                                _    <- clock.advance(300.millis)
                                _    <- edit.get
                                v1   <- f.validating.current
                                e    <- f.error.current
                            yield (v0, v1, e.map(_.code))
                        }
                    yield out
            yield
                assert(during, "the rule is due for the change, so validating shows through the debounce")
                assert(!after, "and goes once the run has published")
                assert(error == Present("required"), s"what the run found: $error")
            end for
        }
    }

    "a blur no rule is due for, during the debounce of a change's run, leaves that run to publish" in {
        Clock.withTimeControl { clock =>
            for
                (blurred, error, after) <-
                    for
                        form <- mkForm
                        f    <- form.field("x").rules(Validator.required().on(Activation.Change)).debounce(300.millis).declare
                        out  <- Scope.run {
                            for
                                _    <- f.valueRef.set("")
                                edit <- Fiber.init(f.onEvent("", Activation.Change))
                                _    <- clock.awaitPendingSleeper(300.millis)
                                _    <- f.onEvent("", Activation.Blur)
                                v0   <- f.validating.current
                                _    <- clock.advance(300.millis)
                                _    <- edit.get
                                e    <- f.error.current
                                v1   <- f.validating.current
                            yield (v0, e.map(_.code), v1)
                        }
                    yield out
            yield
                assert(blurred, "the change's run is still pending after the blur, so validating stays")
                assert(error == Present("required"), s"the change's run published what it found: $error")
                assert(!after, "and validating went with it")
            end for
        }
    }

    "a submit during a change's debounce ends validating, and the run it overtook publishes nothing over it" in {
        Clock.withTimeControl { clock =>
            for
                (submitted, during, error, after) <-
                    for
                        form <- mkForm
                        f    <- form.field("x").rules(Validator.required()).debounce(300.millis).declare
                        out  <- Scope.run {
                            for
                                _    <- f.valueRef.set("")
                                edit <- Fiber.init(f.onEvent("", Activation.Change))
                                _    <- clock.awaitPendingSleeper(300.millis)
                                _    <- f.valueRef.set("y")
                                s    <- f.validateForSubmit
                                v0   <- f.validating.current
                                _    <- clock.advance(300.millis)
                                _    <- edit.get
                                e    <- f.error.current
                                v1   <- f.validating.current
                            yield (s, v0, e, v1)
                        }
                    yield out
            yield
                assert(submitted == Absent, s"the submit judged the value it read: $submitted")
                assert(!during, "the submit is the field's last run, so validating went with it")
                assert(error == Absent, s"the change's run, about the empty value, did not publish over the submit: $error")
                assert(!after, "and validating stays off")
            end for
        }
    }

    "a keystroke no rule is due for drops the verdict of a run still in flight for the old value" in {
        for
            (typed, error, pending, after) <-
                for
                    started <- Latch.init(1)
                    release <- Latch.init(1)
                    form    <- mkForm
                    f       <- form.field("").rules(Validator.async[String](v =>
                        started.release.andThen(release.await).andThen(
                            (if v == "taken" then Present(FieldError("taken")) else Absent): Maybe[FieldError]
                        )
                    )).declare
                    out <- Scope.run {
                        for
                            _    <- f.valueRef.set("taken")
                            blur <- Fiber.init(f.onEvent("taken", Activation.Blur))
                            _    <- started.await
                            _    <- f.valueRef.set("taken!")
                            _    <- f.onEvent("taken!", Activation.Change)
                            n0   <- f.notChecked.current
                            _    <- release.release
                            _    <- blur.get
                            e    <- f.error.current
                            n    <- f.notChecked.current
                            v    <- f.validating.current
                        yield (n0, e, n, v)
                    }
                yield out
        yield
            assert(typed, "the rule held to the blur has not judged the new value")
            assert(error == Absent, s"the verdict about the old value is not shown: $error")
            assert(pending, "the rule is still not checked for the new value")
            assert(!after, "and validating went with the run")
        end for
    }

    /** An async rule that records every value it was asked about, and fails on `bad`. */
    private def recording(received: AtomicRef[Chunk[String]], bad: String)(using Frame): Validator[String] =
        Validator.async[String](v =>
            received.updateAndGet(_ :+ v).map(_ => (if v == bad then Present(FieldError(bad)) else Absent): Maybe[FieldError])
        )

    "keystrokes faster than the debounce run a rule due on the change once, on the last value" in {
        Clock.withTimeControl { clock =>
            for
                (received, error, after) <-
                    for
                        received <- AtomicRef.init(Chunk.empty[String])
                        form     <- mkForm
                        f        <- form.field("").rules(recording(received, "abc").on(Activation.Change)).debounce(300.millis).declare
                        out      <- Scope.run {
                            for
                                k1 <- Fiber.init(f.valueRef.set("a").andThen(f.onEvent("a", Activation.Change)))
                                _  <- clock.awaitPendingSleepers(1)
                                k2 <- Fiber.init(f.valueRef.set("ab").andThen(f.onEvent("ab", Activation.Change)))
                                _  <- clock.awaitPendingSleepers(2)
                                k3 <- Fiber.init(f.valueRef.set("abc").andThen(f.onEvent("abc", Activation.Change)))
                                _  <- clock.awaitPendingSleepers(3)
                                _  <- clock.advance(300.millis)
                                _  <- k1.get
                                _  <- k2.get
                                _  <- k3.get
                                r  <- received.get
                                e  <- f.error.current
                                v  <- f.validating.current
                            yield (r, e.map(_.code), v)
                        }
                    yield out
            yield
                assert(received == Chunk("abc"), s"only the last keystroke's run asked the rule, about the last value: $received")
                assert(error == Present("abc"), s"and published what it found: $error")
                assert(!after, "validating went with it")
            end for
        }
    }

    "a submit during a blur's debounce keeps that run from asking its rule about the old value" in {
        Clock.withTimeControl { clock =>
            for
                (submitted, received, error, after) <-
                    for
                        received <- AtomicRef.init(Chunk.empty[String])
                        form     <- mkForm
                        f        <- form.field("").rules(recording(received, "a")).debounce(300.millis).declare
                        out      <- Scope.run {
                            for
                                _    <- f.valueRef.set("a")
                                blur <- Fiber.init(f.onEvent("a", Activation.Blur))
                                _    <- clock.awaitPendingSleeper(300.millis)
                                _    <- f.valueRef.set("ab")
                                s    <- f.validateForSubmit
                                _    <- clock.advance(300.millis)
                                _    <- blur.get
                                r    <- received.get
                                e    <- f.error.current
                                v    <- f.validating.current
                            yield (s, r, e, v)
                        }
                    yield out
            yield
                assert(submitted == Absent, s"the submit judged the value it read: $submitted")
                assert(received == Chunk("ab"), s"the blur's run, overtaken by the submit, sent no request for the old value: $received")
                assert(error == Absent, s"the submit's verdict stands: $error")
                assert(!after, "and validating stays off")
            end for
        }
    }

    "a run overtaken between claiming the field and showing validating leaves validating off once both have ended" in {
        for
            (between, after, error) <-
                for
                    claimed <- Latch.init(1)
                    resume  <- Latch.init(1)
                    form    <- mkForm
                    f       <- form.field("").rules(Validator.minLength(2)).declare
                    out     <- Scope.run {
                        for
                            _     <- f.valueRef.set("a")
                            first <- FormField.afterClaim.let(claimed.release.andThen(resume.await))(
                                Fiber.init(f.onEvent("a", Activation.Change))
                            )
                            _  <- claimed.await
                            _  <- f.valueRef.set("ab")
                            _  <- f.onEvent("ab", Activation.Change)
                            v0 <- f.validating.current
                            _  <- resume.release
                            _  <- first.get
                            v1 <- f.validating.current
                            e  <- f.error.current
                        yield (v0, v1, e)
                    }
                yield out
        yield
            assert(!between, "the later run ended, and validating with it")
            assert(!after, "the overtaken run, resumed, shows no validating for a field no run is judging")
            assert(error == Absent, s"the later run's verdict stands: $error")
        end for
    }

    "a run on the value the field holds that an edit overtakes as it starts does not publish the old value's verdict" in {
        for
            (edited, after) <-
                for
                    paused <- Latch.init(1)
                    resume <- Latch.init(1)
                    form   <- mkForm
                    f      <- form.field("").rules(Validator.required() and Validator.minLength(3)).declare
                    out    <- Scope.run {
                        for
                            first <- FormField.startingRun.let(paused.release.andThen(resume.await))(Fiber.init(f.revalidate))
                            _     <- paused.await
                            _     <- f.valueRef.set("ok")
                            _     <- f.onEvent("ok", Activation.Change) // the edit, as the change observer delivers it
                            e0    <- f.error.current
                            _     <- resume.release
                            _     <- first.get
                            e1    <- f.error.current
                        yield (e0.map(_.code), e1.map(_.code))
                    }
                yield out
        yield
            assert(edited == Present("min-length"), s"the edit judged the new value: $edited")
            assert(after == Present("min-length"), s"and the run that started before it did not publish over it: $after")
        end for
    }

    "a submit an edit overtakes as it starts judges the value the field holds, and hands that value over" in {
        for
            (submitted, error, valid) <-
                for
                    paused <- Latch.init(1)
                    resume <- Latch.init(1)
                    form   <- mkForm
                    f      <- form.field("").rules(Validator.required() and Validator.minLength(3)).declare
                    out    <- Scope.run {
                        for
                            submit <- FormField.startingRun.let(paused.release.andThen(resume.await))(Fiber.init(f.verdict))
                            _      <- paused.await
                            _      <- f.valueRef.set("okay")
                            _      <- f.onEvent("okay", Activation.Change) // the edit, as the change observer delivers it
                            _      <- resume.release
                            s      <- submit.get
                            e      <- f.error.current
                            ok     <- form.isValid()
                            v      <- ok.current
                        yield ((s.failure.map(_.code), s.value), e.map(_.code), v)
                    }
                yield out
        yield
            assert(submitted == (Absent, Present("okay")), s"the submit judged, and hands over, the value the field holds: $submitted")
            assert(error == Absent, s"no verdict about the old value is shown: $error")
            assert(valid, "and the form agrees that the field's value is valid")
        end for
    }

    "a bound blur an edit overtakes as it starts judges the value the field holds" in {
        for
            (touched, error, valid) <-
                for
                    paused <- Latch.init(1)
                    resume <- Latch.init(1)
                    form   <- mkForm
                    f      <- form.field("").rules(Validator.required() and Validator.minLength(3)).declare
                    ui = uic.TextArea().bind(f).render
                    area <- elementWithClass(ui, "p-textarea")
                    blur = area.attrs.onBlur.getOrElse(throw new AssertionError("bind wires no blur trigger"))
                    out <- Scope.run {
                        for
                            left <- FormField.startingRun.let(paused.release.andThen(resume.await))(Fiber.init(handled(blur)))
                            _    <- paused.await
                            _    <- f.valueRef.set("okay")
                            _    <- f.onEvent("okay", Activation.Change) // the edit, as the change observer delivers it
                            _    <- resume.release
                            _    <- left.get
                            t    <- f.touched.current
                            e    <- f.error.current
                            ok   <- form.isValid()
                            v    <- ok.current
                        yield (t, e.map(_.code), v)
                    }
                yield out
        yield
            assert(touched, "the blur marks the field touched")
            assert(error == Absent, s"no verdict about the value the blur started on is shown: $error")
            assert(valid, "and the form agrees that the field's value is valid")
        end for
    }

    // ---- what the gated view counts ------------------------------------------------------------

    "a rule not yet checked keeps isValid() false and leaves isValid(onlyVisible = true) and the submit button alone" in {
        for
            (gatedTyping, ungatedTyping, disabledTyping, ungatedAfterSubmit) <-
                for
                    form     <- mkForm
                    f        <- form.field("").rules(Validator.required().on(Activation.Submit)).declare
                    ungated  <- form.isValid()
                    gated    <- form.isValid(onlyVisible = true)
                    disabled <- form.submitDisabled
                    _        <- f.valueRef.set("x")
                    _        <- f.onEvent("x", Activation.Change)
                    g0       <- gated.current
                    u0       <- ungated.current
                    d0       <- disabled.signal.current
                    _        <- form.submit(Kyo.unit)
                    u1       <- ungated.current
                yield (g0, u0, d0, u1)
        yield
            assert(gatedTyping, "nothing is shown, so the gated view is valid")
            assert(!ungatedTyping, "the rule has not judged the value, so the ungated view is not")
            assert(!disabledTyping, "and the submit that would run the rule stays open")
            assert(ungatedAfterSubmit, "the submit judged the value valid")
        end for
    }

    "a field-array row's unchecked rule counts the same way in both views" in {
        for
            (gatedTyping, ungatedTyping, ungatedAfterSubmit) <-
                for
                    captured <- AtomicRef.init(Chunk.empty[FormField[String]])
                    form     <- mkForm
                    arr      <- form.fieldArray(0) { (row, _) =>
                        for
                            fld <- row.field("").rules(Validator.required().on(Activation.Submit)).declare
                            _   <- captured.updateAndGet(_ :+ fld)
                        yield (fragment(): UI)
                    }
                    ungated <- form.isValid()
                    gated   <- form.isValid(onlyVisible = true)
                    _       <- arr.add
                    flds    <- captured.get
                    fld = flds.head
                    // The row's change observer checks the mount value, then waits on the ref again.
                    _  <- assertEventually(fld.valueRef.waiters.map(_ == 1))
                    _  <- fld.valueRef.set("x")
                    _  <- assertEventually(fld.touched.current)
                    g0 <- gated.current
                    u0 <- ungated.current
                    _  <- form.submit(Kyo.unit)
                    u1 <- ungated.current
                    _  <- arr.removeAt(0)
                yield (g0, u0, u1)
        yield
            assert(gatedTyping, "a row's unchecked rule shows nothing, so the gated view is valid")
            assert(!ungatedTyping, "and the ungated view is not")
            assert(ungatedAfterSubmit, "until the submit judged it")
        end for
    }

end FormValidationTest
