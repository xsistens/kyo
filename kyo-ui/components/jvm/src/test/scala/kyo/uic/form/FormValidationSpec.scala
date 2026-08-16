package kyo.uic.form

import java.time.LocalDate
import java.time.ZoneId
import kyo.*
import kyo.UI.*
import kyo.uic.UicTest

// The date cases compare java.time values directly, which under strict equality needs an instance.
// The library itself does not: DateCodec carries the equality its typed field needs.
private given CanEqual[LocalDate, LocalDate] = CanEqual.derived
private given CanEqual[ZoneId, ZoneId]       = CanEqual.derived

/** Deterministic checks for the validation layer's error-lifecycle contract — the
  * behaviour that keeps a server verdict from deadlocking the submit button, plus the
  * typed-field path. Runs on the JVM (kyo effects discharged with
  * `KyoApp.Unsafe.runAndBlock`); it drives the signal graph directly (no DOM), which is
  * exactly where the value-scoping lives.
  */
class FormValidationSpec extends UicTest:
    import AllowUnsafe.embrace.danger

    private def run[A](v: A < (Async & Abort[Throwable])): A =
        KyoApp.Unsafe.runAndBlock(Duration.Infinity)(Abort.run[Throwable](v)) match
            case Result.Success(Result.Success(a)) => a
            case other                             => sys.error(s"effect failed: $other")

    /** A form built the way `Form.mounted` builds one, minus the `UI.mounted` wrapper —
      * so a test can hold the instance and read its signals.
      */
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
            () => counter.incrementAndGet.map(n => s"id$n"), // distinct ids (field domIds + array row keys)
            _ => Kyo.unit                                    // focus is a no-op off-DOM
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
            clientErr  <- Signal.initRef(Absent: Maybe[FieldError])
            serverErr  <- Signal.initRef(Absent: Maybe[ServerError[A]])
            epoch      <- AtomicInt.init(0)
            count      <- Signal.initRef(0)
        yield (
            new FormField[A](
                valueRef,
                initial,
                baseline,
                "test-field-id",
                touched,
                validating,
                clientErr,
                serverErr,
                epoch,
                rules,
                Set[Activation.Field](Activation.Submit),
                Duration.Zero,
                ErrorTranslator.default,
                count
            ),
            valueRef
        )

    /** A form whose id minting is a real counter (so fields get distinct ids) and whose
      * focus command appends the targeted id to `focused` — lets a test assert exactly
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
            id => focused.updateAndGet(_ :+ id).unit
        )

    "submit focuses the first invalid field in declaration order" in {
        val (focused, firstId) = run {
            for
                sink <- Signal.initRef(Chunk.empty[String])
                form <- mkFocusForm(sink)
                a    <- form.field(Validator.required(), Activation.Submit) // empty → invalid
                _    <- form.field(Validator.required(), Activation.Submit) // also invalid, but later
                _    <- form.submit(Kyo.unit)
                f    <- sink.get
            yield (f, a.domId)
        }
        assert(focused.headMaybe.contains(firstId), s"focus goes to the first invalid field; got $focused want $firstId")
    }

    "focusable(false) excludes a field, so focus lands on the next invalid one" in {
        val (focused, secondId) = run {
            for
                sink <- Signal.initRef(Chunk.empty[String])
                form <- mkFocusForm(sink)
                _    <- form.field(Validator.required(), Activation.Submit).map(_.focusable(false))
                b    <- form.field(Validator.required(), Activation.Submit)
                _    <- form.submit(Kyo.unit)
                f    <- sink.get
            yield (f, b.domId)
        }
        assert(focused.headMaybe.contains(secondId), s"focus skips focusable(false); got $focused want $secondId")
    }

    "focusInvalidOnSubmit = false suppresses focus entirely" in {
        val noFocus = run {
            for
                sink <- Signal.initRef(Chunk.empty[String])
                form <- mkFocusForm(sink, focus = false)
                _    <- form.field(Validator.required(), Activation.Submit)
                _    <- form.submit(Kyo.unit)
                f    <- sink.get
            yield f.isEmpty
        }
        assert(noFocus, "no focus command is emitted when the behaviour is disabled")
    }

    "dirty tracking: edit → dirty, markPristine → clean, reset returns to the rebaselined baseline" in {
        val (d0, d1, d2, d3, resetVal, d4) = run {
            for
                form <- mkForm
                f    <- form.field("a", Validator.all[String](), Activation.Submit)
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
        }
        assert(!d0, "clean at construction")
        assert(d1, "dirty after an edit")
        assert(!d2, "clean after markPristine")
        assert(d3, "dirty after editing past the new baseline")
        assert((resetVal == "b"), "reset returns to the rebaselined baseline, not the original initial")
        assert(!d4, "clean after reset")
    }

    "form.isDirty aggregates the subtree, including child scopes" in {
        val (clean, dirtyChild, cleanAfterPristine) = run {
            for
                form  <- mkForm
                _     <- form.field("x", Validator.all[String](), Activation.Submit)
                child <- form.child()
                cf    <- child.field("y", Validator.all[String](), Activation.Submit)
                sig   <- form.isDirty // read after fields + child are declared
                c0    <- sig.current  // false
                _     <- cf.valueRef.set("z")
                c1    <- sig.current  // true — a dirty child field bubbles up
                _     <- form.markPristine
                c2    <- sig.current  // false
            yield (c0, c1, c2)
        }
        assert(!clean, "a pristine form is not dirty")
        assert(dirtyChild, "a dirty child field makes the whole form dirty")
        assert(!cleanAfterPristine, "markPristine clears the whole subtree")
    }

    "field-array rows participate in submit validation and error aggregation" in {
        val (emptyErrs, filledErrs) = run {
            for
                f1 <- mkForm
                _  <- f1.fieldArray(2)((row, _) => row.field(Validator.required(), Activation.Submit).map(_ => (fragment(): UI)))
                s1 <- f1.allErrors
                _  <- f1.submit(Kyo.unit) // reveals: both empty required rows fail
                e1 <- s1.current

                f2 <- mkForm
                _  <- f2.fieldArray(2)((row, _) => row.field("ok", Validator.required(), Activation.Submit).map(_ => (fragment(): UI)))
                s2 <- f2.allErrors
                _  <- f2.submit(Kyo.unit) // both rows already satisfy required
                e2 <- s2.current
            yield (e1.length, e2.length)
        }
        assert((emptyErrs == 2), s"two empty required rows → 2 aggregated errors, got $emptyErrs")
        assert((filledErrs == 0), s"two filled rows → 0 errors, got $filledErrs")
    }

    "add / remove change the aggregation live; a dirty row makes the form dirty" in {
        val (d0, d1, d2, d3) = run {
            for
                captured <- AtomicRef.init(Chunk.empty[FormField[String]])
                form     <- mkForm
                arr <- form.fieldArray(0) { (row, _) =>
                    for
                        fld <- row.field("x", Validator.required(), Activation.Submit)
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
        }
        assert(!d0, "empty array → clean")
        assert(!d1, "a fresh row at its baseline → still clean")
        assert(d2, "editing a row field makes the form dirty (array folds into isDirty)")
        assert(!d3, "removing the dirty row makes the form clean again")
    }

    "move / moveDown reorder rows — row.index reflects the new positions" in {
        val (i0, i1, i0b, i1b) = run {
            for
                ctls <- AtomicRef.init(Chunk.empty[FieldArray.Row])
                form <- mkForm
                arr <- form.fieldArray(0) { (row, ctl) =>
                    ctls.updateAndGet(_ :+ ctl).andThen(row.field(Validator.all[String](), Activation.Submit).map(_ => (fragment(): UI)))
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
        }
        assert((i0 == 0 && i1 == 1), s"initial order (0,1), got ($i0,$i1)")
        assert((i0b == 1 && i1b == 0), s"after move(0,1) order (1,0), got ($i0b,$i1b)")
    }

    "fieldArrayOf: a cross-row check validates over all rows and flags duplicates per row (value-scoped)" in {
        val (uniqueClean, dupFormErr, dupRowErr, clearedOnEdit) = run {
            for
                form <- mkForm
                arr <- form.fieldArrayOf[FormField[String]](2) { (row, _) =>
                    row.field(Validator.all[String](), Activation.Submit).map(f => ((fragment(): UI), f))
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
        }
        assert(uniqueClean, "distinct rows → no cross-row error")
        assert(dupFormErr, "duplicate rows → form-level 'dup' raised and submit blocked")
        assert(dupRowErr, "the duplicate row is flagged inline via setError")
        assert(clearedOnEdit, "editing the flagged row clears its value-scoped error without re-running the check")
    }

    "built-in rules: pattern / url / numeric min-max / matchesField (empty passes where required owns it)" in {
        val checks = run {
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
                patF.isDefined,
                patO.isEmpty,
                patE.isEmpty,
                urlF.isDefined,
                urlO.isEmpty,
                minF.isDefined,
                minO.isEmpty,
                maxF.isDefined,
                maxO.isEmpty,
                matchF.isDefined,
                matchO.isEmpty
            )
        }
        assert(checks.forall(identity), s"every built-in rule case holds; got $checks")
    }

    "field.satisfy: post-hoc predicate rule (sugar over addRule) registers and validates" in {
        val (invalidWhenBad, validWhenGood) = run {
            for
                form <- mkForm
                f    <- form.field(Validator.all[String](), Activation.Submit)
                _ = f.satisfy("must-be-ok")(_ == "ok") // pure + chainable, mirrors form.satisfy
                _  <- f.valueRef.set("nope")
                e1 <- f.validateForSubmit
                _  <- f.valueRef.set("ok")
                e2 <- f.validateForSubmit
            yield (e1.exists(_.code == "must-be-ok"), e2.isEmpty)
        }
        assert(invalidWhenBad, "field.satisfy fails when the predicate is false")
        assert(validWhenGood, "passes when the predicate is true")
    }

    "isValid: ungated tracks raw field validity; onlyVisible gates on touched/submit-reveal" in {
        val (ungatedInvalid, visibleStillValid, ungatedValid, visibleValid) = run {
            for
                form    <- mkForm
                f       <- form.field(Validator.required(), Activation.Submit) // touched=false, submitCount=0
                ungated <- form.isValid()                                      // onlyVisible = false
                visible <- form.isValid(onlyVisible = true)
                _  <- f.validateForSubmit // records the required error, but the field stays untouched / unrevealed
                u1 <- ungated.current     // raw error present → invalid
                s1 <- visible.current     // error not DISPLAYED (untouched) → still "valid"
                _  <- f.valueRef.set("x")
                _  <- f.validateForSubmit // clears the error
                u2 <- ungated.current
                s2 <- visible.current
            yield (u1, s1, u2, s2)
        }
        assert(!ungatedInvalid, "ungated: a recorded-but-hidden required error makes isValid() false")
        assert(visibleStillValid, "onlyVisible: the same hidden error does NOT count (not displayed)")
        assert(ungatedValid, "ungated: fixing the field makes it valid")
        assert(visibleValid, "onlyVisible: valid too")
    }

    "Activation.Change: mount validates silently; a real change reveals inline and re-validates live" in {
        val (rawAtMount, hiddenAtMount, revealedAfterChange, clearedWhenValid) = run {
            for
                form <- mkForm
                f    <- form.field(Validator.required(), Activation.Change) // touched = false
                _    <- f.revalidate                                        // the observer's mount emission → validate SILENTLY
                e0   <- f.error.current                                     // raw error computed
                v0   <- f.visibleError.current                              // but NOT displayed (untouched, no submit)
                _    <- f.onEvent("")                                       // a real change (still empty) → touched + validate
                v1   <- f.visibleError.current                              // now revealed inline
                _    <- f.valueRef.set("x")
                _    <- f.onEvent("x")                                      // change to a valid value → re-validates, clears
                v2   <- f.visibleError.current
            yield (e0.isDefined, v0.isEmpty, v1.isDefined, v2.isEmpty)
        }
        assert(rawAtMount, "mount: raw error is computed")
        assert(hiddenAtMount, "mount: the error is NOT revealed (silent)")
        assert(revealedAfterChange, "a real change reveals the error inline (touched)")
        assert(clearedWhenValid, "typing a valid value clears it live")
    }

    "errorEntries: field errors carry their field id; form-level errors carry the scope anchor (else Absent)" in {
        val (fieldTagged, fieldIdMatches, formNoAnchor, formWithAnchor) = run {
            for
                form <- mkForm
                f    <- form.field(Validator.required(), Activation.Submit)
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
        }
        assert(fieldTagged, "a field error is present and field-tagged")
        assert(fieldIdMatches, "the field error carries its field's domId (for focus-jump)")
        assert(formNoAnchor, "a form-level error with no scope anchor carries Absent")
        assert(formWithAnchor, "after form.anchor(id), a form-level error carries that id")
    }

    "reactive invariant: feeds isValid live, and displays per Reveal mode" in {
        val (invalidWhenOff, validWhenMet, hiddenPreSubmit, shownPostSubmit, immediateShown) = run {
            for
                form <- mkForm
                a    <- form.field(0, Validator.all[Int](), Activation.Submit)
                b    <- form.field(0, Validator.all[Int](), Activation.Submit)
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
                x     <- form2.field(0, Validator.all[Int](), Activation.Submit)
                _ = form2.satisfy("x-pos", Reveal.Immediate)(x.value.map(_ > 0))
                es2 <- form2.errorEntries
                imm <- es2.current // Reveal.Immediate + failing → shown at once
            yield (v0, v1, pre.exists(_.error.code == "sum-10"), post.exists(_.error.code == "sum-10"), imm.exists(_.error.code == "x-pos"))
        }
        assert(!invalidWhenOff, "a failing invariant makes isValid false")
        assert(validWhenMet, "a holding invariant makes isValid true")
        assert(!hiddenPreSubmit, "Reveal.OnSubmit invariant is hidden before submit")
        assert(shownPostSubmit, "Reveal.OnSubmit invariant is shown after submit")
        assert(immediateShown, "Reveal.Immediate invariant is shown as soon as it fails")
    }

    "a server error is value-scoped: shown while the value is unchanged, hidden after an edit" in {
        val (shown, afterEdit) = run {
            for
                (f, valueRef) <- mkField("", Validator.all(), "bob")
                _             <- f.setError(Present(FieldError("username-taken")))
                ve1           <- f.visibleError.current
                _             <- valueRef.set("bobby")
                ve2           <- f.visibleError.current
            yield (ve1.isDefined, ve2.isDefined)
        }
        assert(shown, "server error is visible while the value matches")
        assert(!afterEdit, "server error disappears the moment the value changes")
    }

    "form-level raise errors do NOT disable submit (they cannot be fixed client-side)" in {
        val disabled = run {
            for
                form <- mkForm
                _    <- form.field(Validator.required(), Activation.Submit)
                _    <- form.raise(FieldError("invalid-credentials"))
                sig  <- form.submitDisabled
                d    <- sig.current
            yield d
        }
        assert(!disabled, "a raised form-level error must leave the submit button enabled")
    }

    "a displayed FIELD error still disables submit, then re-enables once the value changes" in {
        val (blocked, unblocked) = run {
            for
                (f, valueRef) <- mkField("", Validator.all(), "bob")
                _             <- f.setError(Present(FieldError("username-taken")))
                c1            <- f.visibleErrorChunk.current
                _             <- valueRef.set("bobby")
                c2            <- f.visibleErrorChunk.current
            yield (c1.nonEmpty, c2.nonEmpty)
        }
        assert(blocked, "field error contributes to the gate while shown")
        assert(!unblocked, "editing the field drops it from the gate")
    }

    "a typed Boolean field validates against its own type and value-scopes its server error" in {
        val (invalidWhenFalse, validWhenTrue, serverShown, serverHidden) = run {
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
        }
        assert(invalidWhenFalse, "false fails the accept rule")
        assert(!validWhenTrue, "true passes the accept rule")
        assert(serverShown, "server error visible while the Boolean is unchanged")
        assert(!serverHidden, "server error hides once the Boolean flips")
    }

    "a typed Double field (InputNumber) validates and value-scopes across a numeric edit" in {
        val (tooLow, okAtEdge, serverShown, serverHidden) = run {
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
        }
        assert(tooLow, "0.0 fails the minimum rule")
        assert(!okAtEdge, "21.0 passes the minimum rule")
        assert(serverShown, "server error visible while the number is unchanged")
        assert(!serverHidden, "server error hides once the number changes")
    }

    "a typed Set[String] field (MultiSelect) validates on selection and value-scopes on change" in {
        val (emptyFails, nonEmptyOk, serverShown, serverHidden) = run {
            for
                (f, valueRef) <- mkField(Set.empty[String], Validator.satisfy[Set[String]]("pick-one")(_.nonEmpty), Set.empty[String])
                r1            <- f.validateForSubmit                         // {} → nothing selected
                _             <- valueRef.set(Set("de"))
                r2            <- f.validateForSubmit                         // {de} → passes
                _             <- f.setError(Present(FieldError("server-x"))) // scoped to {de}
                s1            <- f.visibleError.current
                _             <- valueRef.set(Set("de", "en"))               // selection change diverges from the scope
                s2            <- f.visibleError.current
            yield (r1.isDefined, r2.isDefined, s1.isDefined, s2.isDefined)
        }
        assert(emptyFails, "empty selection fails the pick-one rule")
        assert(!nonEmptyOk, "a non-empty selection passes")
        assert(serverShown, "server error visible while the selection is unchanged")
        assert(!serverHidden, "server error hides once the selection changes")
    }

    "a typed Int numberField derives the value from the bound Double and rounds defensively" in {
        val (initial, tooLow, ok, rounded) = run {
            for
                form <- mkForm
                nf   <- form.numberField[Int](1, Validator.satisfy[Int]("min-3")(_ >= 3), Activation.Submit)
                v0   <- nf.value.current                // initial 1
                r0   <- nf.underlying.validateForSubmit // 1 < 3 → fails
                _    <- nf.underlying.valueRef.set(5.0)
                r1   <- nf.underlying.validateForSubmit // 5 ≥ 3 → passes
                _    <- nf.underlying.valueRef.set(3.5) // defensive: masking normally prevents this
                v1   <- nf.value.current
            yield (v0, r0.isDefined, r1.isEmpty, v1)
        }
        assert((initial == 1), "initial Double derives to Int 1")
        assert(tooLow, "1 fails the min-3 rule")
        assert(ok, "5 passes")
        assert((rounded == 4), "a stray 3.5 rounds to 4 (rint), never a fractional Int")
    }

    "a LocalDate dateField parses the ISO string and skips its rules while empty" in {
        import java.time.LocalDate
        val (emptyValue, emptyValid, parsed, pastFails, futureOk) = run {
            for
                form <- mkForm
                df <- form.dateField[LocalDate](
                    Absent,
                    Validator.satisfy[LocalDate]("after-2020")(_.isAfter(LocalDate.of(2020, 1, 1))),
                    Activation.Submit
                )
                v0 <- df.value.current                // empty → Absent
                r0 <- df.underlying.validateForSubmit // empty → the date rule is skipped
                _  <- df.underlying.valueRef.set("2019-06-01")
                v1 <- df.value.current                // parses to a typed LocalDate
                r1 <- df.underlying.validateForSubmit // 2019 fails after-2020
                _  <- df.underlying.valueRef.set("2021-06-01")
                r2 <- df.underlying.validateForSubmit // 2021 passes
            yield (v0.isEmpty, r0.isEmpty, v1.contains(LocalDate.of(2019, 6, 1)), r1.isDefined, r2.isEmpty)
        }
        assert(emptyValue, "empty picker → Absent value")
        assert(emptyValid, "empty picker → date rule skipped, valid")
        assert(parsed, "ISO string parses to the typed LocalDate")
        assert(pastFails, "2019 fails the after-2020 rule")
        assert(futureOk, "2021 passes")
    }

    "a ZonedDateTime dateField applies the codec's configured zone" in {
        import java.time.{LocalDate, ZoneId, ZonedDateTime}
        given DateCodec[ZonedDateTime] = DateCodec.zoned(ZoneId.of("Europe/Berlin"))
        val (rightZone, rightDay, midnight, roundTrips) = run {
            for
                form <- mkForm
                df   <- form.dateField[ZonedDateTime](Absent, Validator.all[ZonedDateTime](), Activation.Submit)
                _    <- df.underlying.valueRef.set("2026-08-01")
                v    <- df.value.current
                iso  <- df.underlying.valueRef.get
            yield (
                v.exists(_.getZone == ZoneId.of("Europe/Berlin")),
                v.exists(_.toLocalDate == LocalDate.of(2026, 8, 1)),
                v.exists(_.getHour == 0),
                iso == "2026-08-01"
            )
        }
        assert(rightZone, "value carries the configured zone")
        assert(rightDay, "value is the picked calendar day")
        assert(midnight, "value is at start-of-day (default LocalTime.MIDNIGHT)")
        assert(roundTrips, "the ISO source string is unchanged")
    }

end FormValidationSpec
