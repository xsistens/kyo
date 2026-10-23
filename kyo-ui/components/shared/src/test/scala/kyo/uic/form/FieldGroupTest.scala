package kyo.uic.form

import java.time.LocalDate
import kyo.*
import kyo.UI.*
import kyo.uic
import kyo.uic.DateKind.given
import kyo.uic.UicTest

/** The typed field group: `form.fields((name = spec, ...))` declares every spec at once and hands back the named tuple of
  * declared handles, `form.submit(f)` hands the handler one named tuple of validated values, and `Form.scopeOf` finds a
  * group's scope. The order form below is the reference example, written exactly as an app writes it.
  */
class FieldGroupTest extends UicTest:

    private inline val preamble =
        "import java.time.LocalDate\nimport kyo.*\nimport kyo.UI.*\nimport kyo.uic\nimport kyo.uic.DateKind.given\nimport kyo.uic.form.*\n"

    /** What the order form's handler is called with. */
    private type Order = (String, LocalDate, Chunk[(String, Int)], (String, String))

    /** A form as `Form.mounted` builds one, minus the mount, whose focus command appends to `focused`. */
    private def mkForm(focused: SignalRef[Chunk[String]])(using Frame): Form < Sync =
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
            focusInvalidOnSubmit = true,
            () => counter.incrementAndGet.map(n => s"f$n"),
            id => focused.updateAndGet(_ :+ id).unit,
            ownsSubmit = true
        )

    private def freshForm(using Frame): Form < Sync = Signal.initRef(Chunk.empty[String]).map(mkForm)

    /** The reference example. `order` is called with the group's members by name, which is itself the check that each
      * one arrives as its validated type.
      */
    private def orderForm(
        form: Form,
        order: (String, LocalDate, Chunk[(sku: String, qty: Int)], (street: String, city: String)) => UI.Action
    )(using Frame) =
        for
            f <- form.fields((
                customer = form.field.rules(Validator.required()),
                due = form.field(Maybe.empty[LocalDate]).rules(Validator.chosen()),
                lines = form.array(initial = 1)(row =>
                    (
                        sku = row.field.rules(Validator.required()),
                        qty = row.numberField(1).rules(Validator.min(1))
                    )
                ),
                shipping = form.child(scope =>
                    (
                        street = scope.field.rules(Validator.required()),
                        city = scope.field.rules(Validator.required())
                    )
                )
            ))
        yield (
            f,
            div(
                uic.Input().bind(f.customer),
                uic.DatePicker[LocalDate]().bind(f.due),
                f.lines.render((line, row) =>
                    div(uic.Input().bind(line.sku), uic.InputNumber().bind(line.qty), uic.Button("Remove").onClick(row.remove))
                ),
                uic.Input().bind(f.shipping.street),
                uic.Input().bind(f.shipping.city),
                uic.Button("Order").onClick(form.submit(f)(v => order(v.customer, v.due, v.lines, v.shipping)))
            )
        )

    /** The order form with a handler that records what it was called with. */
    private def recordingForm(using Frame) =
        for
            focused <- Signal.initRef(Chunk.empty[String])
            form    <- mkForm(focused)
            sink    <- AtomicRef.init(Maybe.empty[Order])
            built   <- orderForm(
                form,
                (c, d, ls, s) => sink.set(Present((c, d, ls.map(l => (l.sku, l.qty)), (s.street, s.city))))
            )
        yield (form, built._1, built._2, sink, focused)

    private val day = LocalDate.of(2026, 11, 2)

    // ---- types ---------------------------------------------------------------------------------

    "submit's value is the named tuple of the validated types, nested for arrays and children" in {
        typeCheck(
            preamble +
                """
def check[T](t: T)(using T =:= (customer: String, due: LocalDate, lines: Chunk[(sku: String, qty: Int)], shipping: (street: String, city: String))): Unit = ()
def x(form: Form)(using Frame) =
  form.fields((
    customer = form.field.rules(Validator.required()),
    due      = form.field(Maybe.empty[LocalDate]).rules(Validator.chosen()),
    lines    = form.array(initial = 1)(row => (sku = row.field.rules(Validator.required()), qty = row.numberField(1).rules(Validator.min(1)))),
    shipping = form.child(scope => (street = scope.field.rules(Validator.required()), city = scope.field.rules(Validator.required())))
  )).map(f => form.submit(f)(v => { check(v); Kyo.unit }))"""
        )
    }

    "a member reads as the handle its spec declares, refined to what its rules yield" in {
        typeCheck(
            preamble + """
def x(form: Form)(using Frame) =
  form.fields((
    due   = form.field(Maybe.empty[LocalDate]).rules(Validator.chosen()),
    qty   = form.numberField(1).rules(Validator.refine[Int, Long]("odd")(n => if n % 2 == 1 then Present(n.toLong) else Absent)),
    lines = form.array()(row => (sku = row.field)),
    ship  = form.child(scope => (city = scope.field))
  )).map { f =>
    val due: FormField[Maybe[LocalDate]] { type Valid = LocalDate } = f.due
    val qty: NumberField[Int] { type Valid = Long }                 = f.qty
    val lines: GroupArray[Tuple1["sku"], Tuple1[FormField[String] { type Valid = String }]] = f.lines
    val city: FormField[String] { type Valid = String }             = f.ship.city
    val scope: Form                                                 = Form.scopeOf(f.ship)
    val top: Form                                                   = Form.scopeOf(f)
    f.lines.render((line, row) => { val sku: FormField[String] = line.sku; val at: Signal[Int] = row.index; div() })
  }"""
        )
    }

    // ---- names: a group has nothing but its fields ----------------------------------------------

    "fields named like every operation a group, an array or a row has read the field, by name and in the value" in {
        for
            form <- freshForm
            f    <- form.fields((
                submit = form.field("s"),
                scope = form.field("sc"),
                add = form.field("a"),
                rows = form.field("r"),
                render = form.field("rd"),
                map = form.field("m"),
                size = form.numberField(3),
                lines = form.array(initial = 1)(row =>
                    (
                        remove = row.field("rm"),
                        index = row.numberField(7),
                        moveUp = row.field("u"),
                        moveDown = row.field("d")
                    )
                ),
                shipping = form.child(scope => (scope = scope.field("in"), submit = scope.field("too")))
            ))
            // Read by name: each is the field, typed as the field, not a method of something else.
            submitF: FormField[String] { type Valid = String }    = f.submit
            scopeF: FormField[String] { type Valid = String }     = f.scope
            addF: FormField[String] { type Valid = String }       = f.add
            rowsF: FormField[String] { type Valid = String }      = f.rows
            renderF: FormField[String] { type Valid = String }    = f.render
            mapF: FormField[String] { type Valid = String }       = f.map
            sizeF: NumberField[Int] { type Valid = Int }          = f.size
            childScope: FormField[String] { type Valid = String } = f.shipping.scope
            rows <- f.lines.currentRows
            removeF: FormField[String] { type Valid = String } = rows.head.remove
            indexF: NumberField[Int] { type Valid = Int }      = rows.head.index
            values <- Kyo.foreach(Chunk(submitF, scopeF, addF, rowsF, renderF, mapF, childScope, removeF))(_.value.current)
            sink <- AtomicRef.init(Maybe.empty[(String, String, String, String, String, String, Int, String, Int, String, String, String)])
            _    <- form.submit(f)(v =>
                sink.set(Present((
                    v.submit,
                    v.scope,
                    v.add,
                    v.rows,
                    v.render,
                    v.map,
                    v.size,
                    v.lines.head.remove,
                    v.lines.head.index,
                    v.lines.head.moveUp,
                    v.shipping.scope,
                    v.shipping.submit
                )))
            )
            got <- sink.get
        yield
            assert(values == Chunk("s", "sc", "a", "r", "rd", "m", "in", "rm"))
            assert(got == Present(("s", "sc", "a", "r", "rd", "m", 3, "rm", 7, "u", "in", "too")), s"the handler got: $got")
        end for
    }

    "a name the named tuple answers itself is refused at the declaration" in {
        typeCheckFailure(
            preamble + """
def x(form: Form)(using Frame) = form.fields((name = form.field, toString = form.field))"""
        )("no implicit values were found that match type scala.util.NotGiven[(\"toString\" : String) <:< kyo.uic.form.FieldGroup.Reserved]")
        typeCheckFailure(
            preamble + """
def x(form: Form)(using Frame) = form.fields((lines = form.array()(row => (hashCode = row.field))))"""
        )("no member may be named toString, hashCode, equals, getClass, isInstanceOf, asInstanceOf, apply, ==, != or ##")
    }

    "a group has no submit of its own, so the old spelling does not compile" in {
        typeCheckFailure(
            preamble + """
def x(form: Form)(using Frame) = form.fields((customer = form.field)).map(f => f.submit(v => Kyo.unit))"""
        )("value submit is not a member of (customer : kyo.uic.form.FormField[String]{type Valid = String})")
    }

    // ---- what the compiler says when it is wrong ------------------------------------------------

    "a mistyped name in the handler is reported against the value's named tuple type" in {
        typeCheckFailure(
            preamble + """
def x(form: Form)(using Frame) =
  form.fields((customer = form.field, qty = form.numberField(1))).map(f => form.submit(f)(v => { v.custmer; Kyo.unit }))"""
        )("value custmer is not a member of (customer : String, qty : Int)")
    }

    "a mistyped name on the group is reported against the group's named tuple type" in {
        typeCheckFailure(
            preamble + """
def x(form: Form)(using Frame) =
  form.fields((customer = form.field, qty = form.numberField(1))).map(f => f.custmer)"""
        )(
            "value custmer is not a member of (customer : kyo.uic.form.FormField[String]{type Valid = String}, qty :\n  kyo.uic.form.NumberField[Int]{type Valid = Int})"
        )
    }

    "a field bound to a control of another value type does not compile" in {
        typeCheckFailure(
            preamble + """
def x(form: Form)(using Frame) =
  form.fields((due = form.field(Maybe.empty[LocalDate]).rules(Validator.chosen()))).map(f => uic.Input().bind(f.due))"""
        )(
            "Found:    kyo.uic.form.FormField[kyo.Maybe[java.time.LocalDate]]{\n  type Valid = java.time.LocalDate}\nRequired: kyo.uic.form.FormField[String]"
        )
    }

    "a declared field in the tuple is refused with the group's own message" in {
        typeCheckFailure(
            preamble + """
def x(form: Form)(using Frame) =
  form.field.declare.map(name => form.fields((name = name)))"""
        )("Cannot declare a field group from these specs. Every member must be an UNDECLARED spec")
    }

    // ---- behaviour ------------------------------------------------------------------------------

    "the example renders every member bound by its own id, and a valid submit hands over the exact values" in {
        for
            (_, f, ui, sink, _) <- recordingForm
            _                   <- f.customer.valueRef.set("ACME")
            _                   <- f.due.valueRef.set(Present(day))
            rows                <- f.lines.currentRows
            _                   <- rows.head.sku.valueRef.set("A-1")
            _                   <- rows.head.qty.underlying.valueRef.set(2.0)
            _                   <- f.shipping.street.valueRef.set("Main 1")
            _                   <- f.shipping.city.valueRef.set("Berlin")
            ids                 <- Kyo.foreach(Chunk(f.customer.domId, rows.head.sku.domId, f.shipping.city.domId))(elementWithId(ui, _))
            button              <- elementsWithClass(ui, "p-button").map(_.last)
            _                   <- click(button)
            got                 <- sink.get
        yield
            assert(ids.length == 3, "the top field, the row field and the child field are bound by their ids")
            assert(got == Present(("ACME", day, Chunk(("A-1", 2)), ("Main 1", "Berlin"))), s"the handler got: $got")
        end for
    }

    "an invalid top field blocks the handler, shows its error and takes focus" in {
        for
            (form, f, _, sink, focused) <- recordingForm
            _                           <- fill(f)
            _                           <- f.customer.valueRef.set("")
            _                           <- form.submit(f)(_ => Kyo.unit)
            got                         <- sink.get
            shown                       <- f.customer.visibleError.current
            went                        <- focused.get
        yield
            assert(got.isEmpty, "the handler did not run")
            assert(shown.map(_.code) == Present("required"), s"the field shows why: $shown")
            assert(went == Chunk(f.customer.domId), s"focus went to it: $went")
        end for
    }

    "an invalid field in a row blocks the handler, shows its error and takes focus" in {
        for
            (form, f, _, _, focused) <- recordingForm
            _                        <- fill(f)
            _                        <- f.lines.add
            rows                     <- f.lines.currentRows
            _                        <- rows(1).qty.underlying.valueRef.set(0.0) // below min(1); its sku is empty too
            ran                      <- AtomicBoolean.init(false)
            _                        <- form.submit(f)(_ => ran.set(true))
            r                        <- ran.get
            sku                      <- rows(1).sku.visibleError.current
            qty                      <- rows(1).qty.visibleError.current
            went                     <- focused.get
        yield
            assert(!r, "the handler did not run")
            assert(sku.map(_.code) == Present("required"), s"the empty sku shows why: $sku")
            assert(qty.map(_.code) == Present("min"), s"the zero quantity shows why: $qty")
            assert(went == Chunk(rows(1).sku.domId), s"focus went to the first invalid row field: $went")
        end for
    }

    "an invalid field in the child blocks the handler and shows its error in the child's own summary" in {
        for
            (form, f, _, _, _) <- recordingForm
            _                  <- fill(f)
            _                  <- f.shipping.city.valueRef.set("")
            ran                <- AtomicBoolean.init(false)
            _                  <- form.submit(f)(_ => ran.set(true))
            r                  <- ran.get
            shown              <- f.shipping.city.visibleError.current
            childSig           <- Form.scopeOf(f.shipping).allErrors
            child              <- childSig.current
            topSig             <- Form.scopeOf(f).allErrors
            top                <- topSig.current
        yield
            assert(!r, "the handler did not run")
            assert(shown.map(_.code) == Present("required"), s"the city shows why: $shown")
            assert(child.map(_.code) == Chunk("required"), s"the child scope reports it: $child")
            assert(top.map(_.code) == Chunk("required"), s"and so does the form around it: $top")
        end for
    }

    "scopeOf names the form for the group, the child scope for a child, and a row's own scope for a row" in {
        for
            (form, f, _, _, _) <- recordingForm
            rows               <- f.lines.currentRows
            child = Form.scopeOf(f.shipping)
            row   = Form.scopeOf(rows.head)
        yield
            assert(Form.scopeOf(f) eq form)
            assert(!(child eq form) && !(row eq form) && !(row eq child))
            assert(child.outer eq form, "the child scope was made in the form")
            assert(row.outer eq form, "and so was the row's")
        end for
    }

    "a member spec made from another form is declared in the scope of the group that holds it" in {
        for
            form <- freshForm
            // `form.field` inside the child builder: the group decides where the field lives, not the spec.
            f        <- form.fields((name = form.field("n"), ship = form.child(_ => (city = form.field.rules(Validator.required())))))
            _        <- form.submit(f)(_ => Kyo.unit)
            childSig <- Form.scopeOf(f.ship).allErrors
            child    <- childSig.current
        yield assert(child.map(_.code) == Chunk("required"), s"the child scope holds the field: $child")
        end for
    }

    "a group of another form submitted from this one opens its fields' gates, so the failure shows" in {
        for
            other <- freshForm
            form  <- freshForm
            f     <- other.fields((customer = other.field.rules(Validator.required()), note = other.field("x")))
            ran   <- AtomicBoolean.init(false)
            _     <- form.submit(f)(_ => ran.set(true))
            r     <- ran.get
            shown <- f.customer.visibleError.current
            count <- other.submitCount.current
        yield
            assert(!r, "the handler did not run")
            assert(shown.map(_.code) == Present("required"), s"the field shows why: $shown")
            assert(count == 0, "its own form was never submitted")
        end for
    }

    "rows added, removed and moved are what the submit hands over, in their current order" in {
        for
            (form, f, _, sink, _) <- recordingForm
            _                     <- fill(f)
            _                     <- f.lines.add
            _                     <- f.lines.add
            rows                  <- f.lines.currentRows
            _                     <- Kyo.foreach(rows.zip(Chunk("a", "b", "c")))((r, s) => r.sku.valueRef.set(s))
            _                     <- f.lines.removeAt(1)
            _                     <- f.lines.move(1, 0)
            _                     <- form.submit(f)(v => sink.set(Present(("", day, v.lines.map(l => (l.sku, l.qty)), ("", "")))))
            got                   <- sink.get
        yield assert(got.map(_._3) == Present(Chunk(("c", 1), ("a", 1))), s"b removed, c moved first: $got")
        end for
    }

    "a row's own control, handed over beside it by render, removes that row" in {
        for
            (form, f, ui, sink, _) <- recordingForm
            _                      <- fill(f)
            _                      <- f.lines.add
            rows                   <- f.lines.currentRows
            _                      <- rows(1).sku.valueRef.set("B-2")
            buttons                <- elementsWithClass(ui, "p-button")
            // One Remove per row, then Order: the first Remove belongs to the first row.
            _   <- click(buttons.head)
            now <- f.lines.currentRows
            _   <- form.submit(f)(v => sink.set(Present(("", day, v.lines.map(l => (l.sku, l.qty)), ("", "")))))
            got <- sink.get
        yield
            assert(buttons.length == 3, s"two rows, two Remove buttons and Order: ${buttons.length}")
            assert(now.length == 1)
            assert(got.map(_._3) == Present(Chunk(("B-2", 1))), s"the first row removed itself: $got")
        end for
    }

    "an array hands over the rows it held when the submit started" in {
        for
            form    <- freshForm
            entered <- Latch.init(1)
            release <- Latch.init(1)
            slow = Validator.async[String](_ => entered.release.andThen(release.await).andThen(Absent))
            f <- form.fields((
                name = form.field("n").rules(slow).on(Activation.Submit),
                lines = form.array(initial = 1)(row => (sku = row.field("x")))
            ))
            seen  <- AtomicInt.init(-1)
            fiber <- Fiber.init(form.submit(f)(v => seen.set(v.lines.length)))
            _     <- entered.await
            _     <- f.lines.add // lands while the name's rule is in flight
            _     <- release.release
            _     <- fiber.get
            got   <- seen.get
            now   <- f.lines.currentRows
        yield
            assert(now.length == 2, "the row was added")
            assert(got == 1, s"but the submit validated and handed over the row it started with: $got")
        end for
    }

    "each field's rules run once per submit, at the top, in every row and in the child" in {
        for
            (ran, top, row, kid) <-
                for
                    form <- freshForm
                    topN <- AtomicInt.init(0)
                    rowN <- AtomicInt.init(0)
                    kidN <- AtomicInt.init(0)
                    count = (n: AtomicInt) => Validator.async[String](_ => n.incrementAndGet.andThen(Absent))
                    // Submit-only, so the silent first validation a row's `Activation.Change` observer runs at `add`
                    // does not count: only what the submit runs is measured.
                    f <- form.fields((
                        name = form.field("x").rules(count(topN)).on(Activation.Submit),
                        lines = form.array(initial = 2)(row => (sku = row.field("y").rules(count(rowN)).on(Activation.Submit))),
                        ship = form.child(scope => (city = scope.field("z").rules(count(kidN)).on(Activation.Submit)))
                    ))
                    ran <- AtomicBoolean.init(false)
                    _   <- form.submit(f)(_ => ran.set(true))
                    r   <- ran.get
                    t   <- topN.get
                    rw  <- rowN.get
                    k   <- kidN.get
                yield (r, t, rw, k)
        yield
            assert(ran, "everything held, so the handler ran")
            assert(top == 1, s"the top field ran once: $top")
            assert(row == 2, s"each of the two row fields ran once: $row")
            assert(kid == 1, s"the child field ran once: $kid")
        end for
    }

    /** Fills every field of the order form with a valid value. */
    private def fill(f: OrderGroup)(using Frame): Unit < Sync =
        for
            _    <- f.customer.valueRef.set("ACME")
            _    <- f.due.valueRef.set(Present(day))
            rows <- f.lines.currentRows
            _    <- Kyo.foreach(rows)(_.sku.valueRef.set("A-1"))
            _    <- f.shipping.street.valueRef.set("Main 1")
            _    <- f.shipping.city.valueRef.set("Berlin")
        yield ()

    private type OrderGroup = (
        customer: FormField[String] { type Valid = String },
        due: FormField[Maybe[LocalDate]] { type Valid = LocalDate },
        lines: GroupArray[
            ("sku", "qty"),
            (FormField[String] { type Valid = String }, NumberField[Int] { type Valid = Int })
        ],
        shipping: (street: FormField[String] { type Valid = String }, city: FormField[String] { type Valid = String })
    )

end FieldGroupTest
