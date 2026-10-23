package kyo.uic.form

import java.time.LocalDate
import kyo.*
import kyo.UI.*
import kyo.uic
import kyo.uic.DateKind.given
import kyo.uic.UicTest

/** The typed field group: `form.fields((name = spec, ...))` declares every spec at once, reads the handles by name, and
  * `submit` hands the handler one named tuple of validated values. The order form below is the prototype's reference
  * example, written exactly as an app would write it.
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
                f.lines.render(line => div(uic.Input().bind(line.sku), uic.InputNumber().bind(line.qty))),
                uic.Input().bind(f.shipping.street),
                uic.Input().bind(f.shipping.city),
                uic.Button("Order").onClick(f.submit(v => order(v.customer, v.due, v.lines, v.shipping)))
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
        yield (built._1, built._2, sink, focused)

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
  )).map(f => f.submit(v => { check(v); Kyo.unit }))"""
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
    val city: FormField[String] { type Valid = String }             = f.ship.city
    val scope: Form                                                 = f.ship.scope
    f.lines.render(line => { val sku: FormField[String] = line.sku; div() })
  }"""
        )
    }

    // ---- what the compiler says when it is wrong ------------------------------------------------

    "a mistyped name in the handler is reported against the value's named tuple type" in {
        typeCheckFailure(
            preamble + """
def x(form: Form)(using Frame) =
  form.fields((customer = form.field, qty = form.numberField(1))).map(f => f.submit(v => { v.custmer; Kyo.unit }))"""
        )("value custmer is not a member of (customer : String, qty : Int)")
    }

    "a mistyped name on the group is reported against the group's type" in {
        typeCheckFailure(
            preamble + """
def x(form: Form)(using Frame) =
  form.fields((customer = form.field)).map(f => f.custmer)"""
        )("value custmer is not a member of kyo.uic.form.FieldGroup[Tuple1[(\"customer\" : String)]")
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
            (f, ui, sink, _) <- recordingForm
            _                <- f.customer.valueRef.set("ACME")
            _                <- f.due.valueRef.set(Present(day))
            rows             <- f.lines.currentRows
            _                <- rows.head.sku.valueRef.set("A-1")
            _                <- rows.head.qty.underlying.valueRef.set(2.0)
            _                <- f.shipping.street.valueRef.set("Main 1")
            _                <- f.shipping.city.valueRef.set("Berlin")
            ids              <- Kyo.foreach(Chunk(f.customer.domId, rows.head.sku.domId, f.shipping.city.domId))(elementWithId(ui, _))
            button           <- elementWithClass(ui, "p-button")
            _                <- click(button)
            got              <- sink.get
        yield
            assert(ids.length == 3, "the top field, the row field and the child field are bound by their ids")
            assert(got == Present(("ACME", day, Chunk(("A-1", 2)), ("Main 1", "Berlin"))), s"the handler got: $got")
        end for
    }

    "an invalid top field blocks the handler, shows its error and takes focus" in {
        for
            (f, _, sink, focused) <- recordingForm
            _                     <- fill(f)
            _                     <- f.customer.valueRef.set("")
            _                     <- f.submit(_ => Kyo.unit)
            got                   <- sink.get
            shown                 <- f.customer.visibleError.current
            went                  <- focused.get
        yield
            assert(got.isEmpty, "the handler did not run")
            assert(shown.map(_.code) == Present("required"), s"the field shows why: $shown")
            assert(went == Chunk(f.customer.domId), s"focus went to it: $went")
        end for
    }

    "an invalid field in a row blocks the handler, shows its error and takes focus" in {
        for
            (f, _, sink, focused) <- recordingForm
            _                     <- fill(f)
            _                     <- f.lines.add
            rows                  <- f.lines.currentRows
            _                     <- rows(1).qty.underlying.valueRef.set(0.0) // below min(1); its sku is empty too
            ran                   <- AtomicBoolean.init(false)
            _                     <- f.submit(_ => ran.set(true))
            r                     <- ran.get
            sku                   <- rows(1).sku.visibleError.current
            qty                   <- rows(1).qty.visibleError.current
            went                  <- focused.get
        yield
            assert(!r, "the handler did not run")
            assert(sku.map(_.code) == Present("required"), s"the empty sku shows why: $sku")
            assert(qty.map(_.code) == Present("min"), s"the zero quantity shows why: $qty")
            assert(went == Chunk(rows(1).sku.domId), s"focus went to the first invalid row field: $went")
        end for
    }

    "an invalid field in the child blocks the handler and shows its error in the child's own summary" in {
        for
            (f, _, sink, _) <- recordingForm
            _               <- fill(f)
            _               <- f.shipping.city.valueRef.set("")
            ran             <- AtomicBoolean.init(false)
            _               <- f.submit(_ => ran.set(true))
            r               <- ran.get
            shown           <- f.shipping.city.visibleError.current
            childSig        <- f.shipping.scope.allErrors
            child           <- childSig.current
        yield
            assert(!r, "the handler did not run")
            assert(shown.map(_.code) == Present("required"), s"the city shows why: $shown")
            assert(child.map(_.code) == Chunk("required"), s"the child scope reports it: $child")
        end for
    }

    "rows added, removed and moved are what the submit hands over, in their current order" in {
        for
            (f, _, sink, _) <- recordingForm
            _               <- fill(f)
            _               <- f.lines.add
            _               <- f.lines.add
            rows            <- f.lines.currentRows
            _               <- Kyo.foreach(rows.zip(Chunk("a", "b", "c")))((r, s) => r.sku.valueRef.set(s))
            _               <- f.lines.removeAt(1)
            _               <- f.lines.move(1, 0)
            _               <- f.submit(v => sink.set(Present(("", day, v.lines.map(l => (l.sku, l.qty)), ("", "")))))
            got             <- sink.get
            _               <- rows(0).remove
            _               <- f.submit(v => sink.set(Present(("", day, v.lines.map(l => (l.sku, l.qty)), ("", "")))))
            after           <- sink.get
        yield
            assert(got.map(_._3) == Present(Chunk(("c", 1), ("a", 1))), s"b removed, c moved first: $got")
            assert(after.map(_._3) == Present(Chunk(("c", 1))), s"a row removing itself drops out too: $after")
        end for
    }

    "each field's rules run once per submit, at the top, in every row and in the child" in {
        for
            (ran, top, row, kid) <-
                for
                    focused <- Signal.initRef(Chunk.empty[String])
                    form    <- mkForm(focused)
                    topN    <- AtomicInt.init(0)
                    rowN    <- AtomicInt.init(0)
                    kidN    <- AtomicInt.init(0)
                    count = (n: AtomicInt) => Validator.async[String](_ => n.incrementAndGet.andThen(Absent))
                    // Submit-only, so the silent first validation a row's `Activation.Change` observer runs at `add`
                    // does not count: only what the submit runs is measured.
                    f <- form.fields((
                        name = form.field("x").rules(count(topN)).on(Activation.Submit),
                        lines = form.array(initial = 2)(row => (sku = row.field("y").rules(count(rowN)).on(Activation.Submit))),
                        ship = form.child(scope => (city = scope.field("z").rules(count(kidN)).on(Activation.Submit)))
                    ))
                    ran <- AtomicBoolean.init(false)
                    _   <- f.submit(_ => ran.set(true))
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

    private type OrderGroup = FieldGroup[
        ("customer", "due", "lines", "shipping"),
        (
            FormField[String] { type Valid = String },
            FormField[Maybe[LocalDate]] { type Valid = LocalDate },
            GroupArray[
                ("sku", "qty"),
                (FormField[String] { type Valid = String }, NumberField[Int] { type Valid = Int }),
                (String, Int)
            ],
            FieldGroup[
                ("street", "city"),
                (FormField[String] { type Valid = String }, FormField[String] { type Valid = String }),
                (String, String)
            ]
        ),
        (
            String,
            LocalDate,
            Chunk[(sku: String, qty: Int)],
            (street: String, city: String)
        )
    ]

end FieldGroupTest
