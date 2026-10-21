package kyo.uic

import kyo.*

/** The dispatch behind every `A | Signal[A]` setter in the module.
  *
  * One setter per slot carries all three bindings a value slot can hold, and which one a call
  * lands on is decided HERE, on the runtime class of the argument. That makes this the place the
  * module's whole setter surface rests on: a `SignalRef` reaching the `Dyn` branch would silently
  * downgrade every editable control (Slider, Knob, Rating, InputNumber) from two-way to one-way,
  * with no compile error anywhere.
  */
class ReactiveValueTest extends UicTest:

    "constant binds Const" in {
        ReactiveValue(42) match
            case ReactiveValue.Const(v) => assert(v == 42)
            case other                  => fail(s"expected a constant binding, got $other")
    }

    "a writable SignalRef binds two-way" in {
        for ref <- Signal.initRef(1)
        yield ReactiveValue(ref) match
            case ReactiveVariable(r) => assert(r eq ref)
            case other               => fail(s"expected a two-way binding, got $other")
    }

    "any other Signal binds one-way" in {
        for
            ref <- Signal.initRef(1)
            sig = ref.map(_ * 2)
        yield ReactiveValue(sig) match
            case ReactiveVariable(_)  => fail("a derived signal must not bind two-way")
            case ReactiveValue.Dyn(s) => assert(s eq sig)
            case other                => fail(s"expected a one-way binding, got $other")
    }

    "readOnly is how a caller opts a ref out of write-back" in {
        // The choice is made on the runtime class, so ascribing `ref: Signal[Int]` would NOT opt
        // out: only handing over a different object does.
        for
            ref <- Signal.initRef(1)
            view = ref.readOnly
        yield
            val ascribed: Signal[Int] = ref
            assert(ReactiveValue(ascribed).isInstanceOf[ReactiveVariable[?]])
            assert(!ReactiveValue(view).isInstanceOf[ReactiveVariable[?]])
    }

    "a two-way binding still matches the read-only Dyn path" in {
        // Read-only hosts match `case Dyn(sig)` and must keep matching a ReactiveVariable, which is
        // what lets `disabled`/`readonly` share one constructor with the editable value slots.
        for ref <- Signal.initRef(true)
        yield ReactiveValue(ref) match
            case ReactiveValue.Dyn(s) => assert(s eq ref)
            case other                => fail(s"a two-way binding must read as Dyn, got $other")
    }

    "the reactive cases are exact: SignalRef is Signal's only named subclass" in {
        for
            ref <- Signal.initRef(1)
            derived = ref.map(identity)
        yield assert(
            ref.isInstanceOf[Signal.SignalRef[?]] &&
                !derived.isInstanceOf[Signal.SignalRef[?]] &&
                !ref.readOnly.isInstanceOf[Signal.SignalRef[?]]
        )
    }

    "the union setter reaches a component's slot" in {
        for
            ref <- Signal.initRef(5.0)
            sig = ref.map(_ * 2)
        yield
            def bindingOf(s: uic.Slider): String = s.valueBinding match
                case Present(ReactiveVariable(_))    => "two-way"
                case Present(ReactiveValue.Dyn(_))   => "one-way"
                case Present(ReactiveValue.Const(_)) => "const"
                case Absent                          => "unset"
            assert(bindingOf(uic.Slider().value(1.0)) == "const")
            assert(bindingOf(uic.Slider().value(ref)) == "two-way")
            assert(bindingOf(uic.Slider().value(sig)) == "one-way")
            assert(bindingOf(uic.Slider().value(ref.readOnly)) == "one-way")
    }
end ReactiveValueTest
