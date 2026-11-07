package kyo.uic

import kyo.*

class PropOpsTest extends UicTest:

    "present" - {
        "the element type comes from the optional prop, whatever its static type" in {
            for
                ref <- Signal.initRef("live")
                constant: Maybe[UI.Prop[String]]       = Present("fixed")
                bareSignal: Maybe[Signal[String]]      = Present(ref.map(_ + "!"))
                asRef: Maybe[Signal.SignalRef[String]] = Present(ref)
                union: Maybe[Signal[String] | String]  = Present("u")
                none: Maybe[UI.Prop[String]]           = Absent
                c: UI.Prop[Maybe[String]]              = constant.present
                s: UI.Prop[Maybe[String]]              = bareSignal.present
                r: UI.Prop[Maybe[String]]              = asRef.present
                u: UI.Prop[Maybe[String]]              = union.present
                n: UI.Prop[Maybe[String]]              = none.present
                sv <- UI.Prop.signal(s).current
                rv <- UI.Prop.signal(r).current
            yield
                assert(!c.isInstanceOf[Signal[?]] && c.asInstanceOf[Maybe[String]] == Present("fixed"), s"constant: $c")
                assert(!u.isInstanceOf[Signal[?]] && u.asInstanceOf[Maybe[String]] == Present("u"), s"union: $u")
                assert(!n.isInstanceOf[Signal[?]] && n.asInstanceOf[Maybe[String]] == Absent, s"none: $n")
                assert((sv, rv) == (Present("live!"), Present("live")), s"signal $sv, ref $rv")
            end for
        }
    }

end PropOpsTest
