package kyo

import kyo.Length.*

class LengthTest extends kyo.test.Test[Any]:

    "parse" - {
        "reads each unit" in {
            assert(Length.parse("10px") == Px(10))
            assert(Length.parse("50%") == Pct(50))
            assert(Length.parse("1.5em") == Em(1.5))
            assert(Length.parse("50vh") == Vh(50))
        }

        "a bare number is px" in {
            assert(Length.parse("12") == Px(12))
        }

        "auto, in any case" in {
            assert(Length.parse("auto") == Auto)
            assert(Length.parse("AUTO") == Auto)
        }

        "surrounding space and a sign" in {
            assert(Length.parse(" 10px ") == Px(10))
            assert(Length.parse("-5px") == Px(-5))
        }

        "a rem is no em, and stays an expression" in {
            assert(Length.parse("1.5rem") == Calc("1.5rem"))
        }

        "what it cannot read is carried as an expression" in {
            assert(Length.parse("calc(100% - 2rem)") == Calc("calc(100% - 2rem)"))
            assert(Length.parse("foo") == Calc("foo"))
            assert(Length.parse("") == Calc(""))
        }

        "what only a JVM double takes is no number" in {
            assert(Length.parse("NaN") == Calc("NaN"))
            assert(Length.parse("Infinity") == Calc("Infinity"))
            assert(Length.parse("12f") == Calc("12f"))
            assert(Length.parse("1dpx") == Calc("1dpx"))
        }
    }

    "Px resolves to its integer value" in {
        assert(Length.resolve(Px(42), 100) == 42)
    }

    "Pct resolves to percentage of parent" in {
        assert(Length.resolve(Pct(50), 200) == 100)
    }

    "Pct(25) resolves correctly" in {
        assert(Length.resolve(Pct(25), 400) == 100)
    }

    "Em resolves to its integer value (1em = 1 cell)" in {
        assert(Length.resolve(Em(3), 100) == 3)
    }

    "Auto fills the parent dimension" in {
        assert(Length.resolve(Auto, 150) == 150)
    }

    "Vh resolves as a percentage of the parent dimension" in {
        assert(Length.resolve(Vh(50), 200) == 100)
    }

    "Calc resolves to the parent dimension (opaque expression)" in {
        assert(Length.resolve(Calc("100vh - 60px"), 150) == 150)
    }

    "int extension .vh creates Vh" in {
        assert((100.vh) == Vh(100.0))
    }

    "double extension .vh creates Vh" in {
        assert((33.5.vh) == Vh(33.5))
    }

    "Vh equality" in {
        assert(Vh(100) == Vh(100))
        assert(Vh(100) != Vh(50))
    }

    "Calc equality" in {
        assert(Calc("100vh - 60px") == Calc("100vh - 60px"))
        assert(Calc("100vh - 60px") != Calc("100vh"))
    }

    "resolveOrAuto on Px returns Present" in {
        assert(Length.resolveOrAuto(Px(10), 100) == kyo.Present(10))
    }

    "resolveOrAuto on Auto returns Absent" in {
        assert(Length.resolveOrAuto(Auto, 100) == kyo.Absent)
    }

    "Px is directly usable as Px without conversion" in {
        val px: Length.Px = Length.Px(5)
        assert(px == Length.Px(5))
    }

    "Em numeric value is accessible without toPx" in {
        val em = Length.Em(3)
        assert(em.value == 3.0)
    }

    "int extension .px creates Px" in {
        assert((10.px) == Px(10.0))
    }

    "int extension .pct creates Pct" in {
        assert((25.pct) == Pct(25.0))
    }

    "int extension .em creates Em" in {
        assert((2.em) == Em(2.0))
    }

    "double extension .px creates Px" in {
        assert((1.5.px) == Px(1.5))
    }

    "Px(0) is the zero constant" in {
        assert(Length.zero == Px(0))
    }

    "Px equality" in {
        assert(Px(7) == Px(7))
        assert(Px(7) != Px(8))
    }

    "Pct equality" in {
        assert(Pct(50) == Pct(50))
        assert(Pct(50) != Pct(51))
    }

    "Em equality" in {
        assert(Em(2) == Em(2))
        assert(Em(2) != Em(3))
    }

    "Auto equality to itself" in {
        assert((Auto: Length) == Auto)
    }

    "Px resolve with zero parent" in {
        assert(Length.resolve(Px(5), 0) == 5)
    }

    "Pct resolve with zero parent" in {
        assert(Length.resolve(Pct(100), 0) == 0)
    }

    "negative Px resolves correctly" in {
        assert(Length.resolve(Px(-10), 100) == -10)
    }

    "implicit int conversion to Px" in {
        val l: Px = 8
        assert(l == Px(8.0))
    }

    "implicit double conversion to Px" in {
        val l: Px = 3.14
        assert(l == Px(3.14))
    }

end LengthTest
