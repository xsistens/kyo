package kyo.uic

/** The GLYPH-SET SIZES, pinned to the generated objects.
  *
  * Both sets are machine-generated and both state their size in their file header
  * (`Icons.scala`: primeicons 7.0.0, `FioriIcons.scala`: SAP-icons-v5). Prose that
  * restates such a number drifts from it silently — the README carried 313 against a
  * generated set of 309 — so the numbers are counted from the compiled objects here and
  * quoted from this test.
  */
class IconsTest extends UicTest:

    /** Zero-argument members returning an [[IconGlyph]] — one per generated icon. */
    private def glyphCount(obj: Any): Int =
        // `equals` rather than `==`: strict equality has no instance for two `Class[?]`.
        obj.getClass.getMethods.count(m => m.getParameterCount == 0 && m.getReturnType.equals(classOf[IconGlyph]))

    "the generated glyph sets carry the sizes their generators recorded" in {
        assert(glyphCount(Icons) == IconsTest.PrimeIconCount, s"PrimeIcons: found ${glyphCount(Icons)}")
        assert(glyphCount(FioriIcons) == IconsTest.FioriIconCount, s"FioriIcons: found ${glyphCount(FioriIcons)}")
        // A spot check that the reflection counts glyphs and not incidental members.
        assert(Icons.check.name.nonEmpty, "a PrimeIcons glyph carries its name")
        assert(FioriIcons.save.name.nonEmpty, "a FioriIcons glyph carries its name")
    }
end IconsTest

object IconsTest:
    /** The PrimeIcons set (`Icons.scala`, primeicons 7.0.0). */
    val PrimeIconCount = 309

    /** The legacy SAP-icons-v5 set (`FioriIcons.scala`). */
    val FioriIconCount = 705
end IconsTest
