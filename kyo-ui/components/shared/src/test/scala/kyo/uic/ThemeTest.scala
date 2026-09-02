package kyo.uic

import kyo.*

/** The token layer's SELECTOR contract — the part of theming that decides whether an
  * override lands, and that used to live only in a source comment.
  *
  * Everything here is pure string and pair algebra: `Theme` emits blocks, `preset` emits
  * a consumer's block in the same shape, and `tokens` reads the same list back. What the
  * browser then does with the cascade is asserted in the consuming app's walk, not here.
  */
class ThemeTest extends UicTest:

    // ── the emission, reconstructed independently ───────────────────────────────────

    /** A literal re-statement of what the sheet is supposed to be, written the way the
      * emission was written BEFORE it became a block list. Keeping it here rather than a
      * stored golden string means the pin is readable, and it catches a reordering or a
      * changed selector rather than merely a changed byte count.
      */
    private def expectedTokensCss: String =
        def varBlock(selector: String, pairs: Seq[(String, String)]): String =
            if pairs.isEmpty then ""
            else pairs.map((n, vl) => s"--$n:$vl").mkString(s"$selector{", ";", "}")
        val auraLight = generated.Tokens.auraLight.toMap
        def diff(preset: Seq[(String, String)]): Seq[(String, String)] =
            preset.filterNot((n, vl) => auraLight.get(n).contains(vl))
        val presets = Seq(
            "material" -> (generated.Tokens.materialLight, generated.Tokens.materialDark),
            "lara"     -> (generated.Tokens.laraLight, generated.Tokens.laraDark),
            "nora"     -> (generated.Tokens.noraLight, generated.Tokens.noraDark)
        )
        val root = varBlock(""":root, [data-theme], [data-scheme]""", generated.Tokens.auraLight)
        val dark = varBlock("""[data-scheme="dark"]""", generated.Tokens.auraDark)
        val scoped = presets.flatMap { (name, sets) =>
            val (light, darkSet) = sets
            Seq(
                varBlock(s"""[data-theme="$name"]""", diff(light)),
                varBlock(s"""[data-theme="$name"][data-scheme="dark"]""", darkSet)
            )
        }
        (root +: dark +: scoped).filter(_.nonEmpty).mkString("\n")
    end expectedTokensCss

    "primeTokensCss is exactly the four-block emission, byte for byte" in {
        assert(Theme.primeTokensCss == expectedTokensCss)
    }

    "the four selector forms are emitted, in resolution order" in {
        val css = Theme.primeTokensCss
        // Blocks are joined with "\n", so a leading newline distinguishes the standalone
        // dark block from the paired one it is also a suffix of.
        val iRoot     = css.indexOf(""":root, [data-theme], [data-scheme]{""")
        val iDark     = css.indexOf("\n" + """[data-scheme="dark"]{""")
        val iMaterial = css.indexOf("\n" + """[data-theme="material"]{""")
        val iPaired   = css.indexOf("\n" + """[data-theme="material"][data-scheme="dark"]{""")
        assert(iRoot == 0, "the Aura base set opens the sheet")
        assert(iDark > 0 && iMaterial > 0 && iPaired > 0, "every block form is present")
        // Source order IS resolution order here: the first three selectors are all
        // (0,1,0), so a later declaration wins. The paired block is (0,2,0) and wins
        // outright, which is what a consumer's own preset has to reproduce.
        assert(iRoot < iDark, "dark overrides come after the base set")
        assert(iDark < iMaterial, "a preset diff comes after the dark set it may re-cover")
        assert(iMaterial < iPaired, "a preset's dark block comes last")
    }

    "every shipped preset carries a non-empty diff and a dark block" in {
        val css = Theme.primeTokensCss
        Theme.Preset.values.toSeq.filterNot(_ == Theme.Preset.Aura).foreach { p =>
            // varBlock drops an empty pair list, so presence IS non-emptiness.
            assert(css.contains(s"""[data-theme="${p.attr}"]{"""), s"${p.attr} has a diff block")
            assert(
                css.contains(s"""[data-theme="${p.attr}"][data-scheme="dark"]{"""),
                s"${p.attr} has a dark block"
            )
        }
    }

    "Preset.attr is the data-theme value the sheet keys on" in {
        assert(Theme.Preset.Aura.attr == "aura")
        assert(Theme.Preset.Material.attr == "material")
        assert(Theme.Preset.Lara.attr == "lara")
        assert(Theme.Preset.Nora.attr == "nora")
    }

    // ── the token accessor ──────────────────────────────────────────────────────────

    "tokens(Aura, Light) is the Aura base set, with unique names" in {
        val t = Theme.tokens(Theme.Preset.Aura, Theme.Scheme.Light)
        assert(t.toMap == generated.Tokens.auraLight.toMap)
        assert(t.map(_._1).distinct.size == t.size, "a folded set names each token once")
    }

    "tokens(Aura, Dark) takes the dark value where the dark set overrides one" in {
        val light      = generated.Tokens.auraLight.toMap
        val overridden = generated.Tokens.auraDark.find((n, vl) => light.get(n).exists(_ != vl))
        assert(overridden.isDefined, "the dark set overrides at least one base token")
        val (name, darkValue) = overridden.get
        val folded            = Theme.tokens(Theme.Preset.Aura, Theme.Scheme.Dark).toMap
        assert(folded.get(name).contains(darkValue))
        assert(!folded.get(name).contains(light(name)))
    }

    /** The case that decides how `tokens` may be implemented. A dark element carrying a
      * preset resolves FOUR blocks, and `<preset>Dark` is only the last of them — so
      * `<preset>Light ++ <preset>Dark` would silently drop whatever the Aura dark set
      * contributes and nothing else re-covers.
      */
    "tokens(Material, Dark) keeps the Aura dark tokens that Material never mentions" in {
        val auraLight        = generated.Tokens.auraLight.toMap
        val materialDiff     = generated.Tokens.materialLight.filterNot((n, vl) => auraLight.get(n).contains(vl))
        val coveredByPreset  = (materialDiff.map(_._1) ++ generated.Tokens.materialDark.map(_._1)).toSet
        val onlyFromAuraDark = generated.Tokens.auraDark.filterNot((n, _) => coveredByPreset.contains(n))
        assert(onlyFromAuraDark.nonEmpty, "the Aura dark set contributes tokens Material does not re-declare")
        val folded = Theme.tokens(Theme.Preset.Material, Theme.Scheme.Dark).toMap
        onlyFromAuraDark.foreach((n, vl) => assert(folded.get(n).contains(vl), s"$n survives from the Aura dark set"))
    }

    // ── the consumer preset ─────────────────────────────────────────────────────────

    "preset emits the plain block and the paired dark block that out-specifies it" in {
        val css     = Theme.preset("brand", Seq("p-primary-400" -> "#1ed760")).render
        val iPlain  = css.indexOf("""[data-theme="brand"] {""")
        val iPaired = css.indexOf("""[data-theme="brand"][data-scheme="dark"] {""")
        assert(iPlain >= 0 && iPaired > iPlain)
        assert(css.contains("--p-primary-400: #1ed760;"))
    }

    "the paired block repeats the light tokens, so a dark scheme does not lose them" in {
        val css  = Theme.preset("brand", Seq("p-primary-400" -> "#1ed760", "p-surface-900" -> "#121212")).render
        val dark = css.substring(css.indexOf("""[data-theme="brand"][data-scheme="dark"] {"""))
        assert(dark.contains("--p-primary-400: #1ed760;"))
        assert(dark.contains("--p-surface-900: #121212;"))
    }

    "a dark delta lands after its light counterpart in the paired block" in {
        val css = Theme
            .preset("brand", Seq("p-primary-400" -> "#111111"), Seq("p-primary-400" -> "#222222"))
            .render
        val dark = css.substring(css.indexOf("""[data-theme="brand"][data-scheme="dark"] {"""))
        assert(dark.indexOf("#111111") < dark.indexOf("#222222"), "later declaration wins, so it goes last")
        // The plain block is untouched by the delta.
        val plain = css.substring(0, css.indexOf("""[data-theme="brand"][data-scheme="dark"] {"""))
        assert(plain.contains("#111111") && !plain.contains("#222222"))
    }

    "a preset with nothing to say emits nothing" in {
        assert(Theme.preset("brand", Nil).isEmpty)
        assert(Theme.preset("brand", Nil, Nil).isEmpty)
    }

    "a dark-only preset emits just the paired block" in {
        val css = Theme.preset("brand", Nil, Seq("p-surface-900" -> "#000000")).render
        assert(!css.contains("""[data-theme="brand"] {"""))
        assert(css.contains("""[data-theme="brand"][data-scheme="dark"] {"""))
    }

    "a derived preset starts from a shipped one" in {
        val derived = Theme.tokens(Theme.Preset.Aura, Theme.Scheme.Dark) ++ Seq("p-primary-400" -> "#1ed760")
        val css     = Theme.preset("brand", derived).render
        assert(css.contains("--p-primary-400: #1ed760;"))
        // Last declaration wins inside the block, so the override sits after the base value.
        val block = css.substring(css.indexOf("""[data-theme="brand"] {"""))
        assert(block.lastIndexOf("--p-primary-400:") > block.indexOf("--p-surface-0:"))
    }
end ThemeTest
