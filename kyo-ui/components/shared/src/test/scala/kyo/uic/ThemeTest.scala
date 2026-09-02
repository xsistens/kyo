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
        val auraLight = Tokens.auraLight.toMap
        def diff(preset: Seq[(String, String)]): Seq[(String, String)] =
            preset.filterNot((n, vl) => auraLight.get(n).contains(vl))
        val presets = Seq(
            "material" -> (Tokens.materialLight, Tokens.materialDark),
            "lara"     -> (Tokens.laraLight, Tokens.laraDark),
            "nora"     -> (Tokens.noraLight, Tokens.noraDark)
        )
        val root = varBlock(""":root, [data-theme], [data-scheme]""", Tokens.auraLight)
        val dark = varBlock("""[data-scheme="dark"]""", Tokens.auraDark)
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
        assert(t.toMap == Tokens.auraLight.toMap)
        assert(t.map(_._1).distinct.size == t.size, "a folded set names each token once")
    }

    "tokens(Aura, Dark) takes the dark value where the dark set overrides one" in {
        val light      = Tokens.auraLight.toMap
        val overridden = Tokens.auraDark.find((n, vl) => light.get(n).exists(_ != vl))
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
        val auraLight        = Tokens.auraLight.toMap
        val materialDiff     = Tokens.materialLight.filterNot((n, vl) => auraLight.get(n).contains(vl))
        val coveredByPreset  = (materialDiff.map(_._1) ++ Tokens.materialDark.map(_._1)).toSet
        val onlyFromAuraDark = Tokens.auraDark.filterNot((n, _) => coveredByPreset.contains(n))
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
    // ── the slim sheet ──────────────────────────────────────────────────────────────

    private def slim: String =
        Theme.cssFor(Tokens.auraLight, Tokens.auraDark, ComponentCss.button, ComponentCss.slider)

    "cssFor carries the sheets it was given, plus base and the kyo remainder" in {
        assert(slim.contains(".p-button"), "a named sheet is present")
        assert(slim.contains(".p-slider"), "the other named sheet is present")
        assert(slim.contains(".p-hidden-accessible"), "base is always included")
        assert(slim.contains(".p-uic-invalid-message"), "the kyo remainder is always included")
    }

    /** The assertion the whole exercise turns on. A `cssFor` that quietly included
      * everything would pass every other case here and save nothing.
      */
    /** The probes have to come from the twelve components the always-included parts never
      * name — see the floor case below. `organizationchart` and `stepper` are two of them.
      */
    "cssFor OMITS a sheet it was not given" in {
        assert(Theme.css.contains(".p-organizationchart"), "the full sheet has it")
        assert(!slim.contains(".p-organizationchart"), "the slim sheet does not")
        assert(Theme.css.contains(".p-stepper"), "the full sheet has it")
        assert(!slim.contains(".p-stepper"), "the slim sheet does not")
    }

    /** What `cssFor` CANNOT drop, pinned so nobody promises otherwise.
      *
      * `primeExtraCss` is one flat document of kyo's own glue, 66.5 KB of it, and it is
      * keyed on component classes: measured 2026-09-02, **62 of the 74 component names
      * appear in it or in `base`**, so a page that places no table still ships the table's
      * slot rules. The rules are inert without the component's own sheet, so this costs
      * bytes rather than correctness — but it means the slim path has a floor, and the
      * floor is component-specific rather than neutral. Splitting the remainder the way
      * `ComponentCss` is split is a separate piece of work.
      */
    "the kyo remainder is not component-neutral, so cssFor has a floor" in {
        assert(Theme.primeExtraCss.contains(".p-datatable-tbody"), "the remainder names DataTable")
        assert(slim.contains(".p-datatable-tbody"), "and it rides along even unnamed")
        assert(slim.contains(".p-galleria"), "so does Galleria, which was never asked for")
        assert(Theme.primeExtraCss.length > 60000, "the floor is real, not incidental")
    }

    /** The rung worth taking first: every sheet, one preset. Nothing can go missing, and
      * measured on a consuming app it is 771 035 of the 1 010 629 bundle bytes the slim
      * path saves — 76 % of the win for a change that needs no maintained list.
      */
    "cssFor with ComponentCss.all keeps every sheet and drops the other presets" in {
        val rungOne = Theme.cssFor(Tokens.auraLight, Tokens.auraDark, ComponentCss.all)
        assert(rungOne.contains(".p-organizationchart"), "every sheet is present")
        assert(rungOne.contains(".p-stepper"))
        assert(!rungOne.contains("""[data-theme="material"]"""), "Material's diff is gone")
        assert(!rungOne.contains("""[data-theme="nora"]"""), "so is Nora's")
        assert(rungOne.length < Theme.css.length, "and it is smaller than the full sheet")
    }

    "cssFor is a fraction of the full sheet" in {
        // Measured 2026-09-02: slim 243 223 chars against 683 691, a factor of 2.8. Most of
        // what remains is one preset's token pairs plus the ~62 KB remainder above; the two
        // named sheets are a rounding error next to those.
        assert(slim.length * 2 < Theme.css.length, s"slim=${slim.length} full=${Theme.css.length}")
    }

    "cssFor puts the two token sets on the selectors the full sheet uses" in {
        assert(slim.contains(""":root, [data-theme], [data-scheme]{"""))
        assert(slim.contains("""[data-scheme="dark"]{"""))
        // Aura's dark set declares the surface ramp; the light set declares the primary ramp.
        val darkBlock = slim.substring(
            slim.indexOf("""[data-scheme="dark"]{"""),
            slim.indexOf("}", slim.indexOf("""[data-scheme="dark"]{"""))
        )
        assert(darkBlock.contains("--p-surface-900:"), "the dark set landed in the dark block")
    }

    "cssFor with no dark tokens emits no dark block" in {
        val lightOnly = Theme.cssFor(Tokens.auraLight, Nil, ComponentCss.button)
        assert(lightOnly.contains(""":root, [data-theme], [data-scheme]{"""))
        assert(!lightOnly.contains("""[data-scheme="dark"]{"""))
    }

    "a brand preset composes onto a slim sheet the same way it does onto the full one" in {
        val brand = Theme.preset("brand", Seq("p-primary-400" -> "#1ed760"))
        val sheet = slim + "\n" + brand.render
        assert(sheet.indexOf("""[data-theme="brand"] {""") > sheet.indexOf(""":root, [data-theme]"""))
        assert(sheet.contains("--p-primary-400: #1ed760;"))
    }
end ThemeTest
