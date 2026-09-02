package gen

import java.nio.file.Files

/** Parses the token JSON emitted by `gen/extract.mjs` (gen/work/tokens/<preset>.json,
  * produced by the MIT `@primeuix/styled` engine's own `toVariables` output) and
  * emits `kyo/uic/generated/Tokens.scala`: per preset a full light set plus the
  * dark override set, as `Seq[(String, String)]` pairs (names without the leading
  * `--`, for `Stylesheet.vars`-style consumption).
  */
object TokensGen:
    private val Presets = Seq("aura", "material", "lara", "nora")

    private def chunked(ident: String, pairs: Seq[(String, String)]): String =
        val groups = pairs.grouped(250).toSeq
        val defs = groups.zipWithIndex
            .map { (g, i) =>
                val body = g
                    .map { (n, v) =>
                        val ve = v.replace("\\", "\\\\").replace("\"", "\\\"")
                        s"""    ("$n", "$ve")"""
                    }
                    .mkString(",\n")
                s"""  private def ${ident}_$i: Seq[(String, String)] = Seq(
           |$body
           |  )""".stripMargin
            }
            .mkString("\n\n")
        // `def`, never `val`: a val in an object is built by the static initialiser, so it is
        // retained the moment anything touches `Tokens` at all, and a consumer that names one
        // preset would still carry all eight sets. One def per accessor is what lets Scala.js
        // method-level DCE keep only what a bundle references — the same rule PrimeIconsGen
        // states for the 309 icon defs.
        val concat =
            if groups.isEmpty then s"  def $ident: Seq[(String, String)] = Seq.empty"
            else s"  def $ident: Seq[(String, String)] = ${groups.indices.map(i => s"${ident}_$i").mkString(" ++ ")}"
        s"$defs\n\n$concat"
    end chunked

    def run(): Unit =
        val tokensDir = Common.repoRoot.resolve("gen/work/tokens")
        require(Files.isDirectory(tokensDir), s"token JSON missing — run gen/fetch.sh first ($tokensDir)")

        val sections = Presets.flatMap { preset =>
            val json = ujson.read(Files.readString(tokensDir.resolve(s"$preset.json")))
            def pairs(key: String): Seq[(String, String)] =
                json(key).arr.toSeq.map(e => (e(0).str.stripPrefix("--"), e(1).str))
            val light = pairs("light")
            val dark  = pairs("dark")
            println(s"$preset: ${light.size} light + ${dark.size} dark tokens")
            Seq(chunked(s"${preset}Light", light), chunked(s"${preset}Dark", dark))
        }

        val content =
            s"""${Common.header(s"@primeuix/themes ${Common.PrimeUixVersion} via gen/extract.mjs")}package kyo.uic
         |
         |/** PrimeOne design tokens (`--p-*`, stored without the leading `--`) per
         |  * preset x color scheme. `<preset>Light` is the complete set for
         |  * `Stylesheet.vars`/`scopedVars`; `<preset>Dark` holds only the dark-scheme
         |  * overrides the engine emits under the dark selector.
         |  *
         |  * Each accessor is a `def`, so Scala.js method-level dead-code elimination keeps
         |  * only the preset and scheme a bundle actually names; `Theme.cssFor` is the entry
         |  * point that consumes them directly. For the same reason there is no `all`, no
         |  * `byName` and no `Map` here: a lookup that maps a value to a token set references
         |  * every token set, and the whole 765 KB comes along.
         |  */
         |object Tokens:
         |
         |${sections.mkString("\n\n")}
         |
         |end Tokens
         |""".stripMargin

        Common.write(Common.outputDir.resolve("Tokens.scala"), content)
    end run
end TokensGen
