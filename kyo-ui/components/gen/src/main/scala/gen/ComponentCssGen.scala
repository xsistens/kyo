package gen

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** Wraps the per-component CSS emitted by `gen/extract.mjs` (the .css files under
  * gen/work/css — `@primeuix/styles` with every `dt('token.path')` resolved to
  * `var(--p-...)`)
  * into `kyo/uic/generated/ComponentCss.scala`: one `def` per component plus
  * `base`. Strings are chunked below the JVM's 64K string-constant limit and
  * emitted as triple-quoted literals (no interpolation, no escape processing —
  * CSS `content: "\\2713"`-style escapes survive verbatim).
  */
object ComponentCssGen:
    private val ChunkSize = 40000

    private def cssDef(ident: String, css: String): String =
        require(!css.contains("\"\"\""), s"CSS for $ident contains a triple quote")
        val chunks = css.grouped(ChunkSize).toSeq
        if chunks.sizeIs <= 1 then
            s"""  def $ident: String = \"\"\"${css}\"\"\""""
        else
            val parts = chunks.zipWithIndex
                .map((c, i) => s"""  private def ${ident}_$i: String = \"\"\"${c}\"\"\"""")
                .mkString("\n")
            s"$parts\n  def $ident: String = ${chunks.indices.map(i => s"${ident}_$i").mkString(" + ")}"
        end if
    end cssDef

    def run(): Unit =
        val cssDir = Common.repoRoot.resolve("gen/work/css")
        require(Files.isDirectory(cssDir), s"extracted CSS missing — run gen/fetch.sh first ($cssDir)")

        val files = Files
            .list(cssDir)
            .iterator()
            .asScala
            .filter(_.getFileName.toString.endsWith(".css"))
            .toSeq
            .sortBy(_.getFileName.toString)

        val defs = files.map { p =>
            val name = p.getFileName.toString.stripSuffix(".css")
            cssDef(Common.toIdent(name), Files.readString(p).trim + "\n")
        }
        val names = files.map(p => Common.toIdent(p.getFileName.toString.stripSuffix(".css")))
        println(s"component css: ${names.mkString(", ")}")

        val content =
            s"""${Common.header(s"@primeuix/styles ${Common.PrimeUixVersion} via gen/extract.mjs")}package kyo.uic
         |package generated
         |
         |/** Per-component PrimeOne CSS (`.p-*` selectors consuming `var(--p-*)` tokens
         |  * from [[Tokens]]). Preset-independent: only the token VALUES differ between
         |  * presets. `all` concatenates every extracted sheet.
         |  */
         |object ComponentCss:
         |
         |${defs.mkString("\n\n")}
         |
         |  def all: String = Seq(${names.mkString(", ")}).mkString("\\n")
         |
         |end ComponentCss
         |""".stripMargin

        Common.write(Common.outputDir.resolve("ComponentCss.scala"), content)
    end run
end ComponentCssGen
