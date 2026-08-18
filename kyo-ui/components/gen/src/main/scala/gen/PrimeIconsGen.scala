package gen

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*

/** Parses the raw SVGs shipped by `primeicons` (gen/npm/node_modules/primeicons/raw-svg)
  * and emits `kyo/uic/generated/Icons.scala` — the PRIMARY icon set: one `def` per
  * icon so Scala.js method-level DCE strips unused icons' path strings from
  * downstream bundles.
  *
  * Icons with several `<path>` elements are merged into one path-data string —
  * every primeicons subpath starts with an absolute `M`, so concatenation is
  * valid SVG path grammar.
  */
object PrimeIconsGen:
    private val ViewBoxRe = """viewBox="0 0 (\d+) (\d+)"""".r
    private val PathDRe   = """<path[^>]*\bd="([^"]+)"""".r

    final case class ParsedIcon(name: String, ident: String, pathData: String, w: Int, h: Int)

    /** `java.lang.Object` members can't be (re)declared with a different signature
      * inside an object — icons named like them get an `Icon` suffix.
      */
    private val objectMembers =
        Set("clone", "equals", "finalize", "getClass", "hashCode", "notify", "notifyAll", "toString", "wait")

    private def ident(name: String): String =
        val id = Common.toIdent(name)
        if objectMembers(id) then id + "Icon" else id

    def parseIcon(name: String, svg: String): Option[ParsedIcon] =
        val paths = PathDRe.findAllMatchIn(svg).map(_.group(1).trim).toSeq
        for
            vb <- ViewBoxRe.findFirstMatchIn(svg)
            if paths.nonEmpty
        yield ParsedIcon(name, ident(name), paths.mkString(" "), vb.group(1).toInt, vb.group(2).toInt)
        end for
    end parseIcon

    def run(): Unit =
        val svgDir = Common.repoRoot.resolve("gen/npm/node_modules/primeicons/raw-svg")
        require(Files.isDirectory(svgDir), s"primeicons not installed — run gen/fetch.sh first ($svgDir)")
        val icons = Files
            .list(svgDir)
            .iterator()
            .asScala
            .filter(_.getFileName.toString.endsWith(".svg"))
            .toSeq
            .sortBy(_.getFileName.toString)
            .flatMap { p =>
                val name = p.getFileName.toString.stripSuffix(".svg")
                parseIcon(name, Files.readString(p))
            }

        require(icons.nonEmpty, s"no icons parsed from $svgDir")
        val dupes = icons.groupBy(_.ident).filter(_._2.sizeIs > 1)
        require(dupes.isEmpty, s"ident collisions: ${dupes.keys.mkString(", ")}")

        val defs = icons
            .map { i =>
                s"""  /** ${i.name} */
           |  def ${i.ident}: IconGlyph = IconGlyph("${i.name}", "${i.pathData}", ${i.w}, ${i.h})""".stripMargin
            }
            .mkString("\n\n")

        val content =
            s"""${Common.header(s"primeicons ${Common.PrimeIconsVersion} raw-svg")}// ${icons.size} icons.
         |package kyo.uic
         |
         |/** The PrimeIcons glyph set (primary). Each icon is a `def`, so Scala.js
         |  * eliminates the path data of icons a bundle never references.
         |  * Usage: `uic.Icon(uic.Icons.check)`.
         |  */
         |object Icons:
         |$defs
         |end Icons
         |""".stripMargin

        Common.write(Common.outputDir.resolve("Icons.scala"), content)
    end run
end PrimeIconsGen
