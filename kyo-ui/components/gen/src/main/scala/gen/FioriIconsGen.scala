package gen

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*

/** Parses the `.js` icon modules under `gen/input/icons` (`@ui5/webcomponents-icons` dist/v5,
  * SAP-icons-v5 collection) and emits `kyo/uic/generated/FioriIcons.scala`: one `def`
  * per icon so Scala.js method-level DCE strips unused icons' path strings from
  * downstream bundles while autocomplete still surfaces the full set.
  */
object FioriIconsGen:
    private val PathDataRe = """const pathData\s*=\s*"([^"]*)"""".r
    private val ViewBoxRe  = """const viewBox\s*=\s*"0 0 (\d+) (\d+)"""".r

    final case class ParsedIcon(name: String, ident: String, pathData: String, w: Int, h: Int)

    def parseIcon(name: String, source: String): Option[ParsedIcon] =
        for
            path <- PathDataRe.findFirstMatchIn(source).map(_.group(1))
            vb   <- ViewBoxRe.findFirstMatchIn(source)
            if path.nonEmpty
        yield ParsedIcon(name, Common.toIdent(name), path, vb.group(1).toInt, vb.group(2).toInt)

    def run(): Unit =
        val iconsDir = Common.inputDir.resolve("icons")
        val icons = Files
            .list(iconsDir)
            .iterator()
            .asScala
            .filter(_.getFileName.toString.endsWith(".js"))
            .toSeq
            .sortBy(_.getFileName.toString)
            .flatMap { p =>
                val name = p.getFileName.toString.stripSuffix(".js")
                parseIcon(name, Files.readString(p))
            }

        require(icons.nonEmpty, s"no icons parsed from $iconsDir")
        val dupes = icons.groupBy(_.ident).filter(_._2.sizeIs > 1)
        require(dupes.isEmpty, s"ident collisions: ${dupes.keys.mkString(", ")}")

        val defs = icons
            .map { i =>
                s"""  /** ${i.name} */
           |  def ${i.ident}: IconGlyph = IconGlyph("${i.name}", "${i.pathData}", ${i.w}, ${i.h})""".stripMargin
            }
            .mkString("\n\n")

        val content =
            s"""${Common.GeneratedHeader}// Source: @ui5/webcomponents-icons ${Common.UI5Version}, dist/v5 (SAP-icons-v5), ${icons.size} icons.
         |package kyo.uic
         |
         |/** The legacy SAP-icons-v5 glyph set (kept alongside the primary PrimeIcons set). Each icon is a `def`, so Scala.js eliminates the
         |  * path data of icons a bundle never references. Usage: `uic.Icon(uic.FioriIcons.save)`.
         |  */
         |object FioriIcons:
         |$defs
         |end FioriIcons
         |""".stripMargin

        Common.write(Common.outputDir.resolve("FioriIcons.scala"), content)
    end run
end FioriIconsGen
