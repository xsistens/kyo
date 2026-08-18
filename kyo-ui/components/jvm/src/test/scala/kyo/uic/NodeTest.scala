package kyo.uic

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters.*

/** The module's COMPONENT COUNT, pinned to the compiled surface.
  *
  * "How many components" had no answer in source: the number quoted in prose was the
  * count of `.scala` files at the top level of the package, which is neither the number
  * of types nor the number of things a caller can place. Counting rule, decided here and
  * enforced below:
  *
  *   - a COMPONENT is a public, concrete, top-level class in package `kyo.uic` that
  *     implements [[Node]] — exactly the set a caller can construct and place;
  *   - nested types (`Select.State`, `CheckBox.Checked`), companion objects, traits
  *     ([[Node]], [[FormControl]] and its family) and the model carriers that are not
  *     components (`TreeNode`, `ListItem`, `Column`, `MenuItem`, ...) are not counted.
  *
  * The count is read from the compiled classes rather than restated, so it cannot drift
  * from the code; the assertion below is what a README sentence should quote.
  */
class NodeTest extends UicTest:

    /** Public, concrete, top-level `kyo.uic` classes implementing [[Node]], by simple name. */
    private def componentNames: List[String] =
        val marker    = classOf[Button]
        val classesIn = Paths.get(marker.getProtectionDomain.getCodeSource.getLocation.toURI).resolve("kyo").resolve("uic")
        val loader    = marker.getClassLoader
        // `Files.list` opens a descriptor on the directory; the suite's leak check fails
        // the run if it is not closed, so the stream is drained inside `use`.
        val stream = Files.list(classesIn)
        val files =
            try stream.iterator.asScala.toList
            finally stream.close()
        files
            .map(_.getFileName.toString)
            .filter(_.endsWith(".class"))
            .map(_.dropRight(".class".length))
            // Nested types and companion objects both carry a '$' in the JVM name.
            .filterNot(_.contains("$"))
            .flatMap { simple =>
                val c = Class.forName(s"kyo.uic.$simple", false, loader)
                val isComponent =
                    classOf[Node].isAssignableFrom(c) &&
                        !c.isInterface &&
                        !java.lang.reflect.Modifier.isAbstract(c.getModifiers) &&
                        java.lang.reflect.Modifier.isPublic(c.getModifiers)
                if isComponent then List(simple) else Nil
            }
            .sorted
    end componentNames

    "the component count is the number of placeable Node types, read from the compiled surface" in {
        val names = componentNames
        // A spot check that the rule selects what it claims to.
        assert(names.contains("Button"), "Button is a component")
        assert(names.contains("DataTable"), "a generic component counts once, not per type argument")
        assert(names.contains("FileUpload"), "the newer controls count too")
        assert(!names.contains("Node"), "the base trait is not a component")
        assert(!names.contains("FormControl"), "the form-control traits are not components")
        assert(!names.contains("TreeNode"), "a model carrier is not a component")
        assert(!names.contains("Column"), "a column descriptor is not a component")
        assert(!names.contains("Theme"), "the stylesheet object is not a component")
        // The number itself. Update it deliberately, with the component that changed it.
        assert(
            names.size == NodeTest.ComponentCount,
            s"component count changed: expected ${NodeTest.ComponentCount}, found ${names.size}\n${names.mkString(", ")}"
        )
    }
end NodeTest

object NodeTest:
    /** The count asserted above — the one number prose may quote. */
    val ComponentCount = 83
end NodeTest
