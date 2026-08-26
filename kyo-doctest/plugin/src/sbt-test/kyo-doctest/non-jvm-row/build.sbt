// Scripted test: doctest refuses to run on a non-JVM cross-project row.
//
// The task forks a JVM. On a Scala.js, Wasm, or Native row the classpath carries
// artifacts that JVM cannot load, so the fork dies inside the runner with a
// NoClassDefFoundError instead of reporting a doctest result. The aggregate
// command already skips those rows; this pins that a direct invocation is
// refused too, and that the JVM row sharing the same README keeps working.
//
// No Scala.js here on purpose: the rule keys off the sbt-crossproject row
// directory name, so a plain project in `js/` reproduces the condition without
// pulling a second platform into the scripted build.

ThisBuild / scalaVersion := sys.props("kyo.doctest.scalaVersion")

lazy val root = (project in file("."))
    .aggregate(jvm, js)
    .disablePlugins(KyoDoctestPlugin)
    .settings(name := "non-jvm-row")

lazy val jvm = (project in file("jvm"))
    .enablePlugins(KyoDoctestPlugin)
    .settings(
        name := "non-jvm-row-jvm",
        doctestScalacOptions := Seq("-release", "17"),
        // Runner classpath injected by the plugin's scriptedDependencies (no ivy resolution).
        doctestExtraClasspath := IO.readLines(file(sys.props("kyo.doctest.runnerCpFile"))).map(file)
    )

lazy val js = (project in file("js"))
    .enablePlugins(KyoDoctestPlugin)
    .settings(
        name := "non-jvm-row-js",
        doctestScalacOptions := Seq("-release", "17"),
        doctestExtraClasspath := IO.readLines(file(sys.props("kyo.doctest.runnerCpFile"))).map(file)
    )
