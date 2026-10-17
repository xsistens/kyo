package gen

/** Runs all generators: `sbt gen/run`. Inputs live in gen/input + gen/work +
  * gen/npm/node_modules (populate with gen/fetch.sh); output goes to
  * modules/components/src/main/scala/kyo/uic/generated.
  */
object Main:
    def main(args: Array[String]): Unit =
        println(s"repo root: ${Common.repoRoot}")
        FioriIconsGen.run()
        PrimeIconsGen.run()
        TokensGen.run()
        ComponentCssGen.run()
        println("generation complete")
    end main
end Main
