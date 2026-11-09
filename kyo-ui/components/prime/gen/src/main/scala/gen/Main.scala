package gen

/** Runs all generators: `sbt kyo-ui-components-gen/run` from the repo root. Inputs live in
  * input/, work/ and npm/node_modules/ under kyo-ui/components/prime/gen (populate with its fetch.sh);
  * output goes to kyo-ui/components/prime/shared/src/main/scala/kyo/uic/generated.
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
