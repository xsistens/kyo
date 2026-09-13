package kyo.apollo.codegen

/** Demo local `@client` field declarations for the bundled `codegenExample`
  * schema — wired via `apolloClientFieldsClass` in `build.sbt` so `example` /
  * `kyoUiExample` get generated `ClientFields` descriptors and `_.code.isFavorite`
  * accessors. A real app points `apolloClientFieldsClass` at its own object.
  */
object DemoClientFields extends ClientFieldDsl:
    val fields: List[ClientFieldDecl] = declare(
        onType("Country")(
            field("isFavorite", "Boolean", default = "false"),
            field("tags", "List[String]", default = "Nil")
        ),
        onType("Query")(
            field("cartOpen", "Boolean", default = "false")
        )
    )
end DemoClientFields
