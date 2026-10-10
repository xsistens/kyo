package kyo.apollo.codegen

/** A single local `@client` field declaration — pure metadata about code the
  * generator emits. [[ApolloClientWriter]] turns each into a `ClientField.create`
  * descriptor (under the generated `ClientFields` object) plus a chained accessor
  * (`_.code.name.<field>`) next to the server selectors.
  *
  * @param onType  the GraphQL object type the field hangs off (`"Country"`, or
  *                `"Query"`/`"Mutation"`/`"Subscription"` for global state on the
  *                operation root).
  * @param name    the client field name (its named-tuple slot and cache key).
  * @param tpe     the field's Scala value type, verbatim — any `Schema`-derivable
  *                type: a scalar, `Maybe[…]`, `Chunk[…]`, a (fully-qualified) case
  *                class, or arbitrary nesting. Emitted as `ClientField.create[Origin, tpe]`.
  * @param default a Scala expression for the value read when the field was never
  *                written (`"false"`, `"Chunk.empty"`, `"Absent"`, a literal, …).
  */
final case class ClientFieldDecl(
    onType: String,
    name: String,
    tpe: String,
    default: String
) derives CanEqual

object ClientFieldDecl:

    /** `Type.field: ScalaType = default`; the type runs up to the first `=` that does
      * not start a `=>`, so function types stay intact.
      */
    private val Declaration =
        """\s*([_A-Za-z][_0-9A-Za-z]*)\.([_A-Za-z][_0-9A-Za-z]*)\s*:\s*(.+?)\s*=(?!>)\s*(.+?)\s*""".r

    /** Read a declaration written the way a Scala field reads, as a build passes it:
      * `--client-field "Country.isFavorite: Boolean = false"`.
      *
      * @throws CodegenException.MalformedClientField if `spec` has another shape.
      */
    def parse(spec: String): ClientFieldDecl =
        spec match
            case Declaration(onType, name, tpe, default) => ClientFieldDecl(onType, name, tpe, default)
            case _                                       => throw CodegenException.MalformedClientField(spec)
end ClientFieldDecl
