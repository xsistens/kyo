package kyo.apollo.codegen

/** A single local `@client` field declaration — pure metadata, so the object
  * holding these compiles independently of the code the codegen emits.
  * [[ApolloClientWriter]] turns each into a `ClientField.create` descriptor (under
  * the generated `ClientFields` object) plus a chained accessor
  * (`_.code.name.<field>`) next to the server selectors.
  *
  * @param onType  the GraphQL object type the field hangs off (`"Country"`, or
  *                `"Query"`/`"Mutation"`/`"Subscription"` for global state on the
  *                operation root).
  * @param name    the client field name (its named-tuple slot and cache key).
  * @param tpe     the field's Scala value type, verbatim — any `Schema`-derivable
  *                type: a scalar, `Option[…]`, `List[…]`, a (fully-qualified) case
  *                class, or arbitrary nesting. Emitted as `ClientField.create[Origin, tpe]`.
  * @param default a Scala expression for the value read when the field was never
  *                written (`"false"`, `"Nil"`, `"None"`, a literal, …).
  */
final case class ClientFieldDecl(
    onType: String,
    name: String,
    tpe: String,
    default: String
) derives CanEqual

/** A small authoring DSL so a declarations object reads declaratively. Mix in and
  * expose a `val fields: List[ClientFieldDecl]`:
  *
  * {{{
  * object MyClientFields extends ClientFieldDsl:
  *   val fields = declare(
  *     onType("Country")(
  *       field("isFavorite", "Boolean", default = "false"),
  *       field("tags", "List[String]", default = "Nil"),
  *       field("note", "Option[myapp.Note]", default = "None")),  // myapp.Note derives Schema
  *     onType("Query")(
  *       field("cartOpen", "Boolean", default = "false")))
  * }}}
  *
  * The codegen loads `fields` by reflection (it runs on the codegen classpath, so
  * `ClientFieldDecl` is the same class — no marshalling).
  */
trait ClientFieldDsl:

    /** A field declaration still missing its owning type, applied by [[onType]]. */
    opaque type Pending = String => ClientFieldDecl

    def field(name: String, tpe: String, default: String): Pending =
        t => ClientFieldDecl(t, name, tpe, default)

    def onType(t: String)(pending: Pending*): List[ClientFieldDecl] =
        pending.iterator.map(_(t)).toList

    def declare(groups: List[ClientFieldDecl]*): List[ClientFieldDecl] =
        groups.iterator.flatten.toList
end ClientFieldDsl
