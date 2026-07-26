package kyo.apollo.api

/** A single entry within a GraphQL selection set — either a field or an inline
  * fragment. Mirrors apollo-kotlin's sealed `CompiledSelection`.
  *
  * A whole operation's response shape is a tree of these: the operation's
  * [[Operation.rootField]] holds the top-level selections, each of which may
  * carry nested selections of its own. This tree is what later powers response
  * normalization and cache-key computation.
  */
sealed trait CompiledSelection

/** A selected field, e.g. `missionName` or `launches(limit: $limit) { ... }`.
  *
  * @param name       the field name as declared in the schema
  * @param fieldType  the field's GraphQL type reference (apollo-kotlin's `type`)
  * @param alias      the response alias, when the field is queried under a
  *                   different key than its name; `None` otherwise
  * @param arguments  the arguments supplied to the field, in declaration order
  * @param selections the field's own selection set, empty for leaf/scalar fields
  * @param client     `true` for a local `@client` field: normalized into and read
  *                   from the cache like any field, but pruned from the printed
  *                   document and tolerant of a cache miss (yields a default
  *                   rather than throwing). Server fields are `false`.
  */
final case class CompiledField(
    name: String,
    fieldType: CompiledType,
    alias: Option[String] = None,
    arguments: List[CompiledArgument] = Nil,
    selections: List[CompiledSelection] = Nil,
    client: Boolean = false,
    stream: Option[StreamDirective] = None
) extends CompiledSelection:
    /** The key this field's value is stored under in the response: its [[alias]]
      * when present, otherwise its [[name]].
      */
    def responseName: String = alias.getOrElse(name)
end CompiledField

/** An inline fragment narrowing the selection to a concrete type condition,
  * e.g. `... on Launch { ... }`. An empty [[typeCondition]] with empty
  * [[possibleTypes]] is an *anonymous* fragment (`... { ... }`) — the shape a
  * bare `@defer` group takes, spliced unconditionally by the field collector.
  *
  * @param typeCondition the type the fragment applies to (empty for anonymous)
  * @param possibleTypes the concrete object types the condition can match at
  *                      runtime (used to decide whether the fragment's
  *                      selections apply to a given `__typename`)
  * @param selections    the fragment's selection set
  * @param defer         the `@defer` directive when this fragment's fields are
  *                      delivered incrementally over multipart; `None` otherwise
  */
final case class CompiledFragment(
    typeCondition: String,
    possibleTypes: List[String] = Nil,
    selections: List[CompiledSelection] = Nil,
    defer: Option[DeferDirective] = None
) extends CompiledSelection

/** The `@defer` directive on an inline fragment: the fragment's fields are
  * delivered as a later multipart payload rather than in the initial response.
  *
  * @param label a client-chosen label correlating the incremental patch to this
  *              fragment (auto-derived from the field response name for a
  *              single-field `.deferred`, explicit for a `defer("label", …)` group)
  * @param `if`  the name of an optional Boolean operation variable gating the
  *              defer at runtime (`@defer(if: $var)`); `None` = always deferred
  */
final case class DeferDirective(label: String, `if`: Option[String] = None)

/** The `@stream` directive on a list field: the field's list is delivered
  * incrementally over multipart — the initial response carries the first
  * [[initialCount]] items, and each later part appends more. Unlike `@defer`, the
  * field's result type is unchanged (`List[T]`); the list simply starts short and
  * grows, so the assembler splices appended items rather than merging an object.
  *
  * @param label        a client-chosen label correlating the incremental patch to
  *                     this field (auto-derived from the field's response name by
  *                     the `.streamed` marker)
  * @param initialCount how many items the server includes in the initial payload
  *                     before it begins streaming the remainder
  * @param `if`         the name of an optional Boolean operation variable gating
  *                     the stream at runtime (`@stream(if: $var)`); `None` = always
  */
final case class StreamDirective(label: String, initialCount: Int, `if`: Option[String] = None)
