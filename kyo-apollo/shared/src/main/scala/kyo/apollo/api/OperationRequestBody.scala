package kyo.apollo.api

import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Composes the wire-format request body for an [[Operation]].
  *
  * Mirrors apollo-kotlin's `composeJsonRequest`: emits the standard GraphQL POST
  * body `{ "query", "operationName", "variables", "extensions" }`. The
  * `variables` object is the operation's own [[Operation.variables]] (a
  * [[Json.JObj]] built per-variable through each value's `Schema`), so this type
  * no longer needs a custom-scalar registry. This replaces the ad-hoc
  * `Json.JObj(...)` request building the Phase 01 demo did inline.
  *
  * Field order is fixed (`query`, `operationName`, `variables`, then an optional
  * `extensions`) so request bodies serialise deterministically and are
  * diff/test-friendly. `extensions` is omitted entirely when empty — a plain
  * query sends no `extensions` object on the wire.
  */
object OperationRequestBody:

    /** Build the request body for `operation` as a [[Json]] value.
      *
      * @param operation  the operation to serialise
      * @param extensions optional top-level `extensions` (omitted when empty)
      */
    def apply[D](
        operation: Operation[D],
        extensions: Map[String, Json] = Map.empty
    ): Json =
        val fields = VectorMap.newBuilder[String, Json]
        fields += "query"                                  -> Json.JStr(operation.document)
        fields += "operationName"                          -> Json.JStr(operation.name)
        fields += "variables"                              -> operation.variables
        if extensions.nonEmpty then fields += "extensions" -> Json.JObj(VectorMap.from(extensions))
        Json.JObj(fields.result())
    end apply

    /** Render the request body for `operation` as compact JSON text. */
    def render[D](
        operation: Operation[D],
        extensions: Map[String, Json] = Map.empty
    ): String =
        apply(operation, extensions).render
end OperationRequestBody
