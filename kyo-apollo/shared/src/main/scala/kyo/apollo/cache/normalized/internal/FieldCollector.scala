package kyo.apollo.cache.normalized.internal

import kyo.Chunk
import kyo.apollo.api.*
import kyo.discard
import scala.collection.mutable

/** Flattens a GraphQL selection set into the concrete [[CompiledField]]s that
  * apply to an object of a given `__typename`.
  *
  * Both the write path ([[Normalizer]]) and the read path
  * ([[CacheBatchReader]]) need the *same* view of a selection set, so this logic
  * lives in one place: plain fields pass through, an inline fragment's fields
  * are spliced in when its `possibleTypes` include the object's type (or it
  * carries no constraint), and fields sharing a response name are merged by
  * concatenating their sub-selections. Keeping the two walks in lock-step is
  * what makes a `writeOperation` → `readOperation` round-trip see an identical
  * field set. Mirrors apollo-kotlin's field-collection step.
  */
private[internal] object FieldCollector:

    /** The implicitly-collected `__typename` field. */
    val TypenameField: CompiledField =
        CompiledField("__typename", CompiledNamedType("String"))

    /** Collect the fields of `selections` that apply to `typename`.
      *
      * @param selections     the selection set to flatten
      * @param typename       the concrete type of the object being read/written,
      *                       used to decide which inline fragments apply
      * @param injectTypename when true, `__typename` is added if not already
      *                       selected — the write path wants it so keys can be
      *                       computed and fragments re-resolved on read; the read
      *                       path leaves it off so only fields the caller actually
      *                       selected are required from the cache.
      */
    def collect(
        selections: Chunk[CompiledSelection],
        typename: String,
        injectTypename: Boolean
    ): Chunk[CompiledField] =
        val collected = mutable.LinkedHashMap.empty[String, CompiledField]

        def add(field: CompiledField): Unit =
            discard(collected.updateWith(field.responseName) {
                case Some(existing) =>
                    Some(existing.copy(selections = existing.selections ++ field.selections))
                case None => Some(field)
            })

        def go(sels: Chunk[CompiledSelection]): Unit = sels.foreach {
            case field: CompiledField => add(field)
            case fragment: CompiledFragment =>
                if fragment.possibleTypes.isEmpty || fragment.possibleTypes.contains(typename) then
                    go(fragment.selections)
        }

        go(selections)
        if injectTypename && !collected.contains(TypenameField.responseName) then add(TypenameField)
        Chunk.from(collected.values)
    end collect
end FieldCollector
