package kyo.apollo.json

import kyo.Chunk

/** Walks and splices a rooted response path (`["a", 0, "b"]`, mixing field names
  * and list indices) into a [[Json]] tree — the addressing an incremental-delivery
  * (`@defer`) patch uses to merge into the accumulated response. Field-unions
  * objects (later wins), matching the normalized cache's record merge.
  */
object JsonPath:

    private given CanEqual[String | Int, String | Int] = CanEqual.derived

    /** Parse a wire `path` array into `String` (field) / `Int` (index) segments. */
    def parse(json: Json): List[String | Int] = json match
        case Json.JArr(items) =>
            items.toList.flatMap {
                case Json.JStr(name)  => Some(name)
                case Json.JNum(index) => Some(index.toInt)
                case _                => None
            }
        case _ => Nil

    /** Splice `patch` into `target` at `path`. An empty path merges at the root; a
      * segment that doesn't match the tree's shape is a no-op (leaves `target`).
      */
    def splice(target: Json, path: List[String | Int], patch: Json): Json =
        path match
            case Nil => merge(target, patch)
            case (field: String) :: rest =>
                target match
                    case Json.JObj(fields) =>
                        val child = fields.getOrElse(field, Json.JObj(Map.empty))
                        Json.JObj(fields.updated(field, splice(child, rest, patch)))
                    case _ => target
            case (index: Int) :: rest =>
                target match
                    case Json.JArr(items) if index >= 0 && index < items.size =>
                        val updated = splice(items(index), rest, patch)
                        Json.JArr(Chunk.from(items.toList.updated(index, updated)))
                    case _ => target

    /** Splice `@stream`ed `items` into the list at `path`, whose final segment is the
      * start index the items go at. The list GROWS: items at indices ≥ its current
      * size are appended (a `@stream` patch always targets the tail, contiguously from
      * `initialCount`), while a rare in-range index overwrites. A non-terminal index
      * segment recurses into a list element (a `@stream` nested inside a list); a shape
      * mismatch, a negative start index, or a present-but-non-list target is a no-op
      * (never a throw — a malformed `path` must not crash the incremental stream).
      * Non-contiguous start indices (a gap past the current tail) are not part of the
      * spec's delivery contract and collapse onto the tail rather than padding holes.
      */
    def spliceItems(target: Json, path: List[String | Int], items: List[Json]): Json =
        path match
            case (index: Int) :: Nil if index >= 0 =>
                target match
                    case Json.JArr(existing) =>
                        val grown = items.zipWithIndex.foldLeft(existing.toList) { case (acc, (item, k)) =>
                            val pos = index + k
                            if pos < acc.size then acc.updated(pos, item) else acc :+ item
                        }
                        Json.JArr(Chunk.from(grown))
                    case _ => target
            case (field: String) :: rest =>
                target match
                    case Json.JObj(fields) =>
                        val child = fields.getOrElse(field, Json.JArr(Chunk.from(List.empty[Json])))
                        Json.JObj(fields.updated(field, spliceItems(child, rest, items)))
                    case _ => target
            case (index: Int) :: rest =>
                target match
                    case Json.JArr(existing) if index >= 0 && index < existing.size =>
                        val updated = spliceItems(existing(index), rest, items)
                        Json.JArr(Chunk.from(existing.toList.updated(index, updated)))
                    case _ => target
            case Nil => target

    /** Field-union two JSON values: objects merge key-wise (recursing on shared
      * object keys, `patch` winning on scalars/lists); anything else is replaced.
      */
    private def merge(base: Json, patch: Json): Json = (base, patch) match
        case (Json.JObj(a), Json.JObj(b)) =>
            val merged = (a.keySet ++ b.keySet).iterator.map { key =>
                val value = (a.get(key), b.get(key)) match
                    case (Some(av), Some(bv)) => merge(av, bv)
                    case (_, Some(bv))        => bv
                    case (Some(av), _)        => av
                    case _                    => Json.JNull
                key -> value
            }.toMap
            Json.JObj(merged)
        case (_, replacement) => replacement
end JsonPath
