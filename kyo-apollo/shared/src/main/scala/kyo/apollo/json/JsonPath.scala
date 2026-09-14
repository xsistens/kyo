package kyo.apollo.json

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present

/** Walks and splices a rooted response path (`["a", 0, "b"]`, mixing field names
  * and list indices) into a [[Json]] tree — the addressing an incremental-delivery
  * (`@defer`) patch uses to merge into the accumulated response. Field-unions
  * objects (later wins), matching the normalized cache's record merge.
  *
  * A path that does not fit the tree is `Absent`, never a silent no-op: the caller
  * must be able to tell a patch that landed from one that was dropped.
  */
object JsonPath:

    private given CanEqual[String | Int, String | Int] = CanEqual.derived

    /** Parse a wire `path` array into `String` (field) / `Int` (index) segments.
      * `Absent` unless `json` is an array whose every segment is a string or a JSON
      * number with an exact, non-negative `Int` value.
      */
    def parse(json: Json): Maybe[Chunk[String | Int]] = json match
        case Json.JArr(items) =>
            items.foldLeft(Maybe(Chunk.empty[String | Int])) { (path, item) =>
                path.flatMap(segments => segment(item).map(segments.append))
            }
        case _ => Absent

    private def segment(json: Json): Maybe[String | Int] = json match
        case Json.JStr(name) => Present(name)
        case other           => Json.integral(other).filter(i => i >= 0 && i.isValidInt).map(_.toInt)

    /** Splice `patch` into `target` at `path`. An empty path merges at the root. A
      * field segment into a non-object, or an index segment outside the list (or into
      * a non-list), is `Absent`.
      */
    def splice(target: Json, path: Chunk[String | Int], patch: Json): Maybe[Json] =
        path.headMaybe match
            case Absent => Present(merge(target, patch))
            case Present(field: String) =>
                target match
                    case Json.JObj(fields) =>
                        val child = fields.getOrElse(field, Json.JObj(Map.empty))
                        splice(child, path.dropLeft(1), patch).map(spliced => Json.JObj(fields.updated(field, spliced)))
                    case _ => Absent
            case Present(index: Int) =>
                target match
                    case Json.JArr(items) if index >= 0 && index < items.size =>
                        splice(items(index), path.dropLeft(1), patch).map(spliced => Json.JArr(replaced(items, index, spliced)))
                    case _ => Absent

    /** Splice `@stream`ed `items` into the list at `path`, whose final segment is the
      * start index the items go at. The normal case appends at the tail (`index ==
      * size`) in O(items); an in-range index overwrites from there and grows past the
      * end if needed. A non-terminal index segment recurses into a list element (a
      * `@stream` nested inside a list). `Absent` for a start index past the tail (a gap
      * the protocol never sends), a negative index, a path that does not end in an
      * index, or any segment that does not fit the tree.
      */
    def spliceItems(target: Json, path: Chunk[String | Int], items: Chunk[Json]): Maybe[Json] =
        path.headMaybe match
            case Present(index: Int) if path.size == 1 =>
                target match
                    case Json.JArr(existing) if index >= 0 && index == existing.size =>
                        // `Chunk.append` is O(1); `Chunk.concat` would copy the whole list.
                        Present(Json.JArr(items.foldLeft(existing)(_.append(_))))
                    case Json.JArr(existing) if index >= 0 && index < existing.size =>
                        Present(Json.JArr(existing.take(index).concat(items).concat(existing.dropLeft(index + items.size))))
                    case _ => Absent
            case Present(field: String) =>
                target match
                    case Json.JObj(fields) =>
                        val child = fields.getOrElse(field, Json.JArr(Chunk.empty))
                        spliceItems(child, path.dropLeft(1), items).map(spliced => Json.JObj(fields.updated(field, spliced)))
                    case _ => Absent
            case Present(index: Int) =>
                target match
                    case Json.JArr(existing) if index >= 0 && index < existing.size =>
                        spliceItems(existing(index), path.dropLeft(1), items)
                            .map(spliced => Json.JArr(replaced(existing, index, spliced)))
                    case _ => Absent
            case _ => Absent

    private def replaced(items: Chunk[Json], index: Int, value: Json): Chunk[Json] =
        items.take(index).append(value).concat(items.dropLeft(index + 1))

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
