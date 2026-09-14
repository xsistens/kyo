package kyo.apollo.cache.normalized

import kyo.<
import kyo.Absent
import kyo.Chunk
import kyo.Frame
import kyo.Kyo
import kyo.Maybe
import kyo.Present
import kyo.Sync
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.FieldKey
import kyo.apollo.cache.normalized.api.Record
import kyo.apollo.cache.normalized.api.RecordValue
import kyo.apollo.json.Json

/** Development-time warnings about writes the cache cannot make sense of but is not
  * entitled to refuse. Handed to [[ApolloStore]] like the other policies, and
  * [[CacheDiagnostics.off]] by default.
  *
  * There is exactly one warning today, and it is the hazard that comes with storing an
  * object that has no identity: such an object is addressed by its INDEX under its
  * parent ([[kyo.apollo.cache.normalized.internal.Normalizer]]), so a write of a
  * reordered or shortened list merges element-wise into the slot the previous element
  * held. Fields the new occupant did not mention survive from the old one, and the read
  * that follows is structurally valid and semantically wrong — no miss, no error, just a
  * value belonging to two different objects at once.
  *
  * The cache cannot tell that from a legitimate in-place change of a list element:
  * telling them apart needs an identity, which is precisely what is missing. So this
  * warns rather than acts, and it is off unless asked for — a diagnostic that fires on
  * correct programs gets muted, and a muted diagnostic protects nobody. Apollo Client
  * makes the same call from the other side of the same problem, warning "Cache data may
  * be lost when replacing the X field of a Y object" and offering `merge: true` to
  * silence it.
  *
  * {{{
  * ApolloStore(MemoryCache(), diagnostics = CacheDiagnostics.toStdErr) // console.error in a browser
  * ApolloStore(MemoryCache(), diagnostics = CacheDiagnostics.to(log))  // or your own sink
  * }}}
  *
  * A value rather than a global switch: a global would be shared by every store in the
  * process, which is wrong for a library and untestable under a parallel suite.
  */
final case class CacheDiagnostics(sink: Maybe[String => Unit]):

    /** Whether anything is listening — checked before the diff work, so diagnostics that
      * are off cost one `Maybe` test per written record.
      */
    def enabled: Boolean = sink.isDefined

    /** The warnings for every field of `incoming` that CONTRADICTS a stored value on a
      * positionally-keyed record — the signature of a list whose elements moved.
      *
      * Three conditions, so the signal is not "a field changed": the record is
      * positionally keyed (its key ends in `.<digits>`, which only the normalizer's list
      * branch mints), the field already had a value, and the new value differs and is not
      * null. A field the incoming write merely ADDS is the ordinary disjoint-selection
      * case and says nothing; a field it repeats identically says nothing either.
      *
      * Pure, so a write can compute its warnings against the very state it commits to
      * (inside [[NormalizedCache.transact]], which may run more than once) and
      * [[report]] only those of the attempt that landed.
      */
    private[normalized] def positionalConflicts(existing: Record, incoming: Record): Chunk[String] =
        if !enabled || !CacheDiagnostics.isPositional(incoming.key) then Chunk.empty
        else
            val typename = existing.get(FieldKey.Typename) match
                case Present(RecordValue.Scalar(Json.JStr(t))) => s"'$t'"
                case _                                         => "this type"
            Chunk.from(incoming.fields).flatMap { (fieldKey, incomingValue) =>
                val old = existing.fields.get(fieldKey)
                val contradicted =
                    fieldKey != FieldKey.Typename &&
                        incomingValue != RecordValue.Null &&
                        old.exists(_ != incomingValue)
                if !contradicted then Chunk.empty
                else
                    Chunk(
                        s"kyo-apollo: the write to '${incoming.key.render}' replaced field '${fieldKey.render}' " +
                            s"(${old.get} -> $incomingValue) on a record addressed BY POSITION. If the " +
                            s"list was reordered or shortened, the fields this write did NOT mention " +
                            s"still belong to the element that used to sit here. Declare a cache " +
                            s"identity for $typename, or select the same field set from every " +
                            s"operation that writes it."
                    )
                end if
            }

    /** Hand `warnings` to the sink, in order (nothing when diagnostics are off). */
    private[normalized] def report(warnings: Chunk[String])(using Frame): Unit < Sync =
        if warnings.isEmpty then Kyo.unit
        else sink.fold(Kyo.unit)(emit => Sync.defer(warnings.foreach(emit)))
end CacheDiagnostics

object CacheDiagnostics:
    /** Nothing is reported. The default every store gets. */
    val off: CacheDiagnostics = CacheDiagnostics(Absent)

    /** Report through `f`. */
    def to(f: String => Unit): CacheDiagnostics = CacheDiagnostics(Present(f))

    /** Report to stderr — `console.error` on Scala.js, the channel Apollo Client's own
      * cache warnings use.
      */
    val toStdErr: CacheDiagnostics = to(msg => java.lang.System.err.println(msg))

    /** A key the normalizer's list branch minted: `<parent>.<field>.<index>`. An entity
      * key carries no dot unless a custom generator puts one there, in which case the
      * worst outcome is a spurious warning on a diagnostic the caller asked for.
      */
    private[normalized] def isPositional(cacheKey: CacheKey): Boolean =
        val key = cacheKey.render
        val i   = key.lastIndexOf('.')
        i > 0 && i < key.length - 1 && key.substring(i + 1).forall(_.isDigit)
    end isPositional
end CacheDiagnostics
