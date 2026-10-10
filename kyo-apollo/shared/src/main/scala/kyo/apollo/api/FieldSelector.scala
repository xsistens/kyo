package kyo.apollo.api

/** The typed handle every generated object-field selector implements: `Origin`
  * is the type declaring the field, `Leaf` is the type the field selects into
  * (the element type for list fields), and [[fieldName]] is the field's schema
  * name. Codegen makes each `` `field$sel` `` class extend this trait, so a
  * field can be handed to library configuration as a value that provably exists
  * on its type, instead of a raw string that fails silently when it drifts from
  * the schema. `ConnectionFieldPolicy.of` is the first consumer:
  *
  * {{{
  * ConnectionFieldPolicy.of(Playlist.tracks(), PlaylistTrackConnection.edges)
  * }}}
  */
trait FieldSelector[Origin, Leaf]:
    /** The field's schema name, as emitted by codegen. */
    def fieldName: String
end FieldSelector
