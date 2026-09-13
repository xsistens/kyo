package kyo.apollo.api

/** Helpers for the `@defer` / `@stream` incremental-delivery directives. */
object Defer:

    /** True if `rootField`'s selection tree contains any `@defer`ed fragment or
      * `@stream`ed field — i.e. the operation uses incremental delivery. Drives the
      * multipart `Accept` header ([[kyo.apollo.network.http.HttpRequestComposer]])
      * and the streaming transport path ([[kyo.apollo.interceptor.NetworkInterceptor]]).
      */
    def has(rootField: CompiledField): Boolean =
        hasIn(rootField.selections)

    private def hasIn(selections: List[CompiledSelection]): Boolean =
        selections.exists {
            case field: CompiledField       => field.stream.isDefined || hasIn(field.selections)
            case fragment: CompiledFragment => fragment.defer.isDefined || hasIn(fragment.selections)
        }
end Defer
