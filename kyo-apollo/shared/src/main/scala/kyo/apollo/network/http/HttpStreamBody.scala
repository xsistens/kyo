package kyo.apollo.network.http

import kyo.*
import kyo.apollo.exception.HttpEngineFailure

/** The body of an [[HttpEngine.StreamResponse]]: either a fully-buffered string (the
  * default for non-streaming engines and non-multipart responses) or a live stream
  * of UTF-8 text chunks (a `multipart/mixed` incremental-delivery body). kyo-http
  * hands a live response body only inside `HttpClient.sendWith`'s continuation; the
  * seam hands it back to the transport as a lazy stream instead. A chunked body that
  * drops mid-stream aborts with the engine's [[HttpEngineFailure]].
  */
enum HttpStreamBody:
    case Buffered(text: String)
    case Chunked(chunks: Stream[String, Async & Scope & Abort[HttpEngineFailure]])
