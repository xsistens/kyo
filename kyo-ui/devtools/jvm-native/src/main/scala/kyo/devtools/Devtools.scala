package kyo.devtools

/** The server-side build of [[DevtoolsApi]].
  *
  * `runMount` is deliberately absent rather than present-and-inert: there is no document to draw on here, and
  * a wrapper that compiled and then quietly measured nothing is exactly the kind of half-working a devtool
  * cannot afford. Server-push apps use `Devtools.runHandlers`.
  */
object Devtools extends DevtoolsApi
