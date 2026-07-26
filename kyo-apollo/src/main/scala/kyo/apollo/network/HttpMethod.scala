package kyo.apollo.network

/** The HTTP method used to send a GraphQL operation.
  *
  * apollo-kotlin supports exactly two: `Get` (operation encoded as URL query
  * params — cacheable and required for GET-based APQ) and `Post` (the default —
  * a JSON body). The [[kyo.apollo.network.http.HttpRequestComposer]] (Task 3) reads
  * [[ApolloRequest.httpMethod]] to decide which shape to emit.
  */
enum HttpMethod derives CanEqual:
    case Get, Post
