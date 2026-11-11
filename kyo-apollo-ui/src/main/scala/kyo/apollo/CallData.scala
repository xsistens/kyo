package kyo.apollo

import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.network.ApolloResponse
import kyo.apollo.runtime.ApolloCall

/** The data of a call's first response, failing with the error the response carries. The call's `execute` keeps a transport or
  * GraphQL error inside the response; the UI's query and poll paths take the data or an `Abort`, as they always did.
  */
private[apollo] object CallData:

    def of[D](call: ApolloCall[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): D < (Async & Abort[ApolloException]) =
        Scope.run(call.execute.map { response =>
            response.error match
                case Present(e) => Abort.fail(e)
                case Absent     =>
                    response.data match
                        case Present(d) => d
                        case Absent     =>
                            Abort.fail(DefaultApolloException("the response carried neither data nor an error"))
        })

end CallData
