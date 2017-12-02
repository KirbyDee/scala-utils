package com.kirbydee.ws

import play.api.libs.ws.{WSRequest, WSResponse}
import com.kirbydee.utils.Implicits._

import scala.concurrent.{ExecutionContext, Future}

object Run {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * Implicit Helper class for a WebSocket Request being processed.
          *
          * @param request The request to process
          */
        implicit class WSRequestHelper(request: WSRequest) {

            /**
              * Executes the WebSocket Request which returns the future of a WebSocket Response.
              *
              * @param executor The implicit Execution context to run with
              * @return The Future of a WebSocket Response
              */
            @inline def |>(implicit executor: ExecutionContext): Future[WSResponse] =
                Run run request

            /**
              * Executes the WebSocket Request which returns the mapped future of a WebSocket Response.
              *
              * @param f The function to map the response after processing the request
              * @param executor The implicit Execution context to run with
              * @return The Future of the mapped WebSocket Response
              */
            @inline def |>[M](f: WSResponse => M)(implicit executor: ExecutionContext): Future[M] =
                |> > f
        }
    }
    object Implicits extends Implicits

    /**
      * Executes the WebSocket Request which returns the future of a WebSocket Response.
      *
      * @param request The request to process
      * @param executor The implicit Execution context to run with
      * @return The Future of a WebSocket Response
      */
    def run(request: WSRequest)(implicit executor: ExecutionContext): Future[WSResponse] =
        request.execute()
}