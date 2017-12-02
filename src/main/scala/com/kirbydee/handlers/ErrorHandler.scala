package com.kirbydee.handlers

import com.kirbydee.logr.Logr
import play.api.http.HttpErrorHandler
import play.api.libs.json.{JsString, Json}
import play.api.mvc.Results._
import play.api.mvc._
import com.kirbydee.utils.Implicits._

import scala.concurrent._

class ErrorHandler extends HttpErrorHandler with Logr {

    /**
      * When a client error happens.
      *
      * @param request The request which caused the client error
      * @param statusCode The status code of the error
      * @param message The message of the error
      * @return Future result of the request
      */
    override def onClientError(request: RequestHeader, statusCode: Int, message: String): Future[Result] = Future successful {
        (statusCode, message) match {
            case (_, NonEmpty()) => clientErrorResult(request, statusCode, message)
            case (401, _)        => clientErrorResult(request, statusCode, "Unauthorized access")
            case (404, _)        => clientErrorResult(request, statusCode, "Not Found")
            case _               => Status(statusCode)
        }
    }

    private def clientErrorResult(request: RequestHeader, statusCode: Int, message: String): Result =
        new Status(statusCode)(Json.obj(
            "errorMessage" -> message,
            "requestURI"   -> request.uri,
            "method"       -> request.method
        ))

    /**
      * When a server error happens.
      *
      * @param request The request which caused the server error
      * @param exception The exception which caused the error
      * @return Future result of the request
      */
    override def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = Future successful {
        logRequestError(request, exception.getMessage, Some(exception))
        InternalServerError {
            Json.obj {
                ("errorMessage", JsString(exception.getMessage))
            }
        }
    }

    /**
      * Logs the request with the given error message and optional exception that was thrown.
      *
      * @param request The request which caused the error
      * @param message The message of the error
      * @param exceptionOp The optional error that has been thrown
      */
    private def logRequestError(request: RequestHeader, message: String, exceptionOp: Option[Throwable] = None): Unit = {
        val errorMessage = s"${requestToString(request)}: $message"
        exceptionOp match {
            case Some(exception) => error(s"$errorMessage\n${exception.getMessage}", exception)
            case None            => error(errorMessage)

        }
    }

    /**
      * Maps the request to a readable string.
      *
      * @param request The request to String
      * @return "request.method request.uri"
      */
    private def requestToString(request: RequestHeader): String =
        s"${request.method} ${request.uri}"
}
