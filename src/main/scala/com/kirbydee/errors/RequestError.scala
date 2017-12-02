package com.kirbydee.errors

import com.kirbydee.utils.FutureUtils.Implicits._

import scala.concurrent.{ExecutionContext, Future}

object RequestError {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /*
         * val / types assignments
         */
        val RequestError         = com.kirbydee.errors.RequestError
        val NotFoundRequest      = com.kirbydee.errors.NotFoundRequestError
        val UnauthorizedRequest  = com.kirbydee.errors.UnauthorizedRequestError
        val MalformedRequest     = com.kirbydee.errors.MalformedRequestError

        /**
          * Implicit Helper function for Future Optional Results, that may fail (is None),
          * and thus will create an client side error message.
          *
          * @param ft The Future that holds a Result or not
          * @tparam R The type of the Result
          */
        implicit class RequestErrorFutureOptionHelper[R](val ft: Future[Option[R]]) {

            /**
              * Throws an ClientError inside the future of the Optional Result, if it doesn't exist.
              *
              * @param error The error to show
              * @param executor The implicit Execution Context for handling futures
              * @return The Future returning the Result itself or the error if not existing
              */
            def %(error: RequestError)(implicit executor: ExecutionContext): Future[R] = ft > {
                case Some(r) => r
                case None    => throw error
            }
        }

        /**
          * Implicit Helper function for Future List Results, that may fail (is Empty),
          * and thus will create an client side error message.
          *
          * @param ft The Future that holds the Results or not
          * @tparam R The type of the Result
          */
        implicit class RequestErrorFutureListHelper[R](val ft: Future[List[R]]) {

            /**
              * Throws an ClientError inside the future of the List Results, if they don't exist.
              *
              * @param error The error to show
              * @param executor The implicit Execution Context for handling futures
              * @return The Future returning the Result itself or the error if not existing
              */
            def %(error: RequestError)(implicit executor: ExecutionContext): Future[List[R]] = ft > {
                case Nil => throw error
                case r   => r
            }
        }

        /**
          * Implicit Helper function for Optional Results, that may fail (is None),
          * and thus will create an client side error message.
          *
          * @param op The Option that holds a Result or not
          * @tparam R The type of the Result
          */
        implicit class RequestErrorOptionHelper[R](val op: Option[R]) {

            /**
              * Throws an ClientError inside he of the Optional Result, if it doesn't exist.
              *
              * @param error The error to show
              * @return The Result itself or the error if not existing
              */
            def %(error: RequestError): R = op match {
                case Some(r) => r
                case None    => throw error
            }
        }

        /**
          * Implicit Helper function for List Results, that may fail (is Empty),
          * and thus will create an client side error message.
          *
          * @param ls The List that holds a Result or not
          * @tparam R The type of the Result
          */
        implicit class RequestErrorListHelper[R](val ls: List[R]) {

            /**
              * Throws an ClientError inside he of the List Result, if it doesn't exist.
              *
              * @param error The error to show
              * @return The Result itself or the error if not existing
              */
            def %(error: RequestError): List[R] = ls match {
                case Nil => throw error
                case r   => r
            }
        }
    }
    object Implicits extends Implicits

    def apply(message: String, status: Int): RequestError =
        new RequestError(message, status)
}

class RequestError(val message: String, val status: Int) extends Exception(message)

/*
 * Malformed Request (400)
 */
object MalformedRequestError {

    def apply(message: String): MalformedRequestError =
        new MalformedRequestError(message)

    def missing(param: String): MalformedRequestError =
        new MalformedRequestError(missingText(param))

    def missing(param: String, params: String*): MalformedRequestError =
        MalformedRequestError.missing(param +: params.toList)

    def missing(params: List[String]): MalformedRequestError =
        new MalformedRequestError(missingText(params mkString " or "))

    /**
      * Missing request parameters test.
      */
    private def missingText(params: String): String =
        s"Missing parameter: $params"
}
class MalformedRequestError(override val message: String) extends RequestError(message, 400)

/*
 * Unauthorized Request (401)
 */
case object UnauthorizedRequestError extends RequestError("Unauthorized access", 401)

/*
 * Not Found Request (404)
 */
case class NotFoundRequestError(override val message: String) extends RequestError(message, 404)

/*
 * Not Found Request (500)
 */
case object InternalServerError extends RequestError("Internal Server Error", 500)