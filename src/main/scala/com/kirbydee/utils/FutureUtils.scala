package com.kirbydee.utils

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{implicitConversions, postfixOps}

object FutureUtils {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * Future implicit class Helper
          *
          * @param ft The Future
          */
        implicit class FutureHelper[T](ft: Future[T]) {

            /**
              * Checks, if Future is completed or not.
              *
              * @return Future.isCompleted
              */
            @inline def ? : Boolean =
                ft.isCompleted

            /**
              * Maps over the Future
              *
              * @param f The mapping function
              * @param executor The implicit Execution context to run with
              * @return Future.map
              */
            @inline def >[A](f: T => A)(implicit executor: ExecutionContext): Future[A] =
                ft map f

            /**
              * FlatMaps over the Future
              *
              * @param f The mapping function
              * @param executor The implicit Execution context to run with
              * @return Future.flatMap
              */
            @inline def >>[A](f: T => Future[A])(implicit executor: ExecutionContext): Future[A] =
                ft flatMap f

            /**
              * Maps over the Future
              *
              * @param a The independent value to map to
              * @param executor The implicit Execution context to run with
              * @return Future.map
              */
            @inline def >>>[A](a: => A)(implicit executor: ExecutionContext): Future[A] =
                > (_ => a)

            /**
              * Filters on given function
              *
              * @param f The function to filter on
              * @param executor The implicit Execution context to run with
              * @return Future.filter
              */
            @inline def <~(f: T => Boolean)(implicit executor: ExecutionContext): Future[T] =
                ft filter f

            /**
              * Fallbacks on given function. It is automatically surrounded by a Future.
              *
              * @param f The fallback function
              * @param executor The implicit Execution context to run with
              * @return fallbackTo with Future of f
              */
            @inline def fallback[U >: T](f: => U)(implicit executor: ExecutionContext): Future[U] =
                ft recover { case _ => f }
        }

        /**
          * List Future implicit class Helper
          *
          * @param l The list with future inside
          */
        implicit class ListFutureHelper[T](l: List[Future[T]]) {

            /**
              * Puts the list of Futures together.
              *
              * @param executor The implicit Execution Context for handling futures
              * @return Future.sequence
              */
            @inline def sequence(implicit executor: ExecutionContext): Future[List[T]] =
                Future sequence l
        }

        /**
          * Future of Future implicit class Helper
          *
          * @param f The future of future
          */
        implicit class FutureFutureHelper[T](f: Future[Future[T]]) {

            /**
              * Flattens the future of future
              *
              * @param executor The implicit Execution Context for handling futures
              * @return Future.flatMap(identity)
              */
            @inline def flatten(implicit executor: ExecutionContext): Future[T] =
                f >> identity
        }
    }
    object Implicits extends Implicits
}
