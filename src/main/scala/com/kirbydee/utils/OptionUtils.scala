package com.kirbydee.utils

import com.kirbydee.utils.FutureUtils.Implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{implicitConversions, postfixOps}

object OptionUtils {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * Option implicit class Helper
          *
          * @param o The Option
          */
        implicit class OptionHelper[T](o: Option[T]) extends ContainerHelper[T, Option](o) {

            /**
              * Gets the element in the Option.
              *
              * @return Option.get
              */
            override protected def get: T =
                o.get

            /**
              * Checks, if Option is defined or not.
              *
              * @return Option.isDefined
              */
            @inline override def ? : Boolean =
                o.isDefined

            /**
              * Runs the exist function on the Option
              *
              * @param f The exists function
              * @return Option.exist
              */
            @inline def ?(f: T => Boolean): Boolean =
                o exists f

            /**
              * Runs the contain function on the Option
              *
              * @param e The element
              * @return Option.contain
              */
            @inline def ?(e: T): Boolean =
                o contains e

            /**
              * Maps over the Option
              *
              * @param f The mapping function
              * @return Option.map
              */
            @inline override def >[B](f: (T) => B): Option[B] =
                o map f

            /**
              * FlatMaps over the Option
              *
              * @param f The mapping function
              * @return Option.flatMap
              */
            @inline override def >>[B](f: (T) => Option[B]): Option[B] =
                o flatMap f

            /**
              * Filters on given function
              *
              * @return Option.filter
              */
            @inline override def <~(f: (T) => Boolean): Option[T] =
                o filter f
        }

        /**
          * Option Future implicit class Helper
          *
          * @param ftOp The Option Future
          */
        implicit class OptionFutureHelper[A](ftOp: Option[Future[A]]) {

            /**
              * Transforms the Option Future to a Future Option
              *
              * @param executor The execution context to run with
              * @return The transformed Option
              */
            @inline def switch(implicit executor: ExecutionContext): Future[Option[A]] = ftOp match {
                case Some(future) => future > Some.apply
                case None         => Future(None)
            }
        }

        /**
          * Option List implicit class Helper
          *
          * @param ops The Option List
          */
        implicit class OptionListHelper[A](ops: Option[List[A]]) {

            /**
              * Gets the lit out of the option list. If the Option is not given, the list is empty.
              *
              * @return The list
              */
            @inline def list: List[A] =
                ops.toList.flatten
        }
    }
    object Implicits extends Implicits
}
