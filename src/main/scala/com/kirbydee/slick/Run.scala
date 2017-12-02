package com.kirbydee.slick

import com.kirbydee.db.DB
import com.kirbydee.utils.Implicits._
import slick.lifted.RunnableCompiled

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{implicitConversions, postfixOps, reflectiveCalls}


/**
  * The Slick Run object
  */
object Run extends slick.lifted.Aliases {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * Helper trait for Run implicit classes.
          */
        trait RunHelper {

            // type of Output
            type O

            /**
              * Runs the Output.
              *
              * @param db Implicit Database
              * @param executor The execution context to run with
              * @return The mapped Future
              */
            @inline def |>(implicit db: DB, executor: ExecutionContext): Future[O]

            /**
              * Runs the Output and maps directly.
              *
              * @param f The function applied to the output of the run
              * @param db Implicit Database
              * @param executor The execution context to run with
              * @return The mapped Future
              */
            @inline def |>[M](f: O => M)(implicit db: DB, executor: ExecutionContext): Future[M] =
                |> > f

            /**
              * Runs the Output and maps directly.
              *
              * @param f The function applied to the output of the run
              * @param db Implicit Database
              * @param executor The execution context to run with
              * @return The mapped Future
              */
            @inline def |>>[M](f: O => Future[M])(implicit db: DB, executor: ExecutionContext): Future[M] =
                |> >> f
        }

        /**
          * Helper trait for Run implicit classes, that have a runHead.
          */
        trait RunListHelper extends RunHelper {

            // type inside of the list
            type A

            // type of Output
            override type O = List[A]

            /**
              * Runs the Output, but only returns the first element of the list
              *
              * @param db Implicit Database
              * @param executor The execution context to run with
              * @return The first element B out of the output given as a Future
              */
            @inline def !>(implicit db: DB, executor: ExecutionContext): Future[Option[A]]

            /**
              * Runs the Output and maps on all the elements in the list.
              *
              * @param f The function applied to the elements in the output list of the run
              * @param db Implicit Database
              * @param executor The execution context to run with
              * @return The mapped Future
              */
            @inline def ||>[M](f: A => M)(implicit db: DB, executor: ExecutionContext): Future[List[M]] =
                |> > (_ > f)

            /**
              * Runs the Output and maps on all the elements in the list.
              *
              * @param f The function applied to the elements in the output list of the run
              * @param db Implicit Database
              * @param executor The execution context to run with
              * @return The mapped Future
              */
            @inline def ||>>[M](f: A => Future[M])(implicit db: DB, executor: ExecutionContext): Future[List[M]] =
                |> >> (l => Future sequence (l > f))

            /**
              * Runs the Output, but only returns the first element of the list, and maps directly.
              *
              * @param f The function applied to the output of the run, if the element exists
              * @param db Implicit Database
              * @param executor The execution context to run with
              * @return The mapped first element B out of the output given as a Future
              */
            @inline def !>[M](f: A => M)(implicit db: DB, executor: ExecutionContext): Future[Option[M]] =
                !> > (_ > f)

            /**
              * Runs the Output, but only returns the first element of the list, and maps directly.
              *
              * @param f The function applied to the output of the run, if the element exists
              * @param db Implicit Database
              * @param executor The execution context to run with
              * @return The mapped first element B out of the output given as a Future
              */
            @inline def !!>[M](f: A => Option[M])(implicit db: DB, executor: ExecutionContext): Future[Option[M]] =
                !> > (_ >> f)

            /**
              * Runs the Output, but only returns the first element of the list, and maps directly.
              *
              * @param f The function applied to the output of the run
              * @param db Implicit Database
              * @param executor The execution context to run with
              * @return The mapped first element B out of the output given as a Future
              */
            @inline def !>>[M](f: A => Future[M])(implicit db: DB, executor: ExecutionContext): Future[Option[M]] =
                !> >> (_ > f switch)

            /**
              * Runs the Output, but only returns the first element of the list, and maps directly.
              *
              * @param f The function applied to the output of the run, if the element exists
              * @param db Implicit Database
              * @param executor The execution context to run with
              * @return The mapped first element B out of the output given as a Future
              */
            @inline def !!>>[M](f: A => Future[Option[M]])(implicit db: DB, executor: ExecutionContext): Future[Option[M]] =
                !> >> (a => (a > f).switch >(_.flatten))

            /**
              * Runs the Output and maps the None case directly, and lets the Some case the same.
              *
              * @param f The function applied to the None case of the output of the run
              * @param db Implicit Database
              * @param executor The execution context to run with
              * @return The mapped first element B out of the output given as a Future
              */
            @inline def ?>(f: => A)(implicit db: DB, executor: ExecutionContext): Future[A] = !> map {
                case Some(future) => future
                case None         => f
            }

            /**
              * Runs the Output and maps the None case directly, and lets the Some case the same.
              *
              * @param f The function applied to the None case of the output of the run
              * @param db Implicit Database
              * @param executor The execution context to run with
              * @return The mapped first element B out of the output given as a Future
              */
            @inline def ?>>[M](f: => Future[A])(implicit db: DB, executor: ExecutionContext): Future[A] = !> >> {
                case Some(future) => Future(future)
                case None         => f
            }
        }

        /**
          * Helper function for Run for compiled queries.
          *
          * @param queryCompiled The compiled query to run
          * @tparam T The type of Table
          * @tparam B The type inside the Table
          */
        implicit class RunQueryCompiledHelper[T, B](queryCompiled: RunnableCompiled[T, Seq[B]]) extends RunListHelper {

            // type inside of the list
            override type A = B

            /**
              * Runs the Compiled Query.
              *
              * @param db      Implicit Database
              * @param executor The execution context to run with
              * @return The List of B out of the compiled query given as a Future
              */
            @inline override def |>(implicit db: DB, executor: ExecutionContext): Future[O] =
                Run(queryCompiled)

            /**
              * Runs the Compiled Query, but only returns the first element of the list
              *
              * @param db      Implicit Database
              * @param executor The execution context to run with
              * @return The first element B out of the compiled query given as a Future
              */
            @inline override def !>(implicit db: DB, executor: ExecutionContext): Future[Option[A]] =
                Run head queryCompiled
        }

        /**
          * Helper function for Run for queries.
          *
          * @param query The query to run
          * @tparam T The type of Table
          * @tparam B The type inside the Table
          */
        implicit class RunQueryHelper[T, B](query: Query[T, B, Seq]) extends RunListHelper {

            // type inside of the list
            override type A = B

            /**
              * Runs the Query.
              *
              * @param db      Implicit Database
              * @param executor The execution context to run with
              * @return The List of B out of the query given as a Future
              */
            @inline override def |>(implicit db: DB, executor: ExecutionContext): Future[O] =
                Run(query)

            /**
              * Runs the Query, but only returns the first element of the list
              *
              * @param db      Implicit Database
              * @param executor The execution context to run with
              * @return The first element B out of the query given as a Future
              */
            @inline override def !>(implicit db: DB, executor: ExecutionContext): Future[Option[A]] =
                Run head query
        }

        /**
          * Helper function for Run for actions.
          *
          * @param action The action to run
          * @tparam A The type of Action
          */
        implicit class RunActionHelper[A](action: DBIO[A]) extends RunHelper {

            // type of Output
            override type O = A

            /**
              * Runs the Action.
              *
              * @param db      Implicit Database
              * @param executor The execution context to run with
              * @return The Future action
              */
            @inline override def |>(implicit db: DB, executor: ExecutionContext): Future[O] =
                Run(action)
        }
    }
    object Implicits extends Implicits

    /**
      * Runs the compiled query
      *
      * @param q The compiled query to run
      * @param db Implicit Database
      * @param executor The execution context to run with
      * @tparam T The type of Table
      * @tparam B The type inside the Table
      * @return The List of B out of the compiled query given as a Future
      */
    def apply[T, B, P](q: RunnableCompiled[T, Seq[B]])(implicit db: DB, executor: ExecutionContext): Future[List[B]] =
        db run q

    /**
      * Runs the compiled query, but only returns the first element.
      *
      * @param q The compiled query to run
      * @param db Implicit Database
      * @param executor The execution context to run with
      * @tparam T The type of Table
      * @tparam B The type inside the Table
      * @return The List of B out of the compiled query given as a Future
      */
    def head[T, B](q: RunnableCompiled[T, Seq[B]])(implicit db: DB, executor: ExecutionContext): Future[Option[B]] =
        db runHead q

    /**
      * Runs the query
      *
      * @param q The query to run
      * @param db Implicit Database
      * @param executor The execution context to run with
      * @tparam T The type of Table
      * @tparam B The type inside the Table
      * @return The List of B out of the query given as a Future
      */
    def apply[T, B](q: Query[T, B, Seq])(implicit db: DB, executor: ExecutionContext): Future[List[B]] =
        db run q

    /**
      * Runs the query, but only returns the first element.
      *
      * @param q The query to run
      * @param db Implicit Database
      * @param executor The execution context to run with
      * @tparam T The type of Table
      * @tparam B The type inside the Table
      * @return The List of B out of the query given as a Future
      */
    def head[T, B](q: Query[T, B, Seq])(implicit db: DB, executor: ExecutionContext): Future[Option[B]] =
        db runHead q

    /**
      * Runs the Action
      *
      * @param a The action to run
      * @param db Implicit Database
      * @param executor The execution context to run with
      * @tparam A The type of Action
      * @return The Future action
      */
    def apply[A](a: DBIO[A])(implicit db: DB, executor: ExecutionContext): Future[A] =
        db run a
}