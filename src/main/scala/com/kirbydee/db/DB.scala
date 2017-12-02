package com.kirbydee.db

import play.api.db.slick.HasDatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.lifted.RunnableCompiled
import com.kirbydee.utils.Implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

trait DB extends HasDatabaseConfigProvider[JdbcProfile] {
    import dbConfig.driver.api._

    /**
      * Runs a Compiled Query.
      *
      * @param queryCompiled The Compiled Query to run on the database
      * @param executor The execution context to run with
      * @return The Future of the list of Items returned by the Compiled Query
      */
    def run[T, B](queryCompiled: RunnableCompiled[T, Seq[B]])(implicit executor: ExecutionContext): Future[List[B]] =
        run(queryCompiled result) >(_.toList)

    /**
      * Runs a Compiled Query, but only returns the first element.
      *
      * @param queryCompiled The Compiled Query to run on the database
      * @param executor The execution context to run with
      * @return The Future of the first Item returned by the Compiled Query
      */
    def runHead[T, B, P](queryCompiled: RunnableCompiled[T, Seq[B]])(implicit executor: ExecutionContext): Future[Option[B]] =
        run(queryCompiled.result) >(_.headOption)

    /**
      * Runs a Query.
      *
      * @param query The Query to run on the database
      * @param executor The execution context to run with
      * @return The Future of the list of Items returned by the Query
      */
    def run[T, B](query: Query[T, B, Seq])(implicit executor: ExecutionContext): Future[List[B]] =
        run(query result) >(_.toList)

    /**
      * Runs a Query, but only returns the first element.
      *
      * @param query The Query to run on the database
      * @param executor The execution context to run with
      * @return The Future of the first Item returned by the Query
      */
    def runHead[T, B](query: Query[T, B, Seq])(implicit executor: ExecutionContext): Future[Option[B]] =
        run(query.result headOption)

    /**
      * Runs a lifted Column.
      *
      * @param rep The lifted column
      * @param executor The execution context to run with
      * @tparam S The type of the column
      * @return The Future of the item inside of the column
      */
    def run[S](rep: Rep[S])(implicit executor: ExecutionContext): Future[S] =
        run(rep result)

    /**
      * Run an action to get data from db.
      *
      * @param action The action to run
      * @param executor The execution context to run with
      * @tparam A Type of the return Future
      * @return The Future of the data returned by the database query
      */
    def run[A](action: DBIO[A])(implicit executor: ExecutionContext): Future[A] =
        db run action
}