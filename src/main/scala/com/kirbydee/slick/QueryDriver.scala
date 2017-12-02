package com.kirbydee.slick

import slick.driver.{MySQLDriver, PostgresDriver}

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}

// TODO: cannot use driver as dynamic parameter for some reason.. a lot of code duplication here
//// MySQL Query
//object QueryMySQL extends QueryDriver(MySQLDriver)

// Postgres Query
//object QueryPostgres extends QueryDriver(PostgresDriver)

/**
  * The Query object
  */
trait QueryDriver

/**
  * Postgres Query
  */
object QueryPostgres extends QueryDriver {
    import PostgresDriver.api._

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * Helper function for a Table Query.
          *
          * @param query The Table Query
          * @tparam T The type of Table
          * @tparam B The type inside the Table
          */
        implicit class TableQueryHelper[T <: Table[B], B](query: Query[T, B, Seq]) {

            /**
              * Applies the slick filter functions (Table -> Boolean) on the query.
              *
              * @param f The first slick filter function
              * @param fs The slick filter functions
              * @return A new filtered Table Query
              */
            @inline def <<~(f: T => Rep[Boolean], fs: (T => Rep[Boolean])*): Query[T, B, Seq] =
                <<~(f +: fs.toList)

            /**
              * Applies the slick filter functions (Table -> Boolean) on the query.
              *
              * @param fs The slick filter functions
              * @return A new filtered Table Query
              */
            @inline def <<~(fs: List[(T => Rep[Boolean])]): Query[T, B, Seq] = fs.foldLeft(query: Query[T, B, Seq]) {
                case (q, f) => q <~ f
            }

            /**
              * Applies the slick filter function (Table -> Boolean) on the query.
              *
              * @param f The slick filter function
              * @return A new filtered Table Query
              */
            @inline def <~(f: T => Rep[Boolean]): Query[T, B, Seq] =
                query filter f
        }

        /**
          * Helper function for a slick filter function (Table -> Boolean).
          *
          * @param f The filter function
          * @tparam T The type of Table
          * @tparam B The type inside the Table
          */
        implicit class FilterHelper[T <: Table[B], B](f: T => Rep[Boolean]) {

            /**
              * AND function on the first and second slick filter function.
              *
              * @param f2 The second filter function
              * @return A new filter function, that is the combination of the two
              */
            @inline def &&(f2: T => Rep[Boolean]): T => Rep[Boolean] = t =>
                f(t) && f2(t)

            /**
              * OR function on the first and second slick filter function.
              *
              * @param f2 The second filter function
              * @return A new filter function, that is the combination of the two
              */
            @inline def ||(f2: T => Rep[Boolean]): T => Rep[Boolean] = t =>
                f(t) || f2(t)

            /**
              * NOT function on the slick filter function.
              *
              * @return A new filter function, that is the negative of the given slick filter function
              */
            @inline def unary_! : T => Rep[Boolean] = t =>
                !f(t)
        }
    }
    object Implicits extends Implicits
}

/**
  * MySQL Query
  */
object QueryMySQL extends QueryDriver {
    import MySQLDriver.api._

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * Helper function for a Table Query.
          *
          * @param query The Table Query
          * @tparam T The type of Table
          * @tparam B The type inside the Table
          */
        implicit class TableQueryHelper[T <: Table[B], B](query: Query[T, B, Seq]) {

            /**
              * Applies the slick filter functions (Table -> Boolean) on the query.
              *
              * @param f The first slick filter function
              * @param fs The slick filter functions
              * @return A new filtered Table Query
              */
            @inline def <<~(f: T => Rep[Boolean], fs: (T => Rep[Boolean])*): Query[T, B, Seq] =
                <<~(f +: fs.toList)

            /**
              * Applies the slick filter functions (Table -> Boolean) on the query.
              *
              * @param fs The slick filter functions
              * @return A new filtered Table Query
              */
            @inline def <<~(fs: List[(T => Rep[Boolean])]): Query[T, B, Seq] = fs.foldLeft(query: Query[T, B, Seq]) {
                case (q, f) => q <~ f
            }

            /**
              * Applies the slick filter function (Table -> Boolean) on the query.
              *
              * @param f The slick filter function
              * @return A new filtered Table Query
              */
            @inline def <~(f: T => Rep[Boolean]): Query[T, B, Seq] =
                query filter f
        }

        /**
          * Helper function for a slick filter function (Table -> Boolean).
          *
          * @param f The filter function
          * @tparam T The type of Table
          * @tparam B The type inside the Table
          */
        implicit class FilterHelper[T <: Table[B], B](f: T => Rep[Boolean]) {

            /**
              * AND function on the first and second slick filter function.
              *
              * @param f2 The second filter function
              * @return A new filter function, that is the combination of the two
              */
            @inline def &&(f2: T => Rep[Boolean]): T => Rep[Boolean] = t =>
                f(t) && f2(t)

            /**
              * OR function on the first and second slick filter function.
              *
              * @param f2 The second filter function
              * @return A new filter function, that is the combination of the two
              */
            @inline def ||(f2: T => Rep[Boolean]): T => Rep[Boolean] = t =>
                f(t) || f2(t)

            /**
              * NOT function on the slick filter function.
              *
              * @return A new filter function, that is the negative of the given slick filter function
              */
            @inline def unary_! : T => Rep[Boolean] = t =>
                !f(t)
        }
    }
    object Implicits extends Implicits
}