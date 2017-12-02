package com.kirbydee.utils

import scala.language.implicitConversions

/**
  * Predicate extension, used to easily chain together predicate functions.
  */
object Predicate {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        // implicit conversion between (A => Boolean) to a Predicate case class
        implicit def implicitPredicate[A](p: A => Boolean): Predicate[A] =
            Predicate(p)
    }
    object Implicits extends Implicits
}

/**
  * The case class used for extension methods.
  *
  * @param self The predicate function itself
  * @tparam A The type of input of the predicate function
  */
case class Predicate[A](self: A => Boolean) {

    /**
      * Chain together 2 predicate functions with AND.
      *
      * @param other The second predicate function
      * @return The chained AND predicate function
      */
    def and(other: A => Boolean): A => Boolean = a =>
        self(a) && other(a)

    /**
      * Chain together 2 predicate functions with OR.
      *
      * @param other The second predicate function
      * @return The chained OR predicate function
      */
    def or(other: A => Boolean): A => Boolean = a =>
        self(a) || other(a)

    /**
      * Negate the predicate function.
      *
      * @return The negated predicate function
      */
    def unary_! : A => Boolean = a =>
        !self(a)
}
