package com.kirbydee.utils

import scala.language.implicitConversions

object Ternary {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        // Implicit Ternary assignment
        implicit def implicitTernary(b: Boolean): Ternary = Ternary(b)
    }
    object Implicits extends Implicits
}

/**
  * Ternary class used to create a Ternary Operator.
  *
  * @param b The boolean used for the Ternary Operator
  */
case class Ternary(b: Boolean) {

    /**
      * Creates the TRUE path of the Ternary Operator.
      *
      * @param t The TRUE function applied if the boolean of the Ternary Operator is TRUE.
      * @tparam X The type of return value of the TRUE path
      * @return Either t if b is TRUE or f if b is FALSE
      */
    @inline def ?[X](t: => X) = new {

        /**
          * Creates the FALSE path of the Ternary Operator.
          *
          * @param f The FALSE function applied if the boolean of the Ternary Operator is FALSE.
          * @tparam Y The type of return value of the FALSE path (needs to be subtype of X)
          * @return Either t if b is TRUE or f if b is FALSE
          */
        @inline def |[Y >: X](f: => Y): Y = if(b) t else f

        /**
          * No FALSE path to supply.
          * Executes the TRUE path if needed, else nothing.
          *
          * @return Unit
          */
        @inline def end = if(b) t else ()
    }

    /**
      * Creates the FALSE path of the Ternary Operator.
      *
      * @param f The FALSE function applied if the boolean of the Ternary Operator is FALSE.
      * @tparam X The type of return value of the FALSE path
      * @return Either f if b is FALSE or t if b is TRUE
      */
    @inline def !?[X](f: => X) = new {

        /**
          * Creates the TRUE path of the Ternary Operator.
          *
          * @param t The TRUE function applied if the boolean of the Ternary Operator is TRUE.
          * @tparam Y The type of return value of the TRUE path (needs to be subtype of X)
          * @return Either f if b is FALSE or t if b is TRUE
          */
        @inline def |[Y >: X](t: => Y): Y = if(b) t else f

        /**
          * No TRUE path to supply.
          * Executes the FALSE path if needed, else nothing.
          *
          * @return Unit
          */
        @inline def end = if(b) () else f
    }

    /**
      * Optional Ternary Operator. If given condition is true, we return a
      * Some(t), else a None.
      *
      * @param t The TRUE path inside the Some
      * @tparam X The type of return value of the TRUE path
      * @return Some(t) if b is true, None otherwise
      */
    @inline def ??[X](t: => X): Option[X] =
        if(b) Some(t) else None
}