package com.kirbydee.utils

import play.api.mvc.{Handler, RequestHeader}
import play.api.routing.{Router => PlayRouter}

object PartialFunctionUtils {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * Implicit helper class for partial functions.
          *
          * @param partial The partial function
          */
        implicit class RouterHelper[A, B](partial: PartialFunction[A, B]) {

            /**
              * Shortcut for "orElse" to append other partial functions together
              *
              * @param otherPartial The other function to test
              * @return Combined partial function
              */
            @inline def |[A1 <: A, B1 >: B](otherPartial: PartialFunction[A1, B1]): PartialFunction[A1, B1] =
                partial orElse otherPartial
        }
    }
    object Implicits extends Implicits
}