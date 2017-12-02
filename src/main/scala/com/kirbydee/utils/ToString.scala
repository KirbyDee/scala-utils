package com.kirbydee.utils

import play.api.libs.json.{JsArray, JsObject, JsValue}

import scala.annotation.implicitNotFound
import scala.language.postfixOps

object ToString {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /*
         * val / types assignments for easier access
         */
        implicit val toStringString  = new ToString[String] {}
        implicit val toStringBoolean = new ToString[Boolean] {}
        implicit val toStringShort   = new ToString[Short] {}
        implicit val toStringInt     = new ToString[Int] {}
        implicit val toStringLong    = new ToString[Long] {}
        implicit val toStringFloat   = new ToString[Float] {}
        implicit val toStringDouble  = new ToString[Double] {}
        implicit val toStringJson    = new ToString[JsValue] {}
        implicit val toStringJsObj   = new ToString[JsObject] {}
        implicit val toStringJsArr   = new ToString[JsArray] {}
    }
    object Implicits extends Implicits
}

@implicitNotFound(msg = "\nNo matching implicit ToString found of Type [${T}].\nSee default implementations under com.logograb.utils.ToString.Implicits\n")
trait ToString[T] {

    /**
      * Parses the object of type T to String.
      *
      * @param t The object to parse
      * @return The parsed object as String
      */
    def parse(t: T): String =
        t.toString
}