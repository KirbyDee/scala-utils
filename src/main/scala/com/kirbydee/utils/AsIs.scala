package com.kirbydee.utils

import org.joda.time.DateTime
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

import scala.annotation.implicitNotFound
import scala.language.{implicitConversions, postfixOps}
import scala.util.Try

object AsIs {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /*
         * val / types assignments for easier access
         */
        implicit val asIsString = new AsIs[String] {
            override def as(s: String): String = s
        }
        implicit val asIsBoolean = new AsIs[Boolean] {
            override def as(s: String): Boolean = s.toBoolean
        }
        implicit val asIsByte = new AsIs[Byte] {
            override def as(s: String): Byte = s.toByte
        }
        implicit val asIsShort = new AsIs[Short] {
            override def as(s: String): Short = s.toShort
        }
        implicit val asIsInt = new AsIs[Int] {
            override def as(s: String): Int = s.toInt
        }
        implicit val asIsLong = new AsIs[Long] {
            override def as(s: String): Long = s.toLong
        }
        implicit val asIsLongList = new AsIs[List[Long]] {
            override def as(s: String): List[Long] = s.split(",").toList.map(_.toLong)
        }
        implicit val asIsFloat = new AsIs[Float] {
            override def as(s: String): Float = s.toFloat
        }
        implicit val asIsDouble = new AsIs[Double] {
            override def as(s: String): Double = s.toDouble
        }
        implicit val asIsDateTime = new AsIs[DateTime] {
            override def as(s: String): DateTime = DateTime parse s
        }
        implicit val asIsJs = new AsIs[JsValue] {
            override def as(s: String): JsValue = Json parse s
        }
        implicit val asIsJsObj = new AsIs[JsObject] {
            override def as(s: String): JsObject = (asIsJs as s).asInstanceOf[JsObject]
        }
        implicit val asIsJsArr = new AsIs[JsArray] {
            override def as(s: String): JsArray = (asIsJs as s).asInstanceOf[JsArray]
        }
        implicit val asIsExtension = new AsIs[Extension] {
            override def as(s: String): Extension = (ExtensionUtils from s).get
        }

        /**
          * Converter implicit class Helper
          *
          * @param s The string to convert
          */
        implicit class ConverterHelper(s: String) {

            /**
              * Checks, if the given string is actually of type T or not
              *
              * @return True, if the string given is a boolean, false otherwise
              */
            @inline def is[T](implicit converter: AsIs[T]): Boolean =
                converter is s

            /**
              * Converts string to given type.
              *
              * @return The converted Type
              */
            @inline def as[T](implicit converter: AsIs[T]): T =
                converter as s

            /**
              * Converts string to given type.
              *
              * @return The converted Type
              */
            @inline def asOpt[T](implicit converter: AsIs[T]): Option[T] =
                converter asOpt s
        }
    }
    object Implicits extends Implicits
}

@implicitNotFound(msg = "\nNo matching implicit AsIs found of Type [${T}].\nSee default implementations under com.kirbydee.utils.AsIs.Implicits\n")
trait AsIs[T] {

    /**
      * Converts given string to type T.
      *
      * @param s The string to convert
      * @return Input string as type T
      */
    def as(s: String): T

    /**
      * Converts given string to type T.
      *
      * @param s The string to convert
      * @return Input string as type Option[T]
      */
    def asOpt(s: String): Option[T] =
        asTry(s) toOption

    /**
      * Checks, if given string is actually of type T or not
      *
      * @param s The string to parse
      * @return True, if the string given is of type T, false otherwise
      */
    def is(s: String): Boolean =
        asTry(s) isSuccess

    /**
      * Tries to Convert given string to type T.
      *
      * @param s The string to convert
      * @return Input string as type Try[T]
      */
    private def asTry(s: String): Try[T] =
        Try(as(s))
}
