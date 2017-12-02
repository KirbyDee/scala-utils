package com.kirbydee.utils

import com.kirbydee.utils.AsIs.Implicits._
import com.kirbydee.utils.OptionUtils.Implicits._
import org.apache.commons.lang3.StringEscapeUtils
import org.joda.time.DateTime
import play.api.libs.json._

import scala.language.{implicitConversions, postfixOps}

object JsUtils {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /*
         * val / types assignments for easier access
         */
        type DateJson                     = com.kirbydee.utils.JsUtils.DateJson
        val ignoreRead:  Reads[None.type] = Reads.pure(None)
        val ignoreWrite: OWrites[Any]     = OWrites[Any](_ => Json.obj())

        /**
          * Implicitly casts a field to a Json Path.
          */
        implicit def fieldToPath(field: String): JsPath =
            __ \ field

        // Implicit map writes
        implicit def mapWrites: Writes[Map[String, Any]] = new Writes[Map[String, Any]] {

            // transform map to JsObject
            override def writes(o: Map[String, Any]): JsValue =
                Json.toJson(o mapValues toJsValue)
        }

        // Implicit tuple writes
        implicit def tupleWrites: Writes[(String, Any)] = new Writes[(String, Any)] {

            // transform tuple to JsObject
            override def writes(o: (String, Any)): JsValue = o match {
                case (key, value) => Json.obj(key -> toJsValue(value))
            }
        }

        /**
          * Lookup the field given.
          *
          * @param js JsValue
          * @param field The field to lookup
          * @return JsLookupResult with either has the field in it (existing) or not.
          */
        private def lookup(js: JsValue, field: String): JsLookupResult =
            js \ field

        /**
          * JsValue String implicit class Helper
          *
          * @param js JsValue
          */
        implicit class JsFieldHelper(js: JsValue) {

            /**
              * Sanitizes the Json.
              */
            @inline def sanitize: String =
                StringEscapeUtils.escapeEcmaScript(js.toString).replace("\\\"", "\"")

            /**
              * Gets the field as Int value, None if either not existing or not of type Int.
              *
              * @param field The field to lookup
              * @return Optional value of the field as type Int
              */
            @inline def int(field: String): Option[Int] =
                lookup(js, field).int

            /**
              * Gets the field as Long value, None if either not existing or not of type Long.
              *
              * @param field The field to lookup
              * @return Optional value of the field as type Long
              */
            @inline def long(field: String): Option[Long] =
                lookup(js, field).long

            /**
              * Gets the field as Float value, None if either not existing or not of type Float.
              *
              * @param field The field to lookup
              * @return Optional value of the field as type Float
              */
            @inline def float(field: String): Option[Float] =
                lookup(js, field).float

            /**
              * Gets the field as Double value, None if either not existing or not of type Double.
              *
              * @param field The field to lookup
              * @return Optional value of the field as type Double
              */
            @inline def double(field: String): Option[Double] =
                lookup(js, field).double

            /**
              * Gets the field as String value, None if either not existing or not of type String.
              *
              * @param field The field to lookup
              * @return Optional value of the field as type String
              */
            @inline def string(field: String): Option[String] =
                lookup(js, field).string

            /**
              * Gets the field as Boolean value, None if either not existing or not of type Boolean.
              *
              * @param field The field to lookup
              * @return Optional value of the field as type Boolean
              */
            @inline def boolean(field: String): Option[Boolean] =
                lookup(js, field).boolean

            /**
              * Gets the field as JsValue value, None if either not existing or not of type JsValue.
              *
              * @param field The field to lookup
              * @return Optional value of the field as type JsValue
              */
            @inline def js(field: String): Option[JsValue] =
                lookup(js, field).js
        }

        /**
          * JsValue String implicit class Helper
          *
          * @param field The field to lookup
          */
        implicit class FieldJsHelper(field: String) extends DateJson {

            /**
              * Gets the field as Int value, None if either not existing or not of type Int.
              *
              * @param js JsValue
              * @return Optional value of the field as type Int
              */
            @inline def int(js: JsValue): Option[Int] =
                lookup(js, field).int

            /**
              * Gets the field as Long value, None if either not existing or not of type Long.
              *
              * @param js JsValue
              * @return Optional value of the field as type Long
              */
            @inline def long(js: JsValue): Option[Long] =
                lookup(js, field).long

            /**
              * Gets the field as Float value, None if either not existing or not of type Float.
              *
              * @param js JsValue
              * @return Optional value of the field as type Float
              */
            @inline def float(js: JsValue): Option[Float] =
                lookup(js, field).float

            /**
              * Gets the field as Double value, None if either not existing or not of type Double.
              *
              * @param js JsValue
              * @return Optional value of the field as type Double
              */
            @inline def double(js: JsValue): Option[Double] =
                lookup(js, field).double

            /**
              * Gets the field as String value, None if either not existing or not of type String.
              *
              * @param js JsValue
              * @return Optional value of the field as type String
              */
            @inline def string(js: JsValue): Option[String] =
                lookup(js, field).string

            /**
              * Gets the field as Boolean value, None if either not existing or not of type Boolean.
              *
              * @param js JsValue
              * @return Optional value of the field as type Boolean
              */
            @inline def boolean(js: JsValue): Option[Boolean] =
                lookup(js, field).boolean

            /**
              * Gets the field as JsValue value, None if either not existing or not of type JsValue.
              *
              * @param js JsValue
              * @return Optional value of the field as type JsValue
              */
            @inline def js(js: JsValue): Option[JsValue] =
                lookup(js, field).js

            /**
              * Reads the field as an int.
              */
            @inline def intRead: Reads[Int] =
                field.read[Int]

            /**
              * Reads the field as an int.
              */
            @inline def intReadNullable: Reads[Option[Int]] =
                field.readNullable[Int]

            /**
              * Writes the field as an int.
              */
            @inline def intWrite: OWrites[Int] =
                field.write[Int]

            /**
              * Writes the field as an int.
              */
            @inline def intWriteNullable: OWrites[Option[Int]] =
                field.writeNullable[Int]

            /**
              * Reads the field as a long.
              */
            @inline def longRead: Reads[Long] =
                field.read[Long]

            /**
              * Reads the field as a long.
              */
            @inline def longReadNullable: Reads[Option[Long]] =
                field.readNullable[Long]

            /**
              * Writes the field as a long.
              */
            @inline def longWrite: OWrites[Long] =
                field.write[Long]

            /**
              * Writes the field as a long.
              */
            @inline def longWriteNullable: OWrites[Option[Long]] =
                field.writeNullable[Long]

            /**
              * Parses the field as a float.
              */
            @inline def floatRead: Reads[Float] =
                field.read[Float]

            /**
              * Parses the field as a float.
              */
            @inline def floatReadNullable: Reads[Option[Float]] =
                field.readNullable[Float]

            /**
              * Reads the field as a double.
              */
            @inline def doubleRead: Reads[Double] =
                field.read[Double]

            /**
              * Reads the field as a double.
              */
            @inline def doubleReadNullable: Reads[Option[Double]] =
                field.readNullable[Double]

            /**
              * Writes the field as a double.
              */
            @inline def doubleWrite: OWrites[Double] =
                field.write[Double]

            /**
              * Writes the field as a double.
              */
            @inline def doubleWriteNullable: OWrites[Option[Double]] =
                field.writeNullable[Double]

            /**
              * Reads the field as a string.
              */
            @inline def stringRead: Reads[String] =
                field.read[String]

            /**
              * Reads the field as a string.
              */
            @inline def stringReadNullable: Reads[Option[String]] =
                field.readNullable[String]

            /**
              * Writes the field as a string.
              */
            @inline def stringWrite: OWrites[String] =
                field.write[String]

            /**
              * Writes the field as a string.
              */
            @inline def stringWriteNullable: OWrites[Option[String]] =
                field.writeNullable[String]

            /**
              * Reads the field as Json Object.
              */
            @inline def jsonRead: Reads[JsObject] =
                field.read[JsObject]

            /**
              * Reads the field as Json Object.
              */
            @inline def jsonReadNullable: Reads[Option[JsObject]] =
                field.readNullable[JsObject]

            /**
              * Writes the field as Json Object.
              */
            @inline def jsonWrite: OWrites[JsObject] =
                field.write[JsObject]

            /**
              * Writes the field as Json Object.
              */
            @inline def jsonWriteNullable: OWrites[Option[JsObject]] =
                field.writeNullable[JsObject]

            /**
              * Reads the field as a datetime.
              */
            @inline def datetimeRead: Reads[DateTime] =
                field.read[DateTime](dateReads)

            /**
              * Reads the field as a datetime.
              */
            @inline def datetimeReadNullable: Reads[Option[DateTime]] =
                field.readNullable[DateTime](dateReads)

            /**
              * Writes the field as a datetime.
              */
            @inline def datetimeWrite: OWrites[DateTime] =
                field.write[DateTime](dateWrites)

            /**
              * Writes the field as a datetime.
              */
            @inline def datetimeWriteNullable: OWrites[Option[DateTime]] =
                field.writeNullable[DateTime](dateWrites)

            /**
              * Reads the field as a list
              */
            @inline def listRead[T](implicit r: Reads[T]): Reads[List[T]] =
                field.read[List[T]]

            /**
              * Reads the field as a list
              */
            @inline def listReadNullable[T](implicit r: Reads[T]): Reads[Option[List[T]]] =
                field.readNullable[List[T]]

            /**
              * Writes the field as a list
              */
            @inline def listWrite[T](implicit w: Writes[T]): OWrites[List[T]] =
                field.write[List[T]]

            /**
              * Writes the field as a list
              */
            @inline def listWriteNullable[T](implicit w: Writes[T]): OWrites[Option[List[T]]] =
                field.writeNullable[List[T]]

            /**
              * Reads a nillable list.
              */
            @inline def listReadNillable[T](implicit r: Reads[List[T]]): Reads[List[T]] =
                field.readNullable[List[T]] > (_.toList.flatten)

            /**
              * Writes a nillable list.
              */
            @inline def listWriteNillable[T](implicit w: Writes[List[T]]): OWrites[List[T]] = OWrites[List[T]] {
                case Nil => Json.obj()
                case ls  => Json.obj(field -> Json.toJson(ls))
            }
        }

        /**
          * JsValue String implicit class Helper
          *
          * @param jsLookup The js lookup result
          */
        implicit class JsLookupHelper(jsLookup: JsLookupResult) {

            /**
              * Gets the field as Int value, None if either not existing or not of type Int.
              *
              * @return Optional value of the field as type Int
              */
            @inline def int: Option[Int] =
                as[Int]

            /**
              * Gets the field as Long value, None if either not existing or not of type Long.
              *
              * @return Optional value of the field as type Long
              */
            @inline def long: Option[Long] =
                as[Long]

            /**
              * Gets the field as Float value, None if either not existing or not of type Float.
              *
              * @return Optional value of the field as type Float
              */
            @inline def float: Option[Float] =
                as[Float]

            /**
              * Gets the field as Double value, None if either not existing or not of type Double.
              *
              * @return Optional value of the field as type Double
              */
            @inline def double: Option[Double] =
                as[Double]

            /**
              * Gets the field as String value, None if either not existing or not of type String.
              *
              * @return Optional value of the field as type String
              */
            @inline def string: Option[String] =
                jsLookup.asOpt[String]

            /**
              * Gets the field as Boolean value, None if either not existing or not of type Boolean.
              *
              * @return Optional value of the field as type Boolean
              */
            @inline def boolean: Option[Boolean] =
                as[Boolean]

            /**
              * Gets the field as JsValue value, None if either not existing or not of type JsValue.
              *
              * @return Optional value of the field as type JsValue
              */
            @inline def js: Option[JsValue] =
                as[JsValue]

            /**
              * Gets the field as JsObject value, None if either not existing or not of type JsObject.
              *
              * @return Optional value of the field as type JsObject
              */
            @inline def jsObj: Option[JsObject] =
                as[JsObject]

            /**
              * Gets the field as JsArray value, None if either not existing or not of type JsArray.
              *
              * @return Optional value of the field as type JsArray
              */
            @inline def jsArr: Option[JsArray] =
                as[JsArray]

            /**
              * Gets the type T from the JsLookup. It is either directly the type T
              * or a String, which needs to be cast to T.
              *
              * @param fjs Implicit Json Reads object for Type T
              * @param asIs Implicit AsIs object for Type T
              * @tparam T The type of the field
              * @return The optional field of Type T
              */
            @inline private def as[T](implicit fjs: Reads[T], asIs: AsIs[T]): Option[T] =
                jsLookup.asOpt[T] || (jsLookup.asOpt[String] >> (a => a.asOpt[T]))
        }

        /**
          * JsObject implicit class Helper
          *
          * @param js JsObject
          */
        implicit class JsObjectHelper(js: JsObject) {

            /**
              * Checks, if the JsObject is empty (no keys) or not.
              *
              * @return True, if the JsObject is empty, false otherwise
              */
            @inline def isEmpty: Boolean =
                js.asOpt[JsObject] > (_.keys.isEmpty) | true

            /**
              * Checks, if the JsObject is non-empty (no keys) or not.
              *
              * @return True, if the JsObject is non-empty, false otherwise
              */
            @inline def nonEmpty: Boolean =
                !isEmpty

            /**
              * Tries to add the optional JsValue to the JsObject.
              * Adds it, if existing, returns original JsObject if not.
              *
              * @param field The new field to add
              * @param value The optional value to that field
              * @return Either the original JsObject (if value given is not existing) or a new one
              *         with the JsValue added
              */
            @inline def +(field: String, value: Option[JsValue]): JsObject = value match {
                case None    => js
                case Some(v) => js + (field -> v)
            }

            /**
              * Tries to add the optional JsValue to the JsObject.
              * Adds it, if existing, returns original JsObject if not.
              *
              * @param field The new field to add
              * @param value The optional value to that field
              * @tparam T The type of the field
              * @return Either the original JsObject (if value given is not existing) or a new one
              *         with the JsValue added
              */
            @inline def +[T](field: String, value: Option[T])(implicit tjs: Writes[T]): JsObject =
                js + (field, value.map(v => Json.toJson(v)))

            /**
              * Tries to add the optional JsValue to the JsObject.
              * Adds it, if existing, returns original JsObject if not.
              *
              * @param path The path to the value
              * @param value The optional value to that field
              * @return Either the original JsObject (if value given is not existing) or a new one
              *         with the JsValue added
              */
            @inline def +(path: JsPath, value: Option[JsValue]): JsObject = value match {
                case None    => js
                case Some(v) => JsZipper(js).createOrUpdatePath(path, v).value.asInstanceOf[JsObject]
            }

            /**
              * Tries to add the optional JsValue to the JsObject.
              * Adds it, if existing, returns original JsObject if not.
              *
              * @param path The path to the value
              * @param value The optional value to that field
              * @tparam T The type of the field
              * @return Either the original JsObject (if value given is not existing) or a new one
              *         with the JsValue added
              */
            @inline def +[T](path: JsPath, value: Option[T])(implicit tjs: Writes[T]): JsObject =
                js + (path, value.map(v => Json.toJson(v)))
        }

        /**
          * JsResult implicit class Helper
          *
          * @param j JsResult
          */
        implicit class JsResultHelper[T](j: JsResult[T]) extends ContainerHelper[T, JsResult](j) {

            /**
              * Gets the element in the JsResult.
              *
              * @return JsResult.get
              */
            override protected def get: T =
                j.get

            /**
              * Checks, if JsResult succeeded or not.
              *
              * @return JsResult.isSuccess
              */
            @inline override def ? : Boolean =
                j.isSuccess

            /**
              * Maps over the JsResult
              *
              * @param f The mapping function
              * @return JsResult.map
              */
            @inline override def >[A](f: T => A): JsResult[A] =
                j map f

            /**
              * FlatMaps over the JsResult
              *
              * @param f The mapping function
              * @return JsResult.flatMap
              */
            @inline override def >>[A](f: T => JsResult[A]): JsResult[A] =
                j flatMap f

            /**
              * Filters on given function
              *
              * @return JsResult.filter
              */
            @inline override def <~(f: T => Boolean): JsResult[T] =
                j filter f
        }

        /**
          * Implicit Reads Helper class.
          *
          * @param reads The reads object
          * @tparam A The type of Reads
          */
        implicit class JsReadsHelper[A](reads: Reads[A]) {

            /**
              * getOrElse method.
              *
              * @param b The "or" param
              * @return Reads.getorElse
              */
            @inline def |[B](b: B)(implicit ev: A <:< Option[B]): Reads[B] =
                reads > (a => ev(a) | b)

            /**
              * orElse method.
              *
              * @param reads2 Second reads
              * @return Reads.orElse
              */
            @inline def ||(reads2: Reads[A]): Reads[A] =
                reads orElse reads2

            /**
              * map method.
              *
              * @param f Map function
              * @return Reads.map
              */
            @inline def >[B](f: A => B): Reads[B] =
                reads map f

            /**
              * flatMap method.
              *
              * @param f FlatMap function
              * @return Reads.flatMap
              */
            @inline def >>[B](f: A => Reads[B]): Reads[B] =
                reads flatMap f

            /**
              * filter method.
              *
              * @param f Filter function
              * @return Reads.filter
              */
            @inline def <~(f: A => Boolean): Reads[A] =
                reads filter f
        }
    }
    object Implicits extends Implicits

    // Any to JsValue
    def toJsValue(v: Any): JsValue = v match {
        case i: Int         => Json toJson i
        case l: Long        => Json toJson l
        case f: Float       => Json toJson f
        case d: Double      => Json toJson d
        case b: Boolean     => Json toJson b
        case s: String      => Json toJson s
        case _              => JsNull
    }

    /**
      * The JSON Reads and Writes for DateTime
      */
    trait DateJson {

        /*
         * Data Json Reads and Writes
         */
        implicit lazy val dateReads  = Reads.jodaDateReads("yyyy-MM-dd HH:mm:ss")
        implicit lazy val dateWrites = Writes.jodaDateWrites("yyyy-MM-dd HH:mm:ss")
    }
}