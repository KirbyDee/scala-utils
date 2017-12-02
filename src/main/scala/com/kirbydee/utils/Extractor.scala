package com.kirbydee.utils

import com.kirbydee.utils
import com.kirbydee.utils.AsIs.Implicits._
import com.kirbydee.utils.OptionUtils.Implicits._
import com.kirbydee.utils.ListUtils.Implicits._
import org.joda.time.DateTime
import play.api.libs.json.{JsArray, JsObject, JsValue}

import scala.language.postfixOps

object Extractor {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /*
         * val assignments for Extractor Objects for easier access
         */
        val &                       = utils.&
        val NonNil                  = utils.NonNil
        val Singleton               = utils.Singleton
        val StartsWith              = utils.StartsWith
        val EndsWith                = utils.EndsWith
        val Empty                   = utils.Empty
        val NonEmpty                = utils.NonEmpty
        val Length                  = utils.Length
        val GreaterThanZero         = utils.GreaterThanZero
        val GreaterOrEqualThanZero  = utils.GreaterOrEqualThanZero
        val LessThanZero            = utils.LessThanZero
        val LessOrEqualThanZero     = utils.LessOrEqualThanZero
        val LowerCase               = utils.LowerCase
        val UpperCase               = utils.UpperCase
        val DeCapitalize            = utils.DeCapitalize
        val Capitalize              = utils.Capitalize
        val LowerCaseEqual          = utils.LowerCaseEqual
        val IsString                = utils.IsString
        val IsBoolean               = utils.IsBoolean
        val IsByte                  = utils.IsByte
        val IsShort                 = utils.IsShort
        val IsInt                   = utils.IsInt
        val IsLong                  = utils.IsLong
        val IsLongList              = utils.IsLongList
        val IsFloat                 = utils.IsFloat
        val IsDouble                = utils.IsDouble
        val IsDateTime              = utils.IsDateTime
        val IsJson                  = utils.IsJson
        val IsJsObj                 = utils.IsJsObj
        val IsJsArr                 = utils.IsJsArr
        val IsExtension             = utils.IsExtension
        val string                  = utils.AsString
        val boolean                 = utils.AsBoolean
        val byte                    = utils.AsByte
        val short                   = utils.AsShort
        val int                     = utils.AsInt
        val long                    = utils.AsLong
        val longs                   = utils.AsLongList
        val float                   = utils.AsFloat
        val double                  = utils.AsDouble
        val datetime                = utils.AsDateTime
        val json                    = utils.AsJson
        val jsObj                   = utils.AsJsObj
        val jsArr                   = utils.AsJsArr
        val extension               = utils.AsExtension
    }
    object Implicits extends Implicits
}

/**
  * Splitter to apply two pattern matches on the same scrutinee.
  */
object & {
    def unapply[A](a: A): Some[(A, A)] =
        Some((a, a))
}

/**
  * Extractor object matching nonEmpty list.
  */
object NonNil {
    def unapply(l: List[_]): Boolean =
        l?
}

/**
  * Extractor object matching a singleton list.
  */
object Singleton {
    def unapply(l: List[_]): Boolean =
        l.size == 1
}

/**
  * Extractor object matching first character.
  */
object StartsWith {
    def unapply(s: String): Option[Char] =
        s.headOption
}

/**
  * Extractor object matching last character.
  */
object EndsWith {
    def unapply(s: String): Option[Char] =
        s.reverse.headOption
}

/**
  * Extractor object matching Empty.
  */
object Empty {
    def unapply(map: Map[_, _]): Boolean =
        map.isEmpty

    def unapply(seq: Seq[_]): Boolean =
        seq.isEmpty

    def unapply(s: String): Boolean =
        s.isEmpty
}

/**
  * Extractor object matching NonEmpty.
  */
object NonEmpty {
    def unapply(map: Map[_, _]): Boolean =
        map.nonEmpty

    def unapply(seq: Seq[_]): Boolean =
        seq.nonEmpty

    def unapply(s: String): Boolean =
        !s.isEmpty
}

/**
  * Extractor object matching length.
  */
object Length {
    def unapply(map: Map[_, _]): Some[Int] =
        Some(map.size)

    def unapply(seq: Seq[_]): Some[Int] =
        Some(seq.length)

    def unapply(s: String): Some[Int] =
        Some(s.length)
}

/**
  * Extractor for a number which is greater than 0.
  */
object GreaterThanZero {
    def unapply(n: Int): Boolean =
        n > 0
    def unapply(n: Long): Boolean =
        n > 0
    def unapply(n: Float): Boolean =
        n > 0
    def unapply(n: Double): Boolean =
        n > 0
}

/**
  * Extractor for a number which is greater or equal than 0.
  */
object GreaterOrEqualThanZero {
    def unapply(n: Int): Boolean =
        n >= 0
    def unapply(n: Long): Boolean =
        n >= 0
    def unapply(n: Float): Boolean =
        n >= 0
    def unapply(n: Double): Boolean =
        n >= 0
}

/**
  * Extractor for a number which is less than 0.
  */
object LessThanZero {
    def unapply(n: Int): Boolean =
        n < 0
    def unapply(n: Long): Boolean =
        n < 0
    def unapply(n: Float): Boolean =
        n < 0
    def unapply(n: Double): Boolean =
        n < 0
}

/**
  * Extractor for a number which is less or equal than 0.
  */
object LessOrEqualThanZero {
    def unapply(n: Int): Boolean =
        n <= 0
    def unapply(n: Long): Boolean =
        n <= 0
    def unapply(n: Float): Boolean =
        n <= 0
    def unapply(n: Double): Boolean =
        n <= 0
}

/**
  * Extractor object for Lower Case
  */
object LowerCase {
    def unapply(s: String): Boolean =
        s.toLowerCase == s
}

/**
  * Extractor object for Upper Case
  */
object UpperCase {
    def unapply(s: String): Boolean =
        s.toUpperCase == s
}

/**
  * Extractor object for DeCapitalize
  */
object DeCapitalize {
    def unapply(s: String): Boolean =
        s.headOption? (c => c.toLower == c)
}

/**
  * Extractor object for Capitalize
  */
object Capitalize {
    def unapply(s: String): Boolean =
        s.headOption? (c => c.toUpper == c)
}

/**
  * Extractor object for Equal String
  */
object LowerCaseEqual {
    def unapply(s: String): Option[String] =
        Some(s.toLowerCase)
}

/**
  * Extractor object for Is
  */
abstract class IsExtractor[T](implicit asIs: AsIs[T]) {

    def unapply(s: String): Boolean =
        s.is[T]
}
object IsString    extends IsExtractor[String]
object IsBoolean   extends IsExtractor[Boolean]
object IsByte      extends IsExtractor[Byte]
object IsShort     extends IsExtractor[Short]
object IsInt       extends IsExtractor[Int]
object IsLong      extends IsExtractor[Long]
object IsLongList  extends IsExtractor[List[Long]]
object IsFloat     extends IsExtractor[Float]
object IsDouble    extends IsExtractor[Double]
object IsDateTime  extends IsExtractor[DateTime]
object IsJson      extends IsExtractor[JsValue]
object IsJsObj     extends IsExtractor[JsObject]
object IsJsArr     extends IsExtractor[JsArray]
object IsExtension extends IsExtractor[Extension]

/**
  * Extractor object for As
  */
abstract class AsExtractor[T](implicit asIs: AsIs[T]) {

    def unapply(s: String): Option[T] =
        s.asOpt[T]
}
object AsString    extends AsExtractor[String]
object AsBoolean   extends AsExtractor[Boolean]
object AsByte      extends AsExtractor[Byte]
object AsShort     extends AsExtractor[Short]
object AsInt       extends AsExtractor[Int]
object AsLong      extends AsExtractor[Long]
object AsLongList  extends AsExtractor[List[Long]]
object AsFloat     extends AsExtractor[Float]
object AsDouble    extends AsExtractor[Double]
object AsDateTime  extends AsExtractor[DateTime]
object AsJson      extends AsExtractor[JsValue]
object AsJsObj     extends AsExtractor[JsObject]
object AsJsArr     extends AsExtractor[JsArray]
object AsExtension extends AsExtractor[Extension]