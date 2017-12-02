package com.kirbydee.utils
import scala.language.postfixOps
import scala.util.matching.{ Regex => ScalaRegex }

object Regex {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        // normal regex
        val word                = "[\\w]+"r
        val letterString        = "\\p{L}"
        val letter              = s"[$letterString]"r
        val letters             = s"[$letterString]+"r
        val numberString        = "0-9"
        val number              = s"[$numberString]"r
        val numbers             = s"[$numberString]+"r
        val decimalNumbers      = s"[$numberString\\.]+"r
        val characterString     = """\p{L}\!@#\$%\^\*\-_\.\?\&\'/""" + numberString
        val character           = s"[$characterString]"r
        val characters          = s"[$characterString]+"r
        val charactersWithSpace = s"[$characterString,\\s]+"r
        val positiveWholeNumber = """\d+"""r
        val space               = """\s"""r
        val quotes              = "\""r
        val date                = """\d{4}-(1[0-2]|0?[1-9])-(3[0-1]|[12][0-9]|0?[1-9])"""r
        val hours               = "2[0-3]|1[0-9]|0?[0-9]"r
        val minutes             = "[0-5][0-9]|0?[0-9]"r
        val seconds             = minutes

        // regex for email (http://emailregex.com/)
        val emailRegex: ScalaRegex = """(?:[a-z0-9!#$%&'*+=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])"""r
    }
    object Implicits extends Implicits
}