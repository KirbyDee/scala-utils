package com.kirbydee.parsers

import com.kirbydee.utils.Implicits._
import scala.language.postfixOps
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Reader
import scalaz.{-\/, \/, \/-}

trait Parser extends JavaTokenParsers {

    /*
     * Regex
     */
    val pageName = s"[$letterString-$numberString]+"r

    /**
      * Parser for a time. The format of a time is either on of the following:
      * <ul>
      *     <li>hh:mm</li>
      *     <li>hh:mm:ss</li>
      * </ul>
      *
      * @return The Parser of the time
      */
    protected def time: Parser[String] =
        hours~":"~minutes~(":"~seconds).? ^^ {
            case h~_~m~Some(_~s) => s"$h:$m:$s"
            case h~_~m~None      => s"$h:$m"
        }

    /**
      * Parser for DateTime. The format of a datetime is either on of the following:
      * <ul>
      *     <li>yyyy-MM-dd</li>
      *     <li>yyyy-MM-dd hh:mm</li>
      *     <li>yyyy-MM-dd hh:mm:ss</li>
      *     <li>yyyy-MM-ddThh:mm</li>
      *     <li>yyyy-MM-ddThh:mm:ss</li>
      * </ul>
      *
      * @return The Parser of the datetime
      */
    protected def datetime: Parser[String] =
        date~(("T" | space.*)~> time).? ^^ {
            case d~(Some(t)) => s"${d}T$t"
            case d~None      => d
        }

    /**
      * Processes the parser with the entry point given on the input string given.
      *
      * @param entryPoint The entry point of the parser
      * @param input The input string to parse
      * @tparam T The result type
      * @return Either a good result or a bad error message
      */
    protected def parse[T](entryPoint: Parser[T], input: String): String \/ T = parseAll(entryPoint, input) match {
        case Success(result, next) => success(result, next)
        case Failure(msg, next)    => failure(msg, next)
        case Error(msg, next)      => error(msg, next)
    }

    /**
      * Success. Returns a good result.
      *
      * @param result The result of the parser
      * @param next The input object with information about the string that has been parsed
      * @tparam T The return type
      * @return \/-(result)
      */
    protected def success[T](result: T, next: Reader[_]): \/-[T] =
        \/-(result)

    /**
      * Failure. Returns a readable error message.
      *
      * @param msg The error message
      * @param next The input object with information about the string that has been parsed
      * @return -\/(errorMessage.formatted)
      */
    protected def failure(msg: String, next: Reader[_]): -\/[String] =
        -\/(s"""Error while parsing "${next.source.toString}" at position ${next.offset}: $msg""")

    /**
      * Error. Returns a readable error message.
      *
      * @param msg The error message
      * @param next The input object with information about the string that has been parsed
      * @return -\/(errorMessage.formatted)
      */
    protected def error(msg: String, next: Reader[_]): -\/[String] =
        -\/(s"""Internal server error while parsing URL part "${next.source.toString}"""")
}