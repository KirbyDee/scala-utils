package com.kirbydee.utils

import java.math.BigInteger
import java.net.{URLDecoder, URLEncoder}
import java.security.{MessageDigest, SecureRandom}
import java.util.Base64
import javax.xml.bind.DatatypeConverter

import com.kirbydee.utils.Ternary.Implicits._
import org.apache.commons.codec.binary.Hex

import scala.language.{implicitConversions, postfixOps}
import scala.util.Try

object StringUtils {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * String implicit class Helper
          *
          * @param string The string to manipulate
          */
        implicit class StringHelper(string: String) {

            /**
              * De-Capitalizes the string (first letter to lowercase)
              *
              * @return The same string but the first letter in lowercase
              */
            @inline def deCapitalize: String =
                StringUtils deCapitalize string

            /**
              * Computes the sha1 hash of the input string.
              *
              * @return The sha1 hash surrounded by a Try block, since it can fail
              */
            @inline def sha1: String =
                StringUtils sha1Digest string

            /**
              * Computes the md5 hash of the input string.
              *
              * @return The md5 hash surrounded by a Try block, since it can fail
              */
            @inline def md5: String =
                StringUtils md5Digest string

            /**
              * HTML decodes the input string.
              *
              * @return The HTML decoded string
              */
            @inline def htmlDecode: String =
                StringUtils htmlDecode string

            /**
              * HTML encodes the input string.
              *
              * @return The HTML encoded string
              */
            @inline def htmlEncode: String =
                StringUtils htmlEncode string

            /**
              * Base64 decodes the input string.
              *
              * @return The Base64 decoded string
              */
            @inline def base64Decode: String =
                StringUtils base64Decode string

            /**
              * Base64 encodes the input string.
              *
              * @return The Base64 encoded string
              */
            @inline def base64Encode: String =
                StringUtils base64Encode string

            /**
              * Converts the base64 image string to a byteArray
              *
              * @return The byte array
              */
            @inline def base64Image: Try[Array[Byte]] =
                StringUtils base64Image string

            /**
              * Turns the string into a HTML file name
              *
              * @return The name of the html file
              */
            @inline def html: String =
                StringUtils html string

            /**
              * Removes the extension of the string
              *
              * @return The string without the extension
              */
            @inline def withoutExtension: String =
                StringUtils withoutExtension string
        }
    }
    object Implicits extends Implicits

    /**
      * De-Capitalizes the string (first letter to lowercase)
      *
      * @param string The string to de-capitalizes
      * @return The same string but the first letter in lowercase
      */
    def deCapitalize(string: String): String = string match {
        case Empty()        => ""
        case s@ LowerCase() => s
        case s              =>
            val chars = string.toCharArray
            chars(0) = chars(0).toLower
            new String(chars)
    }

    /**
      * Computes the sha1 hash of the input string.
      *
      * @param input The string to hash
      * @return The sha1 hash surrounded by a Try block, since it can fail
      */
    def sha1Digest(input: => String): String =
        digest(input)("SHA-1")

    /**
      * Computes the md5 hash of the input string.
      *
      * @param input The string to hash
      * @return The md5 hash surrounded by a Try block, since it can fail
      */
    def md5Digest(input: => String): String =
        digest(input)("MD5")

    /**
      * Digests the input string with the given algorithm.
      *
      * @param input The string to hash
      * @param algorithm THe algorithm to digest the input string with
      * @return The md5 hash surrounded by a Try block, since it can fail
      */
    private def digest(input: => String)(algorithm: String): String = {
        val cript = MessageDigest getInstance algorithm
        cript.reset()
        cript.update(input getBytes "utf8")
        new String(Hex encodeHex cript.digest())
    }

    /**
      * HTML decodes the input string.
      *
      * @param string The string to decode
      * @return The HTML decoded string
      */
    def htmlDecode(string: String): String =
        URLDecoder decode (string, "utf-8")

    /**
      * HTML encode the input string.
      *
      * @param string The string to encode
      * @return The HTML encoded string
      */
    def htmlEncode(string: String): String =
        URLEncoder encode (string, "utf-8")

    /**
      * Base64 decodes the input string.
      *
      * @param string The string to decode
      * @return The Base64 decoded string
      */
    def base64Decode(string: String): String =
        new String(Base64.getDecoder decode string, "utf-8")

    /**
      * Base64 encodes the input string.
      *
      * @param string The string to encode
      * @return The Base64 encoded string
      */
    def base64Encode(string: String): String =
        new String(Base64.getEncoder encode string.getBytes("utf-8"), "utf-8")
    /**
      * Converts the base64 image string to a byteArray
      *
      * @param string The string to convert
      * @return The byte array
      */
    def base64Image(string: String): Try[Array[Byte]] = Try {
        DatatypeConverter.parseBase64Binary(string.substring(string.indexOf(",") + 1))
    }

    /**
      * Turns the string into a HTML file name
      *
      * @param string The string to make the html file name
      * @return The name of the html file
      */
    def html(string: String): String =
        (string endsWith ".html") ? string | s"$string.html"

    /**
      * Removes the extension of the string
      *
      * @param string The string to remove the extension from
      * @return The string without the extension
      */
    def withoutExtension(string: String): String = (string split "\\.").toList match {
        case Nil     => string
        case x :: xs => x
    }

    /**
      * Generates a random Hash given by the number of bits
      *
      * @param numBits The number of bits
      * @return The random hash
      */
    def generateRandomHash(numBits: Int): String = {
        val random = new SecureRandom
        new BigInteger(numBits, random).toString(32)
    }
}
