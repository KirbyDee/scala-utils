package com.kirbydee.utils

import com.kirbydee.macros.ExtensionMacro
import com.kirbydee.utils.OptionUtils.Implicits._

object ExtensionUtils {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /*
         * val assignments for Extensions for easier access
         */
        val JPG  = com.kirbydee.utils.JPG
        val JPEG = com.kirbydee.utils.JPEG
        val PNG  = com.kirbydee.utils.PNG
        val AVI  = com.kirbydee.utils.AVI
        val OGG  = com.kirbydee.utils.OGG
        val GIF  = com.kirbydee.utils.GIF
        val M4V  = com.kirbydee.utils.M4V
        val MPEG = com.kirbydee.utils.MPEG
        val MPG  = com.kirbydee.utils.MPG
        val MPE  = com.kirbydee.utils.MPE
        val MP4  = com.kirbydee.utils.MP4
        val JSON = com.kirbydee.utils.JSON

        /**
          * Helper for to get Extension from a URI.
          *
          * @param uri The URI to get the extension from
          */
        implicit class ExtensionHelper(uri: String) {

            /**
              * Gets the possible extension of the URI.
              *
              * @return Optional extension
              */
            @inline def extension: Option[Extension] = {
                uri lastIndexOf "." match {
                    case LessThanZero() => None
                    case index          => from(uri substring index+1)
                }
            }

            /**
              * Gets the possible extension of the URI, or else, the default provided.
              *
              * @param default The default extension
              * @return Optional extension or the default
              */
            @inline def extension(default: Extension): Extension =
                extension | default
        }
    }
    object Implicits extends Implicits

    /**
      * Converts the extension supplied to a possible Extension.
      *
      * @param extension The extension as String
      * @return The Optional Extension object
      */
    def from(extension: String): Option[Extension] =
        ExtensionMacro from extension
}