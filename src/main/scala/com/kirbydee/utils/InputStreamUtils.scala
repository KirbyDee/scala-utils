package com.kirbydee.utils

import java.awt.Dimension
import java.awt.image.BufferedImage
import java.io._
import javax.imageio.stream.ImageInputStream
import javax.imageio.{ImageIO, ImageReader}

import com.kirbydee.utils.OptionUtils.Implicits._
import org.apache.commons.io.IOUtils
import play.api.libs.json.{JsValue, Json}

import scala.language.{implicitConversions, postfixOps}
import scala.util.Try

object InputStreamUtils {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * InputStream implicit class Helper
          *
          * @param is The inputStream to manipulate
          */
        implicit class InputStreamHelper(is: InputStream) {

            /**
              * Converts the input stream into byte array.
              *
              * @return The byte array of the input stream
              */
            @inline def bytes: Array[Byte] = wrapClose { is =>
                Stream.continually(is.read).takeWhile(-1 !=).map(_.toByte).toArray
            }

            /**
              * Parses the inputStream into a possible JsValue.
              *
              * @return The possible JsValue
              */
            @inline def json: Option[JsValue] = wrapClose { is =>
                Try(Json parse is).toOption
            }

            /**
              * Parses the inputStream into a possible JsValue.
              *
              * @return The possible JsValue
              */
            @inline def string: Option[String] = wrapClose { is =>
                Try(IOUtils.toString(is, "UTF-8")).toOption
            }

            /**
              * Wraps the InputStream function code, to finally close the InputStream after using.
              *
              * @param f The InputStream function
              * @tparam A The return Type of the InputStream function
              * @return The return value of the given function
              */
            private def wrapClose[A](f: InputStream => A): A = try {
                f(is)
            } finally is.close()

            /**
              * Returns the read BufferedImage out of the InputStream
              *
              * @return The optional buffered image
              */
            @inline def image: Option[BufferedImage] =
                Try(ImageIO read is).toOption
        }

        /**
          * ImageInputStream implicit class Helper
          *
          * @param is The imageInputStream to manipulate
          */
        implicit class ImageInputStreamHelper(is: ImageInputStream) {

            /**
              * Gets the dimensions of the image.
              *
              * @return image dimensions
              */
            @inline def imageDimension: Dimension = {
                Try {
                    val readers = ImageIO.getImageReaders(is)
                    readers.hasNext match {
                        case true  =>
                            val reader: ImageReader = readers.next
                            reader.setInput(is)
                            val width: Int  = reader.getWidth(0)
                            val height: Int = reader.getHeight(0)
                            reader.dispose()
                            new Dimension(width, height)
                        case false =>
                            new Dimension()
                    }
                } toOption
            } | new Dimension()
        }
    }
    object Implicits extends Implicits
}
