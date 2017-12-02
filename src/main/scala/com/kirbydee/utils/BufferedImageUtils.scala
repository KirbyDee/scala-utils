package com.kirbydee.utils

import java.awt.Image
import java.awt.image.BufferedImage

import scala.language.{implicitConversions, postfixOps}

object BufferedImageUtils {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * BufferedImage implicit class Helper
          *
          * @param image The image to manipulate
          */
        implicit class BufferedImageHelper(image: BufferedImage) {

            /**
              * Scales Width (only if given width is positive)
              *
              * @param width The width to scale
              * @return The scales image
              */
            @inline def scaleWidth(width: Int): BufferedImage = width match {
                case GreaterThanZero() => scale(width, (image.getHeight * width) / image.getWidth)
                case _                 => image
            }

            /**
              * Scales Height (only if given width is positive)
              *
              * @param height The height to scale
              * @return The scales image
              */
            @inline def scaleHeight(height: Int): BufferedImage = height match {
                case GreaterThanZero() => scale((image.getWidth * height) / image.getHeight, height)
                case _                 => image
            }

            /**
              * Scales the image to the given width and height.
              *
              * @param width The width to scale
              * @param height The height to scale
              * @return The scales image
              */
            @inline def scale(width: Int, height: Int): BufferedImage = (width - image.getWidth, height - image.getHeight) match {
                // same image, do not need to scale
                case (0, 0) => image

                // scale
                case _                                 =>
                    val tempImg = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
                    val graphics2D = tempImg.createGraphics()
                    graphics2D.drawImage(image.getScaledInstance(width, height, Image.SCALE_SMOOTH), 0, 0, null)
                    graphics2D.dispose()
                    tempImg;
            }
        }
    }
    object Implicits extends Implicits
}
