package com.kirbydee.utils

import java.awt.image.BufferedImage
import java.io.{IOException, InputStream}
import java.net.{HttpURLConnection, URL, URLConnection}
import javax.imageio.ImageIO

import com.kirbydee.utils.OptionUtils.Implicits._
import com.kirbydee.utils.FutureUtils.Implicits._
import com.kirbydee.utils.Ternary.Implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{implicitConversions, postfixOps}
import scala.util.Try

object HttpUtils {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /*
         * val / types assignments for easier access
         */
        val UserAgent: String = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.95 Safari/537.11"

        /**
          * Http String implicit class Helper
          *
          * @param urlString The url as a String
          */
        implicit class HttpStringHelper(urlString: String) {

            /**
              * Check if URL exists or not
              *
              * @return True, if the URL can be reached, false otherwise
              */
            @inline def checkURL: Boolean = Try {
                new URL(urlString)?
            } getOrElse false

            /**
              * Transforms the url into a HTTP one.
              *
              * @return HTTP url
              */
            @inline def http: String =
                protocol("http://")

            /**
              * Transforms the url into a HTTPs one.
              *
              * @return HTTPs url
              */
            @inline def https: String =
                protocol("https://")

            /**
              * Transforms the url into the given Protocol one (http:// or https://) (just the String).
              *
              * @param pro The protocol to transform to (just the String)
              * @return PROTOCOL url
              */
            @inline def protocol(pro: String): String =
                (urlString.startsWith("http://") || urlString.startsWith("https://")) ? urlString | s"$pro$urlString"

            /**
              * Transforms the string into a URL.
              *
              * @return The created URL
              */
            @inline def url: URL =
                new URL(urlString.http)
        }

        /**
          * Http URL implicit class Helper
          *
          * @param url The url
          */
        implicit class HttpUrlHelper(url: URL) {

            /**
              * Check if URL exists or not
              *
              * @return True, if the URL can be reached, false otherwise
              */
            @inline def ? : Boolean =
                exists

            /**
              * Check if URL exists or not
              *
              * @return True, if the URL can be reached, false otherwise
              */
            @inline def exists: Boolean =
                HttpUtils exists url

            /**
              * Opens the URL Connection with the additional UserAgent
              * supplied.
              *
              * @return The URL Connection
              */
            @inline def openConnectionUA: URLConnection =
                openConnectionUA(UserAgent)

            /**
              * Opens the URL Connection with the additional UserAgent
              * supplied.
              *
              * @return The URL Connection
              */
            @inline def openConnectionUA(userAgent: String): URLConnection = {
                val conn = url.openConnection
                conn.setRequestProperty("User-Agent", userAgent)
                conn
            }

            /**
              * Download the content of the URL.
              *
              * @param executor The implicit Execution Context for handling futures
              * @return InputStream
              */
            @inline def download(implicit executor: ExecutionContext): Future[Option[InputStream]] = {
                @annotation.tailrec
                def go(tries: Int): Option[InputStream] = tries match {
                    case 0 => None
                    case _ => try {
                        Option(url.openStream())
                    } catch {
                        case e: IOException => go(tries - 1)
                    }
                }
                Future(go(3))
            }

            /**
              * Download the Image of the the URL
              *
              * @param executor The implicit Execution Context for handling futures
              * @return BufferedImage
              */
            @inline def downloadImage(implicit executor: ExecutionContext): Future[Option[BufferedImage]] = download > {
                _ >> { in =>
                    // try to convert downloaded content to image
                    val image = Try {
                        ImageIO read in
                    } toOption

                    // close input stream
                    in.close()

                    // return image
                    image
                }
            }
        }

        /**
          * Http URLConnection implicit class Helper
          *
          * @param conn The url connection
          */
        implicit class HttpUrlConnectionHelper(conn: URLConnection) {

            /**
              * Download the content of the URL Connection.
              *
              * @param executor The implicit Execution Context for handling futures
              * @return InputStream
              */
            @inline def download(implicit executor: ExecutionContext): Future[Option[InputStream]] = {
                @annotation.tailrec
                def go(tries: Int): Option[InputStream] = tries match {
                    case 0 => None
                    case _ => try {
                        Option(conn.getInputStream)
                    } catch {
                        case e: IOException => go(tries - 1)
                    }
                }
                Future(go(3))
            }
        }
    }
    object Implicits extends Implicits

    /**
      * Check if URL exists or not
      *
      * @param url The URL to check
      * @return True, if the URL can be reached, false otherwise
      */
    def exists(url: URL): Boolean = {
        val connection =  url.openConnection().asInstanceOf[HttpURLConnection]
        connection.setRequestMethod("HEAD")
        connection.connect()
        connection.getResponseCode / 100 == 2
    }
}
