package com.kirbydee.aws

import java.io.InputStream
import java.net.URL

import com.kirbydee.utils.JsUtils.Implicits._

import awscala.s3.S3Object
import org.apache.commons.io.IOUtils
import org.joda.time.DateTime
import play.api.libs.json.{JsNull, JsObject, JsValue, Json}

import scala.concurrent.duration._
import scala.language.postfixOps

case class S3File(s3Object: S3Object) {

    /**
      * Returns the InputStream of the s3 content.
      *
      * @return The InputStream
      */
    def content: InputStream =
        s3Object.content

    /**
      * Returns the content of the s3 file as String.
      *
      * @return The InputStream as String
      */
    def contentText: String = {
        // get input stream and format it to string
        val inputStream = content
        val text = IOUtils toString(inputStream, "UTF-8")

        // close input stream
        inputStream.close()

        // return body
        text
    }

    /**
      * Returns the content of the s3 file as Json.
      *
      * @return The InputStream as Json
      */
    def contentJs: JsValue =
        contentJs(JsNull)

    /**
      * Returns the content of the s3 file as Json.
      *
      * @param default Give a default value, if the content is not a json
      * @return The InputStream as Json
      */
    def contentJs(default: JsValue): JsValue =
        Json.parse(contentText).validate[JsObject] | default

    /**
      * Gets the public URL to the file.
      *
      * @return The public url
      */
    def publicUrl: URL =
        s3Object.publicUrl

    /**
      * Gets the private URL to the file.
      * Expires in 1 hour.
 *
      * @return The signed url
      */
    def privateUrl(implicit s3: S3): URL =
        privateUrl(1 hour)

    /**
      * Gets the private URL to the file.
      *
      * @param duration Expire duration
      * @return The signed url
      */
    def privateUrl(duration: Duration)(implicit s3: S3): URL =
        s3Object.generatePresignedUrl(new DateTime().plus(duration.toMillis))(s3.awsS3)

    /**
      * Destroy the S3 object.
      *
      * @param s3 The implicit S3 object
      */
    def destroy(implicit s3: S3): Unit =
        s3Object.destroy()(s3.awsS3)
}