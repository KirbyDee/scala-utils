package com.kirbydee.aws

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO
import com.kirbydee.utils.Implicits._

import awscala.s3.{Bucket, PutObjectResult}
import com.kirbydee.config.{Development, Environment, Localhost, Production}
import com.kirbydee.logr.Logr
import com.kirbydee.utils.{Extension, ToString}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{postfixOps, reflectiveCalls}

object S3Bucket {

    // The extension of a s3 bucket
    val s3BucketExtension: String = "logograb.com"

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * S3Bucket implicit class Helper
          *
          * @param name The name of the Bucket
          */
        implicit class S3BucketHelper(name: String) {

            /**
              * Returns the environment S3Bucket name.
              *
              * @param s3 The implicit S3 object
              * @param env The implicit Environment
              * @return The name of the s3 bucket
              */
            @inline def s3EnvName(implicit s3: S3, env: Environment): String = env match {
                case Production  => s"$name.$s3BucketExtension"
                case Development => s"$name.dev.$s3BucketExtension"
                case Localhost   => s""
            }

            /**
              * Creates an environment S3Bucket with the name given.
              *
              * @param s3 The implicit S3 object
              * @param env The implicit Environment
              * @return The created S3Bucket with the environment given
              */
            @inline def s3Env(implicit s3: S3, env: Environment): S3Bucket =
                s3EnvName s3

            /**
              * Creates a S3Bucket with the name given.
              *
              * @param s3 The implicit S3 object
              * @return The created S3Bucket
              */
            @inline def s3(implicit s3: S3): S3Bucket =
                S3Bucket(name)
        }
    }
    object Implicits extends Implicits
}

case class S3Bucket(name: String)(implicit s3: S3) extends Logr { b =>

    // the actual bucket
    val bucket: Future[Option[Bucket]] = s3 bucket name

    /**
      * Downloads the file from s3.
      *
      * @param f Transformation function of fetched S3File
      * @param executor The implicit Execution Context for handling futures
      * @return The future optional transformed S3File
      */
    def >>[A](f: S3File => A)(implicit executor: ExecutionContext): Object {
        def |(fileKeyOp: Option[String]): Future[Option[A]]
        def |(fileKey: String): Future[Option[A]]
    } = new {

        /**
          * Specifies the file key of the S3File
          *
          * @param fileKey The path to the file
          * @return The future optional transformed S3File
          */
        def |(fileKey: String): Future[Option[A]] =
            s3.get(b, fileKey)(f)

        /**
          * Specifies the file key of the S3File
          *
          * @param fileKeyOp The path to the file
          * @return The future optional transformed S3File
          */
        def |(fileKeyOp: Option[String]): Future[Option[A]] = fileKeyOp match {
            case Some(fileKey) =>
                this | fileKey
            case None          =>
                warn("FileKey given for downloading S3 file is None")
                Future(None)
        }
    }

    /**
      * Downloads the file from s3.
      *
      * @param f Transformation function of fetched S3File
      * @param g Function used if the S3File doesn't exist
      * @param executor The implicit Execution Context for handling futures
      * @return The future optional transformed S3File
      */
    def >>[A, AA >: A](f: S3File => A, g: => AA)(implicit executor: ExecutionContext): Object {
        def |(fileKeyOp: Option[String]): Future[AA]
        def |(fileKey: String): Future[AA]
    } = new {

        /**
          * Specifies the file key of the S3File
          *
          * @param fileKey The path to the file
          * @return The future optional transformed S3File
          */
        def |(fileKey: String): Future[AA] =
            getWithDefault(>>(f) | fileKey)

        /**
          * Specifies the file key of the S3File
          *
          * @param fileKeyOp The path to the file
          * @return The future optional transformed S3File
          */
        def |(fileKeyOp: Option[String]): Future[AA] =
            getWithDefault(>>(f) | fileKeyOp)

        /**
          * Gets the S3File with a default supplied.
          *
          * @param file The possible S3File
          * @return Either the transformed S3File or the default (if not existing)
          */
        private def getWithDefault(file: Future[Option[A]]): Future[AA] = file > {
            case Some(a) => a
            case None    => g
        }
    }


    // Trait used for uploading to s3
    trait << {

        /**
          * Upload to s3 with given fileKey
          */
        def |(fileKey: String)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]] =
            | (fileKey, publicRead = false)

        /**
          * Upload to s3 with given fileKey
          */
        def |(fileKey: String, publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]

        /**
          * Upload to s3 with given (possible) fileKey (does not upload, if fileKey is None)
          */
        def |(fileKeyOp: Option[String])(implicit executor: ExecutionContext): Future[Option[PutObjectResult]] =
            | (fileKeyOp, publicRead = false)

        /**
          * Upload to s3 with given (possible) fileKey (does not upload, if fileKey is None)
          */
        def |(fileKeyOp: Option[String], publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]] = fileKeyOp match {
            case Some(fileKey) =>
                this | (fileKey, publicRead = publicRead)
            case None          =>
                warn("FileKey given for uploading file to S3 is None")
                Future(None)
        }
    }

    /**
      * Store the byte array under fileKey provided.
      *
      * @param contentType The type of content
      * @param content The content to store
      */
    def <<(contentType: String, content: Array[Byte]): Object {
        def |(fileKey: String)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
        def |(fileKey: String, publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
        def |(fileKeyOp: Option[String])(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
        def |(fileKeyOp: Option[String], publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
    } = new << {

        /**
          * Store byte array.
          *
          * @param fileKey The path to the file to upload
          * @param publicRead Flag, if the it should be public readable
          * @param executor The implicit Execution Context for handling futures
          */
        override def |(fileKey: String, publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]] =
            s3.put(b)(fileKey, contentType, content, publicRead = publicRead)
    }

    /**
      * Store the image under fileKey provided.
      *
      * @param extension The extension of the image
      * @param image The content to store
      */
    def <<(extension: Extension, image: BufferedImage): Object {
        def |(fileKey: String)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
        def |(fileKey: String, publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
        def |(fileKeyOp: Option[String])(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
        def |(fileKeyOp: Option[String], publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
    } = new << {

        /**
          * Store image.
          *
          * @param fileKey The path to the file to upload
          * @param executor The implicit Execution Context for handling futures
          */
        override def |(fileKey: String, publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]] ={
            // build byte array and content type
            val formatName = extension.toString.replace(".", "")
            val outputStream = new ByteArrayOutputStream
            ImageIO.write(image, formatName, outputStream)
            val contentType = s"image/$formatName"
            val bytes = outputStream.toByteArray
            outputStream.close()

            // put byte array
            << (contentType, bytes) | (fileKey, publicRead = publicRead)
        }
    }

    /**
      * Store the text under fileKey provided.
      *
      * @param text The text to store
      */
    def <<(text: String): << with Object {
        def |(fileKey: String)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
        def |(fileKey: String, publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
        def |(fileKeyOp: Option[String])(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
        def |(fileKeyOp: Option[String], publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
    } = new << {

        /**
          * Store text.
          *
          * @param fileKey The path to the file to upload
          * @param executor The implicit Execution Context for handling futures
          */
        override def |(fileKey: String, publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]] =
            << ("text/plain", text.getBytes) | (fileKey, publicRead = publicRead)
    }

    /**
      * Store the text under fileKey provided.
      *
      * @param content The content to store
      * @param to The implicit ToString
      * @tparam C The type of the Content
      */
    def <<[C](content: C)(implicit to: ToString[C]): Object {
        def |(fileKey: String)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
        def |(fileKey: String, publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
        def |(fileKeyOp: Option[String])(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
        def |(fileKeyOp: Option[String], publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]]
    } = new << {

        /**
          * Store text.
          *
          * @param fileKey The path to the file to upload
          * @param executor The implicit Execution Context for handling futures
          */
        override def |(fileKey: String, publicRead: Boolean)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]] =
            << ("text/plain", to.parse(content).getBytes) | (fileKey, publicRead = publicRead)
    }
}