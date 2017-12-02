package com.kirbydee.aws

import java.net.{HttpURLConnection, URL, URLEncoder}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import javax.inject.Inject

import awscala.s3.{Bucket, PutObjectResult, S3Object, S3 => AWSS3}
import com.amazonaws.regions.Region
import com.amazonaws.services.s3.model.ObjectMetadata
import com.amazonaws.{regions => awsregions}
import com.kirbydee.config.Configuration
import com.kirbydee.logr.Logr
import com.kirbydee.utils.FutureUtils.Implicits._
import com.kirbydee.utils.GreaterThanZero
import com.kirbydee.utils.OptionUtils.Implicits._
import org.apache.commons.codec.binary.Base64

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{implicitConversions, postfixOps}

class S3 @Inject()(config: Configuration) extends Logr {

    // create s3 object
    implicit val awsS3: AWSS3 = AWSS3(
        config.aws.key,
        config.aws.secret
    )(Region getRegion (awsregions.Regions fromName config.aws.region))
    val MAC: Mac = {
        val MAC = Mac.getInstance("HmacSHA1")
        MAC.init(new SecretKeySpec(config.aws.secret.getBytes("UTF8"), "HmacSHA1"))
        MAC
    }

    /**
      * Get the S3 bucket with given name.
      *
      * @param bucketName The name of the bucket
      * @return The future optional bucket with name given
      */
    def bucket(bucketName: String): Future[Option[Bucket]] = Future successful {
        debug(s"Create S3 Bucket: $bucketName")
        awsS3 bucket bucketName
    }

    /**
      * Apply a function to the bucket.
      *
      * @param bucket The S3 bucket
      * @param f The function to apply to the bucket
      * @param executor The implicit Execution Context for handling futures
      * @tparam A The type of the function return value
      * @return The mapped future optional bucket
      */
    private def withBucket[A](bucket: S3Bucket)(f: Bucket => A)(implicit executor: ExecutionContext): Future[Option[A]] = bucket.bucket map {
        case Some(b) =>
            Some(f(b))
        case None    =>
            warn(s"""S3 Bucket with name "${bucket.name}" does not exist: NOP""")
            None
    } recover { case e =>
        error(e.getMessage, e)
        None
    }

    /**
      * With bucket name given, download the file under fileKey provided and transform it with given function.
      *
      * @param bucket The S3 bucket
      * @param fileKey The path to the file to download
      * @param f The function to apply to the bucket
      * @param executor The implicit Execution Context for handling futures
      * @tparam A The type of the function return value
      * @return The mapped future optional bucket
      */
    def get[A](bucket: S3Bucket, fileKey: String)(f: S3File => A)(implicit executor: ExecutionContext): Future[Option[A]] = {
        debug(s"S3 GET ${bucket.name}/$fileKey")
        withBucket(bucket)(_.get(fileKey) > S3File.apply > f) > (_ flatten)
    }

    def privateURLExists(bucketName: String, fileKey: String): Boolean = {
        // get HEAD url
        val headURL = signS3ResourceURL(s"/$bucketName/$fileKey", 3600, "HEAD")

        // try connecting to it
        checkIfURLExists(headURL)
    }

    private def signS3ResourceURL(resource: String, expires: Int, method: String = "GET"): String = {
        // expires 1h after calling this method
        val expireDate = (System.currentTimeMillis.asInstanceOf[Double] / 1000 + expires).asInstanceOf[Long]
        val data = s"$method\n\n\n$expireDate\n$resource"

        // return signed URL
        s"https://s3-eu-west-1.amazonaws.com$resource?AWSAccessKeyId=${config.aws.key}&Expires=$expireDate&Signature=${signS3Data(data)}"
    }

    private def checkIfURLExists(url: String): Boolean = {
        val connection =  new URL(url).openConnection().asInstanceOf[HttpURLConnection]
        connection.setRequestMethod("HEAD")
        connection.connect()
        connection.getResponseCode / 100 == 2
    }

    private def signS3Data(data: String): String = {
        def go(data: String, tries: Int): String = try {
            val signBytes = MAC.doFinal(data.getBytes("UTF8"))
            URLEncoder.encode(new String(Base64.encodeBase64(signBytes)), "UTF-8")
        } catch {
            case e: Exception => tries match {
                case GreaterThanZero() => go(data, tries - 1)
                case _                 => throw new Exception(s"data: $data, message: ${e.getMessage}", e)
            }
        }

        // try at least 3 times
        go(data, 3)
    }

    /**
      * With bucket name given, store the file under fileKey provided and transform it with given function.
      *
      * @param bucket The S3 bucket
      * @param fileKey The path to the file to download
      * @param contentType The type of content
      * @param content The content to store
      * @param publicRead Flag, if the it should be public readable (default false)
      * @param executor The implicit Execution Context for handling futures
      */
    def put(bucket: S3Bucket)(fileKey: String, contentType: String, content: Array[Byte], publicRead: Boolean = false)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]] = {
        debug(s"S3 PUT ${bucket.name}/$fileKey")
        val metadata = new ObjectMetadata()
        metadata.setContentLength(content.length)
        metadata.setContentType(contentType)
        withBucket(bucket)(b => publicRead match {
            case true  => b.putObjectAsPublicRead(fileKey, content, metadata)
            case false => b.putObject(fileKey, content, metadata)
        })
    }

    def copy(bucketTo: S3Bucket, fileKeyFrom: String, bucketFrom: S3Bucket)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]] =
        copy(bucketTo, fileKeyFrom, bucketFrom, fileKeyFrom)

    def copy(bucketTo: S3Bucket, fileKeyFrom: String, bucketFrom: S3Bucket, fileKeyTo: String)(implicit executor: ExecutionContext): Future[Option[PutObjectResult]] = for {
        // get the right FROM and TO bucket
        bFromOpt <- bucketFrom.bucket
        bToOpt   <- bucketTo.bucket

        // copy s3 objects
        s3PutOpt = for {
            bFrom <- bFromOpt
            bTo   <- bToOpt

            _ = debug(s"S3 copy ${bFrom.name}/$fileKeyFrom to ${bTo.name}/$fileKeyTo")
            s3ObjectFrom  = S3Object(bFrom, fileKeyFrom)
            s3ObjectTo    = S3Object(bTo, fileKeyTo)
            s3Put = awsS3.copy(s3ObjectFrom, s3ObjectTo)
        } yield s3Put
    } yield s3PutOpt
}