package com.kirbydee.aws

import awscala.sqs.Message
import com.kirbydee.utils.Implicits._
import play.api.libs.json.{JsObject, Json, Reads}

case class SQSMessage(message: Message) {

    // The pointer to the message in the queue
    val receiptHandle: String = message.getReceiptHandle

    // The body as json
    val bodyJs: JsObject = (Json parse message.getBody.base64Decode).as[JsObject]

    /**
      * Destroy the SQS Message, thus removing it from the Queue.
      *
      * @param sqs The implicit SQS object
      */
    def destroy(implicit sqs: SQS): Unit =
        message.destroy()(sqs.awsSQS)

    /**
      * Transforms the SQS Message to json.
      */
    def toJson: JsObject =
        bodyJs ++ Json.obj("receiptHandle" -> receiptHandle)

    /**
      * Gets the value with the key supplied from the body of the message.
      *
      * @param key The JSON key
      * @param jsRead The implicit Json Reader
      * @tparam T The type of the value
      * @return The optional value of the key, if existing
      */
    def get[T](key: String)(implicit jsRead: Reads[T]): Option[T] =
        (bodyJs \ key).asOpt[T]
}