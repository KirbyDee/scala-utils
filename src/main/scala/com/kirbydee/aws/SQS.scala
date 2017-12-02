package com.kirbydee.aws

import javax.inject.Inject

import awscala.sqs.{Queue, SQS => AWSSQS}
import com.amazonaws.regions.Region
import com.amazonaws.services.sqs.model.SendMessageBatchResult
import com.amazonaws.{regions => awsregions}
import com.kirbydee.config.Configuration
import com.kirbydee.logr.Logr
import com.kirbydee.utils.Implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

class SQS @Inject()(config: Configuration) extends Logr {

    // create sqs object
    implicit val awsSQS: AWSSQS = AWSSQS(
        config.aws.key,
        config.aws.secret
    )(Region getRegion (awsregions.Regions fromName config.aws.region))

    /**
      * Get the SQS queue with given name.
      *
      * @param queueName The name of the queue
      * @return The future optional queue with name given
      */
    def queue(queueName: String): Future[Option[Queue]] = Future successful {
        debug(s"Create SQS Queue: $queueName")
        awsSQS queue queueName
    }

    /**
      * Apply a function to the queue.
      *
      * @param queue The SQS Queue
      * @param f The function to apply to the queue
      * @param executor The implicit Execution Context for handling futures
      * @tparam A The type of the function return value
      * @return The mapped future optional queue
      */
    private def withQueue[A](queue: SQSQueue)(f: Queue => A)(implicit executor: ExecutionContext): Future[Option[A]] = queue.queue map {
        case Some(q) =>
            Some(f(q))
        case None    =>
            warn(s"""SQS Queue with name "${queue.name}" does not exist: NOP""")
            None
    } recover  { case e =>
        error(e.getMessage, e)
        None
    }

    /**
      * With queue name given, receive "count" amount of messages and transform them with given function.
      *
      * @param queue The SQS queue to get the messages from
      * @param count The amount of messages returned by the queue
      * @param f The function to apply to the messages
      * @param executor The implicit Execution Context for handling futures
      * @tparam A The type of the function return value
      * @return The mapped future optional list of messages
      */
    def get[A](queue: SQSQueue, count: Int = 10)(f: SQSMessage => A)(implicit executor: ExecutionContext): Future[Option[List[A]]] = {
        @annotation.tailrec
        def go(q: Queue, n: Int, messages: List[SQSMessage] = Nil, tries: Int = 2): List[SQSMessage] = (n, tries) match {
            case  (LessOrEqualThanZero(), _) | (_, LessOrEqualThanZero()) => messages
            case (_, t)                                                   => awsSQS.receiveMessage(q, n) match {
                case Nil  => go(q, n, messages, t - 1)
                case list =>
                    debug(s"SQSQueue (${queue.name}) -> received Messages (${list.size})")
                    go(q, Math max (n - list.size, 0), messages ++ (list.toList > SQSMessage.apply))
            }
        }

        withQueue(queue)(q => go(q, count) > f)
    }

    /**
      * With queue name given, send a list of messages.
      *
      * @param queue The SQS queue to send the messages to
      * @param messages The messages to send
      * @param executor The implicit Execution Context for handling futures
      */
    def put(queue: SQSQueue)(messages: List[String])(implicit executor: ExecutionContext): Future[Option[SendMessageBatchResult]] = {
        messages match {
            // messages are available to send
            case NonEmpty() => withQueue(queue)(q => awsSQS sendMessages(q, messages >(_.base64Encode)))

            // no messages to send
            case _          => Future(None)
        }
    }
}