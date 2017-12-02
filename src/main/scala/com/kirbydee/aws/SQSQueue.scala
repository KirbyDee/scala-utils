package com.kirbydee.aws

import awscala.sqs.Queue
import com.amazonaws.services.sqs.model.SendMessageBatchResult
import com.kirbydee.config.Environment
import com.kirbydee.utils.Implicits._
import com.kirbydee.utils.ToString

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object SQSQueue {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * SQSQueue implicit class Helper
          *
          * @param name The name of the Queue
          */
        implicit class SQSQueueHelper(name: String) {

            /**
              * Creates an environment SQSQueue with the name given.
              *
              * @param sqs The implicit SQS object
              * @param env The implicit Environment
              * @return The created SQSQueue with the environment as prefix
              */
            @inline def sqsEnv(implicit sqs: SQS, env: Environment): SQSQueue =
                s"$env-$name" sqs

            /**
              * Creates a SQSQueue with the name given.
              *
              * @param sqs The implicit SQS object
              * @return The created SQSQueue
              */
            @inline def sqs(implicit sqs: SQS): SQSQueue =
                SQSQueue(name)
        }
    }
    object Implicits extends Implicits
}

case class SQSQueue(name: String)(implicit sqs: SQS) { q =>

    // the actual queue
    val queue: Future[Option[Queue]] = sqs queue name

    /**
      * Returns 1 message from the queue and maps over the function supplied.
      *
      * @param f The mapping function
      * @param executor The implicit Execution Context for handling futures
      * @return The future optional mapped SQS message
      */
    def >>[A](f: SQSMessage => A)(implicit executor: ExecutionContext): Future[Option[A]] =
        (>>>(f) | 1) > (_ >> (_.headOption))

    /**
      * Returns multiple message from the queue and maps over the function supplied.
      *
      * @param count The number of messages fetched from the SQS queue at the same time
      * @param executor The implicit Execution Context for handling futures
      * @return The future optional mapped list of SQS message
      */
    def >>>(count: Int)(implicit executor: ExecutionContext): Future[Option[List[SQSMessage]]] =
        >>>(m => m) | count

    /**
      * Returns multiple message from the queue and maps over the function supplied.
      *
      * @param f The mapping function
      * @param executor The implicit Execution Context for handling futures
      * @return The future optional mapped list of SQS message
      */
    def >>>[A](f: SQSMessage => A)(implicit executor: ExecutionContext) = new {

        /**
          * Adds the number of messages that are fetched at the same time from the SQS queue.
          *
          * @param count The number of messages fetched from the SQS queue at the same time
          * @return The future optional mapped list of SQS messages
          */
        def |(count: Int): Future[Option[List[A]]] =
            sqs.get(q, count)(f)
    }

    /**
      * Sends the message to the SQS queue.
      *
      * @param message The message to send
      * @param to The implicit ToString
      * @param executor The implicit Execution Context for handling futures
      */
    def <<[T](message: T)(implicit to: ToString[T], executor: ExecutionContext): Future[Option[SendMessageBatchResult]] =
        <<<(List(message))

    /**
      * Sends the messages to the SQS queue.
      *
      * @param message The first message to send
      * @param messages The rest messages to send
      * @param to The implicit ToString
      * @param executor The implicit Execution Context for handling futures
      */
    def <<<[T](message: T, messages: T*)(implicit to: ToString[T], executor: ExecutionContext): Future[Option[SendMessageBatchResult]] =
        <<<(message +: messages.toList)

    /**
      * Sends the list of messages to the SQS queue.
      *
      * @param messages The list of messages to send
      * @param to The implicit ToString
      * @param executor The implicit Execution Context for handling futures
      */
    def <<<[T](messages: List[T])(implicit to: ToString[T], executor: ExecutionContext): Future[Option[SendMessageBatchResult]] =
        sqs.put(q)(messages > to.parse)
}