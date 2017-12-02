package com.kirbydee.exec

import com.kirbydee.logr.Logr

import scala.concurrent.{ExecutionContext, Future}

trait Executor[A] extends Logr {

    // Name of the Executor
    protected val name: String = this.getClass.getSimpleName

    /**
      * Executes the Manager (called from Akka Scheduler).
      * Execute method ALWAYS returns a Future successful (has recover inside)
      */
    final def execute(implicit executor: ExecutionContext): Future[Option[A]] = {
        debug(s"Start Executor $name")
        executeImpl recover {
            case e => failed(e); None
        }
    }

    /**
      * Override this method to execute the Manager
      */
    protected def executeImpl(implicit executor: ExecutionContext): Future[Option[A]]

    /**
      * Logs failed Executor error.
      */
    protected def failed(e: Throwable): Unit =
        failed("", e)

    /**
      * Logs failed Executor error with given message.
      */
    protected def failed(message: => String, e: Throwable): Unit =
        error(s"Failed Executor $name: $message ${e.getMessage}", e)

    /**
      * Logs failed Executor error.
      */
    protected def failed(message: => String): Unit =
        error(s"Failed Executor $name: $message")
}
case object Execute