package com.kirbydee.utils

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{implicitConversions, postfixOps}

trait Timer {

    trait TimeUnit {

        // time unit
        val unit: String

        /**
          * Formats the Nanoseconds in the Unit desired.
          *
          * @param t The time to format
          * @return The formatted time
          */
        def format(t: Long): String =
            s"${"%.3f".format(convert(t))}$unit"

        /**
          * Converts the Nanoseconds in the Unit desired.
          *
          * @param t The time to convert
          * @return The converted time
          */
        def convert(t: Long): Double
    }
    case object NanoSecond  extends TimeUnit {
        override val unit: String = "ns"
        override def convert(t: Long): Double = t.toDouble
    }
    case object MicroSecond extends TimeUnit {
        override val unit: String = "μs"
        override def convert(t: Long): Double = t.toDouble / 1000
    }
    case object MilliSecond extends TimeUnit {
        override val unit: String = "ms"
        override def convert(t: Long): Double = t.toDouble / 1000000
    }
    case object Second      extends TimeUnit {
        override val unit: String = "s"
        override def convert(t: Long): Double = t.toDouble / 1000000000
    }

    /**
      * Times the bock given with default name "Timer"
      * and prints the time in nanoseconds in stdout.
      *
      * @param block The block to time
      * @tparam R The response type of the block
      * @return The response of the ran block
      */
    def time[R](block: => R): R =
        time("Timer", MilliSecond)(block)

    /**
      * Times the bock and name given
      * and prints the time in nanoseconds in stdout.
      *
      * @param timeUnit Unit to show
      * @param name The name of the Timer
      * @param block The block to time
      * @tparam R The response type of the block
      * @return The response of the ran block
      */
    def time[R](name: String, timeUnit: TimeUnit)(block: => R): R = {
        val t0 = System.nanoTime()
        try {
            block
        } finally {
            val t1 = System.nanoTime()
            printTime(name)(t0, t1, timeUnit)
        }
    }

    /**
      * Times the future and name given
      * and prints the time in nanoseconds in stdout.
      *
      * @param future The future to time
      * @param executor The implicit Execution Context for handling futures
      * @tparam R The response type of the future
      * @return The given future
      */
    def timeFuture[R](future: Future[R])(implicit executor: ExecutionContext): Future[R] =
        timeFuture("Timer", MilliSecond)(future)

    /**
      * Times the future with default name "Timer"
      * and prints the time in nanoseconds in stdout.
      *
      * @param timeUnit Unit to show
      * @param name The name of the Timer
      * @param future The future to time
      * @param executor The implicit Execution Context for handling futures
      * @tparam R The response type of the future
      * @return The given future
      */
    def timeFuture[R](name: String, timeUnit: TimeUnit)(future: Future[R])(implicit executor: ExecutionContext): Future[R] = {
        val t0 = System.nanoTime()
        future onComplete {
            case _ =>
                val t1 = System.nanoTime()
                printTime(name)(t0, t1, timeUnit)
        }
        future
    }

    private def printTime(name: String)(t0: Long, t1: Long, timeUnit: TimeUnit): Unit =
        println(f"($name) Elapsed time: ${timeUnit.format(t1 - t0)}")
}