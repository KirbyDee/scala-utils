package com.kirbydee.logr

import ch.qos.logback.classic.spi.{ILoggingEvent, IThrowableProxy}
import com.kirbydee.utils.StringUtils
import com.kirbydee.utils.Implicits._

import scala.language.postfixOps

trait Converter {

    /**
      * Converts the logging event provided to a readable string according to the implementation.
      *
      * @param event The logging event to convert
      * @return The Optional String of the event
      */
    def convert(implicit event: ILoggingEvent): Option[String]
}

object HashPatternConverter {

    def apply(): HashPatternConverter =
        new HashPatternConverter
}

class HashPatternConverter extends Converter {

    /**
      * Converts the logging event provided to a Hash.
      *
      * @param event The logging event to convert
      * @return The Optional Hash of the event
      */
    override def convert(implicit event: ILoggingEvent): Option[String] = Some {
        StringUtils.sha1Digest {
            Option(event.getThrowableProxy) match {
                // if there is a stacktrace, we take it has hash
                case Some(_)    => toStackHash

                // if there is no stacktrace, the hash consists out of the level, logger name and message
                case None       => toNonStackHash
            }
        }
    }

    /**
      * Maps the logging event to a stack based Hash.
      *
      * @param event The logging event to convert
      * @return The Hash of the stacktrace of the event
      */
    private def toStackHash(implicit event: ILoggingEvent): String =
        StacktracePatternConverter().convert(event) | toNonStackHash

    /**
      * Maps the logging event to a non-stack based Hash.
      *
      * @param event The logging event to convert
      * @return The Hash of the loggerLevel, loggerName and loggerMessage of the event
      */
    private def toNonStackHash(implicit event: ILoggingEvent): String =
        s"${event.getLevel}${event.getLoggerName}${event.getMessage}"
}

object StacktracePatternConverter {

    def apply(): StacktracePatternConverter =
        new StacktracePatternConverter
}

class StacktracePatternConverter extends Converter {

    /**
      * Converts the logging event provided to a Stacktrace.
      *
      * @param event The logging event to convert
      * @return The Optional Stacktrace of the event
      */
    override def convert(implicit event: ILoggingEvent): Option[String] = {

        @annotation.tailrec
        def go(ex: (Option[String], Option[IThrowableProxy], Set[IThrowableProxy])): (Option[String], Option[IThrowableProxy], Set[IThrowableProxy]) = ex match {
            case (stackOp, Some(t), set) if !set.contains(t)    =>
                val newStackPrefix = s"${stackOp > (stack => s"\n$stack") | ""}${t.getClassName}: ${t.getMessage}\n"
                val newStack = t.getStackTraceElementProxyArray.map(_.getSTEAsString).foldLeft(newStackPrefix)((a, b) => s"$a    $b\n")
                go(Some(newStack), Option(t.getCause), set + t)
            case (stack, t, set)                                =>
                (stack, t, set)
        }

        // build stacktrace
        go((None, Option(event.getThrowableProxy), Set.empty))._1
    }
}