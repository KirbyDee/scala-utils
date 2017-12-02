package com.kirbydee.logr

import play.api.Logger
import com.kirbydee.utils.Ternary.Implicits._

import scala.language.postfixOps

trait Logr {

    // the play this.logger
    private val logger: Logger = Logger(this.getClass)

    /**
      * Logs a message with the `TRACE` level.
      *
      * @param message the message to log
      */
    def trace(message: => String): Unit =
        this.logger.isTraceEnabled? (this.logger trace message) end

    /**
      * Logs a message with the `TRACE` level.
      *
      * @param message the message to log
      * @param throwable the associated exception
      */
    def trace(message: => String, throwable: => Throwable): Unit =
        this.logger.isTraceEnabled? (this.logger trace (message, throwable)) end

    /**
      * Logs a message with the `DEBUG` level.
      *
      * @param message the message to log
      */
    def debug(message: => String): Unit =
        this.logger.isDebugEnabled? (this.logger debug message) end

    /**
      * Logs a message with the `DEBUG` level.
      *
      * @param message the message to log
      * @param throwable the associated exception
      */
    def debug(message: => String, throwable: => Throwable): Unit =
        this.logger.isDebugEnabled? (this.logger debug (message, throwable)) end

    /**
      * Logs a message with the `INFO` level.
      *
      * @param message the message to log
      */
    def info(message: => String): Unit =
        this.logger.isInfoEnabled? (this.logger info message) end

    /**
      * Logs a message with the `INFO` level.
      *
      * @param message the message to log
      * @param throwable the associated exception
      */
    def info(message: => String, throwable: => Throwable): Unit =
        this.logger.isInfoEnabled? (this.logger info (message, throwable)) end

    /**
      * Logs a message with the `WARN` level.
      *
      * @param message the message to log
      */
    def warn(message: => String): Unit =
        this.logger.isWarnEnabled? (this.logger warn message) end

    /**
      * Logs a message with the `WARN` level.
      *
      * @param message the message to log
      * @param throwable the associated exception
      */
    def warn(message: => String, throwable: => Throwable): Unit =
        this.logger.isWarnEnabled? (this.logger warn (message, throwable)) end

    /**
      * Logs a message with the `ERROR` level.
      *
      * @param message the message to log
      */
    def error(message: => String): Unit =
        this.logger.isErrorEnabled? (this.logger error message) end

    /**
      * Logs a message with the `ERROR` level.
      *
      * @param message the message to log
      * @param throwable the associated exception
      */
    def error(message: => String, throwable: => Throwable): Unit =
        this.logger.isErrorEnabled? (this.logger error (message, throwable)) end
}
