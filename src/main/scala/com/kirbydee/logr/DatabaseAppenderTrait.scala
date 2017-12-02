package com.kirbydee.logr

import java.net.InetAddress
import java.sql.{Connection, DriverManager}

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.UnsynchronizedAppenderBase
import org.joda.time.DateTime

import scala.language.postfixOps

trait DatabaseAppenderTrait extends UnsynchronizedAppenderBase[ILoggingEvent] {

    // db connection
    val url: String
    val user: String
    val password: String
    val driver: String
    lazy val connection: Connection = {
        Class forName driver
        DriverManager getConnection (url, user, password)
    }

    // The name of this server (host name)
    val hostName: String = InetAddress.getLocalHost.getHostName

    // The insert statement
    val insertStatement: String = "INSERT INTO play_logs (level, logger, message, stacktrace, hash, date_entered, hostname) VALUES (?, ?, ?, ?, ?, ?, ?)"

    /**
      * Append log event to the queue.
      *
      * @param event The event to log
      */
    override def append(event: ILoggingEvent): Unit = {
        // create insert statement
        val prepareStatement = connection prepareStatement insertStatement
        prepareStatement.setString(1, event.getLevel.toString)
        prepareStatement.setString(2, event.getLoggerName)
        prepareStatement.setString(3, event.getMessage)
        prepareStatement.setString(4, StacktracePatternConverter() convert event orNull)
        prepareStatement.setString(5, HashPatternConverter() convert event getOrElse "")
        prepareStatement.setString(6, new DateTime(event.getTimeStamp).toString("yyyy-MM-dd hh:mm:ss"))
        prepareStatement.setString(7, this.hostName)

        // execute statement
        prepareStatement executeUpdate
    }
}