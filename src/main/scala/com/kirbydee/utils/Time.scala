package com.kirbydee.utils

import com.kirbydee.utils.Extractor.Implicits._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.concurrent.duration.{Duration, _}
import scala.language.{implicitConversions, postfixOps}

object Time {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /*
         * val assignments for Extensions for easier access
         */
        val now       = DateTime.now()
        val today     = now
        val yesterday = now - (1 day)
        val tomorrow  = now + (1 day)
        val Year      = com.kirbydee.utils.Year
        val Month     = com.kirbydee.utils.Month
        val Day       = com.kirbydee.utils.Day
        val Hour      = com.kirbydee.utils.Hour
        val Minute    = com.kirbydee.utils.Minute
        val Second    = com.kirbydee.utils.Second

        /**
          * Implicit Helper Class for Times in format of String.
          *
          * @param stringTime The time as String
          */
        implicit class StringTimeHelper(stringTime: String) {

            /**
              * Parses the string given to a DateTime
              */
            @inline def time: DateTime =
                DateTime.parse(stringTime)
        }

        /**
          * Implicit Helper Class for DateTimes.
          *
          * @param dateTime The time as DateTime
          */
        implicit class DateTimeHelper(dateTime: DateTime) {

            /**
              * Gets the Time of Day from the DateTime
              *
              * @return TimeOfDay
              */
            @inline def timeOfDay: TimeOfDay = TimeOfDay(
                Hour(dateTime.getHourOfDay),
                Minute(dateTime.getMinuteOfHour),
                Second(dateTime.getSecondOfMinute)
            )

            /**
              * Formats the DateTime to a string "yyyy-MM-dd hh:mm:ss".
              *
              * @return The formatted String representation of the DateTime
              */
            @inline def format: String =
                format("yyyy-MM-dd hh:mm:ss")

            /**
              * Formats the DateTime to a string "yyyy-MM-dd".
              *
              * @return The formatted String representation of the DateTime
              */
            @inline def formatShort: String =
                format("yyyy-MM-dd")

            /**
              * Formats the DateTime to a string.
              *
              * @param pattern The pattern to format
              * @return The formatted String representation of the DateTime
              */
            @inline def format(pattern: String): String =
                DateTimeFormat.forPattern(pattern).print(dateTime)

            /**
              * DateTime plus given duration.
              *
              * @param duration The duration to add
              * @return The newly DateTime
              */
            @inline def +(duration: Duration): DateTime =
                dateTime plus duration.toMillis

            /**
              * DateTime minus given duration.
              *
              * @param duration The duration to subtract
              * @return The newly DateTime
              */
            @inline def -(duration: Duration): DateTime =
                dateTime minus duration.toMillis

            /**
              * Checks, if the first DateTime is the same than the second one.
              *
              * @param dateTime2 The second DateTime
              * @return True, if the first DateTime is the same (same date), false otherwise
              */
            @inline def ===(dateTime2: DateTime): Boolean =
                compare(dateTime2)(_ == _)

            /**
              * Checks, if the first DateTime is greater than the second one.
              *
              * @param dateTime2 The second DateTime
              * @return True, if the first DateTime is greater (newer date), false otherwise
              */
            @inline def >(dateTime2: DateTime): Boolean =
                compare(dateTime2)(_ > _)

            /**
              * Checks, if the first DateTime is greater or equal than the second one.
              *
              * @param dateTime2 The second DateTime
              * @return True, if the first DateTime is greater or equal (newer date, or same), false otherwise
              */
            @inline def >=(dateTime2: DateTime): Boolean =
                compare(dateTime2)(_ >= _)

            /**
              * Checks, if the first DateTime is smaller than the second one.
              *
              * @param dateTime2 The second DateTime
              * @return True, if the first DateTime is smaller (older date), false otherwise
              */
            @inline def <(dateTime2: DateTime): Boolean =
                compare(dateTime2)(_ < _)

            /**
              * Checks, if the first DateTime is smaller or equal than the second one.
              *
              * @param dateTime2 The second DateTime
              * @return True, if the first DateTime is smaller or equal (older date, or same), false otherwise
              */
            @inline def <=(dateTime2: DateTime): Boolean =
                compare(dateTime2)(_ <= _)

            /**
              * Compares the current DateTime with the DateTime and the comparison function given.
              *
              * @param dateTime2 The second DateTime
              * @param f The comparison function
              * @return True, if the comparison holds, false otherwise
              */
            @inline def compare(dateTime2: DateTime)(f: (Long, Long) => Boolean): Boolean =
                f(dateTime.getMillis, dateTime2.getMillis)
        }
    }
    object Implicits extends Implicits
}


trait Time[T <: Time[T]] {

    // The value of the Time
    val value: Int

    /**
      * Checks if THIS time is after the given one.
      *
      * @param time The time to check with
      * @return True if THIS is after, false otherwise
      */
    def isAfter(time: T): Boolean =
        value > time.value

    /**
      * Checks if THIS time is before the given one.
      *
      * @param time The time to check with
      * @return True if THIS is before, false otherwise
      */
    def isBefore(time: T): Boolean =
        value < time.value

    /**
      * Checks if THIS time is equal the given one.
      *
      * @param time The time to check with
      * @return True if THIS is equal, false otherwise
      */
    def isEqual(time: T): Boolean =
        value == time.value
}
case class Year(override val value: Int) extends Time[Year] { y =>

    /**
      * Creates a DateTime with given year, month and day.
      *
      * @param m The month
      */
    def /(m: Month): Object {def /(d: Day): DateTime} = new {

        /**
          * Creates a DateTime with given year, month and day.
          *
          * @param d The day
          */
        def /(d: Day): DateTime =
            new DateTime(y.value, m.value, d.value, 0, 0)
    }
}
case class Month(override val value: Int) extends Time[Month]
case class Day(value: Int) extends Time[Day]

object TimeOfDay {

    // the regex to get the time
    val TIME = "(2[0-3]|1[0-9]|0?[0-9]):([0-5][0-9]|0?[0-9]):([0-5][0-9]|0?[0-9])"r

    /**
      * Tries to get the TimeOfDay object from the string given.
      *
      * @param time The time as string
      * @return The optional TimeOfDay
      */
    def from(time: String): Option[TimeOfDay] = time match {
        case TIME(int(h), int(m), int(s)) => Some(TimeOfDay(Hour(h), Minute(m), Second(s)))
        case _                            => None
    }
}
case class TimeOfDay(hour: Hour, minute: Minute, second: Second) {

    // converts the time of the day into milliseconds
    def toMilli: Long =
        hour.toMilli + minute.toMilli + second.toMilli

    // checks if THIS time of the day is after the given one
    def isAfter(timeOfDay: TimeOfDay): Boolean =
        toMilli > timeOfDay.toMilli

    // subtracts THIS time of the day with the given one, and returns it in milliseconds
    def -(timeOfDay: TimeOfDay): Long =
        toMilli - timeOfDay.toMilli

    // adds of THIS time of the day with the given one, and returns it in milliseconds
    def +(timeOfDay: TimeOfDay): Long =
        toMilli + timeOfDay.toMilli
}
case class Hour(override val value: Int) extends Time[Hour] {

    // converts the hour into milliseconds
    def toMilli: Long =
        value * 60 * 60 * 1000
}
case class Minute(override val value: Int) extends Time[Minute] {

    // converts the minute into milliseconds
    def toMilli: Long =
        value * 60 * 1000
}
case class Second(override val value: Int) extends Time[Second] {

    // converts the second into milliseconds
    def toMilli: Long =
        value * 1000
}