package com.kirbydee.logr

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.filter.Filter
import ch.qos.logback.core.spi.FilterReply
import ch.qos.logback.core.spi.FilterReply._

class EventThrottleFilter extends Filter[ILoggingEvent] {

    /*
     * Throttle times
     */
    private val throttleModeTriggerTimeMilliSecs: Long = 6000
    private val throttledEmailIntervalMilliSecs: Long = 900000
    private val throttleModeStopTimeMilliSecs: Long = 3600000

    /*
     * Stateful variables which are keeping track of throttling
     */
    private var lastTriggerTime: Long = 0
    private var lastEventTime: Long = 0
    private var inThrottleMode: Boolean = false

    // Level threshold
    private var level: Level = Level.ERROR

    /**
      * Return true if event passed as the event is NOT throttled,
      * false otherwise.
      */
    override def decide(event: ILoggingEvent): FilterReply = {
        // if event level is less than ERROR we do NOT trigger the event
        if (!levelThreshold(event)) return DENY

        // check filter
        if (filterOut(event)) return DENY

        // if in throttle mode already AND throttle should be disabled, we turn off throttle mode AND trigger the event
        if (this.inThrottleMode && shouldDisableThrottle(event)) {
            this.inThrottleMode = false
            return triggeringEvent(event)
        }

        // (so far we have event level ERROR): if we are in throttle mode, check throttle time
        if (this.inThrottleMode) return isThrottleTimeExceeded(event)

        // (so far we have event level ERROR BUT throttle is enabled): if throttle should be enabled, do it and do NOT trigger the event
        if (shouldEnableThrottle(event)) {
            this.inThrottleMode = true
            return notTriggeringEvent(event)
        }

        // everything is "fine": trigger the event
        triggeringEvent(event)
    }

    def filterOut(event: ILoggingEvent): Boolean =
        false

    def levelThreshold(event: ILoggingEvent): Boolean =
        event.getLevel.isGreaterOrEqual(this.level)

    def shouldEnableThrottle(event: ILoggingEvent): Boolean =
        this.lastEventTime + this.throttleModeTriggerTimeMilliSecs > event.getTimeStamp

    def isThrottleTimeExceeded(event: ILoggingEvent): FilterReply = {
        if (this.lastTriggerTime + this.throttledEmailIntervalMilliSecs < event.getTimeStamp) return triggeringEvent (event)
        notTriggeringEvent(event)
    }

    def shouldDisableThrottle(event: ILoggingEvent): Boolean =
        this.lastEventTime + this.throttleModeStopTimeMilliSecs < event.getTimeStamp

    def triggeringEvent(event: ILoggingEvent): FilterReply = {
        this.lastEventTime = event.getTimeStamp
        this.lastTriggerTime = this.lastEventTime
        ACCEPT
    }

    def notTriggeringEvent(event: ILoggingEvent): FilterReply = {
        this.lastEventTime = event.getTimeStamp
        DENY
    }

    def setLevel(level: Level): Unit =
        this.level = level
}
