package ru.spbau.sazanovich.nikita.bot

import org.joda.time.DateTime

/** Represents an event stored in users' calendars. */
case class CalendarEvent(name: String, startDateTime: DateTime, endDateTime: DateTime) {

  require(startDateTime.compareTo(endDateTime) <= 0)

  override def toString: String =
    "from " + startDateTime.toString + " to " + endDateTime.toString +
      " named " + "\"" + name + "\""
}
