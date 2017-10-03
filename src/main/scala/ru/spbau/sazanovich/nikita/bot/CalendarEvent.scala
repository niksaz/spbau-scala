package ru.spbau.sazanovich.nikita.bot

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import ru.spbau.sazanovich.nikita.bot.utils.DateTimeUtils

/** Represents an event stored in users' calendars. */
case class CalendarEvent(name: String, startDateTime: DateTime, endDateTime: DateTime) {

  import CalendarEvent._

  require(startDateTime.compareTo(endDateTime) <= 0)

  def transformToHumanReadableString(currentDate: DateTime): String = {
    val dateTimeFormatter = DateTimeFormat.forPattern("HH:mm")
    val startDateString = transformDateToHumanReadableString(currentDate, startDateTime)
    val startTimeString = dateTimeFormatter.print(startDateTime)
    val endDateString = transformDateToHumanReadableString(currentDate, endDateTime)
    val endTimeString = dateTimeFormatter.print(endDateTime)
    val resultBuilder = new StringBuilder()
    resultBuilder.append(
      if (startDateString == endDateString) {
        // If the event happens to be on the same day, then we mention the day only once.
        startDateString + " from " + startTimeString + " to " + endTimeString
      } else {
        // If the event start and end dates are on different dates, then we should mention them
        // both.
        "from " + startTimeString + " " + startDateString +
          " to " + endDateString + " " + endTimeString
      })
    resultBuilder.append(" named " + "\"" + name + "\"").toString
  }

}

object CalendarEvent {

  private def transformDateToHumanReadableString(
      currentDate: DateTime, futureDate: DateTime): String = {
    assert(currentDate.compareTo(futureDate) < 0)
    if (DateTimeUtils.areOnTheSameDay(currentDate, futureDate)) {
      "today"
    } else if (DateTimeUtils.areOnTheSameDay(currentDate.plusDays(1), futureDate)) {
      "tomorrow"
    } else {
      val dateTimeFormatter = DateTimeFormat.forPattern("dd-MM-yyyy")
      dateTimeFormatter.print(futureDate)
    }
  }
}
