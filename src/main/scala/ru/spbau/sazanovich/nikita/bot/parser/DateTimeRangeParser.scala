package ru.spbau.sazanovich.nikita.bot.parser

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/** Parses range of [[org.joda.time.DateTime]].
  * Does not verify the semantic meaning of the range, i.e. the start time can happen after end
  * time.
  */
trait DateTimeRangeParser extends RegexParsers {

  import DateTimeRangeParser._

  override def skipWhitespace = true

  override protected val whiteSpace: Regex = "[ \t\r\f]+".r

  lazy val dateNow: DateTime = null

  private val todayDateParser: Parser[DateTime] = "today" ^^ {
    // We ignore time later: only date of [[DateTime]] is used.
    _ => dateNow
  }

  private val tomorrowDateParser: Parser[DateTime] = "tomorrow" ^^ {
    // We ignore time later: only date of [[DateTime]] is used.
    _ => dateNow.plusDays(1)
  }

  private val genericDateParser: Parser[DateTime] = "[0-9]{2}-[0-9]{2}-[0-9]{4}".r >> {
    dateString =>
      try {
        val date = DateTime.parse(dateString, dateFormatter)
        success(date)
      } catch {
        case _: IllegalArgumentException => failure("Incorrect date format.")
      }
  }

  /** Parses the date, i.e. DateTime's dd-MM-yyyy part. */
  private val dateParser: Parser[DateTime] =
    todayDateParser | tomorrowDateParser | genericDateParser

  /** Parses the hours and minutes, i.e. DateTime's HH:mm part. */
  private val timeParser: Parser[DateTime] = "[0-9]{2}:[0-9]{2}".r >> {
    timeString =>
      try {
        val time = DateTime.parse(timeString, timeFormatter)
        success(time)
      } catch {
        case _: IllegalArgumentException => failure("Incorrect time format.")
      }
  }

  private val prefixDateTimeRangeParser: Parser[(DateTime, DateTime)] =
    dateParser ~ ("from" ~> timeParser ~ ("to" ~> timeParser)) ^^ {
      case date ~ (startTime ~ endTime) =>
        val startDateTime = dateAndTimeToDateTime(date, startTime)
        val endDateTime = dateAndTimeToDateTime(date, endTime)
        (startDateTime, endDateTime)
    }

  private val infixDateTimeRangeParser: Parser[(DateTime, DateTime)] =
    "from" ~> timeParser ~ (dateParser ~ ("to" ~> (timeParser ~ dateParser))) ^^ {
      case startTime ~ (startDate ~ (endTime ~ (endDate))) =>
        val startDateTime = dateAndTimeToDateTime(startDate, startTime)
        val endDateTime = dateAndTimeToDateTime(endDate, endTime)
        (startDateTime, endDateTime)
    }

  val dateTimeRangeParser: Parser[(DateTime, DateTime)] =
    prefixDateTimeRangeParser | infixDateTimeRangeParser
}

object DateTimeRangeParser {

  private val dateFormatter: DateTimeFormatter = DateTimeFormat.forPattern("dd-MM-yyyy")

  private val timeFormatter: DateTimeFormatter = DateTimeFormat.forPattern("HH:mm")

  private val dateTimeFormatter: DateTimeFormatter = DateTimeFormat.forPattern("dd-MM-yyyyHH:mm")

  private def dateAndTimeToDateTime(date: DateTime, time: DateTime): DateTime = {
    DateTime.parse(dateFormatter.print(date) + timeFormatter.print(time), dateTimeFormatter)
  }
}
