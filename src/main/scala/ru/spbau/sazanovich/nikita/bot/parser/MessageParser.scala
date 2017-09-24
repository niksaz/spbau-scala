package ru.spbau.sazanovich.nikita.bot.parser

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import ru.spbau.sazanovich.nikita.bot.CalendarEvent
import ru.spbau.sazanovich.nikita.bot.parser.MessageParser.UserMessage

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/** Parses user input and returns corresponding [[UserMessage]]. */
object MessageParser extends RegexParsers {

  override def skipWhitespace = true

  override protected val whiteSpace: Regex = "[ \t\r\f]+".r

  val dateFormatter: DateTimeFormatter = DateTimeFormat.forPattern("dd-MM-yyyyHH:mm")
  val dateParser: Parser[String] = "[0-9]{2}-[0-9]{2}-[0-9]{4}".r
  val timeParser: Parser[String] = "[0-9]{2}:[0-9]{2}".r

  val dateTimeParser: Parser[DateTime] =
    dateParser ~ timeParser ^^ {
      case date ~ time => DateTime.parse(date + time, dateFormatter)
    }

  val nameParser: Parser[String] = "\"" ~> ("[^\"]+".r <~ "\"")

  val createEventParser: Parser[CreateEventMessage] =
    "Create event from" ~> dateTimeParser ~ (
      "to" ~> dateTimeParser ~ ("named" ~> nameParser)) >> {
      case startDateTime ~ (endDateTime ~ name) =>
        if (startDateTime.compareTo(endDateTime) <= 0) {
          success(CreateEventMessage(CalendarEvent(name, startDateTime, endDateTime)))
        } else {
          // Raising an error, since there is no need to try other parsers -- the error is semantic.
          err("The end time happens before start time. :?")
        }
    }

  val whatIsNextParser: Parser[WhatIsNext] =
    "What is next?" ^^ (_ => WhatIsNext())

  val intParser: Parser[Int] = "[1-9][0-9]*".r ^^ (_.toInt)

  val whatAreNextParser: Parser[WhatAreNextEventsMessage] =
    "What are next" ~> (intParser <~ "events?") ^^
      (numberOfEvents => WhatAreNextEventsMessage(numberOfEvents))

  val userMessageParser: Parser[UserMessage] =
    createEventParser | whatIsNextParser | whatAreNextParser

  def parse(text: String): UserMessage = {
    parse(userMessageParser, text) match {
      case Success(userMessage, _) => userMessage
      case Error(message, _) => IncorrectMessage(message)
      case _ => IncorrectMessage("I do not understand you. :(")
    }
  }

  /** Stands for a parsed user message. */
  trait UserMessage

  case class CreateEventMessage(calendarEvent: CalendarEvent) extends UserMessage

  case class WhatIsNext() extends UserMessage

  case class WhatAreNextEventsMessage(numberOfEvents: Int) extends UserMessage

  case class IncorrectMessage(message: String) extends UserMessage
}
