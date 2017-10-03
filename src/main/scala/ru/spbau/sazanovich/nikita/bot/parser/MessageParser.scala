package ru.spbau.sazanovich.nikita.bot.parser

import org.joda.time.DateTime
import ru.spbau.sazanovich.nikita.bot.CalendarEvent
import ru.spbau.sazanovich.nikita.bot.parser.MessageParser.UserMessage

import scala.util.matching.Regex

/** Parses user input and returns corresponding [[UserMessage]]. */
class MessageParser(val requestDate: DateTime) extends DateTimeRangeParser with NameParser {

  import MessageParser._

  override def skipWhitespace = true

  override protected val whiteSpace: Regex = "[ \t\r\f]+".r

  override lazy val dateNow: DateTime = requestDate

  val createEventParser: Parser[CreateEventMessage] =
    "Create event" ~> dateTimeRangeParser ~ ("named" ~> nameParser) >> {
      case (startDateTime, endDateTime) ~ name =>
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
}

object MessageParser {

  /** Stands for a parsed user message. */
  trait UserMessage

  case class CreateEventMessage(calendarEvent: CalendarEvent) extends UserMessage

  case class WhatIsNext() extends UserMessage

  case class WhatAreNextEventsMessage(numberOfEvents: Int) extends UserMessage

  case class IncorrectMessage(message: String) extends UserMessage
}