package ru.spbau.sazanovich.nikita.bot.parser

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import ru.spbau.sazanovich.nikita.bot.CalendarEvent
import MessageParser.UserMessage


/** Parses user input and returns corresponding [[UserMessage]]. */
class MessageParser extends RegexParsers {

  import MessageParser._

  override def skipWhitespace = true

  override protected val whiteSpace: Regex = "[ \t\r\f]+".r

  val dateFormatter: DateTimeFormatter = DateTimeFormat.forPattern("dd-MM-yyyyHH:mm")
  val dateParser: Parser[String] = "[0-9]{2}-[0-9]{2}-[0-9]{4}".r ^^ (date => date)
  val timeParser: Parser[String] = "[0-9]{2}:[0-9]{2}".r ^^ (date => date)

  val dateTimeParser: Parser[DateTime] =
    dateParser ~ timeParser ^^ {
      case date ~ time => DateTime.parse(date + time, dateFormatter)
    }

  val quoteParser: Parser[String] = "\"".r ^^ (quote => quote)
  val messageWithoutQuotesParser: Parser[String] = "[^\"]+".r ^^ (quote => quote)

  val nameParser: Parser[String] =
    quoteParser ~> (messageWithoutQuotesParser <~ quoteParser) ^^ (name => name)

  val createEventParser: Parser[CreateEventMessage] =
    "Create event from" ~> dateTimeParser ~ (
      "to" ~> dateTimeParser ~ ("named" ~> nameParser)) ^? {
        case startDateTime ~ (endDateTime ~ name) if startDateTime.compareTo(endDateTime) <= 0 =>
          CreateEventMessage(CalendarEvent(name, startDateTime, endDateTime))
      }

  val whatIsNext: Parser[WhatIsNext] =
    "What is next?" ^^ (_ => WhatIsNext())

  val eventsParser: Parser[String] = "events?".r ^^ (word => word)
  val intParser: Parser[Int] = "[1-9][0-9]*".r ^^ (_.toInt)

  val whatAreNext: Parser[WhatAreNextEventsMessage] =
    "What are next" ~> (intParser <~ eventsParser) ^^
      (numberOfEvents => WhatAreNextEventsMessage(numberOfEvents))

  val userMessageParser: Parser[UserMessage] =
    createEventParser | whatIsNext | whatAreNext
}

object MessageParser extends MessageParser {

  def parse(text: String): UserMessage = {
    parse(userMessageParser, text) match {
      case Success(userMessage, _) => userMessage
      case _ => IncorrectMessage("Incorrect message. :(")
    }
  }

  /** Stands for a parsed user message. */
  trait UserMessage

  case class CreateEventMessage(calendarEvent: CalendarEvent) extends UserMessage

  case class WhatIsNext() extends UserMessage

  case class WhatAreNextEventsMessage(numberOfEvents: Int) extends UserMessage

  case class IncorrectMessage(message: String) extends UserMessage
}
