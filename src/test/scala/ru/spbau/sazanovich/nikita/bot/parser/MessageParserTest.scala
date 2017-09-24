package ru.spbau.sazanovich.nikita.bot.parser

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.scalatest.FunSuite
import ru.spbau.sazanovich.nikita.bot.CalendarEvent

class MessageParserTest extends FunSuite {

  val dateFormatter: DateTimeFormatter = DateTimeFormat.forPattern("dd-MM-yyyy HH:mm")

  test("parseCreateEvent") {
     val result =
       MessageParser.parse(
         "Create event from 30-05-1998 10:00 to 30-05-1998 12:00 named \"Surprise\"")
    val event =
      CalendarEvent(
        "Surprise",
        DateTime.parse("30-05-1998 10:00", dateFormatter),
        DateTime.parse("30-05-1998 12:00", dateFormatter))
    assert(result == MessageParser.CreateEventMessage(event))
  }

  test("parseCreateEventWithEndTimeBeforeStartTime") {
    val result =
      MessageParser.parse(
        "Create event from 30-05-1998 12:00 to 30-05-1998 10:00 named \"Surprise\"")
    assert(result == MessageParser.IncorrectMessage("Incorrect message. :("))
  }

  test("parseWhatIsNext") {
    val result = MessageParser.parse("What is next?")
    assert(result == MessageParser.WhatIsNext())
  }

  test("parseWhatAreNext") {
    val result = MessageParser.parse("What are next 5 events?")
    assert(result == MessageParser.WhatAreNextEventsMessage(5))
  }
}
