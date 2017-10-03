package ru.spbau.sazanovich.nikita.bot.parser

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.scalatest.{BeforeAndAfter, FunSuite}
import ru.spbau.sazanovich.nikita.bot.CalendarEvent

class MessageParserTest extends FunSuite with BeforeAndAfter {

  val dateFormatter: DateTimeFormatter = DateTimeFormat.forPattern("dd-MM-yyyy HH:mm")

  val testDateTime: DateTime = DateTime.parse("30-05-1998 09:00", dateFormatter)

  var messageParser: MessageParser = _

  before {
    messageParser = new MessageParser(testDateTime)
  }

  test("parseCreateEventByProvidingPrefixDateTimeRange") {
     val result =
       messageParser.parse(
         "Create event today from 10:00 to 12:00 named \"Surprise\"")
    val event =
      CalendarEvent(
        "Surprise",
        DateTime.parse("30-05-1998 10:00", dateFormatter),
        DateTime.parse("30-05-1998 12:00", dateFormatter))
    assert(result == MessageParser.CreateEventMessage(event))
  }

  test("parseCreateEventByProvidingInfixDateTimeRange") {
    val result =
      messageParser.parse(
        "Create event from 10:00 today to 12:00 05-06-1998 named \"Holidays\"")
    val event =
      CalendarEvent(
        "Holidays",
        DateTime.parse("30-05-1998 10:00", dateFormatter),
        DateTime.parse("05-06-1998 12:00", dateFormatter))
    assert(result == MessageParser.CreateEventMessage(event))
  }

  test("parseCreateEventWithEndTimeBeforeStartTime") {
    val result =
      messageParser.parse(
        "Create event from 12:00 30-05-1998 to 10:00 30-05-1998 named \"Surprise\"")
    assert(result == MessageParser.IncorrectMessage("The end time happens before start time. :?"))
  }

  test("parseCreateEventWithIncorrectDate") {
    val result = messageParser.parse(
      "Create event from 80-05-1998 12:00 to 90-05-1998 10:00 named \"Surprise\"")
    assert(result == MessageParser.IncorrectMessage("I do not understand you. :("))
  }

  test("parseWhatIsNext") {
    val result = messageParser.parse("What is next?")
    assert(result == MessageParser.WhatIsNext())
  }

  test("parseWhatAreNext") {
    val result = messageParser.parse("What are next 5 events?")
    assert(result == MessageParser.WhatAreNextEventsMessage(5))
  }

  test("parseIncorrectMessage") {
    val result = messageParser.parse("What should I do?")
    assert(result == MessageParser.IncorrectMessage("I do not understand you. :("))
  }
}
