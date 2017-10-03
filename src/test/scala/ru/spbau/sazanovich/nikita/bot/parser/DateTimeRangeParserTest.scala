package ru.spbau.sazanovich.nikita.bot.parser

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.scalatest.{BeforeAndAfter, FunSuite}

class DateTimeRangeParserTest extends FunSuite with BeforeAndAfter {

  val dateFormatter: DateTimeFormatter = DateTimeFormat.forPattern("dd-MM-yyyy HH:mm")

  val testDateTime: DateTime = DateTime.parse("30-05-1998 09:00", dateFormatter)

  private var dateTimeRangeParserImpl: DateTimeRangeParserImpl = _

  before {
    dateTimeRangeParserImpl = new DateTimeRangeParserImpl(testDateTime)
  }

  test("parsePrefixDateTimeRange") {
    val timeRange = dateTimeRangeParserImpl.parse("today from 10:00 to 12:00")
    val expectedTimeRange = (
      DateTime.parse("30-05-1998 10:00", dateFormatter),
      DateTime.parse("30-05-1998 12:00", dateFormatter))
    assert(timeRange.get == expectedTimeRange)
  }

  test("parseInfixDateTimeRange") {
    val timeRange = dateTimeRangeParserImpl.parse("from 10:00 today to 12:00 05-06-1998")
    val expectedTimeRange = (
      DateTime.parse("30-05-1998 10:00", dateFormatter),
      DateTime.parse("05-06-1998 12:00", dateFormatter))
    assert(timeRange.get == expectedTimeRange)
  }

  test("parseDateTimeRangeWithKeywords") {
    val timeRange = dateTimeRangeParserImpl.parse("from 10:00 today to 10:00 tomorrow")
    val expectedTimeRange = (
      DateTime.parse("30-05-1998 10:00", dateFormatter),
      DateTime.parse("31-05-1998 10:00", dateFormatter))
    assert(timeRange.get == expectedTimeRange)
  }
}

private class DateTimeRangeParserImpl(val requestDate: DateTime) extends DateTimeRangeParser {

  override lazy val dateNow: DateTime = requestDate

  def parse(text: String): Option[(DateTime, DateTime)] = {
    parse(dateTimeRangeParser, text) match {
      case Success(dateTimeRange, _) => Option(dateTimeRange)
      case _ => Option.empty
    }
  }
}
