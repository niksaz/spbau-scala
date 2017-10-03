package ru.spbau.sazanovich.nikita.bot.utils

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.scalatest.FunSuite
import ru.spbau.sazanovich.nikita.bot.utils.DateTimeUtils.areOnTheSameDay

class DateTimeUtilsTest extends FunSuite {

  val dateFormatter: DateTimeFormatter = DateTimeFormat.forPattern("dd-MM-yyyy HH:mm")

  test("onTheSameDay") {
    val dateTime1 = DateTime.parse("30-05-1998 10:00", dateFormatter)
    val dateTime2 = DateTime.parse("30-05-1998 12:00", dateFormatter)
    assert(areOnTheSameDay(dateTime1, dateTime2))
  }

  test("onDifferentDays") {
    val dateTime1 = DateTime.parse("30-05-1998 10:00", dateFormatter)
    val dateTime2 = DateTime.parse("18-12-1998 12:00", dateFormatter)
    assert(!areOnTheSameDay(dateTime1, dateTime2))
  }

  test("inDifferentYears") {
    val dateTime1 = DateTime.parse("30-05-1998 10:00", dateFormatter)
    val dateTime2 = DateTime.parse("30-05-2000 10:00", dateFormatter)
    assert(!areOnTheSameDay(dateTime1, dateTime2))
  }
}
