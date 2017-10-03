package ru.spbau.sazanovich.nikita.bot.utils

import org.joda.time.DateTime

object DateTimeUtils {

  def areOnTheSameDay(dateTime1: DateTime, dateTime2: DateTime): Boolean = {
    dateTime1.year() == dateTime2.year() && dateTime1.dayOfYear() == dateTime2.dayOfYear()
  }
}
