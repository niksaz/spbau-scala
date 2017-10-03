package ru.spbau.sazanovich.nikita.bot

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.models.Message
import org.joda.time.DateTime
import ru.spbau.sazanovich.nikita.bot.CalendarBot.{DatabaseErrorMessage, NoNextEventsMessage, SuccessfullyScheduledEventMessage}
import ru.spbau.sazanovich.nikita.bot.CalendarStorage.{GetNextUserCalendarEventsFrom, NextUserCalendarEvents, ScheduleEvent, ScheduleEventSuccess}
import ru.spbau.sazanovich.nikita.bot.parser.MessageParser
import ru.spbau.sazanovich.nikita.bot.parser.MessageParser.{CreateEventMessage, IncorrectMessage, WhatAreNextEventsMessage, WhatIsNext}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.DurationInt
import scala.util.Success

class CalendarBotLoggingActor(bot: CalendarBot) extends Actor {

  override def receive: PartialFunction[Any, Unit] = {
    case "Log" => bot.log()
  }
}

class CalendarBot(val token: String, val database: ActorRef)
    extends TelegramBot with Polling with Commands {

  val requestsSinceLastLogging = new AtomicInteger()

  def log(): Unit = {
    val numberOfRequests = requestsSinceLastLogging.getAndSet(0)
    // TODO(niksaz): Replace println with a more mature logging system.
    println("requests for the period: " + numberOfRequests)
  }

  onMessage {
    implicit message =>
      message.text.foreach { text =>
        processMessageText(text)
      }
  }

  def processMessageText(text: String)(implicit message: Message): Unit = {
    requestsSinceLastLogging.addAndGet(1)
    val chatId = message.chat.id
    implicit val timeout: Timeout = Timeout(1.second)
    val requestDate = DateTime.now()
    val messageParser = new MessageParser(requestDate)
    messageParser.parse(text) match {
      case CreateEventMessage(event) =>
        (database ? ScheduleEvent(chatId, event)).onComplete {
          case Success(ScheduleEventSuccess) =>
            reply(SuccessfullyScheduledEventMessage)
          case _ =>
            reply(DatabaseErrorMessage)
        }
      case WhatIsNext() =>
        (database ? GetNextUserCalendarEventsFrom(chatId, requestDate, 1)).onComplete {
          case Success(NextUserCalendarEvents(events)) =>
            reply(generateReplyForNextEvent(requestDate, events))
          case _ =>
            reply(DatabaseErrorMessage)
        }
      case WhatAreNextEventsMessage(numberOfEvents) =>
        (database ? GetNextUserCalendarEventsFrom(chatId, requestDate, numberOfEvents)).onComplete {
          case Success(NextUserCalendarEvents(events)) =>
            reply(generateReplyForNextEvents(requestDate, events, numberOfEvents))
          case _ =>
            reply(DatabaseErrorMessage)
        }
      case IncorrectMessage(errorMessage) =>
        reply(errorMessage)
    }
  }

  private def generateReplyForNextEvent(
      dateNow: DateTime, nextEvents: ArrayBuffer[CalendarEvent]): String = {
    if (nextEvents.isEmpty)
      NoNextEventsMessage
    else
      "Next event is " + transformCalendarEventsToHumanReadableString(dateNow, nextEvents)
  }

  private def generateReplyForNextEvents(
      dateNow: DateTime, nextEvents: ArrayBuffer[CalendarEvent], numberOfEvents: Int): String = {
    if (nextEvents.isEmpty) {
      NoNextEventsMessage
    } else {
      val replyText = new StringBuilder()
      replyText.append(
        if (nextEvents.size < numberOfEvents)
          "You only have " + formatNextEvents(nextEvents.size)
        else
          "Your next " + formatNextEvents(nextEvents.size)
      )
      replyText.append(": ")
      replyText
        .append(transformCalendarEventsToHumanReadableString(dateNow, nextEvents))
        .toString()
    }
  }

  private def transformCalendarEventsToHumanReadableString(
      dateNow: DateTime, nextEvents: ArrayBuffer[CalendarEvent]): String = {
    nextEvents.map(_.transformToHumanReadableString(dateNow)).mkString(", ")
  }

  private def formatNextEvents(numberOfEvents: Int): String = {
    if (numberOfEvents == 1) numberOfEvents + " event" else numberOfEvents + " events"
  }
}

object CalendarBot {

  /** Reply to a user when the event is scheduled successfully. */
  val SuccessfullyScheduledEventMessage = "Scheduled the event!"

  /** Reply to a user when there is no events scheduled after current time. */
  val NoNextEventsMessage = "No events scheduled from now. :)"

  /** Reply to a user when the database error occurs. */
  val DatabaseErrorMessage = "Error in the database. Try later."
}
