package ru.spbau.sazanovich.nikita.bot

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.models.Message
import ru.spbau.sazanovich.nikita.bot.CalendarStorage.{GetNextUserCalendarEvents, NextUserCalendarEvents, ScheduleEvent}
import ru.spbau.sazanovich.nikita.bot.parser.MessageParser
import ru.spbau.sazanovich.nikita.bot.parser.MessageParser.{CreateEventMessage, IncorrectMessage, WhatAreNextEventsMessage, WhatIsNext}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.DurationInt
import scala.util.Success

class CalendarBotPingActor(bot: CalendarBot) extends Actor {

  override def receive: PartialFunction[Any, Unit] = {
    case "Ping" => bot.receivePing()
  }
}

class CalendarBot(val token: String, val database: ActorRef)
    extends TelegramBot with Polling with Commands {

  def receivePing(): Unit = {
  }

  onMessage {
    implicit message =>
      message.text.foreach { text =>
        processMessageText(text)
      }
  }

  def processMessageText(text: String)(implicit message: Message): Unit = {
    val chatId = message.chat.id
    implicit val timeout: Timeout = Timeout(1.second)
    MessageParser.parse(text) match {
      case CreateEventMessage(event) =>
        database ! ScheduleEvent(chatId, event)
        reply("Scheduled the event!")
      case WhatIsNext() =>
        (database ? GetNextUserCalendarEvents(chatId, 1)).onComplete {
          case Success(NextUserCalendarEvents(events)) =>
            reply(generateReplyForNextEvent(events))
          case _ =>
            reply("Error in the database. Try later.")
        }
      case WhatAreNextEventsMessage(numberOfEvents) =>
        (database ? GetNextUserCalendarEvents(chatId, numberOfEvents)).onComplete {
          case Success(NextUserCalendarEvents(events)) =>
            reply(generateReplyForNextEvents(events, numberOfEvents))
          case _ =>
            reply("Error in the database. Try later.")
        }
      case IncorrectMessage(errorMessage) =>
        reply(errorMessage)
    }
  }

  private def generateReplyForNextEvent(nextEvents: ArrayBuffer[CalendarEvent]): String = {
    if (nextEvents.isEmpty) "No events scheduled from now. :)" else "Next event is " + nextEvents(0)
  }

  private def generateReplyForNextEvents
      (nextEvents: ArrayBuffer[CalendarEvent], numberOfEvents: Int): String = {
    if (nextEvents.isEmpty) {
      "No events scheduled from now. :)"
    } else {
      val replyText = new StringBuilder()
      replyText.append(
        if (nextEvents.size < numberOfEvents)
          "You only have " + formatNextEvents(nextEvents.size)
        else
          "Your next " + formatNextEvents(nextEvents.size)
      )
      replyText.append(": ")
      replyText.append(nextEvents.map(_.toString).mkString(", ")).toString()
    }
  }

  private def formatNextEvents(numberOfEvents: Int): String = {
    if (numberOfEvents == 1) numberOfEvents + " event" else numberOfEvents + " events"
  }
}
