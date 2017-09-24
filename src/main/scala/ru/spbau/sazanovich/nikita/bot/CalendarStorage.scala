package ru.spbau.sazanovich.nikita.bot

import akka.persistence.PersistentActor
import org.joda.time.DateTime
import ru.spbau.sazanovich.nikita.bot.CalendarStorage._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Persists user events and allows to query them. */
class CalendarStorage extends PersistentActor {

  private type CalendarEvents = ArrayBuffer[CalendarEvent]

  override def persistenceId = "calendar-database"

  val usersCalendarEvents: mutable.HashMap[Long, CalendarEvents] = mutable.HashMap.empty

  override def receiveRecover: Receive = {
    case persistentCommand: PersistentCommand => receivePersistentCommand(persistentCommand)
  }

  override def receiveCommand: Receive = {
    case persistentCommand: PersistentCommand =>
      persist(persistentCommand)(receivePersistentCommand)
      sender ! ScheduleEventSuccess
    case GetNextUserCalendarEvents(id, numberOfEvents) =>
      val userCalendarEvents = usersCalendarEvents.getOrElseUpdate(id, ArrayBuffer.empty)
      val nextUserEvents = computeNextUserEvents(userCalendarEvents, numberOfEvents)
      sender ! NextUserCalendarEvents(nextUserEvents)
  }

  def receivePersistentCommand(persistentCommand: PersistentCommand): Unit = {
    persistentCommand match {
      case ScheduleEvent(id, event) =>
        val userCalendarEvents = usersCalendarEvents.getOrElseUpdate(id, ArrayBuffer.empty)
        userCalendarEvents += event
    }
  }

  private def computeNextUserEvents(
      userCalendarEvents: CalendarEvents, numberOfEvents: Int): CalendarEvents = {
    require(numberOfEvents > 0)
    // TODO: Implement keeping the buffer in sorted order to not resort it.
    val sortedEvents =
      userCalendarEvents.sortWith(
        (event1, event2) => event1.startDateTime.compareTo(event2.startDateTime) < 0)
    val nowDate = DateTime.now()
    val nextEvents: ArrayBuffer[CalendarEvent] = ArrayBuffer.empty
    // TODO: Add binary search to find the start of the iteration.
    val sortedEventsIterator = sortedEvents.iterator
    while (sortedEventsIterator.hasNext && nextEvents.size < numberOfEvents) {
      val nextEvent = sortedEventsIterator.next()
      if (nowDate.compareTo(nextEvent.endDateTime) < 0) {
        nextEvents += nextEvent
      }
    }
    nextEvents
  }
}

object CalendarStorage {

  trait PersistentCommand

  case class ScheduleEvent(id: Long, event: CalendarEvent) extends PersistentCommand

  case class ScheduleEventSuccess()

  trait Query

  case class GetNextUserCalendarEvents(id: Long, numberOfEvents: Int) extends Query

  /** Contains next user events sorted by their start times. */
  case class NextUserCalendarEvents(events: ArrayBuffer[CalendarEvent])
}
