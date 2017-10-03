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
    case GetNextUserCalendarEventsFrom(id, dateNow, numberOfEvents) =>
      val userCalendarEvents = usersCalendarEvents.getOrElseUpdate(id, ArrayBuffer.empty)
      val nextUserEvents = computeNextUserEventsFrom(dateNow, userCalendarEvents, numberOfEvents)
      sender ! NextUserCalendarEvents(nextUserEvents)
  }

  def receivePersistentCommand(persistentCommand: PersistentCommand): Unit = {
    persistentCommand match {
      case ScheduleEvent(id, event) =>
        val userCalendarEvents = usersCalendarEvents.getOrElseUpdate(id, ArrayBuffer.empty)
        userCalendarEvents += event
    }
  }

  private def computeNextUserEventsFrom(
    dateNow: DateTime, userCalendarEvents: CalendarEvents, numberOfEvents: Int): CalendarEvents = {
    require(numberOfEvents > 0)
    // TODO(niksaz): Implement keeping the buffer in sorted order to not resort it.
    val sortedEvents =
      userCalendarEvents.sortBy(_.startDateTime.getMillis)
    // TODO(niksaz): Add binary search to find the start of the iteration.
    sortedEvents.filter(event => dateNow.compareTo(event.endDateTime) < 0).take(numberOfEvents)
  }
}

object CalendarStorage {

  trait PersistentCommand

  case class ScheduleEvent(id: Long, event: CalendarEvent) extends PersistentCommand

  case class ScheduleEventSuccess()

  trait Query

  case class GetNextUserCalendarEventsFrom(id: Long, nowDate: DateTime, numberOfEvents: Int)
    extends Query

  /** Contains next user events sorted by their start times. */
  case class NextUserCalendarEvents(events: ArrayBuffer[CalendarEvent])
}
