package ru.spbau.sazanovich.nikita.bot

import akka.actor.{ActorSystem, Props}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension

object Main extends App {

  private val EveryMinuteScheduleName = "every minute"

  private val token = "415211493:AAFU3eGbyvEK6CjNvcdTzAaFFsKBWJVRD88"

  val system = ActorSystem()
  val scheduler = QuartzSchedulerExtension(system)
  val database = system.actorOf(Props(classOf[CalendarStorage]))

  private val bot = new CalendarBot(token, database)
  val actor = system.actorOf(Props(classOf[CalendarBotLoggingActor], bot))

  scheduler.createSchedule(EveryMinuteScheduleName, None, "0 * * * * ? *")
  scheduler.schedule(EveryMinuteScheduleName, actor, "Log")

  bot.run()
}