package ru.spbau.sazanovich.nikita.bot

import akka.actor.{ActorSystem, Props}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension

object Main extends App {

  val token = "415211493:AAFU3eGbyvEK6CjNvcdTzAaFFsKBWJVRD88"

  val system = ActorSystem()
  val scheduler = QuartzSchedulerExtension(system)
  val database = system.actorOf(Props(classOf[CalendarStorage]))

  private val bot = new CalendarBot(token, database)
  val actor = system.actorOf(Props(classOf[CalendarBotPingActor], bot))

  scheduler.createSchedule("every minute", None, "  0/1 * * 1/1 * ? *")

  bot.run()
}