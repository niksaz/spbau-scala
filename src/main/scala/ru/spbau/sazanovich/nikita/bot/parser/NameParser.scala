package ru.spbau.sazanovich.nikita.bot.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/** Parses a string enclosed in quotes, but not containing them. */
trait NameParser extends RegexParsers {

  override def skipWhitespace = true

  override protected val whiteSpace: Regex = "[ \t\r\f]+".r

  val nameParser: Parser[String] = "\"" ~> ("[^\"]+".r <~ "\"")
}
