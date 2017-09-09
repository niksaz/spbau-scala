package ru.spbau.sazanovich.nikita

import ru.spbau.sazanovich.nikita.error.ConsoleErrorReporter

import scala.io.StdIn

/** Evaluates an arithmetic expression entered from stdin and outputs the result to console. */
object Calculator {

  // Flag to control the printing of debug information to stdout
  private val debugInfoEnabled = true

  def main(args: Array[String]): Unit = {
    val expressionString = StdIn.readLine()
    val result = parseAndEvaluate(expressionString)
    if (result.isDefined) {
      println(result.get)
    } else {
      println("Result was not computed")
    }
  }

  private def parseAndEvaluate(expressionString: String): Option[Double] = {
    val consoleErrorReporter = new ConsoleErrorReporter

    val scanner = new Scanner(expressionString, consoleErrorReporter)
    val tokens = scanner.scanTokens()
    if (debugInfoEnabled) {
      for (token <- tokens) {
        println("# token: " + token)
      }
    }

    val parser = new Parser(tokens, consoleErrorReporter)
    val expr = parser.parse()
    if (consoleErrorReporter.hasErrorOccurred) {
      return Option.empty
    }
    if (debugInfoEnabled) {
      val astStringRepresentation = new AstPrinter().print(expr)
      println("# " + astStringRepresentation)
    }

    // TODO: Actually evaluate the expression
    Option(0.0)
  }
}
