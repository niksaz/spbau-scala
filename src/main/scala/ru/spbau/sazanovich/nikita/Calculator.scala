package ru.spbau.sazanovich.nikita

import scala.io.StdIn

/** Evaluates an arithmetic expression entered from stdin and outputs the result to stdout. */
object Calculator {

  // Flag to control the printing of debug information to stdout
  private val debugInfoEnabled = true

  // Flag for keeping track of whether an error was reported
  private var errorOccurred = false

  def main(args: Array[String]): Unit = {
    val expressionString = StdIn.readLine()
    val result = parseAndEvaluate(expressionString)
    if (errorOccurred) {
      println("Result was not computed since there was an error.")
    } else {
      println(result)
    }
  }

  private def parseAndEvaluate(expressionString: String): Double = {
    val consoleErrorReporter = new ErrorReporter {
      override def reportError(errorMessage: String): Unit = {
        println("Error: " + errorMessage)
        errorOccurred = true
      }
    }
    val scanner = Scanner(expressionString, consoleErrorReporter)
    val tokens = scanner.scanTokens()
    if (debugInfoEnabled) {
      for (token <- tokens) {
        println("# token: " + token)
      }
    }
    // TODO: Actually evaluate the expression
    0.0
  }
}
