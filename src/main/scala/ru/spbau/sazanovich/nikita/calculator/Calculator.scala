package ru.spbau.sazanovich.nikita.calculator

import com.google.common.annotations.VisibleForTesting
import ru.spbau.sazanovich.nikita.calculator.error.{ConsoleErrorReporter, ErrorReporter}

import scala.io.StdIn

/** Evaluates an arithmetic expression entered from stdin and outputs the result to console. */
object Calculator {

  // Flag to control the printing of debug information to stdout
  private val debugInfoEnabled = true

  def main(args: Array[String]): Unit = {
    val expressionString = StdIn.readLine()
    val consoleErrorReporter = new ConsoleErrorReporter
    val result = parseAndEvaluate(expressionString, consoleErrorReporter)
    if (result.isDefined) {
      println(result.get)
    } else {
      println("Result was not computed")
    }
  }

  @VisibleForTesting
  private[calculator] def parseAndEvaluate(
      expressionString: String, errorReporter: ErrorReporter): Option[Double] = {
    val scanner = new Scanner(expressionString, errorReporter)
    val tokens = scanner.scanTokens()
    if (debugInfoEnabled) {
      for (token <- tokens) {
        println("# token: " + token)
      }
    }

    val parser = new Parser(tokens, errorReporter)
    val expr = parser.parse()
    if (errorReporter.hasErrorOccurred) {
      return Option.empty
    }
    if (debugInfoEnabled) {
      val astStringRepresentation = AstPrinter().print(expr)
      println("# " + astStringRepresentation)
    }
    Option(Evaluator().evaluate(expr))
  }
}
