package ru.spbau.sazanovich.nikita.calculator

import org.mockito.Matchers.any
import org.mockito.Mockito.{times, verify}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FunSuite}
import ru.spbau.sazanovich.nikita.calculator.error.ErrorReporter

/** Integration tests for the parts of Calculator. */
class CalculatorTest extends FunSuite with BeforeAndAfter with MockitoSugar {

  private var errorReporter: ErrorReporter = _

  before {
    errorReporter = mock[ErrorReporter]
  }

  test("testParseAndEvaluateExpression") {
    val result = parseAndEvaluateWithoutErrors("(13.0   + -(6 - 5)) / 2 / 3 ")
    assert(result == Option(2.0))
  }

  test("testParseAndEvaluateExpressionWithIdentifiers") {
    val result = parseAndEvaluateWithoutErrors("cos(0.0)+sqrt(25.)")
    assert(result == Option(6.0))
  }

  private def parseAndEvaluateWithoutErrors(expressionString: String): Option[Double] = {
    parseAndEvaluateWithErrorsExpected(expressionString, 0)
  }

  private def parseAndEvaluateWithErrorsExpected(
      expressionString: String, expectedNumberOfErrorsReported: Int): Option[Double] = {
    val result = Calculator.parseAndEvaluate(expressionString, errorReporter)
    verify(errorReporter, times(expectedNumberOfErrorsReported)).reportError(any())
    result
  }
}
