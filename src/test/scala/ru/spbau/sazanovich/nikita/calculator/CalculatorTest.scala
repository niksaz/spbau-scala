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

  test("testparseAndEvaluateExpression") {
    val result = Calculator.parseAndEvaluate("(13.0   + -(6 - 5)) / 2 / 3 ", errorReporter)
    verify(errorReporter, times(0)).reportError(any())
    assert(result == Option(2.0))
  }
}
