package ru.spbau.sazanovich.nikita.calculator

import org.mockito.Matchers.any
import org.mockito.Mockito.{times, verify}
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.mockito.MockitoSugar
import ru.spbau.sazanovich.nikita.calculator.Expr._
import ru.spbau.sazanovich.nikita.calculator.Token.{ASTERISK_TOKEN, MINUS_TOKEN, PLUS_TOKEN}
import ru.spbau.sazanovich.nikita.calculator.error.ErrorReporter

/** Unit tests for [[Parser]]. */
class EvaluatorTest extends FunSuite with BeforeAndAfter with MockitoSugar {

  private var errorReporter: ErrorReporter = _

  before {
    errorReporter = mock[ErrorReporter]
  }

  test("evaluateExpressionWithBrackets") {
    // 5.0 + 18.0 * (12.0 - 10.0)
    val expr =
        Binary(
            Literal(5.0),
          PLUS_TOKEN,
            Binary(
                Literal(18.0),
                ASTERISK_TOKEN,
                Grouping(
                    Binary(
                        Literal(12.0),
                        MINUS_TOKEN,
                        Literal(10.0)))))
    val result = evaluateWithoutErrors(expr)
    assert(result == Option(41.0))
  }

  test("evaluateExpressionWithIdentifier") {
    val expr =
        Identifier(
            Token(TokenType.IDENTIFIER, "sqrt", null),
            Literal(16.0)
        )
    val result = evaluateWithoutErrors(expr)
    assert(result == Option(4.0))
  }

  private def evaluateWithoutErrors(expr: Expr): Option[Double] = {
    evaluateWithErrorsExpected(expr, 0)
  }

  private def evaluateWithErrorsExpected(
      expr: Expr, expectedNumberOfErrorsReported: Int): Option[Double] = {
    val evaluator = Evaluator(errorReporter)
    val result = evaluator.evaluateIfPossible(expr)
    verify(errorReporter, times(expectedNumberOfErrorsReported)).reportError(any())
    result
  }
}
