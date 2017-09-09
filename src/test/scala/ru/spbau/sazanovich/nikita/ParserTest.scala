package ru.spbau.sazanovich.nikita

import org.mockito.Matchers.any
import org.mockito.Mockito.{times, verify}
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.mockito.MockitoSugar
import ru.spbau.sazanovich.nikita.Expr._
import ru.spbau.sazanovich.nikita.ParserTest.{NUMBER_1_TOKEN, NUMBER_2_TOKEN}
import ru.spbau.sazanovich.nikita.Token._
import ru.spbau.sazanovich.nikita.TokenType._
import ru.spbau.sazanovich.nikita.error.ErrorReporter

/** Unit tests for [[Parser]]. */
class ParserTest extends FunSuite with BeforeAndAfter with MockitoSugar {

  private var errorReporter: ErrorReporter = _

  before {
    errorReporter = mock[ErrorReporter]
  }

  test("parseSimpleExpr") {
    val expr = parseWithoutErrors(
        LEFT_BRACE_TOKEN, NUMBER_1_TOKEN, PLUS_TOKEN, NUMBER_2_TOKEN, RIGHT_BRACE_TOKEN, EOF_TOKEN)
    val expectedExpr = Grouping(Binary(Literal(1.0), PLUS_TOKEN, Literal(2.0)))
    assert(expr == expectedExpr)
  }

  test("parseIncorrectExpr") {
    val expr = parseAndExpectNErrors(List(LEFT_BRACE_TOKEN, EOF_TOKEN), 1)
    assert(expr == null)
  }

  private def parseWithoutErrors(tokens: Token*): Expr = {
    parseAndExpectNErrors(tokens.toList, 0)
  }

  private def parseAndExpectNErrors(
      tokens: List[Token], expectedNumberOfErrorsReported: Int): Expr = {
    val parser = new Parser(tokens, errorReporter)
    val expr = parser.parse()
    verify(errorReporter, times(expectedNumberOfErrorsReported)).reportError(any())
    expr
  }
}

object ParserTest {

  private val NUMBER_1_TOKEN = Token(NUMBER, "1.0", 1.0)
  private val NUMBER_2_TOKEN = Token(NUMBER, "2.0", 2.0)
}
