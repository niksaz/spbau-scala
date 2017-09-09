package ru.spbau.sazanovich.nikita.calculator

import ru.spbau.sazanovich.nikita.calculator.Expr._
import ru.spbau.sazanovich.nikita.calculator.ParserTest._
import ru.spbau.sazanovich.nikita.calculator.Token._
import ru.spbau.sazanovich.nikita.calculator.TokenType._
import ru.spbau.sazanovich.nikita.calculator.error.TestSuiteWithErrorReporter

/** Unit tests for [[Parser]]. */
class ParserTest extends TestSuiteWithErrorReporter {

  test("parseSimpleExpr") {
    val expr = parseWithoutErrors(
        LEFT_BRACE_TOKEN, NUMBER_1_TOKEN, PLUS_TOKEN, NUMBER_2_TOKEN, RIGHT_BRACE_TOKEN, EOF_TOKEN)
    val expectedExpr = Grouping(Binary(Literal(1.0), PLUS_TOKEN, Literal(2.0)))
    assert(expr == expectedExpr)
  }

  test("parseIncorrectExpr") {
    val expr = parseWithErrorsExpected(List(LEFT_BRACE_TOKEN, EOF_TOKEN), 1)
    assert(expr == null)
  }

  test("parseIdentifierExpr") {
    val expr =
        parseWithoutErrors(
            SQRT_TOKEN, LEFT_BRACE_TOKEN, NUMBER_1_TOKEN, RIGHT_BRACE_TOKEN, EOF_TOKEN)
    val expectedExpr = Identifier(Token(IDENTIFIER, "sqrt", null), Literal(1.0))
    assert(expr == expectedExpr)
  }

  private def parseWithoutErrors(tokens: Token*): Expr = {
    parseWithErrorsExpected(tokens.toList, 0)
  }

  private def parseWithErrorsExpected(
      tokens: List[Token], expectedNumberOfErrorsReported: Int): Expr = {
    val parser = new Parser(tokens, errorReporter)
    val expr = parser.parse()
    verifyNumberOfErrorsReported(expectedNumberOfErrorsReported)
    expr
  }
}

object ParserTest {

  private val SQRT_TOKEN = Token(IDENTIFIER, "sqrt", null)

  private val NUMBER_1_TOKEN = Token(NUMBER, "1.0", 1.0)
  private val NUMBER_2_TOKEN = Token(NUMBER, "2.0", 2.0)
}
