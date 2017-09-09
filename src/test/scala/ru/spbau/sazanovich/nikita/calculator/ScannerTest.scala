package ru.spbau.sazanovich.nikita.calculator

import org.mockito.Matchers.any
import org.mockito.Mockito.{times, verify}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FunSuite}
import ru.spbau.sazanovich.nikita.calculator.Token.{EOF_TOKEN, LEFT_BRACE_TOKEN, PLUS_TOKEN, RIGHT_BRACE_TOKEN}
import ru.spbau.sazanovich.nikita.calculator.TokenType._
import ru.spbau.sazanovich.nikita.calculator.error.ErrorReporter

/** Unit tests for [[Scanner]]. */
class ScannerTest extends FunSuite with BeforeAndAfter with MockitoSugar {

  private var errorReporter: ErrorReporter = _

  before {
    errorReporter = mock[ErrorReporter]
  }

  test("testScanningCorrectExpression") {
    val tokens = getTokensWithoutErrors("(  1\t  + 15.0)   ")
    verifyTokens(tokens,
        LEFT_BRACE_TOKEN,
        Token(NUMBER, "1", 1),
        PLUS_TOKEN,
        Token(NUMBER, "15.0", 15),
        RIGHT_BRACE_TOKEN,
        EOF_TOKEN)
  }

  test("testScanningTokensAfterErrorOccurred") {
    val tokens = getTokensWithErrorsExpected("(  11 .  + 5", 1)
    verifyTokens(tokens,
        LEFT_BRACE_TOKEN,
        Token(NUMBER, "11", 11),
        PLUS_TOKEN,
        Token(NUMBER, "5", 5),
        EOF_TOKEN)
  }

  test("testDotNumberScanning") {
    val tokens = getTokensWithoutErrors(".12")
    verifyTokens(tokens,
        Token(NUMBER, ".12", .12),
        EOF_TOKEN)
  }

  test("testNumberDotNumberScanning") {
    val tokens = getTokensWithoutErrors("5446.12")
    verifyTokens(tokens,
        Token(NUMBER, "5446.12", 5446.12),
        EOF_TOKEN)
  }

  test("testNumberDotScanning") {
    val tokens = getTokensWithoutErrors("5446.")
    verifyTokens(tokens,
        Token(NUMBER, "5446.", 5446.0),
        EOF_TOKEN)
  }

  test("testDotIsNotNumber") {
    val tokens = getTokensWithErrorsExpected(".", 1)
    verifyTokens(tokens,
        EOF_TOKEN)
  }

  test("testIdentifierScanning") {
    val tokens = getTokensWithoutErrors("sqrt()")
    assert(tokens.size == 4)
    verifyTokens(tokens,
        Token(IDENTIFIER, "sqrt", null),
        LEFT_BRACE_TOKEN,
        RIGHT_BRACE_TOKEN,
        EOF_TOKEN)
  }

  private def verifyTokens(tokens: List[Token], expectedTokens: Token*): Unit = {
    assert(tokens.size == expectedTokens.size)
    val tokenIterator = tokens.iterator
    val expectedTokensIterator = expectedTokens.iterator
    while (tokenIterator.hasNext) {
      assert(tokenIterator.next() == expectedTokensIterator.next())
    }
  }

  private def getTokensWithoutErrors(expressionString: String): List[Token] = {
    getTokensWithErrorsExpected(expressionString, 0)
  }

  private def getTokensWithErrorsExpected(
      expressionString: String, expectedNumberOfErrorsReported: Int): List[Token] = {
    val scanner = new Scanner(expressionString, errorReporter)
    val tokens = scanner.scanTokens()
    verify(errorReporter, times(expectedNumberOfErrorsReported)).reportError(any())
    tokens
  }
}
