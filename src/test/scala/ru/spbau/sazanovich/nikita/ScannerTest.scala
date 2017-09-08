package ru.spbau.sazanovich.nikita

import org.mockito.Matchers.any
import org.mockito.Mockito.{times, verify}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FunSuite}
import ru.spbau.sazanovich.nikita.TokenType.{EOF, LEFT_BRACE, NUMBER, PLUS, RIGHT_BRACE, TokenType}

/** Unit tests for [[Scanner]]. */
class ScannerTest extends FunSuite with BeforeAndAfter with MockitoSugar {

  private var errorReporter: ErrorReporter  = _

  before {
    errorReporter = mock[ErrorReporter]
  }

  test("testScanningCorrectExpression") {
    val tokens = getTokensWithoutErrors("(  1\t  + 15.0)   ")
    assert(tokens.size == 6)
    verifyToken(tokens(0), LEFT_BRACE, "(", null)
    verifyToken(tokens(1), NUMBER, "1", 1)
    verifyToken(tokens(2), PLUS, "+", null)
    verifyToken(tokens(3), NUMBER, "15.0", 15)
    verifyToken(tokens(4), RIGHT_BRACE, ")", null)
    verifyToken(tokens(5), EOF, "", null)
  }

  test("testScanningTokensAfterErrorOccurred") {
    val tokens = getTokensAndExpectNErrors("(  11 .  + 5", 1)
    assert(tokens.size == 5)
    verifyToken(tokens(0), LEFT_BRACE, "(", null)
    verifyToken(tokens(1), NUMBER, "11", 11)
    verifyToken(tokens(2), PLUS, "+", null)
    verifyToken(tokens(3), NUMBER, "5", 5)
    verifyToken(tokens(4), EOF, "", null)
  }

  test("testDotNumberScanning") {
    val tokens = getTokensWithoutErrors(".12")
    assert(tokens.size == 2)
    verifyToken(tokens(0), NUMBER, ".12", .12)
    verifyToken(tokens(1), EOF, "", null)
  }

  test("testNumberDotNumberScanning") {
    val tokens = getTokensWithoutErrors("5446.12")
    assert(tokens.size == 2)
    verifyToken(tokens(0), NUMBER, "5446.12", 5446.12)
    verifyToken(tokens(1), EOF, "", null)
  }

  test("testNumberDotScanning") {
    val tokens = getTokensWithoutErrors("5446.")
    assert(tokens.size == 2)
    verifyToken(tokens(0), NUMBER, "5446.", 5446)
    verifyToken(tokens(1), EOF, "", null)
  }

  test("testDotIsNotNumber") {
    val tokens = getTokensAndExpectNErrors(".", 1)
    assert(tokens.size == 1)
    verifyToken(tokens(0), EOF, "", null)
  }

  test("testIdentifierScanning") {
  }

  private def verifyToken(
      token: Token, tokenType: TokenType, lexeme: String, literal: Any): Unit = {
    assert(token.tokenType == tokenType)
    assert(token.lexeme == lexeme)
    assert(token.literal == literal)
  }

  private def getTokensWithoutErrors(expressionString: String): List[Token] = {
    getTokensAndExpectNErrors(expressionString, 0)
  }

  private def getTokensAndExpectNErrors(
      expressionString: String, expectedNumberOfErrorsReported: Int): List[Token] = {
    val scanner = Scanner(expressionString, errorReporter)
    val tokens = scanner.scanTokens()
    verify(errorReporter, times(expectedNumberOfErrorsReported)).reportError(any())
    tokens
  }
}
