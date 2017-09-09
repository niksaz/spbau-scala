package ru.spbau.sazanovich.nikita

import ru.spbau.sazanovich.nikita.TokenType.TokenType
import ru.spbau.sazanovich.nikita.error.ErrorReporter

import scala.collection.mutable.ListBuffer

/** Scans the expression string and produces tokens from it. */
class Scanner(val expressionString: String, val errorReporter: ErrorReporter) {

  private val tokens = ListBuffer[Token]()
  private var start = 0
  private var current = 0

  def scanTokens(): List[Token] = {
    while (!reachedTheEnd) {
      start = current
      scanToken()
    }
    tokens.+=(Token.EOF_TOKEN)
    tokens.toList
  }

  private def reachedTheEnd = current >= expressionString.length

  private def scanToken(): Unit = {
    val nextChar = advance()
    nextChar match {
      case '(' => addToken(Token.LEFT_BRACE_TOKEN)
      case ')' => addToken(Token.RIGHT_BRACE_TOKEN)
      case '+' => addToken(Token.PLUS_TOKEN)
      case '-' => addToken(Token.PLUS_TOKEN)
      case '*' => addToken(Token.ASTERISK_TOKEN)
      case '/' => addToken(Token.SLASH_TOKEN)
      case ' ' | '\r' | '\t' =>
      // Numbers like .1234 or .1
      case '.' if isDigit(peek()) => scanNumber()
      case char if isDigit(char) => scanNumber()
      // We are fine with only lowercase letters for the numeric functions like sqrt()
      case char if isLowercaseAlpha(char) => scanIdentifier()
      case _ => errorReporter.reportError("unexpected symbol at " + (current - 1) + ": " + nextChar)
    }
  }

  private def scanNumber(): Unit = {
    while (isDigit(peek())) {
      advance()
    }
    if (peek() == '.') {
      // Skipping the '.'
      advance()
      while (isDigit(peek())) {
        advance()
      }
    }
    addToken(TokenType.NUMBER, expressionString.substring(start, current).toDouble)
  }

  private def scanIdentifier(): Unit = {
    while (isLowercaseAlpha(peek())) {
      advance()
    }
    addToken(TokenType.IDENTIFIER, null)
  }

  private def isDigit(char: Char) = char >= '0' && char <= '9'

  private def isLowercaseAlpha(char: Char) = char >= 'a' && char <= 'z'

  private def advance(): Char = {
    val nextChar = expressionString.charAt(current)
    current += 1
    nextChar
  }

  private def peek(): Char = {
    if (reachedTheEnd) '\0' else expressionString.charAt(current)
  }

  private def addToken(tokenType: TokenType, literal: Any): Unit = {
    val lexeme = expressionString.substring(start, current)
    addToken(Token(tokenType, lexeme, literal))
  }

  private def addToken(token: Token): Unit = {
    tokens.+=(token)
  }
}
