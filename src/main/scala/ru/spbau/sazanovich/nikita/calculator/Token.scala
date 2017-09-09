package ru.spbau.sazanovich.nikita.calculator

import ru.spbau.sazanovich.nikita.calculator.TokenType.TokenType

/** Represents a token, found in the expression. [[TokenType.NUMBER]] contains numeric value for
  * the ease of computing later.
  */
case class Token(tokenType: TokenType, lexeme: String, value: Double) {

  override def toString: String = "(" + tokenType.toString + ", " + lexeme + ", " + value + ")"
}

object Token {

  val LEFT_BRACE_TOKEN = Token(TokenType.LEFT_BRACE, "(", 0.0)
  val RIGHT_BRACE_TOKEN = Token(TokenType.RIGHT_BRACE, ")", 0.0)

  val PLUS_TOKEN = Token(TokenType.PLUS, "+", 0.0)
  val MINUS_TOKEN = Token(TokenType.MINUS, "-", 0.0)
  val ASTERISK_TOKEN = Token(TokenType.ASTERISK, "*", 0.0)
  val SLASH_TOKEN = Token(TokenType.SLASH, "/", 0.0)

  val EOF_TOKEN = Token(TokenType.EOF, "", 0.0)
}
