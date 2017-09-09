package ru.spbau.sazanovich.nikita.calculator

import ru.spbau.sazanovich.nikita.calculator.TokenType.TokenType

/** Represents a token, found in the expression. Token.value is used to save [[TokenType.NUMBER]]
  * numeric value for the ease of computing later. Could be extended to store string and other
  * literals.
  */
case class Token(tokenType: TokenType, lexeme: String, literal: Any) {

  override def toString: String = "(" + tokenType.toString + ", " + lexeme + ", " + literal + ")"
}

object Token {

  val LEFT_BRACE_TOKEN = Token(TokenType.LEFT_BRACE, "(", null)
  val RIGHT_BRACE_TOKEN = Token(TokenType.RIGHT_BRACE, ")", null)

  val PLUS_TOKEN = Token(TokenType.PLUS, "+", null)
  val MINUS_TOKEN = Token(TokenType.MINUS, "-", null)
  val ASTERISK_TOKEN = Token(TokenType.ASTERISK, "*", null)
  val SLASH_TOKEN = Token(TokenType.SLASH, "/", null)

  val EOF_TOKEN = Token(TokenType.EOF, "", null)
}
