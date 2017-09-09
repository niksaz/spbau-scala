package ru.spbau.sazanovich.nikita

import ru.spbau.sazanovich.nikita.TokenType.TokenType

/** Represents a token, found in the expression. [[TokenType.NUMBER]] contains numeric value as
  * literal for the ease of computing later.
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
