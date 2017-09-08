package ru.spbau.sazanovich.nikita

import ru.spbau.sazanovich.nikita.TokenType.TokenType

/** Represents a token, found in the expression. [[TokenType.NUMBER]] contains numeric value as
  * literal for the ease of computing later.
  */
case class Token(tokenType: TokenType, lexeme: String, literal: Any) {

  override def toString: String = "(" + tokenType.toString + ", " + lexeme + ", " + literal + ")"
}
