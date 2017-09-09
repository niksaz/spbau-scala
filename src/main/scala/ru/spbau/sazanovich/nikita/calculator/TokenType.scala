package ru.spbau.sazanovich.nikita.calculator

/** [[Enumeration]] for tokens, we expect to find in an arithmetic expression. */
object TokenType extends Enumeration {

  type TokenType = Value
  val LEFT_BRACE, RIGHT_BRACE, MINUS, PLUS, SLASH, ASTERISK,
      IDENTIFIER, NUMBER, EOF = Value
}
