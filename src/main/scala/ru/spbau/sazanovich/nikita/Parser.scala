package ru.spbau.sazanovich.nikita

import ru.spbau.sazanovich.nikita.TokenType.TokenType
import ru.spbau.sazanovich.nikita.error.ErrorReporter

/** Parses a list of tokens into AST. If an error is met, reports it back to [[ErrorReporter]]. */
class Parser(val tokens: List[Token], val errorReporter: ErrorReporter) {

  private var current = 0

  def parse(): Expr = {
    try {
      addition()
    } catch {
      case _: ParseError => null
    }
  }

  private def expression(): Expr = addition()

  private def addition(): Expr = {
    var expr = multiplication()
    while (advanceIfNextTokenMatchAny(TokenType.MINUS, TokenType.PLUS)) {
      val operator = previous
      val rightExpr = multiplication()
      expr = Expr.Binary(expr, operator, rightExpr)
    }
    expr
  }

  private def multiplication(): Expr = {
    var expr = unary()
    while (advanceIfNextTokenMatchAny(TokenType.SLASH, TokenType.ASTERISK)) {
      val operator = previous
      val rightExpr = unary()
      expr = Expr.Binary(expr, operator, rightExpr)
    }
    expr
  }

  private def unary(): Expr = {
    if (advanceIfNextTokenMatchAny(TokenType.MINUS)) {
      val operator = previous
      val rightExpr = unary()
      return Expr.Unary(operator, rightExpr)
    }
    primary()
  }

  private def primary(): Expr = {
    if (advanceIfNextTokenMatchAny(TokenType.NUMBER)) {
      return Expr.Literal(previous.literal)
    }
    if (advanceIfNextTokenMatchAny(TokenType.LEFT_BRACE)) {
      val expr = expression()
      consume(TokenType.RIGHT_BRACE, "expected ')' after expression")
      return Expr.Grouping(expr)
    }
    throw error(peek, "expected expression")
  }

  private def advanceIfNextTokenMatchAny(types: TokenType*): Boolean = {
    for (tokenType <- types) {
      if (check(tokenType)) {
        advance()
        return true
      }
    }
    false
  }

  private def consume(tokenType: TokenType, errorMessage: String): Token = {
    if (check(tokenType)) {
      return advance()
    }
    throw error(peek, errorMessage)
  }

  private def error(token: Token, message: String): ParseError = {
    val tokenDescriptionLexeme = if (token.tokenType == TokenType.EOF) "EOF" else token.lexeme
    errorReporter.reportError(message + ", but found " + tokenDescriptionLexeme)
    new ParseError
  }

  private def check(tokenType: TokenType): Boolean = {
    if (isAtEnd) return false
    peek.tokenType == tokenType
  }

  private def advance(): Token = {
    if (!isAtEnd) {
      current += 1
    }
    previous
  }

  private def isAtEnd = peek.tokenType == TokenType.EOF

  private def peek: Token = tokens(current)

  private def previous: Token = tokens(current - 1)

  private class ParseError() extends RuntimeException {
  }
}
