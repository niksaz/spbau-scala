package ru.spbau.sazanovich.nikita.calculator

import java.util.NoSuchElementException

import ru.spbau.sazanovich.nikita.calculator.error.ErrorReporter

/** Evaluates AST. */
case class Evaluator(errorReporter: ErrorReporter) extends Expr.Visitor[Double] {

  def evaluateIfPossible(expr: Expr): Option[Double] = {
    try {
      val result = evaluate(expr)
      Option(result)
    } catch {
      case _: EvaluationError => Option.empty
    }
  }

  private def evaluate(expr: Expr): Double = expr.accept(this)

  override def visitBinaryExpr(expr: Expr.Binary): Double = {
    val leftValue = evaluate(expr.left)
    val rightValue = evaluate(expr.right)

    expr.operator.tokenType match {
      case TokenType.MINUS => leftValue - rightValue
      case TokenType.PLUS =>  leftValue + rightValue
      case TokenType.ASTERISK => leftValue * rightValue
      case TokenType.SLASH => leftValue / rightValue
      case tokenType =>
          throw new IllegalStateException("Binary expression with TokenType: " + tokenType)
    }
  }

  override def visitGroupingExpr(expr: Expr.Grouping): Double = evaluate(expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): Double = expr.value.asInstanceOf[Double]

  override def visitUnaryExpr(expr: Expr.Unary): Double = {
    val rightValue = evaluate(expr.right)
    expr.operator.tokenType match {
      case TokenType.MINUS => -rightValue.asInstanceOf[Double]
      case tokenType =>
          throw new IllegalStateException("Unary expression with TokenType: " + tokenType)
    }
  }

  override def visitIdentifierExpr(expr: Expr.Identifier): Double = {
    val right = evaluate(expr.right)
    val identifier = expr.identifier
    val identifierFn = try {
      Evaluator.IDENTIFIER_MAP(identifier.lexeme)
    } catch {
      case _: NoSuchElementException =>
          throw error("identifier \"" + identifier.lexeme + "\" is not defined")
    }
    identifierFn.apply(right)
  }

  private def error(message: String): EvaluationError = {
    errorReporter.reportError(message)
    new EvaluationError
  }

  private class EvaluationError() extends RuntimeException {
  }
}

object Evaluator {

  private val IDENTIFIER_MAP: Map[String, (Double) => Double] =
      Map("sqrt" -> (x => Math.sqrt(x)),
          "sin" -> (x => Math.sin(x)),
          "cos" -> (x => Math.cos(x)))
}
