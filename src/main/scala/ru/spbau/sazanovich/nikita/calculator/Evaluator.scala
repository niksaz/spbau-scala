package ru.spbau.sazanovich.nikita.calculator

case class Evaluator() extends Expr.Visitor[Double] {

  def evaluate(expr: Expr): Double = expr.accept(this)

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

  override def visitLiteralExpr(expr: Expr.Literal): Double = expr.value

  override def visitUnaryExpr(expr: Expr.Unary): Double = {
    val rightValue = evaluate(expr.right)
    expr.operator.tokenType match {
      case TokenType.MINUS => -rightValue.asInstanceOf[Double]
      case tokenType =>
          throw new IllegalStateException("Unary expression with TokenType: " + tokenType)
    }
  }
}
