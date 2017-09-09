package ru.spbau.sazanovich.nikita

// Prints the AST to console. Used exclusively for debugging purposes.
class AstPrinter extends Expr.Visitor[String] {

  def print(expr: Expr): String = expr.accept(this)

  def visitBinaryExpr(expr: Expr.Binary): String = {
    parenthesize(expr.operator.lexeme, expr.left, expr.right)
  }

  def visitGroupingExpr(expr: Expr.Grouping): String = {
    parenthesize("group", expr.expression)
  }

  def visitLiteralExpr(expr: Expr.Literal): String = {
    if (expr.value == null) return "null"
    expr.value.toString
  }

  def visitUnaryExpr(expr: Expr.Unary): String = {
    parenthesize(expr.operator.lexeme, expr.right)
  }

  private def parenthesize(name: String, exprs: Expr*) = {
    val builder = new StringBuilder
    builder.append("(").append(name)
    for (expr <- exprs) {
      builder.append(" ")
      builder.append(expr.accept(this))
    }
    builder.append(")")
    builder.toString
  }
}
