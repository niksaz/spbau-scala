package ru.spbau.sazanovich.nikita.calculator

// Prints the AST to console. Used exclusively for debugging purposes.
case class AstPrinter() extends Expr.Visitor[String] {

  def print(expr: Expr): String = expr.accept(this)

  override def visitBinaryExpr(expr: Expr.Binary): String = {
    parenthesize(expr.operator.lexeme, expr.left, expr.right)
  }

  override def visitGroupingExpr(expr: Expr.Grouping): String = {
    parenthesize("group", expr.expression)
  }

  override def visitLiteralExpr(expr: Expr.Literal): String = {
    expr.value.toString
  }

  override def visitUnaryExpr(expr: Expr.Unary): String = {
    parenthesize(expr.operator.lexeme, expr.right)
  }

  override def visitIdentifierExpr(expr: Expr.Identifier): String = {
    parenthesize(expr.identifier.lexeme, expr.right)
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
