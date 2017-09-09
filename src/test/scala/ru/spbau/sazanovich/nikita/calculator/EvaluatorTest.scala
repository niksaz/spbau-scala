package ru.spbau.sazanovich.nikita.calculator

import org.scalatest.FunSuite
import ru.spbau.sazanovich.nikita.calculator.Expr.{Binary, Grouping, Literal}
import ru.spbau.sazanovich.nikita.calculator.Token.{ASTERISK_TOKEN, MINUS_TOKEN, PLUS_TOKEN}

/** Unit tests for [[Parser]]. */
class EvaluatorTest extends FunSuite {

  test("evaluateExpressionWithBrackets") {
    // 5.0 + 18.0 * (12.0 - 10.0)
    val expr =
        Binary(
            Literal(5.0),
          PLUS_TOKEN,
            Binary(
                Literal(18.0),
                ASTERISK_TOKEN,
                Grouping(
                    Binary(
                        Literal(12.0),
                        MINUS_TOKEN,
                        Literal(10.0)))))
    val result = evaluate(expr)
    assert(result == 41.0)
  }

  private def evaluate(expr: Expr): Double = Evaluator().evaluate(expr)
}
