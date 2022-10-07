package shll

import com.typesafe.scalalogging.Logger
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import shll.ast.Ast
import shll.backends.{Backend, PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ShllLexerAndParser
import shll.optimizers.DeadCodeEliminator

class TestEliminator {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter()
  val showProgress = true

  def printAst(input: Ast): Unit = {
    println(pp.print(input))
  }

  def optimize(node: Ast): Ast = {
    if (showProgress)
      logger.info(s"Optimizing " + pp.print(node))
    val eliminated = DeadCodeEliminator().eliminate(node)
    if (showProgress)
      logger.info(s"Optimized " + pp.print(eliminated))
    eliminated
  }

  def eliminatedEquals(input: String, expected: String): Unit = {
    if (showProgress)
      logger.info(s"Parsing $input")
    val ast = ShllLexerAndParser().parse(input)
    val optimized = optimize(ast)
    val optimizedPrinted = pp.print(optimized)
    val exp = ShllLexerAndParser().parse(expected)
    val expectedPrinted = pp.print(exp)
    if (expectedPrinted != optimizedPrinted)
      if (showProgress)
        logger.info(s"Expected " + pp.print(exp))
      assertEquals(exp, optimized)
  }
  @Test def testVal1(): Unit = {
    eliminatedEquals(
      """
        |(block
        |  (def-val x 1)
        |  (def-val y 2)
        |  x
        |)
        |""".stripMargin,
      """
            |(block
            |  (def-val x 1)
            |  x
            |)
            |""".stripMargin
    )
  }

  @Test def testVal2(): Unit = {
    eliminatedEquals(
      """
        |(block
        |  (def-val x 1)
        |  1
        |)
        |""".stripMargin,
      """
        |(block
        |  1
        |)
        |""".stripMargin
    )
  }

  @Test def testSumFun(): Unit = {
    eliminatedEquals(
      """
        |(block
        |   (def-fun sum (lp (: values [list [int]])) [int]
        |     (block
        |       (def-val s 0)
        |       (for i values
        |         (assign s (+ s i))
        |       )
        |       s
        |     )
        |   )
        |   (sum (range 1 101))
        |)
        |""".stripMargin,
      """
        |(block
        |   (def-fun sum (lp (: values [list [int]])) [int]
        |     (block
        |       (def-val s 0)
        |       (for i values
        |         (assign s (+ s i))
        |       )
        |       s
        |     )
        |   )
        |   (sum (range 1 101))
        |)
        |""".stripMargin
    )
  }
}
