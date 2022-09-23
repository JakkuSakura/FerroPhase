package shll

import com.typesafe.scalalogging.Logger
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import rust.{RustPrettyPrinter, RustRunnerBackend}
import shll.ast.AST
import shll.backends.{Backend, NothingBackend, PrettyPrinter, PrettyPrinterBackend, ShllPrettyPrinter}
import shll.frontends.ShllLexerAndParser
import shll.optimizers.{DeadCodeEliminator, Specializer}

class TestOptimizers {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter()
//  val backend: Backend = NothingBackend()
  val backend: Backend = RustRunnerBackend()
  val showProgress = true
  def printAst(input: AST): Unit = {
    println(pp.print(input))
  }
  def optimize(node: AST): AST = {
    if (showProgress)
      logger.info(s"Optimizing " + pp.print(node))
    val specialized = Specializer().specialize(node)
    if (showProgress)
      logger.info(s"Eliminating " + pp.print(specialized))
    val eliminated = DeadCodeEliminator().eliminate(specialized)
    if (showProgress)
      logger.info(s"Optimized " + pp.print(eliminated))
    eliminated
  }
  def specializedEquals(input: String, expected: String, feedBackend: Boolean=true): Unit = {
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
    if (feedBackend)
      backend.process(optimized)
  }
  @Test def testFunc(): Unit = {
    specializedEquals(
      """
        |(block
        |   (def-fun foo (list (field a [int])) [int]
        |     a
        |   )
        |   (foo 1)
        |)
        |""".stripMargin,
      "1"
    )
  }

  @Test def testStruct(): Unit = {
    specializedEquals(
      """
        |(block
        |   (def-struct Foo (list (field a [int])))
        |   (select (Foo a=1) a)
        |)
        |""".stripMargin,
      "1"
    )
  }

  @Test def testIfElse(): Unit = {
    specializedEquals(
      "(if true 1 2)",
      "1"
    )
    specializedEquals(
      "(if false 1 2)",
      "2"
    )
  }

  @Test def testOrd(): Unit = {
    specializedEquals(
      "(== 1 2)",
      "false"
    )
    specializedEquals(
      "(== 1 1)",
      """
        |true
        |""".stripMargin
    )
    specializedEquals(
      "(!= 1 2)",
      "true"
    )
    specializedEquals(
      "(!= 1 1)",
      "false"
    )

    specializedEquals(
      "(> 15 5)",
      "true"
    )
    specializedEquals(
      "(>= 15 5)",
      "true"
    )
    specializedEquals(
      "(< 15 5)",
      "false"
    )
    specializedEquals(
      "(<= 15 5)",
      "false"
    )
  }

  @Test def testBasicOps(): Unit = {
    specializedEquals(
      "(+ 1 2)",
      "3"
    )
    specializedEquals(
      "(- 1 2)",
      "-1"
    )
    specializedEquals(
      "(* 3 5)",
      "15"
    )
    specializedEquals(
      "(/ 15 5)",
      "3"
    )
    specializedEquals(
      "(% 15 5)",
      "0"
    )
  }

  @Test def testList(): Unit = {
    specializedEquals(
      "(list (+ 1 2))",
      "(list 3)"
    )
  }

  @Test def testForLoop(): Unit = {
    specializedEquals(
      "(for i (list 1 2 3) (print i))",
      "(block (print 1) (print 2) (print 3))"
    )
  }

  @Test def testForLoopRange(): Unit = {
    specializedEquals(
      "(for i (range 0 10) (print i))",
      "(for i (range 0 10) (print i))"
    )
  }


  @Test def testVariable(): Unit = {
    specializedEquals(
      "(block (def-val i 5) i)",
      "5"
    )
    specializedEquals(
      "(block (def-val i 5) (assign i 6) i)",
      "6"
    )
  }

  @Test def testTypeApply(): Unit = {
    specializedEquals(
      "(block [list int])",
      "(block (def-type list_int [list int]) [list_int])",
      feedBackend = false
    )
  }

  @Test def testFunBodyApply(): Unit = {
    specializedEquals(
      """
        |(block
        |   (def-fun sum (list (field a [int]) (field b [int])) [int]
        |     (+ a b)
        |   )
        |   (sum 1 2)
        |)
        |""".stripMargin,
      "3"
    )
  }

  @Test def testSum(): Unit = {
    specializedEquals(
      """
        |(block
        |   (def-fun sum (list (field values [list [int]])) [int]
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
        |  (def-val values (range 1 101))
        |  (def-val s 0)
        |  (for i values
        |     (assign s (+ s i))
        |  )
        |  s
        |)""".stripMargin
    )
  }

  @Test def testScopedEliminator(): Unit = {
    specializedEquals(
      """
        |(block
        |   (def-val a 1)
        |   (block
        |     (def-val a 1)
        |     (print a)
        |   )
        |)
        |""".stripMargin,
      """
        |(print 1)
        |""".stripMargin,
      false
    )
  }
}
