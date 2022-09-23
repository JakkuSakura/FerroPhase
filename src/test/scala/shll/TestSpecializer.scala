package shll

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import rust.RustPrettyPrinter
import shll.ast.AST
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ShllLexerAndParser
import shll.optimizers.{DeadCodeEliminator, Specializer}

class TestSpecializer {
  val pp: PrettyPrinter = ShllPrettyPrinter()
  val showProgress = true
  def printAst(input: AST): Unit = {
    println(pp.print(input))
  }
  def optimize(node: AST): AST = {
    val specialized = Specializer().specialize(node)
    val eliminated = DeadCodeEliminator().eliminate(specialized)
    eliminated
  }
  def specializedEquals(input: String, expected: String): Unit = {
    if (showProgress)
        println(s"Parsing $input")
    val ast = ShllLexerAndParser().parse(input)
    if (showProgress)
        println(s"Specializing " + pp.print(ast))
    val optimized = optimize(ast)

    if (showProgress)
        println(s"Optimized " + pp.print(optimized))
    val optimizedPrinted = pp.print(optimized)
    val exp = ShllLexerAndParser().parse(expected)
    val expectedPrinted = pp.print(exp)
    if (expectedPrinted != optimizedPrinted)
      if (showProgress)
          println(s"Expected " + pp.print(exp))
      assertEquals(exp, optimized)
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

  @Test def testVariable(): Unit = {
    specializedEquals(
      "(block (def-val i 5) i)",
      "(block (def-val i 5) 5)"
    )
    specializedEquals(
      "(block (def-val i 5) (assign i 6) i)",
      "(block (def-val i 5) (assign i 6) 6)"
    )
  }

  @Test def testTypeApply(): Unit = {
    specializedEquals(
      "[list int]",
      "(block (def-type list_int [list int]) [list_int])"
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
}
