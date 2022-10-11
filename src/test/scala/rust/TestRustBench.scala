package rust

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import shll.TestHelper

import com.typesafe.scalalogging.Logger
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import rust.{RustPrettyPrinter, RustRunner}
import shll.ast.Ast
import shll.backends.{
  Backend,
  NothingBackend,
  PrettyPrinter,
  PrettyPrinterBackend,
  ShllPrettyPrinter
}
import shll.frontends.ShllLexerAndParser
import shll.optimizers.{DeadCodeEliminator, Flattener, Specializer}

class TestRustBench {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter()
  //  val backend: Backend = NothingBackend()
  val backend: RustRunner = RustRunner()
  val showProgress = true

  def printAst(input: Ast): Unit = {
    println(pp.print(input))
  }

  def optimize(node: Ast): Ast = {
    if (showProgress)
      logger.info(s"Optimizing " + pp.print(node))
    val specialized = Specializer().specialize(node)
    if (showProgress)
      logger.info(s"Eliminating " + pp.print(specialized))
    val eliminated1 = DeadCodeEliminator().eliminate(specialized)
    if (showProgress)
      logger.info(s"Flattening " + pp.print(eliminated1))
    val flatten = Flattener().flatten(eliminated1)
    if (showProgress)
      logger.info(s"Optimized " + pp.print(flatten))
    flatten
  }

  def compareBench(code: String): Unit = {
    val ast = ShllLexerAndParser().parse(code)
    logger.info("Running unoptimized code")
    val duration = backend.getRuntimeDuration(ast)
    logger.info(s"Running unoptimized code: $duration ms")
    logger.info("Running optimized code")
    val optimized = optimize(ast)
    val duration2 = backend.getRuntimeDuration(optimized)
    logger.info(s"Running optimized code: $duration2 ms(${duration - duration2} ms faster)")
  }

  @Test def testStaticFunctionCallInList(): Unit = {
    compareBench(
      """
        |(block
        |  (def-type pass (lp) [fun (lp (: a [int])) [int]])
        |  (def-fun call (lp (: funs [list pass])) [int]
        |    (block
        |      (def-val s 0)
        |      (for f funs
        |         (for i (range 0 1000)
        |           (assign s (+ s (f i)))
        |         )
        |      )
        |      s
        |    )
        |  )
        |  (def-val x (call
        |   (list
        |     (fun (lp (: a [int])) [int] (+ a 0))
        |     (fun (lp (: a [int])) [int] (+ a 1))
        |   )
        |  ))
        |  (print x)
        |)
        |""".stripMargin
    )

  }
}
