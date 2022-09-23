package shll

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import shll.backends.PrettyPrinter
import shll.frontends.ShllLexerAndParser
import shll.optimizers.Specializer

class ShllAstTest {
  @Test def testParser(): Unit = {
    val t = ShllLexerAndParser().parse("(block (A) (B))")
    println(PrettyPrinter().print(t))
  }

  @Test def testSpecializer(): Unit = {
    val src = ShllLexerAndParser().parse(
      """
        |(block
        |   (def-fun foo (list (field a [int])) [int]
        |     a
        |   )
        |   (foo 1)
        |)
        |""".stripMargin)
    println(PrettyPrinter().print(src))
    val specialized = Specializer().specialize(src)
    println(PrettyPrinter().print(specialized))
    val expected = ShllLexerAndParser().parse(
      """
        |(block
        |  (def-fun foo_0 (list) [int] a)
        |  (def-fun foo (list (field a [int])) [int] a)
        |  (foo_0)
        |)
        |""".stripMargin)
    assertEquals(expected, specialized)
  }

  @Test def testStruct(): Unit = {
    val src = ShllLexerAndParser().parse(
      """
        |(block
        |   (def-struct Foo (list (field a [int])))
        |   (select (Foo a=1) a)
        |)
        |""".stripMargin)
    println(PrettyPrinter().print(src))
    val specialized = Specializer().specialize(src)
    println(PrettyPrinter().print(specialized))
    val expected = ShllLexerAndParser().parse(
      """
        |(block
        |  (def-struct Foo (list (field a [int])))
        |  1
        |)
        |""".stripMargin)
    assertEquals(expected, specialized)
  }

}
