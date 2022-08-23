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
    val t = ShllLexerAndParser().parse(
      """
        |(block
        |   (def-fun foo (list (field a [int])) [int]
        |     a
        |   )
        |   (foo 1)
        |)
        |""".stripMargin)
    println(PrettyPrinter().print(t))
    val t2 = Specializer().specialize(t)
    println(PrettyPrinter().print(t2))

  }
}
