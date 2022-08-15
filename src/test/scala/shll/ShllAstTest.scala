package shll

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class ShllAstTest {
  @Test def testParser(): Unit = {
    val t = ShllParser().parse("(block (A) (B))")
    println(PrettyPrinter().print(t))
  }

  @Test def testSpecializer(): Unit = {
    val t = ShllParser().parse(
      """
        |(block
        |   (def-fun foo (list (field a (type Int))) (type Int) a)
        |   (foo 1)
        |)
        |""".stripMargin)
    println(PrettyPrinter().print(t))
    val t2 = Specializer().specialize(t)
    println(PrettyPrinter().print(t2))

  }
}
