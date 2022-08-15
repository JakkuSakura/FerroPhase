import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import shll.ShllParser

class ShllAstTest {
  @Test def test_parser(): Unit = {
    val t = ShllParser().parse("(block (A) (B))")
    println(t)
  }

}
