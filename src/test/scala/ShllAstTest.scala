import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import shll.AntlrAstParser

class ShllAstTest {
  @Test def test_parser(): Unit = {
    val t = AntlrAstParser().parse("(A)")
    println(t)
  }

}
