package rust

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals
import shll.TestHelper

class TestRustParser {
  def assertParsedEquals(expected: String, input: String): Unit = {
    assertEquals(
      TestHelper.parseCode(expected),
      RustParser().parse(input)
    )
  }
  @Test def testMain(): Unit = {
    assertParsedEquals(
      "(def-fun main (lp) [unit] (block))",
      "fn main() { }"
    )
  }

  @Test def testFun(): Unit = {
    assertParsedEquals(
      "(def-fun foo (lp (: bar [int])) [int] bar)",
      "fn foo(bar: i32) -> i32 { bar }"
    )
  }
}
