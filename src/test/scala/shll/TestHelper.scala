package shll

import shll.ast.Ast
import shll.frontends.ShllLexerAndParser

case object TestHelper {
  def parseCode(code: String): Ast = {
    ShllLexerAndParser().parse(code)
  }
}
