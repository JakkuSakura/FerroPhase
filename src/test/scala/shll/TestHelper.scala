package shll

import shll.ast.AST
import shll.frontends.ShllLexerAndParser

case object TestHelper {
  def parseCode(code: String): AST = {
    ShllLexerAndParser().parse(code)
  }
}
