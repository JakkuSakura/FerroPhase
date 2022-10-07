package shll.frontends

import shll.ast.Ast

case class ShllLexerAndParser() {
  def parse(s: String): Ast = {
    val astParser = AntlrAstParser()
    val parsed = astParser.parse(s)
    val applyParser = TypeChecker()
    applyParser.typeCheckAndConvert(parsed)
  }
}
