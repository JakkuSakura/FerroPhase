package shll.frontends

import shll.ast.AST

case class ShllLexerAndParser() {
  def parse(s: String): AST = {
    val astParser = AntlrAstParser()
    val parsed = astParser.parse(s)
    val applyParser = ApplyParser()
    applyParser.parse(parsed)
  }
}
