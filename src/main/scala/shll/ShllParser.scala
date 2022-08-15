package shll

case class ShllParser () {
  def parse(s: String): AST = {
    val astParser = AntlrAstParser()
    val parsed = astParser.parse(s)
    val applyParser = ApplyParser()
    applyParser.parse(parsed)
  }
}
