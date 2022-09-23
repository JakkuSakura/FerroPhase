package shll.backends

case class TextTool(
    NL: String = "\n",
    INDENT: String = "  "
) {
  def indent(s: String): String = s
    .split(NL)
    .map(x => INDENT + x)
    .mkString(NL)
}
