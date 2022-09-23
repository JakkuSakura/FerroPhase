package shll.backends

import shll.ast.AST

trait Backend {
  def process(node: AST): Unit
}

case class NothingBackend() extends Backend {
  def process(node: AST): Unit = {}
}

case class PrettyPrinterBackend(pp: PrettyPrinter) extends Backend {
  def process(node: AST): Unit = {
    println(pp.print(node))
  }
}
