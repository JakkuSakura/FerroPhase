package shll.backends

import shll.ast.Ast

trait Backend {
  def process(node: Ast): Unit
}

case class NothingBackend() extends Backend {
  def process(node: Ast): Unit = {}
}

case class PrettyPrinterBackend(pp: PrettyPrinter) extends Backend {
  def process(node: Ast): Unit = {
    println(pp.print(node))
  }
}
