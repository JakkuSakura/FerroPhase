package shll.backends

import shll.ast.AST

trait PrettyPrinter {
  def print(s: AST): String
}
