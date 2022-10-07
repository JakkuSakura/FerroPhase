package shll.backends

import shll.ast.Ast

trait PrettyPrinter {
  def print(s: Ast): String
}
