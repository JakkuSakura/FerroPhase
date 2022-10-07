package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.*
import shll.ast.AstHelper.*
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ParamUtil.*

import scala.collection.mutable

case class Flattener() extends AstWalker {
  def flatten(ast: Ast): Ast = {
    walk(ast)
  }
  override def walkBlock(d: Block): Ast = {
    val filteredStmts = d.children
      .flatMap {
        case Block(stmts) if stmts.forall(_.isInstanceOf[Block]) =>
          stmts.map(walkNode)
        case x => List(walkNode(x))
      }

    if (filteredStmts.length == 1)
      filteredStmts.head
    else
      Block(filteredStmts)
  }

}
