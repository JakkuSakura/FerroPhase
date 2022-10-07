package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.*
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ParamUtil.*
import shll.ast.AstHelper.*

import scala.collection.mutable

case class DeadCodeEliminator() extends AstWalker {
  private val flow = FlowAnalysis()

  def eliminate(n: AST): AST = {
    flow.analyze(n)
    walkNode(n)
  }

  def checkKeepStatement(ctx: FlowAnalysisContext, x: AST): Boolean = {
    ctx.isReachable(x, LiteralUnknown())
  }
  override def walkBlock(n: Block): AST = {
    val ctx = flow.contextHistory(n)
    val filteredStmts = n.children
      .filter(x => checkKeepStatement(ctx, x))
      .map(x => walkNode(x))

    Block(filteredStmts)
  }

}
