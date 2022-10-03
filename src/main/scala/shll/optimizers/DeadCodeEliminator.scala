package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.*
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ParamUtil.*
import shll.ast.AstHelper.*

import scala.collection.mutable

case class DeadCodeEliminator() {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter(newlines = false)
  private val flow = FlowAnalysis()

  def eliminate(n: AST): AST = {
    flow.analyze(n)
    eliminateNode(n)
  }

  def eliminateNode(n: AST): AST = {
    val ctx = flow.contextHistory(n)
    val x = n match {
      case n: Block => eliminateBlock(n, ctx)
      case n: Apply => eliminateApply(n, ctx)
      case n: Ident => eliminateIdent(n, ctx)
      case n: LiteralInt => n
      case n: LiteralDecimal => n
      case n: LiteralString => n
      case n: LiteralBool => n
      case n: LiteralList => LiteralList(n.value.map(eliminateNode))
      case n: Field => eliminateField(n, ctx)
      case n: Select => eliminateSelect(n, ctx)
      case n: Cond => eliminateCond(n, ctx)
      case n: ForEach => eliminateForEach(n, ctx)
      case n: ApplyType => eliminateTypeApply(n, ctx)
      case n: DefType => eliminateDefType(n, ctx)
      case n: Assign => eliminateAssign(n, ctx)
      case n: ApplyFun => eliminateFunApply(n, ctx)
      case n: DefFun => eliminateDefFun(n, ctx)
      case n: DefVal => eliminateDefVal(n, ctx)
      case n: Parameters => Parameters(n.params.map(eliminateNode(_).asInstanceOf[Field]))
      case x => throw SpecializeException("cannot eliminate", x)
    }
    val orig = pp.print(n)
    val res = pp.print(x)
    if (orig != res)
      logger.debug("Eliminated " + orig + " => " + res)
    x
  }

  def eliminateField(n: Field, ctx: FlowAnalysisContext): Field = {
    val value = eliminateNode(n.ty)
    Field(n.name, value)
  }
  def eliminateDefVal(
      n: DefVal,
      ctx: FlowAnalysisContext
  ): DefVal = {
    DefVal(n.name, eliminateNode(n.value))
  }
  def eliminateIdent(id: Ident, ctx: FlowAnalysisContext): AST = {
    id
  }

  def eliminateApply(n: Apply, ctx: FlowAnalysisContext): AST = {
    n
  }

  def eliminateTypeApply(n: ApplyType, ctx: FlowAnalysisContext): AST = { n }

  def eliminateDefType(n: DefType, ctx: FlowAnalysisContext): AST = {
    n
  }

  def checkKeepStatement(ctx: FlowAnalysisContext, x: AST): Boolean = {
    ctx.isReachable(x, LiteralUnknown())
  }
  def eliminateBlock(n: Block, ctx: FlowAnalysisContext): AST = {
    val filteredStmts = n.body
      .filter(x => checkKeepStatement(ctx, x))
      .map(x => eliminateNode(x))

    Block(filteredStmts)
  }

  def eliminateSelect(n: Select, ctx: FlowAnalysisContext): AST = {
    val obj = eliminateNode(n.obj)
    Select(obj, n.field)
  }
  def eliminateCond(n: Cond, ctx: FlowAnalysisContext): AST = {
    val cond = eliminateNode(n.cond)
    val conseq = eliminateNode(n.consequence)
    val alt = eliminateNode(n.alternative)
    val condTotal = Cond(cond, conseq, alt)
    condTotal

  }

  def eliminateForEach(n: ForEach, ctx: FlowAnalysisContext): AST = {
    val iterable = eliminateNode(n.iterable)
    val body = eliminateNode(n.body)
    val f = ForEach(n.variable, iterable, body)
    f
  }
  def eliminateDefFun(fun: DefFun, ctx: FlowAnalysisContext): DefFun = {
    DefFun(
      fun.name,
      eliminateNode(fun.args).asInstanceOf,
      eliminateNode(fun.ret),
      fun.body.map(eliminateNode)
    )
  }

  def eliminateAssign(n: Assign, ctx: FlowAnalysisContext): AST = {
    val value = eliminateNode(n.value)
    val ass = Assign(n.target, value)
    ass
  }
  def eliminateFunApply(n: ApplyFun, ctx: FlowAnalysisContext): AST = {
    val args = eliminateNode(n.args).asInstanceOf[Parameters]
    val ret = eliminateNode(n.ret)
    val body = eliminateNode(n.body)
    val newApply = ApplyFun(args, ret, body)
    newApply
  }
}
