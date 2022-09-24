package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.{AST, *}
import shll.ast.AstTool.*
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ParamUtil.*

import scala.collection.mutable

case class SingleBlockEliminator() {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter(newlines = false)

  def eliminate(n: AST): AST = {
    val ctx = FlowAnalysis().analyze(n)
    eliminateNode(n, ctx)
  }

  def eliminateNode(n: AST, ctx: FlowAnalysisContext): AST = {
    //    logger.debug("Eliminating " + pp.print(n))
    val x = n match {
      case n: Block => eliminateBlock(n, ctx)
      case n: Apply => eliminateApply(n, ctx)
      case n: Ident => eliminateIdent(n, ctx)
      case n: LiteralInt => n
      case n: LiteralDecimal => n
      case n: LiteralString => n
      case n: LiteralBool => n
      case n: LiteralList => LiteralList(n.value.map(eliminateNode(_, ctx)))
      case n: Field => eliminateField(n, ctx)
      case n: Select => eliminateSelect(n, ctx)
      case n: Cond => eliminateCond(n, ctx)
      case n: ForEach => eliminateForEach(n, ctx)
      case n: ApplyType => eliminateTypeApply(n, ctx)
      case n: DefType => eliminateDefType(n, ctx)
      case n: Assign => eliminateAssign(n, ctx)
      case n: ApplyFun => eliminateFunApply(n, ctx)
      case n: Parameters => Parameters(n.params.map(eliminateNode(_, ctx).asInstanceOf[Field]))
      case x => throw SpecializeException("cannot eliminate", x)
    }
    val orig = pp.print(n)
    val res = pp.print(x)
    if (orig != res)
      logger.debug("Eliminated " + orig + " => " + res)
    x
  }

  def eliminateField(n: Field, ctx: FlowAnalysisContext): Field = {
    val value = eliminateNode(n.ty, ctx)
    Field(n.name, value)
  }

  def eliminateDefVal(
                       n: DefVal,
                       ctx: FlowAnalysisContext
                     ): DefVal = {
    DefVal(n.name, eliminateNode(n.value, ctx))
  }

  def eliminateIdent(id: Ident, ctx: FlowAnalysisContext): AST = {
    id
  }

  def eliminateApply(n: Apply, ctx: FlowAnalysisContext): AST = {
    n
  }

  def eliminateTypeApply(n: ApplyType, ctx: FlowAnalysisContext): AST = {
    n
  }

  def eliminateDefType(n: DefType, ctx: FlowAnalysisContext): AST = {
    n
  }


  def eliminateSelect(n: Select, ctx: FlowAnalysisContext): AST = {
    val obj = eliminateNode(n.obj, ctx)
    Select(obj, n.field)
  }

  def eliminateCond(n: Cond, ctx: FlowAnalysisContext): AST = {
    val cond = eliminateNode(n.cond, ctx)
    val conseq = eliminateNode(n.consequence, ctx)
    val alt = eliminateNode(n.alternative, ctx)
    val condTotal = Cond(cond, conseq, alt)
    condTotal
  }

  def eliminateForEach(n: ForEach, ctx: FlowAnalysisContext): AST = {
    val iterable = eliminateNode(n.iterable, ctx)
    val body = eliminateNode(n.body, ctx)
    val f = ForEach(n.variable, iterable, body)
    f
  }

  def eliminateDefFun(fun: DefFun, ctx: FlowAnalysisContext): DefFun = {
    DefFun(
      fun.name,
      eliminateNode(fun.args, ctx).asInstanceOf,
      eliminateNode(fun.ret, ctx),
      fun.body.map(eliminateNode(_, ctx))
    )
  }

  def eliminateAssign(n: Assign, ctx: FlowAnalysisContext): AST = {
    val value = eliminateNode(n.value, ctx)
    val ass = Assign(n.name, value)
    ass
  }

  def eliminateFunApply(n: ApplyFun, ctx: FlowAnalysisContext): AST = {
    val args = eliminateNode(n.args, ctx).asInstanceOf[Parameters]
    val ret = eliminateNode(n.ret, ctx)
    val body = eliminateNode(n.body, ctx)
    val newApply = ApplyFun(args, ret, body)
    newApply
  }
  def eliminateBlock(d: Block, ctx: FlowAnalysisContext): AST = {
    val filteredStmts = d.body
      .map {
        case n: DefFun =>
          eliminateDefFun(n, ctx)
        case n: DefVal =>
          eliminateDefVal(n, ctx)
        case x => eliminateNode(x, ctx)
      }

    if (filteredStmts.length == 1)
      filteredStmts.head
    else
      Block(filteredStmts)
  }

}
