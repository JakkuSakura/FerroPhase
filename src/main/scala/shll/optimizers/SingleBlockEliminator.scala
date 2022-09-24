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
    eliminateNode(n)
  }

  def eliminateNode(n: AST): AST = {
    //    logger.debug("Eliminating " + pp.print(n))
    val x = n match {
      case n: Block => eliminateBlock(n)
      case n: Apply => eliminateApply(n)
      case n: Ident => eliminateIdent(n)
      case n: LiteralInt => n
      case n: LiteralDecimal => n
      case n: LiteralString => n
      case n: LiteralBool => n
      case n: LiteralList => LiteralList(n.value.map(eliminateNode))
      case n: Field => eliminateField(n)
      case n: Select => eliminateSelect(n)
      case n: Cond => eliminateCond(n)
      case n: ForEach => eliminateForEach(n)
      case n: ApplyType => eliminateTypeApply(n)
      case n: DefType => eliminateDefType(n)
      case n: Assign => eliminateAssign(n)
      case n: ApplyFun => eliminateFunApply(n)
      case n: Parameters => Parameters(n.params.map(eliminateNode(_).asInstanceOf[Field]))
      case x => throw SpecializeException("cannot eliminate", x)
    }
    val orig = pp.print(n)
    val res = pp.print(x)
    if (orig != res)
      logger.debug("Eliminated " + orig + " => " + res)
    x
  }

  def eliminateField(n: Field): Field = {
    val value = eliminateNode(n.ty)
    Field(n.name, value)
  }

  def eliminateDefVal(
      n: DefVal
  ): DefVal = {
    DefVal(n.name, eliminateNode(n.value))
  }

  def eliminateIdent(id: Ident): AST = {
    id
  }

  def eliminateApply(n: Apply): AST = {
    n
  }

  def eliminateTypeApply(n: ApplyType): AST = {
    n
  }

  def eliminateDefType(n: DefType): AST = {
    n
  }

  def eliminateSelect(n: Select): AST = {
    val obj = eliminateNode(n.obj)
    Select(obj, n.field)
  }

  def eliminateCond(n: Cond): AST = {
    val cond = eliminateNode(n.cond)
    val conseq = eliminateNode(n.consequence)
    val alt = eliminateNode(n.alternative)
    val condTotal = Cond(cond, conseq, alt)
    condTotal
  }

  def eliminateForEach(n: ForEach): AST = {
    val iterable = eliminateNode(n.iterable)
    val body = Block(eliminateNode(n.body))
    val f = ForEach(n.variable, iterable, body)
    f
  }

  def eliminateDefFun(fun: DefFun): DefFun = {
    DefFun(
      fun.name,
      eliminateNode(fun.args).asInstanceOf,
      eliminateNode(fun.ret),
      fun.body.map(eliminateNode)
    )
  }

  def eliminateAssign(n: Assign): AST = {
    val value = eliminateNode(n.value)
    val ass = Assign(n.name, value)
    ass
  }

  def eliminateFunApply(n: ApplyFun): AST = {
    val args = eliminateNode(n.args).asInstanceOf[Parameters]
    val ret = eliminateNode(n.ret)
    val body = eliminateNode(n.body)
    val newApply = ApplyFun(args, ret, body)
    newApply
  }
  def eliminateBlock(d: Block): AST = {
    val filteredStmts = d.body
      .map {
        case n: DefFun =>
          eliminateDefFun(n)
        case n: DefVal =>
          eliminateDefVal(n)
        case x => eliminateNode(x)
      }

    if (filteredStmts.length == 1)
      filteredStmts.head
    else
      Block(filteredStmts)
  }

}
