package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.*
import shll.ast.AstHelper.*
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ParamUtil.*

import scala.collection.mutable

case class Flattener() {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter(newlines = false)

  def flatten(n: AST): AST = {
    flattenNode(n)
  }

  def flattenNode(n: AST): AST = {
    //    logger.debug("Eliminating " + pp.print(n))
    val x = n match {
      case n: Block => eliminateBlock(n)
      case n: Apply => eliminateApply(n)
      case n: Ident => eliminateIdent(n)
      case n: LiteralInt => n
      case n: LiteralDecimal => n
      case n: LiteralString => n
      case n: LiteralBool => n
      case n: LiteralList => LiteralList(n.value.map(flattenNode))
      case n: Field => eliminateField(n)
      case n: Select => eliminateSelect(n)
      case n: Cond => eliminateCond(n)
      case n: ForEach => eliminateForEach(n)
      case n: ApplyType => eliminateTypeApply(n)
      case n: DefType => eliminateDefType(n)
      case n: DefVal => eliminateDefVal(n)
      case n: DefFun => eliminateDefFun(n)
      case n: Assign => eliminateAssign(n)
      case n: ApplyFun => eliminateFunApply(n)
      case n: Parameters => Parameters(n.params.map(flattenNode(_).asInstanceOf[Field]))
      case x => throw SpecializeException("cannot eliminate", x)
    }
//    val orig = pp.print(n)
//    val res = pp.print(x)
//    if (orig != res)
//      logger.debug("Eliminated " + orig + " => " + res)
    x
  }

  def eliminateField(n: Field): Field = {
    val value = flattenNode(n.ty)
    Field(n.name, value)
  }

  def eliminateDefVal(
      n: DefVal
  ): DefVal = {
    DefVal(n.name, flattenNode(n.value))
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
    val obj = flattenNode(n.obj)
    Select(obj, n.field)
  }

  def eliminateCond(n: Cond): AST = {
    val cond = flattenNode(n.cond)
    val conseq = flattenNode(n.consequence)
    val alt = flattenNode(n.alternative)
    val condTotal = Cond(cond, conseq, alt)
    condTotal
  }

  def eliminateForEach(n: ForEach): AST = {
    val iterable = flattenNode(n.iterable)
    val body = flattenNode(n.body)
    val f = ForEach(n.variable, iterable, body)
    f
  }

  def eliminateDefFun(fun: DefFun): DefFun = {
    DefFun(
      fun.name,
      flattenNode(fun.params).asInstanceOf,
      flattenNode(fun.ret),
      flattenNode(fun.body)
    )
  }

  def eliminateAssign(n: Assign): AST = {
    val value = flattenNode(n.value)
    val ass = Assign(n.target, value)
    ass
  }

  def eliminateFunApply(n: ApplyFun): AST = {
    val args = flattenNode(n.params).asInstanceOf[Parameters]
    val ret = flattenNode(n.ret)
    val body = flattenNode(n.body)
    val newApply = ApplyFun(args, ret, body)
    newApply
  }
  def eliminateBlock(d: Block): AST = {
    val filteredStmts = d.children
      .flatMap {
        case Block(stmts) if stmts.forall(_.isInstanceOf[Block]) =>
          stmts.map(flattenNode)
        case x => List(flattenNode(x))
      }

    if (filteredStmts.length == 1)
      filteredStmts.head
    else
      Block(filteredStmts)
  }

}
