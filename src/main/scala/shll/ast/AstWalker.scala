package shll.ast

import com.typesafe.scalalogging.Logger
import shll.ast.*
import shll.ast.AstHelper.*
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ParamUtil.*

import scala.collection.mutable

class AstWalker() {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter(newlines = false)

  def walk(n: Ast): Ast = {
    walkNode(n)
  }

  def walkNode(n: Ast): Ast = {
    //    logger.debug("Eliminating " + pp.print(n))
    val x = n match {
      case n: Block => walkBlock(n)
      case n: Apply => walkApply(n)
      case n: Ident => walkIdent(n)
      case n: BuildList => BuildList(n.value.map(walkNode))
      case n: Field => walkField(n)
      case n: Param => walkParam(n)
      case n: Select => walkSelect(n)
      case n: Cond => walkCond(n)
      case n: ForEach => walkForEach(n)
      case n: Compose => walkTypeApply(n)
      case n: DefType => walkDefType(n)
      case n: DefVal => walkDefVal(n)
      case n: DefFun => walkDefFun(n)
      case n: Assign => walkAssign(n)
      case n: BuildFun => walkFunApply(n)
      case n: Params => Params(n.params.map(walkNode(_).asInstanceOf[Param]))
      case n => n
    }
//    val orig = pp.print(n)
//    val res = pp.print(x)
//    if (orig != res)
//      logger.debug("Eliminated " + orig + " => " + res)
    x
  }

  def walkField(n: Field): Field = {
    val value = walkNode(n.ty)
    Field(n.name, value)
  }

  def walkParam(n: Param): Param = {
    val value = walkNode(n.ty)
    Param(n.name, value)
  }
  def walkDefVal(
      n: DefVal
  ): DefVal = {
    DefVal(n.name, walkNode(n.value))
  }

  def walkIdent(id: Ident): Ast = {
    id
  }

  def walkApply(n: Apply): Ast = {
    n
  }

  def walkTypeApply(n: Compose): Ast = {
    n
  }

  def walkDefType(n: DefType): Ast = {
    n
  }

  def walkSelect(n: Select): Ast = {
    val obj = walkNode(n.obj)
    Select(obj, n.field)
  }

  def walkCond(n: Cond): Ast = {
    val cond = walkNode(n.cond)
    val conseq = walkNode(n.consequence)
    val alt = walkNode(n.alternative)
    val condTotal = Cond(cond, conseq, alt)
    condTotal
  }

  def walkForEach(n: ForEach): Ast = {
    val iterable = walkNode(n.iterable)
    val body = walkNode(n.body)
    val f = ForEach(n.variable, iterable, body)
    f
  }

  def walkDefFun(fun: DefFun): DefFun = {
    DefFun(
      fun.name,
      walkNode(fun.params).asInstanceOf,
      walkNode(fun.ret),
      walkNode(fun.body)
    )
  }

  def walkAssign(n: Assign): Ast = {
    val value = walkNode(n.value)
    val ass = Assign(n.target, value)
    ass
  }

  def walkFunApply(n: BuildFun): Ast = {
    val args = walkNode(n.params).asInstanceOf[Params]
    val ret = walkNode(n.ret)
    val body = walkNode(n.body)
    val newApply = BuildFun(args, ret, body)
    newApply
  }
  def walkBlock(d: Block): Ast = {
    Block(
      d.children.map(walkNode)
    )
  }

}
