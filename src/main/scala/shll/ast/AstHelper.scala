package shll.ast

import shll.frontends.ParserException

case object AstHelper {
  def defFun(name: String, args: List[(String, Ast)], ret: Ast, body: Ast): DefFun =
    DefFun(Ident(name), Params(args.map(x => Param(Ident(x._1), x._2))), ret, body)

  def declFun(name: String, args: List[(String, Ast)], ret: Ast): DeclFun =
    DeclFun(Ident(name), Params(args.map(x => Param(Ident(x._1), x._2))), ret)

  def defType(name: String, args: List[String], body: Ast): DefType =
    DefType(Ident(name), Params(args.map(x => Param(Ident(x), AstHelper.tAny))), body)
  def literalType(s: String): Ident = Ident(s)

  def tBool: Ident = literalType("bool")
  def tInt: Ident = literalType("int")
  def tNumeric: Ident = literalType("numeric")
  def tString: Ident = literalType("string")
  def tChar: Ident = literalType("char")
  def tAny: Ident = literalType("any")
  def tUnit: Ident = literalType("unit")
  def tIdent: Ident = literalType("ident")
  def tParams: Ident = literalType("params")
  def tFields: Ident = literalType("fields")
  def tList(t: Ast): Apply = Apply(Ident("listof"), PosArgs(List(t)), KwArgs(Nil))
  def tFun(params: Ast, ret: Ast): Fun =
    Fun(params.asInstanceOf[Params], ret, None, None)
  def block(n: Ast*): Block = Block(n.toList)
  def forEach(i: String, iterable: Ast, body: Ast): ForEach = ForEach(Ident(i), iterable, body)
  def applyFun(n: String, args: Ast*): Apply = Apply(Ident(n), PosArgs(args.toList), KwArgs(Nil))

  def isLiteral(n: Ast, ctx: ValueContext): Boolean = {
    n match {
      case _: LiteralInt => true
      case _: LiteralBool => true
      case _: LiteralDecimal => true
      case _: LiteralChar => true
      case _: LiteralString => true
      case x: BuildList => x.value.map(isLiteral(_, ctx)).forall(identity)
      case Ident(name) if ctx.getValue(name).exists(isLiteral(_, ctx)) => true
      case _ => false
    }
  }

  def isFinite(n: Ast, ctx: ValueContext): Boolean = {
    n match {
      case x: BuildList => true
      case Ident(name) if ctx.getValue(name).exists(isFinite(_, ctx)) => true
      case _ => false
    }
  }

  def argsToRange(
      args: Params
  ): Array[Int] = {
    args.params.indices.toArray
  }

  def argsToKeys(
      args: Params
  ): Array[String] = {
    args.params.map { a => a.name.name }.toArray
  }

  def argsToRange(
      args: Fields
  ): Array[Int] = {
    args.fields.indices.toArray
  }

  def argsToKeys(
      args: Fields
  ): Array[String] = {
    args.fields.map { a => a.name.name }.toArray
  }
}
