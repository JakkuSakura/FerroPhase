package shll.ast

import shll.frontends.ParserException

case object AstTool {
  def isLiteral(n: AST, ctx: ValueContext): Boolean = {
    n match {
      case _: LiteralInt => true
      case _: LiteralBool => true
      case _: LiteralDecimal => true
      case _: LiteralChar => true
      case _: LiteralString => true
      case x: LiteralList => x.value.map(isLiteral(_, ctx)).forall(identity)
      case Ident(name) if ctx.getValue(name).exists(isLiteral(_, ctx)) => true
      case _ => false
    }
  }

  def isFinite(n: AST, ctx: ValueContext): Boolean = {
    n match {
      case x: LiteralList => true
      case Ident(name) if ctx.getValue(name).exists(isFinite(_, ctx)) => true
      case _ => false
    }
  }

  def argsToRange(
      args: Parameters
  ): Array[Int] = {
    args.params.indices.toArray
  }

  def argsToKeys(
      args: Parameters
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
