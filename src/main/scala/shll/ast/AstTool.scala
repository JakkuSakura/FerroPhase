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

  def hasSideEffects(n: AST, ctx: ValueContext): Boolean = {
    // TODO: more compenrehensive side effect analysis
    n match {
      case Apply(Ident(name), _, _) if ctx.getStruct(name).isEmpty => true
      case Assign(name, value) => ctx.getValue(name.name).isDefined && ctx.getValueShallow(name.name).isEmpty
      case Block(body) => body.exists(hasSideEffects(_, ctx))
      case _ => false
    }
  }
  def argsToRange(
      args: LiteralList
  ): Array[Int] = {
    args.value.indices.toArray
  }

  def argsToKeys(
      args: LiteralList
  ): Array[String] = {
    args.value.map {
      case a: Field => a.name.name
      case a => throw ParserException("cannot convert to keys " + a)
    }.toArray
  }
}
