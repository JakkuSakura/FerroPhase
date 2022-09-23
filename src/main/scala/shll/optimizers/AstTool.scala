package shll.optimizers

import shll.ast.*

case object AstTool {
  def isConstant(n: AST): Boolean = {
    n match {
      case _: LiteralInt => true
      case _: LiteralBool => true
      case _: LiteralDecimal => true
      case _: LiteralChar => true
      case _: LiteralString => true
      case x: LiteralList => x.value.map(isConstant).forall(identity)
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
      case a => throw SpecializeException("cannot convert to keys", a)
    }.toArray
  }
}
