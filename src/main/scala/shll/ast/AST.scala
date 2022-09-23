package shll.ast

import shll.*

object AST {
  var count: Int = 0
  def getNum: Int = {
    count += 1
    count
  }
}
trait AST(var num: Int = AST.getNum) extends Cloneable{
  def reNumber(): Unit = {
    num = AST.getNum
  }

  def duplicate(): this.type = {
    val dup: this.type = this.clone().asInstanceOf[this.type]
    dup.reNumber()
    dup
  }
}

case class Ident(name: String) extends AST()
// basic elements
case class LiteralInt(value: Int, raw: String) extends AST()
case class LiteralBool(value: Boolean) extends AST()
case class LiteralDecimal(value: Double, raw: String) extends AST()
case class LiteralChar(value: Char, raw: String) extends AST()
case class LiteralString(value: String, raw: String) extends AST()
case class LiteralList(value: List[AST]) extends AST()
case class LiteralUnknown() extends AST()
case class Field(name: Ident, ty: AST) extends AST()
case class KeyValue(name: Ident, value: AST) extends AST()
case class Apply(fun: AST, args: List[AST], kwArgs: List[KeyValue]) extends AST()

case class DefFun(name: Ident, args: LiteralList, ret: AST, body: Option[AST]) extends AST()
// TODO: rename to ApplyFun, etc
case class FunApply(args: LiteralList, ret: AST, body: AST) extends AST()
case class DefVal(name: Ident, value: AST) extends AST()
case class DefType(name: Ident, value: AST) extends AST()
case class Assign(name: Ident, value: AST) extends AST()
case class Cond(cond: AST, consequence: AST, alternative: AST) extends AST()
case class While(cond: AST, body: AST) extends AST()
case class Block(body: List[AST]) extends AST()
case class ForEach(variable: Ident, iterable: AST, body: AST) extends AST()

case class TypeApply(fun: AST, args: List[AST], kwArgs: List[KeyValue]) extends AST()
case class DefStruct(name: Ident, fields: LiteralList) extends AST()
case class StructApply(s: AST, values: List[KeyValue] = Nil) extends AST()
case class Select(obj: AST, field: Ident) extends AST()
