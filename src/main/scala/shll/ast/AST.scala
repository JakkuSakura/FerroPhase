package shll.ast

import org.antlr.v4.runtime.Token
import shll.*

object AST {
  var count: Int = 0
  def genNum: Int = {
    count += 1
    count
  }
}
trait AST(var num: Int = AST.genNum, var token: Option[Token] = None, var origin: Option[AST] = None) extends Cloneable{

  def duplicate(): this.type = {
    val dup: this.type = this.clone().asInstanceOf[this.type]
    num = AST.genNum
    dup
  }
  def withToken(token: Token): this.type = {
    this.token = Some(token)
    this
  }
}

case class Ident(name: String) extends AST()

case class LiteralInt(value: Int) extends AST()
case class LiteralBool(value: Boolean) extends AST()
case class LiteralDecimal(value: Double) extends AST()
case class LiteralChar(value: Char) extends AST()
case class LiteralString(value: String) extends AST()
case class LiteralList(value: List[AST]) extends AST()
// may use List[Param] in the future
case class Parameters(params: List[Field]) extends AST()
case class PosArgs(args: List[AST]) extends AST()
case class KwArgs(args: List[KeyValue]) extends AST()
case class Fields(fields: List[Field]) extends AST()
case class LiteralUnknown() extends AST()
case class Field(name: Ident, ty: AST) extends AST()
case class KeyValue(name: Ident, value: AST) extends AST()
case class Apply(fun: AST, args: PosArgs, kwArgs: KwArgs) extends AST()

case class DefFun(name: Ident, args: Parameters, ret: AST, body: Option[AST]) extends AST()
// form of (fun (list (field a [int])) x)
case class ApplyFun(args: Parameters, ret: AST, body: AST) extends AST()
case class DefVal(name: Ident, value: AST) extends AST()
case class DefType(name: Ident, value: AST) extends AST()
case class Assign(name: AST, value: AST) extends AST()
case class Cond(cond: AST, consequence: AST, alternative: AST) extends AST()
case class While(cond: AST, body: AST) extends AST()
case class Block(body: List[AST]) extends AST() {

}
case object Block {
  def apply(block: Block): Block = block

  def apply(block: AST): Block = Block(List(block))

  def apply(): Block = Block(Nil)
}
case class ForEach(variable: Ident, iterable: AST, body: AST) extends AST()
// form of [type args]
case class ApplyType(fun: AST, args: PosArgs, kwArgs: KwArgs) extends AST()
case class DefStruct(name: Ident, fields: Fields) extends AST()
// form of (struct key=value)
case class ApplyStruct(s: AST, values: KwArgs) extends AST()

case class Select(obj: AST, field: Ident) extends AST()
