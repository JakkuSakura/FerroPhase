package shll.ast

import org.antlr.v4.runtime.Token
import shll.*
import shll.backends.ShllPrettyPrinter

object Ast {
  var count: Int = 0
  def genNum: Int = {
    count += 1
    count
  }

}
trait Ast(
    var num: Int = Ast.genNum,
    var token: Option[Token] = None,
    var origin: Option[Ast] = None
) extends Cloneable {

  def duplicate(): this.type = {
    val dup: this.type = this.clone().asInstanceOf[this.type]
    num = Ast.genNum
    dup
  }
  def withToken(token: Token): this.type = {
    this.token = Some(token)
    this
  }

  override def toString: String = ShllPrettyPrinter.print(this)
}

case class Ident(name: String) extends Ast()

case class LiteralInt(value: Int) extends Ast()
case class LiteralBool(value: Boolean) extends Ast()
case class LiteralDecimal(value: Double) extends Ast()
case class LiteralChar(value: Char) extends Ast()
case class LiteralString(value: String) extends Ast()
case class BuildList(value: List[Ast]) extends Ast()
case class LiteralUnknown() extends Ast()
case class Param(name: Ident, ty: Ast) extends Ast()

case class Params(params: List[Param]) extends Ast()
case class PosArgs(args: List[Ast]) extends Ast()
case class KwArg(name: Ident, value: Ast) extends Ast()
case class KwArgs(args: List[KwArg]) extends Ast()
case class Field(name: Ident, ty: Ast) extends Ast()
case class Fields(fields: List[Field]) extends Ast()
case class Apply(fun: Ast, args: PosArgs, kwArgs: KwArgs) extends Ast()

case class DefFun(name: Ident, params: Params, ret: Ast, body: Ast) extends Ast()
case class DeclFun(name: Ident, params: Params, ret: Ast) extends Ast()
// form of (fun (list (field a [int])) x)
case class BuildFun(params: Params, ret: Ast, body: Ast) extends Ast()
case class DefVal(name: Ident, value: Ast) extends Ast()
case class DefType(name: Ident, params: Params, value: Ast) extends Ast()
case class Assign(target: Ast, value: Ast) extends Ast()
case class Cond(cond: Ast, consequence: Ast, alternative: Ast) extends Ast()
case class While(cond: Ast, body: Ast) extends Ast()
case class Block(children: List[Ast]) extends Ast()
case class ForEach(variable: Ident, iterable: Ast, body: Ast) extends Ast()
// form of [type args]
case class Compose(fun: Ast, args: PosArgs, kwArgs: KwArgs) extends Ast()
case class DefStruct(name: Ident, fields: Fields) extends Ast()
// form of (struct key=value)
case class BuildStruct(s: Ast, values: KwArgs) extends Ast()

case class Select(obj: Ast, field: Ident) extends Ast()
case class Get(obj: Ast, field: Ident) extends Ast()
