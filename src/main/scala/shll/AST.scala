package shll

trait AST
case class Ident(name: String) extends AST
// basic elements
case class Literal[T](value: T, raw: String) extends AST
case class Field(name: Ident, ty: AST) extends AST
case class Apply(fun: AST, args: List[AST], kwArgs: List[Field]) extends AST
// other elements
case class DefFunc(name: Ident, args: AST, ret: AST, body: AST) extends AST
case class Let(name: Ident, value: AST) extends AST
case class Assign(name: Ident, value: AST) extends AST
case class If(cond: AST, consequence: AST, alternative: AST) extends AST
case class While(cond: AST, body: AST) extends AST
case class Block(body: List[AST]) extends AST
case class ForIn(name: Ident, iter: AST, body: AST) extends AST
case class DefStruct(name: Ident, fields: List[Field]) extends AST
case class TypeApply(fun: AST, args: List[AST], kw_args: List[Field]) extends AST

