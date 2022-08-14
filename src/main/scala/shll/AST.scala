package shll

trait AST
case class Ident(name: String) extends AST
// basic elements
case class Literal[T](value: T, raw: String) extends AST
case class Field(name: String, ty: AST) extends AST
case class Apply(fun: AST, args: List[AST], kw_args: List[Field]) extends AST
// other elements
case class DefFunc(name: String, args: List[Field], ret: Type, body: AST) extends AST
case class Let(name: String, ty: Type, value: AST) extends AST
case class Assign(name: String, value: AST) extends AST
case class TypeApply(fun: AST, args: List[AST], kw_args: List[Field]) extends AST
case class If(cond: AST, consequence: AST, alternative: AST) extends AST
case class While(cond: AST, body: AST) extends AST
case class ForIn(name: String, iter: AST, body: AST) extends AST
