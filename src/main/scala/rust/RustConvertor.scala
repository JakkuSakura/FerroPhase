package rust

import shll.ast.*

case class RustConvertor() {
  def convert(n: RustAST): AST = {
    parseRustAstToShllAST(n)
  }
  def mapLiteralType(s: String): String = {
    s match {
      case "i32" => "int"
      case "()" => "unit"
      case x => x
    }
  }

  def parseRustAstToShllAST(n: RustAST): AST = {
    n match {
      case RustItems(attrs, items) =>
        val filtered = items.filterNot(_.isInstanceOf[RustUnknownAST])
        filtered match {
          case x if x.length == 1 => parseRustAstToShllAST(x.head)
          case _ =>
            Block(filtered.map(parseRustAstToShllAST))
        }
      case RustBody(stmts) if stmts.length == 1 => stmts.map(parseRustAstToShllAST).head
      case RustBody(stmts) => Block(stmts.map(parseRustAstToShllAST))
      case RustDefFun(name, args, ret, body) =>
        DefFun(
          Ident(name),
          Parameters(args.params.map(parseRustAstToShllAST).map(_.asInstanceOf[Field])),
          AstHelper.literalType(mapLiteralType(ret)),
          Some(parseRustAstToShllAST(body))
        )
      case RustUnit() => AstHelper.literalType("unit")
      case RustParam(name, ty, byValue) =>
        Field(Ident(name), AstHelper.literalType(mapLiteralType(ty)))
      case RustIdent(name) => Ident(name)
      case RustUnknownAST(_) => LiteralUnknown()
    }
  }

}
