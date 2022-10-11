package rust

import shll.ast.*

case class RustConvertor() {
  def convert(n: RustAst): Ast = {
    parseRustAstToShllAST(n)
  }
  def mapLiteralType(s: String): String = {
    s match {
      case "i32" => "int"
      case "()" => "unit"
      case x => x
    }
  }

  def parseRustAstToShllAST(n: RustAst): Ast = {
    n match {
      case RustItems(attrs, items) =>
        val filtered = items.filterNot(_.isInstanceOf[RustUnknownAst])
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
          Params(args.params.map(parseRustAstToShllAST).map(_.asInstanceOf[Param])),
          AstHelper.literalType(mapLiteralType(ret)),
          parseRustAstToShllAST(body)
        )
      case RustUnit() => AstHelper.tUnit
      case RustParam(name, ty, byValue) =>
        Param(Ident(name), AstHelper.literalType(mapLiteralType(ty)))
      case RustIdent(name) => Ident(name)
      case RustUnknownAst(_) => LiteralUnknown()
    }
  }

}
