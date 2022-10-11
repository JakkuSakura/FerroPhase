package rust

import io.circe.Json

trait RustAst

case class RustUnknownAst(raw: Json) extends RustAst {
  override def toString: String = {
    val r = raw.noSpaces
    "RustUnknownAST(" + r.substring(0, Math.min(10, r.length)) + ")"
  }
}
case class RustUnit() extends RustAst
case class RustItems(attrs: List[Json], items: List[RustAst]) extends RustAst
case class RustBody(stmts: List[RustAst]) extends RustAst
case class RustParam(name: String, ty: String, byValue: Boolean) extends RustAst
case class RustParams(params: List[RustParam]) extends RustAst
case class RustDefFun(name: String, args: RustParams, ret: String, body: RustBody) extends RustAst
case class RustIdent(name: String) extends RustAst
