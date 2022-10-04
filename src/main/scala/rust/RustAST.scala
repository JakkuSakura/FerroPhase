package rust

import io.circe.Json

trait RustAST

case class RustUnknownAST(raw: Json) extends RustAST {
  override def toString: String = {
    val r = raw.noSpaces
    "RustUnknownAST(" + r.substring(0, Math.min(10, r.length)) + ")"
  }
}
case class RustUnit() extends RustAST
case class RustItems(attrs: List[Json], items: List[RustAST]) extends RustAST
case class RustBody(stmts: List[RustAST]) extends RustAST
case class RustParam(name: String, ty: String, byValue: Boolean) extends RustAST
case class RustParams(params: List[RustParam]) extends RustAST
case class RustDefFun(name: String, args: RustParams, ret: String, body: RustBody) extends RustAST
case class RustIdent(name: String) extends RustAST
