package rust
import java.io.File
import java.lang.ProcessBuilder.Redirect
import java.nio.file.Files
import io.circe.{Decoder, HCursor, Json}
import shll.ast.*
import io.circe.syntax.*
import scala.io.Source
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
class RustParser {
  def getAST(n: String): RustItems = {
    // rustc -Z ast-json - on old rust, in json format
    // rustc -Z unpretty=ast-tree - on latest rust, in ron format
    val proc = ProcessBuilder("rustc", "-Z", "ast-json", "-")
      .redirectError(Redirect.INHERIT)
      .start()
    proc.getOutputStream.write(n.getBytes)
    proc.getOutputStream.close()
    val code = proc.waitFor()
    if (code != 0)
      throw Exception("Compilation failed, status code " + code)
    val s = Source.fromInputStream(proc.getInputStream).mkString

    val j = io.circe.parser
      .parse(s)
      .fold(err => throw Exception("Failed to parse JSON: " + err), identity)

    parseJsonToRustItems(j.hcursor)
  }
  def parseJsonToRustAST(j: HCursor): RustAST = {
    val kind = j
      .downField("kind")
      .downField("variant")
      .as[String]
      .fold(err => throw Exception("Failed to parse Rust kind: " + err), identity)
    kind match {
      case "Fn" =>
        parseJsonToRustDefFun(j)
      case "Expr" =>
        parseJsonToRustExpr(j)
      case "Path" =>
        RustIdent(
          j.downField("kind")
            .downField("fields")
            .downN(1)
            .downField("segments")
            .downN(0)
            .downField("ident")
            .downField("name")
            .as[String]
            .fold(err => throw Exception("Failed to parse JSON: " + err), identity)
        )
      case _ =>
//        println("Failed to parse: " + j.value.noSpaces)
        RustUnknownAST(j.value)
    }
  }

  def parseJsonToRustExpr(j: HCursor): RustAST = {
    parseJsonToRustAST(
      j.downField("kind")
        .downField("fields")
        .downN(0)
        .as[HCursor]
        .fold(err => throw Exception("Failed to parse Rust Expr: " + err), identity)
    )
  }
  def parseJsonToBody(j: HCursor): RustBody = {
    val stmts = j
      .downField("stmts")
      .as[List[HCursor]]
      .map(_.map(parseJsonToRustAST))
      .getOrElse(throw Exception("Failed to parse body: " + j))
    RustBody(stmts)
  }
  def parseJsonToRustParam(j: HCursor): RustParam = {
    val name =
      j
        .downField("pat")
        .downField("kind")
        .downField("fields")
        .downN(1)
        .downField("name")
        .as[String]
        .fold(
          err => throw Exception("Failed to parse param name: " + err),
          identity
        )
    val byValue = j
      .downField("pat")
      .downField("kind")
      .downField("fields")
      .downN(0)
      .downField("variant")
      .as[String]
      .fold(err => throw Exception("Failed to parse param by_value: " + err), identity) == "ByValue"
    val ty =
      j.downField("ty")
        .downField("kind")
        .downField("fields")
        .downN(1)
        .downField("segments")
        .downN(0)
        .downField("ident")
        .downField("name")
        .as[String]
        .fold(err => throw Exception("Failed to parse param ty: " + err), identity)
    RustParam(name, ty, byValue)
  }
  def parseJsonToRustParams(j: HCursor): RustParams = {
    val params = j
      .as[List[HCursor]]
      .map(_.map(parseJsonToRustParam))
      .getOrElse(throw Exception("Failed to parse params: " + j))
    RustParams(params)
  }
  def parseJsonToRustReturnType(j: HCursor): String = {
    if (j.downField("variant").as[String].exists(_ == "Default"))
      return "()"
    j
      .downField("fields")
      .downN(0)
      .downField("kind")
      .downField("fields")
      .downN(1)
      .downField("segments")
      .downN(0)
      .downField("ident")
      .downField("name")
      .as[String]
      .fold(err => throw Exception("Failed to parse return type: " + j.value), identity)
  }
  def parseJsonToRustDefFun(j: HCursor): RustDefFun = {
//    println("Parsing function: " + j.value.noSpaces)
    val name = j.downField("ident").downField("name").as[String].getOrElse("unknown")
    val args = parseJsonToRustParams(
      j.downField("kind")
        .downField("fields")
        .downN(0)
        .downField("sig")
        .downField("decl")
        .downField("inputs")
        .as[HCursor]
        .fold(
          err => throw Exception("Failed to parse args: " + err),
          identity
        )
    )

    val ret = parseJsonToRustReturnType(
      j.downField("kind")
        .downField("fields")
        .downN(0)
        .downField("sig")
        .downField("decl")
        .downField("output")
        .as[HCursor]
        .fold(
          err => throw Exception("Failed to parse r: " + err),
          identity
        )
    )

    val body = parseJsonToBody(
      j
        .downField("kind")
        .downField("fields")
        .downN(0)
        .downField("body")
        .as[HCursor]
        .fold(err => throw Exception("Failed to parse body: " + err), identity)
    )
    RustDefFun(name, args, ret, body)
  }
  def parseJsonToRustItems(j: HCursor): RustItems = {

    RustItems(
      j
        .downField("attrs")
        .as[List[Json]]
        .fold(err => throw Exception("Failed to parse JSON: " + err), identity),
      j
        .downField("items")
        .as[List[HCursor]]
        .fold(err => throw Exception("Failed to parse JSON: " + err), identity)
        .map(parseJsonToRustAST)
    )
  }
  def parse(n: String): AST = {
    val ast = getAST(n)
    parseRustAstToShllAST(ast)
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
