package rust
import java.io.File
import java.lang.ProcessBuilder.Redirect
import java.nio.file.Files
import io.circe.{Decoder, HCursor, Json}
import shll.ast.*
import io.circe.syntax.*
import scala.io.Source

object RustAstFlavor {
  val RustcOld: String = "rustc-old"
  val Rustc: String = "rustc"
  val Syn: String = "syn"
}

def callRustAstCli(n: String, flavor: String): Json = {
  // rustc -Z ast-json - on old rust, in json format
  // rustc -Z unpretty=ast-tree - on latest rust, in ron format
  // rust-ast - wrapper around new rustc, in json format
  val proc = ProcessBuilder("rust-ast", "--flavor", flavor)
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
  j
}
case class RustParser(
    flavor: String = RustAstFlavor.RustcOld
) {
  def parse(n: String): RustItems = {
    val j = callRustAstCli(n, flavor)
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

}
