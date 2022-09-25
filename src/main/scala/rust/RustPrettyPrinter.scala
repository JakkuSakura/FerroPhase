package rust

import shll.*
import shll.ast.*
import shll.backends.{PrettyPrinter, TextTool}

import java.lang.ProcessBuilder.Redirect
import scala.io.Source

case class RustPrettyPrinter() extends PrettyPrinter {
  val IDENT = "  "
  val NL = "\n"
  val textTool: TextTool = TextTool(NL = NL, INDENT = IDENT)

  val primitiveTypes: Map[String, String] = Map(
    "int" -> "i32",
    "float" -> "f64",
    "string" -> "String",
    "bool" -> "bool",
    "char" -> "char",
    "unit" -> "()",
    "any" -> "Any",
    "list" -> "Vec"
  )
  def printType(a: AST): String = {
    a match {
      case Ident(x) => primitiveTypes.getOrElse(x, x)
      case ApplyType(fun, PosArgs(Nil), KwArgs(Nil)) =>
        printType(fun)
      case ApplyType(fun, args, KwArgs(Nil)) =>
        printType(fun) + "<" + args.args.map(printType).mkString(", ") + ">"
    }
  }
  def printImpl(a: AST): String = {
    a match {
      case x: Apply if x.args.args.isEmpty =>
        s"${printImpl(x.fun)}{${printImpl(x.kwArgs)}}"
      case x: Apply if x.fun == Ident("print") && x.args.args.length == 1 =>
        s"print!(\"{:?} \", ${printImpl(x.args.args.head)});"
      case x: Apply if x.fun == Ident("range") && x.args.args.length == 2 =>
        s"${printImpl(x.args.args.head)}..${printImpl(x.args.args.last)}"
      case x: Apply if x.fun == Ident("+") && x.args.args.length == 2 =>
        s"(${printImpl(x.args.args.head)}+${printImpl(x.args.args.last)})"
      case x: Apply if x.kwArgs.args.isEmpty =>
        s"${printImpl(x.fun)}(${printImpl(x.args)})"
      case Cond(cond, consequence, alternative) =>
        s"if ${printImpl(cond)} { ${printImpl(consequence)} } else {${printImpl(alternative)}}"
      case ForEach(target, iter, body) =>
        s"for ${target.name} in ${printImpl(iter)} {$NL ${textTool.indent(printImpl(body))} $NL}"
      case Block(Nil) =>
        "{}"
      case Block(body) =>
        s"{$NL${body.map(x => IDENT + printImpl(x)).mkString(NL)}$NL}"
      case Ident(name) =>
        name
      case LiteralInt(v) =>
        v.toString
      case LiteralDecimal(v) =>
        v.toString
      case LiteralChar(v) =>
        s"'$v'"
      case LiteralString(v) =>
        s"\"$v\""
      case LiteralBool(value) => value.toString
      case LiteralList(value) =>
        s"vec![${value.map(printImpl).mkString(", ")}]"
      case PosArgs(args) =>
        args.map(printImpl).mkString(", ")
      case KwArgs(args) =>
        args.map(x => printImpl(x)).mkString(", ")
      case KeyValue(name, value) =>
        s"${name.name}: ${printImpl(value)}"
      case Field(name, ty) =>
        s"pub ${name.name}: ${printImpl(ty)}"
      case a: ApplyType =>
        printType(a)
      case DefVal(name, body) =>
        s"let mut ${name.name} = ${printImpl(body)};"
      case DefFun(name, args, ret, body) =>
        s"fn ${name.name}(${args.params.map(printImpl).mkString(", ")}) -> ${printImpl(ret)}" + (body match {
          case Some(b) => s" { ${printImpl(b)} }"
          case None => ";"
        })

      case Assign(target, value) =>
        s"${printImpl(target)} = ${printImpl(value)}"
      case DefStruct(name, fields) =>
        s"struct ${name.name} { ${fields.fields.map(printImpl).mkString(", ")} }"
      case ApplyStruct(s, values) =>
        s"${printImpl(s)} {" + printImpl(values) + s"}"
      case DefType(name, value) =>
        s"type ${name.name} = ${printImpl(value)}"
      case Select(obj, field) =>
        s"${printImpl(obj)}.${field.name}"
    }
  }
  def print(a: AST): String = {
    val raw = printImpl(a)
    raw
  }
}
