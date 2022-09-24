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
  def printList(l: List[AST]): String = {
    l.map(printImpl).mkString(", ")
  }

  def printDict(d: List[KeyValue]): String = {
    d.map(printImpl).mkString(", ")
  }

  val primitiveTypes: Map[String, String] = Map(
    "int" -> "i32",
    "float" -> "f64",
    "string" -> "String",
    "bool" -> "bool",
    "char" -> "char",
    "unit" -> "()",
    "any" -> "Any",
    "list" -> "Vec",
  )
  def printType(a: AST): String = {
    a match {
      case Ident(x) => primitiveTypes.getOrElse(x, x)
      case ApplyType(fun, Nil, Nil) => printType(fun)
      case ApplyType(fun, args, Nil) => printType(fun) + "<" + printType(args.head) + ">"
    }
  }
  def printImpl(a: AST): String = {
    a match {
      case Apply(f, Nil, kwArgs) =>
        s"${printImpl(f)}{${printDict(kwArgs)}}"
      case Apply(Ident("print"), List(arg), Nil) =>
        s"print!(\"{:?} \", ${printImpl(arg)});"
      case Apply(Ident("range"), List(lhs, rhs), Nil) =>
        s"${printImpl(lhs)}..${printImpl(rhs)}"
      case Apply(Ident("+"), List(lhs, rhs), Nil) =>
        s"(${printImpl(lhs)}+${printImpl(rhs)})"
      case Apply(f, args, Nil) =>
        s"${printImpl(f)}(${printList(args)})"
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
        s"vec![${printList(value)}]"
      case KeyValue(name, value) =>
        s"${name.name}: ${printImpl(value)}"
      case Field(name, ty) =>
        s"pub ${name.name}: ${printImpl(ty)}"
      case a: ApplyType =>
        printType(a)
      case DefVal(name, body) =>
        s"let mut ${name.name} = ${printImpl(body)};"
      case DefFun(name, args, ret, body) =>
        s"fn ${name.name}(${printList(args.value)}) -> ${printImpl(ret)}" + (body match {
          case Some(b) => s" { ${printImpl(b)} }"
          case None => ";"
        })

      case Assign(target, value) =>
        s"${target.name} = ${printImpl(value)}"
      case DefStruct(name, fields) =>
        s"struct ${name.name} { ${printList(fields.value)} }"
      case ApplyStruct(s, values) =>
        s"${printImpl(s)} { ${values
            .map(x => s"${x.name} = ${printImpl(x.value)}")
            .mkString(", ")} " +
          s"}"
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
