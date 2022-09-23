package rust

import shll.*
import shll.ast.*
import shll.backends.PrettyPrinter

case class RustPrettyPrinter() extends PrettyPrinter {
  val IDENT = "  "
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
      case TypeApply(fun, Nil, Nil) => printType(fun)
      case TypeApply(fun, args, Nil) => printType(fun) + "<" + printType(args.head) + ">"
    }
  }
  def printImpl(a: AST): String = {
    a match {
      case Apply(f, Nil, kwArgs) =>
        s"${printImpl(f)}{${printDict(kwArgs)}}"
      case Apply(Ident("print"), List(arg), Nil) =>
        s"println!(\"{:?}\", ${printImpl(arg)});"
      case Apply(f, args, Nil) =>
        s"${printImpl(f)}(${printList(args)})"
      case Cond(cond, consequence, alternative) =>
        s"if ${printImpl(cond)} { ${printImpl(consequence)} } else {${printImpl(alternative)}}"
      case ForEach(target, iter, body) =>
        s"for ${target.name} in ${printImpl(iter)} { ${printImpl(body)} }"
      case Block(Nil) =>
        "{}"
      case Block(body) =>
        s"{\n${body.map(x => IDENT + printImpl(x)).mkString("\n")}\n}"
      case Ident(name) =>
        name
      case LiteralInt(v, _) =>
        v.toString
      case LiteralDecimal(v, _) =>
        v.toString
      case LiteralChar(v, _) =>
        s"'$v'"
      case LiteralString(v, _) =>
        s"\"$v\""
      case LiteralBool(value) => value.toString
      case LiteralList(value) =>
        s"vec![${printList(value)}]"
      case KeyValue(name, value) =>
        s"${name.name}: ${printImpl(value)}"
      case Field(name, ty) =>
        s"pub ${name.name}: ${printImpl(ty)}"
      case a: TypeApply =>
        printType(a)
      case DefVal(name, body) =>
        s"let ${name.name} = ${printImpl(body)};"
      case DefFun(name, args, ret, body) =>
        s"fn ${name.name}(${printList(args.value)}) -> ${printImpl(ret)}" + (body match {
          case Some(b) => s" { ${printImpl(b)} }"
          case None => ";"
        })

      case Assign(target, value) =>
        s"${target.name} = ${printImpl(value)}"
      case DefStruct(name, fields, Nil) =>
        s"struct ${name.name} { ${printList(fields.value)} }"
      case DefStruct(name, fields, values) =>
        s"${name.name} { ${values
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
