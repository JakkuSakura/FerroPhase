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
  def printImpl(a: AST): String = {
    a match {
      case Apply(f, Nil, kwArgs) =>
        s"${printImpl(f)}{${printDict(kwArgs)}}"
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
      case LiteralInt(_, raw) =>
        raw
      case LiteralDecimal(_, raw) =>
        raw
      case LiteralChar(_, raw) =>
        raw
      case LiteralString(_, raw) =>
        raw
      case LiteralList(value) =>
        s"vec![${printList(value)}]"
      case KeyValue(name, value) =>
        s"${name.name}: ${printImpl(value)}"
      case Field(name, ty) =>
        s"pub ${name.name}: ${printImpl(ty)}"
      case TypeApply(Ident("int"), args, kwArgs) =>
        "i64"
      case TypeApply(f, args, kwArgs) =>
        s"[${printImpl(f)} ${printList(args)} ${printDict(kwArgs)}]".replaceAll(" +", " ")
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
      case Select(obj, field) =>
        s"${printImpl(obj)}.${field.name}"
    }
  }
  def print(a: AST): String = {
    val raw = printImpl(a)
    raw
  }
}
