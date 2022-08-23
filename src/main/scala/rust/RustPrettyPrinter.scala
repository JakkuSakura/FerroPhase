package rust

import shll.*
import shll.ast.*

case class RustPrettyPrinter() {
  val IDENT = "  "
  def printList(l: List[AST]): String = {
    l.map(printImpl).mkString(" ")
  }

  def printDict(d: List[KeyValue]): String = {
    d.map(printImpl).mkString(" ")
  }
  def printImpl(a: AST): String = {
    a match {
      case Apply(f, Nil, kwArgs) =>
        s"${printImpl(f)}(${printDict(kwArgs)})"
      case Apply(f, args, Nil) =>
        s"${printImpl(f)}(${printList(args)})"
      case Apply(f, args, kwArgs) =>
        s"${printImpl(f)}(${printList(args)} ${printDict(kwArgs)})"
      case Cond(cond, consequence, alternative) =>
        s"if ${printImpl(cond)} { ${printImpl(consequence)} } else {${printImpl(alternative)}}"
      case ForIn(target, iter, body) =>
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
        s"${name.name}=${printImpl(value)}"
      case Field(name, ty) =>
        s"${name.name}=${printImpl(ty)}"
      case TypeApply(f, args, kwArgs) =>
        s"(type ${printImpl(f)} ${printList(args)} ${printDict(kwArgs)})"
      case DefVal(name, body) =>
        s"let ${name.name} = ${printImpl(body)};"
      case DefFun(name, args, ret, body) =>
        s"fn ${name.name}(${printList(args.value)}) -> ${printImpl(ret)} { ${printImpl(body)} }"
      case Assign(target, value) =>
        s"${target.name} = ${printImpl(value)}"
    }
  }
  def print(a: AST): String = {
    val raw = printImpl(a)
    raw
  }
}
