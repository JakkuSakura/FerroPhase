package shll

case class PrettyPrinter() {
  val IDENT = "  "
  def printList(l: List[AST]): String = {
    l.map(printImpl).mkString(" ")
  }

  def printDict(d: List[KeyValue]): String = {
    d.map(printImpl).mkString(" ")
  }
  def printImpl(a: AST): String = {
    a match {
      case Apply(f, Nil, Nil) =>
        s"(${printImpl(f)})"
      case Apply(f, Nil, kwArgs) =>
        s"(${printImpl(f)} ${printDict(kwArgs)})"
      case Apply(f, args, Nil) =>
        s"(${printImpl(f)} ${printList(args)})"
      case Apply(f, args, kwArgs) =>
        s"(${printImpl(f)} ${printList(args)} ${printDict(kwArgs)})"
      case Cond(cond, consequence, alternative) =>
        s"(if ${printImpl(cond)} ${printImpl(consequence)} ${printImpl(alternative)})"
      case ForIn(target, iter, body) =>
        s"(for ${target.name} ${printImpl(iter)} ${printImpl(body)})"
      case Block(Nil) =>
        "(block)"
      case Block(body) =>
        s"(block\n${body.map(x => IDENT + printImpl(x)).mkString("\n")}\n)"
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
      case LiteralList(Nil) =>
        s"(list)"
      case LiteralList(value) =>
        s"(list ${printList(value)})"
      case KeyValue(name, value) =>
        s"${name.name}=${printImpl(value)}"
      case Field(name, ty) =>
        s"${name.name}=${printImpl(ty)}"
      case TypeApply(f, args, kwArgs) =>
        s"(type ${printImpl(f)} ${printList(args)} ${printDict(kwArgs)})"
      case DefVal(name, body) =>
        s"(def-val ${name.name} ${printImpl(body)})"
      case DefFun(name, args, ret, body) =>
        s"(def-fun ${name.name} ${printImpl(args)} ${printImpl(ret)} ${printImpl(body)})"
      case Assign(target, value) =>
        s"(assign ${target.name} ${printImpl(value)})"
    }
  }
  def print(a: AST): String = {
    val raw = printImpl(a)
    raw
  }
}
