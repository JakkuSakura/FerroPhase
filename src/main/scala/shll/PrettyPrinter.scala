package shll

case class PrettyPrinter() {
  def printList(l: List[AST]): String = {
    l.map(printImpl).mkString(" ")
  }
  def printDict(d: List[Field]): String = {
    d.map(printImpl).mkString(" ")
  }
  def printImpl(a: AST): String = {
    a match {
      case Apply(f, args, kwArgs) =>
        s"(${printImpl(f)} ${printList(args)} ${printDict(kwArgs)})"
      case If(cond, consequence, alternative) =>
        s"(if ${printImpl(cond)} ${printImpl(consequence)} ${printImpl(alternative)})"
      case ForIn(target, iter, body) =>
        s"(for ${target.name} ${printImpl(iter)} ${printImpl(body)})"
      case Block(body) =>
        s"(block ${printList(body)})"
      case Ident(name) =>
        name
      case Literal(_, raw) =>
        raw
      case Field(name, value) =>
        s"${name.name}=${printImpl(value)}"
      case TypeApply(f, args, kwArgs) =>
        s"(type ${printImpl(f)} ${printList(args)} ${printDict(kwArgs)})"
      case Let(name, body) =>
        s"(let ${name.name} ${printImpl(body)})"
      case Assign(target, value) =>
        s"(assign ${target.name} ${printImpl(value)})"
    }
  }
  def print(a: AST): String = {
    val raw = printImpl(a)
    raw.replaceAll("\\s+", " ")
      .replaceAll("\\s+\\)", ")")
  }
}
