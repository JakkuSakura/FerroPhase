package shll

case class ApplyParser() {
  def getArgOpt(a: Apply, pos: Int, key: String): Option[AST] = {
    val p1 = a.args.lift(pos)
    val p2 = a.kwArgs.find(_.name == key)
    if (p1.isDefined && p2.isDefined)
      throw Exception("Duplicate key: " + key)
    else
      p1.orElse(p2)
  }

  def getArg(a: Apply, pos: Int, key: String): AST =
    getArgOpt(a, pos, key).getOrElse(throw Exception("Missing key: " + key))

  def parse(n: AST): AST = {
    n match {
      case a @ Apply(Ident("if"), args, kwArgs) =>
        If(getArg(a, 0, "cond"), getArg(a, 1, "then"), getArg(a, 2, "else"))
      case a @ Apply(Ident("while"), args, kwArgs) =>
        While(getArg(a, 0, "cond"), getArg(a, 1, "body"))
      case a @ Apply(Ident("for"), args, kwArgs) =>
        ForIn(getArg(a, 0, "name"), getArg(a, 1, "iter"), getArg(a, 2, "body"))
      case a @ Apply(Ident("def-fun"), args, kwArgs) =>
        DefFunc(
          getArg(a, 0, "name"),
          getArg(a, 1, "args"),
          getArg(a, 2, "ret"),
          getArg(a, 3, "body")
        )
      case a @ Apply(Ident("let"), args, kwArgs) =>
        Let(getArg(a, 0, "name"), getArg(a, 1, "value"))
      case a @ Apply(Ident("assign"), args, kwArgs) =>
        Assign(getArg(a, 0, "name"), getArg(a, 1, "value"))
      case a @ Apply(Ident("type"), args, kwArgs) =>
        TypeApply(args, kwArgs)
      case _ => n
    }
  }
}
