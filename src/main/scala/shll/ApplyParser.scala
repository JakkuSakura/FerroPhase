package shll

case class ApplyParser() {
  def getArgOpt(a: Apply, pos: Int, key: String): Option[AST] = {
    val p1 = a.args.lift(pos)
    val p2 = a.kwArgs.find(_.name.name == key)
    if (p1.isDefined && p2.isDefined)
      throw Exception("Duplicate key: " + key)
    else
      p1.orElse(p2)
  }

  def getArg(a: Apply, pos: Int, key: String): AST =
    getArgOpt(a, pos, key).getOrElse(throw Exception("Missing key: " + key))

  def getIdentArg(a: Apply, pos: Int, key: String): Ident =
    getArg(a, pos, key) match {
      case i: Ident => i
      case _ => throw Exception("Expected Ident, got: " + getArg(a, pos, key))
    }
  def parse(n: AST): AST = {
    n match {
      case a @ Apply(Ident("if"), args, kwArgs) =>
        Cond(getArg(a, 0, "cond"), getArg(a, 1, "then"), getArg(a, 2, "else"))
      case a @ Apply(Ident("while"), args, kwArgs) =>
        While(getArg(a, 0, "cond"), getArg(a, 1, "body"))
      case a @ Apply(Ident("for"), args, kwArgs) =>
        ForIn(getIdentArg(a, 0, "name"), getArg(a, 1, "iter"), getArg(a, 2, "body"))
      case a @ Apply(Ident("def-fun"), args, kwArgs) =>
        DefFun(
          getIdentArg(a, 0, "name"),
          parse(getArg(a, 1, "args")).asInstanceOf[LiteralList],
          getArg(a, 2, "ret"),
          getArg(a, 3, "body")
        )
      case a @ Apply(Ident("def-val"), args, kwArgs) =>
        DefVal(getIdentArg(a, 0, "name"), getArg(a, 1, "value"))
      case a @ Apply(Ident("assign"), args, kwArgs) =>
        Assign(getIdentArg(a, 0, "name"), getArg(a, 1, "value"))
      case a @ Apply(Ident("block"), args, kwArgs) =>
        Block(args.map(parse))
      case a @ Apply(Ident("list"), args, kwArgs) =>
        LiteralList(args.map(parse))
      case a @ Apply(Ident("type"), args, kwArgs) =>
        TypeApply(args.head, args.slice(1, args.length), kwArgs)
      case _ => n
    }
  }
}
