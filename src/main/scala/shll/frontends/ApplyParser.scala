package shll.frontends

import shll.ast.*

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

  def checkParams(a: Apply, knownArgs: Array[Int], knownKwArgs: Array[String]): Unit = {
    for (i <- a.args.indices) {
      knownArgs.lift(i) match {
        case None => throw Exception(s"Unknown positional argument: $i")
        case Some(x) if a.kwArgs.exists(_.name.name == knownKwArgs(x)) => throw Exception(s"Duplicate key: $i vs ${knownKwArgs(x)}")
        case _ =>
      }
    }
    val nameOccurance = a.kwArgs.map(_.name.name).groupBy(identity).view.mapValues(_.size).toMap
    for (n <- nameOccurance.keys) {
      if (nameOccurance(n) > 1)
        throw Exception(s"Duplicate key: $n")
      if (!knownKwArgs.contains(n))
        throw Exception(s"Unknown keyword argument: $n")
    }
  }
  def parse(n: AST): AST = {

    n match {
      case a @ Apply(Ident("if"), args, kwArgs) =>
        checkParams(a, Array(0, 1, 2), Array("cond", "then", "else"))
        Cond(getArg(a, 0, "cond"), getArg(a, 1, "then"), getArg(a, 2, "else"))
      case a @ Apply(Ident("while"), args, kwArgs) =>
        checkParams(a, Array(0, 1), Array("cond", "body"))
        While(getArg(a, 0, "cond"), getArg(a, 1, "body"))
      case a @ Apply(Ident("for"), args, kwArgs) =>
        checkParams(a, Array(0, 1, 2), Array("name", "iter", "body"))
        ForIn(getIdentArg(a, 0, "name"), getArg(a, 1, "iter"), getArg(a, 2, "body"))
      case a @ Apply(Ident("def-fun"), args, kwArgs) =>
        checkParams(a, Array(0, 1, 2, 3), Array("name", "args", "ret", "body"))
        DefFun(
          getIdentArg(a, 0, "name"),
          parse(getArg(a, 1, "args")).asInstanceOf[LiteralList],
          getArg(a, 2, "ret"),
          getArg(a, 3, "body")
        )
      case a @ Apply(Ident("def-val"), args, kwArgs) =>
        checkParams(a, Array(0, 1), Array("name", "value"))
        DefVal(getIdentArg(a, 0, "name"), getArg(a, 1, "value"))
      case a @ Apply(Ident("def-struct"), args, kwArgs) =>
        checkParams(a, Array(0, 1), Array("name", "fields"))
        DefStruct(
          getIdentArg(a, 0, "name"),
          parse(getArg(a, 1, "fields")).asInstanceOf[LiteralList]
        )
      case a @ Apply(Ident("assign"), args, kwArgs) =>
        checkParams(a, Array(0, 1), Array("name", "value"))
        Assign(getIdentArg(a, 0, "name"), getArg(a, 1, "value"))
      case a @ Apply(Ident("block"), args, kwArgs) =>
        // Block is special
        if (kwArgs.nonEmpty) {
            throw Exception("Block does not support keyword arguments yet")
        }
        Block(args.map(parse))
      case a @ Apply(Ident("select"), args, kwArgs) =>
        checkParams(a, Array(0, 1), Array("obj", "field"))
        Select(getIdentArg(a, 0, "obj"), getIdentArg(a, 1, "field"))
      case a @ Apply(Ident("list"), args, kwArgs) =>
        // List is special
        if (kwArgs.nonEmpty) {
            throw Exception("List does not support keyword arguments yet")
        }
        LiteralList(args.map(parse))
      case _ => n
    }
  }
}
