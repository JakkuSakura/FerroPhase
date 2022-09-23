package shll.frontends

import shll.ast.{AST, Apply, Ident, KeyValue}

case object ParamUtil {
  def getArgOpt(a: Apply, pos: Int, key: String): Option[AST] = {
    val p1 = a.args.lift(pos)
    val p2 = a.kwArgs.find(_.name.name == key)
    if (p1.isDefined && p2.isDefined)
      throw ParserException("Duplicate key: " + key)
    else
      p1.orElse(p2)
  }

  def getArg(a: Apply, pos: Int, key: String): AST =
    getArgOpt(a, pos, key).getOrElse(throw Exception("Missing key: " + key))

  def getIdentArg(a: Apply, pos: Int, key: String): Ident =
    getArg(a, pos, key) match {
      case i: Ident => i
      case _ => throw ParserException("Expected Ident, got: " + getArg(a, pos, key))
    }

  def checkParams(a: Apply, knownArgs: Array[Int], knownKwArgs: Array[String]): Unit = {
    collectParams(a.args, a.kwArgs, knownArgs, knownKwArgs)
  }
  def collectParams(args: List[AST], kwArgs: List[KeyValue], knownArgs: Array[Int], knownKwArgs: Array[String]): Map[String, AST] = {
    val res = collection.mutable.Map[String, AST]()
    for (i <- args.indices) {
      knownArgs.lift(i) match {
        case None => throw ParserException(s"Unknown positional argument: $i")
        case Some(x) if kwArgs.exists(_.name.name == knownKwArgs(x)) =>
          throw Exception(s"Duplicate key: $i vs ${knownKwArgs(x)}")
        case Some(x) =>
          res(knownKwArgs(x)) = args(i)
      }
    }
    for (kw <- kwArgs) {
      if (res.contains(kw.name.name))
        throw ParserException(s"Duplicate key: ${kw.name.name}")
      res(kw.name.name) = kw.value
    }
    res.toMap
  }
}
