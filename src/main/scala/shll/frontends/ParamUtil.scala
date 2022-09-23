package shll.frontends

import shll.ast._

case object ParamUtil {
  def getArgOpt(args: List[AST], kwArgs: List[KeyValue], pos: Int, key: String): Option[AST] = {
    val p1 = args.lift(pos)
    val p2 = kwArgs.find(_.name.name == key)
    if (p1.isDefined && p2.isDefined)
      throw ParserException("Duplicate key: " + key)
    else
      p1.orElse(p2)
  }

  def getArg(args: List[AST], kwArgs: List[KeyValue], pos: Int, key: String): AST =
    getArgOpt(args, kwArgs, pos, key).getOrElse(throw Exception("Missing key: " + key))

  def getIdentArg(args: List[AST], kwArgs: List[KeyValue], pos: Int, key: String): Ident =
    getArg(args, kwArgs, pos, key) match {
      case i: Ident => i
      case _ => throw ParserException("Expected Ident, got: " + getArg(args, kwArgs, pos, key))
    }

  def checkArguments(args: List[AST], kwArgs: List[KeyValue], knownArgs: Array[Int], knownKwArgs: Array[String]): Unit = {
    collectArguments(args, kwArgs, knownArgs, knownKwArgs)
  }

  def collectArguments(args: List[AST], kwArgs: List[KeyValue], knownArgs: Array[Int], knownKwArgs: Array[String]): Map[String, AST] = {
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
