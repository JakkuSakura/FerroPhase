package shll.frontends

import shll.ast.*
import ParamUtil._

case class ApplyParser() {

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
            throw ParserException("Block does not support keyword arguments yet")
        }
        Block(args.map(parse))
      case a @ Apply(Ident("field"), args, kwArgs) =>
        checkParams(a, Array(0, 1), Array("name", "type"))
        Field(getIdentArg(a, 0, "name"), getArg(a, 1, "type"))
      case a @ Apply(Ident("select"), args, kwArgs) =>
        checkParams(a, Array(0, 1), Array("obj", "field"))
        Select(getArg(a, 0, "obj"), getIdentArg(a, 1, "field"))
      case a @ Apply(Ident("list"), args, kwArgs) =>
        // List is special
        if (kwArgs.nonEmpty) {
            throw ParserException("List does not support keyword arguments yet")
        }
        LiteralList(args.map(parse))
      case _ => n
    }
  }
}
