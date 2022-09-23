package shll.frontends

import shll.ast.*
import ParamUtil._

case class ApplyParser() {
  def getArgAndParse(n: Apply, i: Int, name: String): AST = parse(ParamUtil.getArg(n, i, name))
  def getArgOptAndParse(n: Apply, i: Int, name: String): Option[AST] = ParamUtil.getArgOpt(n, i, name).map(parse)
  def parse(n: AST): AST = {
    n match {
      case a @ Apply(Ident("if"), args, kwArgs) =>
        checkArguments(a, Array(0, 1, 2), Array("cond", "then", "else"))
        Cond(getArgAndParse(a, 0, "cond"), getArgAndParse(a, 1, "then"), getArgAndParse(a, 2, "else"))
      case a @ Apply(Ident("while"), args, kwArgs) =>
        checkArguments(a, Array(0, 1), Array("cond", "body"))
        While(getArgAndParse(a, 0, "cond"), getArgAndParse(a, 1, "body"))
      case a @ Apply(Ident("for"), args, kwArgs) =>
        checkArguments(a, Array(0, 1, 2), Array("name", "iter", "body"))
        ForEach(getIdentArg(a, 0, "name"), getArgAndParse(a, 1, "iter"), getArgAndParse(a, 2, "body"))
      case a @ Apply(Ident("def-fun"), args, kwArgs) =>
        checkArguments(a, Array(0, 1, 2, 3), Array("name", "args", "ret", "body"))
        DefFun(
          getIdentArg(a, 0, "name"),
          getArgAndParse(a, 1, "args").asInstanceOf[LiteralList],
          getArgAndParse(a, 2, "ret"),
          getArgOptAndParse(a, 3, "body")
        )
      case a @ Apply(Ident("def-val"), args, kwArgs) =>
        checkArguments(a, Array(0, 1), Array("name", "value"))
        DefVal(getIdentArg(a, 0, "name"), getArgAndParse(a, 1, "value"))
      case a @ Apply(Ident("def-struct"), args, kwArgs) =>
        checkArguments(a, Array(0, 1), Array("name", "fields"))
        DefStruct(
          getIdentArg(a, 0, "name"),
          parse(getArgAndParse(a, 1, "fields")).asInstanceOf[LiteralList]
        )
      case a @ Apply(Ident("assign"), args, kwArgs) =>
        checkArguments(a, Array(0, 1), Array("name", "value"))
        Assign(getIdentArg(a, 0, "name"), getArgAndParse(a, 1, "value"))
      case a @ Apply(Ident("block"), args, kwArgs) =>
        // Block is special
        if (kwArgs.nonEmpty) {
            throw ParserException("Block does not support keyword arguments yet")
        }
        Block(args.map(parse))
      case a @ Apply(Ident("field"), args, kwArgs) =>
        checkArguments(a, Array(0, 1), Array("name", "type"))
        Field(getIdentArg(a, 0, "name"), getArgAndParse(a, 1, "type"))
      case a @ Apply(Ident("select"), args, kwArgs) =>
        checkArguments(a, Array(0, 1), Array("obj", "field"))
        Select(getArgAndParse(a, 0, "obj"), getIdentArg(a, 1, "field"))
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
