package shll.frontends

import shll.ast.*
import ParamUtil.*
import shll.ast.AstTool.*

case class ApplyParser() {
  def getArgAndParse(args: List[AST], kwArgs: List[KeyValue], i: Int, name: String): AST = parse(
    ParamUtil.getArg(args, kwArgs, i, name)
  )
  def getArgOptAndParse(
      args: List[AST],
      kwArgs: List[KeyValue],
      i: Int,
      name: String
  ): Option[AST] = ParamUtil.getArgOpt(args, kwArgs, i, name).map(parse)
  def parse(n: AST): AST = {
    n match {
      case Apply(Ident("if"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1, 2), Array("cond", "then", "else"))
        Cond(
          getArgAndParse(args, kwArgs, 0, "cond"),
          getArgAndParse(args, kwArgs, 1, "then"),
          getArgAndParse(args, kwArgs, 2, "else")
        )
      case Apply(Ident("while"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1), Array("cond", "body"))
        While(getArgAndParse(args, kwArgs, 0, "cond"), getArgAndParse(args, kwArgs, 1, "body"))
      case Apply(Ident("for"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1, 2), Array("name", "iter", "body"))
        ForEach(
          getIdentArg(args, kwArgs, 0, "name"),
          getArgAndParse(args, kwArgs, 1, "iter"),
          getArgAndParse(args, kwArgs, 2, "body")
        )
      case Apply(Ident("def-fun"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1, 2, 3), Array("name", "args", "ret", "body"))
        DefFun(
          getIdentArg(args, kwArgs, 0, "name"),
          getArgAndParse(args, kwArgs, 1, "args").asInstanceOf[LiteralList],
          getArgAndParse(args, kwArgs, 2, "ret"),
          getArgOptAndParse(args, kwArgs, 3, "body")
        )
      case Apply(Ident("def-val"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1), Array("name", "value"))
        DefVal(getIdentArg(args, kwArgs, 0, "name"), getArgAndParse(args, kwArgs, 1, "value"))
      case Apply(Ident("def-type"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1), Array("name", "value"))
        DefType(getIdentArg(args, kwArgs, 0, "name"), getArgAndParse(args, kwArgs, 1, "value"))
      case Apply(Ident("def-struct"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1), Array("name", "fields"))
        DefStruct(
          getIdentArg(args, kwArgs, 0, "name"),
          parse(getArgAndParse(args, kwArgs, 1, "fields")).asInstanceOf[LiteralList]
        )
      case Apply(Ident("assign"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1), Array("name", "value"))
        Assign(getIdentArg(args, kwArgs, 0, "name"), getArgAndParse(args, kwArgs, 1, "value"))
      case Apply(Ident("block"), args, kwArgs) =>
        // Block is special
        if (kwArgs.nonEmpty) {
          throw ParserException("Block does not support keyword arguments yet")
        }
        Block(args.map(parse))
      case Apply(Ident("field"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1), Array("name", "type"))
        Field(getIdentArg(args, kwArgs, 0, "name"), getArgAndParse(args, kwArgs, 1, "type"))
      case Apply(Ident("select"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1), Array("obj", "field"))
        Select(getArgAndParse(args, kwArgs, 0, "obj"), getIdentArg(args, kwArgs, 1, "field"))
      case Apply(Ident("list"), args, kwArgs) =>
        // List is special
        if (kwArgs.nonEmpty) {
          throw ParserException("List does not support keyword arguments yet")
        }
        LiteralList(args.map(parse))
      case Apply(Ident("fun"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1, 2), Array("params", "returns", "body"))
        val params = getArgAndParse(args, kwArgs, 0, "params")
        val returns = getArgAndParse(args, kwArgs, 1, "returns")
        val body = getArgAndParse(args, kwArgs, 2, "body")
        FunApply(params.asInstanceOf[LiteralList], returns, body)
      case TypeApply(fun, args, kwArgs) =>
        TypeApply(parse(fun), args.map(parse), kwArgs.map(kv => KeyValue(kv.name, parse(kv.value))))
      case Ident(name) => Ident(name)
      case x if isLiteral(x, ValueContext()) => x
      case Apply(fun, args, kwArgs) =>
        Apply(parse(fun), args.map(parse), kwArgs.map(kv => KeyValue(kv.name, parse(kv.value))))
      case LiteralList(items) => LiteralList(items.map(parse))
      case Field(name, ty) => Field(name, parse(ty))
      case _ => throw ParserException("Unhandled type: " + n)
    }
  }
}
