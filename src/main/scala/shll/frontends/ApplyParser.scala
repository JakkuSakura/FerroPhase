package shll.frontends

import shll.ast.*
import shll.frontends.ParamUtil.*
import shll.ast.AstTool.*

case class ApplyParser() {
  def getArgAndParse(args: PosArgs, kwArgs: KwArgs, i: Int, name: String): AST = parse(
    ParamUtil.getArg(args, kwArgs, i, name)
  )
  def getArgOptAndParse(
      args: PosArgs,
      kwArgs: KwArgs,
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
          Block(getArgAndParse(args, kwArgs, 2, "body"))
        )
      case Apply(Ident("def-fun"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1, 2, 3), Array("name", "args", "ret", "body"))
        DefFun(
          getIdentArg(args, kwArgs, 0, "name"),
          getArgAndParse(args, kwArgs, 1, "args").asInstanceOf,
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
          parse(getArgAndParse(args, kwArgs, 1, "fields")).asInstanceOf
        )
      case Apply(Ident("assign"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1), Array("name", "value"))
        Assign(getIdentArg(args, kwArgs, 0, "name"), getArgAndParse(args, kwArgs, 1, "value"))
      case Apply(Ident("block"), args, kwArgs) =>
        // Block is special
        if (kwArgs.args.nonEmpty) {
          throw ParserException("Block does not support keyword arguments yet")
        }
        Block(args.args.map(parse))
      case Apply(Ident(":"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1), Array("name", "type"))
        Field(getIdentArg(args, kwArgs, 0, "name"), getArgAndParse(args, kwArgs, 1, "type"))
      case Apply(Ident("select"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1), Array("obj", "field"))
        Select(getArgAndParse(args, kwArgs, 0, "obj"), getIdentArg(args, kwArgs, 1, "field"))
      case Apply(Ident("list"), args, kwArgs) =>
        // List is special
        if (kwArgs.args.nonEmpty) {
          throw ParserException("List does not support keyword arguments yet")
        }
        LiteralList(args.args.map(parse))
      case Apply(Ident("lp"), args, kwArgs) =>
        // List is special
        if (kwArgs.args.nonEmpty) {
          throw ParserException("Parameters does not support keyword arguments yet")
        }
        Parameters(args.args.map(parse).map(_.asInstanceOf[Field]))
      case Apply(Ident("lf"), args, kwArgs) =>
        // List is special
        if (kwArgs.args.nonEmpty) {
          throw ParserException("Parameters does not support keyword arguments yet")
        }
        Fields(args.args.map(parse).map(_.asInstanceOf[Field]))
      case Apply(Ident("fun"), args, kwArgs) =>
        checkArguments(args, kwArgs, Array(0, 1, 2), Array("params", "returns", "body"))
        val params = getArgAndParse(args, kwArgs, 0, "params")
        val returns = getArgAndParse(args, kwArgs, 1, "returns")
        val body = getArgAndParse(args, kwArgs, 2, "body")
        ApplyFun(params.asInstanceOf, returns, body)
      case ApplyType(fun, args, kwArgs) =>
        ApplyType(parse(fun), parse(args).asInstanceOf, parse(kwArgs).asInstanceOf)
      case Ident(name) => Ident(name)
      case x if isLiteral(x, ValueContext()) => x
      case Apply(fun, args, kwArgs) =>
        Apply(parse(fun), parse(args).asInstanceOf, parse(kwArgs).asInstanceOf)
      case LiteralList(items) => LiteralList(items.map(parse))
      case Field(name, ty) => Field(name, parse(ty))
      case PosArgs(args) => PosArgs(args.map(parse))
      case KwArgs(args) => KwArgs(args.map(kv => KeyValue(kv.name, parse(kv.value))))
      case KeyValue(name, value) => KeyValue(name, parse(value))
      case Parameters(args) => Parameters(args.map(parse).map(_.asInstanceOf[Field]))
      case Fields(args) => Fields(args.map(parse).map(_.asInstanceOf[Field]))
      case _ => throw ParserException("Unhandled type: " + n)
    }
  }
}
