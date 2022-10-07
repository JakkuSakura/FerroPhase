package shll.frontends

import shll.ast.*
import shll.frontends.ParamUtil.*
import shll.ast.AstHelper.*

case class ApplyParser() {
  val decls: Map[String, DeclFun] = Map(
    "if" -> AstHelper.declFun(
      "if",
      List(("cond", AstHelper.tBool), ("then", AstHelper.tAny), ("else", AstHelper.tAny)),
      AstHelper.tAny
    ),
    "while" -> AstHelper.declFun(
      "while",
      List(("cond", AstHelper.tBool), ("body", AstHelper.tAny)),
      AstHelper.tUnit
    ),
    "for" -> AstHelper.declFun(
      "for",
      List(
        ("variable", AstHelper.tLiteral),
        ("iterable", AstHelper.tAny),
        ("body", AstHelper.tAny)
      ),
      AstHelper.tUnit
    ),
    "def-fun" -> AstHelper.declFun(
      "def-fun",
      List(
        ("name", AstHelper.tLiteral),
        ("params", AstHelper.tAny),
        ("ret", AstHelper.tAny),
        ("body", AstHelper.tAny)
      ),
      AstHelper.tUnit
    ),
    "decl-fun" -> AstHelper.declFun(
      "def-fun",
      List(
        ("name", AstHelper.tLiteral),
        ("params", AstHelper.tAny),
        ("ret", AstHelper.tAny)
      ),
      AstHelper.tUnit
    ),
    "def-val" -> AstHelper.declFun(
      "def-val",
      List(
        ("name", AstHelper.tLiteral),
        ("value", AstHelper.tAny)
      ),
      AstHelper.tUnit
    ),
    "def-type" -> AstHelper.declFun(
      "def-type",
      List(
        ("name", AstHelper.tLiteral),
        ("value", AstHelper.tAny)
      ),
      AstHelper.tUnit
    ),
    "def-struct" -> AstHelper.declFun(
      "def-struct",
      List(
        ("name", AstHelper.tLiteral),
        ("fields", AstHelper.tList(AstHelper.tAny)) // TODO: make it more explicit
      ),
      AstHelper.tUnit
    ),
    "assign" -> AstHelper.declFun(
      "assign",
      List(
        ("name", AstHelper.tLiteral),
        ("value", AstHelper.tAny)
      ),
      AstHelper.tUnit
    ),
    ":" -> AstHelper.declFun(
      ":",
      List(
        ("name", AstHelper.tLiteral),
        ("value", AstHelper.tAny)
      ),
      AstHelper.tAny
    ),
    "select" -> AstHelper.declFun(
      "select",
      List(
        ("obj", AstHelper.tAny),
        ("field", AstHelper.tLiteral)
      ),
      AstHelper.tAny
    ),
    "fun" -> AstHelper.declFun(
      "fun",
      List(
        ("params", AstHelper.tAny),
        ("returns", AstHelper.tAny),
        ("body", AstHelper.tAny)
      ),
      AstHelper.tAny
    )
    // block is special
    //    "block" -> AstHelper.defFun(
    //      "block",
    //      List(
    //        ("body", AstHelper.tAny)
    //      ),
    //      AstHelper.tUnit
    //    ),
    // list is special, lp, lf
    //    "list" -> AstHelper.defFun(
    //      "list",
    //      List(
    //        ("body", AstHelper.tAny)
    //      ),
    //      AstHelper.tList(AstHelper.tAny)
    //    ),
  )

  def parse(n: AST): AST = {
    n match {
      case Apply(Ident(name), args, kwArgs) if decls.contains(name) =>
        val d: DeclFun = decls(name)
        val collected =
          collectArguments(args, kwArgs, argsToRange(d.params), argsToKeys(d.params))
            .map(x => x._1 -> parse(x._2))
        name match {
          case "if" =>
            Cond(
              collected("cond"),
              collected("then"),
              collected("else")
            )
          case "while" =>
            While(
              collected("cond"),
              collected("body")
            )

          case "for" =>
            ForEach(
              collected("variable").asInstanceOf[Ident],
              collected("iterable"),
              collected("body")
            )
          case "def-fun" =>
            DefFun(
              collected("name").asInstanceOf[Ident],
              collected("params").asInstanceOf[Parameters],
              collected("ret"),
              collected("body")
            )
          case "decl-fun" =>
            DeclFun(
              collected("name").asInstanceOf[Ident],
              collected("params").asInstanceOf[Parameters],
              collected("ret")
            )
          case "def-val" =>
            DefVal(
              collected("name").asInstanceOf[Ident],
              collected("value")
            )
          case "def-type" =>
            DefType(
              collected("name").asInstanceOf[Ident],
              collected("value")
            )
          case "def-struct" =>
            DefStruct(
              collected("name").asInstanceOf[Ident],
              collected("fields").asInstanceOf[Fields]
            )

          case "assign" =>
            Assign(
              collected("name").asInstanceOf[Ident],
              collected("value")
            )
          case ":" =>
            Field(
              collected("name").asInstanceOf[Ident],
              collected("value")
            )
          case "select" =>
            Select(
              collected("obj"),
              collected("field").asInstanceOf[Ident]
            )
          case "fun" =>
            ApplyFun(
              collected("params").asInstanceOf[Parameters],
              collected("returns"),
              collected("body")
            )
        }
      case Apply(Ident("block"), args, kwArgs) =>
        // Block is special
        if (kwArgs.args.nonEmpty) {
          throw ParserException("Block does not support keyword arguments yet")
        }
        Block(args.args.map(parse))
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
