package shll.frontends

import shll.ast.*
import shll.frontends.ParamUtil.*
import shll.ast.AstHelper.*
import shll.backends.ShllPrettyPrinter

case class TypeCheckException(n: Ast, ty: Ast)
    extends Exception(
      "Type error: " + ShllPrettyPrinter.print(n) + " is not of type " + ShllPrettyPrinter.print(ty)
    )

case class TypeChecker() {

  def internalTypeCheck(n: Ast, ty: Ast): Unit = {
    if (ty == AstHelper.tAny) return
    if (ty == AstHelper.tIdent && !n.isInstanceOf[Ident]) throw TypeCheckException(n, ty)
    if (ty == AstHelper.tParams && !n.isInstanceOf[Params]) throw TypeCheckException(n, ty)
    if (ty == AstHelper.tFields && !n.isInstanceOf[Fields]) throw TypeCheckException(n, ty)

  }
  def typeCheckAndConvert(n: Ast): Ast = {
    n match {
      case Apply(Ident(name), args, kwArgs) if Builtins.builtinAsts.contains(name) =>
        val d: DeclFun = Builtins.builtinAsts(name)
        val collected =
          collectArguments(args, kwArgs, argsToRange(d.params), argsToKeys(d.params))
            .map(x => x._1 -> typeCheckAndConvert(x._2))

        for (p <- d.params.params) {
          internalTypeCheck(collected(p.name.name), p.ty)
        }

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
              collected("params").asInstanceOf[Params],
              collected("ret"),
              collected("body")
            )
          case "decl-fun" =>
            DeclFun(
              collected("name").asInstanceOf[Ident],
              collected("params").asInstanceOf[Params],
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
              collected("params").asInstanceOf[Params],
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
              collected("params").asInstanceOf[Params],
              collected("returns"),
              collected("body")
            )
        }
      case Apply(Ident("block"), args, kwArgs) =>
        // Block is special
        if (kwArgs.args.nonEmpty) {
          throw ParserException("Block does not support keyword arguments yet")
        }
        Block(args.args.map(typeCheckAndConvert))
      case Apply(Ident("list"), args, kwArgs) =>
        // List is special
        if (kwArgs.args.nonEmpty) {
          throw ParserException("List does not support keyword arguments yet")
        }
        LiteralList(args.args.map(typeCheckAndConvert))
      case Apply(Ident("lp"), args, kwArgs) =>
        // List is special
        if (kwArgs.args.nonEmpty) {
          throw ParserException("Parameters does not support keyword arguments yet")
        }
        Params(args.args.map(typeCheckAndConvert).map {
          case x: Field => Param(x.name, x.ty)
          case x: Param => x
        })
      case Apply(Ident("lf"), args, kwArgs) =>
        // List is special
        if (kwArgs.args.nonEmpty) {
          throw ParserException("Parameters does not support keyword arguments yet")
        }
        Fields(args.args.map(typeCheckAndConvert).map(_.asInstanceOf[Field]))
      case ApplyType(fun, args, kwArgs) =>
        ApplyType(
          typeCheckAndConvert(fun),
          typeCheckAndConvert(args).asInstanceOf,
          typeCheckAndConvert(kwArgs).asInstanceOf
        )
      case Ident(name) => Ident(name)
      case x if isLiteral(x, ValueContext()) => x
      case Apply(fun, args, kwArgs) =>
        Apply(
          typeCheckAndConvert(fun),
          typeCheckAndConvert(args).asInstanceOf,
          typeCheckAndConvert(kwArgs).asInstanceOf
        )
      case LiteralList(items) => LiteralList(items.map(typeCheckAndConvert))
      case Field(name, ty) => Field(name, typeCheckAndConvert(ty))
      case Param(name, ty) => Param(name, typeCheckAndConvert(ty))
      case PosArgs(args) => PosArgs(args.map(typeCheckAndConvert))
      case KwArgs(args) => KwArgs(args.map(kv => KwArg(kv.name, typeCheckAndConvert(kv.value))))
      case KwArg(name, value) => KwArg(name, typeCheckAndConvert(value))
      case Params(args) => Params(args.map(typeCheckAndConvert).map(_.asInstanceOf[Param]))
      case Fields(args) => Fields(args.map(typeCheckAndConvert).map(_.asInstanceOf[Field]))
      case _ => throw ParserException("Unhandled type: " + n)
    }
  }
}
