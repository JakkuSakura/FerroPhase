package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.*

import scala.collection.mutable
case class SpecializeException(msg: String, node: AST) extends Exception(msg + ": " + node)
case class ValueContext(
    values: Map[String, AST] = Map.empty,
    tyValues: Map[String, AST] = Map.empty,
    parent: Option[ValueContext] = None
) {
  def getValue(name: String): Option[AST] = {
    values.get(name).orElse(parent.flatMap(_.getValue(name)))
  }

  def getTyValue(name: String): Option[AST] = {
    tyValues.get(name).orElse(parent.flatMap(_.getTyValue(name)))
  }
}
object ValueContext {
  def empty: ValueContext = ValueContext()
  def from(
      parent: ValueContext,
      values: Map[String, AST] = Map.empty,
      tyValues: Map[String, AST] = Map.empty
  ): ValueContext = {
    ValueContext(values, tyValues, Some(parent))

  }
}
case class SpecializeCache(
    funcDeclMap: mutable.HashMap[String, DefFun] = mutable.HashMap.empty,
    clsDeclMap: mutable.HashMap[String, Nothing] = mutable.HashMap.empty,
    specializedFunctions: mutable.HashMap[String, DefFun] = mutable.HashMap.empty,
    specializeId: mutable.HashMap[String, Int] = mutable.HashMap.empty
) {
  def getAndIncrSpecializeId(name: String): Int = {
    specializeId.get(name) match {
      case Some(id) =>
        val newId = id + 1
        specializeId += (name -> newId)
        newId
      case None =>
        val newId = 0
        specializeId += (name -> newId)
        newId
    }
  }
  def allocateSpecializedIdent(name: String): Ident = {
    Ident(s"${name}_${getAndIncrSpecializeId(name)}")
  }
}

case class Specializer() {
  var logger: Logger = Logger[this.type]
  var cache: SpecializeCache = SpecializeCache()
  def specialize(n: AST): AST = {
    cache = SpecializeCache()
    val v = specializeNode(n, ValueContext())
    val specialized = cache.specializedFunctions.values.toList
    if (specialized.isEmpty) {
      v
    } else
      v match {
        case decls: Block =>
          Block(specialized ::: decls.body)
        case _ => throw SpecializeException("cannot specialize", v)
      }
  }

  def specializeNode(n: AST, ctx: ValueContext): AST = {
    logger.debug("Specializing " + n)
    n match {
      case d: DefFun => specializeDefFun(d, ctx)
      case n: Block => specializeBlock(n, ctx)
//      case ds: AstDecls => AstDeclsImpl(ds.decls.map(specializeDecl(_, ctx)))
      case n: Apply => specializeApply(n, ctx)
      case n: Ident => specializeIdent(n, ctx)
      case n: LiteralInt => n
      case n: LiteralDecimal => n
      case n: LiteralString => n
      case n: LiteralList => LiteralList(n.value.map(specializeNode(_, ctx)))
      case n: Field => specializeField(n, ctx)
      case x => throw SpecializeException("cannot specialize", x)
    }

  }

  def specializeKeyValue(kv: KeyValue, ctx: ValueContext): KeyValue = {
    KeyValue(kv.name, specializeNode(kv.value, ctx))
  }
  def specializeField(n: Field, ctx: ValueContext): Field = {
    val value = specializeNode(n.ty, ctx)
    Field(n.name, value)
  }
  def specializeDefVal(n: DefVal, ctx: ValueContext): DefVal = {
    val value = specializeNode(n.value, ctx)
    DefVal(n.name, value)
  }
  def specializeIdent(id: Ident, ctx: ValueContext): AST = {
    ctx.getValue(id.name).getOrElse(id)
  }

  def specializeApply(n: Apply, ctx: ValueContext): AST = {
    n.fun match {
      case id: Ident if cache.funcDeclMap.contains(id.name) =>
        val func = cache.funcDeclMap(id.name)
        specializeFunctionApply(func, n.args, n.kwArgs, ctx)
      case _ =>
        val f = specializeNode(n.fun, ctx)
        Apply(
          f,
          n.args.map(specializeNode(_, ctx)),
          n.kwArgs.map(specializeKeyValue(_, ctx))
        )
    }
  }
  def specializeBlock(d: Block, ctx: ValueContext): Block = {
    var ctx1 = ctx
    val stmts = d.body.map {
      case s: DefVal =>
        val x = specializeDefVal(s, ctx1)
        ctx1 = ValueContext.from(ctx1, Map(s.name.name -> s.value))
        x
      case s =>
        specializeNode(s, ctx1)
    }
    Block(stmts)
  }

  def specializeDecl(d: AST, ctx: ValueContext): AST = {
    logger.debug("Specializing decl " + d)
    d match {
      case d: DefFun => specializeDefFun(d, ctx)
//      case c: AstClassDecl => specializeClassDecl(c, ctx)
      case _ => throw SpecializeException("cannot specialize ", d)
    }
  }
//  def specializeClassDecl(c: AstClassDecl, ctx: ValueContext): AstClassDecl = {
//    cache.clsDeclMap(c.name) = c
//    c
//  }
  def isSpecializedFunctionDecl(d: DefFun): Boolean = {
    d.args match {
      case LiteralList(value) => value.isEmpty
//      case _ => false
    }
  }
  def isConstant(n: AST): Boolean = {
    n match {
      case _: LiteralInt => true
      case _: LiteralDecimal => true
      case _: LiteralString => true
      case _ => false
    }
  }

  def prepareCtx(
      ctx: ValueContext,
      d: Map[String, AST],
      oldBody: AST
  ): (AST, ValueContext) = {
    val newCtx = ValueContext.from(
      ctx,
      d.map {
        case k -> v if isConstant(v) => k -> v
        case k -> v => k -> v
      },
      Map.empty
    )
    val prepareValues = d.flatMap {
      case k -> v if !isConstant(v) =>
        Some(
          DefVal(Ident(k), v)
        )
      case _ => None
    }.toList
    val newBody = if (prepareValues.nonEmpty) {
      oldBody match {
        case b: Block =>
          Block(
            prepareValues ::: b.body
          )
        case _ =>
          Block(
            prepareValues :+ oldBody
          )
      }
    } else {
      oldBody
    }
    (newBody, newCtx)
  }

  def specializeFunctionApply(
      func: DefFun,
      args: List[AST],
      kwArgs: List[KeyValue],
      ctx: ValueContext
  ): Apply = {
    // TODO: process args
    val mapping = kwArgs.map { a =>
      a.name.name -> specializeNode(a, ctx)
    }.toMap
    val (newBody, newCtx) = prepareCtx(ctx, mapping, func.body)
    val body = specializeNode(newBody, newCtx)

    val newFunc = func
      .copy(
        name = cache.allocateSpecializedIdent(func.name.name),
        body = body,
        args = LiteralList(Nil),
        ret = func.ret
      )
    cache.specializedFunctions(newFunc.name.name) = newFunc
    Apply(newFunc.name, Nil, Nil)
  }

  def specializeDefFun(
      d: DefFun,
      ctx: ValueContext
  ): DefFun = {
    cache.funcDeclMap(d.name.name) = d
    if (isSpecializedFunctionDecl(d)) {
      // TODO evaluate constants
      val body = specializeNode(d.body, ctx)
      d.copy(body = body)
    } else {
      d
    }
  }
}
