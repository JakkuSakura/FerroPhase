package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.*
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ParamUtil.*

import scala.collection.mutable

case class DeadCodeEliminatorContext(
    used: mutable.Set[AST] = mutable.Set.empty,
    context: ValueContext = ValueContext()
) {
  def withValues(values: Map[String, AST]): DeadCodeEliminatorContext = copy(context = context.withValues(values))
  def withStructs(structs: Map[String, DefStruct]): DeadCodeEliminatorContext = copy(context = context.withStructs(structs))
  def withFunctions(functions: Map[String, DefFun]): DeadCodeEliminatorContext = copy(context = context.withFunctions(functions))
}
case class DeadCodeEliminator() {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter(newlines = false)

  def eliminate(n: AST): AST = {
    eliminateNode(n, DeadCodeEliminatorContext())
  }

  def eliminateNode(n: AST, ctx: DeadCodeEliminatorContext): AST = {
    logger.debug("Eliminating " + pp.print(n))

    n match {
      case n: Block => eliminateBlock(n, ctx)
      case n: Apply => eliminateApply(n, ctx)
      case n: Ident => eliminateIdent(n, ctx)
      case n: LiteralInt => n
      case n: LiteralDecimal => n
      case n: LiteralString => n
      case n: LiteralBool => n
      case n: LiteralList => LiteralList(n.value.map(eliminateNode(_, ctx)))
      case n: Field => eliminateField(n, ctx)
      case n: Select => eliminateSelect(n, ctx)
      case n: Cond => eliminateCond(n, ctx)
      case n: ForEach => eliminateForEach(n, ctx)
      case n: TypeApply => eliminateTypeApply(n, ctx)
      case n: DefType => eliminateDefType(n, ctx)
      case x => throw SpecializeException("cannot eliminate", x)
    }

  }

  def eliminateKeyValue(kv: KeyValue, ctx: DeadCodeEliminatorContext): KeyValue = {
    KeyValue(kv.name, eliminateNode(kv.value, ctx))
  }
  def eliminateField(n: Field, ctx: DeadCodeEliminatorContext): Field = {
    val value = eliminateNode(n.ty, ctx)
    Field(n.name, value)
  }
  def eliminateDefVal(
      n: DefVal,
      ctx: DeadCodeEliminatorContext
  ): (DefVal, DeadCodeEliminatorContext) = {
    val value = eliminateNode(n.value, ctx)
    val newCtx = ctx.withValues(
      Map(
        n.name.name -> value
      )
    )
    (DefVal(n.name, value), newCtx)
  }
  def eliminateIdent(id: Ident, ctx: DeadCodeEliminatorContext): AST = {
    ctx.context.getValue(id.name).getOrElse(id)
  }

  def eliminateApply(n: Apply, ctx: DeadCodeEliminatorContext): AST = {
    ctx.used += n
    ctx.used += n.fun
    ctx.used ++= n.args
    ctx.used ++= n.kwArgs.map(_.value)

    eliminateNode(n.fun, ctx)
    n.args.map(eliminateNode(_, ctx))
    n.kwArgs.map(_.value).map(eliminateNode(_, ctx))
    n
  }

  def eliminateTypeApply(n: TypeApply, ctx: DeadCodeEliminatorContext): AST = {
    ctx.used += n
    ctx.used += n.fun
    ctx.used ++= n.args
    ctx.used ++= n.kwArgs.map(_.value)

    eliminateNode(n.fun, ctx)
    n.args.map(eliminateNode(_, ctx))
    n.kwArgs.map(_.value).map(eliminateNode(_, ctx))
    n
  }

  def eliminateDefType(n: DefType, context: DeadCodeEliminatorContext): AST = {
    context.used += n
    context.used += n.value
    n
  }

  def eliminateBlock(d: Block, ctx0: DeadCodeEliminatorContext): AST = {
    var ctx = ctx0
    val stmts = d.body.map {
      case s: DefVal =>
        val (x, newCtx) = eliminateDefVal(s, ctx)
        ctx = newCtx
        x
      case s: Assign =>
        val (x, newCtx) = eliminateDefVal(DefVal(s.name, s.value), ctx)
        ctx = newCtx
        Assign(x.name, x.value)
      case d: DefFun =>
        val (x, newCtx) = eliminateDefFun(d, ctx)
        ctx = newCtx
        x
      case n: DefStruct =>
        val (x, newCtx) = eliminateDefStruct(n, ctx)
        ctx = newCtx
        x
      case s =>
        eliminateNode(s, ctx)
    }
    ctx.used ++= stmts.lastOption
    val filteredStmts = stmts.filter(ctx.used.contains)
    if (filteredStmts.length > 1)
      Block(filteredStmts)
    else
      filteredStmts.head

  }

  def eliminateDefStruct(
      c: DefStruct,
      ctx: DeadCodeEliminatorContext
  ): (DefStruct, DeadCodeEliminatorContext) = {
    (c, ctx.withStructs(Map(c.name.name -> c)))
  }

  def isConstant(n: AST): Boolean = {
    n match {
      case _: LiteralInt => true
      case _: LiteralBool => true
      case _: LiteralDecimal => true
      case _: LiteralChar => true
      case _: LiteralString => true
      case x: LiteralList => x.value.map(isConstant).forall(identity)
      case _ => false
    }
  }

  def prepareCtx(
      ctx: DeadCodeEliminatorContext,
      d: Map[String, AST],
      oldBody: AST
  ): (AST, DeadCodeEliminatorContext) = {
    val newCtx = ctx.withValues(
      d.map {
        case k -> v if isConstant(v) => k -> v
        case k -> v => k -> v
      }
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

  def argsToRange(
      args: LiteralList
  ): Array[Int] = {
    args.value.indices.toArray
  }
  def argsToKeys(
      args: LiteralList
  ): Array[String] = {
    args.value.map {
      case a: Field => a.name.name
      case a => throw SpecializeException("cannot convert to keys", a)
    }.toArray
  }

  def eliminateDefFun(
      d: DefFun,
      ctx: DeadCodeEliminatorContext
  ): (DefFun, DeadCodeEliminatorContext) = {
    val newCtx = ctx.withFunctions( Map(d.name.name -> d))
    (d, newCtx)
  }

  def eliminateSelect(n: Select, ctx: DeadCodeEliminatorContext): AST = {
    val obj = eliminateNode(n.obj, ctx)
    obj match {
      case DefStruct(name, fields, values) =>
        values.find(_.name.name == n.field.name) match {
          case Some(v) => v.value
          case None => throw SpecializeException("field not found", n)
        }
      case o => o
    }
  }
  def eliminateCond(n: Cond, ctx: DeadCodeEliminatorContext): AST = {
    val cond = eliminateNode(n.cond, ctx)
    cond match {
      case LiteralBool(true) => eliminateNode(n.consequence, ctx)
      case LiteralBool(false) => eliminateNode(n.alternative, ctx)
      case _ =>
        Cond(
          cond,
          eliminateNode(n.consequence, ctx),
          eliminateNode(n.alternative, ctx)
        )
    }
  }
  def isFinite(n: AST): Boolean = {
    n match {
      case x: LiteralList => true
      case _ => false
    }
  }
  def eliminateForEach(n: ForEach, ctx: DeadCodeEliminatorContext): AST = {
    val iterable = eliminateNode(n.iterable, ctx)
    if (isFinite(iterable)) {
      Block(
        iterable match {
          case LiteralList(value) =>
            value.map { v =>
              val ctx1 = ctx.withValues(Map(n.variable.name -> v))
              eliminateNode(n.body, ctx1)
            }
          case _ => throw SpecializeException("cannot eliminate: not finite", n)
        }
      )
    } else {
      n.copy(
        iterable = iterable,
        body = eliminateNode(n.body, ctx)
      )
    }
  }

}
