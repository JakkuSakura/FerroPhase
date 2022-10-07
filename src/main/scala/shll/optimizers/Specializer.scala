package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.*
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ParamUtil.*
import AstHelper.*

import scala.collection.mutable
case class SpecializeException(msg: String, node: AST)
    extends Exception(msg + ": " + ShllPrettyPrinter.print(node))

case class SpecializeContext(
    cache: Option[SpecializeCache] = None,
    context: ValueContext = ValueContext(),
    parent: Option[SpecializeContext] = None
) {
  def getCache: SpecializeCache = cache.getOrElse(parent.get.getCache)
  def withCache(cache: SpecializeCache): SpecializeContext =
    SpecializeContext(cache = Some(cache), context = context, parent = Some(this))
  def withValues(values: Map[String, AST]): SpecializeContext = {
    SpecializeContext(context = context.withValues(values), parent = Some(this))
  }

  def withValue(name: String, value: AST): SpecializeContext = {
    SpecializeContext(context = context.withValue(name, value), parent = Some(this))
  }

  def withTypes(values: Map[String, DefType]): SpecializeContext = {
    SpecializeContext(context = context.withTypes(values), parent = Some(this))
  }

  def withType(name: String, ty: DefType): SpecializeContext = {
    SpecializeContext(context = context.withType(name, ty), parent = Some(this))
  }
  def withStruct(name: String, struct: DefStruct): SpecializeContext = {
    SpecializeContext(context = context.withStruct(name, struct), parent = Some(this))
  }
  def withFunction(name: String, func: DefFun): SpecializeContext =
    SpecializeContext(context = context.withFunction(name, func), parent = Some(this))
}

case class SpecializeCache(
    specializedStructs: mutable.HashMap[String, DefStruct] = mutable.HashMap.empty,
    specializedTypes: mutable.HashMap[String, DefType] = mutable.HashMap.empty,
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

case class Specializer(
    inlineFunctionApply: Boolean = true,
    inlineVariable: Boolean = true
) {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter(newlines = false)

  val builtinFunctions: Map[String, (Apply, SpecializeContext) => AST] = Map(
    "==" -> binaryOperator { (apply, lhs, rhs, ctx) =>
      if (lhs == rhs)
        LiteralBool(true)
      else if (isLiteral(lhs, ctx) && isLiteral(rhs, ctx))
        LiteralBool(false)
      else
        apply
    },
    "!=" -> binaryOperator { (apply, lhs, rhs, ctx) =>
      if (lhs == rhs)
        LiteralBool(false)
      else if (isLiteral(lhs, ctx) && isLiteral(rhs, ctx))
        LiteralBool(true)
      else
        apply
    },
    ">" -> binaryOperator { (apply, lhs, rhs, ctx) =>
      (lhs, rhs) match {
        case (LiteralInt(l), LiteralInt(r)) => LiteralBool(l > r)
        case (LiteralDecimal(l), LiteralDecimal(r)) => LiteralBool(l > r)
        case _ => apply
      }
    },
    ">=" -> binaryOperator { (apply, lhs, rhs, ctx) =>
      (lhs, rhs) match {
        case (LiteralInt(l), LiteralInt(r)) => LiteralBool(l >= r)
        case (LiteralDecimal(l), LiteralDecimal(r)) => LiteralBool(l >= r)
        case _ => apply
      }
    },
    "<" -> binaryOperator { (apply, lhs, rhs, ctx) =>
      (lhs, rhs) match {
        case (LiteralInt(l), LiteralInt(r)) => LiteralBool(l < r)
        case (LiteralDecimal(l), LiteralDecimal(r)) => LiteralBool(l < r)
        case _ => apply
      }
    },
    "<=" -> binaryOperator { (apply, lhs, rhs, ctx) =>
      (lhs, rhs) match {
        case (LiteralInt(l), LiteralInt(r)) => LiteralBool(l <= r)
        case (LiteralDecimal(l), LiteralDecimal(r)) => LiteralBool(l <= r)
        case _ => apply
      }
    },
    "+" -> binaryOperator { (apply, lhs, rhs, ctx) =>
      (lhs, rhs) match {
        case (LiteralInt(l), LiteralInt(r)) => LiteralInt(l + r)
        case (LiteralDecimal(l), LiteralDecimal(r)) => LiteralDecimal(l + r)
        case (LiteralString(l), LiteralString(r)) => LiteralString(l + r)
        case _ => apply
      }
    },
    "-" -> binaryOperator { (apply, lhs, rhs, ctx) =>
      (lhs, rhs) match {
        case (LiteralInt(l), LiteralInt(r)) => LiteralInt(l - r)
        case (LiteralDecimal(l), LiteralDecimal(r)) => LiteralDecimal(l - r)
        case _ => apply
      }
    },
    "*" -> binaryOperator { (apply, lhs, rhs, ctx) =>
      (lhs, rhs) match {
        case (LiteralInt(l), LiteralInt(r)) => LiteralInt(l * r)
        case (LiteralDecimal(l), LiteralDecimal(r)) => LiteralDecimal(l * r)
        case _ => apply
      }
    },
    "/" -> binaryOperator { (apply, lhs, rhs, ctx) =>
      (lhs, rhs) match {
        case (LiteralInt(l), LiteralInt(r)) => LiteralInt(l / r)
        case (LiteralDecimal(l), LiteralDecimal(r)) => LiteralDecimal(l / r)
        case _ => apply
      }
    },
    "%" -> binaryOperator { (apply, lhs, rhs, ctx) =>
      (lhs, rhs) match {
        case (LiteralInt(l), LiteralInt(r)) => LiteralInt(l % r)
        case (LiteralDecimal(l), LiteralDecimal(r)) => LiteralDecimal(l % r)
        case _ => apply
      }
    },
    "range" -> binaryOperator { (apply, lhs, rhs, ctx) =>
      apply
    }
  )
  def binaryOperator(
      fn: (apply: AST, lhs: AST, rhs: AST, ctx: ValueContext) => AST
  ): (Apply, SpecializeContext) => AST = { (apply, ctx) =>
    {
      checkArguments(apply.args, apply.kwArgs, Array(0, 1), Array("lhs", "rhs"))
      val lhs = specializeNode(getArg(apply.args, apply.kwArgs, 0, "lhs"), ctx)
      val rhs = specializeNode(getArg(apply.args, apply.kwArgs, 1, "rhs"), ctx)
      val a = Apply(apply.fun, PosArgs(List(lhs, rhs)), KwArgs(Nil))
      fn(a, lhs, rhs, ctx.context)
    }
  }
  val builtinTypes: Map[String, (ApplyType, SpecializeContext) => AST] = Map(
    "int" -> simpleType,
    "bool" -> simpleType,
    "numeric" -> simpleType,
    "string" -> simpleType,
    "char" -> simpleType,
    "list" -> simpleGenericType,
    "fun" -> funcType
  )

  def getTypeName(n: AST, ctx: SpecializeContext): String = {
    n match {
      case Ident(name) =>
        ctx.context
          .getType(name)
          .map(getTypeName(_, ctx))
          .getOrElse(builtinTypes.keys.filter(_ == name).head)
      case ApplyType(Ident(name), args, kwArgs) => getTypeName(Ident(name), ctx)
      case _ => throw SpecializeException("Unknown type name", n)
    }
  }
  def isKnownType(n: AST, ctx: SpecializeContext): Boolean = {
    n match {
      case Ident(name) => ctx.context.getType(name).isDefined || builtinTypes.contains(name)
      case ApplyType(Ident(name), args, kwArgs) =>
        isKnownType(Ident(name), ctx) && args.args.forall(isKnownType(_, ctx)) && kwArgs.args
          .forall(x => isKnownType(x.value, ctx))
      case _ => false
    }
  }
  def simpleGenericType: (ApplyType, SpecializeContext) => AST = { (apply, ctx) =>
    checkArguments(apply.args, apply.kwArgs, Array(0), Array("value"))
    val value = specializeNode(getArg(apply.args, apply.kwArgs, 0, "value"), ctx)
    val newApply = ApplyType(apply.fun, PosArgs(List(value)), KwArgs(Nil))
    if (
      isKnownType(apply.fun, ctx) && isKnownType(
        value,
        ctx
      ) && ctx.cache.isDefined
    ) {
      val newName = ctx.getCache.allocateSpecializedIdent(getTypeName(apply.fun, ctx))

      ctx.getCache.specializedTypes += newName.name -> DefType(newName, Parameters(Nil), newApply)
      ApplyType(newName, PosArgs(Nil), KwArgs(Nil))
    } else {
      newApply
    }
  }

  def funcType: (ApplyType, SpecializeContext) => AST = { (apply, ctx) =>
    checkArguments(apply.args, apply.kwArgs, Array(0, 1), Array("params", "return"))
    val params = specializeNode(getArg(apply.args, apply.kwArgs, 0, "params"), ctx)
    val returns = specializeNode(getArg(apply.args, apply.kwArgs, 0, "params"), ctx)

    val newApply = ApplyType(apply.fun, PosArgs(List(params, returns)), KwArgs(Nil))
    newApply
  }
  def simpleType: (ApplyType, SpecializeContext) => AST = { (apply, ctx) =>
    apply
  }
  def specialize(n: AST): AST = {
    specializeNode(n, SpecializeContext())

  }

  def specializeNode(n: AST, ctx: SpecializeContext): AST = {
//    logger.debug("Specializing " + pp.print(n))
    n match {
      case n: Block => specializeBlock(n, ctx)
      case n: Apply => specializeApply(n, ctx)
      case n: Ident => specializeIdent(n, ctx)
      case n: LiteralInt => n
      case n: LiteralDecimal => n
      case n: LiteralString => n
      case n: LiteralBool => n
      case n: LiteralList => LiteralList(n.value.map(specializeNode(_, ctx)))
      case n: Field => specializeField(n, ctx)
      case n: Select => specializeSelect(n, ctx)
      case n: Cond => specializeCond(n, ctx)
      case n: ForEach => specializeForEach(n, ctx)
      case n: ApplyType => specializeTypeApply(n, ctx)
      case n: Assign => specializeAssign(n, ctx)._1
      case n: ApplyFun => specializeFunApply(n, ctx)
      case n: KwArgs => KwArgs(n.args.map(x => specializeNode(x.value, ctx).asInstanceOf[KeyValue]))
      case n: PosArgs => PosArgs(n.args.map(specializeNode(_, ctx)))
      case n: Parameters => Parameters(n.params.map(specializeNode(_, ctx).asInstanceOf[Field]))
      case x => throw SpecializeException("cannot specialize", x)
    }

  }

  def specializeKeyValue(kv: KeyValue, ctx: SpecializeContext): KeyValue = {
    KeyValue(kv.name, specializeNode(kv.value, ctx))
  }
  def specializeField(n: Field, ctx: SpecializeContext): Field = {
    val value = specializeNode(n.ty, ctx)
    Field(n.name, value)
  }
  def specializeDefVal(n: DefVal, ctx: SpecializeContext): (DefVal, SpecializeContext) = {
    val value = specializeNode(n.value, ctx)
    val newCtx = ctx.withValue(n.name.name, value)
    (DefVal(n.name, value), newCtx)
  }
  def specializeIdent(id: Ident, ctx: SpecializeContext): AST = {
    if (inlineVariable) {
      ctx.context.getValue(id.name) match {
        case Some(value) if isLiteral(value, ctx.context) => value.duplicate()
        case _ => id
      }
    } else {
      id
    }
  }

  def specializeApply(n: Apply, ctx: SpecializeContext): AST = {
    n.fun match {
      case Ident(name) if builtinFunctions.contains(name) =>
        val fn = builtinFunctions(name)
        fn(n, ctx)
      case Ident(name) if ctx.context.getFunction(name).isDefined =>
        val func = ctx.context.getFunction(name).get
        specializeFunctionApply(func, n.args, n.kwArgs, ctx)

      case Ident(name) if ctx.context.getStruct(name).isDefined =>
        val struct = ctx.context.getStruct(name).get
        specializeStructApply(struct, n.args, n.kwArgs, ctx)
      case _ =>
        val f = specializeNode(n.fun, ctx)
        Apply(
          f,
          specializeNode(n.args, ctx).asInstanceOf,
          specializeNode(n.kwArgs, ctx).asInstanceOf
        )
    }
  }

  def specializeTypeApply(n: ApplyType, ctx: SpecializeContext): AST = {
    n.fun match {
      case Ident(name) if builtinTypes.contains(name) =>
        val ty = builtinTypes(name)
        ty(n, ctx)
      case Ident(name) if ctx.context.getType(name).isDefined =>
        val ty = ctx.context.getType(name).get
        ty
      case _ =>
        throw SpecializeException("Unknown type", n)
    }
  }
  def specializeBlock(d: Block, ctx0: SpecializeContext): Block = {
    val cache = SpecializeCache()
    var ctx = ctx0.withCache(cache)
    val stmts = d.children.map {
      case s: DefVal =>
        val (x, newCtx) = specializeDefVal(s, ctx)
        ctx = newCtx
        x
      case s: Assign =>
        val (x, newCtx) = specializeAssign(s, ctx)
        ctx = newCtx
        x
      case d: DefFun =>
        val (x, newCtx) = specializeDefFun(d, ctx)
        ctx = newCtx
        x
      case n: DefStruct =>
        val (x, newCtx) = specializeDefStruct(n, ctx)
        ctx = newCtx
        x
      case n: DefType =>
        val (x, newCtx) = specializeDefType(n, ctx)
        ctx = newCtx
        x

      case s =>
        specializeNode(s, ctx)
    }

    Block(
      cache.specializedFunctions.values.toList ::: cache.specializedTypes.values.toList ::: stmts
    )
  }

  def specializeDefStruct(c: DefStruct, ctx: SpecializeContext): (DefStruct, SpecializeContext) = {
    (c, ctx.withStruct(c.name.name, c))
  }

  def prepareBody(
      ctx: SpecializeContext,
      values: Map[String, AST],
      oldBody: AST
  ): AST = {
    val prepareValues = values.map { case k -> v =>
      DefVal(Ident(k), v.duplicate())
    }.toList
    val newBody = if (prepareValues.nonEmpty) {
      oldBody match {
        case b: Block =>
          Block(
            prepareValues ::: b.children
          )
        case _ =>
          Block(
            prepareValues :+ oldBody
          )
      }
    } else {
      oldBody
    }
    specializeNode(newBody, ctx)
  }

  def specializeFunctionApply(
      func: DefFun,
      args: PosArgs,
      kwArgs: KwArgs,
      ctx: SpecializeContext
  ): AST = {
    val mapping =
      collectArguments(args, kwArgs, argsToRange(func.params), argsToKeys(func.params))
        .map { case k -> v =>
          k -> specializeNode(v, ctx)
        }
    val body = prepareBody(
      ctx,
      mapping,
      func.body
    )
    body match {
      case x if isLiteral(x, ctx.context) => body
      case _ if inlineFunctionApply => body
      case _ =>
        val newFunc = DefFun(
          name = ctx.getCache.allocateSpecializedIdent(func.name.name),
          params = Parameters(Nil),
          ret = func.ret,
          body = body
        )
        ctx.getCache.specializedFunctions(newFunc.name.name) = newFunc
        Apply(newFunc.name, PosArgs(Nil), KwArgs(Nil))
    }
  }

  def specializeDefFun(
      d: DefFun,
      ctx: SpecializeContext
  ): (DefFun, SpecializeContext) = {
    val newCtx = ctx.withFunction(d.name.name, d)
    val body = specializeNode(d.body, ctx)
    (d.copy(body = body), newCtx)

  }
  def specializeStructApply(
      n: DefStruct,
      args: PosArgs,
      kwArgs: KwArgs,
      ctx: SpecializeContext
  ): ApplyStruct = {
    val mapping =
      collectArguments(args, kwArgs, argsToRange(n.fields), argsToKeys(n.fields)).map {
        case k -> v =>
          KeyValue(Ident(k), specializeNode(v, ctx))
      }.toList

    ApplyStruct(
      n.name,
      KwArgs(mapping)
    )
  }
  def specializeSelect(n: Select, ctx: SpecializeContext): AST = {
    val obj = specializeNode(n.obj, ctx)
    obj match {
      case ApplyStruct(name, values) =>
        values.args.find(_.name.name == n.field.name) match {
          case Some(v) => v.value
          case None => throw SpecializeException("field not found", n)
        }
      case o => o
    }
  }
  def specializeCond(n: Cond, ctx: SpecializeContext): AST = {
    val cond = specializeNode(n.cond, ctx)
    cond match {
      case LiteralBool(true) => specializeNode(n.consequence, ctx)
      case LiteralBool(false) => specializeNode(n.alternative, ctx)
      case _ =>
        Cond(
          cond,
          specializeNode(n.consequence, ctx),
          specializeNode(n.alternative, ctx)
        )
    }
  }

  def specializeForEach(n: ForEach, ctx: SpecializeContext): AST = {
    val iterable = specializeNode(n.iterable, ctx)
    if (isFinite(iterable, ctx.context)) {
      Block(
        iterable match {
          case LiteralList(value) =>
            value.map { v =>
              val ctx1 = ctx.withValue(n.variable.name, v)
              specializeNode(n.body, ctx1)
            }
          case Ident(name) =>
            ctx.context.getValue(name) match {
              case Some(LiteralList(value)) =>
                value.map { v =>
                  Block(
                    List(
                      DefVal(n.variable, v),
                      specializeNode(n.body, ctx)
                    )
                  )
                }
              case _ =>
                throw SpecializeException("cannot specialize: not a list", n)
            }
          case _ => throw SpecializeException("cannot specialize: not finite", n)
        }
      )
    } else {
      n.copy(
        iterable = iterable,
        body = specializeNode(n.body, ctx)
      )
    }
  }
  def specializeAssign(n: Assign, ctx: SpecializeContext): (AST, SpecializeContext) = {
    val name = n.target.asInstanceOf[Ident]
    // FIXME: this fix is not correct
    ctx.context.updateValue(name.name, LiteralUnknown())
    val value = specializeNode(n.value, ctx)
    ctx.context.updateValue(name.name, value)
    (Assign(n.target, value), ctx)
  }
  def specializeDefType(n: DefType, ctx: SpecializeContext): (DefType, SpecializeContext) = {
    val value = specializeNode(n.value, ctx)
    val t = DefType(n.name, n.params, value)
    val newCtx = ctx.withTypes(Map(n.name.name -> n))
    (t, newCtx)
  }
  def specializeFunApply(n: ApplyFun, ctx: SpecializeContext): AST = {
    val args = specializeNode(n.params, ctx).asInstanceOf[Parameters]
    val returns = specializeNode(n.ret, ctx)
    val body = specializeNode(n.body, ctx)
    ApplyFun(args, returns, body)
  }
}
