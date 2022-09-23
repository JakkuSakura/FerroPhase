package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.{AST, *}
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ParamUtil.*

import scala.collection.mutable
case class SpecializeException(msg: String, node: AST) extends Exception(msg + ": " + node)

case class SpecializeContext(
    cache: Option[SpecializeCache] = None,
    context: ValueContext = ValueContext(),
    parent: Option[SpecializeContext] = None
) {
  def getCache: SpecializeCache = cache.getOrElse(parent.get.getCache)
  def withCache(cache: SpecializeCache): SpecializeContext = copy(cache = Some(cache), parent = Some(this))
  def withValues(values: Map[String, AST]): SpecializeContext = {
    copy(context = context.withValues(values))
  }
  def withStructs(structs: Map[String, DefStruct]): SpecializeContext = {
    copy(context = context.withStructs(structs))
  }
  def withFunctions(functions: Map[String, DefFun]): SpecializeContext = copy(context = context.withFunctions(functions))
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

case class Specializer() {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter(newlines = false)

  val builtinFunctions: Map[String, (Apply, SpecializeContext) => AST] = Map(
    "==" -> binaryOperator { (apply, lhs, rhs) =>
      if (lhs == rhs)
        LiteralBool(true)
      else if (isConstant(lhs) && isConstant(rhs))
        LiteralBool(false)
      else
        apply
    },
    "!=" -> binaryOperator { (apply, lhs, rhs) =>
      if (lhs == rhs)
        LiteralBool(false)
      else if (isConstant(lhs) && isConstant(rhs))
        LiteralBool(true)
      else
        apply
    },
    ">" -> binaryOperator { (apply, lhs, rhs) =>
      (lhs, rhs) match {
        case (LiteralInt(l, lr), LiteralInt(r, rr)) => LiteralBool(l > r)
        case (LiteralDecimal(l, lr), LiteralDecimal(r, rr)) => LiteralBool(l > r)
        case _ => apply
      }
    },
    ">=" -> binaryOperator { (apply, lhs, rhs) =>
      (lhs, rhs) match {
        case (LiteralInt(l, lr), LiteralInt(r, rr)) => LiteralBool(l >= r)
        case (LiteralDecimal(l, lr), LiteralDecimal(r, rr)) => LiteralBool(l >= r)
        case _ => apply
      }
    },
    "<" -> binaryOperator { (apply, lhs, rhs) =>
      (lhs, rhs) match {
        case (LiteralInt(l, lr), LiteralInt(r, rr)) => LiteralBool(l < r)
        case (LiteralDecimal(l, lr), LiteralDecimal(r, rr)) => LiteralBool(l < r)
        case _ => apply
      }
    },
    "<=" -> binaryOperator { (apply, lhs, rhs) =>
      (lhs, rhs) match {
        case (LiteralInt(l, lr), LiteralInt(r, rr)) => LiteralBool(l <= r)
        case (LiteralDecimal(l, lr), LiteralDecimal(r, rr)) => LiteralBool(l <= r)
        case _ => apply
      }
    },
    "+" -> binaryOperator { (apply, lhs, rhs) =>
      (lhs, rhs) match {
        case (LiteralInt(l, lr), LiteralInt(r, rr)) => LiteralInt(l + r, s"(+ $lr $rr)")
        case (LiteralDecimal(l, lr), LiteralDecimal(r, rr)) => LiteralDecimal(l + r, s"(+ $lr $rr)")
        case (LiteralString(l, lr), LiteralString(r, rr)) => LiteralString(l + r, s"(+ $lr $rr)")
        case _ => apply
      }
    },
    "-" -> binaryOperator { (apply, lhs, rhs) =>
      (lhs, rhs) match {
        case (LiteralInt(l, lr), LiteralInt(r, rr)) => LiteralInt(l - r, s"(- $lr $rr)")
        case (LiteralDecimal(l, lr), LiteralDecimal(r, rr)) => LiteralDecimal(l - r, s"(- $lr $rr)")
        case _ => apply
      }
    },
    "*" -> binaryOperator { (apply, lhs, rhs) =>
      (lhs, rhs) match {
        case (LiteralInt(l, lr), LiteralInt(r, rr)) => LiteralInt(l * r, s"(* $lr $rr)")
        case (LiteralDecimal(l, lr), LiteralDecimal(r, rr)) => LiteralDecimal(l * r, s"(* $lr $rr)")
        case _ => apply
      }
    },
    "/" -> binaryOperator { (apply, lhs, rhs) =>
      (lhs, rhs) match {
        case (LiteralInt(l, lr), LiteralInt(r, rr)) => LiteralInt(l / r, s"(/ $lr $rr)")
        case (LiteralDecimal(l, lr), LiteralDecimal(r, rr)) => LiteralDecimal(l / r, s"(/ $lr $rr)")
        case _ => apply
      }
    },
    "%" -> binaryOperator { (apply, lhs, rhs) =>
      (lhs, rhs) match {
        case (LiteralInt(l, lr), LiteralInt(r, rr)) => LiteralInt(l % r, s"(% $lr $rr)")
        case (LiteralDecimal(l, lr), LiteralDecimal(r, rr)) => LiteralDecimal(l % r, s"(% $lr $rr)")
        case _ => apply
      }
    }
  )
  def binaryOperator(
      fn: (apply: AST, lhs: AST, rhs: AST) => AST
  ): (Apply, SpecializeContext) => AST = { (apply, ctx) =>
    {
      checkArguments(apply.args, apply.kwArgs, Array(0, 1), Array("lhs", "rhs"))
      val lhs = specializeNode(getArg(apply.args, apply.kwArgs, 0, "lhs"), ctx)
      val rhs = specializeNode(getArg(apply.args, apply.kwArgs, 1, "rhs"), ctx)
      val a = Apply(apply.fun, List(lhs, rhs), Nil)
      fn(a, lhs, rhs)
    }
  }
  val builtinTypes: Map[String, (TypeApply, SpecializeContext) => AST] = Map(
    "int" -> simpleType,
    "bool" -> simpleType,
    "numeric" -> simpleType,
    "string" -> simpleType,
    "char" -> simpleType,
    "list" -> simpleGenericType
  )

  def getTypeName(name: AST, ctx: SpecializeContext): String = {
    name match {
      case Ident(name) => name
      case _ => throw SpecializeException("Unknown type name", name)
    }
  }
  def isKnownType(name: AST, ctx: SpecializeContext): Boolean = {
    name match {
      case Ident(name) if builtinTypes.contains(name) => true
      case _ => false
    }
  }
  def simpleGenericType: (TypeApply, SpecializeContext) => AST = { (apply, ctx) =>
    checkArguments(apply.args, apply.kwArgs, Array(0), Array("value"))
    val value = specializeNode(getArg(apply.args, apply.kwArgs, 0, "value"), ctx)
    val newApply = TypeApply(apply.fun, List(value), Nil)
    if (isKnownType(apply.fun, ctx) && isKnownType(value, ctx)) {
      val newName = Ident(getTypeName(apply.fun, ctx) + "_" + getTypeName(value, ctx))
      ctx.getCache.specializedTypes += newName.name -> DefType(newName, newApply)
      TypeApply(newName, Nil, Nil)
    } else {
      newApply
    }
  }
  def simpleType: (TypeApply, SpecializeContext) => AST = { (apply, ctx) =>
    apply
  }
  def specialize(n: AST): AST = {
    specializeNode(n, SpecializeContext())

  }

  def specializeNode(n: AST, ctx: SpecializeContext): AST = {
    logger.debug("Specializing " + pp.print(n))
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
      case n: TypeApply => specializeTypeApply(n, ctx)
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
    val newCtx = ctx.withValues(
      Map(
        n.name.name -> value
      )
    )
    (DefVal(n.name, value), newCtx)
  }
  def specializeIdent(id: Ident, ctx: SpecializeContext): AST = {
    ctx.context.getValue(id.name).getOrElse(id)
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
          n.args.map(specializeNode(_, ctx)),
          n.kwArgs.map(specializeKeyValue(_, ctx))
        )
    }
  }

  def specializeTypeApply(n: TypeApply, ctx: SpecializeContext): AST = {
    n.fun match {
      case Ident(name) if builtinTypes.contains(name) =>
        val ty = builtinTypes(name)
        ty(n, ctx)
      case _ =>
        val f = specializeNode(n.fun, ctx)
        TypeApply(
          f,
          n.args.map(specializeNode(_, ctx)),
          n.kwArgs.map(specializeKeyValue(_, ctx))
        )
    }
  }
  def specializeBlock(d: Block, ctx0: SpecializeContext): Block = {
    val cache = SpecializeCache()
    var ctx = ctx0.withCache(cache)
    val stmts = d.body.map {
      case s: DefVal =>
        val (x, newCtx) = specializeDefVal(s, ctx)
        ctx = newCtx
        x
      case s: Assign =>
        val (x, newCtx) = specializeDefVal(DefVal(s.name, s.value), ctx)
        ctx = newCtx
        Assign(x.name, x.value)
      case d: DefFun =>
        val (x, newCtx) = specializeDefFun(d, ctx)
        ctx = newCtx
        x
      case n: DefStruct =>
        val (x, newCtx) = specializeDefStruct(n, ctx)
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
      ctx: SpecializeContext,
      d: Map[String, AST],
      oldBody: AST
  ): (AST, SpecializeContext) = {
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
  def specializeFunctionApply(
      func: DefFun,
      args: List[AST],
      kwArgs: List[KeyValue],
      ctx: SpecializeContext
  ): AST = {
    val mapping =
      collectArguments(args, kwArgs, argsToRange(func.args), argsToKeys(func.args))
        .map { case k -> v =>
          k -> specializeNode(v, ctx)
        }
    val (newBody, newCtx) = prepareCtx(
      ctx,
      mapping,
      func.body.getOrElse(throw SpecializeException("cannot specialize: empty body", func))
    )
    val body = specializeNode(newBody, newCtx)
    body match {
      case x if isConstant(x) => body
      case _ =>
        val newFunc = func
          .copy(
            name = ctx.getCache.allocateSpecializedIdent(func.name.name),
            body = Some(body),
            args = LiteralList(Nil),
            ret = func.ret
          )
        ctx.getCache.specializedFunctions(newFunc.name.name) = newFunc
        Apply(newFunc.name, Nil, Nil)
    }
  }

  def specializeDefFun(
      d: DefFun,
      ctx: SpecializeContext
  ): (DefFun, SpecializeContext) = {
    val newCtx = ctx.withFunctions(Map(d.name.name -> d))
    if (d.body.isDefined) {
      val body = specializeNode(d.body.get, ctx)
      (d.copy(body = Some(body)), newCtx)
    } else {
      (d, newCtx)
    }
  }
  def specializeStructApply(
      n: DefStruct,
      args: List[AST],
      kwArgs: List[KeyValue],
      ctx: SpecializeContext
  ): DefStruct = {
    val mapping =
      collectArguments(args, kwArgs, argsToRange(n.fields), argsToKeys(n.fields)).map {
        case k -> v =>
          KeyValue(Ident(k), specializeNode(v, ctx))
      }.toList

    DefStruct(
      n.name,
      n.fields,
      mapping
    )
  }
  def specializeSelect(n: Select, ctx: SpecializeContext): AST = {
    val obj = specializeNode(n.obj, ctx)
    obj match {
      case DefStruct(name, fields, values) =>
        values.find(_.name.name == n.field.name) match {
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
  def isFinite(n: AST): Boolean = {
    n match {
      case x: LiteralList => true
      case _ => false
    }
  }
  def specializeForEach(n: ForEach, ctx: SpecializeContext): AST = {
    val iterable = specializeNode(n.iterable, ctx)
    if (isFinite(iterable)) {
      Block(
        iterable match {
          case LiteralList(value) =>
            value.map { v =>
              val ctx1 = ctx.withValues(Map(n.variable.name -> v))
              specializeNode(n.body, ctx1)
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

}
