package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.{AST, *}
import shll.frontends.ParamUtil.*

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
    structDeclMap: mutable.HashMap[String, DefStruct] = mutable.HashMap.empty,
    specializedStructs: mutable.HashMap[String, DefStruct] = mutable.HashMap.empty,
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
  private var cache: SpecializeCache = SpecializeCache()

  val builtinFunctions: Map[String, (Apply, ValueContext) => AST] = Map(
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
  def binaryOperator(fn: (apply: AST, lhs: AST, rhs: AST) => AST): (Apply, ValueContext) => AST = {
    (apply, ctx) =>
      {
        checkArguments(apply, Array(0, 1), Array("lhs", "rhs"))
        val lhs = specializeNode(getArg(apply, 0, "lhs"), ctx)
        val rhs = specializeNode(getArg(apply, 1, "rhs"), ctx)
        val a = Apply(apply.fun, List(lhs, rhs), Nil)
        fn(a, lhs, rhs)
      }
  }
  def specialize(n: AST): AST = {
    cache = SpecializeCache()
    val v = specializeNode(n, ValueContext.empty)
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
      case n: Apply => specializeApply(n, ctx)
      case n: Ident => specializeIdent(n, ctx)
      case n: LiteralInt => n
      case n: LiteralDecimal => n
      case n: LiteralString => n
      case n: LiteralBool => n
      case n: LiteralList => LiteralList(n.value.map(specializeNode(_, ctx)))
      case n: Field => specializeField(n, ctx)
      case n: DefStruct => specializeDefStruct(n, ctx)
      case n: Select => specializeSelect(n, ctx)
      case n: Cond => specializeCond(n, ctx)
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
      case id: Ident if builtinFunctions.contains(id.name) =>
        val fn = builtinFunctions(id.name)
        fn(n, ctx)
      case id: Ident if cache.funcDeclMap.contains(id.name) =>
        val func = cache.funcDeclMap(id.name)
        specializeFunctionApply(func, n.args, n.kwArgs, ctx)
      case id: Ident if cache.structDeclMap.contains(id.name) =>
        val struct = cache.structDeclMap(id.name)
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

  def specializeDefStruct(c: DefStruct, ctx: ValueContext): DefStruct = {
    cache.structDeclMap(c.name.name) = c
    c
  }
  def isSpecializedFunctionDecl(d: DefFun): Boolean = {
    d.args match {
      case LiteralList(value) => value.isEmpty
//      case _ => false
    }
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
      ctx: ValueContext
  ): Apply = {
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

    val newFunc = func
      .copy(
        name = cache.allocateSpecializedIdent(func.name.name),
        body = Some(body),
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
    if (isSpecializedFunctionDecl(d) && d.body.isDefined) {
      // TODO evaluate constants
      val body = specializeNode(d.body.get, ctx)
      d.copy(body = Some(body))
    } else {
      d
    }
  }
  def specializeStructApply(
      n: DefStruct,
      args: List[AST],
      kwArgs: List[KeyValue],
      ctx: ValueContext
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
  def specializeSelect(n: Select, ctx: ValueContext): AST = {
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
  def specializeCond(n: Cond, ctx: ValueContext): AST = {
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
}
