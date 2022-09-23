package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.*
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ParamUtil.*
import shll.optimizers.AstTool.isConstant

import scala.collection.mutable

case class DeadCodeEliminatorContext(
    context: ValueContext = ValueContext(),
    private val nameNodeMapping: mutable.Map[AST, String] = mutable.Map.empty,
    private val userDependency: mutable.Map[String, mutable.Set[String]] = mutable.Map.empty,
    private val revUserDependency: mutable.Map[String, mutable.Set[String]] = mutable.Map.empty,
    private val used: mutable.Set[String] = mutable.Set.empty
) {
  def addMapping(node: AST, name: String): Unit = {
    if (!nameNodeMapping.contains(node)) {
      nameNodeMapping += node -> name
    }
  }

  def addMapping(node: AST, parent: AST): Unit = {
    if (getName(parent).isDefined) {
      val parentName = getName(parent).get
      addMapping(node, parentName)
    }
  }

  def addDependency(user: String, usee: String): Unit = {
    userDependency.getOrElseUpdate(user, mutable.Set.empty) += usee
    revUserDependency.getOrElseUpdate(usee, mutable.Set.empty) += user
    if (used.contains(user)) {
      markUsed(usee)
    }
  }
  def getName(node: AST): Option[String] = {
    if (isConstant(node)) {
      Some("constant")
    } else {
      nameNodeMapping.get(node)
    }
  }
  def addDependency(user: AST, usee: AST): Unit = {
    if (nameNodeMapping.contains(user) && nameNodeMapping.contains(usee)) {
      val userName = getName(user).get
      val useeName = getName(usee).get
      if (userName != useeName) {
        addDependency(userName, useeName)
      }
    }
  }

  def markUsed(usee: String): Unit = {
    if (!used.contains(usee)) {
      used += usee
      userDependency.getOrElse(usee, mutable.Set.empty).foreach { user =>
        markUsed(user)
      }
    }

  }

  def markUsed(usee: AST): Unit = {
    if (getName(usee).isDefined) {
      val useeName = getName(usee).get
      markUsed(useeName)
    }
  }
  def isUsed(name: String): Boolean = {
    used.contains(name)
  }

  def isUsed(node: AST): Boolean = {
    getName(node).exists(isUsed)
  }
  def withValues(values: Map[String, AST]): DeadCodeEliminatorContext =
    copy(context = context.withValues(values))
  def withStructs(structs: Map[String, DefStruct]): DeadCodeEliminatorContext =
    copy(context = context.withStructs(structs))
  def withFunctions(functions: Map[String, DefFun]): DeadCodeEliminatorContext =
    copy(context = context.withFunctions(functions))
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
      case n: Assign => eliminateAssign(n, ctx)
      case x => throw SpecializeException("cannot eliminate", x)
    }

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
    ctx.addMapping(n, n.name.name)
    ctx.addMapping(n.name, n.name.name)
    ctx.addDependency(n, value)
    ctx.addDependency(n.name, value)
    (DefVal(n.name, value), newCtx)
  }
  def eliminateIdent(id: Ident, ctx: DeadCodeEliminatorContext): AST = {
    ctx.addMapping(id, id.name)
    id
  }

  def eliminateApply(n: Apply, ctx: DeadCodeEliminatorContext): AST = {
    val fun = eliminateNode(n.fun, ctx)
    val args = n.args.map(eliminateNode(_, ctx))
    val kwArgs = n.kwArgs.map(x => KeyValue(x.name, eliminateNode(x.value, ctx)))
    val newApply = Apply(fun, args, kwArgs)
    ctx.addDependency(newApply, fun)
    args.foreach(ctx.addDependency(newApply, _))
    kwArgs.map(_.value).foreach(ctx.addDependency(newApply, _))
    fun match {
      case Ident(name) if ctx.context.getFunction(name).isEmpty =>
        ctx.addMapping(newApply, "external")
        ctx.markUsed(newApply)
    }
    newApply
  }

  def eliminateTypeApply(n: TypeApply, ctx: DeadCodeEliminatorContext): AST = {
    val fun = eliminateNode(n.fun, ctx)
    val args = n.args.map(eliminateNode(_, ctx))
    val kwArgs = n.kwArgs.map(x => KeyValue(x.name, eliminateNode(x.value, ctx)))
    val newApply = TypeApply(fun, args, kwArgs)
    ctx.addDependency(newApply, fun)
    args.foreach(ctx.addDependency(newApply, _))
    kwArgs.map(_.value).foreach(ctx.addDependency(newApply, _))
    fun match {
      case Ident(name) if ctx.context.getTyValue(name).isEmpty =>
        if (args.isEmpty) {
          ctx.addMapping(newApply, name)
        } else {
          ctx.addMapping(newApply, "builtin_type")
        }
        ctx.markUsed(newApply)
    }
    newApply
  }

  def eliminateDefType(n: DefType, context: DeadCodeEliminatorContext): AST = {
    context.addMapping(n, n.name.name)
    context.addMapping(n.name, n.name.name)
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
        val n = eliminateNode(s, ctx)
        ctx.addMapping(d, n)
        n
    }
    stmts.lastOption.foreach(ctx.markUsed(_))
    val filteredStmts = stmts.filter(ctx.isUsed)
    if (filteredStmts.length == 1)
      filteredStmts.head
    else
      Block(filteredStmts)

  }

  def eliminateDefStruct(
      c: DefStruct,
      ctx: DeadCodeEliminatorContext
  ): (DefStruct, DeadCodeEliminatorContext) = {
    // TODO: process values
    ctx.addMapping(c, c.name.name)
    (c, ctx.withStructs(Map(c.name.name -> c)))
  }

  def eliminateDefFun(
      d: DefFun,
      ctx: DeadCodeEliminatorContext
  ): (DefFun, DeadCodeEliminatorContext) = {
    ctx.addMapping(d, d.name.name)
    d.args.value.foreach { case Field(name, ty) =>
      ctx.addMapping(name, name.name)
    }
    val body = d.body.map(eliminateNode(_, ctx))
    val dd = d.copy(body = body)
    ctx.addMapping(dd, dd.name.name)
    val newCtx = ctx.withFunctions(Map(d.name.name -> d))
    (dd, newCtx)
  }

  def eliminateSelect(n: Select, ctx: DeadCodeEliminatorContext): AST = {
    val obj = eliminateNode(n.obj, ctx)
    ctx.addDependency(n.field, obj)
    Select(obj, n.field)
  }
  def eliminateCond(n: Cond, ctx: DeadCodeEliminatorContext): AST =
    val cond = eliminateNode(n.cond, ctx)
    val conseq = eliminateNode(n.consequence, ctx)
    val alt = eliminateNode(n.alternative, ctx)
    val condTotal = Cond(cond, conseq, alt)
    condTotal

  def eliminateForEach(n: ForEach, ctx: DeadCodeEliminatorContext): AST = {
    ctx.addMapping(n.variable, n.variable.name)
    ctx.addMapping(n, n.variable.name)
    val iterable = eliminateNode(n.iterable, ctx)
    val body = eliminateNode(n.body, ctx)
    val f = ForEach(n.variable, iterable, body)
    ctx.addDependency(f.variable, iterable)
    ctx.addDependency(f.variable, f)
    ctx.addDependency(body, f.variable)
    ctx.addDependency(body, f)
    f
  }
  def eliminateAssign(n: Assign, ctx: DeadCodeEliminatorContext): AST =
    val value = eliminateNode(n.value, ctx)
    val ass = Assign(n.name, value)
    ctx.addMapping(ass, n.name.name)
    ctx.addMapping(ass.name, n.name.name)
    ctx.addDependency(ass, ass.value)
    ctx.addDependency(ass.name, ass)
    ctx.addDependency(ass.name, ass.value)
    ass
}
