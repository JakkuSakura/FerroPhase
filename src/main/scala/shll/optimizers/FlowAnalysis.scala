package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.*
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ParamUtil.*
import shll.ast.AstTool.*

import scala.collection.mutable

case class FlowAnalysisCache(
    private val ufs: mutable.Map[String, String] = mutable.Map.empty,
    private val parent: Option[FlowAnalysisCache] = None
) {
  def findCacheForElement(name: String): FlowAnalysisCache = {
    if (ufs.contains(name)) {
      this
    } else {
      parent.map(_.findCacheForElement(name)).getOrElse(this)
    }
  }
  def findUfsParent(s: String): String = {
    val p = findCacheForElement(s)

    if (p.ufs.get(s).contains(s)) p.ufs(s)
    else {
      if (!p.ufs.contains(s))
        p.ufs += s -> s
      val root = p.findUfsParent(p.ufs(s))
      p.ufs += s -> root
      root
    }

  }
  def forceAddUfs(s: AST): Unit = {
    val name = getName(s)
    if (name.isEmpty) return
    ufs += name.get -> name.get
  }

  def mergeUfs(s1: String, s2: String): Unit = {
    val p1 = findCacheForElement(s1)
    val p2 = findCacheForElement(s2)
    p1.ufs += p1.findUfsParent(s1) -> p2.findUfsParent(s2)
    p2.ufs += p1.findUfsParent(s1) -> p2.findUfsParent(s2)
  }

  def getName(node: AST): Option[String] = {
    Some(ShllPrettyPrinter().print(node) + "@" + node.num)
//    node match {
//      case Ident(name)
//          if !Specializer().builtinTypes.contains(name)
//            && !Specializer().builtinFunctions.contains(name) =>
//        None
//      case x if isLiteral(x, ValueContext()) => None
//      case _ => Some(ShllPrettyPrinter().print(node) + "@" + node.num)
//    }
  }
  def addDependency(user: AST, usee: AST): Unit = {
    val userN = getName(user)
    if (userN.isEmpty) return
    val useeN = getName(usee)
    if (useeN.isEmpty) return
    mergeUfs(userN.get, useeN.get)
  }
  def isUnion(s1: AST, s2: AST): Boolean = {
    val userN = getName(s1)
    if (userN.isEmpty) return false
    val useeN = getName(s2)
    if (useeN.isEmpty) return false
    findUfsParent(userN.get) == findUfsParent(useeN.get)
  }
}
case class FlowAnalysisContext(
    context: ValueContext = ValueContext(),
    private val cache: FlowAnalysisCache = FlowAnalysisCache(),
    private val parent: Option[FlowAnalysisContext] = None
) {
  def getCache: FlowAnalysisCache = cache
  def childContext: FlowAnalysisContext = FlowAnalysisContext(context, FlowAnalysisCache(parent = Some(cache)), parent = Some(this))
  def withValues(values: Map[String, AST]): FlowAnalysisContext =
    FlowAnalysisContext(
      context = context.withValues(values),
      cache = cache,
      parent = Some(this)
    )

  def withValue(name: String, value: AST): FlowAnalysisContext =
    FlowAnalysisContext(
      context = context.withValue(name, value),
      cache = cache,
      parent = Some(this)
    )
  def withStruct(name: String, struct: DefStruct): FlowAnalysisContext =
    FlowAnalysisContext(
      context = context.withStruct(name, struct),
      cache = cache,
      parent = Some(this)
    )
  def withFunction(name: String, func: DefFun): FlowAnalysisContext =
    FlowAnalysisContext(
      context = context.withFunction(name, func),
      cache = cache,
      parent = Some(this)
    )
}
case class FlowAnalysis() {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter(newlines = false)
  val contextHistory: mutable.Map[AST, FlowAnalysisContext] = mutable.Map.empty

  def analyze(n: AST): FlowAnalysisContext = {
    val ctx = FlowAnalysisContext()
    ctx.getCache.addDependency(n, LiteralUnknown())
    analyzeNode(n, ctx)
    ctx
  }

  def analyzeNode(n: AST, ctx: FlowAnalysisContext): Unit = {
//    logger.debug("Eliminating " + pp.print(n))
    n match {
      case n: Block => analyzeBlock(n, ctx)
      case n: Apply => analyzeApply(n, ctx)
      case n: Ident => analyzeIdent(n, ctx)
      case n: LiteralInt =>
      case n: LiteralDecimal =>
      case n: LiteralString =>
      case n: LiteralBool =>
      case n: LiteralList => n.value.foreach(analyzeNode(_, ctx))
      case n: Field => analyzeField(n, ctx)
      case n: Select => analyzeSelect(n, ctx)
      case n: Cond => analyzeCond(n, ctx)
      case n: ForEach => analyzeForEach(n, ctx)
      case n: TypeApply => analyzeTypeApply(n, ctx)
      case n: DefType => analyzeDefType(n, ctx)
      case n: Assign => analyzeAssign(n, ctx)
      case n: FunApply => analyzeFunApply(n, ctx)
      case x => throw SpecializeException("cannot analyze", x)
    }
    contextHistory += n -> ctx
  }

  def analyzeField(n: Field, ctx: FlowAnalysisContext): Unit = {
    analyzeNode(n.ty, ctx)
  }
  def analyzeDefVal(
      n: DefVal,
      ctx: FlowAnalysisContext
  ): FlowAnalysisContext = {
    analyzeNode(n.value, ctx)
    ctx.getCache.forceAddUfs(n)
    ctx.getCache.addDependency(n, n.value)
    ctx.getCache.addDependency(n.name, n.value)
    ctx.withValue(n.name.name, n.value)
  }
  def analyzeIdent(id: Ident, ctx: FlowAnalysisContext): AST = {
    ctx.context.getValue(id.name).foreach(ctx.getCache.addDependency(id, _))
    ctx.context.getType(id.name).foreach(ctx.getCache.addDependency(id, _))
    ctx.context.getFunction(id.name).foreach(ctx.getCache.addDependency(id, _))
    ctx.context.getStruct(id.name).foreach(ctx.getCache.addDependency(id, _))
    id
  }

  def analyzeApply(n: Apply, ctx: FlowAnalysisContext): Unit = {
    analyzeNode(n.fun, ctx)
    n.args.foreach(analyzeNode(_, ctx))
    n.kwArgs.foreach(x => analyzeNode(x.value, ctx))

    n.args.foreach(ctx.getCache.addDependency(n, _))
    n.kwArgs.map(_.value).foreach(ctx.getCache.addDependency(n, _))
    n.fun match {
      case Ident(name)
          if ctx.context.getFunction(name).isEmpty && !Specializer().builtinFunctions.contains(
            name
          ) =>
        ctx.getCache.addDependency(n, LiteralUnknown())
      case _ =>
    }

  }

  def analyzeTypeApply(n: TypeApply, ctx: FlowAnalysisContext): Unit = {
    analyzeNode(n.fun, ctx)
    n.args.foreach(analyzeNode(_, ctx))
    n.kwArgs.foreach(x => analyzeNode(x.value, ctx))

    ctx.getCache.addDependency(n, n.fun)
    n.args.foreach(ctx.getCache.addDependency(n, _))
    n.kwArgs.map(_.value).foreach(ctx.getCache.addDependency(n, _))
    n.fun match {
      case i: Ident =>
        ctx.getCache.addDependency(n, i)
      case _ =>
    }
  }

  def analyzeDefType(n: DefType, context: FlowAnalysisContext): AST = {
    analyzeNode(n.value, context)

    context.getCache.addDependency(n, n.name)
    context.getCache.addDependency(n, n.value)
    n
  }

  def analyzeBlock(d: Block, ctx0: FlowAnalysisContext): Unit = {
    var ctx = ctx0
//    var ctx = ctx0.childContext
    d.body.foreach { x =>
      x match {
        case s: DefVal =>
          val newCtx = analyzeDefVal(s, ctx)
          ctx = newCtx
        case s: Assign =>
          analyzeAssign(s, ctx)
        case d: DefFun =>
          val newCtx = analyzeDefFun(d, ctx)
          ctx = newCtx
        case n: DefStruct =>
          val newCtx = analyzeDefStruct(n, ctx)
          ctx = newCtx
        case s =>
          analyzeNode(s, ctx)
          if (hasSideEffects(s, ctx.context)) {
            ctx.getCache.addDependency(d, s)
          }
      }
      contextHistory += x -> ctx
    }
    d.body.lastOption.foreach(x => ctx.getCache.addDependency(d, x))

  }

  def analyzeDefStruct(
      d: DefStruct,
      ctx: FlowAnalysisContext
  ): FlowAnalysisContext = {
    // TODO: process values
    analyzeNode(d.fields, ctx)

    ctx.getCache.addDependency(d, d.fields)
    ctx.getCache.addDependency(d.name, d.fields)
    ctx.withStruct(d.name.name, d)
  }

  def analyzeDefFun(
      d: DefFun,
      ctx: FlowAnalysisContext
  ): FlowAnalysisContext = {
    analyzeNode(d.args, ctx)
    analyzeNode(d.ret, ctx)
    d.body.foreach(analyzeNode(_, ctx))

    d.body.foreach(ctx.getCache.addDependency(d, _))
    ctx.getCache.addDependency(d, d.args)
    ctx.getCache.addDependency(d, d.ret)
    ctx.getCache.addDependency(d.name, d)
    ctx.withFunction(d.name.name, d)
  }

  def analyzeSelect(n: Select, ctx: FlowAnalysisContext): Unit = {
    analyzeNode(n.obj, ctx)
    ctx.getCache.addDependency(n.field, n.obj)

  }
  def analyzeCond(n: Cond, ctx: FlowAnalysisContext): Unit =
    analyzeNode(n.cond, ctx)
    analyzeNode(n.consequence, ctx)
    analyzeNode(n.alternative, ctx)

  def analyzeForEach(n: ForEach, ctx: FlowAnalysisContext): Unit = {
    analyzeNode(n.iterable, ctx)
    analyzeNode(n.body, ctx)

    ctx.getCache.addDependency(n.variable, n.iterable)
    ctx.getCache.addDependency(n.variable, n)
    ctx.getCache.addDependency(n.body, n.variable)
    ctx.getCache.addDependency(n.body, n)
  }

  def analyzeAssign(n: Assign, ctx: FlowAnalysisContext): Unit = {
    analyzeNode(n.value, ctx)
    ctx.getCache.addDependency(n, n.value)
    ctx.getCache.addDependency(n.name, n)
    ctx.getCache.addDependency(n.name, n.value)

  }
  def analyzeFunApply(n: FunApply, ctx: FlowAnalysisContext): Unit = {
    analyzeNode(n.args, ctx)
    analyzeNode(n.ret, ctx)
    analyzeNode(n.body, ctx)

    ctx.getCache.addDependency(n, n.args)
    ctx.getCache.addDependency(n, n.ret)
    ctx.getCache.addDependency(n, n.body)
  }
}
