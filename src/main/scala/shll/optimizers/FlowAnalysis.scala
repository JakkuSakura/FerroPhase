package shll.optimizers

import com.typesafe.scalalogging.Logger
import shll.ast.*
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.frontends.ParamUtil.*
import shll.ast.AstHelper.*

import scala.collection.mutable

case class FlowAnalysisContext(
    context: ValueContext = ValueContext(),
    private val decls: mutable.Map[String, AST] = mutable.Map.empty,
    private val internalNodes: mutable.Set[String] = mutable.Set.empty,
    private val externalNodes: mutable.Set[String] = mutable.Set.empty,
    private val dataFlow: mutable.Set[(String, String)] = mutable.Set.empty,
    private val executionFlow: mutable.Set[(String, String)] = mutable.Set.empty,
    private val parent: Option[FlowAnalysisContext] = None
) {
  def child(): FlowAnalysisContext = {
    val child = FlowAnalysisContext(
      context,
      mutable.Map.empty,
      mutable.Set.empty,
      mutable.Set.empty,
      dataFlow,
      executionFlow,
      Some(this)
    )
    child
  }

  def printDataflow(): Unit = {
    dataFlow.foreach { case (k, v) => println(s"$k\n->\n$v\n---") }
  }

  def isDataflowReachable(from: String, to: String): Boolean = {
    val queue: mutable.Queue[String] = mutable.Queue(from)
    val visited: mutable.Set[String] = mutable.Set()
    while (queue.nonEmpty) {
      val current = queue.dequeue()

      visited.add(current)
      dataFlow.filter(_._1 == current).foreach {
        case (_, v) if !visited.contains(v) =>
          queue.enqueue(v)
        case _ =>
      }
    }
    visited.contains(to)
  }

  def isReachable(from: String, to: String): Boolean = {
    val queue: mutable.Queue[String] = mutable.Queue(from)
    val visited: mutable.Set[String] = mutable.Set()
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (isDataflowReachable(current, to)) {
        return true
      }
      visited.add(current)
      executionFlow.filter(_._1 == current).foreach {
        case (_, v) if !visited.contains(v) =>
          queue.enqueue(v)
        case _ =>
      }
    }
    false
  }

  def getName(node: AST): String = {
    node match {
      case LiteralUnknown() => "???"
//      case x: Ident
//          if !isLiteral(x, ValueContext())
//            && !Specializer().builtinTypes.contains(x.name)
//            && !Specializer().builtinTypes.contains(x.name) =>
//        x.name
      case _ => ShllPrettyPrinter.print(node) + "#" + node.num
    }
  }

  def addDataFlow(pair: (AST, AST)): Unit = {
    val (from, to) = pair
    val fromN = getName(from)
    val toN = getName(to)
    dataFlow += fromN -> toN
  }

  def addExecutionFlow(pair: (AST, AST)): Unit = {
    val (from, to) = pair
    val fromN = getName(from)
    val toN = getName(to)
    executionFlow += fromN -> toN
  }
  def addDecl(name: Ident, ast: AST): Unit = {
    decls += name.name -> ast
  }
  def getDecl(name: String): Option[AST] = {
    decls.get(name).orElse(parent.flatMap(_.getDecl(name)))
  }
  def addInternalNode(node: AST): Unit = {
    internalNodes += getName(node)
  }
  def isDataflowReachable(from: AST, to: AST): Boolean = {
    val fromN = getName(from)
    val toN = getName(to)
    isDataflowReachable(fromN, toN)
  }

  def isReachable(from: AST, to: AST): Boolean = {
    val fromN = getName(from)
    val toN = getName(to)
    isReachable(fromN, toN)
  }

  def mergeChildNodes(node: AST, other: FlowAnalysisContext): Unit = {
    decls ++= other.decls
    externalNodes ++= other.externalNodes
    internalNodes ++= other.internalNodes
    externalNodes --= internalNodes
  }
}
case class FlowAnalysis() {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter(newlines = true, withNumber = true)
  val contextHistory: mutable.Map[AST, FlowAnalysisContext] = mutable.Map.empty

  def analyze(n: AST): FlowAnalysisContext = {
    val ctx = FlowAnalysisContext()
    ctx.addDataFlow(n -> LiteralUnknown())
    analyzeNode(n, ctx)
    contextHistory(n)
  }

  def analyzeNode(n: AST, ctx: FlowAnalysisContext): Unit = {
//    logger.debug("Eliminating " + pp.print(n))
    if (!n.isInstanceOf[Ident])
      ctx.addInternalNode(n)
    n match {
      case n: Block => analyzeBlock(n, ctx)
      case n: Apply => analyzeApply(n, ctx)
      case n: Ident => analyzeReadIdent(n, ctx)
      case n: LiteralInt =>
      case n: LiteralDecimal =>
      case n: LiteralString =>
      case n: LiteralBool =>
      case n: LiteralList => n.value.foreach(analyzeNode(_, ctx))
      case n: Field => analyzeField(n, ctx)
      case n: Param => analyzeParam(n, ctx)
      case n: Select => analyzeSelect(n, ctx)
      case n: Cond => analyzeCond(n, ctx)
      case n: ForEach => analyzeForEach(n, ctx)
      case n: ApplyType => analyzeApplyType(n, ctx)
      case n: DefType => analyzeDefType(n, ctx)
      case n: Assign => analyzeAssign(n, ctx)
      case n: ApplyFun => analyzeApplyFun(n, ctx)
      case n: Params => n.params.foreach(analyzeNode(_, ctx))
      case n: Fields => n.fields.foreach(analyzeNode(_, ctx))
      case s: DefVal => analyzeDefVal(s, ctx)
      case d: DefFun => analyzeDefFun(d, ctx)
      case n: DefStruct => analyzeDefStruct(n, ctx)
      case x => throw SpecializeException("cannot analyze", x)
    }
//    println("Dataflow at: " + pp.print(n) + "\n===")
    if (!contextHistory.contains(n))
      contextHistory += n -> ctx
//    contextHistory(n).printDataflow()
  }

  def analyzeField(n: Field, ctx: FlowAnalysisContext): Unit = {
    analyzeNode(n.ty, ctx)
  }

  def analyzeParam(n: Param, ctx: FlowAnalysisContext): Unit = {
    analyzeNode(n.ty, ctx)
  }
  def analyzeDefVal(
      n: DefVal,
      ctx0: FlowAnalysisContext
  ): Unit = {
    val ctx = ctx0.child()
    ctx.addDecl(n.name, n)
    analyzeNode(n.value, ctx)
    ctx.addInternalNode(n)
    ctx.addDataFlow(n.value -> n)
    contextHistory += n -> ctx
  }
  def analyzeReadIdent(id: Ident, ctx: FlowAnalysisContext): Unit = {
    if (ctx.getDecl(id.name).isDefined) {
      ctx.addDataFlow(ctx.getDecl(id.name).get -> id)
    }
  }

  def analyzeApply(n: Apply, ctx: FlowAnalysisContext): Unit = {
    if (n.fun == Ident("print"))
      ctx.addDataFlow(n -> LiteralUnknown())
    analyzeNode(n.fun, ctx)
    n.args.args.foreach(analyzeNode(_, ctx))
    n.kwArgs.args.foreach(x => analyzeNode(x.value, ctx))

    n.args.args.foreach(x => ctx.addDataFlow(x -> n))
    n.kwArgs.args.map(_.value).foreach(x => ctx.addDataFlow(x -> n))
    ctx.addDataFlow(n.fun -> n)
//    n.fun match {
//      case Ident(name)
//          if ctx.context.getFunction(name).isEmpty && !Specializer().builtinFunctions.contains(
//            name
//          ) =>
//      case _ =>
//    }

  }

  def analyzeApplyType(n: ApplyType, ctx: FlowAnalysisContext): Unit = {

    analyzeNode(n.fun, ctx)
    n.args.args.foreach(x => analyzeNode(x, ctx))
    n.kwArgs.args.foreach(x => analyzeNode(x.value, ctx))

    ctx.addDataFlow(n.fun -> n)
    n.args.args.foreach(x => ctx.addDataFlow(x -> n))
    n.kwArgs.args.map(_.value).foreach(x => ctx.addDataFlow(x -> n))
  }

  def analyzeDefType(n: DefType, ctx: FlowAnalysisContext): Unit = {
    analyzeNode(n.value, ctx)
    ctx.addInternalNode(n.name)
    ctx.addDataFlow(n -> n.name)
    ctx.addDataFlow(n.value -> n.name)
  }

  def analyzeBlock(n: Block, ctx0: FlowAnalysisContext): Unit = {
    var ctx1 = ctx0.child()
    var ctx = ctx1
    // TODO: current flow analysis is far from complete
    n.children.foreach { x =>
      analyzeNode(x, ctx)
      ctx = contextHistory(x)
    }

    n.children.foreach { x =>
      ctx1.mergeChildNodes(n, contextHistory(x))
      ctx1.addExecutionFlow(n -> x)
    }
    n.children.lastOption.foreach(x => ctx1.addDataFlow(x -> n))
    contextHistory += n -> ctx1
  }

  def analyzeDefStruct(
      d: DefStruct,
      ctx0: FlowAnalysisContext
  ): Unit = {
    val ctx = ctx0.child()
    ctx.addDecl(d.name, d)
    analyzeNode(d.fields, ctx)
    ctx.addInternalNode(d)

    ctx.addDataFlow(d.fields -> d)
    contextHistory += d -> ctx
  }

  def analyzeDefFun(
      n: DefFun,
      ctx0: FlowAnalysisContext
  ): Unit = {
    val ctx = ctx0.child()
    ctx.addDecl(n.name, n)
    analyzeNode(n.params, ctx)
    analyzeNode(n.ret, ctx)
    analyzeNode(n.body, ctx)
    ctx.addDataFlow(n.body -> n)

    ctx.addInternalNode(n)
    ctx.addDataFlow(n.params -> n)
    ctx.addDataFlow(n.ret -> n)
    ctx.mergeChildNodes(n, contextHistory(n.body))

    contextHistory += n -> ctx
  }

  def analyzeSelect(n: Select, ctx: FlowAnalysisContext): Unit = {
    analyzeNode(n.obj, ctx)
    ctx.addDataFlow(n.obj -> n)
  }
  def analyzeCond(n: Cond, ctx: FlowAnalysisContext): Unit = {
    analyzeNode(n.cond, ctx)
    analyzeNode(n.consequence, ctx)
    analyzeNode(n.alternative, ctx)
    ctx.addDataFlow(n.cond -> n)
    ctx.addDataFlow(n.consequence -> n)
    ctx.addDataFlow(n.alternative -> n)
  }

  def analyzeForEach(n: ForEach, ctx0: FlowAnalysisContext): Unit = {
    val ctx = ctx0.child()
    val df = DefVal(n.variable, Select(n.iterable, Ident("next")))
    ctx.addDecl(n.variable, df)
    ctx.addInternalNode(n.variable)

    analyzeNode(df, ctx)
    analyzeNode(n.iterable, ctx)
    analyzeNode(n.body, ctx)
    analyzeNode(n.variable, ctx)
    ctx0.mergeChildNodes(n, ctx)

    ctx0.addDataFlow(n.iterable -> n.variable)
    ctx0.addDataFlow(n.variable -> n.body)

    ctx0.addExecutionFlow(n -> n.body)
    ctx0.addExecutionFlow(n -> n.iterable)

  }

  def analyzeAssign(n: Assign, ctx: FlowAnalysisContext): Unit = {
//    analyzeNodeWrite(n.target, ctx)
    n.target match {
      case x: Ident if ctx.getDecl(x.name).isDefined =>
        ctx.addDataFlow(x -> ctx.getDecl(x.name).get)
      case _ => throw new Exception("Not implemented")
    }
    analyzeNode(n.value, ctx)
    ctx.addDataFlow(n.value -> n)
    ctx.addDataFlow(n -> n.target)
  }
  def analyzeApplyFun(n: ApplyFun, ctx: FlowAnalysisContext): Unit = {
    val ctx1 = ctx.child()
    // TODO: process arguments
    analyzeNode(n.params, ctx1)
    analyzeNode(n.ret, ctx1)
    analyzeNode(n.body, ctx1)
    ctx.mergeChildNodes(n, ctx1)
    ctx1.addDataFlow(n.params -> n)
    ctx1.addDataFlow(n.ret -> n)
    ctx1.addDataFlow(n.body -> n)
  }
}
