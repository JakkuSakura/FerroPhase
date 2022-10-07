package shll

import com.typesafe.scalalogging.Logger
import org.junit.jupiter.api.Test
import shll.ast.*
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}
import shll.optimizers.FlowAnalysis
import org.junit.jupiter.api.Assertions.assertEquals

class TestFlowAnalysis {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter()
  @Test def testBlockApply(): Unit = {
    val fl = FlowAnalysis()
    val blk = AstHelper.block(Apply(Ident("foo"), PosArgs(Nil), KwArgs(Nil)))
    val ctx = fl.analyze(blk)

    assertEquals(true, ctx.isReachable(blk, LiteralUnknown()))
    assertEquals(true, ctx.isReachable(blk.children.head, LiteralUnknown()))
  }

  @Test def testBlockAssign(): Unit = {
    val fl = FlowAnalysis()
    val blk = Block(
      List(
        DefVal(Ident("a"), LiteralInt(1)),
        DefVal(Ident("b"), LiteralInt(1)),
        Ident("a")
      )
    )
    val ctx = fl.analyze(blk)
    assertEquals(true, ctx.isReachable(blk.children(0), LiteralUnknown()))
    assertEquals(false, ctx.isReachable(blk.children(1), LiteralUnknown()))
  }

  @Test def testBlockDefFun(): Unit = {
    val fl = FlowAnalysis()
    val blk = AstHelper.block(
      AstHelper.defFun(
        "a",
        List(("a", AstHelper.tInt)),
        AstHelper.tInt,
        AstHelper.applyFun("+", Ident("a"), Ident("a"))
      ),
      Ident("a")
    )
    val ctx = fl.analyze(blk)
    assertEquals(true, ctx.isReachable(blk.children(0), LiteralUnknown()))
    assertEquals(true, ctx.isReachable(blk.children(1), LiteralUnknown()))
  }

  @Test def testBlockFor(): Unit = {
    val fl = FlowAnalysis()
    val foreach = AstHelper.forEach(
      "a",
      Ident("values"),
      AstHelper.block(Assign(Ident("s"), AstHelper.applyFun("+", Ident("s"), Ident("a"))))
    )
    val blk = AstHelper.block(
      DefVal(Ident("s"), LiteralInt(1)),
      DefVal(Ident("values"), AstHelper.applyFun("range", LiteralInt(1), LiteralInt(10))),
      foreach,
      Ident("s")
    )
    val ctx = fl.analyze(blk)
    assertEquals(true, ctx.isReachable(blk.children(0), LiteralUnknown()))
    assertEquals(true, ctx.isReachable(blk.children(1), LiteralUnknown()))
    assertEquals(true, ctx.isReachable(blk.children(2), LiteralUnknown()))
    assertEquals(true, ctx.isReachable(blk.children(3), LiteralUnknown()))
    assertEquals(true, ctx.isReachable(foreach.body, LiteralUnknown()))
  }

  @Test def testFunApply(): Unit = {
    val blk = TestHelper
      .parseCode("""
        |(block
        |   (def-fun sum (lp (: values [list [int]])) [int]
        |     (block
        |       (def-val s 0)
        |       (for i values
        |         (assign s (+ s i))
        |       )
        |       s
        |     )
        |   )
        |   (sum (range 1 101))
        |)
        |""".stripMargin)
      .asInstanceOf[Block]
    val fl = FlowAnalysis()
    val ctx = fl.analyze(blk)
    assertEquals(true, ctx.isReachable(blk.children(0), LiteralUnknown()))
    assertEquals(true, ctx.isReachable(blk.children(1), LiteralUnknown()))
    assertEquals(
      true,
      fl.contextHistory(blk)
        .isReachable(blk.children(0).asInstanceOf[DefFun].body, LiteralUnknown())
    )
    assertEquals(
      true,
      fl.contextHistory(blk)
        .isReachable(
          blk.children(0).asInstanceOf[DefFun].body.asInstanceOf[Block].children(1),
          LiteralUnknown()
        )
    )

  }
  @Test def testComplexFor(): Unit = {
    val blk = TestHelper
      .parseCode(
        """
        |
        |  (block
        |    (def-val values (range 1 101 ))
        |    (def-val s 0)
        |    (for i values
        |        (assign s (+ s i ))
        |    )
        |    s
        |  )
        |""".stripMargin
      )
      .asInstanceOf[Block]
    val fl = FlowAnalysis()
    val ctx = fl.analyze(blk)

    for (x <- blk.children) {
      assertEquals(true, ctx.isReachable(x, LiteralUnknown()))

    }

  }
}
