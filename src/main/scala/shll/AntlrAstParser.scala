package shll

import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import shll.SHLLParser.*

import scala.jdk.CollectionConverters.*

case class AntlrAstParser() {
  def convertChar(ctx: TerminalNode): LiteralChar = {
    val char = ctx.getText
    LiteralChar(char.charAt(1), char)
  }

  def convertString(ctx: TerminalNode): LiteralString = {
    val string = ctx.getText
    LiteralString(string.substring(1, string.length - 1), string)

  }
  def convertIdent(ctx: TerminalNode): Ident = {
    val ident = ctx.getText
    Ident(ident)
  }
  def convertInteger(ctx: TerminalNode): LiteralInt = {
    val integer = ctx.getText
    LiteralInt(
      integer.toIntOption.getOrElse(
        throw IllegalArgumentException(s"Invalid integer at ${ctx.getSymbol}: $integer")
      ),
      integer
    )

  }

  def convertDecimal(ctx: TerminalNode): LiteralDecimal = {
    val decimal = ctx.getText
    LiteralDecimal(
      decimal.toDoubleOption.getOrElse(
        throw IllegalArgumentException(s"Invalid decimal at ${ctx.getSymbol}: $decimal")
      ),
      decimal
    )

  }

  def convertPosArgs(ctx: PosArgsContext): List[AST] = {
    ctx.term().asScala.map(convertTerm).toList
  }

  def convertKwArg(ctx: KwArgContext): KeyValue = {
    val ident = convertIdent(ctx.IDENT())
    val expr = convertTerm(ctx.term())
    KeyValue(ident, expr)
  }

  def convertKwArgs(ctx: KwArgsContext): List[KeyValue] = {
    ctx.kwArg().asScala.toList.map(convertKwArg)
  }
  def convertApply(ctx: ApplyContext): Apply = {
    Apply(convertTerm(ctx.term()), convertPosArgs(ctx.posArgs()), convertKwArgs(ctx.kwArgs()))
  }

  def convertTypeApply(ctx: ApplyContext): TypeApply = {
    TypeApply(convertTerm(ctx.term()), convertPosArgs(ctx.posArgs()), convertKwArgs(ctx.kwArgs()))
  }
  def convertTerm(ctx: TermContext): AST = {
    ctx match {
      case _ if ctx.CHAR() != null =>
        convertChar(ctx.CHAR())
      case _ if ctx.IDENT() != null =>
        convertIdent(ctx.IDENT())
      case _ if ctx.INTEGER() != null =>
        convertInteger(ctx.INTEGER())
      case _ if ctx.DECIMAL() != null =>
        convertDecimal(ctx.DECIMAL())
      case _ if ctx.STRING() != null =>
        convertString(ctx.STRING())
      case _ if ctx.apply() != null =>
        convertApply(ctx.apply())
      case _ if ctx.typeApply() != null =>
        convertTypeApply(ctx.apply())
    }
  }
  def parse(s: String): AST = {
    val lexer = SHLLLexer(CharStreams.fromString(s))
    val stream = CommonTokenStream(lexer)
    val parser = SHLLParser(stream)
    val term = parser.program()
    convertTerm(term.term())
  }
}
