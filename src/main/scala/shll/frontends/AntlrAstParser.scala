package shll.frontends

import com.typesafe.scalalogging.Logger
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import shll.ast.*
import shll.*
import shll.frontends.SHLLParser.*

import scala.jdk.CollectionConverters.*

case class AntlrAstParser() {
  val logger: Logger = Logger(this.getClass)
  def convertBool(n: TerminalNode): Option[LiteralBool] = {
    n.getText match {
      case "true" => Some(LiteralBool(true).withToken(n.getSymbol))
      case "false" => Some(LiteralBool(false).withToken(n.getSymbol))
      case _ => None
    }
  }
  def convertChar(n: TerminalNode): LiteralChar = {
    val char = n.getText
    LiteralChar(char.charAt(1)).withToken(n.getSymbol)
  }

  def convertString(n: TerminalNode): LiteralString = {
    val string = n.getText
    LiteralString(string.substring(1, string.length - 1)).withToken(n.getSymbol)

  }
  def convertIdent(n: TerminalNode): Ident = {
    val ident = n.getText
    Ident(ident).withToken(n.getSymbol)
  }
  def convertInteger(n: TerminalNode): LiteralInt = {
    val integer = n.getText
    LiteralInt(
      integer.toIntOption.getOrElse(
        throw ParserException(s"Invalid integer at ${n.getSymbol}: $integer")
      )
    ).withToken(n.getSymbol)

  }

  def convertDecimal(n: TerminalNode): LiteralDecimal = {
    val decimal = n.getText
    LiteralDecimal(
      decimal.toDoubleOption.getOrElse(
        throw ParserException(s"Invalid decimal at ${n.getSymbol}: $decimal")
      ),

    ).withToken(n.getSymbol)

  }

  def convertPosArgs(ctx: PosArgsContext): PosArgs = {
    PosArgs(ctx.term().asScala.map(convertTerm).toList).withToken(ctx.start)
  }

  def convertKwArg(ctx: KwArgContext): KeyValue = {
    val ident = convertIdent(ctx.IDENT())
    val expr = convertTerm(ctx.term())
    KeyValue(ident, expr).withToken(ctx.getStart)
  }

  def convertKwArgs(ctx: KwArgsContext): KwArgs = {
    KwArgs(ctx.kwArg().asScala.toList.map(convertKwArg)).withToken(ctx.start)
  }
  def convertApply(ctx: ApplyContext): Apply = {
    Apply(convertTerm(ctx.term()), convertPosArgs(ctx.posArgs()), convertKwArgs(ctx.kwArgs()))
      .withToken(ctx.getStart)
  }

  def convertApplyType(ctx: ApplyTypeContext): ApplyType = {
    ApplyType(convertTerm(ctx.term()), convertPosArgs(ctx.posArgs()), convertKwArgs(ctx.kwArgs()))
      .withToken(ctx.getStart)
  }
  def convertTerm(ctx: TermContext): AST = {
//    logger.debug(s"Converting term: ${ctx.getText}")
    ctx match {
      case _ if ctx.CHAR() != null =>
        convertChar(ctx.CHAR())
      case _ if ctx.BOOL() != null =>
        convertBool(ctx.BOOL()).get
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
      case _ if ctx.applyType() != null =>
        convertApplyType(ctx.applyType())

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
