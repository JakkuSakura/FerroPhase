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
      )
    ).withToken(n.getSymbol)

  }

  def convertTerm(ctx: TermContext): Ast = {
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
      case _ => ???

    }
  }
  def parse(s: String): Ast = {
    val lexer = SHLLLexer(CharStreams.fromString(s))
    val stream = CommonTokenStream(lexer)
    val parser = SHLLParser(stream)
    val term = parser.program()
    Block(term.term().asScala.map(convertTerm).toList)
  }
}
