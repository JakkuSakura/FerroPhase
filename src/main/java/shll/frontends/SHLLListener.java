// Generated from java-escape by ANTLR 4.11.1
package shll.frontends;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link SHLLParser}.
 */
public interface SHLLListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link SHLLParser#term}.
	 * @param ctx the parse tree
	 */
	void enterTerm(SHLLParser.TermContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#term}.
	 * @param ctx the parse tree
	 */
	void exitTerm(SHLLParser.TermContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#kwArg}.
	 * @param ctx the parse tree
	 */
	void enterKwArg(SHLLParser.KwArgContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#kwArg}.
	 * @param ctx the parse tree
	 */
	void exitKwArg(SHLLParser.KwArgContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#kwArgs}.
	 * @param ctx the parse tree
	 */
	void enterKwArgs(SHLLParser.KwArgsContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#kwArgs}.
	 * @param ctx the parse tree
	 */
	void exitKwArgs(SHLLParser.KwArgsContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#posArgs}.
	 * @param ctx the parse tree
	 */
	void enterPosArgs(SHLLParser.PosArgsContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#posArgs}.
	 * @param ctx the parse tree
	 */
	void exitPosArgs(SHLLParser.PosArgsContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#apply}.
	 * @param ctx the parse tree
	 */
	void enterApply(SHLLParser.ApplyContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#apply}.
	 * @param ctx the parse tree
	 */
	void exitApply(SHLLParser.ApplyContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#applyType}.
	 * @param ctx the parse tree
	 */
	void enterApplyType(SHLLParser.ApplyTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#applyType}.
	 * @param ctx the parse tree
	 */
	void exitApplyType(SHLLParser.ApplyTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(SHLLParser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(SHLLParser.ProgramContext ctx);
}