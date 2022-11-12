// Generated from java-escape by ANTLR 4.11.1
package shll.frontends;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link SHLLParser}.
 */
public interface SHLLListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link SHLLParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterLiteral(SHLLParser.LiteralContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitLiteral(SHLLParser.LiteralContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#blocked}.
	 * @param ctx the parse tree
	 */
	void enterBlocked(SHLLParser.BlockedContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#blocked}.
	 * @param ctx the parse tree
	 */
	void exitBlocked(SHLLParser.BlockedContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#block}.
	 * @param ctx the parse tree
	 */
	void enterBlock(SHLLParser.BlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#block}.
	 * @param ctx the parse tree
	 */
	void exitBlock(SHLLParser.BlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#anno}.
	 * @param ctx the parse tree
	 */
	void enterAnno(SHLLParser.AnnoContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#anno}.
	 * @param ctx the parse tree
	 */
	void exitAnno(SHLLParser.AnnoContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#default}.
	 * @param ctx the parse tree
	 */
	void enterDefault(SHLLParser.DefaultContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#default}.
	 * @param ctx the parse tree
	 */
	void exitDefault(SHLLParser.DefaultContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#param}.
	 * @param ctx the parse tree
	 */
	void enterParam(SHLLParser.ParamContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#param}.
	 * @param ctx the parse tree
	 */
	void exitParam(SHLLParser.ParamContext ctx);
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
	 * Enter a parse tree produced by {@link SHLLParser#posArg}.
	 * @param ctx the parse tree
	 */
	void enterPosArg(SHLLParser.PosArgContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#posArg}.
	 * @param ctx the parse tree
	 */
	void exitPosArg(SHLLParser.PosArgContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#arg}.
	 * @param ctx the parse tree
	 */
	void enterArg(SHLLParser.ArgContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#arg}.
	 * @param ctx the parse tree
	 */
	void exitArg(SHLLParser.ArgContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#let}.
	 * @param ctx the parse tree
	 */
	void enterLet(SHLLParser.LetContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#let}.
	 * @param ctx the parse tree
	 */
	void exitLet(SHLLParser.LetContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#for}.
	 * @param ctx the parse tree
	 */
	void enterFor(SHLLParser.ForContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#for}.
	 * @param ctx the parse tree
	 */
	void exitFor(SHLLParser.ForContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#structof}.
	 * @param ctx the parse tree
	 */
	void enterStructof(SHLLParser.StructofContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#structof}.
	 * @param ctx the parse tree
	 */
	void exitStructof(SHLLParser.StructofContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#struct}.
	 * @param ctx the parse tree
	 */
	void enterStruct(SHLLParser.StructContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#struct}.
	 * @param ctx the parse tree
	 */
	void exitStruct(SHLLParser.StructContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#enumof}.
	 * @param ctx the parse tree
	 */
	void enterEnumof(SHLLParser.EnumofContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#enumof}.
	 * @param ctx the parse tree
	 */
	void exitEnumof(SHLLParser.EnumofContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#enum}.
	 * @param ctx the parse tree
	 */
	void enterEnum(SHLLParser.EnumContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#enum}.
	 * @param ctx the parse tree
	 */
	void exitEnum(SHLLParser.EnumContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#traitof}.
	 * @param ctx the parse tree
	 */
	void enterTraitof(SHLLParser.TraitofContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#traitof}.
	 * @param ctx the parse tree
	 */
	void exitTraitof(SHLLParser.TraitofContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#trait}.
	 * @param ctx the parse tree
	 */
	void enterTrait(SHLLParser.TraitContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#trait}.
	 * @param ctx the parse tree
	 */
	void exitTrait(SHLLParser.TraitContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#funof}.
	 * @param ctx the parse tree
	 */
	void enterFunof(SHLLParser.FunofContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#funof}.
	 * @param ctx the parse tree
	 */
	void exitFunof(SHLLParser.FunofContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#fun}.
	 * @param ctx the parse tree
	 */
	void enterFun(SHLLParser.FunContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#fun}.
	 * @param ctx the parse tree
	 */
	void exitFun(SHLLParser.FunContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#kindof}.
	 * @param ctx the parse tree
	 */
	void enterKindof(SHLLParser.KindofContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#kindof}.
	 * @param ctx the parse tree
	 */
	void exitKindof(SHLLParser.KindofContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#kind}.
	 * @param ctx the parse tree
	 */
	void enterKind(SHLLParser.KindContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#kind}.
	 * @param ctx the parse tree
	 */
	void exitKind(SHLLParser.KindContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#when}.
	 * @param ctx the parse tree
	 */
	void enterWhen(SHLLParser.WhenContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#when}.
	 * @param ctx the parse tree
	 */
	void exitWhen(SHLLParser.WhenContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#case}.
	 * @param ctx the parse tree
	 */
	void enterCase(SHLLParser.CaseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#case}.
	 * @param ctx the parse tree
	 */
	void exitCase(SHLLParser.CaseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#generic}.
	 * @param ctx the parse tree
	 */
	void enterGeneric(SHLLParser.GenericContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#generic}.
	 * @param ctx the parse tree
	 */
	void exitGeneric(SHLLParser.GenericContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#selector}.
	 * @param ctx the parse tree
	 */
	void enterSelector(SHLLParser.SelectorContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#selector}.
	 * @param ctx the parse tree
	 */
	void exitSelector(SHLLParser.SelectorContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#implicitApplier}.
	 * @param ctx the parse tree
	 */
	void enterImplicitApplier(SHLLParser.ImplicitApplierContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#implicitApplier}.
	 * @param ctx the parse tree
	 */
	void exitImplicitApplier(SHLLParser.ImplicitApplierContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#positionalApplier}.
	 * @param ctx the parse tree
	 */
	void enterPositionalApplier(SHLLParser.PositionalApplierContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#positionalApplier}.
	 * @param ctx the parse tree
	 */
	void exitPositionalApplier(SHLLParser.PositionalApplierContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#namedApplier}.
	 * @param ctx the parse tree
	 */
	void enterNamedApplier(SHLLParser.NamedApplierContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#namedApplier}.
	 * @param ctx the parse tree
	 */
	void exitNamedApplier(SHLLParser.NamedApplierContext ctx);
	/**
	 * Enter a parse tree produced by {@link SHLLParser#assigner}.
	 * @param ctx the parse tree
	 */
	void enterAssigner(SHLLParser.AssignerContext ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#assigner}.
	 * @param ctx the parse tree
	 */
	void exitAssigner(SHLLParser.AssignerContext ctx);
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
	 * Enter a parse tree produced by {@link SHLLParser#term1}.
	 * @param ctx the parse tree
	 */
	void enterTerm1(SHLLParser.Term1Context ctx);
	/**
	 * Exit a parse tree produced by {@link SHLLParser#term1}.
	 * @param ctx the parse tree
	 */
	void exitTerm1(SHLLParser.Term1Context ctx);
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