// Generated from java-escape by ANTLR 4.11.1
package shll.frontends;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class SHLLParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.11.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, T__16=17, 
		T__17=18, T__18=19, T__19=20, T__20=21, T__21=22, T__22=23, T__23=24, 
		T__24=25, T__25=26, T__26=27, BOOL=28, IDENT=29, INTEGER=30, DECIMAL=31, 
		STRING=32, CHAR=33, WS=34, COMMENT=35, LINE_COMMENT=36;
	public static final int
		RULE_program = 0, RULE_blocked = 1, RULE_block = 2, RULE_anno = 3, RULE_param = 4, 
		RULE_kwArg = 5, RULE_posArg = 6, RULE_arg = 7, RULE_let = 8, RULE_for = 9, 
		RULE_structof = 10, RULE_struct = 11, RULE_enumof = 12, RULE_enum = 13, 
		RULE_traitof = 14, RULE_trait = 15, RULE_funof = 16, RULE_fatArrow = 17, 
		RULE_fun = 18, RULE_kindof = 19, RULE_kind = 20, RULE_when = 21, RULE_case = 22, 
		RULE_generic = 23, RULE_deref = 24, RULE_selector = 25, RULE_implicitApplier = 26, 
		RULE_positionalApplier = 27, RULE_namedApplier = 28, RULE_assigner = 29, 
		RULE_term = 30;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "blocked", "block", "anno", "param", "kwArg", "posArg", "arg", 
			"let", "for", "structof", "struct", "enumof", "enum", "traitof", "trait", 
			"funof", "fatArrow", "fun", "kindof", "kind", "when", "case", "generic", 
			"deref", "selector", "implicitApplier", "positionalApplier", "namedApplier", 
			"assigner", "term"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'{'", "'}'", "'block'", "':'", "'*'", "'**'", "'='", "'let'", 
			"'for'", "'in'", "'structof'", "'struct'", "'enumof'", "'enum'", "'traitof'", 
			"'trait'", "'('", "')'", "'->'", "'=>'", "'kindof'", "'kind'", "'when'", 
			"'case'", "'['", "']'", "'.'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, "BOOL", "IDENT", "INTEGER", "DECIMAL", "STRING", 
			"CHAR", "WS", "COMMENT", "LINE_COMMENT"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "java-escape"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public SHLLParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProgramContext extends ParserRuleContext {
		public TerminalNode EOF() { return getToken(SHLLParser.EOF, 0); }
		public List<TermContext> term() {
			return getRuleContexts(TermContext.class);
		}
		public TermContext term(int i) {
			return getRuleContext(TermContext.class,i);
		}
		public ProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_program; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterProgram(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitProgram(this);
		}
	}

	public final ProgramContext program() throws RecognitionException {
		ProgramContext _localctx = new ProgramContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_program);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(65);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (((_la) & ~0x3f) == 0 && ((1L << _la) & 16968317800L) != 0) {
				{
				{
				setState(62);
				term();
				}
				}
				setState(67);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(68);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class BlockedContext extends ParserRuleContext {
		public List<TermContext> term() {
			return getRuleContexts(TermContext.class);
		}
		public TermContext term(int i) {
			return getRuleContext(TermContext.class,i);
		}
		public BlockedContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_blocked; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterBlocked(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitBlocked(this);
		}
	}

	public final BlockedContext blocked() throws RecognitionException {
		BlockedContext _localctx = new BlockedContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_blocked);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(70);
			match(T__0);
			setState(74);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (((_la) & ~0x3f) == 0 && ((1L << _la) & 16968317800L) != 0) {
				{
				{
				setState(71);
				term();
				}
				}
				setState(76);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(77);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class BlockContext extends ParserRuleContext {
		public BlockedContext blocked() {
			return getRuleContext(BlockedContext.class,0);
		}
		public BlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_block; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitBlock(this);
		}
	}

	public final BlockContext block() throws RecognitionException {
		BlockContext _localctx = new BlockContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_block);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(79);
			match(T__2);
			setState(80);
			blocked();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AnnoContext extends ParserRuleContext {
		public AnnoContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_anno; }
	 
		public AnnoContext() { }
		public void copyFrom(AnnoContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class SingleContext extends AnnoContext {
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public SingleContext(AnnoContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterSingle(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitSingle(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class DictContext extends AnnoContext {
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public DictContext(AnnoContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterDict(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitDict(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class ListContext extends AnnoContext {
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public ListContext(AnnoContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitList(this);
		}
	}

	public final AnnoContext anno() throws RecognitionException {
		AnnoContext _localctx = new AnnoContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_anno);
		try {
			setState(90);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,2,_ctx) ) {
			case 1:
				_localctx = new SingleContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(82);
				match(T__3);
				setState(83);
				term();
				}
				break;
			case 2:
				_localctx = new ListContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(84);
				match(T__3);
				setState(85);
				match(T__4);
				setState(86);
				term();
				}
				break;
			case 3:
				_localctx = new DictContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(87);
				match(T__3);
				setState(88);
				match(T__5);
				setState(89);
				term();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ParamContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public AnnoContext anno() {
			return getRuleContext(AnnoContext.class,0);
		}
		public ParamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_param; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterParam(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitParam(this);
		}
	}

	public final ParamContext param() throws RecognitionException {
		ParamContext _localctx = new ParamContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_param);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(92);
			match(IDENT);
			setState(94);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==T__3) {
				{
				setState(93);
				anno();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class KwArgContext extends ParserRuleContext {
		public KwArgContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_kwArg; }
	 
		public KwArgContext() { }
		public void copyFrom(KwArgContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class WithRenameContext extends KwArgContext {
		public TermContext value;
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public AnnoContext anno() {
			return getRuleContext(AnnoContext.class,0);
		}
		public WithRenameContext(KwArgContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterWithRename(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitWithRename(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class WithoutRenameContext extends KwArgContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public WithoutRenameContext(KwArgContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterWithoutRename(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitWithoutRename(this);
		}
	}

	public final KwArgContext kwArg() throws RecognitionException {
		KwArgContext _localctx = new KwArgContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_kwArg);
		int _la;
		try {
			setState(103);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,5,_ctx) ) {
			case 1:
				_localctx = new WithRenameContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(96);
				match(IDENT);
				setState(98);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==T__3) {
					{
					setState(97);
					anno();
					}
				}

				setState(100);
				match(T__6);
				setState(101);
				((WithRenameContext)_localctx).value = term();
				}
				break;
			case 2:
				_localctx = new WithoutRenameContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(102);
				match(IDENT);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PosArgContext extends ParserRuleContext {
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public PosArgContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_posArg; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterPosArg(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitPosArg(this);
		}
	}

	public final PosArgContext posArg() throws RecognitionException {
		PosArgContext _localctx = new PosArgContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_posArg);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(105);
			term();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ArgContext extends ParserRuleContext {
		public KwArgContext kwArg() {
			return getRuleContext(KwArgContext.class,0);
		}
		public PosArgContext posArg() {
			return getRuleContext(PosArgContext.class,0);
		}
		public ArgContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arg; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterArg(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitArg(this);
		}
	}

	public final ArgContext arg() throws RecognitionException {
		ArgContext _localctx = new ArgContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_arg);
		try {
			setState(109);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(107);
				kwArg();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(108);
				posArg();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LetContext extends ParserRuleContext {
		public LetContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_let; }
	 
		public LetContext() { }
		public void copyFrom(LetContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class UninitializedContext extends LetContext {
		public Token id;
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public UninitializedContext(LetContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterUninitialized(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitUninitialized(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class IntializedContext extends LetContext {
		public Token id;
		public TermContext value;
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public AnnoContext anno() {
			return getRuleContext(AnnoContext.class,0);
		}
		public IntializedContext(LetContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterIntialized(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitIntialized(this);
		}
	}

	public final LetContext let() throws RecognitionException {
		LetContext _localctx = new LetContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_let);
		int _la;
		try {
			setState(120);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,8,_ctx) ) {
			case 1:
				_localctx = new IntializedContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(111);
				match(T__7);
				setState(112);
				((IntializedContext)_localctx).id = match(IDENT);
				setState(114);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==T__3) {
					{
					setState(113);
					anno();
					}
				}

				setState(116);
				match(T__6);
				setState(117);
				((IntializedContext)_localctx).value = term();
				}
				break;
			case 2:
				_localctx = new UninitializedContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(118);
				match(T__7);
				setState(119);
				((UninitializedContext)_localctx).id = match(IDENT);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ForContext extends ParserRuleContext {
		public ForContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_for; }
	 
		public ForContext() { }
		public void copyFrom(ForContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class LoopContext extends ForContext {
		public BlockedContext blocked() {
			return getRuleContext(BlockedContext.class,0);
		}
		public LoopContext(ForContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterLoop(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitLoop(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class WhileContext extends ForContext {
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public BlockedContext blocked() {
			return getRuleContext(BlockedContext.class,0);
		}
		public WhileContext(ForContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterWhile(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitWhile(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class ForEachContext extends ForContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public BlockedContext blocked() {
			return getRuleContext(BlockedContext.class,0);
		}
		public ForEachContext(ForContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterForEach(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitForEach(this);
		}
	}

	public final ForContext for_() throws RecognitionException {
		ForContext _localctx = new ForContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_for);
		try {
			setState(134);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,9,_ctx) ) {
			case 1:
				_localctx = new ForEachContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(122);
				match(T__8);
				setState(123);
				match(IDENT);
				setState(124);
				match(T__9);
				setState(125);
				term();
				setState(126);
				blocked();
				}
				break;
			case 2:
				_localctx = new LoopContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(128);
				match(T__8);
				setState(129);
				blocked();
				}
				break;
			case 3:
				_localctx = new WhileContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(130);
				match(T__8);
				setState(131);
				term();
				setState(132);
				blocked();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class StructofContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public List<ParamContext> param() {
			return getRuleContexts(ParamContext.class);
		}
		public ParamContext param(int i) {
			return getRuleContext(ParamContext.class,i);
		}
		public StructofContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_structof; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterStructof(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitStructof(this);
		}
	}

	public final StructofContext structof() throws RecognitionException {
		StructofContext _localctx = new StructofContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_structof);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(136);
			match(T__10);
			setState(138);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==IDENT) {
				{
				setState(137);
				match(IDENT);
				}
			}

			setState(140);
			match(T__0);
			setState(144);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(141);
				param();
				}
				}
				setState(146);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(147);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class StructContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public List<KwArgContext> kwArg() {
			return getRuleContexts(KwArgContext.class);
		}
		public KwArgContext kwArg(int i) {
			return getRuleContext(KwArgContext.class,i);
		}
		public StructContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_struct; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterStruct(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitStruct(this);
		}
	}

	public final StructContext struct() throws RecognitionException {
		StructContext _localctx = new StructContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_struct);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(149);
			match(T__11);
			setState(151);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==IDENT) {
				{
				setState(150);
				match(IDENT);
				}
			}

			setState(153);
			match(T__0);
			setState(157);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(154);
				kwArg();
				}
				}
				setState(159);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(160);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class EnumofContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public List<ParamContext> param() {
			return getRuleContexts(ParamContext.class);
		}
		public ParamContext param(int i) {
			return getRuleContext(ParamContext.class,i);
		}
		public EnumofContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_enumof; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterEnumof(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitEnumof(this);
		}
	}

	public final EnumofContext enumof() throws RecognitionException {
		EnumofContext _localctx = new EnumofContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_enumof);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(162);
			match(T__12);
			setState(164);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==IDENT) {
				{
				setState(163);
				match(IDENT);
				}
			}

			setState(166);
			match(T__0);
			setState(170);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(167);
				param();
				}
				}
				setState(172);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(173);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class EnumContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public List<KwArgContext> kwArg() {
			return getRuleContexts(KwArgContext.class);
		}
		public KwArgContext kwArg(int i) {
			return getRuleContext(KwArgContext.class,i);
		}
		public EnumContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_enum; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterEnum(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitEnum(this);
		}
	}

	public final EnumContext enum_() throws RecognitionException {
		EnumContext _localctx = new EnumContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_enum);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(175);
			match(T__13);
			setState(177);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==IDENT) {
				{
				setState(176);
				match(IDENT);
				}
			}

			setState(179);
			match(T__0);
			setState(183);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(180);
				kwArg();
				}
				}
				setState(185);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(186);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TraitofContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public List<LetContext> let() {
			return getRuleContexts(LetContext.class);
		}
		public LetContext let(int i) {
			return getRuleContext(LetContext.class,i);
		}
		public TraitofContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_traitof; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterTraitof(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitTraitof(this);
		}
	}

	public final TraitofContext traitof() throws RecognitionException {
		TraitofContext _localctx = new TraitofContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_traitof);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(188);
			match(T__14);
			setState(190);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==IDENT) {
				{
				setState(189);
				match(IDENT);
				}
			}

			setState(192);
			match(T__0);
			setState(196);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__7) {
				{
				{
				setState(193);
				let();
				}
				}
				setState(198);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(199);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TraitContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public List<LetContext> let() {
			return getRuleContexts(LetContext.class);
		}
		public LetContext let(int i) {
			return getRuleContext(LetContext.class,i);
		}
		public TraitContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_trait; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterTrait(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitTrait(this);
		}
	}

	public final TraitContext trait() throws RecognitionException {
		TraitContext _localctx = new TraitContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_trait);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(201);
			match(T__15);
			setState(203);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==IDENT) {
				{
				setState(202);
				match(IDENT);
				}
			}

			setState(205);
			match(T__0);
			setState(209);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__7) {
				{
				{
				setState(206);
				let();
				}
				}
				setState(211);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(212);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class FunofContext extends ParserRuleContext {
		public List<TermContext> term() {
			return getRuleContexts(TermContext.class);
		}
		public TermContext term(int i) {
			return getRuleContext(TermContext.class,i);
		}
		public FunofContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funof; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterFunof(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitFunof(this);
		}
	}

	public final FunofContext funof() throws RecognitionException {
		FunofContext _localctx = new FunofContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_funof);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(214);
			match(T__16);
			setState(218);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (((_la) & ~0x3f) == 0 && ((1L << _la) & 16968317800L) != 0) {
				{
				{
				setState(215);
				term();
				}
				}
				setState(220);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(221);
			match(T__17);
			setState(222);
			match(T__18);
			setState(223);
			term();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class FatArrowContext extends ParserRuleContext {
		public BlockedContext blocked() {
			return getRuleContext(BlockedContext.class,0);
		}
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public FatArrowContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_fatArrow; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterFatArrow(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitFatArrow(this);
		}
	}

	public final FatArrowContext fatArrow() throws RecognitionException {
		FatArrowContext _localctx = new FatArrowContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_fatArrow);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(225);
			match(T__19);
			setState(228);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__0:
				{
				setState(226);
				blocked();
				}
				break;
			case T__2:
			case T__4:
			case T__5:
			case T__7:
			case T__8:
			case T__10:
			case T__11:
			case T__12:
			case T__13:
			case T__14:
			case T__15:
			case T__16:
			case T__20:
			case T__21:
			case T__23:
			case T__24:
			case BOOL:
			case IDENT:
			case INTEGER:
			case DECIMAL:
			case STRING:
			case CHAR:
				{
				setState(227);
				term();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class FunContext extends ParserRuleContext {
		public TermContext ret;
		public FatArrowContext fatArrow() {
			return getRuleContext(FatArrowContext.class,0);
		}
		public List<ParamContext> param() {
			return getRuleContexts(ParamContext.class);
		}
		public ParamContext param(int i) {
			return getRuleContext(ParamContext.class,i);
		}
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public FunContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_fun; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterFun(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitFun(this);
		}
	}

	public final FunContext fun() throws RecognitionException {
		FunContext _localctx = new FunContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_fun);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(230);
			match(T__16);
			setState(234);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(231);
				param();
				}
				}
				setState(236);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(237);
			match(T__17);
			setState(240);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==T__18) {
				{
				setState(238);
				match(T__18);
				setState(239);
				((FunContext)_localctx).ret = term();
				}
			}

			setState(242);
			fatArrow();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class KindofContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public List<ParamContext> param() {
			return getRuleContexts(ParamContext.class);
		}
		public ParamContext param(int i) {
			return getRuleContext(ParamContext.class,i);
		}
		public KindofContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_kindof; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterKindof(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitKindof(this);
		}
	}

	public final KindofContext kindof() throws RecognitionException {
		KindofContext _localctx = new KindofContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_kindof);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(244);
			match(T__20);
			setState(246);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==IDENT) {
				{
				setState(245);
				match(IDENT);
				}
			}

			setState(248);
			match(T__0);
			setState(252);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(249);
				param();
				}
				}
				setState(254);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(255);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class KindContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public List<KwArgContext> kwArg() {
			return getRuleContexts(KwArgContext.class);
		}
		public KwArgContext kwArg(int i) {
			return getRuleContext(KwArgContext.class,i);
		}
		public KindContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_kind; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterKind(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitKind(this);
		}
	}

	public final KindContext kind() throws RecognitionException {
		KindContext _localctx = new KindContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_kind);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(257);
			match(T__21);
			setState(259);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==IDENT) {
				{
				setState(258);
				match(IDENT);
				}
			}

			setState(261);
			match(T__0);
			setState(265);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(262);
				kwArg();
				}
				}
				setState(267);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(268);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class WhenContext extends ParserRuleContext {
		public TermContext cond;
		public FatArrowContext body;
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public FatArrowContext fatArrow() {
			return getRuleContext(FatArrowContext.class,0);
		}
		public WhenContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_when; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterWhen(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitWhen(this);
		}
	}

	public final WhenContext when() throws RecognitionException {
		WhenContext _localctx = new WhenContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_when);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(270);
			match(T__22);
			setState(271);
			((WhenContext)_localctx).cond = term();
			setState(272);
			((WhenContext)_localctx).body = fatArrow();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class CaseContext extends ParserRuleContext {
		public List<WhenContext> when() {
			return getRuleContexts(WhenContext.class);
		}
		public WhenContext when(int i) {
			return getRuleContext(WhenContext.class,i);
		}
		public CaseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_case; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterCase(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitCase(this);
		}
	}

	public final CaseContext case_() throws RecognitionException {
		CaseContext _localctx = new CaseContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_case);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(274);
			match(T__23);
			setState(275);
			match(T__0);
			setState(279);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__22) {
				{
				{
				setState(276);
				when();
				}
				}
				setState(281);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(282);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class GenericContext extends ParserRuleContext {
		public FatArrowContext body;
		public FatArrowContext fatArrow() {
			return getRuleContext(FatArrowContext.class,0);
		}
		public List<ParamContext> param() {
			return getRuleContexts(ParamContext.class);
		}
		public ParamContext param(int i) {
			return getRuleContext(ParamContext.class,i);
		}
		public GenericContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_generic; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterGeneric(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitGeneric(this);
		}
	}

	public final GenericContext generic() throws RecognitionException {
		GenericContext _localctx = new GenericContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_generic);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(284);
			match(T__24);
			setState(288);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(285);
				param();
				}
				}
				setState(290);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(291);
			match(T__25);
			setState(292);
			((GenericContext)_localctx).body = fatArrow();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DerefContext extends ParserRuleContext {
		public DerefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_deref; }
	 
		public DerefContext() { }
		public void copyFrom(DerefContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class DerefTupleContext extends DerefContext {
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public DerefTupleContext(DerefContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterDerefTuple(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitDerefTuple(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class DerefDictContext extends DerefContext {
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public DerefDictContext(DerefContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterDerefDict(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitDerefDict(this);
		}
	}

	public final DerefContext deref() throws RecognitionException {
		DerefContext _localctx = new DerefContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_deref);
		try {
			setState(298);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__4:
				_localctx = new DerefTupleContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(294);
				match(T__4);
				setState(295);
				term();
				}
				break;
			case T__5:
				_localctx = new DerefDictContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(296);
				match(T__5);
				setState(297);
				term();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class SelectorContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public SelectorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selector; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterSelector(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitSelector(this);
		}
	}

	public final SelectorContext selector() throws RecognitionException {
		SelectorContext _localctx = new SelectorContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_selector);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(300);
			match(T__26);
			setState(301);
			match(IDENT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ImplicitApplierContext extends ParserRuleContext {
		public List<ArgContext> arg() {
			return getRuleContexts(ArgContext.class);
		}
		public ArgContext arg(int i) {
			return getRuleContext(ArgContext.class,i);
		}
		public ImplicitApplierContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_implicitApplier; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterImplicitApplier(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitImplicitApplier(this);
		}
	}

	public final ImplicitApplierContext implicitApplier() throws RecognitionException {
		ImplicitApplierContext _localctx = new ImplicitApplierContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_implicitApplier);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(303);
			match(T__24);
			setState(307);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (((_la) & ~0x3f) == 0 && ((1L << _la) & 16968317800L) != 0) {
				{
				{
				setState(304);
				arg();
				}
				}
				setState(309);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(310);
			match(T__25);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PositionalApplierContext extends ParserRuleContext {
		public List<ArgContext> arg() {
			return getRuleContexts(ArgContext.class);
		}
		public ArgContext arg(int i) {
			return getRuleContext(ArgContext.class,i);
		}
		public PositionalApplierContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_positionalApplier; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterPositionalApplier(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitPositionalApplier(this);
		}
	}

	public final PositionalApplierContext positionalApplier() throws RecognitionException {
		PositionalApplierContext _localctx = new PositionalApplierContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_positionalApplier);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(312);
			match(T__16);
			setState(316);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (((_la) & ~0x3f) == 0 && ((1L << _la) & 16968317800L) != 0) {
				{
				{
				setState(313);
				arg();
				}
				}
				setState(318);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(319);
			match(T__17);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class NamedApplierContext extends ParserRuleContext {
		public List<KwArgContext> kwArg() {
			return getRuleContexts(KwArgContext.class);
		}
		public KwArgContext kwArg(int i) {
			return getRuleContext(KwArgContext.class,i);
		}
		public NamedApplierContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_namedApplier; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterNamedApplier(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitNamedApplier(this);
		}
	}

	public final NamedApplierContext namedApplier() throws RecognitionException {
		NamedApplierContext _localctx = new NamedApplierContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_namedApplier);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(321);
			match(T__0);
			setState(325);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(322);
				kwArg();
				}
				}
				setState(327);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(328);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AssignerContext extends ParserRuleContext {
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public AssignerContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_assigner; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterAssigner(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitAssigner(this);
		}
	}

	public final AssignerContext assigner() throws RecognitionException {
		AssignerContext _localctx = new AssignerContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_assigner);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(330);
			match(T__6);
			setState(331);
			term();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TermContext extends ParserRuleContext {
		public BlockContext block() {
			return getRuleContext(BlockContext.class,0);
		}
		public GenericContext generic() {
			return getRuleContext(GenericContext.class,0);
		}
		public LetContext let() {
			return getRuleContext(LetContext.class,0);
		}
		public ForContext for_() {
			return getRuleContext(ForContext.class,0);
		}
		public StructofContext structof() {
			return getRuleContext(StructofContext.class,0);
		}
		public StructContext struct() {
			return getRuleContext(StructContext.class,0);
		}
		public EnumofContext enumof() {
			return getRuleContext(EnumofContext.class,0);
		}
		public EnumContext enum_() {
			return getRuleContext(EnumContext.class,0);
		}
		public TraitofContext traitof() {
			return getRuleContext(TraitofContext.class,0);
		}
		public TraitContext trait() {
			return getRuleContext(TraitContext.class,0);
		}
		public FunofContext funof() {
			return getRuleContext(FunofContext.class,0);
		}
		public FunContext fun() {
			return getRuleContext(FunContext.class,0);
		}
		public KindofContext kindof() {
			return getRuleContext(KindofContext.class,0);
		}
		public KindContext kind() {
			return getRuleContext(KindContext.class,0);
		}
		public CaseContext case_() {
			return getRuleContext(CaseContext.class,0);
		}
		public DerefContext deref() {
			return getRuleContext(DerefContext.class,0);
		}
		public TerminalNode BOOL() { return getToken(SHLLParser.BOOL, 0); }
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public TerminalNode INTEGER() { return getToken(SHLLParser.INTEGER, 0); }
		public TerminalNode DECIMAL() { return getToken(SHLLParser.DECIMAL, 0); }
		public TerminalNode STRING() { return getToken(SHLLParser.STRING, 0); }
		public TerminalNode CHAR() { return getToken(SHLLParser.CHAR, 0); }
		public List<SelectorContext> selector() {
			return getRuleContexts(SelectorContext.class);
		}
		public SelectorContext selector(int i) {
			return getRuleContext(SelectorContext.class,i);
		}
		public List<ImplicitApplierContext> implicitApplier() {
			return getRuleContexts(ImplicitApplierContext.class);
		}
		public ImplicitApplierContext implicitApplier(int i) {
			return getRuleContext(ImplicitApplierContext.class,i);
		}
		public List<PositionalApplierContext> positionalApplier() {
			return getRuleContexts(PositionalApplierContext.class);
		}
		public PositionalApplierContext positionalApplier(int i) {
			return getRuleContext(PositionalApplierContext.class,i);
		}
		public List<NamedApplierContext> namedApplier() {
			return getRuleContexts(NamedApplierContext.class);
		}
		public NamedApplierContext namedApplier(int i) {
			return getRuleContext(NamedApplierContext.class,i);
		}
		public List<AssignerContext> assigner() {
			return getRuleContexts(AssignerContext.class);
		}
		public AssignerContext assigner(int i) {
			return getRuleContext(AssignerContext.class,i);
		}
		public TermContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_term; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).enterTerm(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SHLLListener ) ((SHLLListener)listener).exitTerm(this);
		}
	}

	public final TermContext term() throws RecognitionException {
		TermContext _localctx = new TermContext(_ctx, getState());
		enterRule(_localctx, 60, RULE_term);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(355);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,36,_ctx) ) {
			case 1:
				{
				setState(333);
				block();
				}
				break;
			case 2:
				{
				setState(334);
				generic();
				}
				break;
			case 3:
				{
				setState(335);
				let();
				}
				break;
			case 4:
				{
				setState(336);
				for_();
				}
				break;
			case 5:
				{
				setState(337);
				structof();
				}
				break;
			case 6:
				{
				setState(338);
				struct();
				}
				break;
			case 7:
				{
				setState(339);
				enumof();
				}
				break;
			case 8:
				{
				setState(340);
				enum_();
				}
				break;
			case 9:
				{
				setState(341);
				traitof();
				}
				break;
			case 10:
				{
				setState(342);
				trait();
				}
				break;
			case 11:
				{
				setState(343);
				funof();
				}
				break;
			case 12:
				{
				setState(344);
				fun();
				}
				break;
			case 13:
				{
				setState(345);
				kindof();
				}
				break;
			case 14:
				{
				setState(346);
				kind();
				}
				break;
			case 15:
				{
				setState(347);
				case_();
				}
				break;
			case 16:
				{
				setState(348);
				deref();
				}
				break;
			case 17:
				{
				setState(349);
				match(BOOL);
				}
				break;
			case 18:
				{
				setState(350);
				match(IDENT);
				}
				break;
			case 19:
				{
				setState(351);
				match(INTEGER);
				}
				break;
			case 20:
				{
				setState(352);
				match(DECIMAL);
				}
				break;
			case 21:
				{
				setState(353);
				match(STRING);
				}
				break;
			case 22:
				{
				setState(354);
				match(CHAR);
				}
				break;
			}
			setState(364);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,38,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					setState(362);
					_errHandler.sync(this);
					switch (_input.LA(1)) {
					case T__26:
						{
						setState(357);
						selector();
						}
						break;
					case T__24:
						{
						setState(358);
						implicitApplier();
						}
						break;
					case T__16:
						{
						setState(359);
						positionalApplier();
						}
						break;
					case T__0:
						{
						setState(360);
						namedApplier();
						}
						break;
					case T__6:
						{
						setState(361);
						assigner();
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					} 
				}
				setState(366);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,38,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\u0004\u0001$\u0170\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007\u0002"+
		"\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b\u0002"+
		"\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007\u000f"+
		"\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011\u0002\u0012\u0007\u0012"+
		"\u0002\u0013\u0007\u0013\u0002\u0014\u0007\u0014\u0002\u0015\u0007\u0015"+
		"\u0002\u0016\u0007\u0016\u0002\u0017\u0007\u0017\u0002\u0018\u0007\u0018"+
		"\u0002\u0019\u0007\u0019\u0002\u001a\u0007\u001a\u0002\u001b\u0007\u001b"+
		"\u0002\u001c\u0007\u001c\u0002\u001d\u0007\u001d\u0002\u001e\u0007\u001e"+
		"\u0001\u0000\u0005\u0000@\b\u0000\n\u0000\f\u0000C\t\u0000\u0001\u0000"+
		"\u0001\u0000\u0001\u0001\u0001\u0001\u0005\u0001I\b\u0001\n\u0001\f\u0001"+
		"L\t\u0001\u0001\u0001\u0001\u0001\u0001\u0002\u0001\u0002\u0001\u0002"+
		"\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003"+
		"\u0001\u0003\u0001\u0003\u0003\u0003[\b\u0003\u0001\u0004\u0001\u0004"+
		"\u0003\u0004_\b\u0004\u0001\u0005\u0001\u0005\u0003\u0005c\b\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0003\u0005h\b\u0005\u0001\u0006\u0001"+
		"\u0006\u0001\u0007\u0001\u0007\u0003\u0007n\b\u0007\u0001\b\u0001\b\u0001"+
		"\b\u0003\bs\b\b\u0001\b\u0001\b\u0001\b\u0001\b\u0003\by\b\b\u0001\t\u0001"+
		"\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001"+
		"\t\u0001\t\u0003\t\u0087\b\t\u0001\n\u0001\n\u0003\n\u008b\b\n\u0001\n"+
		"\u0001\n\u0005\n\u008f\b\n\n\n\f\n\u0092\t\n\u0001\n\u0001\n\u0001\u000b"+
		"\u0001\u000b\u0003\u000b\u0098\b\u000b\u0001\u000b\u0001\u000b\u0005\u000b"+
		"\u009c\b\u000b\n\u000b\f\u000b\u009f\t\u000b\u0001\u000b\u0001\u000b\u0001"+
		"\f\u0001\f\u0003\f\u00a5\b\f\u0001\f\u0001\f\u0005\f\u00a9\b\f\n\f\f\f"+
		"\u00ac\t\f\u0001\f\u0001\f\u0001\r\u0001\r\u0003\r\u00b2\b\r\u0001\r\u0001"+
		"\r\u0005\r\u00b6\b\r\n\r\f\r\u00b9\t\r\u0001\r\u0001\r\u0001\u000e\u0001"+
		"\u000e\u0003\u000e\u00bf\b\u000e\u0001\u000e\u0001\u000e\u0005\u000e\u00c3"+
		"\b\u000e\n\u000e\f\u000e\u00c6\t\u000e\u0001\u000e\u0001\u000e\u0001\u000f"+
		"\u0001\u000f\u0003\u000f\u00cc\b\u000f\u0001\u000f\u0001\u000f\u0005\u000f"+
		"\u00d0\b\u000f\n\u000f\f\u000f\u00d3\t\u000f\u0001\u000f\u0001\u000f\u0001"+
		"\u0010\u0001\u0010\u0005\u0010\u00d9\b\u0010\n\u0010\f\u0010\u00dc\t\u0010"+
		"\u0001\u0010\u0001\u0010\u0001\u0010\u0001\u0010\u0001\u0011\u0001\u0011"+
		"\u0001\u0011\u0003\u0011\u00e5\b\u0011\u0001\u0012\u0001\u0012\u0005\u0012"+
		"\u00e9\b\u0012\n\u0012\f\u0012\u00ec\t\u0012\u0001\u0012\u0001\u0012\u0001"+
		"\u0012\u0003\u0012\u00f1\b\u0012\u0001\u0012\u0001\u0012\u0001\u0013\u0001"+
		"\u0013\u0003\u0013\u00f7\b\u0013\u0001\u0013\u0001\u0013\u0005\u0013\u00fb"+
		"\b\u0013\n\u0013\f\u0013\u00fe\t\u0013\u0001\u0013\u0001\u0013\u0001\u0014"+
		"\u0001\u0014\u0003\u0014\u0104\b\u0014\u0001\u0014\u0001\u0014\u0005\u0014"+
		"\u0108\b\u0014\n\u0014\f\u0014\u010b\t\u0014\u0001\u0014\u0001\u0014\u0001"+
		"\u0015\u0001\u0015\u0001\u0015\u0001\u0015\u0001\u0016\u0001\u0016\u0001"+
		"\u0016\u0005\u0016\u0116\b\u0016\n\u0016\f\u0016\u0119\t\u0016\u0001\u0016"+
		"\u0001\u0016\u0001\u0017\u0001\u0017\u0005\u0017\u011f\b\u0017\n\u0017"+
		"\f\u0017\u0122\t\u0017\u0001\u0017\u0001\u0017\u0001\u0017\u0001\u0018"+
		"\u0001\u0018\u0001\u0018\u0001\u0018\u0003\u0018\u012b\b\u0018\u0001\u0019"+
		"\u0001\u0019\u0001\u0019\u0001\u001a\u0001\u001a\u0005\u001a\u0132\b\u001a"+
		"\n\u001a\f\u001a\u0135\t\u001a\u0001\u001a\u0001\u001a\u0001\u001b\u0001"+
		"\u001b\u0005\u001b\u013b\b\u001b\n\u001b\f\u001b\u013e\t\u001b\u0001\u001b"+
		"\u0001\u001b\u0001\u001c\u0001\u001c\u0005\u001c\u0144\b\u001c\n\u001c"+
		"\f\u001c\u0147\t\u001c\u0001\u001c\u0001\u001c\u0001\u001d\u0001\u001d"+
		"\u0001\u001d\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e"+
		"\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e"+
		"\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e"+
		"\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0003\u001e"+
		"\u0164\b\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e"+
		"\u0005\u001e\u016b\b\u001e\n\u001e\f\u001e\u016e\t\u001e\u0001\u001e\u0000"+
		"\u0000\u001f\u0000\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016"+
		"\u0018\u001a\u001c\u001e \"$&(*,.02468:<\u0000\u0000\u0190\u0000A\u0001"+
		"\u0000\u0000\u0000\u0002F\u0001\u0000\u0000\u0000\u0004O\u0001\u0000\u0000"+
		"\u0000\u0006Z\u0001\u0000\u0000\u0000\b\\\u0001\u0000\u0000\u0000\ng\u0001"+
		"\u0000\u0000\u0000\fi\u0001\u0000\u0000\u0000\u000em\u0001\u0000\u0000"+
		"\u0000\u0010x\u0001\u0000\u0000\u0000\u0012\u0086\u0001\u0000\u0000\u0000"+
		"\u0014\u0088\u0001\u0000\u0000\u0000\u0016\u0095\u0001\u0000\u0000\u0000"+
		"\u0018\u00a2\u0001\u0000\u0000\u0000\u001a\u00af\u0001\u0000\u0000\u0000"+
		"\u001c\u00bc\u0001\u0000\u0000\u0000\u001e\u00c9\u0001\u0000\u0000\u0000"+
		" \u00d6\u0001\u0000\u0000\u0000\"\u00e1\u0001\u0000\u0000\u0000$\u00e6"+
		"\u0001\u0000\u0000\u0000&\u00f4\u0001\u0000\u0000\u0000(\u0101\u0001\u0000"+
		"\u0000\u0000*\u010e\u0001\u0000\u0000\u0000,\u0112\u0001\u0000\u0000\u0000"+
		".\u011c\u0001\u0000\u0000\u00000\u012a\u0001\u0000\u0000\u00002\u012c"+
		"\u0001\u0000\u0000\u00004\u012f\u0001\u0000\u0000\u00006\u0138\u0001\u0000"+
		"\u0000\u00008\u0141\u0001\u0000\u0000\u0000:\u014a\u0001\u0000\u0000\u0000"+
		"<\u0163\u0001\u0000\u0000\u0000>@\u0003<\u001e\u0000?>\u0001\u0000\u0000"+
		"\u0000@C\u0001\u0000\u0000\u0000A?\u0001\u0000\u0000\u0000AB\u0001\u0000"+
		"\u0000\u0000BD\u0001\u0000\u0000\u0000CA\u0001\u0000\u0000\u0000DE\u0005"+
		"\u0000\u0000\u0001E\u0001\u0001\u0000\u0000\u0000FJ\u0005\u0001\u0000"+
		"\u0000GI\u0003<\u001e\u0000HG\u0001\u0000\u0000\u0000IL\u0001\u0000\u0000"+
		"\u0000JH\u0001\u0000\u0000\u0000JK\u0001\u0000\u0000\u0000KM\u0001\u0000"+
		"\u0000\u0000LJ\u0001\u0000\u0000\u0000MN\u0005\u0002\u0000\u0000N\u0003"+
		"\u0001\u0000\u0000\u0000OP\u0005\u0003\u0000\u0000PQ\u0003\u0002\u0001"+
		"\u0000Q\u0005\u0001\u0000\u0000\u0000RS\u0005\u0004\u0000\u0000S[\u0003"+
		"<\u001e\u0000TU\u0005\u0004\u0000\u0000UV\u0005\u0005\u0000\u0000V[\u0003"+
		"<\u001e\u0000WX\u0005\u0004\u0000\u0000XY\u0005\u0006\u0000\u0000Y[\u0003"+
		"<\u001e\u0000ZR\u0001\u0000\u0000\u0000ZT\u0001\u0000\u0000\u0000ZW\u0001"+
		"\u0000\u0000\u0000[\u0007\u0001\u0000\u0000\u0000\\^\u0005\u001d\u0000"+
		"\u0000]_\u0003\u0006\u0003\u0000^]\u0001\u0000\u0000\u0000^_\u0001\u0000"+
		"\u0000\u0000_\t\u0001\u0000\u0000\u0000`b\u0005\u001d\u0000\u0000ac\u0003"+
		"\u0006\u0003\u0000ba\u0001\u0000\u0000\u0000bc\u0001\u0000\u0000\u0000"+
		"cd\u0001\u0000\u0000\u0000de\u0005\u0007\u0000\u0000eh\u0003<\u001e\u0000"+
		"fh\u0005\u001d\u0000\u0000g`\u0001\u0000\u0000\u0000gf\u0001\u0000\u0000"+
		"\u0000h\u000b\u0001\u0000\u0000\u0000ij\u0003<\u001e\u0000j\r\u0001\u0000"+
		"\u0000\u0000kn\u0003\n\u0005\u0000ln\u0003\f\u0006\u0000mk\u0001\u0000"+
		"\u0000\u0000ml\u0001\u0000\u0000\u0000n\u000f\u0001\u0000\u0000\u0000"+
		"op\u0005\b\u0000\u0000pr\u0005\u001d\u0000\u0000qs\u0003\u0006\u0003\u0000"+
		"rq\u0001\u0000\u0000\u0000rs\u0001\u0000\u0000\u0000st\u0001\u0000\u0000"+
		"\u0000tu\u0005\u0007\u0000\u0000uy\u0003<\u001e\u0000vw\u0005\b\u0000"+
		"\u0000wy\u0005\u001d\u0000\u0000xo\u0001\u0000\u0000\u0000xv\u0001\u0000"+
		"\u0000\u0000y\u0011\u0001\u0000\u0000\u0000z{\u0005\t\u0000\u0000{|\u0005"+
		"\u001d\u0000\u0000|}\u0005\n\u0000\u0000}~\u0003<\u001e\u0000~\u007f\u0003"+
		"\u0002\u0001\u0000\u007f\u0087\u0001\u0000\u0000\u0000\u0080\u0081\u0005"+
		"\t\u0000\u0000\u0081\u0087\u0003\u0002\u0001\u0000\u0082\u0083\u0005\t"+
		"\u0000\u0000\u0083\u0084\u0003<\u001e\u0000\u0084\u0085\u0003\u0002\u0001"+
		"\u0000\u0085\u0087\u0001\u0000\u0000\u0000\u0086z\u0001\u0000\u0000\u0000"+
		"\u0086\u0080\u0001\u0000\u0000\u0000\u0086\u0082\u0001\u0000\u0000\u0000"+
		"\u0087\u0013\u0001\u0000\u0000\u0000\u0088\u008a\u0005\u000b\u0000\u0000"+
		"\u0089\u008b\u0005\u001d\u0000\u0000\u008a\u0089\u0001\u0000\u0000\u0000"+
		"\u008a\u008b\u0001\u0000\u0000\u0000\u008b\u008c\u0001\u0000\u0000\u0000"+
		"\u008c\u0090\u0005\u0001\u0000\u0000\u008d\u008f\u0003\b\u0004\u0000\u008e"+
		"\u008d\u0001\u0000\u0000\u0000\u008f\u0092\u0001\u0000\u0000\u0000\u0090"+
		"\u008e\u0001\u0000\u0000\u0000\u0090\u0091\u0001\u0000\u0000\u0000\u0091"+
		"\u0093\u0001\u0000\u0000\u0000\u0092\u0090\u0001\u0000\u0000\u0000\u0093"+
		"\u0094\u0005\u0002\u0000\u0000\u0094\u0015\u0001\u0000\u0000\u0000\u0095"+
		"\u0097\u0005\f\u0000\u0000\u0096\u0098\u0005\u001d\u0000\u0000\u0097\u0096"+
		"\u0001\u0000\u0000\u0000\u0097\u0098\u0001\u0000\u0000\u0000\u0098\u0099"+
		"\u0001\u0000\u0000\u0000\u0099\u009d\u0005\u0001\u0000\u0000\u009a\u009c"+
		"\u0003\n\u0005\u0000\u009b\u009a\u0001\u0000\u0000\u0000\u009c\u009f\u0001"+
		"\u0000\u0000\u0000\u009d\u009b\u0001\u0000\u0000\u0000\u009d\u009e\u0001"+
		"\u0000\u0000\u0000\u009e\u00a0\u0001\u0000\u0000\u0000\u009f\u009d\u0001"+
		"\u0000\u0000\u0000\u00a0\u00a1\u0005\u0002\u0000\u0000\u00a1\u0017\u0001"+
		"\u0000\u0000\u0000\u00a2\u00a4\u0005\r\u0000\u0000\u00a3\u00a5\u0005\u001d"+
		"\u0000\u0000\u00a4\u00a3\u0001\u0000\u0000\u0000\u00a4\u00a5\u0001\u0000"+
		"\u0000\u0000\u00a5\u00a6\u0001\u0000\u0000\u0000\u00a6\u00aa\u0005\u0001"+
		"\u0000\u0000\u00a7\u00a9\u0003\b\u0004\u0000\u00a8\u00a7\u0001\u0000\u0000"+
		"\u0000\u00a9\u00ac\u0001\u0000\u0000\u0000\u00aa\u00a8\u0001\u0000\u0000"+
		"\u0000\u00aa\u00ab\u0001\u0000\u0000\u0000\u00ab\u00ad\u0001\u0000\u0000"+
		"\u0000\u00ac\u00aa\u0001\u0000\u0000\u0000\u00ad\u00ae\u0005\u0002\u0000"+
		"\u0000\u00ae\u0019\u0001\u0000\u0000\u0000\u00af\u00b1\u0005\u000e\u0000"+
		"\u0000\u00b0\u00b2\u0005\u001d\u0000\u0000\u00b1\u00b0\u0001\u0000\u0000"+
		"\u0000\u00b1\u00b2\u0001\u0000\u0000\u0000\u00b2\u00b3\u0001\u0000\u0000"+
		"\u0000\u00b3\u00b7\u0005\u0001\u0000\u0000\u00b4\u00b6\u0003\n\u0005\u0000"+
		"\u00b5\u00b4\u0001\u0000\u0000\u0000\u00b6\u00b9\u0001\u0000\u0000\u0000"+
		"\u00b7\u00b5\u0001\u0000\u0000\u0000\u00b7\u00b8\u0001\u0000\u0000\u0000"+
		"\u00b8\u00ba\u0001\u0000\u0000\u0000\u00b9\u00b7\u0001\u0000\u0000\u0000"+
		"\u00ba\u00bb\u0005\u0002\u0000\u0000\u00bb\u001b\u0001\u0000\u0000\u0000"+
		"\u00bc\u00be\u0005\u000f\u0000\u0000\u00bd\u00bf\u0005\u001d\u0000\u0000"+
		"\u00be\u00bd\u0001\u0000\u0000\u0000\u00be\u00bf\u0001\u0000\u0000\u0000"+
		"\u00bf\u00c0\u0001\u0000\u0000\u0000\u00c0\u00c4\u0005\u0001\u0000\u0000"+
		"\u00c1\u00c3\u0003\u0010\b\u0000\u00c2\u00c1\u0001\u0000\u0000\u0000\u00c3"+
		"\u00c6\u0001\u0000\u0000\u0000\u00c4\u00c2\u0001\u0000\u0000\u0000\u00c4"+
		"\u00c5\u0001\u0000\u0000\u0000\u00c5\u00c7\u0001\u0000\u0000\u0000\u00c6"+
		"\u00c4\u0001\u0000\u0000\u0000\u00c7\u00c8\u0005\u0002\u0000\u0000\u00c8"+
		"\u001d\u0001\u0000\u0000\u0000\u00c9\u00cb\u0005\u0010\u0000\u0000\u00ca"+
		"\u00cc\u0005\u001d\u0000\u0000\u00cb\u00ca\u0001\u0000\u0000\u0000\u00cb"+
		"\u00cc\u0001\u0000\u0000\u0000\u00cc\u00cd\u0001\u0000\u0000\u0000\u00cd"+
		"\u00d1\u0005\u0001\u0000\u0000\u00ce\u00d0\u0003\u0010\b\u0000\u00cf\u00ce"+
		"\u0001\u0000\u0000\u0000\u00d0\u00d3\u0001\u0000\u0000\u0000\u00d1\u00cf"+
		"\u0001\u0000\u0000\u0000\u00d1\u00d2\u0001\u0000\u0000\u0000\u00d2\u00d4"+
		"\u0001\u0000\u0000\u0000\u00d3\u00d1\u0001\u0000\u0000\u0000\u00d4\u00d5"+
		"\u0005\u0002\u0000\u0000\u00d5\u001f\u0001\u0000\u0000\u0000\u00d6\u00da"+
		"\u0005\u0011\u0000\u0000\u00d7\u00d9\u0003<\u001e\u0000\u00d8\u00d7\u0001"+
		"\u0000\u0000\u0000\u00d9\u00dc\u0001\u0000\u0000\u0000\u00da\u00d8\u0001"+
		"\u0000\u0000\u0000\u00da\u00db\u0001\u0000\u0000\u0000\u00db\u00dd\u0001"+
		"\u0000\u0000\u0000\u00dc\u00da\u0001\u0000\u0000\u0000\u00dd\u00de\u0005"+
		"\u0012\u0000\u0000\u00de\u00df\u0005\u0013\u0000\u0000\u00df\u00e0\u0003"+
		"<\u001e\u0000\u00e0!\u0001\u0000\u0000\u0000\u00e1\u00e4\u0005\u0014\u0000"+
		"\u0000\u00e2\u00e5\u0003\u0002\u0001\u0000\u00e3\u00e5\u0003<\u001e\u0000"+
		"\u00e4\u00e2\u0001\u0000\u0000\u0000\u00e4\u00e3\u0001\u0000\u0000\u0000"+
		"\u00e5#\u0001\u0000\u0000\u0000\u00e6\u00ea\u0005\u0011\u0000\u0000\u00e7"+
		"\u00e9\u0003\b\u0004\u0000\u00e8\u00e7\u0001\u0000\u0000\u0000\u00e9\u00ec"+
		"\u0001\u0000\u0000\u0000\u00ea\u00e8\u0001\u0000\u0000\u0000\u00ea\u00eb"+
		"\u0001\u0000\u0000\u0000\u00eb\u00ed\u0001\u0000\u0000\u0000\u00ec\u00ea"+
		"\u0001\u0000\u0000\u0000\u00ed\u00f0\u0005\u0012\u0000\u0000\u00ee\u00ef"+
		"\u0005\u0013\u0000\u0000\u00ef\u00f1\u0003<\u001e\u0000\u00f0\u00ee\u0001"+
		"\u0000\u0000\u0000\u00f0\u00f1\u0001\u0000\u0000\u0000\u00f1\u00f2\u0001"+
		"\u0000\u0000\u0000\u00f2\u00f3\u0003\"\u0011\u0000\u00f3%\u0001\u0000"+
		"\u0000\u0000\u00f4\u00f6\u0005\u0015\u0000\u0000\u00f5\u00f7\u0005\u001d"+
		"\u0000\u0000\u00f6\u00f5\u0001\u0000\u0000\u0000\u00f6\u00f7\u0001\u0000"+
		"\u0000\u0000\u00f7\u00f8\u0001\u0000\u0000\u0000\u00f8\u00fc\u0005\u0001"+
		"\u0000\u0000\u00f9\u00fb\u0003\b\u0004\u0000\u00fa\u00f9\u0001\u0000\u0000"+
		"\u0000\u00fb\u00fe\u0001\u0000\u0000\u0000\u00fc\u00fa\u0001\u0000\u0000"+
		"\u0000\u00fc\u00fd\u0001\u0000\u0000\u0000\u00fd\u00ff\u0001\u0000\u0000"+
		"\u0000\u00fe\u00fc\u0001\u0000\u0000\u0000\u00ff\u0100\u0005\u0002\u0000"+
		"\u0000\u0100\'\u0001\u0000\u0000\u0000\u0101\u0103\u0005\u0016\u0000\u0000"+
		"\u0102\u0104\u0005\u001d\u0000\u0000\u0103\u0102\u0001\u0000\u0000\u0000"+
		"\u0103\u0104\u0001\u0000\u0000\u0000\u0104\u0105\u0001\u0000\u0000\u0000"+
		"\u0105\u0109\u0005\u0001\u0000\u0000\u0106\u0108\u0003\n\u0005\u0000\u0107"+
		"\u0106\u0001\u0000\u0000\u0000\u0108\u010b\u0001\u0000\u0000\u0000\u0109"+
		"\u0107\u0001\u0000\u0000\u0000\u0109\u010a\u0001\u0000\u0000\u0000\u010a"+
		"\u010c\u0001\u0000\u0000\u0000\u010b\u0109\u0001\u0000\u0000\u0000\u010c"+
		"\u010d\u0005\u0002\u0000\u0000\u010d)\u0001\u0000\u0000\u0000\u010e\u010f"+
		"\u0005\u0017\u0000\u0000\u010f\u0110\u0003<\u001e\u0000\u0110\u0111\u0003"+
		"\"\u0011\u0000\u0111+\u0001\u0000\u0000\u0000\u0112\u0113\u0005\u0018"+
		"\u0000\u0000\u0113\u0117\u0005\u0001\u0000\u0000\u0114\u0116\u0003*\u0015"+
		"\u0000\u0115\u0114\u0001\u0000\u0000\u0000\u0116\u0119\u0001\u0000\u0000"+
		"\u0000\u0117\u0115\u0001\u0000\u0000\u0000\u0117\u0118\u0001\u0000\u0000"+
		"\u0000\u0118\u011a\u0001\u0000\u0000\u0000\u0119\u0117\u0001\u0000\u0000"+
		"\u0000\u011a\u011b\u0005\u0002\u0000\u0000\u011b-\u0001\u0000\u0000\u0000"+
		"\u011c\u0120\u0005\u0019\u0000\u0000\u011d\u011f\u0003\b\u0004\u0000\u011e"+
		"\u011d\u0001\u0000\u0000\u0000\u011f\u0122\u0001\u0000\u0000\u0000\u0120"+
		"\u011e\u0001\u0000\u0000\u0000\u0120\u0121\u0001\u0000\u0000\u0000\u0121"+
		"\u0123\u0001\u0000\u0000\u0000\u0122\u0120\u0001\u0000\u0000\u0000\u0123"+
		"\u0124\u0005\u001a\u0000\u0000\u0124\u0125\u0003\"\u0011\u0000\u0125/"+
		"\u0001\u0000\u0000\u0000\u0126\u0127\u0005\u0005\u0000\u0000\u0127\u012b"+
		"\u0003<\u001e\u0000\u0128\u0129\u0005\u0006\u0000\u0000\u0129\u012b\u0003"+
		"<\u001e\u0000\u012a\u0126\u0001\u0000\u0000\u0000\u012a\u0128\u0001\u0000"+
		"\u0000\u0000\u012b1\u0001\u0000\u0000\u0000\u012c\u012d\u0005\u001b\u0000"+
		"\u0000\u012d\u012e\u0005\u001d\u0000\u0000\u012e3\u0001\u0000\u0000\u0000"+
		"\u012f\u0133\u0005\u0019\u0000\u0000\u0130\u0132\u0003\u000e\u0007\u0000"+
		"\u0131\u0130\u0001\u0000\u0000\u0000\u0132\u0135\u0001\u0000\u0000\u0000"+
		"\u0133\u0131\u0001\u0000\u0000\u0000\u0133\u0134\u0001\u0000\u0000\u0000"+
		"\u0134\u0136\u0001\u0000\u0000\u0000\u0135\u0133\u0001\u0000\u0000\u0000"+
		"\u0136\u0137\u0005\u001a\u0000\u0000\u01375\u0001\u0000\u0000\u0000\u0138"+
		"\u013c\u0005\u0011\u0000\u0000\u0139\u013b\u0003\u000e\u0007\u0000\u013a"+
		"\u0139\u0001\u0000\u0000\u0000\u013b\u013e\u0001\u0000\u0000\u0000\u013c"+
		"\u013a\u0001\u0000\u0000\u0000\u013c\u013d\u0001\u0000\u0000\u0000\u013d"+
		"\u013f\u0001\u0000\u0000\u0000\u013e\u013c\u0001\u0000\u0000\u0000\u013f"+
		"\u0140\u0005\u0012\u0000\u0000\u01407\u0001\u0000\u0000\u0000\u0141\u0145"+
		"\u0005\u0001\u0000\u0000\u0142\u0144\u0003\n\u0005\u0000\u0143\u0142\u0001"+
		"\u0000\u0000\u0000\u0144\u0147\u0001\u0000\u0000\u0000\u0145\u0143\u0001"+
		"\u0000\u0000\u0000\u0145\u0146\u0001\u0000\u0000\u0000\u0146\u0148\u0001"+
		"\u0000\u0000\u0000\u0147\u0145\u0001\u0000\u0000\u0000\u0148\u0149\u0005"+
		"\u0002\u0000\u0000\u01499\u0001\u0000\u0000\u0000\u014a\u014b\u0005\u0007"+
		"\u0000\u0000\u014b\u014c\u0003<\u001e\u0000\u014c;\u0001\u0000\u0000\u0000"+
		"\u014d\u0164\u0003\u0004\u0002\u0000\u014e\u0164\u0003.\u0017\u0000\u014f"+
		"\u0164\u0003\u0010\b\u0000\u0150\u0164\u0003\u0012\t\u0000\u0151\u0164"+
		"\u0003\u0014\n\u0000\u0152\u0164\u0003\u0016\u000b\u0000\u0153\u0164\u0003"+
		"\u0018\f\u0000\u0154\u0164\u0003\u001a\r\u0000\u0155\u0164\u0003\u001c"+
		"\u000e\u0000\u0156\u0164\u0003\u001e\u000f\u0000\u0157\u0164\u0003 \u0010"+
		"\u0000\u0158\u0164\u0003$\u0012\u0000\u0159\u0164\u0003&\u0013\u0000\u015a"+
		"\u0164\u0003(\u0014\u0000\u015b\u0164\u0003,\u0016\u0000\u015c\u0164\u0003"+
		"0\u0018\u0000\u015d\u0164\u0005\u001c\u0000\u0000\u015e\u0164\u0005\u001d"+
		"\u0000\u0000\u015f\u0164\u0005\u001e\u0000\u0000\u0160\u0164\u0005\u001f"+
		"\u0000\u0000\u0161\u0164\u0005 \u0000\u0000\u0162\u0164\u0005!\u0000\u0000"+
		"\u0163\u014d\u0001\u0000\u0000\u0000\u0163\u014e\u0001\u0000\u0000\u0000"+
		"\u0163\u014f\u0001\u0000\u0000\u0000\u0163\u0150\u0001\u0000\u0000\u0000"+
		"\u0163\u0151\u0001\u0000\u0000\u0000\u0163\u0152\u0001\u0000\u0000\u0000"+
		"\u0163\u0153\u0001\u0000\u0000\u0000\u0163\u0154\u0001\u0000\u0000\u0000"+
		"\u0163\u0155\u0001\u0000\u0000\u0000\u0163\u0156\u0001\u0000\u0000\u0000"+
		"\u0163\u0157\u0001\u0000\u0000\u0000\u0163\u0158\u0001\u0000\u0000\u0000"+
		"\u0163\u0159\u0001\u0000\u0000\u0000\u0163\u015a\u0001\u0000\u0000\u0000"+
		"\u0163\u015b\u0001\u0000\u0000\u0000\u0163\u015c\u0001\u0000\u0000\u0000"+
		"\u0163\u015d\u0001\u0000\u0000\u0000\u0163\u015e\u0001\u0000\u0000\u0000"+
		"\u0163\u015f\u0001\u0000\u0000\u0000\u0163\u0160\u0001\u0000\u0000\u0000"+
		"\u0163\u0161\u0001\u0000\u0000\u0000\u0163\u0162\u0001\u0000\u0000\u0000"+
		"\u0164\u016c\u0001\u0000\u0000\u0000\u0165\u016b\u00032\u0019\u0000\u0166"+
		"\u016b\u00034\u001a\u0000\u0167\u016b\u00036\u001b\u0000\u0168\u016b\u0003"+
		"8\u001c\u0000\u0169\u016b\u0003:\u001d\u0000\u016a\u0165\u0001\u0000\u0000"+
		"\u0000\u016a\u0166\u0001\u0000\u0000\u0000\u016a\u0167\u0001\u0000\u0000"+
		"\u0000\u016a\u0168\u0001\u0000\u0000\u0000\u016a\u0169\u0001\u0000\u0000"+
		"\u0000\u016b\u016e\u0001\u0000\u0000\u0000\u016c\u016a\u0001\u0000\u0000"+
		"\u0000\u016c\u016d\u0001\u0000\u0000\u0000\u016d=\u0001\u0000\u0000\u0000"+
		"\u016e\u016c\u0001\u0000\u0000\u0000\'AJZ^bgmrx\u0086\u008a\u0090\u0097"+
		"\u009d\u00a4\u00aa\u00b1\u00b7\u00be\u00c4\u00cb\u00d1\u00da\u00e4\u00ea"+
		"\u00f0\u00f6\u00fc\u0103\u0109\u0117\u0120\u012a\u0133\u013c\u0145\u0163"+
		"\u016a\u016c";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}