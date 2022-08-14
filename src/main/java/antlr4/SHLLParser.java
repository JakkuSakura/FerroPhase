// Generated from src/main/antlr4/SHLL.g4 by ANTLR 4.10.1
package antlr4;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class SHLLParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.10.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, IDENT=4, INTEGER=5, DECIMAL=6, STRING=7, CHAR=8, 
		WS=9;
	public static final int
		RULE_term = 0, RULE_kwArg = 1, RULE_kwArgs = 2, RULE_posArgs = 3, RULE_apply = 4, 
		RULE_program = 5;
	private static String[] makeRuleNames() {
		return new String[] {
			"term", "kwArg", "kwArgs", "posArgs", "apply", "program"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'='", "'('", "')'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, "IDENT", "INTEGER", "DECIMAL", "STRING", "CHAR", 
			"WS"
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
	public String getGrammarFileName() { return "SHLL.g4"; }

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

	public static class TermContext extends ParserRuleContext {
		public ApplyContext apply() {
			return getRuleContext(ApplyContext.class,0);
		}
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public TerminalNode INTEGER() { return getToken(SHLLParser.INTEGER, 0); }
		public TerminalNode DECIMAL() { return getToken(SHLLParser.DECIMAL, 0); }
		public TerminalNode STRING() { return getToken(SHLLParser.STRING, 0); }
		public TerminalNode CHAR() { return getToken(SHLLParser.CHAR, 0); }
		public TermContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_term; }
	}

	public final TermContext term() throws RecognitionException {
		TermContext _localctx = new TermContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_term);
		try {
			setState(18);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__1:
				enterOuterAlt(_localctx, 1);
				{
				setState(12);
				apply();
				}
				break;
			case IDENT:
				enterOuterAlt(_localctx, 2);
				{
				setState(13);
				match(IDENT);
				}
				break;
			case INTEGER:
				enterOuterAlt(_localctx, 3);
				{
				setState(14);
				match(INTEGER);
				}
				break;
			case DECIMAL:
				enterOuterAlt(_localctx, 4);
				{
				setState(15);
				match(DECIMAL);
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 5);
				{
				setState(16);
				match(STRING);
				}
				break;
			case CHAR:
				enterOuterAlt(_localctx, 6);
				{
				setState(17);
				match(CHAR);
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

	public static class KwArgContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(SHLLParser.IDENT, 0); }
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public KwArgContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_kwArg; }
	}

	public final KwArgContext kwArg() throws RecognitionException {
		KwArgContext _localctx = new KwArgContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_kwArg);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(20);
			match(IDENT);
			setState(21);
			match(T__0);
			setState(22);
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

	public static class KwArgsContext extends ParserRuleContext {
		public List<KwArgContext> kwArg() {
			return getRuleContexts(KwArgContext.class);
		}
		public KwArgContext kwArg(int i) {
			return getRuleContext(KwArgContext.class,i);
		}
		public KwArgsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_kwArgs; }
	}

	public final KwArgsContext kwArgs() throws RecognitionException {
		KwArgsContext _localctx = new KwArgsContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_kwArgs);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(27);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(24);
				kwArg();
				}
				}
				setState(29);
				_errHandler.sync(this);
				_la = _input.LA(1);
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

	public static class PosArgsContext extends ParserRuleContext {
		public List<TermContext> term() {
			return getRuleContexts(TermContext.class);
		}
		public TermContext term(int i) {
			return getRuleContext(TermContext.class,i);
		}
		public PosArgsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_posArgs; }
	}

	public final PosArgsContext posArgs() throws RecognitionException {
		PosArgsContext _localctx = new PosArgsContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_posArgs);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(33);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(30);
					term();
					}
					} 
				}
				setState(35);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
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

	public static class ApplyContext extends ParserRuleContext {
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public PosArgsContext posArgs() {
			return getRuleContext(PosArgsContext.class,0);
		}
		public KwArgsContext kwArgs() {
			return getRuleContext(KwArgsContext.class,0);
		}
		public ApplyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_apply; }
	}

	public final ApplyContext apply() throws RecognitionException {
		ApplyContext _localctx = new ApplyContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_apply);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(36);
			match(T__1);
			setState(37);
			term();
			setState(38);
			posArgs();
			setState(39);
			kwArgs();
			setState(40);
			match(T__2);
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

	public static class ProgramContext extends ParserRuleContext {
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public TerminalNode EOF() { return getToken(SHLLParser.EOF, 0); }
		public ProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_program; }
	}

	public final ProgramContext program() throws RecognitionException {
		ProgramContext _localctx = new ProgramContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_program);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(42);
			term();
			setState(43);
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

	public static final String _serializedATN =
		"\u0004\u0001\t.\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001"+
		"\u0000\u0001\u0000\u0003\u0000\u0013\b\u0000\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0002\u0005\u0002\u001a\b\u0002\n\u0002\f\u0002"+
		"\u001d\t\u0002\u0001\u0003\u0005\u0003 \b\u0003\n\u0003\f\u0003#\t\u0003"+
		"\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0000\u0000\u0006\u0000"+
		"\u0002\u0004\u0006\b\n\u0000\u0000.\u0000\u0012\u0001\u0000\u0000\u0000"+
		"\u0002\u0014\u0001\u0000\u0000\u0000\u0004\u001b\u0001\u0000\u0000\u0000"+
		"\u0006!\u0001\u0000\u0000\u0000\b$\u0001\u0000\u0000\u0000\n*\u0001\u0000"+
		"\u0000\u0000\f\u0013\u0003\b\u0004\u0000\r\u0013\u0005\u0004\u0000\u0000"+
		"\u000e\u0013\u0005\u0005\u0000\u0000\u000f\u0013\u0005\u0006\u0000\u0000"+
		"\u0010\u0013\u0005\u0007\u0000\u0000\u0011\u0013\u0005\b\u0000\u0000\u0012"+
		"\f\u0001\u0000\u0000\u0000\u0012\r\u0001\u0000\u0000\u0000\u0012\u000e"+
		"\u0001\u0000\u0000\u0000\u0012\u000f\u0001\u0000\u0000\u0000\u0012\u0010"+
		"\u0001\u0000\u0000\u0000\u0012\u0011\u0001\u0000\u0000\u0000\u0013\u0001"+
		"\u0001\u0000\u0000\u0000\u0014\u0015\u0005\u0004\u0000\u0000\u0015\u0016"+
		"\u0005\u0001\u0000\u0000\u0016\u0017\u0003\u0000\u0000\u0000\u0017\u0003"+
		"\u0001\u0000\u0000\u0000\u0018\u001a\u0003\u0002\u0001\u0000\u0019\u0018"+
		"\u0001\u0000\u0000\u0000\u001a\u001d\u0001\u0000\u0000\u0000\u001b\u0019"+
		"\u0001\u0000\u0000\u0000\u001b\u001c\u0001\u0000\u0000\u0000\u001c\u0005"+
		"\u0001\u0000\u0000\u0000\u001d\u001b\u0001\u0000\u0000\u0000\u001e \u0003"+
		"\u0000\u0000\u0000\u001f\u001e\u0001\u0000\u0000\u0000 #\u0001\u0000\u0000"+
		"\u0000!\u001f\u0001\u0000\u0000\u0000!\"\u0001\u0000\u0000\u0000\"\u0007"+
		"\u0001\u0000\u0000\u0000#!\u0001\u0000\u0000\u0000$%\u0005\u0002\u0000"+
		"\u0000%&\u0003\u0000\u0000\u0000&\'\u0003\u0006\u0003\u0000\'(\u0003\u0004"+
		"\u0002\u0000()\u0005\u0003\u0000\u0000)\t\u0001\u0000\u0000\u0000*+\u0003"+
		"\u0000\u0000\u0000+,\u0005\u0000\u0000\u0001,\u000b\u0001\u0000\u0000"+
		"\u0000\u0003\u0012\u001b!";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}