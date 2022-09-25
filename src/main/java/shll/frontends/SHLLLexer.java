// Generated from java-escape by ANTLR 4.11.1
package shll.frontends;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class SHLLLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.11.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, IDENT=6, BOOL=7, INTEGER=8, DECIMAL=9, 
		STRING=10, CHAR=11, WS=12;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "T__2", "T__3", "T__4", "IDENT", "BOOL", "INTEGER", "DECIMAL", 
			"STRING", "CHAR", "WS"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'='", "'('", "')'", "'['", "']'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, "IDENT", "BOOL", "INTEGER", "DECIMAL", 
			"STRING", "CHAR", "WS"
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


	public SHLLLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "SHLL.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\u0004\u0000\f\u008f\u0006\uffff\uffff\u0002\u0000\u0007\u0000\u0002\u0001"+
		"\u0007\u0001\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004"+
		"\u0007\u0004\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007"+
		"\u0007\u0007\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b"+
		"\u0007\u000b\u0001\u0000\u0001\u0000\u0001\u0001\u0001\u0001\u0001\u0002"+
		"\u0001\u0002\u0001\u0003\u0001\u0003\u0001\u0004\u0001\u0004\u0001\u0005"+
		"\u0001\u0005\u0005\u0005&\b\u0005\n\u0005\f\u0005)\t\u0005\u0001\u0005"+
		"\u0004\u0005,\b\u0005\u000b\u0005\f\u0005-\u0003\u00050\b\u0005\u0001"+
		"\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001"+
		"\u0006\u0001\u0006\u0001\u0006\u0003\u0006;\b\u0006\u0001\u0007\u0001"+
		"\u0007\u0001\u0007\u0004\u0007@\b\u0007\u000b\u0007\f\u0007A\u0001\u0007"+
		"\u0001\u0007\u0001\u0007\u0004\u0007G\b\u0007\u000b\u0007\f\u0007H\u0001"+
		"\u0007\u0001\u0007\u0001\u0007\u0004\u0007N\b\u0007\u000b\u0007\f\u0007"+
		"O\u0001\u0007\u0003\u0007S\b\u0007\u0001\u0007\u0001\u0007\u0003\u0007"+
		"W\b\u0007\u0001\u0007\u0001\u0007\u0005\u0007[\b\u0007\n\u0007\f\u0007"+
		"^\t\u0007\u0003\u0007`\b\u0007\u0001\b\u0003\bc\b\b\u0001\b\u0004\bf\b"+
		"\b\u000b\b\f\bg\u0001\b\u0001\b\u0004\bl\b\b\u000b\b\f\bm\u0001\t\u0001"+
		"\t\u0001\t\u0001\t\u0005\tt\b\t\n\t\f\tw\t\t\u0001\t\u0001\t\u0001\n\u0001"+
		"\n\u0001\n\u0001\n\u0001\n\u0001\n\u0004\n\u0081\b\n\u000b\n\f\n\u0082"+
		"\u0003\n\u0085\b\n\u0001\n\u0001\n\u0001\u000b\u0004\u000b\u008a\b\u000b"+
		"\u000b\u000b\f\u000b\u008b\u0001\u000b\u0001\u000b\u0001\u0082\u0000\f"+
		"\u0001\u0001\u0003\u0002\u0005\u0003\u0007\u0004\t\u0005\u000b\u0006\r"+
		"\u0007\u000f\b\u0011\t\u0013\n\u0015\u000b\u0017\f\u0001\u0000\u000e\u0003"+
		"\u0000AZ__az\u0005\u0000--09AZ__az\t\u0000!!%&*+--//::<>^^||\u0002\u0000"+
		"XXxx\u0003\u000009AZaz\u0002\u0000OOoo\u0001\u000007\u0002\u0000BBbb\u0001"+
		"\u000001\u0002\u0000++--\u0001\u000019\u0001\u000009\u0002\u0000\"\"^"+
		"^\u0002\u0000\t\n  \u00a5\u0000\u0001\u0001\u0000\u0000\u0000\u0000\u0003"+
		"\u0001\u0000\u0000\u0000\u0000\u0005\u0001\u0000\u0000\u0000\u0000\u0007"+
		"\u0001\u0000\u0000\u0000\u0000\t\u0001\u0000\u0000\u0000\u0000\u000b\u0001"+
		"\u0000\u0000\u0000\u0000\r\u0001\u0000\u0000\u0000\u0000\u000f\u0001\u0000"+
		"\u0000\u0000\u0000\u0011\u0001\u0000\u0000\u0000\u0000\u0013\u0001\u0000"+
		"\u0000\u0000\u0000\u0015\u0001\u0000\u0000\u0000\u0000\u0017\u0001\u0000"+
		"\u0000\u0000\u0001\u0019\u0001\u0000\u0000\u0000\u0003\u001b\u0001\u0000"+
		"\u0000\u0000\u0005\u001d\u0001\u0000\u0000\u0000\u0007\u001f\u0001\u0000"+
		"\u0000\u0000\t!\u0001\u0000\u0000\u0000\u000b/\u0001\u0000\u0000\u0000"+
		"\r:\u0001\u0000\u0000\u0000\u000f_\u0001\u0000\u0000\u0000\u0011b\u0001"+
		"\u0000\u0000\u0000\u0013o\u0001\u0000\u0000\u0000\u0015z\u0001\u0000\u0000"+
		"\u0000\u0017\u0089\u0001\u0000\u0000\u0000\u0019\u001a\u0005=\u0000\u0000"+
		"\u001a\u0002\u0001\u0000\u0000\u0000\u001b\u001c\u0005(\u0000\u0000\u001c"+
		"\u0004\u0001\u0000\u0000\u0000\u001d\u001e\u0005)\u0000\u0000\u001e\u0006"+
		"\u0001\u0000\u0000\u0000\u001f \u0005[\u0000\u0000 \b\u0001\u0000\u0000"+
		"\u0000!\"\u0005]\u0000\u0000\"\n\u0001\u0000\u0000\u0000#\'\u0007\u0000"+
		"\u0000\u0000$&\u0007\u0001\u0000\u0000%$\u0001\u0000\u0000\u0000&)\u0001"+
		"\u0000\u0000\u0000\'%\u0001\u0000\u0000\u0000\'(\u0001\u0000\u0000\u0000"+
		"(0\u0001\u0000\u0000\u0000)\'\u0001\u0000\u0000\u0000*,\u0007\u0002\u0000"+
		"\u0000+*\u0001\u0000\u0000\u0000,-\u0001\u0000\u0000\u0000-+\u0001\u0000"+
		"\u0000\u0000-.\u0001\u0000\u0000\u0000.0\u0001\u0000\u0000\u0000/#\u0001"+
		"\u0000\u0000\u0000/+\u0001\u0000\u0000\u00000\f\u0001\u0000\u0000\u0000"+
		"12\u0005t\u0000\u000023\u0005r\u0000\u000034\u0005u\u0000\u00004;\u0005"+
		"e\u0000\u000056\u0005f\u0000\u000067\u0005a\u0000\u000078\u0005l\u0000"+
		"\u000089\u0005s\u0000\u00009;\u0005e\u0000\u0000:1\u0001\u0000\u0000\u0000"+
		":5\u0001\u0000\u0000\u0000;\u000e\u0001\u0000\u0000\u0000<=\u00050\u0000"+
		"\u0000=?\u0007\u0003\u0000\u0000>@\u0007\u0004\u0000\u0000?>\u0001\u0000"+
		"\u0000\u0000@A\u0001\u0000\u0000\u0000A?\u0001\u0000\u0000\u0000AB\u0001"+
		"\u0000\u0000\u0000B`\u0001\u0000\u0000\u0000CD\u00050\u0000\u0000DF\u0007"+
		"\u0005\u0000\u0000EG\u0007\u0006\u0000\u0000FE\u0001\u0000\u0000\u0000"+
		"GH\u0001\u0000\u0000\u0000HF\u0001\u0000\u0000\u0000HI\u0001\u0000\u0000"+
		"\u0000I`\u0001\u0000\u0000\u0000JK\u00050\u0000\u0000KM\u0007\u0007\u0000"+
		"\u0000LN\u0007\b\u0000\u0000ML\u0001\u0000\u0000\u0000NO\u0001\u0000\u0000"+
		"\u0000OM\u0001\u0000\u0000\u0000OP\u0001\u0000\u0000\u0000P`\u0001\u0000"+
		"\u0000\u0000QS\u0007\t\u0000\u0000RQ\u0001\u0000\u0000\u0000RS\u0001\u0000"+
		"\u0000\u0000ST\u0001\u0000\u0000\u0000T`\u00050\u0000\u0000UW\u0007\t"+
		"\u0000\u0000VU\u0001\u0000\u0000\u0000VW\u0001\u0000\u0000\u0000WX\u0001"+
		"\u0000\u0000\u0000X\\\u0007\n\u0000\u0000Y[\u0007\u000b\u0000\u0000ZY"+
		"\u0001\u0000\u0000\u0000[^\u0001\u0000\u0000\u0000\\Z\u0001\u0000\u0000"+
		"\u0000\\]\u0001\u0000\u0000\u0000]`\u0001\u0000\u0000\u0000^\\\u0001\u0000"+
		"\u0000\u0000_<\u0001\u0000\u0000\u0000_C\u0001\u0000\u0000\u0000_J\u0001"+
		"\u0000\u0000\u0000_R\u0001\u0000\u0000\u0000_V\u0001\u0000\u0000\u0000"+
		"`\u0010\u0001\u0000\u0000\u0000ac\u0007\t\u0000\u0000ba\u0001\u0000\u0000"+
		"\u0000bc\u0001\u0000\u0000\u0000ce\u0001\u0000\u0000\u0000df\u0007\u000b"+
		"\u0000\u0000ed\u0001\u0000\u0000\u0000fg\u0001\u0000\u0000\u0000ge\u0001"+
		"\u0000\u0000\u0000gh\u0001\u0000\u0000\u0000hi\u0001\u0000\u0000\u0000"+
		"ik\u0005.\u0000\u0000jl\u0007\u000b\u0000\u0000kj\u0001\u0000\u0000\u0000"+
		"lm\u0001\u0000\u0000\u0000mk\u0001\u0000\u0000\u0000mn\u0001\u0000\u0000"+
		"\u0000n\u0012\u0001\u0000\u0000\u0000ou\u0005\"\u0000\u0000pt\u0007\f"+
		"\u0000\u0000qr\u0005\\\u0000\u0000rt\u0005\"\u0000\u0000sp\u0001\u0000"+
		"\u0000\u0000sq\u0001\u0000\u0000\u0000tw\u0001\u0000\u0000\u0000us\u0001"+
		"\u0000\u0000\u0000uv\u0001\u0000\u0000\u0000vx\u0001\u0000\u0000\u0000"+
		"wu\u0001\u0000\u0000\u0000xy\u0005\"\u0000\u0000y\u0014\u0001\u0000\u0000"+
		"\u0000z\u0084\u0005\'\u0000\u0000{\u0085\u0007\f\u0000\u0000|}\u0005\\"+
		"\u0000\u0000}\u0085\u0005\"\u0000\u0000~\u0080\u0005\\\u0000\u0000\u007f"+
		"\u0081\t\u0000\u0000\u0000\u0080\u007f\u0001\u0000\u0000\u0000\u0081\u0082"+
		"\u0001\u0000\u0000\u0000\u0082\u0083\u0001\u0000\u0000\u0000\u0082\u0080"+
		"\u0001\u0000\u0000\u0000\u0083\u0085\u0001\u0000\u0000\u0000\u0084{\u0001"+
		"\u0000\u0000\u0000\u0084|\u0001\u0000\u0000\u0000\u0084~\u0001\u0000\u0000"+
		"\u0000\u0085\u0086\u0001\u0000\u0000\u0000\u0086\u0087\u0005\'\u0000\u0000"+
		"\u0087\u0016\u0001\u0000\u0000\u0000\u0088\u008a\u0007\r\u0000\u0000\u0089"+
		"\u0088\u0001\u0000\u0000\u0000\u008a\u008b\u0001\u0000\u0000\u0000\u008b"+
		"\u0089\u0001\u0000\u0000\u0000\u008b\u008c\u0001\u0000\u0000\u0000\u008c"+
		"\u008d\u0001\u0000\u0000\u0000\u008d\u008e\u0006\u000b\u0000\u0000\u008e"+
		"\u0018\u0001\u0000\u0000\u0000\u0014\u0000\'-/:AHORV\\_bgmsu\u0082\u0084"+
		"\u008b\u0001\u0006\u0000\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}