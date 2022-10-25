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
		T__0=1, T__1=2, T__2=3, BOOL=4, IDENT=5, INTEGER=6, DECIMAL=7, STRING=8, 
		CHAR=9, WS=10;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "T__2", "BOOL", "IDENT", "INTEGER", "DECIMAL", "STRING", 
			"CHAR", "WS"
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
			null, null, null, null, "BOOL", "IDENT", "INTEGER", "DECIMAL", "STRING", 
			"CHAR", "WS"
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
		"\u0004\u0000\n\u0087\u0006\uffff\uffff\u0002\u0000\u0007\u0000\u0002\u0001"+
		"\u0007\u0001\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004"+
		"\u0007\u0004\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007"+
		"\u0007\u0007\u0002\b\u0007\b\u0002\t\u0007\t\u0001\u0000\u0001\u0000\u0001"+
		"\u0001\u0001\u0001\u0001\u0002\u0001\u0002\u0001\u0003\u0001\u0003\u0001"+
		"\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001"+
		"\u0003\u0003\u0003%\b\u0003\u0001\u0004\u0001\u0004\u0005\u0004)\b\u0004"+
		"\n\u0004\f\u0004,\t\u0004\u0001\u0004\u0004\u0004/\b\u0004\u000b\u0004"+
		"\f\u00040\u0003\u00043\b\u0004\u0001\u0005\u0001\u0005\u0001\u0005\u0004"+
		"\u00058\b\u0005\u000b\u0005\f\u00059\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0004\u0005?\b\u0005\u000b\u0005\f\u0005@\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0004\u0005F\b\u0005\u000b\u0005\f\u0005G\u0001\u0005\u0003\u0005"+
		"K\b\u0005\u0001\u0005\u0001\u0005\u0003\u0005O\b\u0005\u0001\u0005\u0001"+
		"\u0005\u0005\u0005S\b\u0005\n\u0005\f\u0005V\t\u0005\u0003\u0005X\b\u0005"+
		"\u0001\u0006\u0003\u0006[\b\u0006\u0001\u0006\u0004\u0006^\b\u0006\u000b"+
		"\u0006\f\u0006_\u0001\u0006\u0001\u0006\u0004\u0006d\b\u0006\u000b\u0006"+
		"\f\u0006e\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0005\u0007"+
		"l\b\u0007\n\u0007\f\u0007o\t\u0007\u0001\u0007\u0001\u0007\u0001\b\u0001"+
		"\b\u0001\b\u0001\b\u0001\b\u0001\b\u0004\by\b\b\u000b\b\f\bz\u0003\b}"+
		"\b\b\u0001\b\u0001\b\u0001\t\u0004\t\u0082\b\t\u000b\t\f\t\u0083\u0001"+
		"\t\u0001\t\u0001z\u0000\n\u0001\u0001\u0003\u0002\u0005\u0003\u0007\u0004"+
		"\t\u0005\u000b\u0006\r\u0007\u000f\b\u0011\t\u0013\n\u0001\u0000\u000e"+
		"\u0003\u0000AZ__az\u0005\u0000--09AZ__az\t\u0000!!%&*+--//::<>^^||\u0002"+
		"\u0000XXxx\u0003\u000009AZaz\u0002\u0000OOoo\u0001\u000007\u0002\u0000"+
		"BBbb\u0001\u000001\u0002\u0000++--\u0001\u000019\u0001\u000009\u0002\u0000"+
		"\"\"^^\u0002\u0000\t\n  \u009d\u0000\u0001\u0001\u0000\u0000\u0000\u0000"+
		"\u0003\u0001\u0000\u0000\u0000\u0000\u0005\u0001\u0000\u0000\u0000\u0000"+
		"\u0007\u0001\u0000\u0000\u0000\u0000\t\u0001\u0000\u0000\u0000\u0000\u000b"+
		"\u0001\u0000\u0000\u0000\u0000\r\u0001\u0000\u0000\u0000\u0000\u000f\u0001"+
		"\u0000\u0000\u0000\u0000\u0011\u0001\u0000\u0000\u0000\u0000\u0013\u0001"+
		"\u0000\u0000\u0000\u0001\u0015\u0001\u0000\u0000\u0000\u0003\u0017\u0001"+
		"\u0000\u0000\u0000\u0005\u0019\u0001\u0000\u0000\u0000\u0007$\u0001\u0000"+
		"\u0000\u0000\t2\u0001\u0000\u0000\u0000\u000bW\u0001\u0000\u0000\u0000"+
		"\rZ\u0001\u0000\u0000\u0000\u000fg\u0001\u0000\u0000\u0000\u0011r\u0001"+
		"\u0000\u0000\u0000\u0013\u0081\u0001\u0000\u0000\u0000\u0015\u0016\u0005"+
		"=\u0000\u0000\u0016\u0002\u0001\u0000\u0000\u0000\u0017\u0018\u0005(\u0000"+
		"\u0000\u0018\u0004\u0001\u0000\u0000\u0000\u0019\u001a\u0005)\u0000\u0000"+
		"\u001a\u0006\u0001\u0000\u0000\u0000\u001b\u001c\u0005t\u0000\u0000\u001c"+
		"\u001d\u0005r\u0000\u0000\u001d\u001e\u0005u\u0000\u0000\u001e%\u0005"+
		"e\u0000\u0000\u001f \u0005f\u0000\u0000 !\u0005a\u0000\u0000!\"\u0005"+
		"l\u0000\u0000\"#\u0005s\u0000\u0000#%\u0005e\u0000\u0000$\u001b\u0001"+
		"\u0000\u0000\u0000$\u001f\u0001\u0000\u0000\u0000%\b\u0001\u0000\u0000"+
		"\u0000&*\u0007\u0000\u0000\u0000\')\u0007\u0001\u0000\u0000(\'\u0001\u0000"+
		"\u0000\u0000),\u0001\u0000\u0000\u0000*(\u0001\u0000\u0000\u0000*+\u0001"+
		"\u0000\u0000\u0000+3\u0001\u0000\u0000\u0000,*\u0001\u0000\u0000\u0000"+
		"-/\u0007\u0002\u0000\u0000.-\u0001\u0000\u0000\u0000/0\u0001\u0000\u0000"+
		"\u00000.\u0001\u0000\u0000\u000001\u0001\u0000\u0000\u000013\u0001\u0000"+
		"\u0000\u00002&\u0001\u0000\u0000\u00002.\u0001\u0000\u0000\u00003\n\u0001"+
		"\u0000\u0000\u000045\u00050\u0000\u000057\u0007\u0003\u0000\u000068\u0007"+
		"\u0004\u0000\u000076\u0001\u0000\u0000\u000089\u0001\u0000\u0000\u0000"+
		"97\u0001\u0000\u0000\u00009:\u0001\u0000\u0000\u0000:X\u0001\u0000\u0000"+
		"\u0000;<\u00050\u0000\u0000<>\u0007\u0005\u0000\u0000=?\u0007\u0006\u0000"+
		"\u0000>=\u0001\u0000\u0000\u0000?@\u0001\u0000\u0000\u0000@>\u0001\u0000"+
		"\u0000\u0000@A\u0001\u0000\u0000\u0000AX\u0001\u0000\u0000\u0000BC\u0005"+
		"0\u0000\u0000CE\u0007\u0007\u0000\u0000DF\u0007\b\u0000\u0000ED\u0001"+
		"\u0000\u0000\u0000FG\u0001\u0000\u0000\u0000GE\u0001\u0000\u0000\u0000"+
		"GH\u0001\u0000\u0000\u0000HX\u0001\u0000\u0000\u0000IK\u0007\t\u0000\u0000"+
		"JI\u0001\u0000\u0000\u0000JK\u0001\u0000\u0000\u0000KL\u0001\u0000\u0000"+
		"\u0000LX\u00050\u0000\u0000MO\u0007\t\u0000\u0000NM\u0001\u0000\u0000"+
		"\u0000NO\u0001\u0000\u0000\u0000OP\u0001\u0000\u0000\u0000PT\u0007\n\u0000"+
		"\u0000QS\u0007\u000b\u0000\u0000RQ\u0001\u0000\u0000\u0000SV\u0001\u0000"+
		"\u0000\u0000TR\u0001\u0000\u0000\u0000TU\u0001\u0000\u0000\u0000UX\u0001"+
		"\u0000\u0000\u0000VT\u0001\u0000\u0000\u0000W4\u0001\u0000\u0000\u0000"+
		"W;\u0001\u0000\u0000\u0000WB\u0001\u0000\u0000\u0000WJ\u0001\u0000\u0000"+
		"\u0000WN\u0001\u0000\u0000\u0000X\f\u0001\u0000\u0000\u0000Y[\u0007\t"+
		"\u0000\u0000ZY\u0001\u0000\u0000\u0000Z[\u0001\u0000\u0000\u0000[]\u0001"+
		"\u0000\u0000\u0000\\^\u0007\u000b\u0000\u0000]\\\u0001\u0000\u0000\u0000"+
		"^_\u0001\u0000\u0000\u0000_]\u0001\u0000\u0000\u0000_`\u0001\u0000\u0000"+
		"\u0000`a\u0001\u0000\u0000\u0000ac\u0005.\u0000\u0000bd\u0007\u000b\u0000"+
		"\u0000cb\u0001\u0000\u0000\u0000de\u0001\u0000\u0000\u0000ec\u0001\u0000"+
		"\u0000\u0000ef\u0001\u0000\u0000\u0000f\u000e\u0001\u0000\u0000\u0000"+
		"gm\u0005\"\u0000\u0000hl\u0007\f\u0000\u0000ij\u0005\\\u0000\u0000jl\u0005"+
		"\"\u0000\u0000kh\u0001\u0000\u0000\u0000ki\u0001\u0000\u0000\u0000lo\u0001"+
		"\u0000\u0000\u0000mk\u0001\u0000\u0000\u0000mn\u0001\u0000\u0000\u0000"+
		"np\u0001\u0000\u0000\u0000om\u0001\u0000\u0000\u0000pq\u0005\"\u0000\u0000"+
		"q\u0010\u0001\u0000\u0000\u0000r|\u0005\'\u0000\u0000s}\u0007\f\u0000"+
		"\u0000tu\u0005\\\u0000\u0000u}\u0005\"\u0000\u0000vx\u0005\\\u0000\u0000"+
		"wy\t\u0000\u0000\u0000xw\u0001\u0000\u0000\u0000yz\u0001\u0000\u0000\u0000"+
		"z{\u0001\u0000\u0000\u0000zx\u0001\u0000\u0000\u0000{}\u0001\u0000\u0000"+
		"\u0000|s\u0001\u0000\u0000\u0000|t\u0001\u0000\u0000\u0000|v\u0001\u0000"+
		"\u0000\u0000}~\u0001\u0000\u0000\u0000~\u007f\u0005\'\u0000\u0000\u007f"+
		"\u0012\u0001\u0000\u0000\u0000\u0080\u0082\u0007\r\u0000\u0000\u0081\u0080"+
		"\u0001\u0000\u0000\u0000\u0082\u0083\u0001\u0000\u0000\u0000\u0083\u0081"+
		"\u0001\u0000\u0000\u0000\u0083\u0084\u0001\u0000\u0000\u0000\u0084\u0085"+
		"\u0001\u0000\u0000\u0000\u0085\u0086\u0006\t\u0000\u0000\u0086\u0014\u0001"+
		"\u0000\u0000\u0000\u0014\u0000$*029@GJNTWZ_ekmz|\u0083\u0001\u0006\u0000"+
		"\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}