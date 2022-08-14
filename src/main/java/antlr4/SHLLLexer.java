// Generated from src/main/antlr4/SHLL.g4 by ANTLR 4.10.1
package antlr4;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class SHLLLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.10.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, IDENT=4, INTEGER=5, DECIMAL=6, STRING=7, CHAR=8, 
		WS=9;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "T__2", "IDENT", "INTEGER", "DECIMAL", "STRING", "CHAR", 
			"WS"
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
		"\u0004\u0000\ts\u0006\uffff\uffff\u0002\u0000\u0007\u0000\u0002\u0001"+
		"\u0007\u0001\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004"+
		"\u0007\u0004\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007"+
		"\u0007\u0007\u0002\b\u0007\b\u0001\u0000\u0001\u0000\u0001\u0001\u0001"+
		"\u0001\u0001\u0002\u0001\u0002\u0001\u0003\u0001\u0003\u0005\u0003\u001c"+
		"\b\u0003\n\u0003\f\u0003\u001f\t\u0003\u0001\u0004\u0001\u0004\u0001\u0004"+
		"\u0004\u0004$\b\u0004\u000b\u0004\f\u0004%\u0001\u0004\u0001\u0004\u0001"+
		"\u0004\u0004\u0004+\b\u0004\u000b\u0004\f\u0004,\u0001\u0004\u0001\u0004"+
		"\u0001\u0004\u0004\u00042\b\u0004\u000b\u0004\f\u00043\u0001\u0004\u0003"+
		"\u00047\b\u0004\u0001\u0004\u0001\u0004\u0003\u0004;\b\u0004\u0001\u0004"+
		"\u0001\u0004\u0005\u0004?\b\u0004\n\u0004\f\u0004B\t\u0004\u0003\u0004"+
		"D\b\u0004\u0001\u0005\u0003\u0005G\b\u0005\u0001\u0005\u0004\u0005J\b"+
		"\u0005\u000b\u0005\f\u0005K\u0001\u0005\u0001\u0005\u0004\u0005P\b\u0005"+
		"\u000b\u0005\f\u0005Q\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006"+
		"\u0005\u0006X\b\u0006\n\u0006\f\u0006[\t\u0006\u0001\u0006\u0001\u0006"+
		"\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0004\u0007e\b\u0007\u000b\u0007\f\u0007f\u0003\u0007i\b\u0007\u0001"+
		"\u0007\u0001\u0007\u0001\b\u0004\bn\b\b\u000b\b\f\bo\u0001\b\u0001\b\u0001"+
		"f\u0000\t\u0001\u0001\u0003\u0002\u0005\u0003\u0007\u0004\t\u0005\u000b"+
		"\u0006\r\u0007\u000f\b\u0011\t\u0001\u0000\r\u0003\u0000AZ__az\u0005\u0000"+
		"--09AZ__az\u0002\u0000XXxx\u0003\u000009AZaz\u0002\u0000OOoo\u0001\u0000"+
		"07\u0002\u0000BBbb\u0001\u000001\u0002\u0000++--\u0001\u000019\u0001\u0000"+
		"09\u0002\u0000\"\"^^\u0002\u0000\t\n  \u0086\u0000\u0001\u0001\u0000\u0000"+
		"\u0000\u0000\u0003\u0001\u0000\u0000\u0000\u0000\u0005\u0001\u0000\u0000"+
		"\u0000\u0000\u0007\u0001\u0000\u0000\u0000\u0000\t\u0001\u0000\u0000\u0000"+
		"\u0000\u000b\u0001\u0000\u0000\u0000\u0000\r\u0001\u0000\u0000\u0000\u0000"+
		"\u000f\u0001\u0000\u0000\u0000\u0000\u0011\u0001\u0000\u0000\u0000\u0001"+
		"\u0013\u0001\u0000\u0000\u0000\u0003\u0015\u0001\u0000\u0000\u0000\u0005"+
		"\u0017\u0001\u0000\u0000\u0000\u0007\u0019\u0001\u0000\u0000\u0000\tC"+
		"\u0001\u0000\u0000\u0000\u000bF\u0001\u0000\u0000\u0000\rS\u0001\u0000"+
		"\u0000\u0000\u000f^\u0001\u0000\u0000\u0000\u0011m\u0001\u0000\u0000\u0000"+
		"\u0013\u0014\u0005=\u0000\u0000\u0014\u0002\u0001\u0000\u0000\u0000\u0015"+
		"\u0016\u0005(\u0000\u0000\u0016\u0004\u0001\u0000\u0000\u0000\u0017\u0018"+
		"\u0005)\u0000\u0000\u0018\u0006\u0001\u0000\u0000\u0000\u0019\u001d\u0007"+
		"\u0000\u0000\u0000\u001a\u001c\u0007\u0001\u0000\u0000\u001b\u001a\u0001"+
		"\u0000\u0000\u0000\u001c\u001f\u0001\u0000\u0000\u0000\u001d\u001b\u0001"+
		"\u0000\u0000\u0000\u001d\u001e\u0001\u0000\u0000\u0000\u001e\b\u0001\u0000"+
		"\u0000\u0000\u001f\u001d\u0001\u0000\u0000\u0000 !\u00050\u0000\u0000"+
		"!#\u0007\u0002\u0000\u0000\"$\u0007\u0003\u0000\u0000#\"\u0001\u0000\u0000"+
		"\u0000$%\u0001\u0000\u0000\u0000%#\u0001\u0000\u0000\u0000%&\u0001\u0000"+
		"\u0000\u0000&D\u0001\u0000\u0000\u0000\'(\u00050\u0000\u0000(*\u0007\u0004"+
		"\u0000\u0000)+\u0007\u0005\u0000\u0000*)\u0001\u0000\u0000\u0000+,\u0001"+
		"\u0000\u0000\u0000,*\u0001\u0000\u0000\u0000,-\u0001\u0000\u0000\u0000"+
		"-D\u0001\u0000\u0000\u0000./\u00050\u0000\u0000/1\u0007\u0006\u0000\u0000"+
		"02\u0007\u0007\u0000\u000010\u0001\u0000\u0000\u000023\u0001\u0000\u0000"+
		"\u000031\u0001\u0000\u0000\u000034\u0001\u0000\u0000\u00004D\u0001\u0000"+
		"\u0000\u000057\u0007\b\u0000\u000065\u0001\u0000\u0000\u000067\u0001\u0000"+
		"\u0000\u000078\u0001\u0000\u0000\u00008D\u00050\u0000\u00009;\u0007\b"+
		"\u0000\u0000:9\u0001\u0000\u0000\u0000:;\u0001\u0000\u0000\u0000;<\u0001"+
		"\u0000\u0000\u0000<@\u0007\t\u0000\u0000=?\u0007\n\u0000\u0000>=\u0001"+
		"\u0000\u0000\u0000?B\u0001\u0000\u0000\u0000@>\u0001\u0000\u0000\u0000"+
		"@A\u0001\u0000\u0000\u0000AD\u0001\u0000\u0000\u0000B@\u0001\u0000\u0000"+
		"\u0000C \u0001\u0000\u0000\u0000C\'\u0001\u0000\u0000\u0000C.\u0001\u0000"+
		"\u0000\u0000C6\u0001\u0000\u0000\u0000C:\u0001\u0000\u0000\u0000D\n\u0001"+
		"\u0000\u0000\u0000EG\u0007\b\u0000\u0000FE\u0001\u0000\u0000\u0000FG\u0001"+
		"\u0000\u0000\u0000GI\u0001\u0000\u0000\u0000HJ\u0007\n\u0000\u0000IH\u0001"+
		"\u0000\u0000\u0000JK\u0001\u0000\u0000\u0000KI\u0001\u0000\u0000\u0000"+
		"KL\u0001\u0000\u0000\u0000LM\u0001\u0000\u0000\u0000MO\u0005.\u0000\u0000"+
		"NP\u0007\n\u0000\u0000ON\u0001\u0000\u0000\u0000PQ\u0001\u0000\u0000\u0000"+
		"QO\u0001\u0000\u0000\u0000QR\u0001\u0000\u0000\u0000R\f\u0001\u0000\u0000"+
		"\u0000SY\u0005\"\u0000\u0000TX\u0007\u000b\u0000\u0000UV\u0005\\\u0000"+
		"\u0000VX\u0005\"\u0000\u0000WT\u0001\u0000\u0000\u0000WU\u0001\u0000\u0000"+
		"\u0000X[\u0001\u0000\u0000\u0000YW\u0001\u0000\u0000\u0000YZ\u0001\u0000"+
		"\u0000\u0000Z\\\u0001\u0000\u0000\u0000[Y\u0001\u0000\u0000\u0000\\]\u0005"+
		"\"\u0000\u0000]\u000e\u0001\u0000\u0000\u0000^h\u0005\'\u0000\u0000_i"+
		"\u0007\u000b\u0000\u0000`a\u0005\\\u0000\u0000ai\u0005\"\u0000\u0000b"+
		"d\u0005\\\u0000\u0000ce\t\u0000\u0000\u0000dc\u0001\u0000\u0000\u0000"+
		"ef\u0001\u0000\u0000\u0000fg\u0001\u0000\u0000\u0000fd\u0001\u0000\u0000"+
		"\u0000gi\u0001\u0000\u0000\u0000h_\u0001\u0000\u0000\u0000h`\u0001\u0000"+
		"\u0000\u0000hb\u0001\u0000\u0000\u0000ij\u0001\u0000\u0000\u0000jk\u0005"+
		"\'\u0000\u0000k\u0010\u0001\u0000\u0000\u0000ln\u0007\f\u0000\u0000ml"+
		"\u0001\u0000\u0000\u0000no\u0001\u0000\u0000\u0000om\u0001\u0000\u0000"+
		"\u0000op\u0001\u0000\u0000\u0000pq\u0001\u0000\u0000\u0000qr\u0006\b\u0000"+
		"\u0000r\u0012\u0001\u0000\u0000\u0000\u0011\u0000\u001d%,36:@CFKQWYfh"+
		"o\u0001\u0006\u0000\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}