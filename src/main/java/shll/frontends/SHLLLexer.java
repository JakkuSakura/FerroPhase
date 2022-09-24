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
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, IDENT=6, INTEGER=7, DECIMAL=8, 
		STRING=9, CHAR=10, WS=11;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "T__2", "T__3", "T__4", "IDENT", "INTEGER", "DECIMAL", 
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
			null, null, null, null, null, null, "IDENT", "INTEGER", "DECIMAL", "STRING", 
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
		"\u0004\u0000\u000b\u0082\u0006\uffff\uffff\u0002\u0000\u0007\u0000\u0002"+
		"\u0001\u0007\u0001\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002"+
		"\u0004\u0007\u0004\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002"+
		"\u0007\u0007\u0007\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0001"+
		"\u0000\u0001\u0000\u0001\u0001\u0001\u0001\u0001\u0002\u0001\u0002\u0001"+
		"\u0003\u0001\u0003\u0001\u0004\u0001\u0004\u0001\u0005\u0001\u0005\u0005"+
		"\u0005$\b\u0005\n\u0005\f\u0005\'\t\u0005\u0001\u0005\u0004\u0005*\b\u0005"+
		"\u000b\u0005\f\u0005+\u0003\u0005.\b\u0005\u0001\u0006\u0001\u0006\u0001"+
		"\u0006\u0004\u00063\b\u0006\u000b\u0006\f\u00064\u0001\u0006\u0001\u0006"+
		"\u0001\u0006\u0004\u0006:\b\u0006\u000b\u0006\f\u0006;\u0001\u0006\u0001"+
		"\u0006\u0001\u0006\u0004\u0006A\b\u0006\u000b\u0006\f\u0006B\u0001\u0006"+
		"\u0003\u0006F\b\u0006\u0001\u0006\u0001\u0006\u0003\u0006J\b\u0006\u0001"+
		"\u0006\u0001\u0006\u0005\u0006N\b\u0006\n\u0006\f\u0006Q\t\u0006\u0003"+
		"\u0006S\b\u0006\u0001\u0007\u0003\u0007V\b\u0007\u0001\u0007\u0004\u0007"+
		"Y\b\u0007\u000b\u0007\f\u0007Z\u0001\u0007\u0001\u0007\u0004\u0007_\b"+
		"\u0007\u000b\u0007\f\u0007`\u0001\b\u0001\b\u0001\b\u0001\b\u0005\bg\b"+
		"\b\n\b\f\bj\t\b\u0001\b\u0001\b\u0001\t\u0001\t\u0001\t\u0001\t\u0001"+
		"\t\u0001\t\u0004\tt\b\t\u000b\t\f\tu\u0003\tx\b\t\u0001\t\u0001\t\u0001"+
		"\n\u0004\n}\b\n\u000b\n\f\n~\u0001\n\u0001\n\u0001u\u0000\u000b\u0001"+
		"\u0001\u0003\u0002\u0005\u0003\u0007\u0004\t\u0005\u000b\u0006\r\u0007"+
		"\u000f\b\u0011\t\u0013\n\u0015\u000b\u0001\u0000\u000e\u0003\u0000AZ_"+
		"_az\u0005\u0000--09AZ__az\t\u0000!!%&*+--//::<>^^||\u0002\u0000XXxx\u0003"+
		"\u000009AZaz\u0002\u0000OOoo\u0001\u000007\u0002\u0000BBbb\u0001\u0000"+
		"01\u0002\u0000++--\u0001\u000019\u0001\u000009\u0002\u0000\"\"^^\u0002"+
		"\u0000\t\n  \u0097\u0000\u0001\u0001\u0000\u0000\u0000\u0000\u0003\u0001"+
		"\u0000\u0000\u0000\u0000\u0005\u0001\u0000\u0000\u0000\u0000\u0007\u0001"+
		"\u0000\u0000\u0000\u0000\t\u0001\u0000\u0000\u0000\u0000\u000b\u0001\u0000"+
		"\u0000\u0000\u0000\r\u0001\u0000\u0000\u0000\u0000\u000f\u0001\u0000\u0000"+
		"\u0000\u0000\u0011\u0001\u0000\u0000\u0000\u0000\u0013\u0001\u0000\u0000"+
		"\u0000\u0000\u0015\u0001\u0000\u0000\u0000\u0001\u0017\u0001\u0000\u0000"+
		"\u0000\u0003\u0019\u0001\u0000\u0000\u0000\u0005\u001b\u0001\u0000\u0000"+
		"\u0000\u0007\u001d\u0001\u0000\u0000\u0000\t\u001f\u0001\u0000\u0000\u0000"+
		"\u000b-\u0001\u0000\u0000\u0000\rR\u0001\u0000\u0000\u0000\u000fU\u0001"+
		"\u0000\u0000\u0000\u0011b\u0001\u0000\u0000\u0000\u0013m\u0001\u0000\u0000"+
		"\u0000\u0015|\u0001\u0000\u0000\u0000\u0017\u0018\u0005=\u0000\u0000\u0018"+
		"\u0002\u0001\u0000\u0000\u0000\u0019\u001a\u0005(\u0000\u0000\u001a\u0004"+
		"\u0001\u0000\u0000\u0000\u001b\u001c\u0005)\u0000\u0000\u001c\u0006\u0001"+
		"\u0000\u0000\u0000\u001d\u001e\u0005[\u0000\u0000\u001e\b\u0001\u0000"+
		"\u0000\u0000\u001f \u0005]\u0000\u0000 \n\u0001\u0000\u0000\u0000!%\u0007"+
		"\u0000\u0000\u0000\"$\u0007\u0001\u0000\u0000#\"\u0001\u0000\u0000\u0000"+
		"$\'\u0001\u0000\u0000\u0000%#\u0001\u0000\u0000\u0000%&\u0001\u0000\u0000"+
		"\u0000&.\u0001\u0000\u0000\u0000\'%\u0001\u0000\u0000\u0000(*\u0007\u0002"+
		"\u0000\u0000)(\u0001\u0000\u0000\u0000*+\u0001\u0000\u0000\u0000+)\u0001"+
		"\u0000\u0000\u0000+,\u0001\u0000\u0000\u0000,.\u0001\u0000\u0000\u0000"+
		"-!\u0001\u0000\u0000\u0000-)\u0001\u0000\u0000\u0000.\f\u0001\u0000\u0000"+
		"\u0000/0\u00050\u0000\u000002\u0007\u0003\u0000\u000013\u0007\u0004\u0000"+
		"\u000021\u0001\u0000\u0000\u000034\u0001\u0000\u0000\u000042\u0001\u0000"+
		"\u0000\u000045\u0001\u0000\u0000\u00005S\u0001\u0000\u0000\u000067\u0005"+
		"0\u0000\u000079\u0007\u0005\u0000\u00008:\u0007\u0006\u0000\u000098\u0001"+
		"\u0000\u0000\u0000:;\u0001\u0000\u0000\u0000;9\u0001\u0000\u0000\u0000"+
		";<\u0001\u0000\u0000\u0000<S\u0001\u0000\u0000\u0000=>\u00050\u0000\u0000"+
		">@\u0007\u0007\u0000\u0000?A\u0007\b\u0000\u0000@?\u0001\u0000\u0000\u0000"+
		"AB\u0001\u0000\u0000\u0000B@\u0001\u0000\u0000\u0000BC\u0001\u0000\u0000"+
		"\u0000CS\u0001\u0000\u0000\u0000DF\u0007\t\u0000\u0000ED\u0001\u0000\u0000"+
		"\u0000EF\u0001\u0000\u0000\u0000FG\u0001\u0000\u0000\u0000GS\u00050\u0000"+
		"\u0000HJ\u0007\t\u0000\u0000IH\u0001\u0000\u0000\u0000IJ\u0001\u0000\u0000"+
		"\u0000JK\u0001\u0000\u0000\u0000KO\u0007\n\u0000\u0000LN\u0007\u000b\u0000"+
		"\u0000ML\u0001\u0000\u0000\u0000NQ\u0001\u0000\u0000\u0000OM\u0001\u0000"+
		"\u0000\u0000OP\u0001\u0000\u0000\u0000PS\u0001\u0000\u0000\u0000QO\u0001"+
		"\u0000\u0000\u0000R/\u0001\u0000\u0000\u0000R6\u0001\u0000\u0000\u0000"+
		"R=\u0001\u0000\u0000\u0000RE\u0001\u0000\u0000\u0000RI\u0001\u0000\u0000"+
		"\u0000S\u000e\u0001\u0000\u0000\u0000TV\u0007\t\u0000\u0000UT\u0001\u0000"+
		"\u0000\u0000UV\u0001\u0000\u0000\u0000VX\u0001\u0000\u0000\u0000WY\u0007"+
		"\u000b\u0000\u0000XW\u0001\u0000\u0000\u0000YZ\u0001\u0000\u0000\u0000"+
		"ZX\u0001\u0000\u0000\u0000Z[\u0001\u0000\u0000\u0000[\\\u0001\u0000\u0000"+
		"\u0000\\^\u0005.\u0000\u0000]_\u0007\u000b\u0000\u0000^]\u0001\u0000\u0000"+
		"\u0000_`\u0001\u0000\u0000\u0000`^\u0001\u0000\u0000\u0000`a\u0001\u0000"+
		"\u0000\u0000a\u0010\u0001\u0000\u0000\u0000bh\u0005\"\u0000\u0000cg\u0007"+
		"\f\u0000\u0000de\u0005\\\u0000\u0000eg\u0005\"\u0000\u0000fc\u0001\u0000"+
		"\u0000\u0000fd\u0001\u0000\u0000\u0000gj\u0001\u0000\u0000\u0000hf\u0001"+
		"\u0000\u0000\u0000hi\u0001\u0000\u0000\u0000ik\u0001\u0000\u0000\u0000"+
		"jh\u0001\u0000\u0000\u0000kl\u0005\"\u0000\u0000l\u0012\u0001\u0000\u0000"+
		"\u0000mw\u0005\'\u0000\u0000nx\u0007\f\u0000\u0000op\u0005\\\u0000\u0000"+
		"px\u0005\"\u0000\u0000qs\u0005\\\u0000\u0000rt\t\u0000\u0000\u0000sr\u0001"+
		"\u0000\u0000\u0000tu\u0001\u0000\u0000\u0000uv\u0001\u0000\u0000\u0000"+
		"us\u0001\u0000\u0000\u0000vx\u0001\u0000\u0000\u0000wn\u0001\u0000\u0000"+
		"\u0000wo\u0001\u0000\u0000\u0000wq\u0001\u0000\u0000\u0000xy\u0001\u0000"+
		"\u0000\u0000yz\u0005\'\u0000\u0000z\u0014\u0001\u0000\u0000\u0000{}\u0007"+
		"\r\u0000\u0000|{\u0001\u0000\u0000\u0000}~\u0001\u0000\u0000\u0000~|\u0001"+
		"\u0000\u0000\u0000~\u007f\u0001\u0000\u0000\u0000\u007f\u0080\u0001\u0000"+
		"\u0000\u0000\u0080\u0081\u0006\n\u0000\u0000\u0081\u0016\u0001\u0000\u0000"+
		"\u0000\u0013\u0000%+-4;BEIORUZ`fhuw~\u0001\u0006\u0000\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}