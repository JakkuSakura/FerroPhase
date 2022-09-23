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
		"\u0004\u0000\u000b{\u0006\uffff\uffff\u0002\u0000\u0007\u0000\u0002\u0001"+
		"\u0007\u0001\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004"+
		"\u0007\u0004\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007"+
		"\u0007\u0007\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0001\u0000"+
		"\u0001\u0000\u0001\u0001\u0001\u0001\u0001\u0002\u0001\u0002\u0001\u0003"+
		"\u0001\u0003\u0001\u0004\u0001\u0004\u0001\u0005\u0001\u0005\u0005\u0005"+
		"$\b\u0005\n\u0005\f\u0005\'\t\u0005\u0001\u0006\u0001\u0006\u0001\u0006"+
		"\u0004\u0006,\b\u0006\u000b\u0006\f\u0006-\u0001\u0006\u0001\u0006\u0001"+
		"\u0006\u0004\u00063\b\u0006\u000b\u0006\f\u00064\u0001\u0006\u0001\u0006"+
		"\u0001\u0006\u0004\u0006:\b\u0006\u000b\u0006\f\u0006;\u0001\u0006\u0003"+
		"\u0006?\b\u0006\u0001\u0006\u0001\u0006\u0003\u0006C\b\u0006\u0001\u0006"+
		"\u0001\u0006\u0005\u0006G\b\u0006\n\u0006\f\u0006J\t\u0006\u0003\u0006"+
		"L\b\u0006\u0001\u0007\u0003\u0007O\b\u0007\u0001\u0007\u0004\u0007R\b"+
		"\u0007\u000b\u0007\f\u0007S\u0001\u0007\u0001\u0007\u0004\u0007X\b\u0007"+
		"\u000b\u0007\f\u0007Y\u0001\b\u0001\b\u0001\b\u0001\b\u0005\b`\b\b\n\b"+
		"\f\bc\t\b\u0001\b\u0001\b\u0001\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001"+
		"\t\u0004\tm\b\t\u000b\t\f\tn\u0003\tq\b\t\u0001\t\u0001\t\u0001\n\u0004"+
		"\nv\b\n\u000b\n\f\nw\u0001\n\u0001\n\u0001n\u0000\u000b\u0001\u0001\u0003"+
		"\u0002\u0005\u0003\u0007\u0004\t\u0005\u000b\u0006\r\u0007\u000f\b\u0011"+
		"\t\u0013\n\u0015\u000b\u0001\u0000\r\u0003\u0000AZ__az\u0005\u0000--0"+
		"9AZ__az\u0002\u0000XXxx\u0003\u000009AZaz\u0002\u0000OOoo\u0001\u0000"+
		"07\u0002\u0000BBbb\u0001\u000001\u0002\u0000++--\u0001\u000019\u0001\u0000"+
		"09\u0002\u0000\"\"^^\u0002\u0000\t\n  \u008e\u0000\u0001\u0001\u0000\u0000"+
		"\u0000\u0000\u0003\u0001\u0000\u0000\u0000\u0000\u0005\u0001\u0000\u0000"+
		"\u0000\u0000\u0007\u0001\u0000\u0000\u0000\u0000\t\u0001\u0000\u0000\u0000"+
		"\u0000\u000b\u0001\u0000\u0000\u0000\u0000\r\u0001\u0000\u0000\u0000\u0000"+
		"\u000f\u0001\u0000\u0000\u0000\u0000\u0011\u0001\u0000\u0000\u0000\u0000"+
		"\u0013\u0001\u0000\u0000\u0000\u0000\u0015\u0001\u0000\u0000\u0000\u0001"+
		"\u0017\u0001\u0000\u0000\u0000\u0003\u0019\u0001\u0000\u0000\u0000\u0005"+
		"\u001b\u0001\u0000\u0000\u0000\u0007\u001d\u0001\u0000\u0000\u0000\t\u001f"+
		"\u0001\u0000\u0000\u0000\u000b!\u0001\u0000\u0000\u0000\rK\u0001\u0000"+
		"\u0000\u0000\u000fN\u0001\u0000\u0000\u0000\u0011[\u0001\u0000\u0000\u0000"+
		"\u0013f\u0001\u0000\u0000\u0000\u0015u\u0001\u0000\u0000\u0000\u0017\u0018"+
		"\u0005=\u0000\u0000\u0018\u0002\u0001\u0000\u0000\u0000\u0019\u001a\u0005"+
		"(\u0000\u0000\u001a\u0004\u0001\u0000\u0000\u0000\u001b\u001c\u0005)\u0000"+
		"\u0000\u001c\u0006\u0001\u0000\u0000\u0000\u001d\u001e\u0005[\u0000\u0000"+
		"\u001e\b\u0001\u0000\u0000\u0000\u001f \u0005]\u0000\u0000 \n\u0001\u0000"+
		"\u0000\u0000!%\u0007\u0000\u0000\u0000\"$\u0007\u0001\u0000\u0000#\"\u0001"+
		"\u0000\u0000\u0000$\'\u0001\u0000\u0000\u0000%#\u0001\u0000\u0000\u0000"+
		"%&\u0001\u0000\u0000\u0000&\f\u0001\u0000\u0000\u0000\'%\u0001\u0000\u0000"+
		"\u0000()\u00050\u0000\u0000)+\u0007\u0002\u0000\u0000*,\u0007\u0003\u0000"+
		"\u0000+*\u0001\u0000\u0000\u0000,-\u0001\u0000\u0000\u0000-+\u0001\u0000"+
		"\u0000\u0000-.\u0001\u0000\u0000\u0000.L\u0001\u0000\u0000\u0000/0\u0005"+
		"0\u0000\u000002\u0007\u0004\u0000\u000013\u0007\u0005\u0000\u000021\u0001"+
		"\u0000\u0000\u000034\u0001\u0000\u0000\u000042\u0001\u0000\u0000\u0000"+
		"45\u0001\u0000\u0000\u00005L\u0001\u0000\u0000\u000067\u00050\u0000\u0000"+
		"79\u0007\u0006\u0000\u00008:\u0007\u0007\u0000\u000098\u0001\u0000\u0000"+
		"\u0000:;\u0001\u0000\u0000\u0000;9\u0001\u0000\u0000\u0000;<\u0001\u0000"+
		"\u0000\u0000<L\u0001\u0000\u0000\u0000=?\u0007\b\u0000\u0000>=\u0001\u0000"+
		"\u0000\u0000>?\u0001\u0000\u0000\u0000?@\u0001\u0000\u0000\u0000@L\u0005"+
		"0\u0000\u0000AC\u0007\b\u0000\u0000BA\u0001\u0000\u0000\u0000BC\u0001"+
		"\u0000\u0000\u0000CD\u0001\u0000\u0000\u0000DH\u0007\t\u0000\u0000EG\u0007"+
		"\n\u0000\u0000FE\u0001\u0000\u0000\u0000GJ\u0001\u0000\u0000\u0000HF\u0001"+
		"\u0000\u0000\u0000HI\u0001\u0000\u0000\u0000IL\u0001\u0000\u0000\u0000"+
		"JH\u0001\u0000\u0000\u0000K(\u0001\u0000\u0000\u0000K/\u0001\u0000\u0000"+
		"\u0000K6\u0001\u0000\u0000\u0000K>\u0001\u0000\u0000\u0000KB\u0001\u0000"+
		"\u0000\u0000L\u000e\u0001\u0000\u0000\u0000MO\u0007\b\u0000\u0000NM\u0001"+
		"\u0000\u0000\u0000NO\u0001\u0000\u0000\u0000OQ\u0001\u0000\u0000\u0000"+
		"PR\u0007\n\u0000\u0000QP\u0001\u0000\u0000\u0000RS\u0001\u0000\u0000\u0000"+
		"SQ\u0001\u0000\u0000\u0000ST\u0001\u0000\u0000\u0000TU\u0001\u0000\u0000"+
		"\u0000UW\u0005.\u0000\u0000VX\u0007\n\u0000\u0000WV\u0001\u0000\u0000"+
		"\u0000XY\u0001\u0000\u0000\u0000YW\u0001\u0000\u0000\u0000YZ\u0001\u0000"+
		"\u0000\u0000Z\u0010\u0001\u0000\u0000\u0000[a\u0005\"\u0000\u0000\\`\u0007"+
		"\u000b\u0000\u0000]^\u0005\\\u0000\u0000^`\u0005\"\u0000\u0000_\\\u0001"+
		"\u0000\u0000\u0000_]\u0001\u0000\u0000\u0000`c\u0001\u0000\u0000\u0000"+
		"a_\u0001\u0000\u0000\u0000ab\u0001\u0000\u0000\u0000bd\u0001\u0000\u0000"+
		"\u0000ca\u0001\u0000\u0000\u0000de\u0005\"\u0000\u0000e\u0012\u0001\u0000"+
		"\u0000\u0000fp\u0005\'\u0000\u0000gq\u0007\u000b\u0000\u0000hi\u0005\\"+
		"\u0000\u0000iq\u0005\"\u0000\u0000jl\u0005\\\u0000\u0000km\t\u0000\u0000"+
		"\u0000lk\u0001\u0000\u0000\u0000mn\u0001\u0000\u0000\u0000no\u0001\u0000"+
		"\u0000\u0000nl\u0001\u0000\u0000\u0000oq\u0001\u0000\u0000\u0000pg\u0001"+
		"\u0000\u0000\u0000ph\u0001\u0000\u0000\u0000pj\u0001\u0000\u0000\u0000"+
		"qr\u0001\u0000\u0000\u0000rs\u0005\'\u0000\u0000s\u0014\u0001\u0000\u0000"+
		"\u0000tv\u0007\f\u0000\u0000ut\u0001\u0000\u0000\u0000vw\u0001\u0000\u0000"+
		"\u0000wu\u0001\u0000\u0000\u0000wx\u0001\u0000\u0000\u0000xy\u0001\u0000"+
		"\u0000\u0000yz\u0006\n\u0000\u0000z\u0016\u0001\u0000\u0000\u0000\u0011"+
		"\u0000%-4;>BHKNSY_anpw\u0001\u0006\u0000\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}