grammar SHLL;
BOOL: 'true' | 'false';
IDENT: [a-zA-Z_][a-zA-Z0-9_\-]* | ('+'|'-'|'*'|'/'|'%'|'&'|'|'|'^'|'='|'!'|'<'|'>'|':')+;
INTEGER: '0' [Xx] [a-zA-Z0-9]+
       | '0' [Oo] [0-7]+
       | '0' [Bb] [0-1]+
       | [+-]? '0'
       | [+-]? [1-9][0-9]*;

DECIMAL: [+-]? [0-9]+ '.' [0-9]+;

STRING: '"' ([^"]|'\\"')* '"';
CHAR: '\'' ([^"]|'\\"'|'\\'.+?) '\'';
WS : (' ' | '\t' | '\n' )+ -> skip;

COMMENT
: '/*' .*? '*/' -> skip
;
LINE_COMMENT
: '//' ~[\r\n]* -> skip
;

literal: BOOL | IDENT | INTEGER | DECIMAL | STRING | CHAR;

blocked: '{' term* '}' ;
block: 'block' blocked;
anno: ':' term ;
default: '=' term ;
param: IDENT anno? ;
kwArg: IDENT anno? default | IDENT ;
posArg: term ;
arg: kwArg | posArg ;
let: 'let' kwArg ;
for: 'for' IDENT 'in' term blocked | 'for' blocked | 'for' term blocked ;
structof: 'structof' IDENT? '{' param* '}' ;
struct: 'struct' IDENT? '{' kwArg* '}' ;
enumof: 'enumof' IDENT? '{' param* '}' ;
enum: 'enum' IDENT? '{' kwArg* '}' ;
traitof: 'traitof' IDENT? '{' let* '}' ;
trait: 'trait' IDENT? '{' let* '}' ;
funof: '(' term * ')' '->' term ;
fun: '(' param* ')' '=>' (blocked | term);
kindof: 'kindof' IDENT? '{' param* '}' ;
kind: 'kind' IDENT? '{' kwArg* '}' ;
when: 'when' term '=>' term ;
case: 'case' '{' when* '}' ;
generic: '[' param* ']' '=>' (blocked | term);

selector: '.' IDENT;
implicitApplier: '[' arg* ']';
positionalApplier: '(' arg* ')';
namedApplier: '{' kwArg * '}';
assigner: '=' term;

term: (block | generic | let | for | structof | struct | enumof | enum | traitof | trait |
            funof | fun | kindof | kind | case
        | literal) term1;
term1: (selector| implicitApplier | positionalApplier | namedApplier | assigner) term1 | ;

program: term EOF;
