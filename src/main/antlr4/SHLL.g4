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

program: term * EOF;

blocked: '{' term* '}' ;
block: 'block' blocked;
anno: ':' term # Single
      | ':' '*' term # List
      | ':' '**' term # Dict
      ;
param: IDENT anno? ;
kwArg: IDENT anno? '=' value=term # WithRename
       | IDENT # WithoutRename
       ;
posArg: term ;
arg: kwArg | posArg ;
let: 'let' id=IDENT anno? '=' value=term # Intialized
    | 'let' id=IDENT # Uninitialized;
for: 'for' IDENT 'in' term blocked # ForEach
    | 'for' blocked # Loop
    | 'for' term blocked # While;
structof: 'structof' IDENT? '{' param* '}' ;
struct: 'struct' IDENT? '{' kwArg* '}' ;
enumof: 'enumof' IDENT? '{' param* '}' ;
enum: 'enum' IDENT? '{' kwArg* '}' ;
traitof: 'traitof' IDENT? '{' let* '}' ;
trait: 'trait' IDENT? '{' let* '}' ;
funof: '(' term * ')' '->' term ;
fatArrow: '=>' (blocked | term);
fun: '(' param* ')' ('->' ret=term)? fatArrow;
kindof: 'kindof' IDENT? '{' param* '}' ;
kind: 'kind' IDENT? '{' kwArg* '}' ;
when: 'when' cond=term body=fatArrow;
case: 'case' '{' when* '}' ;
generic: '[' param* ']' body=fatArrow;
deref: '*' term # DerefTuple
    | '**' term # DerefDict;
selector: '.' IDENT;
implicitApplier: '[' arg* ']';
positionalApplier: '(' arg* ')';
namedApplier: '{' kwArg * '}';
assigner: '=' term;

term: (block | generic | let | for | structof | struct | enumof | enum | traitof | trait |
            funof | fun | kindof | kind | case | deref
        | BOOL | IDENT | INTEGER | DECIMAL | STRING | CHAR)
     (selector| implicitApplier | positionalApplier | namedApplier | assigner) *;
