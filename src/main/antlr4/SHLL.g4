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
anno: ':' term # SingleType
      | ':' '*' term # ListType
      | ':' '**' term # DictType
      ;
param: IDENT anno? ('=' default=term)?;
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
// struct { v } where v is a type
// struct { v: v } ok
// struct { v: v = vv } ok default value
// struct { v = vv } ok infered type and default value

struct: 'struct' IDENT? '{' param* '}' ;
// dict { v } where v is a value
// dict { v: v } error no value provided, null type not allowed
// dict { v: v = vv } ok type and value
// dict { v = vv } ok infered type and value
dict: 'dict' IDENT? '{' kwArg* '}' ;
// enum { A }.A kwArg
// enum { A = 1 }.A kwArg
// enum { A: int = 1 }.A kwArg
// let f: E.A = enum E { A = Foo }.A {}
enum: 'enum' IDENT? '{' kwArg* '}' ;
trait: 'trait' IDENT? '{' let* '}' ;
tuple: '(' term * ')';
// return type
// param -> type
narrowArrow: '->' ret=term;
// parameter to a value
// param => value
fatArrow: '=>' (blocked | term);
// (a) => a
// a => a
// (a, b) => a + b
// (a => b => a + b)(1)(2)
// a as parameter

// value that have a type
// pseudo: -> type => value
// a (-> a => a + 1)
// a: int (-> int => a + 1)
doubleArrow: narrowArrow fatArrow;

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

term: (block | generic | let | for | struct | dict | enum | trait |
            tuple | kind | case | deref
        | BOOL | IDENT | INTEGER | DECIMAL | STRING | CHAR)
     (selector| implicitApplier | positionalApplier | namedApplier | assigner | doubleArrow | narrowArrow) *;
