grammar SHLL;
IDENT: [a-zA-Z_][a-zA-Z0-9_\-]* | ('+'|'-'|'*'|'/'|'%'|'&'|'|'|'^'|'='|'!'|'<'|'>'|':')+;
BOOL: 'true' | 'false';
INTEGER: '0' [Xx] [a-zA-Z0-9]+
       | '0' [Oo] [0-7]+
       | '0' [Bb] [0-1]+
       | [+-]? '0'
       | [+-]? [1-9][0-9]*;

DECIMAL: [+-]? [0-9]+ '.' [0-9]+;

STRING: '"' ([^"]|'\\"')* '"';
CHAR: '\'' ([^"]|'\\"'|'\\'.+?) '\'';
WS : (' ' | '\t' | '\n' )+ -> skip;

term: apply | applyType | BOOL | IDENT | INTEGER | DECIMAL | STRING | CHAR ;
kwArg: IDENT '=' term;
kwArgs: kwArg *;
posArgs: term *;
apply: '(' term posArgs kwArgs ')';
applyType: '[' term posArgs kwArgs ']';

program: term EOF;
