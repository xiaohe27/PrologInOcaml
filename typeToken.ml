type token = 
| STRING of (string)
| VARIABLE of (string)
| NAME of (string)
| UNSIGNEDFLOAT of (float)
| UNSIGNEDINTEGER of (int)
| SIGNEDFLOAT of (float)
| SIGNEDINTEGER of (int)
| DOT
| DOUBLEDOT
| COLONHYPHEN
| ARROW
| NOT
| TERM_EQ
| TERM_INEQ
| IS 
| TERM_DECOMP
| TERM_UNIFY
| TERM_NOTUNIFY
| ARITH_EQ
| ARITH_INEQ
| ARITH_LESS
| ARITH_GREATER
| ARITH_GEQ
| ARITH_LEQ
| TERM_ORDER_EQ
| TERM_ORDER_INEQ
| TERM_ORDER_GREATER
| TERM_ORDER_LESS
| TERM_ORDER_GEQ
| TERM_ORDER_LEQ
| DOUBLECOLON
| PLUS
| MINUS
| MULT
| DIV
| BITWISE_AND
| BITWISE_OR
| BITWISE_NOT
| VAR_INSTANTIATED
| SEMICOLON
| COMMA
| COLON
| UMINUS
| UPLUS
| CUT
| LPAREN
| RPAREN
| LBRACKET
| RBRACKET
| PIPE
| EOF