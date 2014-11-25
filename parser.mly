%{
        open projCommon
%}

/* Define the tokens of the language: */

%token <string> STRING  
%token <string> VARIABLE
%token <string> NAME
%token <float> UNSIGNEDFLOAT
%token <int> UNSIGNEDINTEGER
%token <float> SIGNEDFLOAT
%token <int> SIGNEDINTEGER
%token DOT DOUBLEDOT
%token COLONHYPHEN
%token ARROW 
%token NOT
%token TERM_EQ TERM_INEQ IS TERM_DECOMP TERM_UNIFY TERM_NOTUNIFY
       ARITH_EQ ARITH_INEQ ARITH_LESS ARITH_GREATER ARITH_GEQ
       ARITH_LEQ TERM_ORDER_EQ TERM_ORDER_INEQ TERM_ORDER_GREATER
       TERM_ORDER_LESS TERM_ORDER_GEQ TERM_ORDER_LEQ
%token DOUBLECOLON
%token PLUS MINUS
%token MULT DIV
       BITWISE_AND
       BITWISE_OR BITWISE_NOT VAR_INSTANTIATED
%token SEMICOLON COMMA COLON
%token UMINUS UPLUS
%token CUT
%token LPAREN RPAREN LBRACKET RBRACKET PIPE
%token EOF

%token <bool> BOOL
%token UNIT

/* Define the "goal" nonterminal of the grammar: */
%type <projCommon.term> term
%start term

%%

atomic_term:
  | VARIABLE			{ Var $1 }
  | UNIT			{ ConstTerm(UnitConst) }

  | UNSIGNEDINTEGER		{ ConstTerm(IntConst $1) }
  | MINUS UNSIGNEDINTEGER	{ ConstTerm(IntConst (-$2)) }
  | PLUS UNSIGNEDINTEGER	{ ConstTerm(IntConst ($2)) }
  | UNSIGNEDFLOAT		{ ConstTerm(FloatConst $1) }
  | MINUS UNSIGNEDFLOAT		{ ConstTerm(FloatConst (-$2)) }
  | PLUS UNSIGNEDFLOAT		{ ConstTerm(FloatConst ($2)) }
  | STRING			{ ConstTerm(StringConst $1) }
  | BOOL			{ ConstTerm(BoolConst $1) }

compound_term_1:
  |atomic_term			{ $1 }
  
compound_term_2:
  |compound_term_1 {$1}
  |compound_term_2 MULT compound_term_1 { CompoundTerm ("*",[$1;$3])}
  |compound_term_2 DIV compound_term_1 { CompoundTerm ("/",[$1;$3])}
  
compound_term_3:
  |compound_term_2 {$1}
  |compound_term_3 PLUS compound_term_2 { CompoundTerm ("+",[$1;$3])}
  |compound_term_3 MINUS compound_term_2 { CompoundTerm ("-",[$1;$3])}
  
 compound_term:
  |compound_term_3 {$1}

list_term:
| LBRACKET RBRACKET			{ ListTerm [] }
| LBRACKET list_body RBRACKET		{ ListTerm $2 }

list_body:
| compound_term				{ [$1] }
| compound_term PIPE list_body 		{ $1::$3 }

term:
| list_term					{ $1 }
| compound_term			{ $1 }