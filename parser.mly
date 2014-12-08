%{
        open ProjCommon
%}

/* Define the tokens of the language: */

%token <string> STRING  
%token <string> VARIABLE
%token <string> NAME
%token <float> FLOAT /*unsigned*/
%token <int> INT /*unsigned*/
%token DOT DOUBLEDOT
%token COLONHYPHEN QUESTIONHYPHEN
%token ARROW 
%token NOT
%token TERM_EQ TERM_INEQ IS AS TERM_DECOMP TERM_UNIFY TERM_NOTUNIFY
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
%token DOLLAR 

/* Define the "goal" nonterminal of the grammar: */
%type <ProjCommon.rules> rules
%start rules
%type <ProjCommon.program> program
%start program

%%
var:
| VARIABLE				{ Var $1 }
name:
| NAME					{ ConstTerm(StringConst $1) }
atomic_term:
  | var					{ $1 }
  | name				{ $1 }
  | INT						{ ConstTerm(IntConst $1) }
  | MINUS INT				{ ConstTerm(IntConst (-$2)) }
  | FLOAT					{ ConstTerm(FloatConst $1) }
  | MINUS FLOAT				{ ConstTerm(FloatConst (-.$2)) }
  | STRING					{ ConstTerm(StringConst $1) }
  | BOOL					{ ConstTerm(BoolConst $1) } 
  | LPAREN term RPAREN  	{ $2 } 
  | list_term					{ $1 }

compound_term_1:
  |atomic_term			{ $1 }
  |DOLLAR atomic_term		{ CompoundTerm ("$",[$2])}
  
compound_term_200:
  |compound_term_1 {$1}
  |compound_term_1 VAR_INSTANTIATED compound_term_200 { CompoundTerm ("^",[$1;$3])}

compound_term_400:
  |compound_term_200 {$1}
  |compound_term_400 MULT compound_term_200 { CompoundTerm ("*",[$1;$3])}
  |compound_term_400 DIV compound_term_200 	{ CompoundTerm ("/",[$1;$3])}
  
compound_term_500:
  |compound_term_400 {$1}
  |compound_term_500 PLUS compound_term_400 { CompoundTerm ("+",[$1;$3])}
  |compound_term_500 MINUS compound_term_400 { CompoundTerm ("-",[$1;$3])}
  |compound_term_500 BITWISE_AND compound_term_400 	{ CompoundTerm ("\\/",[$1;$3])}
  |compound_term_500 BITWISE_OR compound_term_400 	{ CompoundTerm ("/\\",[$1;$3])}

compound_term_600:
  |compound_term_500 {$1}
  |compound_term_500 COLON compound_term_600 { CompoundTerm (":",[$1;$3])}
  
compound_term_700:
  |compound_term_600 ARITH_EQ compound_term_600				{ CompoundTerm ("=:=",[$1;$3])}
  |compound_term_600 ARITH_INEQ compound_term_600 			{ CompoundTerm ("=\\=",[$1;$3])}
  |compound_term_600 ARITH_GEQ compound_term_600 			{ CompoundTerm (">=",[$1;$3])} 
  |compound_term_600 ARITH_LEQ compound_term_600 			{ CompoundTerm ("=<",[$1;$3])} 
  |compound_term_600 ARITH_GREATER compound_term_600 		{ CompoundTerm (">",[$1;$3])} 
  |compound_term_600 ARITH_LESS compound_term_600 			{ CompoundTerm ("<",[$1;$3])} 
  
  |compound_term_600 TERM_UNIFY compound_term_600 			{ CompoundTerm ("=",[$1;$3])}
  |compound_term_600 TERM_NOTUNIFY compound_term_600 		{ CompoundTerm ("\\=",[$1;$3])}
  |compound_term_600 TERM_EQ compound_term_600 				{ CompoundTerm ("==",[$1;$3])}  
  |compound_term_600 TERM_INEQ compound_term_600 			{ CompoundTerm ("\\==",[$1;$3])} 
  |compound_term_600 IS compound_term_600 					{ CompoundTerm ("is",[$1;$3])} 
  |compound_term_600 AS compound_term_600 					{ CompoundTerm ("as",[$1;$3])} 
  |compound_term_600 TERM_DECOMP compound_term_600 			{ CompoundTerm ("=..",[$1;$3])} 

  |compound_term_600 TERM_ORDER_GEQ compound_term_600		{ CompoundTerm ("@>=",[$1;$3])} 
  |compound_term_600 TERM_ORDER_LEQ compound_term_600 		{ CompoundTerm ("@=<",[$1;$3])} 
  |compound_term_600 TERM_ORDER_GREATER compound_term_600 	{ CompoundTerm ("@>",[$1;$3])} 
  |compound_term_600 TERM_ORDER_LESS compound_term_600 		{ CompoundTerm ("@<",[$1;$3])} 
  |compound_term_600 TERM_ORDER_EQ compound_term_600 		{ CompoundTerm ("=@=",[$1;$3])} 
  |compound_term_600 TERM_ORDER_INEQ compound_term_600 		{ CompoundTerm ("\\=@=",[$1;$3])} 

compound_term_1050:
  |compound_term_600 {$1}
  |compound_term_700 {$1}
  |compound_term_700 ARROW compound_term_1050	{ CompoundTerm ("->",[$1;$3])}
 
    
 compound_term:
  |compound_term_1050 {$1}

list_term:
| LBRACKET RBRACKET					{ ListTerm [] }
| LBRACKET term_list RBRACKET		{ ListTerm $2 }

term:

| compound_term				{ $1 }
| NAME LPAREN term_list RPAREN 	{ PredAsTerm (Predicate ($1,$3)) }
| NOT LPAREN term RPAREN		{ PredAsTerm (Predicate ("not",[$3])) }
/*| predicate					{ PredAsTerm $1 }*/

predicate_list:
| predicate								{ ([$1],[]) }
| predicate COMMA predicate_list	 	{ ($1::(fst $3), ","::snd($3)) }
| predicate SEMICOLON predicate_list	{ ($1::(fst $3), ";"::snd($3)) }

term_list:
| term						{ [$1] }
| term COMMA term_list 		{ $1::$3 }
| term PIPE term_list 		{ $1::$3 }

predicate:
| NAME LPAREN term_list RPAREN    	{ Predicate ($1,$3) }
| NAME								{ Identifier $1 }
| VARIABLE								{ VarAsPred $1}
| NOT LPAREN term RPAREN			{ Predicate ("not",[$3]) }

  |compound_term_600 ARITH_EQ compound_term_600				{ Predicate ("=:=",[$1;$3])}
  |compound_term_600 ARITH_INEQ compound_term_600 			{ Predicate ("=\\=",[$1;$3])}
  |compound_term_600 ARITH_GEQ compound_term_600 			{ Predicate (">=",[$1;$3])} 
  |compound_term_600 ARITH_LEQ compound_term_600 			{ Predicate ("=<",[$1;$3])} 
  |compound_term_600 ARITH_GREATER compound_term_600 		{ Predicate (">",[$1;$3])} 
  |compound_term_600 ARITH_LESS compound_term_600 			{ Predicate ("<",[$1;$3])} 
  |compound_term_600 TERM_UNIFY compound_term_600 			{ Predicate ("=",[$1;$3])}
  |compound_term_600 TERM_NOTUNIFY compound_term_600 		{ Predicate ("\\=",[$1;$3])}
  |compound_term_600 TERM_EQ compound_term_600 				{ Predicate ("==",[$1;$3])}  
  |compound_term_600 TERM_INEQ compound_term_600 			{ Predicate ("\\==",[$1;$3])} 
  |compound_term_600 TERM_ORDER_GEQ compound_term_600		{ Predicate ("@>=",[$1;$3])} 
  |compound_term_600 TERM_ORDER_LEQ compound_term_600 		{ Predicate ("@=<",[$1;$3])} 
  |compound_term_600 TERM_ORDER_GREATER compound_term_600 	{ Predicate ("@>",[$1;$3])} 
  |compound_term_600 TERM_ORDER_LESS compound_term_600 		{ Predicate ("@<",[$1;$3])} 
  |compound_term_600 TERM_ORDER_EQ compound_term_600 		{ Predicate ("=@=",[$1;$3])} 
  |compound_term_600 TERM_ORDER_INEQ compound_term_600 		{ Predicate ("\\=@=",[$1;$3])} 
  |compound_term_600 IS compound_term_600 					{ Predicate ("is",[$1;$3])} 

clause:
| predicate DOT								{ Fact $1 }
| predicate COLONHYPHEN predicate_list DOT  { Rule ($1,$3) }

clause_list:
| clause				 { [$1] }
| clause clause_list	 { $1::$2 }

query:
| QUESTIONHYPHEN predicate_list DOT  { Query $2 }

rules:
| clause_list			{ RuleList $1 }

program:
| rules query			{ Prog ($1,$2) }
| query					{ ProgFromQuery $1 }