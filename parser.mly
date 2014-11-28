%{
        open ProjCommon
%}

/* Define the tokens of the language: */

%token <string> STRING  
%token <string> VARIABLE
%token <string> NAME
%token <float> FLOAT
%token <int> INT
%token DOT DOUBLEDOT
%token COLONHYPHEN QUESTIONHYPHEN
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


/* Define the "goal" nonterminal of the grammar: */
%type <ProjCommon.rules> rules
%start rules
%type <ProjCommon.program> program
%start program

%%

atomic_term:
  | VARIABLE				{ Var $1 }
  | INT						{ ConstTerm(IntConst $1) }
  | FLOAT					{ ConstTerm(FloatConst $1) }
  | STRING					{ ConstTerm(StringConst $1) }
  | BOOL					{ ConstTerm(BoolConst $1) }
  | NAME					{ ConstTerm(StringConst $1) }
  | LPAREN term RPAREN  	{ $2 } 

compound_term_1:
  |atomic_term			{ $1 }
  |CUT					{ CompoundTerm ("!",[])}
  
compound_term_2:
  |compound_term_1 {$1}
  |compound_term_2 MULT compound_term_1 { CompoundTerm ("*",[$1;$3])}
  |compound_term_2 DIV compound_term_1 	{ CompoundTerm ("/",[$1;$3])}
  |compound_term_2 VAR_INSTANTIATED compound_term_1 { CompoundTerm ("^",[$1;$3])}
  |compound_term_2 BITWISE_AND compound_term_1 	{ CompoundTerm ("\\/",[$1;$3])}
  |compound_term_2 BITWISE_OR compound_term_1 	{ CompoundTerm ("/\\",[$1;$3])}
  |compound_term_2 BITWISE_NOT compound_term_1 	{ CompoundTerm ("\\",[$1;$3])}
  
compound_term_3:
  |compound_term_2 {$1}
  |compound_term_3 PLUS compound_term_2 { CompoundTerm ("+",[$1;$3])}
  |compound_term_3 MINUS compound_term_2 { CompoundTerm ("-",[$1;$3])}

compound_term_4:
  |compound_term_3 {$1}
  |compound_term_4 DOUBLECOLON compound_term_3 { CompoundTerm ("::",[$1;$3])}
  
compound_term_5:
  |compound_term_4 {$1}
  |compound_term_5 ARITH_EQ compound_term_4				{ CompoundTerm ("=:=",[$1;$3])}
  |compound_term_5 ARITH_INEQ compound_term_4 			{ CompoundTerm ("=\\=",[$1;$3])}
  |compound_term_5 ARITH_GEQ compound_term_4 			{ CompoundTerm (">=",[$1;$3])} 
  |compound_term_5 ARITH_LEQ compound_term_4 			{ CompoundTerm ("=<",[$1;$3])} 
  |compound_term_5 ARITH_GREATER compound_term_4 		{ CompoundTerm (">",[$1;$3])} 
  |compound_term_5 ARITH_LESS compound_term_4 			{ CompoundTerm ("<",[$1;$3])} 
  
  |compound_term_5 TERM_UNIFY compound_term_4 			{ CompoundTerm ("=",[$1;$3])}
  |compound_term_5 TERM_NOTUNIFY compound_term_4 		{ CompoundTerm ("\\=",[$1;$3])}
  |compound_term_5 TERM_EQ compound_term_4 				{ CompoundTerm ("==",[$1;$3])}  
  |compound_term_5 TERM_INEQ compound_term_4 			{ CompoundTerm ("\\==",[$1;$3])} 
  |compound_term_5 IS compound_term_4 					{ CompoundTerm ("is",[$1;$3])} 
  |compound_term_5 TERM_DECOMP compound_term_4 			{ CompoundTerm ("=..",[$1;$3])} 

  |compound_term_5 TERM_ORDER_GEQ compound_term_4		{ CompoundTerm ("@>=",[$1;$3])} 
  |compound_term_5 TERM_ORDER_LEQ compound_term_4 		{ CompoundTerm ("@=<",[$1;$3])} 
  |compound_term_5 TERM_ORDER_GREATER compound_term_4 	{ CompoundTerm ("@>",[$1;$3])} 
  |compound_term_5 TERM_ORDER_LESS compound_term_4 		{ CompoundTerm ("@<",[$1;$3])} 
  |compound_term_5 TERM_ORDER_EQ compound_term_4 		{ CompoundTerm ("=@=",[$1;$3])} 
  |compound_term_5 TERM_ORDER_INEQ compound_term_4 		{ CompoundTerm ("\\=@=",[$1;$3])} 
  
compound_term_6:
  |compound_term_5 {$1}
  |NOT compound_term_5 	{ CompoundTerm ("not",[$2])}
  

compound_term_7:
  |compound_term_6 {$1}
  |compound_term_6 ARROW compound_term_7	{ CompoundTerm ("->",[$1;$3])}
 
    
 compound_term:
  |compound_term_7 {$1}

list_term:
| LBRACKET RBRACKET					{ ListTerm [] }
| LBRACKET term_list RBRACKET		{ ListTerm $2 }

term:
| list_term					{ $1 }
| compound_term				{ $1 }

term_list:
| term						{ [$1] }
| term COMMA term_list 		{ $1::$3 }
| term PIPE term_list 		{ $1::$3 }

predicate:
| NAME LPAREN term_list RPAREN    	{ Predicate ($1,$3) }
| NAME								{ Identifier $1 }

predicate_list:
| predicate								{ [$1] }
| predicate COMMA predicate_list	 	{ $1::$3 }

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