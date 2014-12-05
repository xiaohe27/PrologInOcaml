E.G.
Aim: test AST/substTest.ml

0. Write ocaml test code in AST/substTest.ml
1. In project's top level, start ocaml
2. In ocaml terminal, type:
	#load "projCommon.cmo";;
	#load "unify.cmo";;
	#use "Test/AST/substTest.ml";;


==================================================
For your convenience, copy and paste the code below.
==================================================
#load "projCommon.cmo";;
#load "lexer.cmo";;
#load "parser.cmo";;
#load "unify.cmo";;
#load "evaluator.cmo";;
#load "interpreter.cmo";;
#load "backtrack.cmo";;
#load "glue.cmo";;


========================
#use "YourTestOcamlFile.ml";;


Notice that for the program below, std prolog cannot find the answer
sis(keke,niu).

sis(joyce,niu).
sis(keke,joyce).
sis(X,Y) :- sis(X,Z), sis(Z,Y).

?-sis(X,Y).


For help, use ?- help(Topic). or ?- apropos(Word).

1 ?- sis(X,Y).
X = joyce,
Y = niu ;
X = keke,
Y = joyce ;
ERROR: Out of local stack

