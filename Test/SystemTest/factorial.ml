open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter
open Glue



let pgm= "factorial(0,1). factorial(N,F) :- "
	^ "N>0, N1 is N-1, factorial(N1,F1),"
	^ "F is N * F1. ?- factorial(3,W).";;


let result = Glue.simulateProgram pgm;;