open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter

(* Lexing.from_channel stdin *)

(*Given a prolog string, return a rule list*)
let parseRules s = Parser.rules Lexer.token (Lexing.from_string s);;

(*Given a prolog string, return a prolog program AST in ocaml*)
let parseProgram s = Parser.program Lexer.token (Lexing.from_string s);;

(*Given a prolog program AST in ocaml, execute its semantics and return a result*)
let addAComma q = match q with 
                      Query (pl,connList) -> (Query (pl, (","::connList)) );;

let execProgram pgm = match pgm with 
                        Prog(rules,query) -> (Interpreter.consult rules (addAComma query) true []) |
			ProgFromQuery(query) -> (Interpreter.consult (RuleList([])) (addAComma query) true []);; 


(* print result *)
let printResult result = match result with
                          (b,sigma) -> (print_string ("\n"^ (string_of_bool b) ^ ".\n" ^ (ProjCommon.string_of_subst sigma) ^ "\n");) ;;

(* A user-friendly way of simulating prolog program: pretty print the result. *)
let simulateProgram pgmStr = let pgm = parseProgram pgmStr in
                                   let result= execProgram pgm in printResult result;;
