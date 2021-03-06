open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter
open Backtrack
open Glue

(*Get the path of the prolog program*)
let file= Sys.argv.(1);;

let lexbuf = Lexing.from_channel (open_in file) ;;

let rec loop rules = 

try (
  let parsedPgm= Parser.program Lexer.token lexbuf in
	let RuleList(existingRules)= rules in
	let newPgm=(match parsedPgm with 
	Prog(RuleList(curRules), query) -> 
	(Prog(RuleList(curRules @ existingRules),query)) |
	
	ProgFromQuery(query) -> (Prog(rules,query))  ) in

	let resultList= List.map (Glue.refineResult (Glue.getQueryFromPgm newPgm)) (Glue.findAllResults newPgm) in 
	let _=	Glue.printResultOneByOne resultList in
	match newPgm with 
	Prog(newRules, _) -> (loop newRules) |
	_ -> (loop rules)
)

with Lexer.EndInput -> (exit 0) 
	| _ -> (exit 1)

in loop (RuleList [])
