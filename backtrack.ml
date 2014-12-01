open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter
open Glue




let rec getIndexedRulesHelper rules curIndex = (
	match rules with 
	RuleList cl -> (match cl with
			[] -> [] |
			curClause::tail -> 
			((curIndex, curClause)::
			(getIndexedRulesHelper (RuleList tail) (curIndex+1) ))));;

let getIndexedRules rules = getIndexedRulesHelper rules 0;;


let getIndexedProgramFromSourceCode () = 
	let _=print_endline "Give the path to the prolog program please." in
	let file= read_line () in 
	let lexbuf = Lexing.from_channel (open_in file) in
	let parsedPgm= Parser.program Lexer.token lexbuf in
	(match parsedPgm with
		Prog(rules, query)-> (getIndexedRules rules, query) |
		ProgFromQuery(query) -> ([], query) ) ;;


let printIndexedRulesFromFile () =
	let (indexedRules,_) = getIndexedProgramFromSourceCode () in
	print_string (ProjCommon.stringOfIndexedRules indexedRules) ;; 
	

let backtrack indexRules query = 
		