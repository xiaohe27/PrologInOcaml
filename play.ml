(*
  interactive-prolog program interpreter 
*)
open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter
open Backtrack
open Glue


(* Try to detect if something is getting piped in *)
let is_interactive = 0 = (Sys.command "[ -t 0 ]")

let _ =
  (if is_interactive
      then print_endline "\nWelcome to the prolog simulator \n"
      else ());
  let rec loop rules = 
  try
	print_endline "input query or program with query please.";
    let lexbuf = Lexing.from_channel stdin
    in (if is_interactive 
          then (print_string "> "; flush stdout)
          else ());

	(try 
	
	let parsedPgm= Parser.program Lexer.token lexbuf in
	let RuleList(existingRules)= rules in
	let newPgm=(match parsedPgm with 
	Prog(RuleList(curRules), query) -> 
	(Prog(RuleList(curRules @ existingRules),query)) |
	
	ProgFromQuery(query) -> (Prog(rules,query))  ) in

	let result= Glue.refineResult (Glue.getQueryFromPgm newPgm) (Glue.execProgram newPgm) in 
	let _=	printResult result in
	match newPgm with 
	Prog(newRules, _) -> (loop newRules) |
	ProgFromQuery(_) -> (loop rules)  		


       with Failure s -> (print_newline();
			   print_endline s;
                           print_newline();
                           loop rules)
           | Parsing.Parse_error ->
             (print_string "\ndoes not parse\n";
              loop rules))
  with Lexer.EndInput -> exit 0
 in loop (RuleList [])
