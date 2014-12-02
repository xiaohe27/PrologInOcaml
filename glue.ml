open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter
open Backtrack



(*Given a prolog program AST in ocaml, execute its semantics and return a result*)
let addAComma q = match q with 
                      Query (pl,connList) -> (Query (pl, (","::connList)) );;

let addACommaToRuleList rules = 
	match rules with 
			

let addCommaToEveryConnList pgm = 
	match pgm with
	Prog(rules, query)-> Prog(rules, query) |
	ProgFromQuery(query) -> ([], query)

(* get pair of indexed rules and query from as string *)
let getIndexedRulesAndQueryFromStr pgmStr = 
	let parsedPgm= Glue.parseProgram pgmStr in
	(match parsedPgm with
		Prog(rules, query)-> (getIndexedRules rules, query) |
		ProgFromQuery(query) -> ([], query) ) ;;


(*Given a prolog string, return a rule list*)
let parseRules s = Parser.rules Lexer.token (Lexing.from_string s);;

(*Given a prolog string, return a prolog program AST in ocaml*)
let parseProgram s = Parser.program Lexer.token (Lexing.from_string s);;

let execProgram pgm = match pgm with 
                        Prog(rules,query) -> (Interpreter.consult rules (addAComma query) true []) |
			ProgFromQuery(query) -> (Interpreter.consult (RuleList([])) (addAComma query) true []);; 

let debugProgram pgm =  match pgm with 
                        Prog(rules,query) -> (Interpreter.consult_debug rules (addAComma query) true [] true) |
			ProgFromQuery(query) -> (Interpreter.consult_debug (RuleList([])) (addAComma query) true [] true);; 


(* print result *)
let rec printResultList resultList =
	match resultList with  
	[] -> () |
	curResult::tail -> (printResult curResult; printResultList tail)

and printResult result = match result with
                          (b,sigma) -> (			   			    
			    print_string ("\n"^ (string_of_bool b) ^ ".\n" ^ (ProjCommon.string_of_subst sigma) ^ "\n");) ;;


(*refine the result so that only the assignment to the free vars in the query get printed*)
let refineResult result pgm = let outputSig=( match pgm with
						   Prog(_,query) -> 
						     (let freeVarsInQ= ProjCommon.freeVarsInQuery query in
						      Interpreter.filter freeVarsInQ (snd result) ) |

						   ProgFromQuery(query) -> 
						     (let freeVarsInQ= ProjCommon.freeVarsInQuery query in
						      Interpreter.filter freeVarsInQ (snd result) )) in 

                              (fst result, outputSig);;

(* A user-friendly way of simulating prolog program: pretty print the result. *)
let simulateProgram pgmStr =
                                   let pgm = parseProgram pgmStr in
                                   let result= execProgram pgm in				  

				   let updatedResult= refineResult result pgm in 
				   printResult updatedResult;;


(* invoke the backtrack algorithm *)
let findAllResults pgmStr = let (indexedRules, query) = Backtrack.getIndexedRulesAndQueryFromStr pgmStr in
			    let goodQuery= addAComma query in
			    let resultList= Backtrack.getAllSol indexedRules goodQuery true [] in
				printResultList resultList ;;
				




