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

let rec addCommToCL clauseList = 
	match clauseList with 
	[] -> [] |
	clause::tail -> (match clause with
			 Rule(hp, (body, connList)) -> (Rule(hp, (body, ","::connList)))::(addCommToCL tail) |
			 _ -> clause :: (addCommToCL tail) ) ;;

let addACommaToRuleList rules = 
	match rules with 
	RuleList(cl) -> (
		RuleList(addCommToCL cl)
	) ;;
			

let addCommaToPgm pgm = 
	match pgm with
	Prog(rules, query)-> Prog(addACommaToRuleList rules, addAComma query) |
	ProgFromQuery(query) -> ProgFromQuery(addAComma query);;


let getQueryFromPgm pgm =
	match pgm with
	Prog(_,query) -> (query) |
	ProgFromQuery(query) -> (query);; 

(*Given a prolog string, return a rule list*)
let parseRules s = addACommaToRuleList (Parser.rules Lexer.token (Lexing.from_string s));;

(*Given a prolog string, return a prolog program AST in ocaml*)
let parseProgram pgmStr = addCommaToPgm(Parser.program Lexer.token (Lexing.from_string pgmStr));;

let execProgram pgm = match (pgm) with 
                        Prog(rules,query) -> (Interpreter.consult rules (query) true []) |
			ProgFromQuery(query) -> (Interpreter.consult (RuleList([])) (query) true []);; 

let debugProgram pgm =  match (pgm) with 
                        Prog(rules,query) -> (Interpreter.consult_debug rules (query) true [] true) |
			ProgFromQuery(query) -> (Interpreter.consult_debug (RuleList([])) (query) true [] true);; 


(* get pair of indexed rules and query from as string *)
let getIndexedRulesAndQueryFromStr pgmStr = 
	let parsedPgm= (parseProgram pgmStr) in
	(match parsedPgm with
		Prog(rules, query)-> (getIndexedRules rules, query) |
		ProgFromQuery(query) -> ([], query) ) ;;


(* print result *)
let rec printResultList resultList =
	match resultList with  
	[] -> () |
	curResult::tail -> (printResult curResult; printResultList tail)

and printResult result = match result with
                          (b,sigma) -> (			   			    
			    print_string ("\n"^ (string_of_bool b) ^ ".\n" ^ (ProjCommon.string_of_subst sigma) ^ "\n");) ;;


(*refine the result so that only the assignment to the free vars in the query get printed*)
let refineResult query result = let outputSig= (let freeVarsInQ= ProjCommon.freeVarsInQuery query in
						Interpreter.filter freeVarsInQ (snd result) ) 
				in (fst result, outputSig);;

(* A user-friendly way of simulating prolog program: pretty print the result. *)
let simulateProgram pgmStr =
                                   let pgm = parseProgram pgmStr in
                                   let result= execProgram pgm in				  

				   let updatedResult= refineResult (getQueryFromPgm pgm) result in 
				   printResult updatedResult;;


(* invoke the backtrack algorithm *)
let findAllResults pgmStr = let (indexedRules, query) = getIndexedRulesAndQueryFromStr pgmStr in
			    let resultList= Backtrack.getAllSol indexedRules query true [] in
				let refinedResults = List.map (refineResult query) resultList in
				printResultList refinedResults ;;
				




