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
	


(*Get all the solutions for a singlePred*)
(*Get a results list*)
let getAllSol4Pred indexedRules pred avlist =
  match indexedRules with
    [] -> ([Interpreter.eval_predicate (RuleList([])) pred []]) |

    curRule::remainingRuleList -> 
      (
       match curRule with 
	 Fact fp -> (match(Unify.unifyPredicates (fp,pred) ) with 
		      	None -> (getAllSol4Pred remainingRuleList pred ) |
		       	Some sig0 -> (true, sig0)::(getAllSol4Pred remainingRuleList pred) ) |
	 
	 Predicate(headPred, (body,connList)) 
		  -> (match (Unify.unifyPredicates (headPred, pred)) with
			None -> (getAllSol4Pred remainingRuleList pred )
			 |
			Some sig0 -> ( let avoidList= Interpreter.getAVList sig0 @ avlist in
				       let renamedBody= Interpreter.renameFreeVarsInClause avoidList curRule in
				       let newBody= List.map (Unify.substInPredicate sig0) renamedBody in
				       let bodyQuery=(Query (newBody, ","::connList)) in

				       match (getAllSol indexedRules bodyQuery avlist) with
					 ([]) -> getAllSol4Pred reaminingRuleList pred avlist |
					 (curResult::resList) -> 
					   (match curResult with
					    (false,_) -> () |
					    (true, tailSig) -> (match (Unify.composeSubst tailSig sig0) with
					        None -> (true, []) |
					        Some finalSig -> (true, finalSig) )) 

				       ) )
      ) 

and getAllSol indexedRules query avlist =


and evalPureQuery query =
   (Interpreter.consult (RuleList([])) (Glue.addAComma query) true [])

and getOneSol singleRule query =

  match 
		
