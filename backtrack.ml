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
(*Get a results list. Will assume conn list for combining bools of predicate list is complete*)
let rec getAllSol4Pred indexedRules pred avlist =
  match indexedRules with
    [] -> ([Interpreter.eval_predicate (RuleList([])) pred []]) |

    curRule::remainingRuleList -> 
      (
       match curRule with 
	 Fact fp -> (match(Unify.unifyPredicates (fp,pred) ) with 
		      	None -> (getAllSol4Pred remainingRuleList pred avlist) |
		       	Some sig0 -> (true, sig0)::(getAllSol4Pred remainingRuleList pred avlist) ) |
	 
	 Rule (headPred, (body,connList)) 
		  -> (match (Unify.unifyPredicates (headPred, pred)) with
			None -> (getAllSol4Pred remainingRuleList pred avlist)
			 |
			Some sig0 -> ( let avoidList= Interpreter.getAVList sig0 @ avlist in
				       let renamedBody= Interpreter.renameFreeVarsInClause avoidList curRule in
				       let newBody= List.map (Unify.substInPredicate sig0) renamedBody in
				       let bodyQuery=(Query (newBody, ","::connList)) in

				       let resList4CurRule = 
					getResultListByApplyingSig (getAllSol indexedRules bodyQuery true avlist) sig0 in
				       
				       resList4CurRule @ (getAllSol4Pred remainingRuleList pred avlist)

				       ) )
      ) 


and getResultListByApplyingSig resList sigma =
match resList with
  [] -> [] |

  fstResult::tail -> (match fstResult with 
		      (false,_) -> (getResultListByApplyingSig tail sigma) |
		      (true, fstSig) -> (match (Unify.composeSubst fstSig sigma) with
					        None -> (true, [])::(getResultListByApplyingSig tail sigma) |
					        Some finalSig -> (true, finalSig)::(getResultListByApplyingSig tail sigma)) ) 


and getAllSol indexedRules query lastBool avlist = (
match query with
Query(predList, connList) -> 
  ( match predList with
    [] -> ((raise (Failure "Query is empty!"))) |
    fstPred::predTailList ->  
	 let fstPredResultList = ( getAllSol4Pred indexedRules fstPred avlist ) in   
        (applyFirstResultToPredList fstPred fstPredResultList predTailList connList indexedRules lastBool avlist) ) 

)


and evalPureQuery query =
   (Interpreter.consult (RuleList([])) (Glue.addAComma query) true [])


and applyFirstResultToPredList fstPred fstPredResultList tailPredList connList indexedRules lastBool avlist =
 
 match fstPredResultList with 
        [] -> (raise (Failure "Should return a result, either true or false")) |
        (bool1, sig1)::remainingFstResultList ->
	  (let freeVarsInFstPred= ProjCommon.freeVarsInPredicate fstPred in
	   let refinedSig= filter freeVarsInFstPred sig1 in

	   let newTailPredList= List.map (substInPredicate refinedSig) tailPredList in
	   let newLastBool= (match connList with
		","::_ -> (bool1 && lastBool) |
		";"::_ -> (bool1 || lastBool) |
		_ -> (raise (Failure "unknown connective."))) in
	 let allResults4FirstResult = (getAllSol indexedRules (Query(newTailPredList, List.tl connList)) newLastBool avlist)
	 in match remainingFstResultList with
		[] -> allResults4FirstResult | 
		
		_ -> (allResults4FirstResult @
			(applyFirstResultToPredList fstPred remainingFstResultList tailPredList connList indexedRules lastBool avlist))	
	)

  

;;
		





















