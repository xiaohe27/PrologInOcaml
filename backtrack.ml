open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter


(* print result *)
let rec printResultList resultList =
	match resultList with  
	[] -> () |
	curResult::tail -> (printResult curResult; printResultList tail)

and printResult result = match result with
                          (b,sigma) -> (			   			    
			    print_string ("\n"^ (string_of_bool b) ^ ".\n" ^ (ProjCommon.string_of_subst sigma) ^ "\n");) ;;



let rec getIndexedRulesHelper rules curIndex = (
	match rules with 
	RuleList cl -> (match cl with
			[] -> [] |
			curClause::tail -> 
			((curIndex, curClause)::
			(getIndexedRulesHelper (RuleList tail) (curIndex+1) ))));;

let getIndexedRules rules = getIndexedRulesHelper rules 0;;



(*Get all the solutions for a singlePred*)
(*Get a results list. Will assume conn list for combining bools of predicate list is complete*)
let rec getAllSol4Pred indexedRules usedRules pred avlist =
  match indexedRules with
    [] -> ([Interpreter.eval_predicate (RuleList([])) pred []]) |

    (i,curRule)::remainingRuleList -> 
      (
       match curRule with 
	 Fact fp -> (	
		 print_string ((string_of_predicate pred) ^ " is trying to match fact "
		^ (string_of_predicate fp) ^ "\n");

			match(Unify.unifyPredicates (fp,pred) ) with 
		      	None -> (getAllSol4Pred remainingRuleList (usedRules @ [(i,curRule)]) pred avlist) |
		       	Some sig0 ->
print_string ("match sig0: "^ string_of_subst sig0);

	 (true, sig0)::(getAllSol4Pred remainingRuleList (usedRules @ [(i,curRule)]) pred avlist) ) |
	 
	 Rule (headPred, (body,connList)) 
		  -> (

print_string ("\n" ^ (string_of_predicate pred) ^ " is trying to match the "
										    ^ (string_of_clause (curRule)) ^ "\n");

			match (Unify.unifyPredicates (headPred, pred)) with
			None -> (getAllSol4Pred remainingRuleList (usedRules @ [(i,curRule)]) pred avlist)
			 |
			Some sig0 -> ( 

print_string ("\n after unifying head and query, the following subst function is gen:\n" ^
	     (ProjCommon.string_of_subst sig0)^"\n");

				       let avoidList= Interpreter.getAVList sig0 @ avlist in
				       let renamedBody= Interpreter.renameFreeVarsInClause avoidList curRule in
				       let newBody= List.map (Unify.substInPredicate sig0) renamedBody in
				       let bodyQuery=(Query (newBody, connList)) in


print_string ("\nAfter renaming, the body of the rule becomes:\n" ^
	     (ProjCommon.stringOfPredList renamedBody connList) ^"\n" );

print_string ("\nAfter applying the subst function gen from unification, new body is :\n"
	     ^ (ProjCommon.stringOfPredList newBody connList) ^"\n" );


				       let resList4CurRule = 
					getResultListByApplyingSig (getAllSol (usedRules @ indexedRules) bodyQuery true avlist) sig0 in
				       
print_string "cur rule's result list is "; printResultList (resList4CurRule);
let _= input_line (stdin) in

				       resList4CurRule @ (getAllSol4Pred remainingRuleList (usedRules @ [(i,curRule)]) pred avlist)

				       

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
    [singlePred] -> (getAllSol4Pred indexedRules [] singlePred avlist) |
    fstPred::predTailList ->  
	 let fstPredResultList = ( getAllSol4Pred indexedRules [] fstPred avlist ) in   
        (applyFirstResultToPredList fstPred fstPredResultList predTailList connList indexedRules lastBool avlist) ) 

)


and evalPureQuery query =
   (Interpreter.consult (RuleList([])) (query) true [])


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


print_string ((stringOfPredList tailPredList (connList)) ^ " is the old pred list\n");
					print_string ("after eval first predicate, subst derived is "^(string_of_subst sig1) ^ "\n");
					print_string ((stringOfPredList newTailPredList (List.tl connList)) ^ " is the updated tail pred list\n");


	 let allResults4FirstResult = (getAllSol indexedRules (Query(newTailPredList, List.tl connList)) newLastBool avlist)
	 in match remainingFstResultList with
		[] -> allResults4FirstResult | 
		
		_ -> (allResults4FirstResult @
			(applyFirstResultToPredList fstPred remainingFstResultList tailPredList connList indexedRules lastBool avlist))	
	)

  

;;
		





















