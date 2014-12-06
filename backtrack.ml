open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter


(*Debugger*)
let rec printDebug indexedRules usedRules sigma pred avlist blacklist =
print_string "\nCur rules:\n";
print_string (ProjCommon.stringOfIndexedRules indexedRules);

print_string "\nUsed rules:\n";
print_string (ProjCommon.stringOfIndexedRules usedRules);

print_string "\nSigma:\n";
print_string (ProjCommon.string_of_subst sigma);

print_string "\nCur pred is:\n";
print_string (ProjCommon.string_of_predicate pred);

print_string "\nAvoid list is:\n";
print_string (ProjCommon.string_of_stringList avlist);

print_string "\nBlacklist is:\n";
print_string (ProjCommon.stringOfBlackList blacklist);

print_string "\n\n";

;;


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

let rec getClauseWithIndex indexedRules i =
   match indexedRules with
	[] -> None |
	(index,clause)::tail -> (if index=i then Some clause
				else getClauseWithIndex tail i )
;;


(*check whether a given str is in the black list of a rule*)
let rec isInBlackList indexedRules blacklist i pred =
	let predStr= string_of_predicate pred in 
        let isPredAllVars= (ProjCommon.onlyVarsInPred pred) in

	match (getItemWithIndexI blacklist i) with
	None -> false |
	Some listI ->
 	 let isHeadPredAllVars = 
	(
		 match  (getClauseWithIndex indexedRules i) 
	with     None -> (raise (Failure ("Rule " ^ (string_of_int i) ^ " is not in the knowledge base.")))
		
	|	Some ruleI -> (
	        let headI = (match ruleI with
				Fact hp -> hp |
				Rule (headPred,_) -> headPred ) in
		
		ProjCommon.onlyVarsInPred headI) 
		

	) in (
		if isHeadPredAllVars && isPredAllVars 
		then (true)
		else  		
	 	(if occursIn predStr listI then true else(
				
		if (ProjCommon.onlyConstInPred pred) then (false)
		else(		
		let constPart=(ProjCommon.getAllConstInPred pred) in
				
		let foundInBlist=(ProjCommon.isContainedInOneStrInTheList listI constPart) in 
		foundInBlist)
		) 

		)
	)

and getItemWithIndexI blacklist i =
match blacklist with 
[] -> None |
(j, sl)::tail -> (if i=j then Some sl else (getItemWithIndexI tail i) )

and occursIn predStr listI = 
	match listI with
	[] -> false |
	h::t -> (if predStr = h then true 
		else occursIn predStr t);;
 
let rec addToBlackList blacklist i predStr =
	match blacklist with
	[] -> [(i,[predStr])] |
	(n, sl)::(tail) ->
	(if n=i then ((n, predStr::sl)::tail) 
	else ((n, sl)::(addToBlackList tail i predStr)));;


(*Get all the solutions for a singlePred*)
(*Get a results list. Will assume conn list for combining bools of predicate list is complete*)
let rec getAllSol4Pred indexedRules usedRules pred avlist blacklist =
   

  match indexedRules with
    [] -> ([Interpreter.eval_predicate (RuleList([])) pred avlist]) |

    (i,curRule)::remainingRuleList -> 
      (
	  if (isInBlackList indexedRules blacklist i pred) then 
([ (false, []) ]) else

       match curRule with 
	 Fact fp -> (	
		
			match(Unify.unifyPredicates (fp,pred) ) with 
		      	None -> (getAllSol4Pred remainingRuleList (usedRules @ [(i,curRule)]) pred avlist blacklist) |
		       	Some sig0 ->

			

	 (true, sig0)::(getAllSol4Pred remainingRuleList (usedRules @ [(i,curRule)]) pred avlist blacklist) ) |
	 
	 Rule (headPred, (body,connList)) 
		  -> (


			match (Unify.unifyPredicates (headPred, pred)) with
			None -> (getAllSol4Pred remainingRuleList (usedRules @ [(i,curRule)]) pred avlist blacklist)
			 |
			Some sig0 -> ( 



let newBlackList= addToBlackList blacklist i (ProjCommon.string_of_predicate pred) in

				       let avoidList= Interpreter.getAVList sig0 @ avlist in
				       let renamedBody= Interpreter.renameFreeVarsInClause avoidList curRule in
				       let newBody= List.map (Unify.substInPredicate sig0) renamedBody in
				       let bodyQuery=(Query (newBody, connList)) in



				       let resList4CurRule = 
					getResultListByApplyingSig (getAllSol (usedRules @ indexedRules) bodyQuery true avoidList
					newBlackList) sig0 in



				       resList4CurRule @ (getAllSol4Pred remainingRuleList (usedRules @ [(i,curRule)]) pred avlist newBlackList)

				       

) )
      ) 


and getResultListByApplyingSig resList sigma =
match resList with
  [] -> [] |

  fstResult::tail -> (


		      match fstResult with 
		      (false,_) -> (getResultListByApplyingSig tail sigma) |
		      (true, fstSig) -> (match (Unify.composeSubst fstSig sigma) with
					        None -> (true, [])::(getResultListByApplyingSig tail sigma) |
					        Some finalSig0 ->

let finalSig = (let updatedEQList= Unify.updateVarInSubst sigma finalSig0 in

		match (Unify.unify updatedEQList) with
			None -> [] |
			Some fs -> fs ) in

 (true, finalSig)::(getResultListByApplyingSig tail sigma)) ) 


and getAllSol indexedRules query lastBool avlist blacklist = (
if lastBool = false then ([]) 
else
match query with
Query(predList, connList) -> 
  ( match predList with
    [] -> ((raise (Failure "Query is empty!"))) |
    [singlePred] -> (
	
	getAllSol4Pred indexedRules [] singlePred avlist blacklist) |

    fstPred::predTailList ->  (	

	 let fstPredResultList = ( getAllSol4Pred indexedRules [] fstPred avlist blacklist) in  


        (applyFirstResultToPredList fstPred fstPredResultList predTailList connList indexedRules lastBool avlist blacklist)
)

) 
)


and evalPureQuery query =
   (Interpreter.consult (RuleList([])) (query) true [])


and applyFirstResultToPredList fstPred fstPredResultList tailPredList connList indexedRules lastBool avlist blacklist =
 
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



	 let allResults4FirstResult =
	ProjCommon.addSigToResultList refinedSig (getAllSol indexedRules (Query(newTailPredList, List.tl connList)) newLastBool avlist blacklist)

	 in


 match remainingFstResultList with
		[] -> allResults4FirstResult | 
		
		_ -> (allResults4FirstResult @
			(applyFirstResultToPredList fstPred remainingFstResultList tailPredList connList indexedRules lastBool avlist blacklist))	
	)

  

;;
		






















