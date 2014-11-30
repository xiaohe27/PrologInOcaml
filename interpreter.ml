open ProjCommon
open Evaluator
open Unify

let rec filter preserveList subst = match subst with
                                [] -> [] |
				(v,term)::tail -> (
				  if occursInList v preserveList then 
				   ((v,term)::(filter preserveList tail))
				  else (filter preserveList tail) )
and occursInList item list = match list with
                               [] -> false |
                               h::tail -> if item = h then true else
                                           occursInList item tail;;

let rec rmLastItem list = match list with [] -> [] |
                                          [h] -> [] |
					  h::t -> h::(rmLastItem t);;

let getBool boolVal = match boolVal with 
                      BoolVal true -> true |
		      BoolVal false -> false |
		      _ -> (raise (Failure "Expect a bool val!"));;

(*rename the free vars in the body of the rule before applying the subst gen from the unification of head and query.*)
let rec renameFreeVarsInClause avoidList clause =
            match clause with
              Fact fp -> [fp] |
              Rule (headPred, (body,_)) -> (
	       let freeVarsInBody= ProjCommon.freeVarsInClause clause in
	       let numOfFreeVars= List.length freeVarsInBody in
	       if numOfFreeVars = 0 then (body) else(

	       let binders= ProjCommon.freeVarsInPredicate headPred in
	       let genFreshVars= ProjCommon.get_n_freshVars numOfFreeVars (avoidList @ binders) in
	       let subst4Fresh= genSubst4Fresh freeVarsInBody genFreshVars in 
	       (List.map (Unify.substInPredicate subst4Fresh) body)  
	         )) 

and genSubst4Fresh oldVarList newNameList =
  if (List.length oldVarList) != (List.length newNameList) then (raise (Failure "cannot gen subst 4 fresh because two list have diff len."))
 else (match (oldVarList, newNameList) with
      ([],_) -> [] |
      (_,[]) -> [] |
      (v1::tail1 , n1::tail2) -> ( (v1,Var n1)::genSubst4Fresh tail1 tail2 )  )
    ;;


let rec getAVList subst = 
  let termList= getTermList subst in
   (getVarStrList termList)

and getTermList sigma= 
 match sigma with 
 []-> [] |
 (v,t)::tail0 -> (t::(getTermList tail0) )

and getVarStrList termList =
  match termList with
   [] -> [] |
   term::tail -> (match term with 
		 Var x -> (x::(getVarStrList tail)) |
		 _ -> (getVarStrList tail) );;

(* consult the predicate in user-defined rules *)

let rec consultSinglePred_debug (rules, usedRules) predicate avlist  debug = match rules with 
                                          RuleList ([]) -> (false, []) |
                                          RuleList (clause::tail) -> (match clause with 
								     Fact fp -> (
								       let _=(
								       if debug then(
								       print_string ((string_of_predicate predicate) ^ " is trying to match fact "
										    ^ (string_of_predicate fp) ^ "\n");) else ()) in
								       match(Unify.unifyPredicates (fp,predicate) ) with 
										None -> (consultSinglePred_debug (RuleList tail, (usedRules @ [clause])) predicate avlist debug ) |
										Some sig0 -> (true, sig0) ) |

								     Rule (headPred, (body,connList)) -> (
								      let _=( if debug then(
								        print_string ("\n" ^ (string_of_predicate predicate) ^ " is trying to match the "
										    ^ (string_of_clause (clause)) ^ "\n");) else ()) in

								       match (Unify.unifyPredicates (headPred, predicate)) with
											       None -> (consultSinglePred_debug (RuleList tail, (usedRules @ [clause]))
													  predicate avlist debug) |
											       Some sig0 -> (

let _= ( if debug then
(print_string ("\n after unifying head and query, the following subst function is gen:\n" ^
	     (ProjCommon.string_of_subst sig0)^"\n");) else ()) in
                                                                                                 let avoidList= getAVList sig0 @ avlist in
												 let renamedBody= renameFreeVarsInClause avoidList clause in

let _= ( if debug then (
print_string ("\nAfter renaming, the body of the rule becomes:\n" ^
	     (ProjCommon.stringOfPredList renamedBody connList) ^"\n" );) else ()) in
												 let newBody= List.map (substInPredicate sig0) renamedBody in

let _= ( if debug then (
print_string ("\nAfter applying the subst function gen from unification, new body is :\n"
	     ^ (ProjCommon.stringOfPredList newBody connList) ^"\n" );) else ()) in

													 match (consult_debug (RuleList(usedRules @ (clause::tail)))
														  (Query (newBody, ","::connList)) true avoidList debug ) with
													   (false,_) -> (false,[]) |
													   (true, tailSig) -> (match (Unify.composeSubst tailSig sig0) with
															       None -> (true, []) |
															       Some finalSig -> (true, finalSig) ) ) ) ) 

     (*Consult a list of predicates*)
     (*returns a result which is of the form bool * subst*)
     (*predicates are left-assoc! But it needs to eval from left to right!*)
and consult_debug rules query lastBool avlist debug  =
    match query with Query(predList, connList) -> (
   
    match predList with 
	 [] -> (raise (Failure "rule's body is empty!")) |

	 [singlePred] -> (let (singleBool, singleSig) =(eval_predicate_debug rules singlePred avlist debug) in
			  match connList with 
			  ","::connTail -> ((singleBool && lastBool), singleSig) |
			  ";"::connTail -> ((singleBool || lastBool), singleSig) |
			  _ -> (raise (Failure "Not enough connectives or unknown connective."))) |

         fstPred::tailPredList -> (let (fstBool, fstSig) = (eval_predicate_debug rules fstPred avlist debug) in
	                                (*we should only add things to the subst list when absolutely needed*)
	                                let freeVarsInFstPred= ProjCommon.freeVarsInPredicate fstPred in
					let refinedSig= filter freeVarsInFstPred fstSig in

	                                let newTailPredList= List.map (substInPredicate refinedSig) tailPredList in

let _= (if debug then (

					print_string ((stringOfPredList predList (connList)) ^ " is the old pred list\n");
					print_string ("after eval first predicate, subst derived is "^(string_of_subst fstSig) ^ "\n");
					print_string ((stringOfPredList newTailPredList (List.tl connList)) ^ " is the updated tail pred list\n");
) else ()
) in

	                                let newLastBool= (match connList with
					  ","::_ -> (fstBool && lastBool) |
					  ";"::_ -> (fstBool || lastBool) |
					  _ -> (raise (Failure "unknown connective."))) in
					(consult_debug rules (Query(newTailPredList, List.tl connList)) newLastBool avlist debug)
                                         ) )



(*Consult a predicate which is either user-defined or built-in function*)
and eval_predicate_debug rules predicate avlist debug = match predicate with 
                                   Identifier fact -> (

let _=(if debug then (
print_string ((fact)^" is a fact!");
) else ()) in

				                         match fact with
				                         "true" -> (true,[]) |
							 "false" -> (false,[]) |
							 "nl" -> (Evaluator.nlOpApply (),[]) |
							 _ -> consultSinglePred_debug (rules,[]) predicate avlist debug) |
						                  

				   Predicate (f, tl) -> (if (Evaluator.isBuiltInOp f)   (*It is built in operation*)
				                          				       
				                          then (
let _=( if debug then (
 print_string (f ^ " is a built-in op.\n"); 
) else () ) in							   
				                            if (List.length tl) == 1 then (
							    let singleTerm = (List.hd tl) in

							    if (Evaluator.isTypeTesting f) then  (*it is type testing*)
							         (Evaluator.typeTest f singleTerm, [])
							    
							    else (if f = "not" then (getBool (Evaluator.monOpApply f (Evaluator.eval_term singleTerm)), [])
								  else if (f = "write") then ((Evaluator.writeOpApply singleTerm),[])
								  else (raise (Failure "cannot generate goal from this unary op.")) )
											      )

							  else (      (*other built-in ops*)
							             if (List.length tl) != 2 then raise (Failure "At the moment, this function is not supported.")
									 else (let eq= (List.hd tl, List.nth tl 1) in
									 
							             (match f with 
								     "=" -> (match (Unify.unify [eq]) with  
									     None -> (false, []) |
									     (Some sig0) -> (true , sig0) ) |

								     "is" -> (let lhs= fst eq in 
									      let rhs= snd eq in
									     let rhsVal = Evaluator.eval_term rhs in
									     
								    
									     
									     match lhs with 
									      ConstTerm _ -> (

let _=( if debug then (
										print_string ("\nlhs is "^ (string_of_term lhs)
											      ^ " and it is constant,  eq op be applied on "
											      ^ (string_of_term lhs) ^" and "^ (string_of_term rhs) ^ "\n" );

) else () ) in


										match(Evaluator.binOpApply "=:=" (Evaluator.eval_term lhs,rhsVal)) with
									                      BoolVal false -> (false,[]) |
											      BoolVal true -> (true,[]) |
											      _ -> (raise (Failure "should ret BoolVal")) ) |
									      Var x -> 

let _=( if debug then (
		    print_string ("\nlhs is "^(string_of_term lhs) ^", and it is a var\n");
) else () ) in

										(true, [(x,Evaluator.val2Term rhsVal)]) |
									      _ -> (false, [])) |

								     _ -> (if (Evaluator.retBool f) then (match (binOpApply f (eval_term (fst eq), eval_term (snd eq)))
													 with BoolVal true -> (true,[]) |
													      BoolVal false -> (false,[]) |
													      _ -> (raise (Failure "Unknown exception.")) )

									   else (raise (Failure "predicate should return boolean!")) ) )) )  )    

				                          else  ( 
let _=( if debug then (
print_string ((ProjCommon.string_of_predicate predicate)^" is user-defined!\n");
) else () ) in

								  consultSinglePred_debug (rules,[]) predicate avlist debug) )  (* User defined functions *)


and consultSinglePred (rules,unusedRules) predicate avlist = consultSinglePred_debug (rules,unusedRules) predicate avlist false

and eval_predicate rules predicate avlist = eval_predicate_debug rules predicate avlist false

and consult rules query lastBool avlist = consult_debug rules query lastBool avlist false;;


