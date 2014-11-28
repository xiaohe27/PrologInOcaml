open ProjCommon
open Evaluator
open Unify

let rec rmLastItem list = match list with [] -> [] |
                                          [h] -> [] |
					  h::t -> h::(rmLastItem t);;

let getBool boolVal = match boolVal with 
                      BoolVal true -> true |
		      BoolVal false -> false |
		      _ -> (raise (Failure "Expect a bool val!"));;

(* consult the predicate in user-defined rules *)
let rec consultSinglePred rules predicate = match rules with 
                                          RuleList ([]) -> (false, []) |
                                          RuleList (clause::tail) -> (match clause with 
								     Fact fp -> (match(Unify.unifyPredicates (fp,predicate) ) with 
										None -> (consultSinglePred (RuleList tail) predicate ) |
										Some sig0 -> (true, sig0) ) |

								     Rule (headPred, (body,connList)) -> (match (Unify.unifyPredicates (headPred, predicate)) with
											       None -> (consultSinglePred (RuleList tail) predicate) |
											       Some sig0 -> (let newBody= List.map (substInPredicate sig0) body in
													 match (consult rules (Query (newBody, connList))) with
													   (false,_) -> (false,[]) |
													   (true, tailSig) -> (match (Unify.composeSubst tailSig sig0) with
															       None -> (true, []) |
															       Some finalSig -> (true, finalSig) ) ) ) ) 

     (*Consult a list of predicates*)
     (*returns a result which is of the form bool * subst*)
     (*predicates are left-assoc!*)
and consult rules query =
    match query with Query(predList, connList) -> (
   
    match predList with 
	 [] -> (raise (Failure "rule's body is empty!")) |

	 [singlePred] -> (eval_predicate rules singlePred) |

         _ -> (let lastPred= List.nth predList ((List.length predList) - 1) in
	       let frontPredList = rmLastItem predList in
	       let lastConn= List.nth connList ((List.length connList)-1) in
	       let frontConnList = rmLastItem connList in
               
	       let lastPredResult= eval_predicate rules lastPred in
	       let frontPredListResult= consult rules (Query(frontPredList, frontConnList)) in

	       let frontBool= fst frontPredListResult in
	       let lastBool= fst lastPredResult in

	       let finalSigOption = Unify.composeSubst (snd lastPredResult) (snd frontPredListResult) in
	       let finalSig = (match finalSigOption with 
			       None -> [] |
			       Some finSigma -> finSigma) in

	      match (lastConn) with
	      (",") -> (frontBool && lastBool, finalSig) |
	      (";") -> (frontBool || lastBool ,finalSig) |
	      _ -> (raise (Failure "unknown logical connective!") ) )  )



(*Consult a predicate which is either user-defined or built-in function*)
and eval_predicate rules predicate = match predicate with 
                                   Identifier fact -> ( match fact with
				                         "true" -> (true,[]) |
							 "false" -> (false,[]) |
							 _ -> consultSinglePred rules predicate ) |
						                  

				   Predicate (f, tl) -> (if (Evaluator.isBuiltInOp f)   (*It is built in operation*)
				                          
				                          then (if (List.length tl) == 1 then (
							    let singleTerm = (List.hd tl) in

							    if (Evaluator.isTypeTesting f) then  (*it is type testing*)
							         (Evaluator.typeTest f singleTerm, [])
							    
							    else (if f == "not" then (getBool (Evaluator.monOpApply f (Evaluator.eval_term singleTerm)), [])
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
									      ConstTerm _ -> (match(Evaluator.binOpApply "=:=" (Evaluator.eval_term lhs,rhsVal)) with
									                      BoolVal false -> (false,[]) |
											      BoolVal true -> (true,[]) |
											      _ -> (raise (Failure "should ret BoolVal")) ) |
									      Var x -> (true, [(x,Evaluator.val2Term rhsVal)]) |
									      _ -> (false, [])) |

								     _ -> (if (Evaluator.retBool f) then (match (binOpApply f (eval_term (fst eq), eval_term (snd eq)))
													 with BoolVal true -> (true,[]) |
													      BoolVal false -> (false,[]) |
													      _ -> (raise (Failure "Unknown exception.")) )

									   else (raise (Failure "predicate should return boolean!")) ) )) )  )    

				                          else  ( consultSinglePred rules predicate) );;  (* User defined functions *)




