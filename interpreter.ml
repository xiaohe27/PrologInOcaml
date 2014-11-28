open ProjCommon
open Evaluator
open Unify


let isBuiltInOp op = if (Evaluator.isTypeTesting op) then true 
                      else if (op = "=" || op = "is") then true else false;; 


let rec consultSinglePred rules predicate = match rules with 
                                          RuleList ([]) -> (false, []) |
                                          RuleList (clause::tail) -> (match clause with 
								     Fact fp -> (match(Unify.unifyPredicates (fp,predicate) ) with 
										None -> (consultSinglePred (RuleList tail) predicate ) |
										Some sig0 -> (true, sig0) ) |

								     Rule (headPred, body) -> (match (Unify.unifyPredicates (headPred, predicate)) with
											       None -> (consultSinglePred (RuleList tail) predicate) |
											       Some sig0 -> (let newBody= List.map (substInPredicate sig0) body in
													 match (consult rules (Query newBody)) with
													   (false,_) -> (false,[]) |
													   (true, tailSig) -> (match (Unify.composeSubst tailSig sig0) with
															       None -> (true, []) |
															       Some finalSig -> (true, finalSig) ) ) ) ) 


      (*returns a result which is of the form bool * subst*)
and consult rules query =
    match query with Query(predList) -> (
   
    match predList with 
	 [] -> (raise (Failure "rule's body is empty!")) |
         fstPred::tailPredList -> (
	    match (consultSinglePred rules fstPred) with
		(false, _) -> (false, []) |
		(true, sig0) -> (if tailPredList = [] then (true, sig0) else (
	                        let newBody= List.map (substInPredicate sig0) tailPredList in
				    match (consult rules (Query newBody)) with
				  (false,_) -> (false,[]) |
				  (true, tailSig) -> (match (Unify.composeSubst tailSig sig0) with
						     None -> (true, []) |
						     Some finalSig -> (true, finalSig))  ) ) 
	       ) );;



let rec eval_predicate rules predicate = match predicate with 
                                   Identifier fact -> ( match fact with
				                         "true" -> (true,[]) |
							 "false" -> (false,[]) |
							 _ -> consultSinglePred rules predicate ) |
						                  

				   Predicate (f, tl) -> (if (isBuiltInOp f)   (*It is built in operation*)
				                          then (if (Evaluator.isTypeTesting f) then  (*it is type testing*)
							         (if (List.length tl)!=1  then (false, []) else (Evaluator.typeTest f (List.hd tl), []) )
							        else (      (*other built-in ops*)
							             if (List.length tl) != 2 then raise (Failure "number of args to the function is wrong")
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
								     _ -> (raise (Failure "Not supported yet."))) )  )    )

				                          else  ( consultSinglePred rules predicate) );;




