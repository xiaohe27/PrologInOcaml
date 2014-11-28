open ProjCommon
open Evaluator
open Unify


let isBuiltInOp op = if (Evaluator.isTypeTesting op) then true 
                      else if (op = "=" || op = "is") then true else false;; 

let rec eval_predicate rules predicate = match predicate with 
                                   Identifier fact -> ( match fact with
				                         "true" -> (true,[]) |
							 "false" -> (false,[]) |
							 _ -> consultSinglePred rules predicate ) |
						                  

				   Predicate (f, tl) -> (if (isBuiltInOp f)   (*It is built in operation*)
				                          then (if (Evaluator.isTypeTesting f) then  (*it is type testing*)
							         (if (List.length tl)!=1  then (false, []) else (Evaluator.typeTest f (List.hd tl), []) )
							        else (      (*other built-in ops*)
							             if (List.length tl) != 2 then raise (Failure "number of args to the function "^f^" is wrong")
									 else (let eq= (List.hd tl, List.nth 2 tl) in
									 
							             match f with 
								     "=" -> (match (Unify.unify [eq]) with 
									     None -> (false, []) |
									    Some sig -> (true, sig) ) (*Unification op*) |

								     "is" -> (let lhs= fst eq in 
									      let rhs= snd eq in
									     let rhsVal = Evaluator.eval_term rhs in
									     
									     match lhs with 
									      ConstTerm _ -> (match(Evaluator.binOpApply "=:=" (lhs,rhs)) with
									                      BoolVal false -> (false,[]) |
											      BoolVal true -> (true,[]) |
											      _ -> (raise (Failure "should ret BoolVal")) ) |
									      Var x -> (true, [(x,Evaluator.val2Term rhsVal)]) |
									      _ -> (false, [])) |
								     _ -> (raise (Failure "Not supported yet.")))  )    )

				                          else  ( consultSinglePred rules predicate) );;




let consultSinglePred rules predicate = match rules with 
                                          RuleList ([]) -> (false, []) |
                                          RuleList (clause::tail) -> () ;;


      (*returns a result which is of the form bool * subst*)
let consult rules query = match query ;;
   
