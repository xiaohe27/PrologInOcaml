open ProjCommon


(*Find the value corresponding to the key in the subst if found*)
let rec getTermFromSubst subst key =
 match subst with
	[] -> None |
	(var,term)::tail -> 
		(if var = key then (Some term)
		else (getTermFromSubst tail key));;

(* Given a substitution and a variable, the replacement term will be returned.  *)
let rec subst_fun subst = fun strVar -> match subst with
				[] -> Var strVar  |
				(var, term)::tail -> if strVar = var then term 
							else subst_fun tail strVar;;

(*Substitute all the matching vars inside a term*)
let rec term_lift_subst subst term = match term with 
                                       Var v -> (subst_fun subst v) 
                            |          ConstTerm c -> (term) 
			    |          CompoundTerm(f,tl) -> CompoundTerm(f, List.map (term_lift_subst subst) tl)
			    |          ListTerm(tl) -> ListTerm(List.map (term_lift_subst subst) tl) 
			    | (PredAsTerm pred) -> (PredAsTerm (substInPredicate subst pred)) 


and substInPredicate subst predicate = match predicate with 
						Identifier _ -> predicate |
						Predicate(f,tl) -> Predicate(f, List.map (term_lift_subst subst) tl) |
						VarAsPred v -> (match (getTermFromSubst subst v) with
								None -> (VarAsPred v) |
								Some term -> (match term with 
										Var x -> (VarAsPred x) |
										ConstTerm c -> (match c with
												BoolConst b -> (Identifier (string_of_const c)) |
												_ -> (raise (Failure ("Callable is expected, found "^ (string_of_term term)))) ) |

										CompoundTerm(f,tl) -> (if (ProjCommon.retBool f) then (Predicate(f,tl)) 
													else (raise (Failure ("Callable is expected, found "^ (string_of_term term)))) ) |

										ListTerm(tl) -> (raise (Failure ("Callable is expected, found "^ (string_of_term term)))) |
										PredAsTerm(pred) -> (pred) ));;
 

(*Occurs check*)
let rec occurs x term = match term with 
			Var y -> (x = y) |
			ConstTerm c -> false |
			CompoundTerm(f,tl) -> occursInList x tl |
			ListTerm(tl) -> occursInList x tl  |
			PredAsTerm pred -> 
				(match pred with
				 Identifier _ -> false |
				 Predicate(f,tl) -> occursInList x tl |
				 VarAsPred v -> (if x = v then true else false))
 
			and occursInList x termList = match termList with
				[] -> false |
				h::t -> if (occurs x h) then true else
					occursInList x t;;



let rec occursInSndPartOfSubst x sigma = match sigma with 
						[] -> false |
						(k,v)::tail -> if occurs x v then true 
								else occursInSndPartOfSubst x tail;;

let rec occursInSubstList x lst = match lst with
                                    [] -> false |
				    (k,v)::tail -> (if x=k then true else occursInSubstList x tail);;


(*substitute the constraints list*)
let rec eqlist_subst subst constraintList= match constraintList with
				       [] -> [] |
				       (term1, term2) :: tail -> (term_lift_subst subst term1,
							          term_lift_subst subst term2) 
								:: eqlist_subst subst tail;;

let rec updateVarInSubst subst sigma =
	match sigma with
	[] -> [] |
	(k,v)::tail -> ((subst_fun subst k), v)::(updateVarInSubst subst tail) ;;

let rec updateSubst sigma substList = match substList with 
						[] -> Some [] |
						(x, term)::tail -> if occursInSndPartOfSubst x sigma then None 
								else								 
								match (updateSubst sigma tail) with
									None -> None |
									Some tailResultList -> Some ((x, term_lift_subst sigma term) :: tailResultList);;

let rec updateListWithElement lst (key, value)= match lst with
						[] -> [(key, value)] |
						(k, v)::tail -> 
							if k=key then (updateListWithElement tail (key, value)) 
							else (k, v):: (updateListWithElement tail (key, value));; 

let rec updateListWithList newList oldList = match newList with 
						[] -> oldList |
						keyValPair :: tail -> updateListWithList tail (
						updateListWithElement oldList keyValPair);;

let rec composeSubst sigma substList = match substList with 
						[] -> Some sigma |
						(x, mt)::tail -> 
							match (updateSubst sigma substList) with
								None -> None |
								Some sigma2 ->
							Some (updateListWithList sigma2 sigma);;


let rec genPairList list1 list2 = if (List.length list1 != List.length list2) 
					then None
				   else match (list1, list2) with 
					([],_) -> Some [] |
					(_, []) -> Some [] | 
					(h1::t1, h2::t2) -> let tailR=genPairList t1 t2 in
						match tailR with None -> None |
							Some tailResultList ->
								Some ((h1,h2)::tailResultList);;



let getHead termList = match termList with                          
				[] -> None |
			        h::_ -> Some h;;
			  

let getTail tl =
                       match tl with 
			   [] -> (ListTerm []) |
			   [t] -> (ListTerm []) |
			   [_;term] -> (match term with 
			              Var _ -> (term) |
				      _ -> (ListTerm [term])
				     ) |
			   _::tail -> (ListTerm tail) ;;
			    


(*Unify the body part of a rule.*)
let rec unify eqlst = match eqlst with 
			[] -> Some [] |
			(s,t)::eqlst' -> (
				if s = t then unify eqlst'        (*delete rule*)
				else (match (s,t) with 
					(ConstTerm _, Var _) -> (unify ((t,s)::eqlst')) |

					(CompoundTerm(_,_), Var _) -> (unify ((t,s)::eqlst')) |
					
					(ListTerm _, Var _) -> (unify ((t,s)::eqlst')) |  (*orient rule*)
					
					(CompoundTerm(f1, termList1), CompoundTerm(f2, termList2) ) -> (
						if f1 = f2 then (
						let pairList= (genPairList termList1 termList2) 
						 in (match pairList with 
							None -> None |
							Some newConstraints -> unify (eqlst' @ newConstraints)	)
						

) else (None) )               (*Decompose rule*)
 
				      | (ListTerm tl1, ListTerm tl2) -> (
					  if (tl1 = [] && tl2 = []) then unify eqlst'
					  else(

					 let tl1Head= getHead tl1 in
					 let tl1Tail= getTail tl1 in
					 let tl2Head= getHead tl2 in
					 let tl2Tail= getTail tl2 in

					 match (tl1Head, tl2Head) with
					   (None, _) -> (None) |
					   (_, None) -> (None) |
					   (Some head1, Some head2) -> (
					     let newConstraints= [(head1,head2);(tl1Tail,tl2Tail)] in
					     unify (eqlst' @ newConstraints)
					    )

					   ) )


|	(Var x, _) -> (
                        if (x = "_") then (Some [])
                        else if (occurs x t) then (None)
			else (
                                let eqlst''= eqlist_subst ([(x,t)]) eqlst' in
				let sigmaResult= unify eqlst'' in
					match sigmaResult with 
						None -> None |
						Some sigma ->

				let sigma2= [(x, term_lift_subst sigma t)] in
					
				composeSubst sigma2 sigma) )    (*eliminate rule*)

|	_ -> (None)
				
));;


(* Used to unify the query and head *)
let rec unifyHead eqlst = match eqlst with 
			[] -> Some [] |
			(s,t)::eqlst' -> (
				if s = t then unifyHead eqlst'        (*delete rule*)
				else (match (s,t) with 
					(ConstTerm _, Var _) -> (unifyHead ((t,s)::eqlst')) |

					(CompoundTerm(_,_), Var _) -> (unifyHead ((t,s)::eqlst')) |
					
					(ListTerm _, Var _) -> (unifyHead ((t,s)::eqlst')) |  (*orient rule*)
					
					(CompoundTerm(f1, termList1), CompoundTerm(f2, termList2) ) -> (
						if f1 = f2 then (
						let pairList= (genPairList termList1 termList2) 
						 in (match pairList with 
							None -> None |
							Some newConstraints -> unifyHead (eqlst' @ newConstraints)	)
						

) else (None) )               (*Decompose rule*)
 
				      | (ListTerm tl1, ListTerm tl2) -> ( 
					  if (tl1 = [] && tl2 = []) then unify eqlst'
					  else(

					 let tl1Head= getHead tl1 in
					 let tl1Tail= getTail tl1 in
					 let tl2Head= getHead tl2 in
					 let tl2Tail= getTail tl2 in

					 match (tl1Head, tl2Head) with
					   (None, _) -> (None) |
					   (_, None) -> (None) |
					   (Some head1, Some head2) -> (
					     let newConstraints= [(head1,head2);(tl1Tail,tl2Tail)] in
					     unify (eqlst' @ newConstraints)
					    )

					   )
					 
					 )


|	(Var x, _) ->   (
                        if (x = "_") then (Some [])
                        else (if (occurs x t) then (None)
			else ( match (unifyHead eqlst') with
				None -> None |
				Some tailResult -> if (occursInSubstList x tailResult) then
				                      (match t with 
							Var _ -> Some tailResult |
							_ -> Some (updateListWithElement tailResult (x,t)) ) 
						      
						       else (Some ((x,t)::tailResult) )  ) ))
				
					(*eliminate rule*)

|	_ -> (None)
				
));;


let rec unifyPredicates (pred1,pred2) = 
	match (pred1, pred2) with
		(Identifier id1, Identifier id2) -> (if id1=id2 then Some [] else None) |
		(Identifier id1, Predicate(f,tl)) -> (None) |
		(Identifier _, VarAsPred _) -> (raise (Failure "Do not waste your time! Maybe after 1000 years we can get the answer?")) |
		(Predicate(f,tl), Identifier id2) -> (None) |
		(Predicate(f1,tl1), Predicate(f2,tl2)) -> (
			if (not(f1 = f2)) then None 
			else( match (genPairList tl1 tl2) with
				None -> None |
				Some eqlst -> (unifyHead eqlst))) |
		(Predicate _, VarAsPred _) -> (raise (Failure "Do not waste your time! Maybe after 1000 years we can get the answer?")) |
		(VarAsPred _, _) -> (raise (Failure "Do not waste your time! Maybe after 1000 years we can get the answer?"));;









