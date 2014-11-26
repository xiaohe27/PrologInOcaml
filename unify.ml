open ProjCommon


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
			    |          ListTerm(tl) -> ListTerm(List.map (term_lift_subst subst) tl);;

(*Occurs check*)
let rec occurs x term = match term with 
			Var y -> (x = y) |
			ConstTerm c -> false |
			CompoundTerm(f,tl) -> occursInList x tl |
			ListTerm(tl) -> occursInList x tl
 
			and occursInList x termList = match termList with
				[] -> false |
				h::t -> if (occurs x h) then true else
					occursInList x t;;



let rec occursInSndPartOfSubst x sigma = match sigma with 
						[] -> false |
						(k,v)::tail -> if occurs x v then true 
								else occursInSndPartOfSubst x tail;;



(*substitute the constraints list*)
let rec eqlist_subst subst constraintList= match constraintList with
				       [] -> [] |
				       (term1, term2) :: tail -> (term_lift_subst subst term1,
							          term_lift_subst subst term2) 
								:: eqlist_subst subst tail;;


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
 
				      | (ListTerm tl1, ListTerm tl2) -> ( let pairList= (genPairList tl1 tl2) 
						 in (match pairList with 
							None -> None |
							Some newConstraints -> unify (eqlst' @ newConstraints)	))


|	(Var x, _) -> (if (occurs x t) then (None)
			else (let eqlst''= eqlist_subst ([(x,t)]) eqlst' in
				let sigmaResult= unify eqlst'' in
					match sigmaResult with 
						None -> None |
						Some sigma ->

				let sigma2= [(x, term_lift_subst sigma t)] in
					
				composeSubst sigma2 sigma) )    (*eliminate rule*)

|	_ -> (None)
				
));;

