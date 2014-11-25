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

