(* ================= Parsing ================= *)
(* Types *)
type const = 
BoolConst of bool 
| IntConst of int
| FloatConst of float
| StringConst of string;;

type term = Var of string | ConstTerm of const |
		CompoundTerm of string * (term list) |
            ListTerm of term list |
	    PredAsTerm of predicate


and predicate = Identifier of string | Predicate of string * (term list)
		| VarAsPred of string ;;

(* predicates can either be separated by comma or by semi-colon *)
type clause = Fact of predicate | Rule of predicate * 
				 (predicate list * string list);;

type query = Query of (predicate list * string list);;

type rules = RuleList of clause list;;

type program = Prog of rules * query | ProgFromQuery of query;;

(* ================= Interpreting ================= *)
(* Values *)
type value =
   BoolVal of bool
  | IntVal of int                                 
  | FloatVal of float
  | StringVal of string                           
  | ListVal of value list
  

type blacklist = (int * string list) list;;



(*value output*)
let rec print_value v =
   match v with
           
  | IntVal n          -> if n < 0 then (print_string "~"; print_int (abs n)) else print_int n 
  | FloatVal r        -> print_float r
  | BoolVal true      -> print_string "true"
  | BoolVal false     -> print_string "false"
  | StringVal s       -> print_string ("\"" ^ s ^ "\"")
 
  | ListVal l         -> print_string "[";
                         (let rec pl = function
                              []     -> print_string "]"
                            | v::vl  -> print_value v;
                                        if vl <> []
                                        then
                                           print_string "; ";
                                        pl vl
                              in pl l)
  

(*substitution*)
type subst = (string * term) list;;

(* result *)
type result = bool * subst ;;

(*indexed rules*)
type indexedRules = (int * clause) list;;



(*print term*)
let string_of_const c =
    match c 
    with IntConst n    -> string_of_int n
       | BoolConst b   -> if b then "true" else "false"
       | FloatConst f  -> string_of_float f
       | StringConst s -> s;;
      

let isInfix op = match op with
					"+" -> (true) |
					"-" -> (true) |
					"*" -> (true) |
					"/" -> (true) |
					"**" -> (true) |
					_ -> false ;;

let rec stringOfTermList tl = match tl with
				[] -> "" |
				[h] -> string_of_term h |
				h::t -> string_of_term h ^ ", "^stringOfTermList t

and string_of_term term=
  match term with
    (Var v) -> (v) |
	(ConstTerm const) -> (string_of_const const) |
	(CompoundTerm(f,tl)) -> ( if ((isInfix f) && (List.length tl) = 2) then (string_of_term (List.hd tl) ^ f ^ string_of_term (List.nth tl 1)) else (
		f ^ "(" ^ (stringOfTermList tl) ^ ")"  ) ) 
	 |
	(ListTerm tl) -> ("[" ^ (stringOfTermList tl) ^ "]") |
	
    PredAsTerm pred -> (string_of_predicate pred)

and string_of_subst subst = match subst with
				[] -> "" |
				(v,t)::tail -> (v ^ "=" ^ (string_of_term t)) ^ 
				 ".\n" ^ (string_of_subst tail)

and string_of_predicate pred=match pred with
				Identifier(id) -> id |
				Predicate(f,tl) -> (f ^ "(" ^
				(stringOfTermList tl) ^ ")" ) |
				VarAsPred(v) -> v ;;

let rec stringOfPredList predList connList= match predList with
					[] -> "" |
					[pred] -> (string_of_predicate pred) |
					pred::tail -> ((string_of_predicate (pred)) ^ (List.hd connList) ^ (stringOfPredList tail (List.tl connList)));;

let string_of_clause clause = match clause with
				Fact fp -> ("Fact "^(string_of_predicate fp) ^ "\t") |
				Rule (hp,(body,connList)) -> ("Rule: "^(string_of_predicate hp) ^ " :- " ^ (stringOfPredList body connList));;

let rec stringOfRuleList rules = match rules with
	RuleList clst -> (match clst with 
		[] -> "" |
		h::t -> (string_of_clause h)^"\n"^
			(stringOfRuleList (RuleList t)));;

let rec stringOfIndexedRules indexRules =
	match indexRules with
	[] -> "" |
	(i,clause)::tail -> ("Rule " ^ (string_of_int i) ^ ":"
				^ (string_of_clause clause) ^
				("\n" ^ stringOfIndexedRules tail)) ;;


let rec string_of_stringList strList =
 match strList with
	[] -> "" |
	str::tail -> str ^ ";" ^(string_of_stringList tail);;

let rec stringOfBlackList blist =
    match blist with
	[] -> "" |
	(n, sl)::(tail) ->
	("(" ^ (string_of_int n) ^ (",") ^(string_of_stringList sl)^")" )
	^";\n"^(stringOfBlackList tail)
;;



(* Fresh Name stuff *)

let int_to_string n =
    let int_to_int_26_list n =
        let rec aux n l =
            if n <= 0 then l else let c = ((n-1) mod 26) in aux ((n -(c+1))/26) (c::l)
        in aux n []
    in
        let rec aux l = match l with [] -> ""
                            | n::ns -> (String.make 1 (Char.chr (n + 97))) ^ aux ns
        in aux (int_to_int_26_list n);;

let freshFor lst = 
    let rec fresh_ n = 
        if List.mem (int_to_string n) lst
           then fresh_ (n+1)
        else int_to_string n
    in fresh_ 1 ;;


let rec get_n_freshVars n lst =
	if(n <= 0) then (raise (Failure "Cannot get 0 fresh vars"))
	else (let fstFresh= freshFor lst in (if n=1 then ([fstFresh])
		else (fstFresh::(get_n_freshVars (n-1) (fstFresh::lst)))) );;
	

(* End Fresh name stuff *)

(*Get free vars in a term*)
let rec freeVarsInTerm term = 
			match term with 
			Var v -> [v] |
			ConstTerm _ -> [] |
			CompoundTerm(f,tl) -> (
				toSingleStrArr (List.map (freeVarsInTerm) tl) ) |

			ListTerm(tl) -> (
				toSingleStrArr (List.map (freeVarsInTerm) tl)) |
			
			PredAsTerm (pred) -> (freeVarsInPredicate pred) 
and

toSingleStrArr listList = 
	match listList with 
	[] -> [] |
	[singleList] -> (
	    match singleList with
		[] -> [] |
		str::tail -> (str::(toSingleStrArr [tail])) ) |

	fstList::tailListList -> (
		fstList @ (toSingleStrArr (tailListList)))

and

(*Get free vars in other structures*)
rmXInList x lst = match lst with []->[] |
				h::t->if h=x then rmXInList x t
					else h::(rmXInList x t)

and rmDup lst = match lst with []->[] |
			h::t-> h::(rmDup (rmXInList h t))


and freeVarsInPredicate pred = 
	match pred with
	Identifier id -> ([]) |
	Predicate (f,tl) -> (toSingleStrArr (List.map (freeVarsInTerm) tl)) |
	VarAsPred v -> [v] ;;

let rec listSubtract list1 list2 = 
	match list2 with
	[] -> list1 |
	h::t -> let newList1 = (rmXInList h list1)
		in(listSubtract newList1 t) ;;

let freeVarsInClause clause = 
	match clause with 
	Fact pred -> (freeVarsInPredicate pred) |
	Rule (head, (body,conn)) -> (let binders= freeVarsInPredicate head in
		let freeVarInBody = (toSingleStrArr (List.map (freeVarsInPredicate) body) ) in rmDup(listSubtract freeVarInBody binders) ) ;;
	
let freeVarsInQuery query=
	match query with 
	Query(predList,connList) -> 
		(toSingleStrArr (List.map (freeVarsInPredicate) predList) );;

let freeVarsInRuleList rules = 
	match rules with
	RuleList(clauseList) -> 
		(toSingleStrArr (List.map (freeVarsInClause) clauseList));;

let freeVarsInProgram pgm =
	match pgm with
	Prog(rules,query) -> (
		let binders= freeVarsInQuery query in
		let freeVarsInRules= freeVarsInRuleList rules in
		rmDup (listSubtract freeVarsInRules binders) ) | (*not precise*)
	
	ProgFromQuery(query) -> (freeVarsInQuery query) ;;


(*Type testing*)
let isTypeTesting op = match op with 
			"var" -> true |
			"nonvar" -> true |
			"atom" -> true |
			"integer" -> true |
			"float" -> true |
			"number" -> true |
			"atomic" -> true |
			"compound" -> true |
			"callable" -> true |
			"list" -> true |
			"is_list" -> true |
			_ -> false;;


let retBool op = match op with 
			">" -> true |
			"<" -> true |
			"=:=" -> true |
			">=" -> true |
			"<=" -> true |
			"=\\=" -> true |
			"," -> true |
			";" -> true |
			_ -> false;;

(* Test whether a function is built-in function *)
let isBuiltInOp op = if (isTypeTesting op) then true 
                      else if (op = "=" || op = "is" || op = "write"
		               || op = "nl" ) then true else (retBool op);; 


(* Test whether a predicate only contains vars in the term list *)
let rec onlyVarsInPred pred =
	match pred with
	Identifier id -> (false) |
	Predicate (f,termL) -> (onlyVarsInTermList termL) |
	VarAsPred v -> (true)
 
and onlyVarsInTermList tl =
	match tl with
	[] -> true |
	t1::tail -> (
		match t1 with
		Var v -> (onlyVarsInTermList tail) | 
		ConstTerm _ -> false |
		CompoundTerm _ -> false |
            	ListTerm _ -> false |
	    	PredAsTerm pred0 -> onlyVarsInPred pred0);;

let rec onlyConstInPred pred =
	match pred with
	Identifier id -> (true) |
	Predicate (f,termL) -> (onlyConstInTermList termL) |
	VarAsPred v -> (false)
 
and onlyConstInTermList tl =
	match tl with
	[] -> true |
	t1::tail -> (
		match t1 with
		Var v -> (false) | 
		ConstTerm _ -> onlyConstInTermList tail |
		CompoundTerm _ -> onlyConstInTermList tail |
            	ListTerm _ -> onlyConstInTermList tail |
	    	PredAsTerm pred0 -> onlyConstInPred pred0);;



(* Test whether a string str1 contains another string str2 *)
let rec strContains str1 str2 =
if str1=str2 then true 
else (
let len1=String.length str1 in
let len2=String.length str2 in 
if len1 <= len2 then false 
else 
let sub1 = String.sub str1 0 (len2) in 
if sub1 = str2 then true 
else
let remain1= String.sub str1 1 (len1 - 1) in 
strContains remain1 str2
);; 
			

let rec isAllTrueBoolList boolList=
match boolList with
[] -> true |
curBool::tail -> 
(if curBool = false then false
else (isAllTrueBoolList tail))
;; 


let rec getAllConstInTermList tl =
match tl with
[] -> [] |

curTerm::tail -> 
(match curTerm with
		Var v -> getAllConstInTermList tail | 
		PredAsTerm pred0 -> getAllConstInTermList tail |
		_ -> curTerm::(getAllConstInTermList tail) 		
	    	
)
;;

let getAllConstInPred pred=
match pred with
	Identifier _ -> [] |
	Predicate (_,termL) -> (getAllConstInTermList termL) |
	VarAsPred _ -> []


(*Given a string list of items in blacklist, 
test whether all the constant terms in constTL 
occur in one entry of the list*)
let rec isContainedInOneStrInTheList strList constTL =
match strList with
[] -> false |
curStr::tail -> (
if (areFoundInOneStr constTL curStr)
then (true)
else (isContainedInOneStrInTheList tail constTL)
) 

and areFoundInOneStr constTL str=
let listOfTermStr= List.map (string_of_term) constTL in
let occursCheckList= List.map (strContains str) listOfTermStr in
isAllTrueBoolList occursCheckList
;;


let addSigToResult sigma result =
match result with
(b,subst) -> (b, sigma @ subst);;

let addSigToResultList sigma rl =
List.map (addSigToResult sigma) rl;;



