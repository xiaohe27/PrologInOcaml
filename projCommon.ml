(* ================= Parsing ================= *)
(* Types *)
type const = 
BoolConst of bool 
| IntConst of int
| FloatConst of float
| StringConst of string;;

type term = Var of string | ConstTerm of const |
		CompoundTerm of string * (term list) |
            ListTerm of term list;;


type predicate = Identifier of string | Predicate of string * (term list);;

		(* predicates can either be separated by comma or by semi-colon *)
type clause = Fact of predicate | Rule of predicate * (predicate list * string list);;

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


(*print term*)
let string_of_const c =
    match c 
    with IntConst n    -> string_of_int n
       | BoolConst b   -> if b then "true" else "false"
       | FloatConst f  -> string_of_float f
       | StringConst s -> "\"" ^ s ^ "\"";;
      

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
	(ListTerm tl) -> ("[" ^ (stringOfTermList tl) ^ "]");;

let rec string_of_subst subst = match subst with
				[] -> "" |
				(v,t)::tail -> (v ^ "=" ^ (string_of_term t)) ^ 
				 ".\n" ^ (string_of_subst tail) ;;

let string_of_predicate pred=match pred with
				Identifier(id) -> id |
				Predicate(f,tl) -> (f ^ "(" ^
				(stringOfTermList tl) ^ ")" ) ;;

let rec stringOfPredList predList connList= match predList with
					[] -> "" |
					[pred] -> (string_of_predicate pred) |
					pred::tail -> ((string_of_predicate (pred)) ^ (List.hd connList) ^ (stringOfPredList tail (List.tl connList)));;

let string_of_clause clause = match clause with
				Fact fp -> ("Fact "^(string_of_predicate fp) ^ "\t") |
				Rule (hp,(body,connList)) -> ("Rule: "^(string_of_predicate hp) ^ " :- " ^ (stringOfPredList body connList));;