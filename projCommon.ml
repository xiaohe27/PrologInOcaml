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

type clause = Fact of predicate | Rule of predicate * (predicate list);;

type query = Query of predicate list;;

type rules = RuleList of clause list;;

type program = Prog of rules * query | ProgFromQuery of query;;


(* ================= Interpreting ================= *)
(* Values and Memory *)
type memory = (string * value) list
and value =
   BoolVal of bool
  | IntVal of int                                 
  | FloatVal of float
  | StringVal of string                           
  | ListVal of value list
  

let make_mem x y = ([(x,y)]:memory)
let rec lookup_mem (gamma:memory) x =
  match gamma with
     []        -> raise (Failure ("identifier "^x^" unbound"))
   | (y,z)::ys -> if x = y then z else lookup_mem ys x
let sum_mem (delta:memory) (gamma:memory) = ((delta@gamma):memory)
let ins_mem (gamma:memory) x y = sum_mem (make_mem x y) gamma

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
  

let compact_memory m =
  let rec comp m rev_comp_m =
      (match m with [] -> List.rev rev_comp_m
        | (x,y) :: m' ->
           if List.exists (fun (x',_) -> x = x') rev_comp_m
              then comp m' rev_comp_m
           else comp m' ((x,y)::rev_comp_m))
  in comp m []

(*memory output*)
let print_memory m =
    let cm = compact_memory m in
    let rec print_m m = 
    (match m with
        []           -> ()
      | (x, v) :: m' -> print_m m';
                        print_string ("val "^x ^ " = ");
                        print_value v;
                        print_string (";\n") ) in
    print_m cm


(* result *)
type result = bool * memory ;;