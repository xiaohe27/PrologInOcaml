type const = 
BoolConst of bool 
| IntConst of int
| FloatConst of float
| StringConst of string
| UnitConst;;

type term = Var of string | ConstTerm of const |
		CompoundTerm of string * (term list) |
            ListTerm of term list;;


type predicate = Identifier of string | Predicate of string * (term list);;

		(* predicates can either be separated by comma or by semi-colon *)
type clause = Fact of predicate | Rule of predicate * (predicate list * string list);;

type query = Query of (predicate list * string list);;

type program = Prog of clause list * query | ProgFromQuery of query;;
