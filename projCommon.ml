type const = 
BoolConst of bool 
| IntConst of int
| FloatConst of float
| StringConst of string
| UnitConst;;

type term = Var of string | ConstTerm of const |
		CompoundTerm of (string * term list) |
            ListTerm of term list;;


type predicate = Identifier of string | Predicate of string * term list;;

type clause = Fact of predicate | Rule of predicate * predicate list;;

type query = Query of predicate * predicate list;;

type program = Prog of clause * clause list * query | Prog of query;;
