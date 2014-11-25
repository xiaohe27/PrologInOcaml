type const = 
BoolConst of bool 
| IntConst of int
| FloatConst of float
| StringConst of string
| UnitConst;;

type term = Var of string | ConstTerm of const |
		CompoundTerm of (string * term list) |
            ListTerm of term list;;
