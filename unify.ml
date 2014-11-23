type arg = VarArg of string | ConstArg of const and
const = 
BoolConst of bool 
| IntConst of int
| FloatConst of float
| StringConst of string
| NilConst
| UnitConst;;

type term = AtomTerm of arg | CompoundTerm of (string * arg list) |
            ListTerm of term list;;



