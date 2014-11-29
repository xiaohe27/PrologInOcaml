open ProjCommon
open Glue
(* Term *)
let term0=ListTerm([Var "x"; CompoundTerm("f",[ConstTerm(IntConst 5); Var "y"]); Var "z"]);;
let termX= Var "X";;
let termY= Var "W";;

let term1= CompoundTerm("x",[term0;termX;termY]);;

let freeVarsInTerm1= ProjCommon.freeVarsInTerm term1;;

let pgmStr= "f(X,Y) :- ((X + Y) * Z) =:= 7.";;
let RuleList([rule])= (Glue.parseRules (pgmStr));;
let fvInRule= ProjCommon.freeVarsInClause rule;;
