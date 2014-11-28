open ProjCommon
open Unify
open Evaluator
open Interpreter

let subst0=[("X",ConstTerm(IntConst 2));("Y",CompoundTerm("cat", [Var "X"; ConstTerm(FloatConst 2.2)]))];;

let subst1=[("X", Var "Y")];;

let pred0=Identifier("Y");;

let pred1=Predicate("Pred1" , [CompoundTerm("cat",[ConstTerm(StringConst("tom")); Var "X"]); CompoundTerm("X",[Var "Y"])]);;


