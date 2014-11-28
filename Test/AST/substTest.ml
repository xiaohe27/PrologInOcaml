open ProjCommon
open Unify

let subst0=[("X",ConstTerm(IntConst 2));("Y",CompoundTerm("cat", [Var "X"; ConstTerm(FloatConst 2.2)]))];;

let subst1=[("X", Var "Y")];;

let pred0=Identifier("Y");;

let pred1=Predicate("Pred1" , [CompoundTerm("cat",[ConstTerm(StringConst("tom")); Var "X"]); CompoundTerm("X",[Var "Y"])]);;

let test1= substInPredicate subst0 pred0;;
let test2= substInPredicate subst0 pred1;;
let test3= substInPredicate subst1 pred0;;
let test4= substInPredicate subst1 pred1;;