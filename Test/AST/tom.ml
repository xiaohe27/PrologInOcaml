open ProjCommon
open Unify
open Evaluator
open Interpreter

let tomTerm=ConstTerm(StringConst "tom");;

let pred=Predicate("cat", [tomTerm]);;

let predX=Predicate("cat", [Var "X"]);;

let clause= Fact(pred);;

let rules= RuleList([clause]);;

let query= Query([predX],[]);;

(*Test now! Test functions consult and eval_predicate*)
let result=consult rules query;;

let eval=eval_predicate rules predX;;