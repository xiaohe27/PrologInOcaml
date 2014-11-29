open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter
open Glue

let pgm1 = "cat(tom). ?-cat(X).";;
let pgm2 = "cat(tom). animal(X):- cat(X). ?-animal(X).";;
let pgm3 = "cat(tom). animal(X):- cat(X). ?-animal(tom).";;

(*let pgm4 = "cat.?-(2 > 5, 3> 7; X is 2 + 3)";;*)

let pgm5 = "even(X) :- Y is X / 2 . ?- even(6) .";;

let result1 = Glue.simulateProgram pgm1;;
let result2 = Glue.simulateProgram pgm2;;
let result3 = Glue.simulateProgram pgm3;;

(*let result4 = Glue.simulateProgram pgm4;;*)
let result5 = Glue.simulateProgram pgm5;;