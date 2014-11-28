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

let result1 = Glue.simulatePrologProgram pgm1;;
let result2 = Glue.simulatePrologProgram pgm2;;
let result3 = Glue.simulatePrologProgram pgm3;;