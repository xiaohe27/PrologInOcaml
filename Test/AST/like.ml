open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter
open Glue

let rules = "likes(mary,food).
likes(mary,wine).
likes(john,wine).
likes(john,mary).";;

let fstQ = "?- likes(mary,food)." 

let sndQ = "?- likes(john,wine)." 
let thirdQ = ?- likes(john,food)."

let pgm = rules ^ fstQ;;

let result = Glue.simulateProgram pgm;;