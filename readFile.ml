open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter
open Glue

(*Get the path of the prolog program*)
let file= Sys.argv.(1);;

let lexbuf = Lexing.from_channel (open_in file) ;;
let pgm= Parser.program Lexer.token lexbuf ;;

let result= Glue.refineResult (execProgram pgm) pgm in printResult result;;