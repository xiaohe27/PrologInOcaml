open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter
open Glue

let clause1Str= "cat(tom).";;
let queryStr= "?-cat(X).";;

let parsedRules= Glue.parseRules (clause1Str);;

let parsedPgm= Glue.parseProgram (clause1Str ^ queryStr);;