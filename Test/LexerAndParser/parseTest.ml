open ProjCommon
open Lexer
open Parser
open Unify
open Evaluator
open Interpreter
open Glue

let clause1Str= "cat(tom).";;
let queryStr= "?-cat(X).";;

let parsedAST= Glue.parse (clause1Str ^ queryStr);;