open Parser
open Lexer


(* Lexing.from_channel stdin *)

(* Entry main *)


let parse s = Parser.rules Lexer.token (Lexing.from_string s);;