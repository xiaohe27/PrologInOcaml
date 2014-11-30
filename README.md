PrologInOcaml
=============

This is a project for implementing Prolog in Ocaml.

Basically, it is an interpreter for Prolog programs.

Participants: He Xiao and Shijiao Yuwen


#Install
sh make.sh

#Run 
##Test parser
Option 1: using ocaml's interactive terminal:
ocaml (enter ocaml command window)
#load "projCommon.cmo";;
#load "lexer.cmo";;
#load "parser.cmo";;
#load "unify.cmo";;
#load "evaluator.cmo";;
#load "interpreter.cmo";;

#use "glue.ml";;

Option 2 (Generate executable): 
ocamlc -o [YourOcamlProgram.exe] projCommon.cmo lexer.cmo parser.cmo
unify.cmo evaluator.cmo interpreter.cmo glue.cmo <YourOcamlProgram.ml>

#Test
GOTO Test/ReadMe.txt for more info.


