ocamlc -I "../../" -o "$1".exe projCommon.cmo lexer.cmo parser.cmo unify.cmo evaluator.cmo interpreter.cmo glue.cmo "$1".ml
