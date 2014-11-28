ocamlc -c projCommon.ml

ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml

ocamllex lexer.mll
ocamlc -c lexer.ml

ocamlc -c unify.ml
ocamlc -c evaluator.ml
ocamlc -c interpreter.ml

ocamlc -c glue.ml
ocamlc -c main.ml
#ocamlc -o Parser_EXE projCommon.cmo lexer.cmo parser.cmo
