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

sh genExecutable.sh play
sh genExecutable.sh readFile
sh genExecutable.sh debugger
sh genExecutable.sh backtrack