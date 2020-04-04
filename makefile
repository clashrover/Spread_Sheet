RUN:
	ocamlc -c bmodule.ml
	ocamllex lexer.mll 
	ocamlyacc parser.mly 
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o main str.cma bmodule.cmo lexer.cmo parser.cmo main.cmo
	./main input.txt sheet.csv 2 5