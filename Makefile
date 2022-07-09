all: expr MicroML

expr:
	ocamlbuild -use-ocamlfind expr.byte

MicroML:
	ocamlbuild -use-ocamlfind MicroML.byte