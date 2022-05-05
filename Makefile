all: expr miniml

expr:
	ocamlbuild -use-ocamlfind expr.byte

miniml:
	ocamlbuild -use-ocamlfind miniml.byte