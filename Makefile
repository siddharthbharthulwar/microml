all: expr Minicaml

expr:
	ocamlbuild -use-ocamlfind expr.byte

Minicaml:
	ocamlbuild -use-ocamlfind Minicaml.byte