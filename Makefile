all : marble.native

marble.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind marble.native

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml calc.out calc ast.mli
