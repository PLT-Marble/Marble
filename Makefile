all : marble.native

.PHONY : test
test : all testall.sh
	./testall.sh

marble.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind marble.native

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml calc.out calc
