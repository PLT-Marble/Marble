all : marble.native

.PHONY : test
test : clean all testall.sh
	./testall.sh

marble.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind marble.native

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml *.ll _build *.native
