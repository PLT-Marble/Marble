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

TARFILES = Makefile  README.md  _tags  ast.ml  codegen.ml  marble.ml  parser.mly \
			sast.ml  scanner.mll  semant.ml  testall.sh  tests/*

marble.tar.gz : $(TARFILES)
	cd .. && tar czf Marble/marble.tar.gz \
		$(TARFILES:%=Marble/%)
