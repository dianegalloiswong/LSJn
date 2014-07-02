OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix \
                -Is global,init,sequent,sequent/structures,reponse,exemples,analyseur \
                -use-menhir
TARGET=native


main:
	$(OCAMLBUILD) main.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f **/*~

cleanall: realclean
