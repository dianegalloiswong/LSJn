OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix \
                -Is copieprouveur,copieprouveur/indexation
TARGET=native


main:
	$(OCAMLBUILD) arbres_main.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~ **/*~ **/**/*~

cleanall: realclean
